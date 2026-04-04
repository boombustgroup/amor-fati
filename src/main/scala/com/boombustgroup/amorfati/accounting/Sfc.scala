package com.boombustgroup.amorfati.accounting

import com.boombustgroup.amorfati.agents.{Banking, Firm, Household}
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.flows.AggregateBatchContract
import com.boombustgroup.amorfati.engine.ledger.LedgerStateAdapter
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.{AssetType, BatchedFlow, EntitySector}

/** Stock-flow consistent (SFC) accounting framework for the simulation.
  *
  * Every monetary flow in the model creates exactly one debit and one credit
  * entry — money is neither created nor destroyed outside of well-defined
  * banking operations (loan origination, NPL write-off, bond issuance, etc.).
  * This object provides the machinery to verify that invariant holds after
  * every simulated month.
  *
  * The verification works in three steps:
  *   1. '''StockState''' — capture all monetary stocks (deposits, loans, debt,
  *      bonds, NFA, …) from the current runtime state.
  *   2. '''SemanticFlows''' — assemble every flow that occurred during the
  *      month, using the exact same values that were applied to balance sheet
  *      updates in Simulation.step.
  *   3. '''validate''' — for each of the 13 identities, check that Δstock =
  *      Σflows within tolerance.
  *
  * Together these 13 identities cover every financial instrument in the model
  * (deposits, loans, government bonds, corporate bonds, mortgages, consumer
  * credit, NBFI credit, interbank positions, NFA, JST debt, FUS balance, and
  * flow-of-funds). Because every asset is some other sector's liability, the
  * Godley sectoral balances rule (S−I)+(G−T)+(X−M)=0 holds by construction when
  * all 13 identities pass.
  *
  * '''When to update this file:''' any new mechanism that modifies a monetary
  * stock (bank capital, deposits, government debt, NFA, bond holdings, or
  * interbank positions) MUST have its semantic flow reflected here and checked
  * in validate — otherwise the check will fail at runtime.
  */
object Sfc:

  opaque type ExecutionIndex = Int
  object ExecutionIndex:
    def apply(value: Int): ExecutionIndex            = value
    extension (index: ExecutionIndex) def value: Int = index

  case class ExecutionBalanceKey(
      sector: EntitySector,
      asset: AssetType,
      index: ExecutionIndex,
  )

  case class ExecutionSnapshot(
      balances: Map[ExecutionBalanceKey, PLN],
  ):
    def balance(key: ExecutionBalanceKey): PLN =
      balances.getOrElse(key, PLN.Zero)

  object ExecutionSnapshot:
    def fromRaw(
        balances: Map[(EntitySector, AssetType, Int), Long],
    ): ExecutionSnapshot =
      ExecutionSnapshot(
        balances.iterator.map { case ((sector, asset, index), amount) =>
          ExecutionBalanceKey(sector, asset, ExecutionIndex(index)) -> PLN.fromRaw(amount)
        }.toMap,
      )

  /** Minimal runtime view needed for stock-side SFC validation. */
  case class RuntimeState(
      world: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      banks: Vector[Banking.BankState],
  )

  /** Point-in-time stock state for SFC validation.
    *
    * Captured twice per month (before and after Simulation.step) so that
    * validate can compute Δstock = curr - prev for each identity. Fields
    * corresponding to disabled mechanisms are simply zero — the identity holds
    * trivially in that case.
    */
  case class StockState(
      hhSavings: PLN,                // Σ household savings (individual mode only, 0 in aggregate)
      hhDebt: PLN,                   // Σ household debt (individual mode only, 0 in aggregate)
      firmCash: PLN,                 // Σ firm cash holdings
      firmDebt: PLN,                 // Σ firm bank loan debt
      bankCapital: PLN,              // aggregate bank equity capital (retained earnings)
      bankDeposits: PLN,             // aggregate bank deposits (HH + firm + JST)
      bankLoans: PLN,                // aggregate bank loan book
      govDebt: PLN,                  // fiscal debt metric from deficit accumulation, distinct from holder-tracked bond stock
      nfa: PLN,                      // net foreign assets (BoP cumulative)
      bankBondHoldings: PLN,         // bank holdings of government bonds
      nbpBondHoldings: PLN,          // NBP holdings of government bonds (QE)
      bondsOutstanding: PLN,         // total government bonds outstanding
      interbankNetSum: PLN,          // Σ interbank net positions (must be 0)
      jstDeposits: PLN,              // local government (JST) deposits at banks
      jstDebt: PLN,                  // local government (JST) cumulative debt
      fusBalance: PLN,               // ZUS/FUS raw surplus/deficit
      nfzBalance: PLN,               // NFZ health fund surplus/deficit
      foreignBondHoldings: PLN,      // non-resident SPW holdings
      ppkBondHoldings: PLN,          // PPK government bond holdings
      mortgageStock: PLN,            // Outstanding mortgage debt
      consumerLoans: PLN,            // Outstanding consumer credit stock
      corpBondsOutstanding: PLN,     // corporate bond stock
      insuranceGovBondHoldings: PLN, // insurance gov bond holdings
      tfiGovBondHoldings: PLN,       // TFI gov bond holdings
      nbfiLoanStock: PLN,            // NBFI credit stock
  )

  /** Semantic monthly flow evidence used by the SFC oracle.
    *
    * These values are assembled in WorldAssemblyEconomics from the intermediate
    * results of Simulation.step. They must match the '''exact''' values used in
    * balance sheet updates — any discrepancy will cause validate to report an
    * SfcIdentityError. Flows for disabled mechanisms are simply zero, so the
    * corresponding identity holds trivially.
    */
  case class SemanticFlows(
      govSpending: PLN,             // total budget expenditure (benefits + transfers + current spend + domestic capital spend + debt service + subventions + domestic EU co-financing)
      govRevenue: PLN,              // total gov revenue (CIT + PIT + VAT + excise + customs + dividend tax + NBP remittance)
      nplLoss: PLN,                 // bank NPL write-off loss (firm loans, after recovery)
      interestIncome: PLN,          // bank interest income from firm loans
      hhDebtService: PLN,           // household debt service payments → bank capital
      totalIncome: PLN,             // aggregate household income (wages + benefits + transfers)
      totalConsumption: PLN,        // aggregate household consumption expenditure
      newLoans: PLN,                // new firm loans originated this month
      nplRecovery: PLN,             // recovered amount from NPL (nplNew × recoveryRate)
      currentAccount: PLN,          // BoP current account balance
      valuationEffect: PLN,         // NFA valuation change from exchange rate movements
      bankBondIncome: PLN,          // bank coupon income from government bonds
      qePurchase: PLN,              // NBP quantitative easing bond purchases
      newBondIssuance: PLN,         // net new government bond issuance
      depositInterestPaid: PLN,     // bank interest paid on deposits → reduces bank capital
      reserveInterest: PLN,         // NBP pays on required reserves
      standingFacilityIncome: PLN,  // Deposit/lombard facility net
      interbankInterest: PLN,       // Interbank interest (net ≈ 0)
      jstDepositChange: PLN,        // JST deposit flow
      jstSpending: PLN,             // JST spending
      jstRevenue: PLN,              // JST revenue
      zusContributions: PLN,        // ZUS contributions
      zusPensionPayments: PLN,      // ZUS pension payments
      zusGovSubvention: PLN,        // ZUS gov subvention
      nfzContributions: PLN,        // NFZ health insurance contributions
      nfzSpending: PLN,             // NFZ health spending
      nfzGovSubvention: PLN,        // NFZ gov subvention
      dividendIncome: PLN,          // net domestic dividends → HH deposits
      foreignDividendOutflow: PLN,  // foreign dividends → CA outflow
      dividendTax: PLN,             // Belka tax → gov revenue
      mortgageInterestIncome: PLN,  // mortgage interest → bank capital
      mortgageNplLoss: PLN,         // mortgage NPL loss → bank capital
      mortgageOrigination: PLN,     // new mortgages issued
      mortgagePrincipalRepaid: PLN, // monthly principal repaid
      mortgageDefaultAmount: PLN,   // gross mortgage defaults (before recovery)
      remittanceOutflow: PLN,       // immigrant remittances → deposit outflow
      fofResidual: PLN,             // flow-of-funds residual (Σ firmRevenue - Σ sectorDemand)
      consumerDebtService: PLN,     // consumer credit: monthly debt service (principal + interest)
      consumerNplLoss: PLN,         // consumer credit: NPL loss (after recovery)
      consumerOrigination: PLN,     // consumer credit: new loan origination
      consumerPrincipalRepaid: PLN, // consumer credit: principal portion of debt service
      consumerDefaultAmount: PLN,   // consumer credit: gross default amount (before recovery)
      corpBondCouponIncome: PLN,    // bank coupon income from corp bonds
      corpBondDefaultLoss: PLN,     // bank loss from corp bond defaults
      corpBondIssuance: PLN,        // new corp bonds issued this month
      corpBondAmortization: PLN,    // corp bond principal repaid
      corpBondDefaultAmount: PLN,   // gross corp bond defaults
      insNetDepositChange: PLN,     // insurance net HH deposit change
      nbfiDepositDrain: PLN,        // TFI deposit drain
      nbfiOrigination: PLN,         // NBFI monthly origination
      nbfiRepayment: PLN,           // NBFI monthly repayment
      nbfiDefaultAmount: PLN,       // NBFI gross monthly defaults
      fdiProfitShifting: PLN,       // FDI profit shifting (service import)
      fdiRepatriation: PLN,         // FDI dividend repatriation (primary income debit)
      diasporaInflow: PLN,          // diaspora remittance inflow → deposit inflow
      tourismExport: PLN,           // inbound tourism → deposit inflow + export
      tourismImport: PLN,           // outbound tourism → deposit outflow + import
      bfgLevy: PLN,                 // BFG levy (bank capital expense)
      bailInLoss: PLN,              // bail-in deposit destruction
      bankCapitalDestruction: PLN,  // Capital wiped when bank fails (shareholders wiped)
      investNetDepositFlow: PLN,    // Investment demand net flow: lagged revenue - current spending
      firmPrincipalRepaid: PLN,     // firm loan principal repaid (deposit destruction)
      unrealizedBondLoss: PLN,      // mark-to-market loss on gov bond portfolio (interest rate risk channel)
      htmRealizedLoss: PLN,         // realized loss from HTM forced reclassification (HTM reclassification channel)
      eclProvisionChange: PLN,      // IFRS 9 ECL provision change (positive = additional provision → capital hit)
  )

  /** Enumeration of exact runtime identities plus legacy diagnostic metric
    * identities. The public-sector metric identities remain here so callers can
    * inspect them explicitly, but they are no longer part of the exact core
    * validation path.
    */
  enum SfcIdentity:
    case BankCapital, BankDeposits, GovDebt, GovBudgetCash, JstCash, ZusCash,
      NfzCash, FpCash, PfronCash, FgspCash, Nfa, BondClearing, InterbankNetting, JstDebt, FusBalance,
      NfzBalance, MortgageStock,
      FlowOfFunds, ConsumerCredit, CorpBondStock, NbfiCredit

  /** A single identity violation, carrying the identity that failed, a
    * human-readable description, and the expected vs actual monetary values so
    * callers can inspect the magnitude of the discrepancy.
    */
  case class SfcIdentityError(
      identity: SfcIdentity,
      msg: String,
      expected: PLN,
      actual: PLN,
  )

  /** Thrown when SFC validation fails — continuing past a broken identity is
    * meaningless.
    */
  class SfcViolationException(val month: Int, val errors: Vector[SfcIdentityError])
      extends RuntimeException({
        import ComputationBoundary.toDouble
        errors
          .map(e => f"Month $month ${e.identity}: Δ=${toDouble(e.actual - e.expected)}%.2f — ${e.msg}")
          .mkString("; ")
      })

  /** Result of SFC validation: Right(()) if all identities hold, Left(errors)
    * otherwise.
    */
  type SfcResult = Either[Vector[SfcIdentityError], Unit]

  /** Build stock state from the current simulation state by aggregating all
    * agent-level stocks.
    */
  def snapshot(
      w: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      banks: Vector[Banking.BankState],
  ): StockState =
    val hhS     = PLN.fromRaw(households.map(_.savings.toLong).sum)
    val hhD     = PLN.fromRaw(households.map(_.debt.toLong).sum)
    val ibNet   = PLN.fromRaw(banks.map(_.interbankNet.toLong).sum)
    val bankAgg = Banking.aggregateFromBanks(banks)
    val bonds   = LedgerStateAdapter.governmentBondCircuit(w, banks)
    StockState(
      hhSavings = hhS,
      hhDebt = hhD,
      firmCash = PLN.fromRaw(firms.map(_.cash.toLong).sum),
      firmDebt = PLN.fromRaw(firms.map(_.debt.toLong).sum),
      bankCapital = bankAgg.capital,
      bankDeposits = bankAgg.deposits,
      bankLoans = bankAgg.totalLoans,
      govDebt = w.gov.cumulativeDebt,
      nfa = w.bop.nfa,
      bankBondHoldings = bonds.bankHoldings,
      nbpBondHoldings = bonds.nbpHoldings,
      bondsOutstanding = bonds.outstanding,
      interbankNetSum = ibNet,
      jstDeposits = w.social.jst.deposits,
      jstDebt = w.social.jst.debt,
      fusBalance = w.social.zus.fusBalance,
      nfzBalance = w.social.nfz.balance,
      foreignBondHoldings = bonds.foreignHoldings,
      ppkBondHoldings = bonds.ppkHoldings,
      mortgageStock = w.real.housing.mortgageStock,
      consumerLoans = bankAgg.consumerLoans,
      corpBondsOutstanding = w.financial.corporateBonds.outstanding,
      insuranceGovBondHoldings = bonds.insuranceHoldings,
      tfiGovBondHoldings = bonds.tfiHoldings,
      nbfiLoanStock = w.financial.nbfi.nbfiLoanStock,
    )

  def snapshot(state: RuntimeState): StockState =
    snapshot(state.world, state.firms, state.households, state.banks)

  /** Validate exact balance-sheet identities. Returns `Right(())` if all pass,
    * or `Left(errors)` with every violated identity and its expected/actual
    * values.
    *
    * Together these exact identities ensure that every financial asset has a
    * matching liability, which implies the Godley sectoral balances rule
    * (S−I)+(G−T)+(X−M)=0 by construction.
    *
    * The monetary circuit closes via sector-level flow-of-funds (Identity 10).
    *
    *   1. Bank capital: Δ = -nplLoss - mortgageNplLoss - consumerNplLoss -
    *      corpBondDefaultLoss - bfgLevy - bankCapitalDestruction +
    *      (interestIncome + hhDebtService + bankBondIncome +
    *      mortgageInterestIncome + consumerDebtService + corpBondCouponIncome -
    *      depositInterestPaid + reserveInterest + standingFacilityIncome +
    *      interbankInterest) × BankProfitRetention
    *   2. Bank deposits: Δ = totalIncome - totalConsumption +
    *      investNetDepositFlow + jstDepositChange + dividendIncome -
    *      foreignDividendOutflow - remittanceOutflow + diasporaInflow +
    *      tourismExport - tourismImport - bailInLoss + consumerOrigination +
    *      insNetDepositChange + nbfiDepositDrain
    *   3. NFA: Δ = currentAccount + valuationEffect (currentAccount includes
    *      -foreignDividendOutflow, -fdiProfitShifting, -fdiRepatriation,
    *      +diasporaInflow)
    *   4. Bond clearing: bankBondHoldings + nbpBondHoldings + ppkBondHoldings +
    *      insuranceGovBondHoldings + tfiGovBondHoldings = bondsOutstanding
    *   5. Interbank netting: Σ interbankNet_i = 0 (trivially 0 in single-bank
    *      mode)
    *   6. Mortgage stock: Δ = origination - principalRepaid - defaultAmount
    *      (trivially 0 when RE disabled)
    *   7. Flow-of-funds: Σ firmRevenue = domesticCons + govPurchases +
    *      investDemand + exports (closes by construction)
    *   8. Consumer credit: Δ consumerLoans = origination - principalRepaid -
    *      defaultAmount
    *   9. Corp bond stock: Δ corpBondsOutstanding = issuance - amortization -
    *      defaultAmount
    *   10. NBFI credit stock: Δ nbfiLoanStock = origination - repayment -
    *       defaultAmount
    *
    * These catch semantic stock-flow mismatches: mis-routed flows, refactoring
    * errors in stock updates, and any new flow that changes a tracked stock
    * without updating the corresponding SFC flow projection. Exact global
    * conservation is enforced separately by ledger execution.
    */
  private case class IdentitySpec(id: SfcIdentity, msg: String, expected: PLN, actual: PLN, tolerance: PLN)

  def validate(
      prev: StockState,               // stocks at the beginning of the month (before Simulation.step)
      curr: StockState,               // stocks at the end of the month (after Simulation.step)
      flows: SemanticFlows,           // all flows that occurred during the month
      tolerance: PLN = PLN(1000.0),   // Residual rounding / stock-flow projection differences
      nfaTolerance: PLN = PLN(1000.0), // NFA (BoP valuation + rounding)
  )(using p: SimParams): SfcResult =
    import SfcIdentity.*

    val identities: Vector[IdentitySpec] = Vector(
      // 1. Bank capital: losses + profit retention
      //    PLN is Long-based — addition is exact, no Kahan needed
      IdentitySpec(
        BankCapital,
        "bank capital change (profit retention + losses)",
        expected = {
          val losses      = flows.nplLoss + flows.mortgageNplLoss + flows.consumerNplLoss +
            flows.corpBondDefaultLoss + flows.bfgLevy + flows.unrealizedBondLoss +
            flows.htmRealizedLoss + flows.eclProvisionChange + flows.bankCapitalDestruction
          val grossIncome = flows.interestIncome + flows.hhDebtService + flows.bankBondIncome +
            flows.mortgageInterestIncome + flows.consumerDebtService + flows.corpBondCouponIncome -
            flows.depositInterestPaid + flows.reserveInterest + flows.standingFacilityIncome +
            flows.interbankInterest
          -losses + grossIncome * p.banking.profitRetention
        },
        actual = curr.bankCapital - prev.bankCapital,
        tolerance,
      ),
      // 2. Bank deposits: HH income − consumption + all deposit-affecting flows
      //    PLN is Long-based — addition is exact, no Kahan needed
      IdentitySpec(
        BankDeposits,
        "bank deposits change",
        expected = flows.totalIncome - flows.totalConsumption + flows.investNetDepositFlow +
          flows.jstDepositChange + flows.dividendIncome - flows.foreignDividendOutflow -
          flows.remittanceOutflow + flows.diasporaInflow + flows.tourismExport -
          flows.tourismImport - flows.bailInLoss + flows.newLoans - flows.firmPrincipalRepaid +
          flows.consumerOrigination + flows.insNetDepositChange + flows.nbfiDepositDrain,
        actual = curr.bankDeposits - prev.bankDeposits,
        tolerance,
      ),
      // 3. NFA: current account + valuation (wider tolerance for FP cancellation)
      IdentitySpec(
        Nfa,
        "NFA change (current account + valuation)",
        expected = flows.currentAccount + flows.valuationEffect,
        actual = curr.nfa - prev.nfa,
        nfaTolerance,
      ),
      // 4. Bond clearing: holders = outstanding (level, not delta)
      IdentitySpec(
        BondClearing,
        s"bond clearing [bank=${curr.bankBondHoldings}, nbp=${curr.nbpBondHoldings}, foreign=${curr.foreignBondHoldings}, ppk=${curr.ppkBondHoldings}, ins=${curr.insuranceGovBondHoldings}, tfi=${curr.tfiGovBondHoldings}, outstanding=${curr.bondsOutstanding}]",
        expected = curr.bondsOutstanding,
        actual = curr.bankBondHoldings + curr.nbpBondHoldings + curr.foreignBondHoldings +
          curr.ppkBondHoldings + curr.insuranceGovBondHoldings + curr.tfiGovBondHoldings,
        PLN.Zero,
      ),
      // 5. Interbank netting: Σ net positions = 0
      IdentitySpec(
        InterbankNetting,
        "interbank netting (should be zero)",
        expected = PLN.Zero,
        actual = curr.interbankNetSum,
        tolerance,
      ),
      // 6. Mortgage stock: origination − repayment − default
      IdentitySpec(
        MortgageStock,
        "mortgage stock change",
        expected = flows.mortgageOrigination - flows.mortgagePrincipalRepaid - flows.mortgageDefaultAmount,
        actual = curr.mortgageStock - prev.mortgageStock,
        tolerance,
      ),
      // 7. Flow-of-funds: residual = 0 (closes by construction)
      IdentitySpec(
        FlowOfFunds,
        "flow-of-funds residual",
        expected = PLN.Zero,
        actual = flows.fofResidual,
        tolerance,
      ),
      // 8. Consumer credit: origination − debtService − default (debtSvc = P+I reduces stock)
      IdentitySpec(
        ConsumerCredit,
        "consumer credit stock change",
        expected = flows.consumerOrigination - flows.consumerDebtService - flows.consumerDefaultAmount,
        actual = curr.consumerLoans - prev.consumerLoans,
        tolerance,
      ),
      // 9. Corporate bond stock: issuance − amortization − default
      IdentitySpec(
        CorpBondStock,
        "corporate bond stock change",
        expected = flows.corpBondIssuance - flows.corpBondAmortization - flows.corpBondDefaultAmount,
        actual = curr.corpBondsOutstanding - prev.corpBondsOutstanding,
        tolerance,
      ),
      // 10. NBFI credit: origination − repayment − default
      IdentitySpec(
        NbfiCredit,
        "NBFI credit stock change",
        expected = flows.nbfiOrigination - flows.nbfiRepayment - flows.nbfiDefaultAmount,
        actual = curr.nbfiLoanStock - prev.nbfiLoanStock,
        tolerance,
      ),
    )

    val errors = identities.collect:
      case IdentitySpec(id, msg, expected, actual, tol) if (actual - expected).abs > tol =>
        SfcIdentityError(id, msg, expected, actual)

    if errors.isEmpty then Right(()) else Left(errors)

  def metricDiagnostics(
      prev: StockState,
      curr: StockState,
      flows: SemanticFlows,
      tolerance: PLN = PLN(1000.0),
  ): Vector[SfcIdentityError] =
    Vector(
      IdentitySpec(
        SfcIdentity.GovDebt,
        "government debt change",
        expected = flows.govSpending - flows.govRevenue,
        actual = curr.govDebt - prev.govDebt,
        tolerance,
      ),
      IdentitySpec(
        SfcIdentity.JstDebt,
        "JST debt change",
        expected = flows.jstSpending - flows.jstRevenue,
        actual = curr.jstDebt - prev.jstDebt,
        tolerance,
      ),
      IdentitySpec(
        SfcIdentity.FusBalance,
        "FUS balance change (contributions - pensions)",
        expected = flows.zusContributions - flows.zusPensionPayments,
        actual = curr.fusBalance - prev.fusBalance,
        tolerance,
      ),
      IdentitySpec(
        SfcIdentity.NfzBalance,
        "NFZ balance change (contributions - spending)",
        expected = flows.nfzContributions - flows.nfzSpending,
        actual = curr.nfzBalance - prev.nfzBalance,
        tolerance,
      ),
    ).collect:
      case IdentitySpec(id, msg, expected, actual, tol) if (actual - expected).abs > tol =>
        SfcIdentityError(id, msg, expected, actual)

  /** Preferred production API: project stocks from runtime state and combine
    * them with explicit flow semantics plus independent ledger execution
    * conservation.
    */
  def validate(
      prev: RuntimeState,
      curr: RuntimeState,
      flows: SemanticFlows,
      batches: Vector[BatchedFlow],
      executionSnapshot: ExecutionSnapshot,
      totalWealth: Long,
      tolerance: PLN,
      nfaTolerance: PLN,
  )(using p: SimParams): SfcResult =
    // In the runtime API, Flow-of-Funds is checked from executed ledger
    // batches. Keep the stock-side identities from the legacy oracle, but
    // neutralize the old hand-assembled residual to avoid double-counting the
    // same concept through two different channels.
    //
    // Public-sector note:
    // exact runtime validation uses executed public cash accounts from ledger
    // execution; public-sector metric identities are available separately via
    // `metricDiagnostics`.
    val baseErrors    =
      validate(snapshot(prev), snapshot(curr), flows.copy(fofResidual = PLN.Zero), tolerance, nfaTolerance).left.toOption.getOrElse(Vector.empty)
    val runtimeErrors = runtimeIdentityErrors(batches, executionSnapshot, totalWealth)
    merge(baseErrors ++ runtimeErrors)

  private def runtimeIdentityErrors(
      batches: Vector[BatchedFlow],
      executionSnapshot: ExecutionSnapshot,
      totalWealth: Long,
  ): Vector[SfcIdentityError] =
    val publicCashErrors =
      runtimeCashIdentity(
        SfcIdentity.GovBudgetCash,
        "government budget cash",
        AccountRef(EntitySector.Government, ExecutionIndex(AggregateBatchContract.GovernmentIndex.Budget)),
        batches,
        executionSnapshot,
      ) ++
        runtimeCashIdentity(
          SfcIdentity.JstCash,
          "JST cash",
          AccountRef(EntitySector.Funds, ExecutionIndex(AggregateBatchContract.FundIndex.Jst)),
          batches,
          executionSnapshot,
        ) ++
        runtimeCashIdentity(
          SfcIdentity.ZusCash,
          "ZUS cash",
          AccountRef(EntitySector.Funds, ExecutionIndex(AggregateBatchContract.FundIndex.Zus)),
          batches,
          executionSnapshot,
        ) ++
        runtimeCashIdentity(
          SfcIdentity.NfzCash,
          "NFZ cash",
          AccountRef(EntitySector.Funds, ExecutionIndex(AggregateBatchContract.FundIndex.Nfz)),
          batches,
          executionSnapshot,
        ) ++
        runtimeCashIdentity(
          SfcIdentity.FpCash,
          "FP cash",
          AccountRef(EntitySector.Funds, ExecutionIndex(AggregateBatchContract.FundIndex.Fp)),
          batches,
          executionSnapshot,
        ) ++
        runtimeCashIdentity(
          SfcIdentity.PfronCash,
          "PFRON cash",
          AccountRef(EntitySector.Funds, ExecutionIndex(AggregateBatchContract.FundIndex.Pfron)),
          batches,
          executionSnapshot,
        ) ++
        runtimeCashIdentity(
          SfcIdentity.FgspCash,
          "FGSP cash",
          AccountRef(EntitySector.Funds, ExecutionIndex(AggregateBatchContract.FundIndex.Fgsp)),
          batches,
          executionSnapshot,
        )
    val fofErrors        =
      if totalWealth == 0L then Vector.empty
      else
        Vector(
          SfcIdentityError(
            SfcIdentity.FlowOfFunds,
            s"ledger execution totalWealth=$totalWealth across ${batches.size} batches",
            expected = PLN.Zero,
            actual = PLN.fromRaw(totalWealth),
          ),
        )
    publicCashErrors ++ fofErrors

  private def merge(errors: Vector[SfcIdentityError]): SfcResult =
    val combined = errors
    if combined.isEmpty then Right(())
    else Left(combined)

  private case class AccountRef(sector: EntitySector, index: ExecutionIndex)

  private def runtimeCashIdentity(
      identity: SfcIdentity,
      label: String,
      account: AccountRef,
      batches: Vector[BatchedFlow],
      executionSnapshot: ExecutionSnapshot,
  ): Vector[SfcIdentityError] =
    val expected = cashAccountNetFlow(account, batches)
    val actual   = cashAccountBalance(account, executionSnapshot)
    if actual != expected then
      Vector(
        SfcIdentityError(
          identity,
          s"$label [expected net=$expected, actual closing=$actual]",
          expected = expected,
          actual = actual,
        ),
      )
    else Vector.empty

  private def cashAccountBalance(
      account: AccountRef,
      executionSnapshot: ExecutionSnapshot,
  ): PLN =
    executionSnapshot.balance(
      ExecutionBalanceKey(account.sector, AssetType.Cash, account.index),
    )

  private def cashAccountNetFlow(account: AccountRef, batches: Vector[BatchedFlow]): PLN =
    PLN.fromRaw(
      batches.iterator.map(batch => cashAccountDelta(account, batch)).sum,
    )

  private def cashAccountDelta(account: AccountRef, batch: BatchedFlow): Long =
    if batch.asset != AssetType.Cash then 0L
    else
      batch match
        case broadcast: BatchedFlow.Broadcast if isAccount(batch.from, broadcast.fromIndex, account) =>
          -broadcast.amounts.iterator.sum
        case broadcast: BatchedFlow.Broadcast if batch.to == account.sector                          =>
          broadcast.amounts.indices.iterator
            .filter(i => broadcast.targetIndices(i) == account.index)
            .map(i => broadcast.amounts(i))
            .sum
        case scatter: BatchedFlow.Scatter if batch.from == account.sector                            =>
          scatter.amounts.indices.iterator
            .filter(i => i == account.index)
            .map(i => -scatter.amounts(i))
            .sum
        case scatter: BatchedFlow.Scatter if batch.to == account.sector                              =>
          scatter.amounts.indices.iterator
            .filter(i => scatter.targetIndices(i) == account.index)
            .map(i => scatter.amounts(i))
            .sum
        case _                                                                                       => 0L

  private def isAccount(sector: EntitySector, index: Int, account: AccountRef): Boolean =
    sector == account.sector && index == ExecutionIndex.value(account.index)
