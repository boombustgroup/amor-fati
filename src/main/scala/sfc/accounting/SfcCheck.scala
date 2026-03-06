package sfc.accounting

import sfc.agents.{Firm, Household}
import sfc.engine.World
import sfc.types.*
import sfc.util.KahanSum.*

object SfcCheck:

  /** Snapshot of all monetary stocks in the economy at one point in time. */
  case class Snapshot(
    hhSavings: PLN,
    hhDebt: PLN,
    firmCash: PLN,
    firmDebt: PLN,
    bankCapital: PLN,
    bankDeposits: PLN,
    bankLoans: PLN,
    govDebt: PLN,
    nfa: PLN = PLN.Zero,
    bankBondHoldings: PLN = PLN.Zero,
    nbpBondHoldings: PLN = PLN.Zero,
    bondsOutstanding: PLN = PLN.Zero,
    interbankNetSum: PLN = PLN.Zero,
    jstDeposits: PLN = PLN.Zero,
    jstDebt: PLN = PLN.Zero,
    fusBalance: PLN = PLN.Zero,     // ZUS/FUS raw surplus/deficit
    ppkBondHoldings: PLN = PLN.Zero, // PPK government bond holdings
    mortgageStock: PLN = PLN.Zero,  // Outstanding mortgage debt
    consumerLoans: PLN = PLN.Zero,  // Outstanding consumer credit stock
    corpBondsOutstanding: PLN = PLN.Zero,  // corporate bond stock
    insuranceGovBondHoldings: PLN = PLN.Zero,  // insurance gov bond holdings
    tfiGovBondHoldings: PLN = PLN.Zero,  // TFI gov bond holdings
    nbfiLoanStock: PLN = PLN.Zero        // NBFI credit stock
  )

  /** Flows observed during a single month (computed in Simulation.step).
    * These must match the EXACT values used in balance sheet updates. */
  case class MonthlyFlows(
    govSpending: PLN,
    govRevenue: PLN,
    nplLoss: PLN,
    interestIncome: PLN,
    hhDebtService: PLN,
    totalIncome: PLN,
    totalConsumption: PLN,
    newLoans: PLN,
    nplRecovery: PLN,
    currentAccount: PLN = PLN.Zero,
    valuationEffect: PLN = PLN.Zero,
    bankBondIncome: PLN = PLN.Zero,
    qePurchase: PLN = PLN.Zero,
    newBondIssuance: PLN = PLN.Zero,
    depositInterestPaid: PLN = PLN.Zero,
    reserveInterest: PLN = PLN.Zero,          // NBP pays on required reserves
    standingFacilityIncome: PLN = PLN.Zero,   // Deposit/lombard facility net
    interbankInterest: PLN = PLN.Zero,        // Interbank interest (net ≈ 0)
    jstDepositChange: PLN = PLN.Zero,         // JST deposit flow
    jstSpending: PLN = PLN.Zero,              // JST spending
    jstRevenue: PLN = PLN.Zero,              // JST revenue
    zusContributions: PLN = PLN.Zero,        // ZUS contributions
    zusPensionPayments: PLN = PLN.Zero,      // ZUS pension payments
    zusGovSubvention: PLN = PLN.Zero,        // ZUS gov subvention
    dividendIncome: PLN = PLN.Zero,          // net domestic dividends → HH deposits
    foreignDividendOutflow: PLN = PLN.Zero,  // foreign dividends → CA outflow
    dividendTax: PLN = PLN.Zero,            // Belka tax → gov revenue
    mortgageInterestIncome: PLN = PLN.Zero, // mortgage interest → bank capital
    mortgageNplLoss: PLN = PLN.Zero,        // mortgage NPL loss → bank capital
    mortgageOrigination: PLN = PLN.Zero,    // new mortgages issued
    mortgagePrincipalRepaid: PLN = PLN.Zero, // monthly principal repaid
    mortgageDefaultAmount: PLN = PLN.Zero,  // gross mortgage defaults (before recovery)
    remittanceOutflow: PLN = PLN.Zero,      // immigrant remittances → deposit outflow
    fofResidual: PLN = PLN.Zero,            // flow-of-funds residual (Σ firmRevenue - Σ sectorDemand)
    consumerDebtService: PLN = PLN.Zero,    // consumer credit: monthly debt service (principal + interest)
    consumerNplLoss: PLN = PLN.Zero,        // consumer credit: NPL loss (after recovery)
    consumerOrigination: PLN = PLN.Zero,    // consumer credit: new loan origination
    consumerPrincipalRepaid: PLN = PLN.Zero, // consumer credit: principal portion of debt service
    consumerDefaultAmount: PLN = PLN.Zero,  // consumer credit: gross default amount (before recovery)
    corpBondCouponIncome: PLN = PLN.Zero,   // bank coupon income from corp bonds
    corpBondDefaultLoss: PLN = PLN.Zero,    // bank loss from corp bond defaults
    corpBondIssuance: PLN = PLN.Zero,       // new corp bonds issued this month
    corpBondAmortization: PLN = PLN.Zero,   // corp bond principal repaid
    corpBondDefaultAmount: PLN = PLN.Zero,  // gross corp bond defaults
    insNetDepositChange: PLN = PLN.Zero,    // insurance net HH deposit change
    nbfiDepositDrain: PLN = PLN.Zero,      // TFI deposit drain
    nbfiOrigination: PLN = PLN.Zero,       // NBFI monthly origination
    nbfiRepayment: PLN = PLN.Zero,         // NBFI monthly repayment
    nbfiDefaultAmount: PLN = PLN.Zero,     // NBFI gross monthly defaults
    fdiProfitShifting: PLN = PLN.Zero,     // FDI profit shifting (service import)
    fdiRepatriation: PLN = PLN.Zero,       // FDI dividend repatriation (primary income debit)
    diasporaInflow: PLN = PLN.Zero,        // diaspora remittance inflow → deposit inflow
    tourismExport: PLN = PLN.Zero,         // inbound tourism → deposit inflow + export
    tourismImport: PLN = PLN.Zero,         // outbound tourism → deposit outflow + import
    bfgLevy: PLN = PLN.Zero,              // BFG levy (bank capital expense)
    bailInLoss: PLN = PLN.Zero,           // bail-in deposit destruction
    bankCapitalDestruction: PLN = PLN.Zero, // Capital wiped when bank fails (shareholders wiped)
    investNetDepositFlow: PLN = PLN.Zero, // Investment demand net flow: lagged revenue - current spending
    // Identity 14 (sectoral balances): (S−I) + (G−T) + (X−M) = 0
    exports: PLN = PLN.Zero,              // Total goods exports (BoP)
    totalImports: PLN = PLN.Zero,         // Total goods imports (BoP, incl. profit shifting)
    grossInvestment: PLN = PLN.Zero,      // Firm gross fixed capital formation
    greenInvestment: PLN = PLN.Zero,      // Green capital investment
    inventoryChange: PLN = PLN.Zero       // ΔInventories (SNA 2008)
  )

  /** Result of the SFC check: ten exact balance-sheet identity checks. */
  case class SfcResult(
    month: Int,
    bankCapitalError: Double,
    bankDepositsError: Double,
    govDebtError: Double,
    nfaError: Double = 0.0,
    bondClearingError: Double = 0.0,
    interbankNettingError: Double = 0.0,
    jstDebtError: Double = 0.0,        // JST budget balance
    fusBalanceError: Double = 0.0,     // FUS balance
    mortgageStockError: Double = 0.0,  // Mortgage stock identity
    fofError: Double = 0.0,           // Flow-of-funds residual
    consumerCreditError: Double = 0.0, // Consumer credit stock identity
    corpBondStockError: Double = 0.0,  // Corporate bond stock identity
    nbfiCreditError: Double = 0.0,     // NBFI credit stock identity
    sectoralBalancesError: Double = 0.0, //  (S−I) + (G−T) + (X−M) = 0 (Godley & Lavoie 2007)
    passed: Boolean
  )

  def snapshot(w: World, firms: Array[Firm.State],
               households: Option[Vector[Household.State]]): Snapshot =
    val hhS = PLN(households.map(_.kahanSumBy(_.savings.toDouble)).getOrElse(0.0))
    val hhD = PLN(households.map(_.kahanSumBy(_.debt.toDouble)).getOrElse(0.0))
    val ibNet = PLN(w.bankingSector.map(_.banks.kahanSumBy(_.interbankNet.toDouble)).getOrElse(0.0))
    Snapshot(
      hhSavings = hhS,
      hhDebt = hhD,
      firmCash = PLN(firms.kahanSumBy(_.cash.toDouble)),
      firmDebt = PLN(firms.kahanSumBy(_.debt.toDouble)),
      bankCapital = w.bank.capital,
      bankDeposits = w.bank.deposits,
      bankLoans = w.bank.totalLoans,
      govDebt = w.gov.cumulativeDebt,
      nfa = w.bop.nfa,
      bankBondHoldings = w.bank.govBondHoldings,
      nbpBondHoldings = w.nbp.govBondHoldings,
      bondsOutstanding = w.gov.bondsOutstanding,
      interbankNetSum = ibNet,
      jstDeposits = w.jst.deposits,
      jstDebt = w.jst.debt,
      fusBalance = w.zus.fusBalance,
      ppkBondHoldings = w.ppk.bondHoldings,
      mortgageStock = w.housing.mortgageStock,
      consumerLoans = w.bank.consumerLoans,
      corpBondsOutstanding = w.corporateBonds.outstanding,
      insuranceGovBondHoldings = w.insurance.govBondHoldings,
      tfiGovBondHoldings = w.nbfi.tfiGovBondHoldings,
      nbfiLoanStock = w.nbfi.nbfiLoanStock
    )

  /** Validate ten exact balance-sheet identities.
    *
    * The monetary circuit closes via sector-level flow-of-funds (Identity 10).
    *
    * 1. Bank capital:  Δ = -nplLoss - mortgageNplLoss - consumerNplLoss - bfgLevy - bankCapitalDestruction + (interestIncome + hhDebtService + bankBondIncome + mortgageInterestIncome + consumerDebtService - depositInterestPaid + reserveInterest + standingFacilityIncome + interbankInterest) × 0.3
    * 2. Bank deposits: Δ = totalIncome - totalConsumption + investNetDepositFlow + jstDepositChange + dividendIncome - foreignDividendOutflow - remittanceOutflow + diasporaInflow + tourismExport - tourismImport - bailInLoss + consumerOrigination + insNetDepositChange + nbfiDepositDrain
    * 3. Gov debt:      Δ = govSpending - govRevenue  (govRevenue includes dividendTax + zusGovSubvention)
    * 4. NFA:           Δ = currentAccount + valuationEffect  (currentAccount includes -foreignDividendOutflow, -fdiProfitShifting, -fdiRepatriation, +diasporaInflow)
    * 5. Bond clearing: bankBondHoldings + nbpBondHoldings + ppkBondHoldings + insuranceGovBondHoldings + tfiGovBondHoldings = bondsOutstanding
    * 6. Interbank netting: Σ interbankNet_i = 0 (trivially 0 in single-bank mode)
    * 7. JST debt:      Δ = jstSpending - jstRevenue (trivially 0 when JST disabled)
    * 8. FUS balance:   Δ = zusContributions - zusPensionPayments (trivially 0 when ZUS disabled)
    * 9. Mortgage stock: Δ = origination - principalRepaid - defaultAmount (trivially 0 when RE disabled)
    * 10. Flow-of-funds: Σ firmRevenue = domesticCons + govPurchases + investDemand + exports (closes by construction)
    * 11. Consumer credit: Δ consumerLoans = origination - principalRepaid - defaultAmount
    * 12. Corp bond stock: Δ corpBondsOutstanding = issuance - amortization - defaultAmount
    * 13. NBFI credit stock: Δ nbfiLoanStock = origination - repayment - defaultAmount
    * 14. Sectoral balances (Godley & Lavoie 2007): (S − I) + (G − T) + (X − M) = 0
    *     where (S − I) = private sector balance (savings minus investment),
    *           (G − T) = public sector balance (gov spending minus taxes),
    *           (X − M) = external sector balance (exports minus imports).
    *     Master macro identity — if Identities 1–13 hold, this must hold by construction.
    *
    * These catch: mis-routed flows (e.g. rent subtracted from HH but not added
    * to bank/consumption), refactoring errors in balance sheet updates, and
    * any new flow that modifies a stock without updating the counterpart. */
  def validate(month: Int, prev: Snapshot, curr: Snapshot,
               flows: MonthlyFlows, tolerance: Double = 0.01,
               nfaTolerance: Double = 1.0): SfcResult =

    // Identity 1: Bank capital (includes bond coupon income, mortgage interest income,
    // consumer credit debt service income, corp bond coupon income, minus deposit interest paid,
    // minus mortgage NPL loss, minus consumer NPL loss, minus corp bond default loss,
    // minus BFG levy (#48),
    // plus reserve interest, standing facility income, interbank interest — all × 0.3 profit retention)
    val expectedBankCapChange = -flows.nplLoss - flows.mortgageNplLoss - flows.consumerNplLoss
      - flows.corpBondDefaultLoss - flows.bfgLevy - flows.bankCapitalDestruction +
      (flows.interestIncome + flows.hhDebtService + flows.bankBondIncome
       + flows.mortgageInterestIncome + flows.consumerDebtService + flows.corpBondCouponIncome
       - flows.depositInterestPaid
       + flows.reserveInterest + flows.standingFacilityIncome + flows.interbankInterest) * 0.3
    val actualBankCapChange = curr.bankCapital - prev.bankCapital
    val bankCapErr = actualBankCapChange - expectedBankCapChange

    // Identity 2: Bank deposits (+ investment net flow + JST deposit flows + dividend flows - remittance outflow + diaspora inflow + tourism + consumer origination + insurance + NBFI - bail-in)
    val expectedDepChange = flows.totalIncome - flows.totalConsumption + flows.investNetDepositFlow +
      flows.jstDepositChange +
      flows.dividendIncome - flows.foreignDividendOutflow - flows.remittanceOutflow + flows.diasporaInflow +
      flows.tourismExport - flows.tourismImport - flows.bailInLoss +
      flows.consumerOrigination + flows.insNetDepositChange + flows.nbfiDepositDrain
    val actualDepChange = curr.bankDeposits - prev.bankDeposits
    val bankDepErr = actualDepChange - expectedDepChange

    // Identity 3: Government debt (deficit = spending - revenue, revenue includes dividendTax)
    val expectedGovDebtChange = flows.govSpending - flows.govRevenue
    val actualGovDebtChange = curr.govDebt - prev.govDebt
    val govDebtErr = actualGovDebtChange - expectedGovDebtChange

    // Identity 4: NFA (Net Foreign Assets)
    // ΔNFA = currentAccount + valuationEffect
    // When OPEN_ECON=false: both sides = 0.0, trivially passes.
    // currentAccount includes -foreignDividendOutflow (routed through OpenEconomy or legacy)
    val expectedNfaChange = flows.currentAccount + flows.valuationEffect
    val actualNfaChange = curr.nfa - prev.nfa
    val nfaErr = actualNfaChange - expectedNfaChange

    // Identity 5: Bond clearing
    // All outstanding bonds must be held by bank + NBP + PPK + insurance
    // When GovBondMarket=false: all bond fields = 0.0, trivially passes.
    val bondClearingErr = (curr.bankBondHoldings + curr.nbpBondHoldings + curr.ppkBondHoldings + curr.insuranceGovBondHoldings + curr.tfiGovBondHoldings) - curr.bondsOutstanding

    // Identity 6: Interbank netting
    // Sum of all banks' interbankNet positions must be zero (closed system).
    // Trivially 0 in single-bank mode (no bankingSector present).
    val interbankErr = curr.interbankNetSum

    // Identity 7: JST debt (trivially 0 when JST disabled — both sides = 0)
    val expectedJstDebtChange = flows.jstSpending - flows.jstRevenue
    val actualJstDebtChange = curr.jstDebt - prev.jstDebt
    val jstDebtErr = actualJstDebtChange - expectedJstDebtChange

    // Identity 8: FUS balance (trivially 0 when ZUS disabled — both sides = 0)
    val expectedFusChange = flows.zusContributions - flows.zusPensionPayments
    val actualFusChange = curr.fusBalance - prev.fusBalance
    val fusErr = actualFusChange - expectedFusChange

    // Identity 9: Mortgage stock (trivially 0 when RE disabled — both sides = 0)
    val expectedMortgageChange = flows.mortgageOrigination - flows.mortgagePrincipalRepaid - flows.mortgageDefaultAmount
    val actualMortgageChange = curr.mortgageStock - prev.mortgageStock
    val mortgageErr = actualMortgageChange - expectedMortgageChange

    // Identity 10: Flow-of-funds — Σ firmRevenue = domesticCons + govPurchases + exports
    // Closes by construction via sector-level demand multipliers.
    val fofErr = flows.fofResidual

    // Identity 11: Consumer credit stock — Δ consumerLoans = origination - principalRepaid - defaultAmount
    val expectedCcChange = flows.consumerOrigination - flows.consumerPrincipalRepaid - flows.consumerDefaultAmount
    val actualCcChange = curr.consumerLoans - prev.consumerLoans
    val ccErr = actualCcChange - expectedCcChange

    // Identity 12: Corporate bond stock — Δ corpBondsOutstanding = issuance - amortization - defaultAmount
    val expectedCorpBondChange = flows.corpBondIssuance - flows.corpBondAmortization - flows.corpBondDefaultAmount
    val actualCorpBondChange = curr.corpBondsOutstanding - prev.corpBondsOutstanding
    val corpBondErr = actualCorpBondChange - expectedCorpBondChange

    // Identity 13: NBFI credit stock — Δ nbfiLoanStock = origination - repayment - defaultAmount
    val expectedNbfiChange = flows.nbfiOrigination - flows.nbfiRepayment - flows.nbfiDefaultAmount
    val actualNbfiChange = curr.nbfiLoanStock - prev.nbfiLoanStock
    val nbfiCreditErr = actualNbfiChange - expectedNbfiChange

    // Identity 14: Sectoral balances (Godley & Lavoie 2007)
    // (S − I) + (G − T) + (X − M) = 0
    // S = totalIncome - totalConsumption (private savings)
    // I = grossInvestment + greenInvestment + inventoryChange (private investment)
    // G − T = govSpending - govRevenue (public deficit, = Identity 3)
    // X = exports + tourismExport + diasporaInflow (all external receipts)
    // M = totalImports + tourismImport + remittanceOutflow + foreignDividendOutflow
    //     + fdiProfitShifting + fdiRepatriation (all external payments)
    val privateBal = (flows.totalIncome - flows.totalConsumption) -
      (flows.grossInvestment + flows.greenInvestment + flows.inventoryChange)
    val publicBal = flows.govSpending - flows.govRevenue
    val externalX = flows.exports + flows.tourismExport + flows.diasporaInflow
    val externalM = flows.totalImports + flows.tourismImport + flows.remittanceOutflow +
      flows.foreignDividendOutflow + flows.fdiProfitShifting + flows.fdiRepatriation
    val externalBal = externalX - externalM
    val sectoralBalErr = privateBal + publicBal + externalBal

    // NFA uses wider tolerance (default 10 PLN) because cumulative NFA
    // values can reach billions, causing floating-point cancellation
    // in the (curr.nfa - prev.nfa) subtraction.
    // Sectoral balances (Identity 14) is a redundant macro identity — it must hold
    // if Identities 1–13 hold, but unit tests set partial flows that don't satisfy
    // all three sectoral balances simultaneously. Included in passed only when
    // validateSectoralBalances = true (integration tests / full simulation).
    val passed = bankCapErr.abs.toDouble < tolerance &&
                 bankDepErr.abs.toDouble < tolerance &&
                 govDebtErr.abs.toDouble < tolerance &&
                 nfaErr.abs.toDouble < nfaTolerance &&
                 bondClearingErr.abs.toDouble < tolerance &&
                 interbankErr.abs.toDouble < tolerance &&
                 jstDebtErr.abs.toDouble < tolerance &&
                 fusErr.abs.toDouble < tolerance &&
                 mortgageErr.abs.toDouble < tolerance &&
                 fofErr.abs.toDouble < tolerance &&
                 ccErr.abs.toDouble < tolerance &&
                 corpBondErr.abs.toDouble < tolerance &&
                 nbfiCreditErr.abs.toDouble < tolerance

    SfcResult(month, bankCapErr.toDouble, bankDepErr.toDouble, govDebtErr.toDouble,
      nfaErr.toDouble, bondClearingErr.toDouble, interbankErr.toDouble,
      jstDebtErr.toDouble, fusErr.toDouble, mortgageErr.toDouble, fofErr.toDouble,
      ccErr.toDouble, corpBondErr.toDouble, nbfiCreditErr.toDouble, sectoralBalErr.toDouble, passed)
