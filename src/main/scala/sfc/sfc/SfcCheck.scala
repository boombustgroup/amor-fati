package sfc.sfc

import sfc.agents.{Firm, FirmOps, Household, HhStatus}
import sfc.engine.{World, BankingSectorState}

object SfcCheck:

  /** Snapshot of all monetary stocks in the economy at one point in time. */
  case class Snapshot(
    hhSavings: Double,
    hhDebt: Double,
    firmCash: Double,
    firmDebt: Double,
    bankCapital: Double,
    bankDeposits: Double,
    bankLoans: Double,
    govDebt: Double,
    nfa: Double = 0.0,
    bankBondHoldings: Double = 0.0,
    nbpBondHoldings: Double = 0.0,
    bondsOutstanding: Double = 0.0,
    interbankNetSum: Double = 0.0,
    jstDeposits: Double = 0.0,
    jstDebt: Double = 0.0,
    fusBalance: Double = 0.0,     // ZUS/FUS raw surplus/deficit
    ppkBondHoldings: Double = 0.0, // PPK government bond holdings
    mortgageStock: Double = 0.0   // Outstanding mortgage debt
  )

  /** Flows observed during a single month (computed in Simulation.step).
    * These must match the EXACT values used in balance sheet updates. */
  case class MonthlyFlows(
    govSpending: Double,
    govRevenue: Double,
    nplLoss: Double,
    interestIncome: Double,
    hhDebtService: Double,
    totalIncome: Double,
    totalConsumption: Double,
    newLoans: Double,
    nplRecovery: Double,
    currentAccount: Double = 0.0,
    valuationEffect: Double = 0.0,
    bankBondIncome: Double = 0.0,
    qePurchase: Double = 0.0,
    newBondIssuance: Double = 0.0,
    depositInterestPaid: Double = 0.0,
    reserveInterest: Double = 0.0,          // NBP pays on required reserves
    standingFacilityIncome: Double = 0.0,   // Deposit/lombard facility net
    interbankInterest: Double = 0.0,        // Interbank interest (net ≈ 0)
    jstDepositChange: Double = 0.0,         // JST deposit flow
    jstSpending: Double = 0.0,              // JST spending
    jstRevenue: Double = 0.0,              // JST revenue
    zusContributions: Double = 0.0,        // ZUS contributions
    zusPensionPayments: Double = 0.0,      // ZUS pension payments
    zusGovSubvention: Double = 0.0,        // ZUS gov subvention
    dividendIncome: Double = 0.0,          // net domestic dividends → HH deposits
    foreignDividendOutflow: Double = 0.0,  // foreign dividends → CA outflow
    dividendTax: Double = 0.0,            // Belka tax → gov revenue
    mortgageInterestIncome: Double = 0.0, // mortgage interest → bank capital
    mortgageNplLoss: Double = 0.0,        // mortgage NPL loss → bank capital
    mortgageOrigination: Double = 0.0,    // new mortgages issued
    mortgagePrincipalRepaid: Double = 0.0, // monthly principal repaid
    mortgageDefaultAmount: Double = 0.0   // gross mortgage defaults (before recovery)
  )

  /** Result of the SFC check: nine exact balance-sheet identity checks. */
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
    passed: Boolean
  )

  def snapshot(w: World, firms: Array[Firm],
               households: Option[Vector[Household]]): Snapshot =
    val hhS = households.map(_.map(_.savings).sum).getOrElse(0.0)
    val hhD = households.map(_.map(_.debt).sum).getOrElse(0.0)
    val ibNet = w.bankingSector.map(_.banks.map(_.interbankNet).sum).getOrElse(0.0)
    Snapshot(
      hhSavings = hhS,
      hhDebt = hhD,
      firmCash = firms.map(_.cash).sum,
      firmDebt = firms.map(_.debt).sum,
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
      mortgageStock = w.housing.mortgageStock
    )

  /** Validate nine exact balance-sheet identities.
    *
    * The model uses a demand multiplier (not direct flow-of-funds) for the
    * firm revenue channel, so the full monetary circuit cannot close exactly.
    * Instead we check identities that ARE exact by construction:
    *
    * 1. Bank capital:  Δ = -nplLoss - mortgageNplLoss + (interestIncome + hhDebtService + bankBondIncome + mortgageInterestIncome - depositInterestPaid + reserveInterest + standingFacilityIncome + interbankInterest) × 0.3
    * 2. Bank deposits: Δ = totalIncome - totalConsumption + jstDepositChange + dividendIncome - foreignDividendOutflow
    * 3. Gov debt:      Δ = govSpending - govRevenue  (govRevenue includes dividendTax + zusGovSubvention)
    * 4. NFA:           Δ = currentAccount + valuationEffect  (currentAccount includes -foreignDividendOutflow)
    * 5. Bond clearing: bankBondHoldings + nbpBondHoldings + ppkBondHoldings = bondsOutstanding
    * 6. Interbank netting: Σ interbankNet_i = 0 (trivially 0 in single-bank mode)
    * 7. JST debt:      Δ = jstSpending - jstRevenue (trivially 0 when JST disabled)
    * 8. FUS balance:   Δ = zusContributions - zusPensionPayments (trivially 0 when ZUS disabled)
    * 9. Mortgage stock: Δ = origination - principalRepaid - defaultAmount (trivially 0 when RE disabled)
    *
    * These catch: mis-routed flows (e.g. rent subtracted from HH but not added
    * to bank/consumption), refactoring errors in balance sheet updates, and
    * any new flow that modifies a stock without updating the counterpart. */
  def validate(month: Int, prev: Snapshot, curr: Snapshot,
               flows: MonthlyFlows, tolerance: Double = 1.0,
               nfaTolerance: Double = 10.0): SfcResult =

    // Identity 1: Bank capital (includes bond coupon income, mortgage interest income,
    // minus deposit interest paid, minus mortgage NPL loss,
    // plus reserve interest, standing facility income, interbank interest — all × 0.3 profit retention)
    val expectedBankCapChange = -flows.nplLoss - flows.mortgageNplLoss +
      (flows.interestIncome + flows.hhDebtService + flows.bankBondIncome
       + flows.mortgageInterestIncome - flows.depositInterestPaid
       + flows.reserveInterest + flows.standingFacilityIncome + flows.interbankInterest) * 0.3
    val actualBankCapChange = curr.bankCapital - prev.bankCapital
    val bankCapErr = actualBankCapChange - expectedBankCapChange

    // Identity 2: Bank deposits (+ JST deposit flows + dividend flows)
    val expectedDepChange = flows.totalIncome - flows.totalConsumption + flows.jstDepositChange +
      flows.dividendIncome - flows.foreignDividendOutflow
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
    // All outstanding bonds must be held by bank + NBP + PPK
    // When GovBondMarket=false: all bond fields = 0.0, trivially passes.
    val bondClearingErr = (curr.bankBondHoldings + curr.nbpBondHoldings + curr.ppkBondHoldings) - curr.bondsOutstanding

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

    // NFA uses wider tolerance (default 10 PLN) because cumulative NFA
    // values can reach billions, causing floating-point cancellation
    // in the (curr.nfa - prev.nfa) subtraction.
    val passed = Math.abs(bankCapErr) < tolerance &&
                 Math.abs(bankDepErr) < tolerance &&
                 Math.abs(govDebtErr) < tolerance &&
                 Math.abs(nfaErr) < nfaTolerance &&
                 Math.abs(bondClearingErr) < tolerance &&
                 Math.abs(interbankErr) < tolerance &&
                 Math.abs(jstDebtErr) < tolerance &&
                 Math.abs(fusErr) < tolerance &&
                 Math.abs(mortgageErr) < tolerance

    SfcResult(month, bankCapErr, bankDepErr, govDebtErr, nfaErr, bondClearingErr, interbankErr,
      jstDebtErr, fusErr, mortgageErr, passed)
