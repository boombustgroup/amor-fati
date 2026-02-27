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
    interbankNetSum: Double = 0.0
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
    depositInterestPaid: Double = 0.0
  )

  /** Result of the SFC check: six exact balance-sheet identity checks. */
  case class SfcResult(
    month: Int,
    bankCapitalError: Double,
    bankDepositsError: Double,
    govDebtError: Double,
    nfaError: Double = 0.0,
    bondClearingError: Double = 0.0,
    interbankNettingError: Double = 0.0,
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
      interbankNetSum = ibNet
    )

  /** Validate five exact balance-sheet identities.
    *
    * The model uses a demand multiplier (not direct flow-of-funds) for the
    * firm revenue channel, so the full monetary circuit cannot close exactly.
    * Instead we check identities that ARE exact by construction:
    *
    * 1. Bank capital:  Δ = -nplLoss + (interestIncome + hhDebtService + bankBondIncome - depositInterestPaid) × 0.3
    * 2. Bank deposits: Δ = totalIncome - totalConsumption
    * 3. Gov debt:      Δ = govSpending - govRevenue
    * 4. NFA:           Δ = currentAccount + valuationEffect
    * 5. Bond clearing: bankBondHoldings + nbpBondHoldings = bondsOutstanding
    * 6. Interbank netting: Σ interbankNet_i = 0 (trivially 0 in single-bank mode)
    *
    * These catch: mis-routed flows (e.g. rent subtracted from HH but not added
    * to bank/consumption), refactoring errors in balance sheet updates, and
    * any new flow that modifies a stock without updating the counterpart. */
  def validate(month: Int, prev: Snapshot, curr: Snapshot,
               flows: MonthlyFlows, tolerance: Double = 1.0,
               nfaTolerance: Double = 10.0): SfcResult =

    // Identity 1: Bank capital (includes bond coupon income, minus deposit interest paid)
    val expectedBankCapChange = -flows.nplLoss +
      (flows.interestIncome + flows.hhDebtService + flows.bankBondIncome
       - flows.depositInterestPaid) * 0.3
    val actualBankCapChange = curr.bankCapital - prev.bankCapital
    val bankCapErr = actualBankCapChange - expectedBankCapChange

    // Identity 2: Bank deposits
    val expectedDepChange = flows.totalIncome - flows.totalConsumption
    val actualDepChange = curr.bankDeposits - prev.bankDeposits
    val bankDepErr = actualDepChange - expectedDepChange

    // Identity 3: Government debt (deficit = spending - revenue)
    val expectedGovDebtChange = flows.govSpending - flows.govRevenue
    val actualGovDebtChange = curr.govDebt - prev.govDebt
    val govDebtErr = actualGovDebtChange - expectedGovDebtChange

    // Identity 4: NFA (Net Foreign Assets)
    // ΔNFA = currentAccount + valuationEffect
    // When OPEN_ECON=false: both sides = 0.0, trivially passes.
    val expectedNfaChange = flows.currentAccount + flows.valuationEffect
    val actualNfaChange = curr.nfa - prev.nfa
    val nfaErr = actualNfaChange - expectedNfaChange

    // Identity 5: Bond clearing
    // All outstanding bonds must be held by bank + NBP (no other holders in Phase 2)
    // When GovBondMarket=false: all bond fields = 0.0, trivially passes.
    val bondClearingErr = (curr.bankBondHoldings + curr.nbpBondHoldings) - curr.bondsOutstanding

    // Identity 6: Interbank netting
    // Sum of all banks' interbankNet positions must be zero (closed system).
    // Trivially 0 in single-bank mode (no bankingSector present).
    val interbankErr = curr.interbankNetSum

    // NFA uses wider tolerance (default 10 PLN) because cumulative NFA
    // values can reach billions, causing floating-point cancellation
    // in the (curr.nfa - prev.nfa) subtraction.
    val passed = Math.abs(bankCapErr) < tolerance &&
                 Math.abs(bankDepErr) < tolerance &&
                 Math.abs(govDebtErr) < tolerance &&
                 Math.abs(nfaErr) < nfaTolerance &&
                 Math.abs(bondClearingErr) < tolerance &&
                 Math.abs(interbankErr) < tolerance

    SfcResult(month, bankCapErr, bankDepErr, govDebtErr, nfaErr, bondClearingErr, interbankErr, passed)
