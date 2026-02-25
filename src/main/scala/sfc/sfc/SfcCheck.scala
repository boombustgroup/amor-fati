package sfc.sfc

import sfc.agents.{Firm, FirmOps, Household, HhStatus}
import sfc.engine.World

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
    govDebt: Double
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
    nplRecovery: Double
  )

  /** Result of the SFC check: three exact balance-sheet identity checks. */
  case class SfcResult(
    month: Int,
    bankCapitalError: Double,
    bankDepositsError: Double,
    govDebtError: Double,
    passed: Boolean
  )

  def snapshot(w: World, firms: Array[Firm],
               households: Option[Vector[Household]]): Snapshot =
    val hhS = households.map(_.map(_.savings).sum).getOrElse(0.0)
    val hhD = households.map(_.map(_.debt).sum).getOrElse(0.0)
    Snapshot(
      hhSavings = hhS,
      hhDebt = hhD,
      firmCash = firms.map(_.cash).sum,
      firmDebt = firms.map(_.debt).sum,
      bankCapital = w.bank.capital,
      bankDeposits = w.bank.deposits,
      bankLoans = w.bank.totalLoans,
      govDebt = w.gov.cumulativeDebt
    )

  /** Validate three exact balance-sheet identities.
    *
    * The model uses a demand multiplier (not direct flow-of-funds) for the
    * firm revenue channel, so the full monetary circuit cannot close exactly.
    * Instead we check identities that ARE exact by construction:
    *
    * 1. Bank capital:  Δ = -nplLoss + interestIncome × 0.3 + hhDebtService × 0.3
    * 2. Bank deposits: Δ = totalIncome - totalConsumption
    * 3. Gov debt:      Δ = govSpending - govRevenue
    *
    * These catch: mis-routed flows (e.g. rent subtracted from HH but not added
    * to bank/consumption), refactoring errors in balance sheet updates, and
    * any new flow that modifies a stock without updating the counterpart. */
  def validate(month: Int, prev: Snapshot, curr: Snapshot,
               flows: MonthlyFlows, tolerance: Double = 1.0): SfcResult =

    // Identity 1: Bank capital
    val expectedBankCapChange = -flows.nplLoss +
      flows.interestIncome * 0.3 + flows.hhDebtService * 0.3
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

    val passed = Math.abs(bankCapErr) < tolerance &&
                 Math.abs(bankDepErr) < tolerance &&
                 Math.abs(govDebtErr) < tolerance

    SfcResult(month, bankCapErr, bankDepErr, govDebtErr, passed)
