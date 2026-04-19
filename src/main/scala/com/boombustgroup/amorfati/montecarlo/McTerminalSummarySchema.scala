package com.boombustgroup.amorfati.montecarlo

import com.boombustgroup.amorfati.agents.Banking
import com.boombustgroup.amorfati.agents.Banking.BankState
import com.boombustgroup.amorfati.agents.Household
import com.boombustgroup.amorfati.engine.flows.FlowSimulation
import com.boombustgroup.amorfati.engine.ledger.LedgerFinancialState
import com.boombustgroup.amorfati.fp.ComputationBoundary
import com.boombustgroup.amorfati.types.*

import java.io.File

private[montecarlo] enum McTerminalSummaryId:
  case Household, Banks

private[montecarlo] final case class McTerminalSummaryRows(seed: Long, rowsById: Map[McTerminalSummaryId, Vector[String]]):
  def rowsFor(id: McTerminalSummaryId): Vector[String] =
    rowsById.getOrElse(id, Vector.empty)

private[montecarlo] object McTerminalSummarySchema:

  private val td = ComputationBoundary

  private case class BankRow(bank: BankState, balances: LedgerFinancialState.BankBalances)

  private def nplRatio(row: BankRow): Share =
    Banking.nplRatio(row.balances.firmLoan, row.bank.nplAmount)

  private def car(row: BankRow): Multiplier =
    Banking.capitalAdequacyRatio(row.bank.capital, row.balances.firmLoan, row.balances.consumerLoan, row.balances.corpBond)

  private[montecarlo] final case class SummarySpec(
      id: McTerminalSummaryId,
      outputFile: (File, McRunConfig) => File,
      csvSchema: McCsvSchema[String],
  )

  private val hhSchema: Vector[(String, Household.Aggregates => String)] = Vector(
    ("HH_Employed", a => s"${a.employed}"),
    ("HH_Unemployed", a => s"${a.unemployed}"),
    ("HH_Retraining", a => s"${a.retraining}"),
    ("HH_Bankrupt", a => s"${a.bankrupt}"),
    ("MeanSavings", a => f"${td.toDouble(a.meanSavings)}%.2f"),
    ("MedianSavings", a => f"${td.toDouble(a.medianSavings)}%.2f"),
    ("Gini_Individual", a => f"${td.toDouble(a.giniIndividual)}%.6f"),
    ("Gini_Wealth", a => f"${td.toDouble(a.giniWealth)}%.6f"),
    ("MeanSkill", a => f"${td.toDouble(a.meanSkill)}%.6f"),
    ("MeanHealthPenalty", a => f"${td.toDouble(a.meanHealthPenalty)}%.6f"),
    ("RetrainingAttempts", a => s"${a.retrainingAttempts}"),
    ("RetrainingSuccesses", a => s"${a.retrainingSuccesses}"),
    ("ConsumptionP10", a => f"${td.toDouble(a.consumptionP10)}%.2f"),
    ("ConsumptionP50", a => f"${td.toDouble(a.consumptionP50)}%.2f"),
    ("ConsumptionP90", a => f"${td.toDouble(a.consumptionP90)}%.2f"),
    ("BankruptcyRate", a => f"${td.toDouble(a.bankruptcyRate)}%.6f"),
    ("MeanMonthsToRuin", a => f"${a.meanMonthsToRuin.toLong.toDouble / com.boombustgroup.amorfati.fp.FixedPointBase.ScaleD}%.2f"),
    ("PovertyRate_50pct", a => f"${td.toDouble(a.povertyRate50)}%.6f"),
    ("PovertyRate_30pct", a => f"${td.toDouble(a.povertyRate30)}%.6f"),
  )

  private val bankSchema: Vector[(String, BankRow => String)] = Vector(
    ("BankId", row => s"${row.bank.id}"),
    ("Deposits", row => f"${td.toDouble(row.balances.totalDeposits)}%.2f"),
    ("Loans", row => f"${td.toDouble(row.balances.firmLoan)}%.2f"),
    ("Capital", row => f"${td.toDouble(row.bank.capital)}%.2f"),
    ("NPL", row => f"${td.toDouble(nplRatio(row))}%.6f"),
    ("CAR", row => f"${td.toDouble(car(row))}%.6f"),
    ("GovBonds", row => f"${td.toDouble(row.balances.govBondAfs + row.balances.govBondHtm)}%.2f"),
    ("InterbankNet", row => f"${td.toDouble(row.balances.interbankLoan)}%.2f"),
    ("Failed", row => s"${row.bank.failed}"),
  )

  private[montecarlo] val specs = Vector(
    // SummarySpec rows are pre-formatted by fromTerminalState, so McCsvSchema only
    // carries the header contract here and render is intentionally identity.
    SummarySpec(
      McTerminalSummaryId.Household,
      McOutputFiles.householdFile,
      McCsvSchema(
        header = "Seed;" + hhSchema.map(_._1).mkString(";"),
        render = identity,
      ),
    ),
    SummarySpec(
      McTerminalSummaryId.Banks,
      McOutputFiles.bankFile,
      McCsvSchema(
        header = "Seed;" + bankSchema.map(_._1).mkString(";"),
        render = identity,
      ),
    ),
  )

  def fromTerminalState(seed: Long, terminalState: FlowSimulation.SimState): McTerminalSummaryRows =
    McTerminalSummaryRows(
      seed,
      Map(
        McTerminalSummaryId.Household -> Vector(renderHouseholdRow(seed, terminalState.householdAggregates)),
        McTerminalSummaryId.Banks     -> terminalState.banks.map(bank =>
          renderBankRow(seed, BankRow(bank, terminalState.ledgerFinancialState.banks(bank.id.toInt))),
        ),
      ),
    )

  private def renderHouseholdRow(seed: Long, agg: Household.Aggregates): String =
    s"$seed;" + hhSchema.map(_._2(agg)).mkString(";")

  private def renderBankRow(seed: Long, row: BankRow): String =
    s"$seed;" + bankSchema.map(_._2(row)).mkString(";")
