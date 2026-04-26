package com.boombustgroup.amorfati.montecarlo

import com.boombustgroup.amorfati.agents.Banking
import com.boombustgroup.amorfati.agents.Banking.BankState
import com.boombustgroup.amorfati.agents.Household
import com.boombustgroup.amorfati.engine.flows.FlowSimulation
import com.boombustgroup.amorfati.engine.ledger.LedgerFinancialState
import com.boombustgroup.amorfati.types.*

import java.io.File

private[montecarlo] enum McTerminalSummaryId:
  case Household, Banks

private[montecarlo] final case class McTerminalSummaryRows(seed: Long, rowsById: Map[McTerminalSummaryId, Vector[String]]):
  def rowsFor(id: McTerminalSummaryId): Vector[String] =
    rowsById.getOrElse(id, Vector.empty)

private[montecarlo] object McTerminalSummarySchema:

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
    ("MeanSavings", a => a.meanSavings.format(2)),
    ("MedianSavings", a => a.medianSavings.format(2)),
    ("Gini_Individual", a => a.giniIndividual.format(6)),
    ("Gini_Wealth", a => a.giniWealth.format(6)),
    ("MeanSkill", a => a.meanSkill.format(6)),
    ("MeanHealthPenalty", a => a.meanHealthPenalty.format(6)),
    ("RetrainingAttempts", a => s"${a.retrainingAttempts}"),
    ("RetrainingSuccesses", a => s"${a.retrainingSuccesses}"),
    ("ConsumptionP10", a => a.consumptionP10.format(2)),
    ("ConsumptionP50", a => a.consumptionP50.format(2)),
    ("ConsumptionP90", a => a.consumptionP90.format(2)),
    ("BankruptcyRate", a => a.bankruptcyRate.format(6)),
    ("MeanMonthsToRuin", a => a.meanMonthsToRuin.format(2)),
    ("PovertyRate_50pct", a => a.povertyRate50.format(6)),
    ("PovertyRate_30pct", a => a.povertyRate30.format(6)),
  )

  private val bankSchema: Vector[(String, BankRow => String)] = Vector(
    ("BankId", row => s"${row.bank.id}"),
    ("Deposits", row => row.balances.totalDeposits.format(2)),
    ("Loans", row => row.balances.firmLoan.format(2)),
    ("Capital", row => row.bank.capital.format(2)),
    ("NPL", row => nplRatio(row).format(6)),
    ("CAR", row => car(row).format(6)),
    ("GovBonds", row => (row.balances.govBondAfs + row.balances.govBondHtm).format(2)),
    ("InterbankNet", row => row.balances.interbankLoan.format(2)),
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
