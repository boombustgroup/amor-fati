package com.boombustgroup.amorfati.montecarlo

import com.boombustgroup.amorfati.agents.Banking.BankState
import com.boombustgroup.amorfati.agents.Household
import com.boombustgroup.amorfati.engine.flows.FlowSimulation
import com.boombustgroup.amorfati.fp.ComputationBoundary

import java.io.File

private[montecarlo] enum McTerminalSummaryId:
  case Household, Banks

private[montecarlo] final case class McTerminalSummaryRows(seed: Long, rowsById: Map[McTerminalSummaryId, Vector[String]]):
  def rowsFor(id: McTerminalSummaryId): Vector[String] =
    rowsById.getOrElse(id, Vector.empty)

private[montecarlo] object McTerminalSummarySchema:

  private val td = ComputationBoundary

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

  private val bankSchema: Vector[(String, BankState => String)] = Vector(
    ("BankId", b => s"${b.id}"),
    ("Deposits", b => f"${td.toDouble(b.deposits)}%.2f"),
    ("Loans", b => f"${td.toDouble(b.loans)}%.2f"),
    ("Capital", b => f"${td.toDouble(b.capital)}%.2f"),
    ("NPL", b => f"${td.toDouble(b.nplRatio)}%.6f"),
    ("CAR", b => f"${td.toDouble(b.car)}%.6f"),
    ("GovBonds", b => f"${td.toDouble(b.govBondHoldings)}%.2f"),
    ("InterbankNet", b => f"${td.toDouble(b.interbankNet)}%.2f"),
    ("Failed", b => s"${b.failed}"),
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
        McTerminalSummaryId.Banks     -> terminalState.banks.map(renderBankRow(seed, _)),
      ),
    )

  private def renderHouseholdRow(seed: Long, agg: Household.Aggregates): String =
    s"$seed;" + hhSchema.map(_._2(agg)).mkString(";")

  private def renderBankRow(seed: Long, bank: BankState): String =
    s"$seed;" + bankSchema.map(_._2(bank)).mkString(";")
