package com.boombustgroup.amorfati.montecarlo

import com.boombustgroup.amorfati.agents.Banking.BankState
import com.boombustgroup.amorfati.agents.Household
import com.boombustgroup.amorfati.engine.flows.FlowSimulation
import com.boombustgroup.amorfati.fp.ComputationBoundary
import com.boombustgroup.amorfati.util.CsvWriter
import zio.ZIO

import java.io.File

private[montecarlo] enum TerminalSummaryCsv:
  case Household, Banks

private[montecarlo] case class SeedTerminalOutputs(seed: Long, rowsByCsv: Map[TerminalSummaryCsv, Vector[String]]):
  def rowsFor(csv: TerminalSummaryCsv): Vector[String] =
    rowsByCsv.getOrElse(csv, Vector.empty)

private[montecarlo] object McTerminalSummaryCsv:

  private val td = ComputationBoundary

  private case class SummarySpec(
      csv: TerminalSummaryCsv,
      outputFile: (File, McRunConfig) => File,
      header: String,
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

  private val specs = Vector(
    SummarySpec(TerminalSummaryCsv.Household, McOutputFiles.householdFile, "Seed;" + hhSchema.map(_._1).mkString(";")),
    SummarySpec(TerminalSummaryCsv.Banks, McOutputFiles.bankFile, "Seed;" + bankSchema.map(_._1).mkString(";")),
  )

  def fromTerminalState(seed: Long, terminalState: FlowSimulation.SimState): SeedTerminalOutputs =
    SeedTerminalOutputs(
      seed,
      Map(
        TerminalSummaryCsv.Household -> Vector(renderHouseholdRow(seed, terminalState.householdAggregates)),
        TerminalSummaryCsv.Banks     -> terminalState.banks.map(renderBankRow(seed, _)),
      ),
    )

  def writeAll(rc: McRunConfig, outputDir: File, results: zio.Chunk[SeedTerminalOutputs]): ZIO[Any, SimError, Unit] =
    ZIO.foreachDiscard(specs)(spec => writeSummaryCsv(spec, rc, outputDir, results))

  private def writeSummaryCsv(spec: SummarySpec, rc: McRunConfig, outputDir: File, results: zio.Chunk[SeedTerminalOutputs]) =
    val outputFile = spec.outputFile(outputDir, rc)
    ZIO
      .attemptBlocking:
        val rows = results.sortBy(_.seed).flatMap(_.rowsFor(spec.csv))
        CsvWriter.write(outputFile, spec.header, rows)(identity)
      .mapError(outputFailure(s"write ${spec.csv.toString.toLowerCase} summary CSV", outputFile))

  private def renderHouseholdRow(seed: Long, agg: Household.Aggregates): String =
    s"$seed;" + hhSchema.map(_._2(agg)).mkString(";")

  private def renderBankRow(seed: Long, bank: BankState): String =
    s"$seed;" + bankSchema.map(_._2(bank)).mkString(";")

  private def outputFailure(operation: String, path: File)(err: Throwable): SimError =
    SimError.OutputFailure(operation, path.getPath, Option(err.getMessage).filter(_.nonEmpty).getOrElse(err.getClass.getSimpleName))
