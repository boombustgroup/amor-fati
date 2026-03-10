package sfc

import sfc.SimOutput.Col
import sfc.accounting.Sfc
import sfc.agents.Banking.BankState
import sfc.agents.Household
import sfc.config.*
import sfc.engine.*
import sfc.init.WorldInit
import sfc.types.*
import sfc.util.CsvWriter
import sfc.util.KahanSum.*

import java.io.File

/** Monte Carlo runner: simulation loop, CSV writers, summary statistics. */
object McRunner:

  /** Run one simulation with given seed. Throws [[Sfc.SfcViolationException]]
    * on any SFC identity violation.
    */
  def runSingle(seed: Int, rc: RunConfig)(using p: SimParams): RunResult =
    val masterSeed = seed.toLong
    val init       = WorldInit.initialize(seed)
    var state      = Simulation.SimState(init.world, init.firms, init.households)

    val results = Array.ofDim[Double](p.timeline.duration, SimOutput.nCols)

    for t <- 0 until p.timeline.duration do
      val stepResult = Simulation.step(state, rc, masterSeed, t)
      stepResult.sfcCheck match
        case Left(errors) => throw Sfc.SfcViolationException(t + 1, errors)
        case Right(())    => // OK
      state = stepResult.state
      results(t) = SimOutput.compute(t, state.world, state.firms, state.households)

    RunResult(TimeSeries.wrap(results), state)

  // $COVERAGE-OFF$ I/O: CSV writers, progress, banner
  def run(rc: RunConfig)(using SimParams): Unit =
    printBanner(rc)
    val results = runAll(rc, defaultProgress(rc))
    val outDir  = new File("mc")
    if !outDir.exists() then outDir.mkdirs()
    writeResults(rc, results, outDir)
    println(s"Saved: mc/${rc.outputPrefix}_terminal.csv")
    println(s"Saved: mc/${rc.outputPrefix}_hh_terminal.csv")
    println(s"Saved: mc/${rc.outputPrefix}_banks_terminal.csv")
    println(s"Saved: mc/${rc.outputPrefix}_timeseries.csv")
    printSummary(rc, results)

  // -- Pure MC loop → immutable McResults --

  def runAll(rc: RunConfig, onProgress: (Int, RunResult, Long) => Unit)(using SimParams): McResults =
    val startTime = System.currentTimeMillis()
    val builder   = Vector.newBuilder[RunResult]
    builder.sizeHint(rc.nSeeds)

    for seed <- 1 to rc.nSeeds do
      val t0     = System.currentTimeMillis()
      val result = runSingle(seed, rc)
      builder += result
      onProgress(seed, result, System.currentTimeMillis() - t0)

    val totalTime = (System.currentTimeMillis() - startTime) / 1000.0
    println(f"\nTotal time: ${totalTime}%.1f seconds")
    McResults(builder.result())

  private def defaultProgress(rc: RunConfig): (Int, RunResult, Long) => Unit =
    (seed, result, dt) =>
      if seed <= 3 || seed % 10 == 0 || seed == rc.nSeeds then
        val last      = result.timeSeries.lastMonth
        val adoption  = last(Col.TotalAdoption.ordinal)
        val inflation = last(Col.Inflation.ordinal)
        val unemp     = last(Col.Unemployment.ordinal)
        println(
          f"  Seed $seed%3d/${rc.nSeeds} (${dt}ms) | " +
            f"Adopt=${adoption * 100}%5.1f%% | pi=${inflation * 100}%5.1f%% | " +
            f"Unemp=${unemp * 100}%5.1f%%",
        )

  // ---------------------------------------------------------------------------
  //  IO: write all CSV files
  // ---------------------------------------------------------------------------

  private def writeResults(rc: RunConfig, results: McResults, dir: File)(using SimParams): Unit =
    writeTerminalCsv(rc, results, dir)
    writeHhTerminalCsv(rc, results, dir)
    writeBankTerminalCsv(rc, results, dir)
    writeTimeseriesCsv(rc, results, dir)

  // -- Per-seed terminal values CSV --
  private def writeTerminalCsv(rc: RunConfig, results: McResults, dir: File): Unit =
    val nCols    = SimOutput.nCols
    val colNames = SimOutput.colNames
    CsvWriter.write(
      new File(dir, s"${rc.outputPrefix}_terminal.csv"),
      "Seed;" + colNames.drop(1).mkString(";"),
      results.runs.zipWithIndex,
    ) { case (run, idx) =>
      val last = run.timeSeries.lastMonth
      val sb   = new StringBuilder
      sb.append(idx + 1)
      for c <- 1 until nCols do sb.append(f";${last(c)}%.6f")
      sb.toString
    }

  // -- Household terminal CSV (schema-driven) --

  private val hhSchema: Vector[(String, Household.Aggregates => String)] = Vector(
    ("HH_Employed", a => s"${a.employed}"),
    ("HH_Unemployed", a => s"${a.unemployed}"),
    ("HH_Retraining", a => s"${a.retraining}"),
    ("HH_Bankrupt", a => s"${a.bankrupt}"),
    ("MeanSavings", a => f"${a.meanSavings.toDouble}%.2f"),
    ("MedianSavings", a => f"${a.medianSavings.toDouble}%.2f"),
    ("Gini_Individual", a => f"${a.giniIndividual.toDouble}%.6f"),
    ("Gini_Wealth", a => f"${a.giniWealth.toDouble}%.6f"),
    ("MeanSkill", a => f"${a.meanSkill}%.6f"),
    ("MeanHealthPenalty", a => f"${a.meanHealthPenalty}%.6f"),
    ("RetrainingAttempts", a => s"${a.retrainingAttempts}"),
    ("RetrainingSuccesses", a => s"${a.retrainingSuccesses}"),
    ("ConsumptionP10", a => f"${a.consumptionP10.toDouble}%.2f"),
    ("ConsumptionP50", a => f"${a.consumptionP50.toDouble}%.2f"),
    ("ConsumptionP90", a => f"${a.consumptionP90.toDouble}%.2f"),
    ("BankruptcyRate", a => f"${a.bankruptcyRate.toDouble}%.6f"),
    ("MeanMonthsToRuin", a => f"${a.meanMonthsToRuin}%.2f"),
    ("PovertyRate_50pct", a => f"${a.povertyRate50.toDouble}%.6f"),
    ("PovertyRate_30pct", a => f"${a.povertyRate30.toDouble}%.6f"),
  )

  private val hhHeader: String = "Seed;" + hhSchema.map(_._1).mkString(";")

  private def writeHhTerminalCsv(rc: RunConfig, results: McResults, dir: File): Unit =
    CsvWriter.write(
      new File(dir, s"${rc.outputPrefix}_hh_terminal.csv"),
      hhHeader,
      results.runs.zipWithIndex,
    ) { case (run, idx) =>
      val agg = run.terminalState.world.hhAgg
      s"${idx + 1};" + hhSchema.map(_._2(agg)).mkString(";")
    }

  // -- Bank terminal CSV (schema-driven) --

  private val bankSchema: Vector[(String, BankState => String)] = Vector(
    ("BankId", b => s"${b.id}"),
    ("Deposits", b => f"${b.deposits.toDouble}%.2f"),
    ("Loans", b => f"${b.loans.toDouble}%.2f"),
    ("Capital", b => f"${b.capital.toDouble}%.2f"),
    ("NPL", b => f"${b.nplRatio}%.6f"),
    ("CAR", b => f"${b.car}%.6f"),
    ("GovBonds", b => f"${b.govBondHoldings.toDouble}%.2f"),
    ("InterbankNet", b => f"${b.interbankNet.toDouble}%.2f"),
    ("Failed", b => s"${b.failed}"),
  )

  private val bankHeader: String = "Seed;" + bankSchema.map(_._1).mkString(";")

  private def writeBankTerminalCsv(rc: RunConfig, results: McResults, dir: File): Unit =
    val rows = for
      (run, idx) <- results.runs.zipWithIndex
      b          <- run.terminalState.world.bankingSector.banks
    yield (idx, b)
    CsvWriter.write(
      new File(dir, s"${rc.outputPrefix}_banks_terminal.csv"),
      bankHeader,
      rows,
    ) { case (idx, b) =>
      s"${idx + 1};" + bankSchema.map(_._2(b)).mkString(";")
    }

  // -- Aggregated time-series (mean, std, p05, p95) via batch API --
  private def writeTimeseriesCsv(rc: RunConfig, results: McResults, dir: File)(using p: SimParams): Unit =
    val nMonths     = p.timeline.duration
    val nCols       = SimOutput.nCols
    val colNames    = SimOutput.colNames
    val headerParts =
      (1 until nCols).map(c => s"${colNames(c)}_mean;${colNames(c)}_std;${colNames(c)}_p05;${colNames(c)}_p95")
    CsvWriter.write(
      new File(dir, s"${rc.outputPrefix}_timeseries.csv"),
      "Month;" + headerParts.mkString(";"),
      0 until nMonths,
    ) { t =>
      val allStats = results.crossSeedStatsAll(t, nCols)
      val sb       = new StringBuilder
      sb.append(t + 1)
      for c <- 1 until nCols do
        val ds = allStats(c)
        sb.append(f";${ds.mean}%.6f;${ds.std}%.6f;${ds.p05}%.6f;${ds.p95}%.6f")
      sb.toString
    }

  // ---------------------------------------------------------------------------
  //  Summary statistics
  // ---------------------------------------------------------------------------

  private def printSummary(rc: RunConfig, results: McResults)(using SimParams): Unit =
    println("\n" + "=" * 54)
    println(s"MONTE CARLO SUMMARY: ${rc.outputPrefix} (N=${rc.nSeeds})")
    println("=" * 54)

    def statsSummary(name: String, col: Col, mult: Double = 1.0): Unit =
      val vals = results.terminalValues(col)
      val ds   =
        if mult == 1.0 then DescriptiveStats.fromSorted(vals)
        else DescriptiveStats.from(vals.map(_ * mult))
      println(f"  $name%-25s mean=${ds.mean}%8.2f +/- ${ds.std}%6.2f  [${ds.p05}%8.2f, ${ds.p95}%8.2f]")

    statsSummary("Inflation (%)", Col.Inflation, 100.0)
    statsSummary("Unemployment (%)", Col.Unemployment, 100.0)
    statsSummary("Total Adoption (%)", Col.TotalAdoption, 100.0)
    statsSummary("Exchange Rate", Col.ExRate)
    statsSummary("Market Wage (PLN)", Col.MarketWage)
    statsSummary("Gov Debt (mld PLN)", Col.GovDebt, 1.0 / 1e9)
    statsSummary("NPL Ratio (%)", Col.NPL, 100.0)

    // Household summary
    println("\nHousehold aggregates at M120:")
    val allHhAgg = results.runs.map(_.terminalState.world.hhAgg)
    if allHhAgg.nonEmpty then
      val avgGini   = allHhAgg.kahanSumBy(_.giniIndividual.toDouble) / allHhAgg.length
      val avgWealth = allHhAgg.kahanSumBy(_.giniWealth.toDouble) / allHhAgg.length
      val avgBankr  = allHhAgg.kahanSumBy(_.bankruptcyRate.toDouble) / allHhAgg.length
      val avgPov50  = allHhAgg.kahanSumBy(_.povertyRate50.toDouble) / allHhAgg.length
      val avgSkill  = allHhAgg.kahanSumBy(_.meanSkill) / allHhAgg.length
      println(f"  Gini (income)        mean=${avgGini * 100}%8.2f%%")
      println(f"  Gini (wealth)        mean=${avgWealth * 100}%8.2f%%")
      println(f"  Bankruptcy rate      mean=${avgBankr * 100}%8.2f%%")
      println(f"  Poverty rate (50%%)  mean=${avgPov50 * 100}%8.2f%%")
      println(f"  Mean skill           mean=${avgSkill}%8.4f")

    println("\nPer-sector adoption at M120:")
    val secNames = SectorDefs.map(_.name)
    for s <- SectorDefs.indices do statsSummary(f"  ${secNames(s)}%-22s", Col.sectorAuto(s), 100.0)

  private def printBanner(rc: RunConfig)(using p: SimParams): Unit =
    val topoLabel  = TOPOLOGY.toString.toUpperCase
    val firmsLabel = f"${p.pop.firmsCount}%,d"
    val hhLabel    = s" | HH=individual (${p.household.count})"
    val bankLabel  = " | BANK=multi (7)"
    println(s"+" + "=" * 68 + "+")
    println(s"|  SFC-ABM v8: N=${rc.nSeeds} seeds, PLN (NBP)${hhLabel}${bankLabel}")
    println(s"|  ${firmsLabel} firms x 6 sectors (GUS 2024) x ${topoLabel} network x 120m")
    println(s"+" + "=" * 68 + "+")
  // $COVERAGE-ON$
