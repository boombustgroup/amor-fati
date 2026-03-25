package com.boombustgroup.amorfati.montecarlo

import com.boombustgroup.amorfati.*
import com.boombustgroup.amorfati.accounting.{InitCheck, Sfc}
import com.boombustgroup.amorfati.agents.Banking.BankState
import com.boombustgroup.amorfati.agents.Household
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.*
import com.boombustgroup.amorfati.init.WorldInit
import com.boombustgroup.amorfati.montecarlo.SimOutput.Col
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.amorfati.util.CsvWriter
import zio.stream.ZStream
import zio.{Clock, Console, ZIO}

import java.io.File
import java.util.concurrent.TimeUnit

/** Monte Carlo runner: simulation loop, per-seed CSV output. */
object McRunner:

  /** Single month snapshot emitted by [[seedStream]]. */
  private case class MonthSnapshot(month: Int, state: Simulation.SimState, monthData: Array[Double])

  /** Run N seeds in parallel, each streaming months via [[seedStream]]. Writes
    * per-seed CSV + aggregated HH/bank CSVs. Fails with [[SimError]].
    */
  def runZIO(rc: McRunConfig)(using p: SimParams): ZIO[Any, SimError, Unit] =
    val parallelism = java.lang.Runtime.getRuntime.availableProcessors()
    for
      _       <- ZIO.attemptBlocking { val d = new File("mc"); if !d.exists() then d.mkdirs() }.orDie
      _       <- Console.printLine(s"  run-id: ${rc.runId}").orDie
      t0      <- Clock.currentTime(TimeUnit.MILLISECONDS)
      results <- ZStream
        .fromIterable(1L to rc.nSeeds.toLong)
        .mapZIOPar(parallelism): seed =>
          for
            st        <- Clock.currentTime(TimeUnit.MILLISECONDS)
            runResult <- materializeSeed(seed, rc)
            et        <- Clock.currentTime(TimeUnit.MILLISECONDS)
            _         <- printSeedDone(seed, rc.nSeeds, runResult, et - st)
            _         <- writeSeedCsv(seed, rc, runResult).orDie
          yield (seed, runResult)
        .runCollect
      _       <- writeHhCsv(rc, results).orDie
      _       <- writeBankCsv(rc, results).orDie
      _       <- Console.printLine("").orDie
      _       <- printSavedZIO(rc).orDie
      t1      <- Clock.currentTime(TimeUnit.MILLISECONDS)
      _       <- Console.printLine(f"\nTotal time: ${(t1 - t0) / 1000.0}%.1f seconds").orDie
    yield ()

  /** Pure simulation — returns Either, no side effects. For tests. */
  def runSingle(seed: Long)(using p: SimParams): Either[SimError, RunResult] =
    initSeed(seed).flatMap(loop(_, seed, 0, Vector.empty))

  /** Streaming simulation — emits one [[MonthSnapshot]] per month. */
  private def seedStream(seed: Long)(using SimParams) =
    ZStream.unwrap(ZIO.fromEither(initSeed(seed)).map(simulateMonths(seed, _)))

  private def simulateMonths(seed: Long, initState: Simulation.SimState)(using p: SimParams) =
    ZStream.unfoldZIO((initState, 0)):
      case (_, month) if month >= p.timeline.duration => ZIO.none
      case (state, month)                             =>
        ZIO
          .fromEither(stepMonth(state, seed, month))
          .map: (newState, monthData) =>
            Some((MonthSnapshot(month + 1, newState, monthData), (newState, month + 1)))

  private def initSeed(seed: Long)(using p: SimParams) =
    val init     = WorldInit.initialize(seed)
    val snapshot = Sfc.snapshot(init.world, init.firms, init.households)
    val errors   = InitCheck.validate(snapshot, init.world.bankingSector, init.firms, init.households)
    if errors.nonEmpty then Left(SimError.Init(errors))
    else Right(Simulation.SimState(init.world, init.firms, init.households))

  private def stepMonth(state: Simulation.SimState, seed: Long, month: Int)(using p: SimParams) =
    val rng    = new scala.util.Random(seed * 10000 + month)
    val result = engine.flows.FlowSimulation.step(state.world, state.firms, state.households, rng)
    // SFC verification: flows through verified interpreter should always be 0L
    val wealth = com.boombustgroup.ledger.Interpreter.totalWealth(
      com.boombustgroup.ledger.Interpreter.applyAll(Map.empty[Int, Long], result.flows),
    )
    if wealth != 0L then
      val err = Sfc.SfcIdentityError(Sfc.SfcIdentity.FlowOfFunds, s"Flow SFC: totalWealth=$wealth", PLN.fromRaw(wealth), PLN.Zero)
      Left(SimError.SfcViolation(month + 1, Vector(err)))
    else
      val newState  = Simulation.SimState(result.newWorld, result.newFirms, result.newHouseholds)
      val monthData = SimOutput.compute(month, result.newWorld, result.newFirms, result.newHouseholds)
      Right((newState, monthData))

  @scala.annotation.tailrec
  private def loop(
      state: Simulation.SimState,
      seed: Long,
      month: Int,
      monthSeries: Vector[Array[Double]],
  )(using p: SimParams): Either[SimError, RunResult] =
    if month >= p.timeline.duration then Right(RunResult(TimeSeries.wrap(monthSeries.toArray), state))
    else
      stepMonth(state, seed, month) match
        case Left(err)                    => Left(err)
        case Right((newState, monthData)) =>
          loop(newState, seed, month + 1, monthSeries :+ monthData)

  /** Consume a seed stream into RunResult, collecting monthly data. */
  private def materializeSeed(seed: Long, rc: McRunConfig)(using p: SimParams) =
    seedStream(seed)
      .tap(s => ZIO.succeed(printMonthProgress(seed, rc.nSeeds, s.month, p.timeline.duration)))
      .runFold((Option.empty[Simulation.SimState], Vector.empty[Array[Double]])):
        case ((_, series), snapshot) => (Some(snapshot.state), series :+ snapshot.monthData)
      .flatMap:
        case (Some(state), series) => ZIO.succeed(RunResult(TimeSeries.wrap(series.toArray), state))
        case _                     => ZIO.fail(SimError.Init(Vector.empty))

  // ---------------------------------------------------------------------------
  //  Per-seed CSV writer
  // ---------------------------------------------------------------------------

  // $COVERAGE-OFF$ I/O: CSV writers, progress, banner
  private def filePrefix(rc: McRunConfig)(using p: SimParams) =
    s"${rc.outputPrefix}_${rc.runId}_${p.timeline.duration}m"

  private def seedFileName(seed: Long, rc: McRunConfig)(using SimParams) =
    f"${filePrefix(rc)}_seed${seed}%03d.csv"

  private def writeSeedCsv(seed: Long, rc: McRunConfig, result: RunResult)(using SimParams) =
    ZIO.attemptBlocking:
      val nCols    = SimOutput.nCols
      val colNames = SimOutput.colNames
      CsvWriter.write(
        new File("mc", seedFileName(seed, rc)),
        colNames.mkString(";"),
        0 until result.timeSeries.nMonths,
      ): t =>
        val row = result.timeSeries.monthRow(t)
        val sb  = new StringBuilder
        sb.append(f"${row(0)}%.0f")
        for c <- 1 until nCols do sb.append(f";${row(c)}%.6f")
        sb.toString

  // ---------------------------------------------------------------------------
  //  HH + Bank CSV writers (from collected results)
  // ---------------------------------------------------------------------------

  private val td = ComputationBoundary

  private val hhSchema: Vector[(String, Household.Aggregates => String)] = Vector(
    ("HH_Employed", a => s"${a.employed}"),
    ("HH_Unemployed", a => s"${a.unemployed}"),
    ("HH_Retraining", a => s"${a.retraining}"),
    ("HH_Bankrupt", a => s"${a.bankrupt}"),
    ("MeanSavings", a => f"${td.toDouble(a.meanSavings)}%.2f"),
    ("MedianSavings", a => f"${td.toDouble(a.medianSavings)}%.2f"),
    ("Gini_Individual", a => f"${td.toDouble(a.giniIndividual)}%.6f"),
    ("Gini_Wealth", a => f"${td.toDouble(a.giniWealth)}%.6f"),
    ("MeanSkill", a => f"${a.meanSkill}%.6f"),
    ("MeanHealthPenalty", a => f"${a.meanHealthPenalty}%.6f"),
    ("RetrainingAttempts", a => s"${a.retrainingAttempts}"),
    ("RetrainingSuccesses", a => s"${a.retrainingSuccesses}"),
    ("ConsumptionP10", a => f"${td.toDouble(a.consumptionP10)}%.2f"),
    ("ConsumptionP50", a => f"${td.toDouble(a.consumptionP50)}%.2f"),
    ("ConsumptionP90", a => f"${td.toDouble(a.consumptionP90)}%.2f"),
    ("BankruptcyRate", a => f"${td.toDouble(a.bankruptcyRate)}%.6f"),
    ("MeanMonthsToRuin", a => f"${a.meanMonthsToRuin}%.2f"),
    ("PovertyRate_50pct", a => f"${td.toDouble(a.povertyRate50)}%.6f"),
    ("PovertyRate_30pct", a => f"${td.toDouble(a.povertyRate30)}%.6f"),
  )

  private val hhHeader = "Seed;" + hhSchema.map(_._1).mkString(";")

  private def writeHhCsv(rc: McRunConfig, results: zio.Chunk[(Long, RunResult)])(using SimParams) =
    ZIO.attemptBlocking:
      val rows = results.map: (seed, r) =>
        val agg = r.terminalState.world.hhAgg
        s"$seed;" + hhSchema.map(_._2(agg)).mkString(";")
      CsvWriter.write(new File("mc", s"${filePrefix(rc)}_hh.csv"), hhHeader, rows)(identity)

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

  private val bankHeader = "Seed;" + bankSchema.map(_._1).mkString(";")

  private def writeBankCsv(rc: McRunConfig, results: zio.Chunk[(Long, RunResult)])(using SimParams) =
    ZIO.attemptBlocking:
      val rows = results.flatMap: (seed, r) =>
        r.terminalState.world.bankingSector.banks.map: b =>
          s"$seed;" + bankSchema.map(_._2(b)).mkString(";")
      CsvWriter.write(new File("mc", s"${filePrefix(rc)}_banks.csv"), bankHeader, rows)(identity)

  // ---------------------------------------------------------------------------
  //  Progress
  // ---------------------------------------------------------------------------

  private val BarWidth = 20

  private def printMonthProgress(seed: Long, total: Int, month: Int, duration: Int): Unit =
    val frac   = month.toDouble / duration
    val filled = (frac * BarWidth).toInt
    val bar    = "\u2588" * filled + "\u2591" * (BarWidth - filled)
    val pct    = (frac * 100).toInt
    print(f"\r  Seed $seed%3d/$total [$bar] $month%3d/${duration}m ($pct%3d%%)")

  private def printSeedDone(seed: Long, total: Int, result: RunResult, dt: Long) =
    val last  = result.timeSeries.lastMonth
    val adopt = last(Col.TotalAdoption.ordinal)
    val pi    = last(Col.Inflation.ordinal)
    val unemp = last(Col.Unemployment.ordinal)
    val bar   = "\u2588" * BarWidth
    Console
      .printLine(
        f"\r  Seed $seed%3d/$total [$bar] done (${dt}ms) | " +
          f"Adopt=${adopt * 100}%5.1f%% | pi=${pi * 100}%5.1f%% | " +
          f"Unemp=${unemp * 100}%5.1f%%",
      )
      .orDie

  private def printSavedZIO(rc: McRunConfig)(using SimParams) =
    val seedFiles = (1L to rc.nSeeds.toLong).map(s => s"mc/${seedFileName(s, rc)}")
    ZIO.foreachDiscard(seedFiles)(f => Console.printLine(s"Saved: $f")) *>
      Console.printLine(s"Saved: mc/${filePrefix(rc)}_hh.csv") *>
      Console.printLine(s"Saved: mc/${filePrefix(rc)}_banks.csv")

  // $COVERAGE-ON$
