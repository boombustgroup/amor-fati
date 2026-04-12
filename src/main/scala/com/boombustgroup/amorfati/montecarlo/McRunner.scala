package com.boombustgroup.amorfati.montecarlo

import com.boombustgroup.amorfati.*
import com.boombustgroup.amorfati.accounting.{InitCheck, Sfc}
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.*
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.flows.FlowSimulation
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import com.boombustgroup.amorfati.montecarlo.McTimeseriesSchema.Col
import zio.stream.ZStream
import zio.{Clock, Console, ZIO}

import java.io.File
import java.util.concurrent.TimeUnit

/** Monte Carlo runner: simulation loop, per-seed CSV output. */
object McRunner:

  /** Single month snapshot emitted by [[seedStream]]. */
  private case class MonthSnapshot(executionMonth: ExecutionMonth, state: FlowSimulation.SimState, monthData: Array[Double])
  private case class SeedTerminalSnapshot(executionMonth: ExecutionMonth, lastMonthData: Array[Double], terminalState: FlowSimulation.SimState)

  /** Run N seeds in parallel, each streaming months via [[seedStream]]. Writes
    * per-seed CSV + aggregated HH/bank CSVs. Fails with [[SimError]].
    */
  def runZIO(rc: McRunConfig)(using p: SimParams): ZIO[Any, SimError, Unit] =
    runZIO(rc, new File("mc"))

  private[amorfati] def runZIO(rc: McRunConfig, outputDir: File)(using p: SimParams): ZIO[Any, SimError, Unit] =
    val parallelism = scala.math.max(1, scala.math.min(java.lang.Runtime.getRuntime.availableProcessors(), rc.nSeeds))
    for
      _         <- McOutputFiles.prepareOutputDir(outputDir)
      _         <- Console.printLine(s"  run-id: ${rc.runId}").mapError(runtimeFailure("print run id"))
      t0        <- Clock.currentTime(TimeUnit.MILLISECONDS)
      summaries <- ZStream
        .fromIterable(1L to rc.nSeeds.toLong)
        .mapZIOPar(parallelism): seed =>
          runSeed(seed, rc, outputDir)
        .runCollect
      _         <- McTerminalSummaryCsv.writeAll(rc, outputDir, summaries)
      _         <- Console.printLine("").mapError(runtimeFailure("print separator"))
      _         <- printSavedZIO(rc, outputDir)
      t1        <- Clock.currentTime(TimeUnit.MILLISECONDS)
      _         <- Console.printLine(f"\nTotal time: ${(t1 - t0) / 1000.0}%.1f seconds").mapError(runtimeFailure("print total time"))
    yield ()

  /** Pure simulation — returns Either, no side effects. For tests. */
  def runSingle(seed: Long, durationMonths: Int = McRunConfig.DefaultRunDuration)(using p: SimParams): Either[SimError, RunResult] =
    McRunConfig.requirePositiveDuration(durationMonths)
    initSeed(seed).flatMap: initState =>
      materializeRun(initState, runtimeSteps(seed, initState).take(durationMonths))

  /** Streaming simulation — emits one [[MonthSnapshot]] per month. */
  private def seedStream(seed: Long, durationMonths: Int)(using SimParams) =
    ZStream.unwrap(ZIO.fromEither(initSeed(seed)).map(simulateMonths(seed, _, durationMonths)))

  private def simulateMonths(seed: Long, initState: FlowSimulation.SimState, durationMonths: Int)(using p: SimParams) =
    val steps = runtimeSteps(seed, initState).take(durationMonths)
    ZStream
      .unfold(steps): iterator =>
        if iterator.hasNext then Some((iterator.next(), iterator))
        else None
      .mapZIO(result => ZIO.fromEither(stepSnapshot(result)))

  private def initSeed(seed: Long)(using p: SimParams) =
    val init    = WorldInit.initialize(InitRandomness.Contract.fromSeed(seed))
    val runtime = Sfc.RuntimeState(init.world, init.firms, init.households, init.banks)
    val errors  = InitCheck.validate(runtime)
    if errors.nonEmpty then Left(SimError.Init(errors))
    else Right(FlowSimulation.SimState.fromInit(init))

  private def runtimeSteps(seed: Long, initState: FlowSimulation.SimState)(using p: SimParams): Iterator[FlowSimulation.StepOutput] =
    MonthDriver.unfoldSteps(initState): state =>
      Some(MonthRandomness.Contract.fromSeed(runtimeRootSeed(seed, state)))

  private def runtimeRootSeed(seed: Long, state: FlowSimulation.SimState): Long =
    // Preserve the runtime month-seed convention used before the shared driver.
    seed * 10000L + state.completedMonth.toLong

  private def stepSnapshot(result: FlowSimulation.StepOutput)(using p: SimParams): Either[SimError, MonthSnapshot] =
    result.sfcResult match
      case Left(errors) =>
        Left(SimError.SfcViolation(result.executionMonth, errors))
      case Right(())    =>
        val monthData = McTimeseriesSchema.compute(
          result.executionMonth,
          result.nextState.world,
          result.nextState.firms,
          result.nextState.households,
          result.nextState.banks,
          result.nextState.householdAggregates,
        )
        Right(MonthSnapshot(result.executionMonth, result.nextState, monthData))

  private def materializeRun(
      initState: FlowSimulation.SimState,
      steps: Iterator[FlowSimulation.StepOutput],
  )(using p: SimParams): Either[SimError, RunResult] =
    val monthSeries = Vector.newBuilder[Array[Double]]

    @scala.annotation.tailrec
    def collect(remaining: Iterator[FlowSimulation.StepOutput], terminal: FlowSimulation.SimState): Either[SimError, RunResult] =
      if !remaining.hasNext then Right(RunResult(TimeSeries.wrap(monthSeries.result().toArray), terminal))
      else
        stepSnapshot(remaining.next()) match
          case Left(err)       => Left(err)
          case Right(snapshot) =>
            monthSeries += snapshot.monthData
            collect(remaining, snapshot.state)

    collect(steps, initState)

  /** Stream a seed directly to its CSV and retain only the terminal summary
    * slice needed for aggregate outputs.
    */
  private def runSeed(seed: Long, rc: McRunConfig, outputDir: File)(using p: SimParams): ZIO[Any, SimError, McTerminalSummaryRows] =
    for
      st       <- Clock.currentTime(TimeUnit.MILLISECONDS)
      terminal <- McTimeseriesCsv.writeStreaming(
        McOutputFiles.seedFile(outputDir, seed, rc),
        seedStream(seed, rc.runDurationMonths)
          .tap(snapshot => printMonthProgressZIO(seed, rc.nSeeds, snapshot.executionMonth, rc.runDurationMonths))
          .map(snapshot => SeedTerminalSnapshot(snapshot.executionMonth, snapshot.monthData, snapshot.state)),
        McTimeseriesSchema.csvSchema.contramap(snapshot => (snapshot.executionMonth, snapshot.lastMonthData)),
        SimError.RuntimeFailure(
          "materialize seed",
          s"seed $seed produced no monthly snapshots for duration ${rc.runDurationMonths}",
        ),
      )
      et       <- Clock.currentTime(TimeUnit.MILLISECONDS)
      _        <- printSeedDone(seed, rc.nSeeds, terminal.lastMonthData, et - st)
    yield McTerminalSummarySchema.fromTerminalState(seed, terminal.terminalState)

  // $COVERAGE-OFF$
  // I/O: CSV writers, progress, banner
  private def runtimeFailure(operation: String)(err: Throwable): SimError =
    SimError.RuntimeFailure(operation, Option(err.getMessage).filter(_.nonEmpty).getOrElse(err.getClass.getSimpleName))

  // ---------------------------------------------------------------------------
  //  Progress
  // ---------------------------------------------------------------------------

  private val BarWidth = 20

  private def printMonthProgressZIO(seed: Long, total: Int, month: ExecutionMonth, duration: Int): ZIO[Any, SimError, Unit] =
    ZIO.attempt(printMonthProgress(seed, total, month, duration)).mapError(runtimeFailure("print month progress"))

  private def printMonthProgress(seed: Long, total: Int, month: ExecutionMonth, duration: Int): Unit =
    val frac   = month.toInt.toDouble / duration
    val filled = (frac * BarWidth).toInt
    val bar    = "\u2588" * filled + "\u2591" * (BarWidth - filled)
    val pct    = (frac * 100).toInt
    print(f"\r  Seed $seed%3d/$total [$bar] ${month.toInt}%3d/${duration}m ($pct%3d%%)")

  private def printSeedDone(seed: Long, total: Int, last: Array[Double], dt: Long) =
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
      .mapError(runtimeFailure("print seed summary"))

  private def printSavedZIO(rc: McRunConfig, outputDir: File) =
    ZIO
      .foreachDiscard(McOutputFiles.savedFiles(outputDir, rc))(file => Console.printLine(s"Saved: ${file.getPath}"))
      .mapError(runtimeFailure("print saved file paths"))

  // $COVERAGE-ON$
