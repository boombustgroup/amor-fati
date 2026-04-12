package com.boombustgroup.amorfati.montecarlo

import com.boombustgroup.amorfati.*
import com.boombustgroup.amorfati.accounting.{InitCheck, Sfc}
import com.boombustgroup.amorfati.agents.Banking.BankState
import com.boombustgroup.amorfati.agents.Household
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.*
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.flows.FlowSimulation
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
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
  private case class MonthSnapshot(executionMonth: ExecutionMonth, state: FlowSimulation.SimState, monthData: Array[Double])

  /** Run N seeds in parallel, each streaming months via [[seedStream]]. Writes
    * per-seed CSV + aggregated HH/bank CSVs. Fails with [[SimError]].
    */
  def runZIO(rc: McRunConfig)(using p: SimParams): ZIO[Any, SimError, Unit] =
    runZIO(rc, new File("mc"))

  private[amorfati] def runZIO(rc: McRunConfig, outputDir: File)(using p: SimParams): ZIO[Any, SimError, Unit] =
    val parallelism = java.lang.Runtime.getRuntime.availableProcessors()
    for
      _       <- prepareOutputDir(outputDir)
      _       <- Console.printLine(s"  run-id: ${rc.runId}").mapError(runtimeFailure("print run id"))
      t0      <- Clock.currentTime(TimeUnit.MILLISECONDS)
      results <- ZStream
        .fromIterable(1L to rc.nSeeds.toLong)
        .mapZIOPar(parallelism): seed =>
          for
            st        <- Clock.currentTime(TimeUnit.MILLISECONDS)
            runResult <- materializeSeed(seed, rc)
            et        <- Clock.currentTime(TimeUnit.MILLISECONDS)
            _         <- printSeedDone(seed, rc.nSeeds, runResult, et - st)
            _         <- writeSeedCsv(seed, rc, outputDir, runResult)
          yield (seed, runResult)
        .runCollect
      _       <- writeHhCsv(rc, outputDir, results)
      _       <- writeBankCsv(rc, outputDir, results)
      _       <- Console.printLine("").mapError(runtimeFailure("print separator"))
      _       <- printSavedZIO(rc, outputDir)
      t1      <- Clock.currentTime(TimeUnit.MILLISECONDS)
      _       <- Console.printLine(f"\nTotal time: ${(t1 - t0) / 1000.0}%.1f seconds").mapError(runtimeFailure("print total time"))
    yield ()

  /** Pure simulation — returns Either, no side effects. For tests. */
  def runSingle(seed: Long, durationMonths: Int = McRunConfig.DefaultRunDuration)(using p: SimParams): Either[SimError, RunResult] =
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
        val monthData = SimOutput.compute(
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

  /** Consume a seed stream into RunResult, collecting monthly data. */
  private def materializeSeed(seed: Long, rc: McRunConfig)(using p: SimParams) =
    seedStream(seed, rc.runDurationMonths)
      .tap(s => printMonthProgressZIO(seed, rc.nSeeds, s.executionMonth, rc.runDurationMonths))
      .runFold((Option.empty[FlowSimulation.SimState], Vector.empty[Array[Double]])):
        case ((_, series), snapshot) => (Some(snapshot.state), series :+ snapshot.monthData)
      .flatMap:
        case (Some(state), series) => ZIO.succeed(RunResult(TimeSeries.wrap(series.toArray), state))
        case _                     => ZIO.fail(SimError.Init(Vector.empty))

  // ---------------------------------------------------------------------------
  //  Per-seed CSV writer
  // ---------------------------------------------------------------------------

  // $COVERAGE-OFF$ I/O: CSV writers, progress, banner
  private def runtimeFailure(operation: String)(err: Throwable): SimError =
    SimError.RuntimeFailure(operation, Option(err.getMessage).filter(_.nonEmpty).getOrElse(err.getClass.getSimpleName))

  private def outputFailure(operation: String, path: File)(err: Throwable): SimError =
    SimError.OutputFailure(operation, path.getPath, Option(err.getMessage).filter(_.nonEmpty).getOrElse(err.getClass.getSimpleName))

  private def prepareOutputDir(outputDir: File): ZIO[Any, SimError, Unit] =
    ZIO
      .attemptBlocking:
        if outputDir.exists() then
          if !outputDir.isDirectory then throw java.io.IOException(s"path exists but is not a directory: ${outputDir.getPath}")
        else if !outputDir.mkdirs() then throw java.io.IOException(s"failed to create output directory: ${outputDir.getPath}")
      .mapError(outputFailure("prepare output directory", outputDir))

  private def filePrefix(rc: McRunConfig) =
    s"${rc.outputPrefix}_${rc.runId}_${rc.runDurationMonths}m"

  private def seedFileName(seed: Long, rc: McRunConfig) =
    f"${filePrefix(rc)}_seed${seed}%03d.csv"

  private def writeSeedCsv(seed: Long, rc: McRunConfig, outputDir: File, result: RunResult) =
    val outputFile = new File(outputDir, seedFileName(seed, rc))
    ZIO
      .attemptBlocking:
        val nCols    = SimOutput.nCols
        val colNames = SimOutput.colNames
        CsvWriter.write(
          outputFile,
          colNames.mkString(";"),
          result.timeSeries.executionMonths,
        ): month =>
          val row = result.timeSeries.monthRow(month)
          val sb  = new StringBuilder
          sb.append(month.toInt)
          for c <- 1 until nCols do sb.append(f";${row(c)}%.6f")
          sb.toString
      .mapError(outputFailure("write per-seed CSV", outputFile))

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

  private val hhHeader = "Seed;" + hhSchema.map(_._1).mkString(";")

  private def writeHhCsv(rc: McRunConfig, outputDir: File, results: zio.Chunk[(Long, RunResult)]) =
    val outputFile = new File(outputDir, s"${filePrefix(rc)}_hh.csv")
    ZIO
      .attemptBlocking:
        val rows = results.map: (seed, r) =>
          val agg = r.terminalState.householdAggregates
          s"$seed;" + hhSchema.map(_._2(agg)).mkString(";")
        CsvWriter.write(outputFile, hhHeader, rows)(identity)
      .mapError(outputFailure("write household summary CSV", outputFile))

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

  private def writeBankCsv(rc: McRunConfig, outputDir: File, results: zio.Chunk[(Long, RunResult)]) =
    val outputFile = new File(outputDir, s"${filePrefix(rc)}_banks.csv")
    ZIO
      .attemptBlocking:
        val rows = results.flatMap: (seed, r) =>
          r.terminalState.banks.map: b =>
            s"$seed;" + bankSchema.map(_._2(b)).mkString(";")
        CsvWriter.write(outputFile, bankHeader, rows)(identity)
      .mapError(outputFailure("write bank summary CSV", outputFile))

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
      .mapError(runtimeFailure("print seed summary"))

  private def printSavedZIO(rc: McRunConfig, outputDir: File) =
    val seedFiles = (1L to rc.nSeeds.toLong).map(s => new File(outputDir, seedFileName(s, rc)).getPath)
    (ZIO.foreachDiscard(seedFiles)(f => Console.printLine(s"Saved: $f")) *>
      Console.printLine(s"Saved: ${new File(outputDir, s"${filePrefix(rc)}_hh.csv").getPath}") *>
      Console.printLine(s"Saved: ${new File(outputDir, s"${filePrefix(rc)}_banks.csv").getPath}"))
      .mapError(runtimeFailure("print saved file paths"))

  // $COVERAGE-ON$
