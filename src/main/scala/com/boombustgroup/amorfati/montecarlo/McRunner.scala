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

import java.io.BufferedWriter
import java.io.File
import java.nio.file.{Files, StandardCopyOption}
import java.util.concurrent.TimeUnit

/** Monte Carlo runner: simulation loop, per-seed CSV output. */
object McRunner:

  /** Single month snapshot emitted by [[seedStream]]. */
  private case class MonthSnapshot(executionMonth: ExecutionMonth, state: FlowSimulation.SimState, monthData: Array[Double])
  private enum TerminalSummaryCsv:
    case Household, Banks

  private case class SeedTerminalOutputs(seed: Long, rowsByCsv: Map[TerminalSummaryCsv, Vector[String]]):
    def rowsFor(csv: TerminalSummaryCsv): Vector[String] =
      rowsByCsv.getOrElse(csv, Vector.empty)

  private case class SeedTerminalSnapshot(lastMonthData: Array[Double], terminalState: FlowSimulation.SimState)

  /** Run N seeds in parallel, each streaming months via [[seedStream]]. Writes
    * per-seed CSV + aggregated HH/bank CSVs. Fails with [[SimError]].
    */
  def runZIO(rc: McRunConfig)(using p: SimParams): ZIO[Any, SimError, Unit] =
    runZIO(rc, new File("mc"))

  private[amorfati] def runZIO(rc: McRunConfig, outputDir: File)(using p: SimParams): ZIO[Any, SimError, Unit] =
    val parallelism = scala.math.max(1, scala.math.min(java.lang.Runtime.getRuntime.availableProcessors(), rc.nSeeds))
    for
      _         <- prepareOutputDir(outputDir)
      _         <- Console.printLine(s"  run-id: ${rc.runId}").mapError(runtimeFailure("print run id"))
      t0        <- Clock.currentTime(TimeUnit.MILLISECONDS)
      summaries <- ZStream
        .fromIterable(1L to rc.nSeeds.toLong)
        .mapZIOPar(parallelism): seed =>
          runSeed(seed, rc, outputDir)
        .runCollect
      _         <- writeHhCsv(rc, outputDir, summaries)
      _         <- writeBankCsv(rc, outputDir, summaries)
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

  /** Stream a seed directly to its CSV and retain only the terminal summary
    * slice needed for aggregate outputs.
    */
  private def runSeed(seed: Long, rc: McRunConfig, outputDir: File)(using p: SimParams): ZIO[Any, SimError, SeedTerminalOutputs] =
    val finalOutputFile = new File(outputDir, seedFileName(seed, rc))
    val tempOutputFile  = new File(outputDir, s"${seedFileName(seed, rc)}.tmp")
    val operation       = "write per-seed CSV"

    val writeSeedFile = for
      st       <- Clock.currentTime(TimeUnit.MILLISECONDS)
      terminal <- ZIO.scoped {
        for
          writer        <- openCsvWriter(tempOutputFile, operation)
          _             <- writeCsvLine(writer, seedCsvHeader, tempOutputFile, operation)
          maybeTerminal <- seedStream(seed, rc.runDurationMonths)
            .tap(snapshot => printMonthProgressZIO(seed, rc.nSeeds, snapshot.executionMonth, rc.runDurationMonths))
            .runFoldZIO(Option.empty[SeedTerminalSnapshot]): (_, snapshot) =>
              writeCsvLine(
                writer,
                renderSeedCsvRow(snapshot.executionMonth, snapshot.monthData),
                tempOutputFile,
                operation,
              ).as(Some(SeedTerminalSnapshot(snapshot.monthData, snapshot.state)))
          terminal      <- maybeTerminal match
            case Some(value) => ZIO.succeed(value)
            case None        =>
              ZIO.fail(
                SimError.RuntimeFailure(
                  "materialize seed",
                  s"seed $seed produced no monthly snapshots for duration ${rc.runDurationMonths}",
                ),
              )
        yield terminal
      }
      _        <- moveOutputFile(tempOutputFile, finalOutputFile, operation)
      et       <- Clock.currentTime(TimeUnit.MILLISECONDS)
      _        <- printSeedDone(seed, rc.nSeeds, terminal.lastMonthData, et - st)
    yield SeedTerminalOutputs(
      seed,
      Map(
        TerminalSummaryCsv.Household -> Vector(renderHouseholdSummaryRow(seed, terminal.terminalState.householdAggregates)),
        TerminalSummaryCsv.Banks     -> renderBankSummaryRows(seed, terminal.terminalState.banks),
      ),
    )

    writeSeedFile.onError(_ => deleteIfExists(tempOutputFile, operation).ignore)

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
        else if !outputDir.mkdirs() && (!outputDir.exists() || !outputDir.isDirectory) then
          throw java.io.IOException(s"failed to create output directory: ${outputDir.getPath}")
      .mapError(outputFailure("prepare output directory", outputDir))

  private def filePrefix(rc: McRunConfig) =
    s"${rc.outputPrefix}_${rc.runId}_${rc.runDurationMonths}m"

  private def seedFileName(seed: Long, rc: McRunConfig) =
    f"${filePrefix(rc)}_seed${seed}%03d.csv"

  private val seedCsvHeader = SimOutput.colNames.mkString(";")

  private def openCsvWriter(outputFile: File, operation: String): ZIO[zio.Scope, SimError, BufferedWriter] =
    ZIO.fromAutoCloseable(
      ZIO
        .attemptBlocking(Files.newBufferedWriter(outputFile.toPath))
        .mapError(outputFailure(s"open $operation writer", outputFile)),
    )

  private def writeCsvLine(writer: BufferedWriter, line: String, outputFile: File, operation: String): ZIO[Any, SimError, Unit] =
    ZIO
      .attemptBlocking:
        writer.write(line)
        writer.newLine()
      .mapError(outputFailure(operation, outputFile))

  private def moveOutputFile(tempFile: File, outputFile: File, operation: String): ZIO[Any, SimError, Unit] =
    ZIO
      .attemptBlocking:
        Files.move(tempFile.toPath, outputFile.toPath, StandardCopyOption.REPLACE_EXISTING)
      .unit
      .mapError(outputFailure(s"finalize $operation", outputFile))

  private def deleteIfExists(file: File, operation: String): ZIO[Any, SimError, Unit] =
    ZIO
      .attemptBlocking(Files.deleteIfExists(file.toPath))
      .unit
      .mapError(outputFailure(s"cleanup $operation temp file", file))

  private def renderSeedCsvRow(month: ExecutionMonth, row: Array[Double]) =
    val sb = new StringBuilder
    sb.append(month.toInt)
    for c <- 1 until SimOutput.nCols do sb.append(f";${row(c)}%.6f")
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

  private def renderHouseholdSummaryRow(seed: Long, agg: Household.Aggregates): String =
    s"$seed;" + hhSchema.map(_._2(agg)).mkString(";")

  private def writeHhCsv(rc: McRunConfig, outputDir: File, results: zio.Chunk[SeedTerminalOutputs]) =
    val outputFile = new File(outputDir, s"${filePrefix(rc)}_hh.csv")
    ZIO
      .attemptBlocking:
        val rows = results.sortBy(_.seed).flatMap(_.rowsFor(TerminalSummaryCsv.Household))
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

  private def renderBankSummaryRows(seed: Long, banks: Vector[BankState]): Vector[String] =
    banks.map: b =>
      s"$seed;" + bankSchema.map(_._2(b)).mkString(";")

  private def writeBankCsv(rc: McRunConfig, outputDir: File, results: zio.Chunk[SeedTerminalOutputs]) =
    val outputFile = new File(outputDir, s"${filePrefix(rc)}_banks.csv")
    ZIO
      .attemptBlocking:
        val rows = results.sortBy(_.seed).flatMap(_.rowsFor(TerminalSummaryCsv.Banks))
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
    val seedFiles = (1L to rc.nSeeds.toLong).map(s => new File(outputDir, seedFileName(s, rc)).getPath)
    (ZIO.foreachDiscard(seedFiles)(f => Console.printLine(s"Saved: $f")) *>
      Console.printLine(s"Saved: ${new File(outputDir, s"${filePrefix(rc)}_hh.csv").getPath}") *>
      Console.printLine(s"Saved: ${new File(outputDir, s"${filePrefix(rc)}_banks.csv").getPath}"))
      .mapError(runtimeFailure("print saved file paths"))

  // $COVERAGE-ON$
