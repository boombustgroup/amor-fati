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
import zio.{Clock, Console, Ref, Task, UIO, ZIO}

import java.io.File
import java.util.concurrent.TimeUnit

/** Monte Carlo runner: simulation loop, per-seed CSV output. */
object McRunner:

  /** Single month snapshot emitted by [[seedStream]]. */
  private case class MonthSnapshot(month: Int, state: Simulation.SimState, monthData: Array[Double])

  /** Pure simulation — returns Either, no side effects. For tests. */
  def runSingle(seed: Long)(using p: SimParams): Either[SimError, RunResult] =
    initSeed(seed).flatMap(loop(_, seed, 0, Vector.empty))

  /** Streaming simulation — emits one [[MonthSnapshot]] per month. */
  private def seedStream(seed: Long)(using SimParams): ZStream[Any, SimError, MonthSnapshot] =
    ZStream.unwrap(ZIO.fromEither(initSeed(seed)).map(simulateMonths(seed, _)))

  private def simulateMonths(seed: Long, initState: Simulation.SimState)(using
      p: SimParams,
  ): ZStream[Any, SimError, MonthSnapshot] =
    ZStream.unfoldZIO((initState, 0)):
      case (_, month) if month >= p.timeline.duration => ZIO.none
      case (state, month)                             =>
        ZIO
          .fromEither(stepMonth(state, seed, month))
          .map: (newState, monthData) =>
            Some((MonthSnapshot(month + 1, newState, monthData), (newState, month + 1)))

  // -- Pure building blocks --

  private def initSeed(seed: Long)(using p: SimParams): Either[SimError.Init, Simulation.SimState] =
    val init     = WorldInit.initialize(seed)
    val snapshot = Sfc.snapshot(init.world, init.firms, init.households)
    val errors   = InitCheck.validate(snapshot, init.world.bankingSector, init.firms, init.households)
    if errors.nonEmpty then Left(SimError.Init(errors))
    else Right(Simulation.SimState(init.world, init.firms, init.households))

  private def stepMonth(
      state: Simulation.SimState,
      seed: Long,
      month: Int,
  )(using SimParams): Either[SimError.SfcViolation, (Simulation.SimState, Array[Double])] =
    val step = Simulation.step(state, seed, month)
    step.sfcCheck match
      case Left(errors) => Left(SimError.SfcViolation(month + 1, errors))
      case Right(())    =>
        val monthData = SimOutput.compute(month, step.state.world, step.state.firms, step.state.households)
        Right((step.state, monthData))

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

  // ---------------------------------------------------------------------------
  //  ZIO entry point — parallel seeds, each writes own CSV
  // ---------------------------------------------------------------------------

  /** Consume a seed stream into RunResult, collecting monthly monthData. */
  private def collectSeed(
      seed: Long,
      rc: McRunConfig,
  )(using p: SimParams): ZStream[Any, SimError, (Long, MonthSnapshot)] =
    seedStream(seed)
      .tap(s => ZIO.succeed(printMonthProgress(seed, rc.nSeeds, s.month, p.timeline.duration)))
      .map(s => (seed, s))

  def runZIO(rc: McRunConfig)(using p: SimParams): ZIO[Any, SimError, Unit] =
    val parallelism = java.lang.Runtime.getRuntime.availableProcessors()
    for
      _       <- ZIO.attemptBlocking { val d = new File("mc"); if !d.exists() then d.mkdirs() }.orDie
      hhRef   <- Ref.make(Vector.empty[String])
      bankRef <- Ref.make(Vector.empty[String])
      _       <- Console.printLine(s"  run-id: ${rc.runId}").orDie
      t0      <- Clock.currentTime(TimeUnit.MILLISECONDS)
      _       <- ZStream
        .fromIterable(1L to rc.nSeeds.toLong)
        .mapZIOPar(parallelism): seed =>
          for
            st <- Clock.currentTime(TimeUnit.MILLISECONDS)
            _  <- collectSeed(seed, rc)
              .runFold(Option.empty[(Simulation.SimState, Vector[Array[Double]])]): (acc, pair) =>
                val (_, snapshot) = pair
                val series        = acc.map(_._2).getOrElse(Vector.empty)
                Some((snapshot.state, series :+ snapshot.monthData))
              .flatMap:
                case None                  => ZIO.fail(SimError.Init(Vector.empty))
                case Some((state, series)) =>
                  val runResult = RunResult(TimeSeries.wrap(series.toArray), state)
                  for
                    et <- Clock.currentTime(TimeUnit.MILLISECONDS)
                    _  <- printSeedDone(seed, rc.nSeeds, runResult, et - st)
                    _  <- writeSeedCsv(seed, rc, runResult).orDie
                    _  <- collectHhRow(seed, runResult, hhRef)
                    _  <- collectBankRows(seed, runResult, bankRef)
                  yield ()
          yield ()
        .runDrain
      _       <- flushHhCsv(rc, hhRef).orDie
      _       <- flushBankCsv(rc, bankRef).orDie
      _       <- Console.printLine("").orDie
      _       <- printSavedZIO(rc).orDie
      t1      <- Clock.currentTime(TimeUnit.MILLISECONDS)
      _       <- Console.printLine(f"\nTotal time: ${(t1 - t0) / 1000.0}%.1f seconds").orDie
    yield ()

  // ---------------------------------------------------------------------------
  //  Per-seed CSV writer
  // ---------------------------------------------------------------------------

  // $COVERAGE-OFF$ I/O: CSV writers, progress, banner
  private def filePrefix(rc: McRunConfig)(using p: SimParams): String =
    s"${rc.outputPrefix}_${rc.runId}_${p.timeline.duration}m"

  private def seedFileName(seed: Long, rc: McRunConfig)(using SimParams): String =
    f"${filePrefix(rc)}_seed${seed}%03d.csv"

  private def writeSeedCsv(seed: Long, rc: McRunConfig, result: RunResult)(using SimParams): Task[Unit] =
    ZIO.attemptBlocking:
      val nCols    = SimOutput.nCols
      val colNames = SimOutput.colNames
      CsvWriter.write(
        new File("mc", seedFileName(seed, rc)),
        colNames.mkString(";"),
        0 until result.timeSeries.nMonths,
      ) { t =>
        val row = result.timeSeries.monthRow(t)
        val sb  = new StringBuilder
        sb.append(f"${row(0)}%.0f")
        for c <- 1 until nCols do sb.append(f";${row(c)}%.6f")
        sb.toString
      }

  // ---------------------------------------------------------------------------
  //  HH + Bank row accumulators (Ref-based, flushed at end)
  // ---------------------------------------------------------------------------

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

  private def collectHhRow(seed: Long, result: RunResult, ref: Ref[Vector[String]]): UIO[Unit] =
    val agg = result.terminalState.world.hhAgg
    val row = s"$seed;" + hhSchema.map(_._2(agg)).mkString(";")
    ref.update(_ :+ row).unit

  private def flushHhCsv(rc: McRunConfig, ref: Ref[Vector[String]])(using SimParams): Task[Unit] =
    for
      rows <- ref.get
      _    <- ZIO.attemptBlocking:
        CsvWriter.write(
          new File("mc", s"${filePrefix(rc)}_hh.csv"),
          hhHeader,
          rows,
        )(identity)
    yield ()

  private val bankSchema: Vector[(String, BankState => String)] = Vector(
    ("BankId", b => s"${b.id}"),
    ("Deposits", b => f"${b.deposits.toDouble}%.2f"),
    ("Loans", b => f"${b.loans.toDouble}%.2f"),
    ("Capital", b => f"${b.capital.toDouble}%.2f"),
    ("NPL", b => f"${b.nplRatio.toDouble}%.6f"),
    ("CAR", b => f"${b.car.toDouble}%.6f"),
    ("GovBonds", b => f"${b.govBondHoldings.toDouble}%.2f"),
    ("InterbankNet", b => f"${b.interbankNet.toDouble}%.2f"),
    ("Failed", b => s"${b.failed}"),
  )

  private val bankHeader: String = "Seed;" + bankSchema.map(_._1).mkString(";")

  private def collectBankRows(seed: Long, result: RunResult, ref: Ref[Vector[String]]): UIO[Unit] =
    val banks = result.terminalState.world.bankingSector.banks
    val rows  = banks.map(b => s"$seed;" + bankSchema.map(_._2(b)).mkString(";"))
    ref.update(_ ++ rows).unit

  private def flushBankCsv(rc: McRunConfig, ref: Ref[Vector[String]])(using SimParams): Task[Unit] =
    for
      rows <- ref.get
      _    <- ZIO.attemptBlocking:
        CsvWriter.write(
          new File("mc", s"${filePrefix(rc)}_banks.csv"),
          bankHeader,
          rows,
        )(identity)
    yield ()

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

  private def printSeedDone(seed: Long, total: Int, result: RunResult, dt: Long): UIO[Unit] =
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

  private def printSavedZIO(rc: McRunConfig)(using SimParams): Task[Unit] =
    val seedFiles = (1L to rc.nSeeds.toLong).map(s => s"mc/${seedFileName(s, rc)}")
    ZIO.foreachDiscard(seedFiles)(f => Console.printLine(s"Saved: $f")) *>
      Console.printLine(s"Saved: mc/${filePrefix(rc)}_hh.csv") *>
      Console.printLine(s"Saved: mc/${filePrefix(rc)}_banks.csv")

  // $COVERAGE-ON$
