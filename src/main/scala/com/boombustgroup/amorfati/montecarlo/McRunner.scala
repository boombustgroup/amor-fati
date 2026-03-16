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
import com.boombustgroup.amorfati.util.{BuildInfo, CsvWriter}
import zio.stream.ZStream
import zio.{Clock, Console, Ref, Task, UIO, ZIO}

import java.io.File
import java.util.concurrent.TimeUnit

/** Monte Carlo runner: simulation loop, per-seed CSV output. */
object McRunner:

  /** Run one simulation with given seed. Throws
    * [[InitCheck.InitValidationException]] on init stock inconsistency, or
    * [[Sfc.SfcViolationException]] on any monthly SFC identity violation.
    */
  private val NoProgress: Int => Unit = _ => ()

  def runSingle(seed: Long, @scala.annotation.unused rc: McRunConfig, onMonth: Int => Unit = NoProgress)(using
      p: SimParams,
  ): RunResult =
    val init     = WorldInit.initialize(seed)
    val snapshot = Sfc.snapshot(init.world, init.firms, init.households)

    val initErrs = InitCheck.validate(snapshot, init.world.bankingSector, init.firms, init.households)
    if initErrs.nonEmpty then throw InitCheck.InitValidationException(initErrs)

    var state = Simulation.SimState(init.world, init.firms, init.households)

    val results = Array.ofDim[Double](p.timeline.duration, SimOutput.nCols)

    for t <- 0 until p.timeline.duration do
      val stepResult = Simulation.step(state, seed, t)
      stepResult.sfcCheck match
        case Left(errors) => throw Sfc.SfcViolationException(t + 1, errors)
        case Right(())    => // OK
      state = stepResult.state
      results(t) = SimOutput.compute(t, state.world, state.firms, state.households)
      onMonth(t + 1)

    RunResult(TimeSeries.wrap(results), state)

  // ---------------------------------------------------------------------------
  //  ZIO entry point — parallel seeds, each writes own CSV
  // ---------------------------------------------------------------------------

  private def runSingleZIO(seed: Long, rc: McRunConfig, onMonth: Int => Unit)(using SimParams): Task[RunResult] =
    ZIO.attemptBlocking(runSingle(seed, rc, onMonth))

  def runZIO(rc: McRunConfig)(using p: SimParams): Task[Unit] =
    val parallelism = java.lang.Runtime.getRuntime.availableProcessors()
    val duration    = p.timeline.duration
    for
      _       <- printBannerZIO(rc)
      _       <- ZIO.attemptBlocking { val d = new File("mc"); if !d.exists() then d.mkdirs() }
      hhRef   <- Ref.make(Vector.empty[String])
      bankRef <- Ref.make(Vector.empty[String])
      _       <- Console.printLine(s"  run-id: ${rc.runId}")
      t0      <- Clock.currentTime(TimeUnit.MILLISECONDS)
      _       <- ZStream
        .fromIterable(1L to rc.nSeeds.toLong)
        .mapZIOPar(parallelism): seed =>
          for
            st     <- Clock.currentTime(TimeUnit.MILLISECONDS)
            result <- runSingleZIO(seed, rc, monthProgressBar(seed, rc.nSeeds, duration))
            et     <- Clock.currentTime(TimeUnit.MILLISECONDS)
            _      <- printSeedDone(seed, rc.nSeeds, result, et - st)
            _      <- writeSeedCsv(seed, rc, result)
            _      <- collectHhRow(seed, result, hhRef)
            _      <- collectBankRows(seed, result, bankRef)
          yield ()
        .runDrain
      _       <- flushHhCsv(rc, hhRef)
      _       <- flushBankCsv(rc, bankRef)
      _       <- Console.printLine("")
      _       <- printSavedZIO(rc)
      t1      <- Clock.currentTime(TimeUnit.MILLISECONDS)
      _       <- Console.printLine(f"\nTotal time: ${(t1 - t0) / 1000.0}%.1f seconds")
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
  //  Progress + Banner
  // ---------------------------------------------------------------------------

  private val BarWidth = 20

  private def monthProgressBar(seed: Long, total: Int, duration: Int): Int => Unit =
    month =>
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

  private[montecarlo] def prepareBanner(rc: McRunConfig)(using p: SimParams): String =
    val commit   = BuildInfo.gitCommit
    val firms    = String.format(java.util.Locale.US, "%,d", p.pop.firmsCount: Integer)
    val asciiArt = com.github.lalyos.jfiglet.FigletFont.convertOneLine("AMOR-FATI").stripTrailing()
    s"""
       |$asciiArt
       |
       |  SFC-ABM  |  commit: $commit
       |
       |  N=${rc.nSeeds} seeds  |  PLN (NBP)  |  HH=${p.household.count}  |  BANK=multi (7)
       |  $firms firms x 6 sectors (GUS 2024) x ${p.topology.label} x ${p.timeline.duration}m
       |
       |  Apache 2.0 | Copyright 2026 BoomBustGroup | www.boombustgroup.com
       |""".stripMargin

  private def printBannerZIO(rc: McRunConfig)(using SimParams): Task[Unit] =
    Console.printLine(prepareBanner(rc))
  // $COVERAGE-ON$
