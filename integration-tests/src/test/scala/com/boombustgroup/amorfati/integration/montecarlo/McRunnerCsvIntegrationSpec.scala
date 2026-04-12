package com.boombustgroup.amorfati.integration.montecarlo

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.fp.{ComputationBoundary, FixedPointBase}
import com.boombustgroup.amorfati.montecarlo.{McRunConfig, McRunner, RunResult, SimError, SimOutput}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import zio.{Runtime, Unsafe, ZIO}

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.Using

class McRunnerCsvIntegrationSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  private val td                 = ComputationBoundary
  private val DurationMonths     = 3
  private val Seeds              = Vector(1L, 2L)
  private val OutputPrefix       = "mc-it"
  private val RunId              = "csvspec"
  private val ExpectedHhHeader   =
    "Seed;HH_Employed;HH_Unemployed;HH_Retraining;HH_Bankrupt;MeanSavings;MedianSavings;Gini_Individual;Gini_Wealth;MeanSkill;MeanHealthPenalty;RetrainingAttempts;RetrainingSuccesses;ConsumptionP10;ConsumptionP50;ConsumptionP90;BankruptcyRate;MeanMonthsToRuin;PovertyRate_50pct;PovertyRate_30pct"
  private val ExpectedBankHeader =
    "Seed;BankId;Deposits;Loans;Capital;NPL;CAR;GovBonds;InterbankNet;Failed"

  private def rc =
    McRunConfig(
      nSeeds = Seeds.length,
      outputPrefix = OutputPrefix,
      runDurationMonths = DurationMonths,
      runId = RunId,
    )

  private def filePrefix(rc: McRunConfig) =
    s"${rc.outputPrefix}_${rc.runId}_${rc.runDurationMonths}m"

  private def seedFileName(seed: Long, rc: McRunConfig) =
    f"${filePrefix(rc)}_seed${seed}%03d.csv"

  private def unsafeRun[A](zio: ZIO[Any, SimError, A]): A =
    Unsafe.unsafe:
      implicit unsafe =>
        Runtime.default.unsafe.run(zio.either).getOrThrowFiberFailure() match
          case Right(value) => value
          case Left(err)    => fail(err.toString)

  private def unsafeRunEither[A](zio: ZIO[Any, SimError, A]): Either[SimError, A] =
    Unsafe.unsafe:
      implicit unsafe =>
        Runtime.default.unsafe.run(zio.either).getOrThrowFiberFailure()

  private def withTempDir[A](f: Path => A): A =
    val dir = Files.createTempDirectory("mc-runner-it-")
    try f(dir)
    finally deleteRecursively(dir)

  private def deleteRecursively(dir: Path): Unit =
    if Files.exists(dir) then
      Using.resource(Files.walk(dir)): stream =>
        stream.iterator.asScala.toVector.sortBy(_.getNameCount).reverse.foreach(Files.deleteIfExists)

  private def readLines(path: Path): Vector[String] =
    Files.readAllLines(path).asScala.toVector

  private def parseCsvRow(line: String): Vector[Double] =
    line.split(';').toVector.map(_.replace(',', '.').toDouble)

  private def expectedRun(seed: Long): RunResult =
    McRunner.runSingle(seed, DurationMonths).fold(err => fail(err.toString), identity)

  private def expectedHhRow(seed: Long, result: RunResult): String =
    val a = result.terminalState.householdAggregates
    s"$seed;${a.employed};${a.unemployed};${a.retraining};${a.bankrupt};" +
      f"${td.toDouble(a.meanSavings)}%.2f;${td.toDouble(a.medianSavings)}%.2f;" +
      f"${td.toDouble(a.giniIndividual)}%.6f;${td.toDouble(a.giniWealth)}%.6f;" +
      f"${td.toDouble(a.meanSkill)}%.6f;${td.toDouble(a.meanHealthPenalty)}%.6f;" +
      s"${a.retrainingAttempts};${a.retrainingSuccesses};" +
      f"${td.toDouble(a.consumptionP10)}%.2f;${td.toDouble(a.consumptionP50)}%.2f;${td.toDouble(a.consumptionP90)}%.2f;" +
      f"${td.toDouble(a.bankruptcyRate)}%.6f;" +
      f"${a.meanMonthsToRuin.toLong.toDouble / FixedPointBase.ScaleD}%.2f;" +
      f"${td.toDouble(a.povertyRate50)}%.6f;${td.toDouble(a.povertyRate30)}%.6f"

  private def expectedBankRows(seed: Long, result: RunResult): Vector[String] =
    result.terminalState.banks.map: b =>
      s"$seed;${b.id};" +
        f"${td.toDouble(b.deposits)}%.2f;${td.toDouble(b.loans)}%.2f;${td.toDouble(b.capital)}%.2f;" +
        f"${td.toDouble(b.nplRatio)}%.6f;${td.toDouble(b.car)}%.6f;" +
        f"${td.toDouble(b.govBondHoldings)}%.2f;${td.toDouble(b.interbankNet)}%.2f;" +
        s"${b.failed}"

  "runZIO" should "write deterministic per-seed and summary CSV files" in withTempDir: outputDir =>
    unsafeRun(McRunner.runZIO(rc, outputDir.toFile))

    val expectedRuns      = Seeds.map(seed => seed -> expectedRun(seed)).toMap
    val expectedFileNames = (
      Seeds.map(seed => seedFileName(seed, rc)) :+
        s"${filePrefix(rc)}_hh.csv" :+
        s"${filePrefix(rc)}_banks.csv"
    ).toSet

    Using.resource(Files.list(outputDir)): stream =>
      stream.iterator.asScala.map(_.getFileName.toString).toSet shouldBe expectedFileNames

    for seed <- Seeds do
      val path   = outputDir.resolve(seedFileName(seed, rc))
      val lines  = readLines(path)
      val result = expectedRuns(seed)

      lines.head shouldBe SimOutput.colNames.mkString(";")
      lines.length shouldBe DurationMonths + 1

      for (line, monthIndex) <- lines.tail.zipWithIndex do
        val actual   = parseCsvRow(line)
        val month    = ExecutionMonth.First.advanceBy(monthIndex)
        val expected = result.timeSeries.monthRow(month)

        actual.length shouldBe SimOutput.nCols

        for col <- 0 until SimOutput.nCols do
          withClue(s"seed=$seed month=${month.toInt} col=$col: ") {
            actual(col) shouldBe (expected(col) +- 1e-6)
          }

    val hhLines = readLines(outputDir.resolve(s"${filePrefix(rc)}_hh.csv"))
    hhLines.head shouldBe ExpectedHhHeader
    hhLines.length shouldBe Seeds.length + 1
    hhLines.tail shouldBe Seeds.map(seed => expectedHhRow(seed, expectedRuns(seed)))

    val bankLines = readLines(outputDir.resolve(s"${filePrefix(rc)}_banks.csv"))
    bankLines.head shouldBe ExpectedBankHeader
    bankLines.length shouldBe 1 + expectedRuns.valuesIterator.map(_.terminalState.banks.length).sum
    bankLines.tail shouldBe Seeds.flatMap(seed => expectedBankRows(seed, expectedRuns(seed)))

  it should "surface a typed error when the output location is not a directory" in withTempDir: tempDir =>
    val outputFile = tempDir.resolve("not-a-directory")
    Files.writeString(outputFile, "occupied")

    unsafeRunEither(McRunner.runZIO(rc, outputFile.toFile)) match
      case Left(SimError.OutputFailure(operation, path, details)) =>
        operation shouldBe "prepare output directory"
        path shouldBe outputFile.toFile.getPath
        details should include("not a directory")
      case other                                       =>
        fail(s"expected typed output failure, got: $other")
