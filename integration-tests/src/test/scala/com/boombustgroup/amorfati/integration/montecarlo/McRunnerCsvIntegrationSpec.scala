package com.boombustgroup.amorfati.integration.montecarlo

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.agents.Banking
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.ledger.LedgerFinancialState
import com.boombustgroup.amorfati.montecarlo.{McRunConfig, McRunner, McTimeseriesSchema, RunResult, SimError}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import zio.{Runtime, Unsafe, ZIO}

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.Using

class McRunnerCsvIntegrationSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

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

  private def parseCsvRow(line: String): Vector[BigDecimal] =
    line.split(';').toVector.map(value => BigDecimal(value.replace(',', '.')))

  private def expectedRun(seed: Long): RunResult =
    McRunner.runSingle(seed, DurationMonths).fold(err => fail(err.toString), identity)

  private def expectedHhRow(seed: Long, result: RunResult): String =
    val a = result.terminalState.householdAggregates
    s"$seed;${a.employed};${a.unemployed};${a.retraining};${a.bankrupt};" +
      s"${a.meanSavings.format(2)};${a.medianSavings.format(2)};" +
      s"${a.giniIndividual.format(6)};${a.giniWealth.format(6)};" +
      s"${a.meanSkill.format(6)};${a.meanHealthPenalty.format(6)};" +
      s"${a.retrainingAttempts};${a.retrainingSuccesses};" +
      s"${a.consumptionP10.format(2)};${a.consumptionP50.format(2)};${a.consumptionP90.format(2)};" +
      s"${a.bankruptcyRate.format(6)};" +
      s"${a.meanMonthsToRuin.format(2)};" +
      s"${a.povertyRate50.format(6)};${a.povertyRate30.format(6)}"

  private def expectedBankRows(seed: Long, result: RunResult): Vector[String] =
    result.terminalState.banks.map: bank =>
      val balances = result.terminalState.ledgerFinancialState.banks(bank.id.toInt)
      val stocks   = LedgerFinancialState.projectBankFinancialStocks(balances)
      s"$seed;${bank.id};" +
        s"${stocks.totalDeposits.format(2)};${stocks.firmLoan.format(2)};${bank.capital.format(2)};" +
        s"${Banking.nplRatio(bank, stocks).format(6)};${Banking.car(bank, stocks, balances.corpBond).format(6)};" +
        s"${Banking.govBondHoldings(stocks).format(2)};${stocks.interbankLoan.format(2)};" +
        s"${bank.failed}"

  "runZIO".should("write deterministic per-seed and summary CSV files").in {
    withTempDir { outputDir =>
      unsafeRun(McRunner.runZIO(rc, outputDir.toFile))

      val expectedRuns      = Seeds.map(seed => seed -> expectedRun(seed)).toMap
      val expectedFileNames = (
        Seeds.map(seed => seedFileName(seed, rc)) :+
          s"${filePrefix(rc)}_hh.csv" :+
          s"${filePrefix(rc)}_banks.csv"
      ).toSet

      Using.resource(Files.list(outputDir)): stream =>
        stream.iterator.asScala.map(_.getFileName.toString).toSet.shouldBe(expectedFileNames)

      for seed <- Seeds do
        val path   = outputDir.resolve(seedFileName(seed, rc))
        val lines  = readLines(path)
        val result = expectedRuns(seed)

        lines.head.shouldBe(McTimeseriesSchema.colNames.mkString(";"))
        lines.length.shouldBe(DurationMonths + 1)

        for (line, monthIndex) <- lines.tail.zipWithIndex do
          val actual   = parseCsvRow(line)
          val month    = ExecutionMonth.First.advanceBy(monthIndex)
          val expected = result.timeSeries.monthRow(month)

          actual.length.shouldBe(McTimeseriesSchema.nCols)

          for col <- 0 until McTimeseriesSchema.nCols do
            withClue(s"seed=$seed month=${month.toInt} col=$col: ") {
              actual(col).shouldBe(decimal(expected(col)) +- BigDecimal("1e-6"))
            }

      val hhLines = readLines(outputDir.resolve(s"${filePrefix(rc)}_hh.csv"))
      hhLines.head.shouldBe(ExpectedHhHeader)
      hhLines.length.shouldBe(Seeds.length + 1)
      hhLines.tail.shouldBe(Seeds.map(seed => expectedHhRow(seed, expectedRuns(seed))))

      val bankLines = readLines(outputDir.resolve(s"${filePrefix(rc)}_banks.csv"))
      bankLines.head.shouldBe(ExpectedBankHeader)
      bankLines.length.shouldBe(1 + expectedRuns.valuesIterator.map(_.terminalState.banks.length).sum)
      bankLines.tail.shouldBe(Seeds.flatMap(seed => expectedBankRows(seed, expectedRuns(seed))))
    }
  }

  it.should("surface a typed error when the output location is not a directory").in {
    withTempDir { tempDir =>
      val outputFile = tempDir.resolve("not-a-directory")
      Files.writeString(outputFile, "occupied")

      unsafeRunEither(McRunner.runZIO(rc, outputFile.toFile)) match
        case Left(SimError.OutputFailure(operation, path, details)) =>
          operation.shouldBe("prepare output directory")
          path.shouldBe(outputFile.toFile.getPath)
          details.should(include("not a directory"))
        case other                                                 =>
          fail(s"expected typed output failure, got: $other")
    }
  }
