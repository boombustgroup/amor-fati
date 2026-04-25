package com.boombustgroup.amorfati.montecarlo

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.agents.Banking
import com.boombustgroup.amorfati.agents.Banking.BankState
import com.boombustgroup.amorfati.engine.ledger.LedgerFinancialState
import com.boombustgroup.amorfati.agents.Household
import com.boombustgroup.amorfati.config.SimParams
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import zio.{Runtime, Unsafe}

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.Using

class McRunnerOutputSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  "runZIO" should "match runSingle CSV outputs on a small deterministic run" in
    withTempDir: outputDir =>
      val rc       = McRunConfig(nSeeds = 2, outputPrefix = "regression", runDurationMonths = 4, runId = "fixed")
      val expected = expectedFiles(rc)

      runToDir(rc, outputDir)

      listFileNames(outputDir) shouldBe expected.keySet
      expected.foreach: (name, lines) =>
        withClue(s"$name: ") {
          Files.readAllLines(outputDir.resolve(name), UTF_8).asScala.toVector shouldBe lines
        }

  private def runToDir(rc: McRunConfig, outputDir: Path): Unit =
    Unsafe.unsafe: unsafe =>
      given Unsafe = unsafe
      Runtime.default.unsafe.run(McRunner.runZIO(rc, outputDir.toFile)).getOrThrowFiberFailure()

  private def expectedFiles(rc: McRunConfig): Map[String, Vector[String]] =
    val results = (1L to rc.nSeeds.toLong).map: seed =>
      seed -> McRunner.runSingle(seed, rc.runDurationMonths).fold(err => fail(err.toString), identity)

    results.iterator
      .map: (seed, result) =>
        seedFileName(seed, rc) -> expectedSeedLines(result)
      .toMap ++ Map(
      hhFileName(rc)   -> expectedHouseholdLines(results.toVector),
      bankFileName(rc) -> expectedBankLines(results.toVector),
    )

  private def expectedSeedLines(result: RunResult): Vector[String] =
    val rows = result.timeSeries.executionMonths.map: month =>
      val row = result.timeSeries.monthRow(month)
      McTimeseriesSchema.csvSchema.render((month, row))
    McTimeseriesSchema.csvSchema.header +: rows

  private val householdHeader =
    "Seed;HH_Employed;HH_Unemployed;HH_Retraining;HH_Bankrupt;MeanSavings;MedianSavings;Gini_Individual;" +
      "Gini_Wealth;MeanSkill;MeanHealthPenalty;RetrainingAttempts;RetrainingSuccesses;ConsumptionP10;" +
      "ConsumptionP50;ConsumptionP90;BankruptcyRate;MeanMonthsToRuin;PovertyRate_50pct;PovertyRate_30pct"

  private def expectedHouseholdLines(results: Vector[(Long, RunResult)]): Vector[String] =
    householdHeader +: results.map: (seed, result) =>
      householdRow(seed, result.terminalState.householdAggregates)

  private def householdRow(seed: Long, agg: Household.Aggregates): String =
    Vector(
      s"$seed",
      s"${agg.employed}",
      s"${agg.unemployed}",
      s"${agg.retraining}",
      s"${agg.bankrupt}",
      f"${decimal(agg.meanSavings)}%.2f",
      f"${decimal(agg.medianSavings)}%.2f",
      f"${decimal(agg.giniIndividual)}%.6f",
      f"${decimal(agg.giniWealth)}%.6f",
      f"${decimal(agg.meanSkill)}%.6f",
      f"${decimal(agg.meanHealthPenalty)}%.6f",
      s"${agg.retrainingAttempts}",
      s"${agg.retrainingSuccesses}",
      f"${decimal(agg.consumptionP10)}%.2f",
      f"${decimal(agg.consumptionP50)}%.2f",
      f"${decimal(agg.consumptionP90)}%.2f",
      f"${decimal(agg.bankruptcyRate)}%.6f",
      f"${decimal(agg.meanMonthsToRuin)}%.2f",
      f"${decimal(agg.povertyRate50)}%.6f",
      f"${decimal(agg.povertyRate30)}%.6f",
    ).mkString(";")

  private val bankHeader =
    "Seed;BankId;Deposits;Loans;Capital;NPL;CAR;GovBonds;InterbankNet;Failed"

  private def expectedBankLines(results: Vector[(Long, RunResult)]): Vector[String] =
    bankHeader +: results.flatMap: (seed, result) =>
      result.terminalState.banks.map: bank =>
        val balances = result.terminalState.ledgerFinancialState.banks(bank.id.toInt)
        bankRow(seed, bank, balances)

  private def bankRow(seed: Long, bank: BankState, balances: LedgerFinancialState.BankBalances): String =
    val stocks = LedgerFinancialState.projectBankFinancialStocks(balances)
    Vector(
      s"$seed",
      s"${bank.id}",
      f"${decimal(stocks.totalDeposits)}%.2f",
      f"${decimal(stocks.firmLoan)}%.2f",
      f"${decimal(bank.capital)}%.2f",
      f"${decimal(Banking.nplRatio(bank, stocks))}%.6f",
      f"${decimal(Banking.car(bank, stocks, balances.corpBond))}%.6f",
      f"${decimal(Banking.govBondHoldings(stocks))}%.2f",
      f"${decimal(stocks.interbankLoan)}%.2f",
      s"${bank.failed}",
    ).mkString(";")

  private def filePrefix(rc: McRunConfig): String =
    s"${rc.outputPrefix}_${rc.runId}_${rc.runDurationMonths}m"

  private def seedFileName(seed: Long, rc: McRunConfig): String =
    f"${filePrefix(rc)}_seed${seed}%03d.csv"

  private def hhFileName(rc: McRunConfig): String =
    s"${filePrefix(rc)}_hh.csv"

  private def bankFileName(rc: McRunConfig): String =
    s"${filePrefix(rc)}_banks.csv"

  private def listFileNames(outputDir: Path): Set[String] =
    Using.resource(Files.list(outputDir)) { paths =>
      paths.iterator().asScala.map(_.getFileName.toString).toSet
    }

  private def withTempDir[A](f: Path => A): A =
    val outputDir = Files.createTempDirectory("mc-runner-output")
    try f(outputDir)
    finally deleteRecursively(outputDir)

  private def deleteRecursively(path: Path): Unit =
    if Files.exists(path) then
      Using.resource(Files.walk(path)) { paths =>
        paths.iterator().asScala.toVector.sortBy(_.getNameCount)(using Ordering.Int.reverse).foreach(Files.deleteIfExists)
      }
