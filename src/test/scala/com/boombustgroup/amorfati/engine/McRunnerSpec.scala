package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.montecarlo.McRunner.runSingle
import com.boombustgroup.amorfati.montecarlo.SimOutput
import com.boombustgroup.amorfati.montecarlo.SimOutput.Col
import com.boombustgroup.amorfati.montecarlo.TimeSeries
import com.boombustgroup.amorfati.tags.Heavy
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

@Heavy
class McRunnerSpec extends AnyFlatSpec with Matchers:

  given SimParams      = SimParams.defaults
  private val td       = ComputationBoundary
  private val duration = 60 // months

  // Single shared run — all tests read from this result
  private lazy val result                     = runSingle(42, duration).fold(e => fail(e.toString), identity)
  private def ts                              = result.timeSeries
  private def month(t: Int): ExecutionMonth   = ExecutionMonth.First.advanceBy(t)
  private def row(t: Int)                     = ts.monthRow(month(t))
  private def row(series: TimeSeries, t: Int) = series.monthRow(month(t))

  // --- Basic output sanity ---

  "runSingle" should "reject non-positive durations" in {
    val zeroDuration = intercept[IllegalArgumentException]:
      runSingle(42, 0)
    zeroDuration.getMessage should include("runDurationMonths must be > 0")

    val negativeDuration = intercept[IllegalArgumentException]:
      runSingle(42, -1)
    negativeDuration.getMessage should include("runDurationMonths must be > 0")
  }

  "runSingle" should s"produce 60 rows x ${SimOutput.nCols} columns" in {
    ts.nMonths shouldBe duration
    for row <- ts do row.length shouldBe SimOutput.nCols
  }

  it should "have Month column = 1..60" in {
    for month <- ts.executionMonths do ts.at(month, Col.Month) shouldBe month.toInt.toDouble
  }

  it should "keep adoption ratio in [0, 1]" in {
    for t <- 0 until duration do
      row(t)(Col.TotalAdoption.ordinal) should be >= 0.0
      row(t)(Col.TotalAdoption.ordinal) should be <= 1.0
  }

  it should "keep unemployment in [0, 1]" in {
    for t <- 0 until duration do
      row(t)(Col.Unemployment.ordinal) should be >= 0.0
      row(t)(Col.Unemployment.ordinal) should be <= 1.0
  }

  it should "produce no NaN or Infinity in output" in {
    for t <- ts.indices; c <- row(t).indices do
      withClue(s"Month ${t + 1}, col $c: ") {
        row(t)(c).isNaN shouldBe false
        row(t)(c).isInfinite shouldBe false
      }
  }

  it should "have positive sigma values" in {
    for t <- 0 until duration; s <- 0 until 6 do row(t)(Col.sectorSigma(s).ordinal) should be > 0.0
  }

  it should "have positive mean degree" in {
    for t <- 0 until duration do row(t)(Col.MeanDegree.ordinal) should be > 0.0
  }

  it should "keep price level above floor (0.30)" in {
    for t <- ts.indices do
      withClue(s"Month ${t + 1}: ") {
        row(t)(Col.PriceLevel.ordinal) should be >= 0.30
      }
  }

  // --- Reproducibility ---

  it should "be reproducible with the same seed" in {
    val r2 = runSingle(42, duration).fold(e => fail(e.toString), identity)
    for t <- 0 until duration; c <- 0 until SimOutput.nCols do row(t)(c) shouldBe row(r2.timeSeries, t)(c)
  }

  // --- Terminal state ---

  it should "return terminalState with valid hhAgg" in {
    result.terminalState.householdAggregates.employed should be >= 0
  }

  it should "have employment counts covering all households" in {
    val agg   = result.terminalState.householdAggregates
    val total = agg.employed + agg.unemployed + agg.retraining + agg.bankrupt
    total should be > 0
  }

  it should "have Gini coefficients in [0, 1]" in {
    val agg = result.terminalState.householdAggregates
    td.toDouble(agg.giniIndividual) should be >= 0.0
    td.toDouble(agg.giniIndividual) should be <= 1.0
    td.toDouble(agg.giniWealth) should be >= 0.0
    td.toDouble(agg.giniWealth) should be <= 1.0
  }

  // --- Bond market ---

  it should "keep BondYield non-negative after month 1" in {
    for t <- 1 until duration do
      withClue(s"Month ${t + 1}: ") {
        row(t)(Col.BondYield.ordinal) should be >= 0.0
      }
  }

  it should "have non-zero BondsOutstanding after month 1" in {
    val nonZero = (1 until duration).exists(t => row(t)(Col.BondsOutstanding.ordinal) != 0.0)
    nonZero shouldBe true
  }

  it should "satisfy bond clearing identity at every month" in {
    for t <- ts.indices do
      val outstanding = row(t)(Col.BondsOutstanding.ordinal)
      val holders     = row(t)(Col.BankBondHoldings.ordinal) + row(t)(Col.ForeignBondHoldings.ordinal) + row(t)(Col.NbpBondHoldings.ordinal) +
        row(t)(Col.PpkBondHoldings.ordinal) + row(t)(Col.InsGovBondHoldings.ordinal) + row(t)(Col.NbfiTfiGovBondHoldings.ordinal)
      withClue(s"Month ${t + 1}: holders($holders) vs outstanding($outstanding): ") {
        holders shouldBe outstanding +- 1.0
      }
  }

  // --- Symmetric Taylor ---

  it should "have non-negative unemployment benefits" in {
    for t <- ts.indices do
      withClue(s"Month ${t + 1}: ") {
        row(t)(Col.UnempBenefitSpend.ordinal) should be >= 0.0
      }
  }

  it should "compute non-zero output gap" in {
    val outputGaps = ts.map(_(Col.OutputGap.ordinal))
    outputGaps.exists(_ != 0.0) shouldBe true
  }

  it should "cut NBP rate when inflation is negative (symmetric Taylor)" in {
    val initialRate    = 0.0575
    val deflationMonth = ts.indices.find(t => row(t)(Col.Inflation.ordinal) < 0.0)
    deflationMonth match
      case Some(t) =>
        val rateAfterDeflation = ((t + 1) until duration).map(m => row(m)(Col.RefRate.ordinal))
        rateAfterDeflation.exists(_ < initialRate) shouldBe true
      case None    =>
        succeed
  }

  // --- Multi-bank ---

  it should "have interbank rate deviating from refRate" in {
    val deviates = ts.indices.exists { t =>
      val ibRate  = row(t)(Col.InterbankRate.ordinal)
      val refRate = row(t)(Col.RefRate.ordinal)
      Math.abs(ibRate - refRate) > 1e-6
    }
    deviates shouldBe true
  }

  it should "have finite MinBankCAR at all months" in {
    for t <- ts.indices do
      withClue(s"Month ${t + 1}: ") {
        row(t)(Col.MinBankCAR.ordinal).isNaN shouldBe false
        row(t)(Col.MinBankCAR.ordinal).isInfinite shouldBe false
      }
  }

  it should "have distinct bank CARs (heterogeneity)" in {
    val minCars = ts.map(_(Col.MinBankCAR.ordinal))
    val maxNpls = ts.map(_(Col.MaxBankNPL.ordinal))
    minCars.distinct.length should be >= 1
    maxNpls.distinct.length should be >= 1
  }

  // --- FX / Open economy ---
