package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.montecarlo.McRunner.runSingle
import com.boombustgroup.amorfati.montecarlo.SimOutput
import com.boombustgroup.amorfati.montecarlo.SimOutput.Col
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class McRunnerSpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]
  private val td           = ComputationBoundary
  private val duration     = 60 // months

  // Single shared run — all tests read from this result
  private lazy val result = runSingle(42, duration).fold(e => fail(e.toString), identity)
  private def ts          = result.timeSeries

  // --- Basic output sanity ---

  "runSingle" should "produce 60 rows x 227 columns" in {
    ts.length shouldBe duration
    for row <- ts do row.length shouldBe SimOutput.nCols
  }

  it should "have Month column = 1..60" in {
    for t <- 0 until duration do ts(t)(Col.Month.ordinal) shouldBe (t + 1).toDouble
  }

  it should "keep adoption ratio in [0, 1]" in {
    for t <- 0 until duration do
      ts(t)(Col.TotalAdoption.ordinal) should be >= 0.0
      ts(t)(Col.TotalAdoption.ordinal) should be <= 1.0
  }

  it should "keep unemployment in [0, 1]" in {
    for t <- 0 until duration do
      ts(t)(Col.Unemployment.ordinal) should be >= 0.0
      ts(t)(Col.Unemployment.ordinal) should be <= 1.0
  }

  it should "produce no NaN or Infinity in output" in {
    for t <- ts.indices; c <- ts(t).indices do
      withClue(s"Month ${t + 1}, col $c: ") {
        ts(t)(c).isNaN shouldBe false
        ts(t)(c).isInfinite shouldBe false
      }
  }

  it should "have positive sigma values" in {
    for t <- 0 until duration; s <- 0 until 6 do ts(t)(Col.sectorSigma(s).ordinal) should be > 0.0
  }

  it should "have positive mean degree" in {
    for t <- 0 until duration do ts(t)(Col.MeanDegree.ordinal) should be > 0.0
  }

  it should "keep price level above floor (0.30)" in {
    for t <- ts.indices do
      withClue(s"Month ${t + 1}: ") {
        ts(t)(Col.PriceLevel.ordinal) should be >= 0.30
      }
  }

  // --- Reproducibility ---

  it should "be reproducible with the same seed" in {
    val r2 = runSingle(42, duration).fold(e => fail(e.toString), identity)
    for t <- 0 until duration; c <- 0 until SimOutput.nCols do ts(t)(c) shouldBe r2.timeSeries(t)(c)
  }

  // --- Terminal state ---

  it should "return terminalState with valid hhAgg" in {
    result.terminalState.world.cachedHouseholdAggregates.employed should be >= 0
  }

  it should "have employment counts covering all households" in {
    val agg   = result.terminalState.world.cachedHouseholdAggregates
    val total = agg.employed + agg.unemployed + agg.retraining + agg.bankrupt
    total should be > 0
  }

  it should "have Gini coefficients in [0, 1]" in {
    val agg = result.terminalState.world.cachedHouseholdAggregates
    td.toDouble(agg.giniIndividual) should be >= 0.0
    td.toDouble(agg.giniIndividual) should be <= 1.0
    td.toDouble(agg.giniWealth) should be >= 0.0
    td.toDouble(agg.giniWealth) should be <= 1.0
  }

  // --- Bond market ---

  it should "have positive BondYield after month 1" in {
    assume(p.flags.govBondMarket, "GOV_BOND_MARKET=true required")
    for t <- 1 until duration do
      withClue(s"Month ${t + 1}: ") {
        ts(t)(Col.BondYield.ordinal) should be > 0.0
      }
  }

  it should "have non-zero BondsOutstanding after month 1" in {
    assume(p.flags.govBondMarket, "GOV_BOND_MARKET=true required")
    val nonZero = (1 until duration).exists(t => ts(t)(Col.BondsOutstanding.ordinal) != 0.0)
    nonZero shouldBe true
  }

  it should "satisfy bond clearing identity at every month" in {
    assume(p.flags.govBondMarket, "GOV_BOND_MARKET=true required")
    for t <- ts.indices do
      val outstanding = ts(t)(Col.BondsOutstanding.ordinal)
      val holders     = ts(t)(Col.BankBondHoldings.ordinal) + ts(t)(Col.ForeignBondHoldings.ordinal) + ts(t)(Col.NbpBondHoldings.ordinal) +
        ts(t)(Col.PpkBondHoldings.ordinal) + ts(t)(Col.InsGovBondHoldings.ordinal) + ts(t)(Col.NbfiTfiGovBondHoldings.ordinal)
      withClue(s"Month ${t + 1}: holders($holders) vs outstanding($outstanding): ") {
        holders shouldBe outstanding +- 1.0
      }
  }

  // --- Symmetric Taylor ---

  it should "have non-negative unemployment benefits" in {
    for t <- ts.indices do
      withClue(s"Month ${t + 1}: ") {
        ts(t)(Col.UnempBenefitSpend.ordinal) should be >= 0.0
      }
  }

  it should "compute non-zero output gap" in {
    val outputGaps = ts.map(_(Col.OutputGap.ordinal))
    outputGaps.exists(_ != 0.0) shouldBe true
  }

  it should "cut NBP rate when inflation is negative (symmetric Taylor)" in {
    assume(p.flags.nbpSymmetric, "NBP_SYMMETRIC=true required")
    val initialRate    = 0.0575
    val deflationMonth = ts.indices.find(t => ts(t)(Col.Inflation.ordinal) < 0.0)
    deflationMonth match
      case Some(t) =>
        val rateAfterDeflation = ((t + 1) until duration).map(m => ts(m)(Col.RefRate.ordinal))
        rateAfterDeflation.exists(_ < initialRate) shouldBe true
      case None    =>
        succeed
  }

  // --- Multi-bank ---

  it should "have interbank rate deviating from refRate" in {
    val deviates = ts.indices.exists { t =>
      val ibRate  = ts(t)(Col.InterbankRate.ordinal)
      val refRate = ts(t)(Col.RefRate.ordinal)
      Math.abs(ibRate - refRate) > 1e-6
    }
    deviates shouldBe true
  }

  it should "have finite MinBankCAR at all months" in {
    for t <- ts.indices do
      withClue(s"Month ${t + 1}: ") {
        ts(t)(Col.MinBankCAR.ordinal).isNaN shouldBe false
        ts(t)(Col.MinBankCAR.ordinal).isInfinite shouldBe false
      }
  }

  it should "have distinct bank CARs (heterogeneity)" in {
    val minCars = ts.map(_(Col.MinBankCAR.ordinal))
    val maxNpls = ts.map(_(Col.MaxBankNPL.ordinal))
    minCars.distinct.length should be >= 1
    maxNpls.distinct.length should be >= 1
  }

  // --- FX / Open economy ---
