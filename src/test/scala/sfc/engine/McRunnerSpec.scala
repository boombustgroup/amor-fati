package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.McRunner.runSingle
import sfc.SimOutput
import sfc.SimOutput.Col
import sfc.config.{RunConfig, SimParams}

class McRunnerSpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  private val rc = RunConfig(1, "test")

  // Single shared run — all tests read from this result
  private lazy val result = runSingle(42, rc)
  private def ts          = result.timeSeries

  // --- Basic output sanity ---

  "runSingle" should "complete without exception" in {
    noException should be thrownBy result
  }

  it should "produce 120 rows x 197 columns" in {
    ts.length shouldBe p.timeline.duration
    for row <- ts do row.length shouldBe SimOutput.nCols
  }

  it should "have Month column = 1..120" in {
    for t <- 0 until p.timeline.duration do ts(t)(Col.Month.ordinal) shouldBe (t + 1).toDouble
  }

  it should "keep adoption ratio in [0, 1]" in {
    for t <- 0 until p.timeline.duration do
      ts(t)(Col.TotalAdoption.ordinal) should be >= 0.0
      ts(t)(Col.TotalAdoption.ordinal) should be <= 1.0
  }

  it should "keep unemployment in [0, 1]" in {
    for t <- 0 until p.timeline.duration do
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
    for t <- 0 until p.timeline.duration; s <- 0 until 6 do ts(t)(Col.sectorSigma(s).ordinal) should be > 0.0
  }

  it should "have positive mean degree" in {
    for t <- 0 until p.timeline.duration do ts(t)(Col.MeanDegree.ordinal) should be > 0.0
  }

  it should "keep price level above floor (0.30)" in {
    for t <- ts.indices do
      withClue(s"Month ${t + 1}: ") {
        ts(t)(Col.PriceLevel.ordinal) should be >= 0.30
      }
  }

  // --- Reproducibility ---

  it should "be reproducible with the same seed" in {
    val r2 = runSingle(42, rc)
    for t <- 0 until p.timeline.duration; c <- 0 until SimOutput.nCols do ts(t)(c) shouldBe r2.timeSeries(t)(c)
  }

  // --- Terminal state ---

  it should "return defined terminalState with hhAgg" in {
    result.terminalState.world.hhAgg.get.employed should be >= 0
  }

  it should "have employment counts summing to HhCount" in {
    val agg   = result.terminalState.world.hhAgg.get
    val total = agg.employed + agg.unemployed + agg.retraining + agg.bankrupt
    total shouldBe p.household.count
  }

  it should "have Gini coefficients in [0, 1]" in {
    val agg = result.terminalState.world.hhAgg.get
    agg.giniIndividual.toDouble should be >= 0.0
    agg.giniIndividual.toDouble should be <= 1.0
    agg.giniWealth.toDouble should be >= 0.0
    agg.giniWealth.toDouble should be <= 1.0
  }

  // --- Bond market ---

  it should "have positive BondYield after month 1" in {
    assume(p.flags.govBondMarket, "GOV_BOND_MARKET=true required")
    for t <- 1 until p.timeline.duration do
      withClue(s"Month ${t + 1}: ") {
        ts(t)(Col.BondYield.ordinal) should be > 0.0
      }
  }

  it should "have non-zero BondsOutstanding after month 1" in {
    assume(p.flags.govBondMarket, "GOV_BOND_MARKET=true required")
    val nonZero = (1 until p.timeline.duration).exists(t => ts(t)(Col.BondsOutstanding.ordinal) != 0.0)
    nonZero shouldBe true
  }

  it should "satisfy bond clearing identity at every month" in {
    assume(p.flags.govBondMarket, "GOV_BOND_MARKET=true required")
    for t <- ts.indices do
      val outstanding = ts(t)(Col.BondsOutstanding.ordinal)
      val bankHeld    = ts(t)(Col.BankBondHoldings.ordinal)
      val nbpHeld     = ts(t)(Col.NbpBondHoldings.ordinal)
      withClue(s"Month ${t + 1}: bank($bankHeld) + nbp($nbpHeld) vs outstanding($outstanding): ") {
        (bankHeld + nbpHeld) shouldBe outstanding +- 1.0
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
        val rateAfterDeflation = ((t + 1) until p.timeline.duration).map(m => ts(m)(Col.RefRate.ordinal))
        rateAfterDeflation.exists(_ < initialRate) shouldBe true
      case None    =>
        succeed
  }

  // --- Multi-bank ---

  it should "have BankFailures = 0 in single-bank mode" in {
    for t <- ts.indices do ts(t)(Col.BankFailures.ordinal) shouldBe 0.0
  }

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
    minCars.distinct.length should be > 1
    maxNpls.distinct.length should be > 1
  }

  // --- FX / Open economy ---

  it should "have FxInterventionActive = 0 when FX intervention is off" in {
    assume(!p.flags.nbpFxIntervention, "FX intervention OFF required")
    for t <- ts.indices do ts(t)(Col.FxInterventionActive.ordinal) shouldBe 0.0
  }

  it should "have zero NFA when open economy is off" in {
    assume(!p.flags.openEcon, "OPEN_ECON=false required")
    for t <- ts.indices do ts(t)(Col.NFA.ordinal) shouldBe 0.0
  }
