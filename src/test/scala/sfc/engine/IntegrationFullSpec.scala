package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.SimOutput
import sfc.SimOutput.Col
import sfc.config.{RunConfig, SimParams}
import sfc.McRunner.runSingle

class IntegrationFullSpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  private def requireAllMechanisms(): Unit =
    assume(p.flags.openEcon, "OPEN_ECON=true required")
    assume(p.flags.nbpQe, "NBP_QE=true required")
    assume(p.flags.nbpFxIntervention, "NBP_FX_INTERVENTION=true required")
    assume(p.flags.io, "IO_MODE=enabled required")

  private lazy val rc = RunConfig(1, "test")

  private lazy val result =
    requireAllMechanisms()
    runSingle(42, rc)

  private def ts = result.timeSeries

  // ==========================================================================
  // Full integration (all 6 mechanisms ON)
  // ==========================================================================

  "Full integration (all mechanisms)" should "complete without exception" in {
    requireAllMechanisms()
    noException should be thrownBy result
  }

  it should "pass all SFC identity checks" in {
    requireAllMechanisms()
    noException shouldBe thrownBy(runSingle(42, rc))
  }

  it should "produce 120 rows x 197 columns" in {
    requireAllMechanisms()
    ts.length shouldBe p.timeline.duration
    for row <- ts do row.length shouldBe SimOutput.nCols
  }

  it should "produce no NaN or Infinity" in {
    requireAllMechanisms()
    for t <- ts.indices; c <- ts(t).indices do
      withClue(s"Month ${t + 1}, col $c: ") {
        ts(t)(c).isNaN shouldBe false
        ts(t)(c).isInfinite shouldBe false
      }
  }

  it should "be reproducible with the same seed" in {
    requireAllMechanisms()
    val r2 = runSingle(42, rc)
    for t <- 0 until p.timeline.duration; c <- 0 until SimOutput.nCols do
      withClue(s"Month ${t + 1}, col $c: ") {
        ts(t)(c) shouldBe r2.timeSeries(t)(c)
      }
  }

  // ==========================================================================
  // Multi-bank specific
  // ==========================================================================

  "Multi-bank" should "have interbank rate deviating from refRate" in {
    requireAllMechanisms()
    val deviates = ts.indices.exists { t =>
      val ibRate  = ts(t)(Col.InterbankRate.ordinal)
      val refRate = ts(t)(Col.RefRate.ordinal)
      Math.abs(ibRate - refRate) > 1e-6
    }
    deviates shouldBe true
  }

  it should "have finite MinBankCAR at all months" in {
    // MinBankCAR can go negative under stress when BANK_FAILURE is not enabled
    requireAllMechanisms()
    for t <- ts.indices do
      withClue(s"Month ${t + 1}: ") {
        ts(t)(Col.MinBankCAR.ordinal).isNaN shouldBe false
        ts(t)(Col.MinBankCAR.ordinal).isInfinite shouldBe false
      }
  }

  it should "have distinct bank CARs (heterogeneity)" in {
    requireAllMechanisms()
    // MinBankCAR and MaxBankNPL should differ from aggregate values
    // At least in some months, MinBankCAR != the overall bank CAR
    val minCars = ts.map(_(Col.MinBankCAR.ordinal))
    val maxNpls = ts.map(_(Col.MaxBankNPL.ordinal))
    // Not all identical across months (banks evolve differently)
    minCars.distinct.length should be > 1
    maxNpls.distinct.length should be > 1
  }

  // ==========================================================================
  // Individual HH specific
  // ==========================================================================

  "Individual households" should "return defined terminalState with hhAgg" in {
    requireAllMechanisms()
    result.terminalState.world.hhAgg.get.employed should be >= 0
  }

  it should "have employment counts summing to HhCount" in {
    requireAllMechanisms()
    val agg   = result.terminalState.world.hhAgg.get
    val total = agg.employed + agg.unemployed + agg.retraining + agg.bankrupt
    total shouldBe p.household.count
  }

  it should "have Gini coefficients in [0, 1]" in {
    requireAllMechanisms()
    val agg = result.terminalState.world.hhAgg.get
    agg.giniIndividual.toDouble should be >= 0.0
    agg.giniIndividual.toDouble should be <= 1.0
    agg.giniWealth.toDouble should be >= 0.0
    agg.giniWealth.toDouble should be <= 1.0
  }

  // ==========================================================================
  // Open economy specific
  // ==========================================================================

  "Open economy" should "have non-zero NFA" in {
    requireAllMechanisms()
    val nfaValues = ts.map(_(Col.NFA.ordinal))
    nfaValues.exists(_ != 0.0) shouldBe true
  }

  it should "have non-zero trade balance" in {
    requireAllMechanisms()
    val tradeBalance = ts.map(_(Col.TradeBalance.ordinal))
    tradeBalance.exists(_ != 0.0) shouldBe true
  }

  it should "have exchange rate varying from base" in {
    requireAllMechanisms()
    val baseRate = p.forex.baseExRate
    val exRates  = ts.map(_(Col.ExRate.ordinal))
    exRates.exists(r => Math.abs(r - baseRate) > 1e-4) shouldBe true
  }
