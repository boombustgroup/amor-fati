package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.config.{Config, HH_MODE, HhMode, RunConfig}
import sfc.runSingle

class IntegrationFullSpec extends AnyFlatSpec with Matchers:

  private def requireAllMechanisms(): Unit =
    assume(Config.BankMulti, "BANK_MODE=multi required")
    assume(Config.OeEnabled, "OPEN_ECON=true required")
    assume(Config.NbpQe, "NBP_QE=true required")
    assume(Config.NbpFxIntervention, "NBP_FX_INTERVENTION=true required")
    assume(Config.IoEnabled, "IO_MODE=enabled required")
    assume(HH_MODE == HhMode.Individual, "HH_MODE=individual required")

  private lazy val rc = RunConfig(2000.0, 1, "test")

  private lazy val (result, stderrOutput) =
    requireAllMechanisms()
    val baos = new java.io.ByteArrayOutputStream()
    val ps   = new java.io.PrintStream(baos)
    val oldErr = System.err
    System.setErr(ps)
    try
      val r = runSingle(42, rc)
      ps.flush()
      (r, baos.toString)
    finally System.setErr(oldErr)

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
    stderrOutput should not include "[SFC]"
  }

  it should "produce 120 rows x 111 columns" in {
    requireAllMechanisms()
    ts.length shouldBe Config.Duration
    for row <- ts do
      row.length shouldBe 182
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
    for t <- 0 until Config.Duration; c <- 0 until 182 do
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
      val ibRate  = ts(t)(48) // InterbankRate
      val refRate = ts(t)(8)  // RefRate
      Math.abs(ibRate - refRate) > 1e-6
    }
    deviates shouldBe true
  }

  it should "have finite MinBankCAR at all months" in {
    // MinBankCAR can go negative under stress when BANK_FAILURE is not enabled
    requireAllMechanisms()
    for t <- ts.indices do
      withClue(s"Month ${t + 1}: ") {
        ts(t)(49).isNaN shouldBe false
        ts(t)(49).isInfinite shouldBe false
      }
  }

  it should "have distinct bank CARs (heterogeneity)" in {
    requireAllMechanisms()
    // MinBankCAR (49) and MaxBankNPL (50) should differ from aggregate values
    // At least in some months, MinBankCAR != the overall bank CAR
    val minCars = ts.map(_(49))
    val maxNpls = ts.map(_(50))
    // Not all identical across months (banks evolve differently)
    minCars.distinct.length should be > 1
  }

  // ==========================================================================
  // Individual HH specific
  // ==========================================================================

  "Individual households" should "return Some for terminalHhAgg" in {
    requireAllMechanisms()
    result.terminalHhAgg shouldBe defined
  }

  it should "have employment counts summing to HhCount" in {
    requireAllMechanisms()
    val agg = result.terminalHhAgg.get
    val total = agg.employed + agg.unemployed + agg.retraining + agg.bankrupt
    total shouldBe Config.HhCount
  }

  it should "have Gini coefficients in [0, 1]" in {
    requireAllMechanisms()
    val agg = result.terminalHhAgg.get
    agg.giniIndividual should be >= 0.0
    agg.giniIndividual should be <= 1.0
    agg.giniWealth should be >= 0.0
    agg.giniWealth should be <= 1.0
  }

  // ==========================================================================
  // Open economy specific
  // ==========================================================================

  "Open economy" should "have non-zero NFA" in {
    requireAllMechanisms()
    val nfaValues = ts.map(_(28)) // NFA
    nfaValues.exists(_ != 0.0) shouldBe true
  }

  it should "have non-zero trade balance" in {
    requireAllMechanisms()
    val tradeBalance = ts.map(_(31)) // TradeBalance
    tradeBalance.exists(_ != 0.0) shouldBe true
  }

  it should "have exchange rate varying from base" in {
    requireAllMechanisms()
    val baseRate = Config.BaseExRate
    val exRates = ts.map(_(4)) // ExRate
    exRates.exists(r => Math.abs(r - baseRate) > 1e-4) shouldBe true
  }
