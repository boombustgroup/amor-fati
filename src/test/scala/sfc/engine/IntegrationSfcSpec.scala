package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.Observables
import sfc.Observables.Col
import sfc.config.{Config, RunConfig}
import sfc.runSingle

class IntegrationSfcSpec extends AnyFlatSpec with Matchers:

  private lazy val rc = RunConfig(2000.0, 1, "test")

  private lazy val (result, stderrOutput) =
    val baos = new java.io.ByteArrayOutputStream()
    val ps = new java.io.PrintStream(baos)
    val oldErr = System.err
    System.setErr(ps)
    try
      val r = runSingle(42, rc)
      ps.flush()
      (r, baos.toString)
    finally System.setErr(oldErr)

  private def ts = result.timeSeries

  // --- SFC identity ---

  "Default integration" should "pass all SFC identity checks over 120 months" in {
    stderrOutput should not include "[SFC]"
  }

  // --- Data quality ---

  it should "produce no NaN or Infinity in output" in {
    for t <- ts.indices; c <- ts(t).indices do
      withClue(s"Month ${t + 1}, col $c: ") {
        ts(t)(c).isNaN shouldBe false
        ts(t)(c).isInfinite shouldBe false
      }
  }

  // --- Bond market (ON by default) ---

  it should "have positive BondYield after month 1" in {
    assume(Config.GovBondMarket, "GOV_BOND_MARKET=true required")
    for t <- 1 until Config.Duration do
      withClue(s"Month ${t + 1}: ") {
        ts(t)(Col.BondYield.ordinal) should be > 0.0
      }
  }

  it should "have non-zero BondsOutstanding after month 1" in {
    assume(Config.GovBondMarket, "GOV_BOND_MARKET=true required")
    // BondsOutstanding = cumulative deficit; can be negative (surplus) or positive
    val nonZero = (1 until Config.Duration).exists(t => ts(t)(Col.BondsOutstanding.ordinal) != 0.0)
    nonZero shouldBe true
  }

  it should "satisfy bond clearing identity at every month" in {
    assume(Config.GovBondMarket, "GOV_BOND_MARKET=true required")
    for t <- ts.indices do
      val outstanding = ts(t)(Col.BondsOutstanding.ordinal)
      val bankHeld = ts(t)(Col.BankBondHoldings.ordinal)
      val nbpHeld = ts(t)(Col.NbpBondHoldings.ordinal)
      withClue(s"Month ${t + 1}: bank($bankHeld) + nbp($nbpHeld) vs outstanding($outstanding): ") {
        (bankHeld + nbpHeld) shouldBe outstanding +- 1.0
      }
  }

  // --- Symmetric Taylor + unemployment benefits (ON by default) ---

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
    assume(Config.NbpSymmetric, "NBP_SYMMETRIC=true required")
    val initialRate = 0.0575
    val deflationMonth = ts.indices.find(t => ts(t)(Col.Inflation.ordinal) < 0.0)
    deflationMonth match
      case Some(t) =>
        // Rate should eventually decrease from initial
        val rateAfterDeflation = ((t + 1) until Config.Duration).map(m => ts(m)(Col.RefRate.ordinal))
        rateAfterDeflation.exists(_ < initialRate) shouldBe true
      case None =>
        // No deflation occurred — test is vacuously true
        succeed
  }

  // --- Multi-bank columns in single-bank mode ---

  it should "have BankFailures = 0 in single-bank mode" in {
    for t <- ts.indices do ts(t)(Col.BankFailures.ordinal) shouldBe 0.0
  }

  // --- FX intervention OFF by default ---

  it should "have FxInterventionActive = 0 when FX intervention is off" in {
    assume(!Config.NbpFxIntervention, "FX intervention OFF required")
    for t <- ts.indices do ts(t)(Col.FxInterventionActive.ordinal) shouldBe 0.0
  }

  // --- Open economy OFF by default ---

  it should "have zero NFA when open economy is off" in {
    assume(!Config.OeEnabled, "OPEN_ECON=false required")
    for t <- ts.indices do ts(t)(Col.NFA.ordinal) shouldBe 0.0
  }

  // --- Price floor ---

  it should "keep price level above floor (0.30)" in {
    for t <- ts.indices do
      withClue(s"Month ${t + 1}: ") {
        ts(t)(Col.PriceLevel.ordinal) should be >= 0.30
      }
  }

  // --- Aggregate HH mode ---

  it should "return defined terminalHhAgg" in {
    result.terminalHhAgg.employed should be >= 0
  }
