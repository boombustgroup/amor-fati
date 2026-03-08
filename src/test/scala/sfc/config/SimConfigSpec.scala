package sfc.config

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.agents.Firm
import sfc.types.*

class SimConfigSpec extends AnyFlatSpec with Matchers:

  "SECTORS" should "have 6 entries" in {
    SECTORS.length shouldBe 6
  }

  it should "have shares summing to ~1.0" in {
    SECTORS.map(_.share.toDouble).sum shouldBe 1.0 +- 0.01
  }

  it should "have positive sigma for every sector" in {
    for s <- SECTORS do s.sigma should be > 0.0
  }

  it should "have known sector names" in {
    SECTORS.map(_.name) should contain allOf (
      "BPO/SSC",
      "Manufacturing",
      "Retail/Services",
      "Healthcare",
      "Public",
      "Agriculture",
    )
  }

  "Config" should "have FirmsCount = 10000 by default" in {
    // Only true when FIRMS_COUNT env var is unset
    if sys.env.get("FIRMS_COUNT").isEmpty then Config.FirmsCount shouldBe 10000
  }

  it should "have Duration = 120" in {
    Config.Duration shouldBe 120
  }

  it should "have ShockMonth = 30" in {
    Config.ShockMonth shouldBe 30
  }

  it should "have WorkersPerFirm = 10" in {
    Config.WorkersPerFirm shouldBe 10
  }

  it should "have positive AI and Hybrid CAPEX" in {
    Config.AiCapex should be > 0.0
    Config.HybridCapex should be > 0.0
  }

  "RunConfig" should "derive isNoBdp correctly" in {
    RunConfig(0.0, 1, "test").isNoBdp shouldBe true
    RunConfig(2000.0, 1, "test").isNoBdp shouldBe false
  }

  "sigmaThreshold" should "return ~0.91 for sigma=2" in {
    Firm.sigmaThreshold(2.0) shouldBe 0.9026 +- 0.01
  }

  it should "return ~0.955 for sigma=5" in {
    Firm.sigmaThreshold(5.0) shouldBe 0.9324 +- 0.01
  }

  it should "return ~0.955 for sigma=10" in {
    Firm.sigmaThreshold(10.0) shouldBe 0.955 +- 0.01
  }

  it should "be capped at 1.0 for sigma=50" in {
    Firm.sigmaThreshold(50.0) should be <= 1.0
    Firm.sigmaThreshold(50.0) shouldBe 1.0 +- 0.01
  }
