package com.boombustgroup.amorfati.config

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.agents.Firm
import com.boombustgroup.amorfati.types.*

class SimConfigSpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]
  private val td           = ComputationBoundary

  "p.sectorDefs" should "have 6 entries" in {
    p.sectorDefs.length shouldBe 6
  }

  it should "have shares summing to ~1.0" in {
    p.sectorDefs.map(s => td.toDouble(s.share)).sum shouldBe 1.0 +- 0.01
  }

  it should "have positive sigma for every sector" in {
    for s <- p.sectorDefs do td.toDouble(s.sigma) should be > 0.0
  }

  it should "have known sector names" in {
    p.sectorDefs.map(_.name) should contain allOf (
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
    if sys.env.get("FIRMS_COUNT").isEmpty then p.pop.firmsCount shouldBe 10000
  }

  it should "have Duration = 120" in {
    p.timeline.duration shouldBe 120
  }

  it should "have WorkersPerFirm = 10" in {
    p.pop.workersPerFirm shouldBe 10
  }

  it should "have positive AI and Hybrid CAPEX" in {
    td.toDouble(p.firm.aiCapex) should be > 0.0
    td.toDouble(p.firm.hybridCapex) should be > 0.0
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
