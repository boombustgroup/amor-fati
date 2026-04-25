package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.agents.Firm
import com.boombustgroup.amorfati.types.*

class SimConfigSpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  "p.sectorDefs" should "have 6 entries" in {
    p.sectorDefs.length shouldBe 6
  }

  it should "have shares summing to ~1.0" in {
    p.sectorDefs.map(_.share.bd).sum shouldBe (BigDecimal("1.0") +- BigDecimal("0.01"))
  }

  it should "have positive sigma for every sector" in {
    for s <- p.sectorDefs do s.sigma.bd should be > BigDecimal(0)
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

  it should "reject schema-breaking sector count changes" in {
    val err = intercept[IllegalArgumentException]:
      p.copy(sectorDefs = p.sectorDefs.dropRight(1))

    err.getMessage.should(include("sectorDefs must have 6 schema sectors"))
  }

  it should "reject schema-breaking sector reordering" in {
    val reordered = Vector(p.sectorDefs(1), p.sectorDefs(0)) ++ p.sectorDefs.drop(2)
    val err       = intercept[IllegalArgumentException]:
      p.copy(sectorDefs = reordered)

    err.getMessage.should(include("sectorDefs must preserve schema order"))
  }

  "HousingConfig" should "reject malformed regional market vectors" in {
    val err = intercept[IllegalArgumentException]:
      p.housing.copy(regionalMarkets = p.housing.regionalMarkets.dropRight(1))

    err.getMessage.should(include("regionalMarkets must have 7 regions"))
  }

  it should "reject reordered regional markets" in {
    val regionalMarkets = p.housing.regionalMarkets
    val swapped         = regionalMarkets.updated(0, regionalMarkets(1)).updated(1, regionalMarkets(0))
    val err             = intercept[IllegalArgumentException]:
      p.housing.copy(regionalMarkets = swapped)

    err.getMessage.should(include("regionalMarkets must preserve region order"))
  }

  it should "reject out-of-range regional value shares" in {
    val malformed = p.housing.regionalMarkets.updated(
      0,
      p.housing.regionalMarkets.head.copy(valueShare = Share.decimal(-1, 2)),
    )
    val err       = intercept[IllegalArgumentException]:
      p.housing.copy(regionalMarkets = malformed)

    err.getMessage.should(include("regionalMarkets valueShare for Warsaw must be in [0,1]"))
  }

  it should "reject out-of-range regional mortgage shares" in {
    val malformed = p.housing.regionalMarkets.updated(
      0,
      p.housing.regionalMarkets.head.copy(mortgageShare = Share.decimal(101, 2)),
    )
    val err       = intercept[IllegalArgumentException]:
      p.housing.copy(regionalMarkets = malformed)

    err.getMessage.should(include("regionalMarkets mortgageShare for Warsaw must be in [0,1]"))
  }

  it should "reject regional value shares that do not sum to 1.0" in {
    val malformed = p.housing.regionalMarkets.updated(
      0,
      p.housing.regionalMarkets.head.copy(valueShare = Share.decimal(20, 2)),
    )
    val err       = intercept[IllegalArgumentException]:
      p.housing.copy(regionalMarkets = malformed)

    err.getMessage.should(include("regionalMarkets value shares must sum to 1.0"))
  }

  it should "reject regional mortgage shares that do not sum to 1.0" in {
    val malformed = p.housing.regionalMarkets.updated(
      0,
      p.housing.regionalMarkets.head.copy(mortgageShare = Share.decimal(20, 2)),
    )
    val err       = intercept[IllegalArgumentException]:
      p.housing.copy(regionalMarkets = malformed)

    err.getMessage.should(include("regionalMarkets mortgage shares must sum to 1.0"))
  }

  "Config" should "have FirmsCount = 10000 by default" in {
    // Only true when FIRMS_COUNT env var is unset
    if sys.env.get("FIRMS_COUNT").isEmpty then p.pop.firmsCount shouldBe 10000
  }

  it should "have WorkersPerFirm = 10" in {
    p.pop.workersPerFirm shouldBe 10
  }

  it should "have positive AI and Hybrid CAPEX" in {
    p.firm.aiCapex.bd should be > BigDecimal(0)
    p.firm.hybridCapex.bd should be > BigDecimal(0)
  }

  "sigmaThreshold" should "return ~0.91 for sigma=2" in
    Firm.sigmaThreshold(Sigma(2)).bd.shouldBe(BigDecimal("0.9026") +- BigDecimal("0.01"))

  it should "return ~0.955 for sigma=5" in
    Firm.sigmaThreshold(Sigma(5)).bd.shouldBe(BigDecimal("0.9324") +- BigDecimal("0.01"))

  it should "return ~0.955 for sigma=10" in
    Firm.sigmaThreshold(Sigma(10)).bd.shouldBe(BigDecimal("0.955") +- BigDecimal("0.01"))

  it should "be capped at 1.0 for sigma=50" in {
    Firm.sigmaThreshold(Sigma(50)).bd.should(be <= BigDecimal("1.0"))
    Firm.sigmaThreshold(Sigma(50)).bd.shouldBe(BigDecimal("1.0") +- BigDecimal("0.01"))
  }
