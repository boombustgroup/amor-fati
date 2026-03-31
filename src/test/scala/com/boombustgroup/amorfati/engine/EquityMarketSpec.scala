package com.boombustgroup.amorfati.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.engine.markets.EquityMarket
import com.boombustgroup.amorfati.types.*

class EquityMarketSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]
  private val td           = ComputationBoundary

  private val initState = EquityMarket.State(
    index = 2400.0,
    marketCap = PLN(1.4e12 * p.pop.firmsCount / 10000.0),
    earningsYield = Rate(0.10),
    dividendYield = Rate(0.057),
    foreignOwnership = Share(0.67),
  )

  "EquityMarket.zero" should "return all-zero state" in {
    val z = EquityMarket.zero
    z.index shouldBe 0.0
    z.marketCap shouldBe PLN.Zero
    z.earningsYield shouldBe Rate.Zero
    z.dividendYield shouldBe Rate.Zero
    z.foreignOwnership shouldBe Share.Zero
    z.lastIssuance shouldBe PLN.Zero
    z.lastDomesticDividends shouldBe PLN.Zero
    z.lastForeignDividends shouldBe PLN.Zero
    z.lastDividendTax shouldBe PLN.Zero
    z.hhEquityWealth shouldBe PLN.Zero
    z.lastWealthEffect shouldBe PLN.Zero
  }

  "EquityMarket.step" should "keep index positive for reasonable inputs" in {
    val state = initState
    state.index should be > 0.0
  }

  "EquityMarket.processIssuance" should "increase market cap and dilute index" in {
    val issuance         = PLN(1e9)
    val result           = EquityMarket.processIssuance(issuance, initState)
    td.toDouble(result.marketCap) shouldBe (td.toDouble(initState.marketCap) + 1e9 +- 1.0)
    result.index should be < initState.index
    val expectedDilution = td.toDouble(initState.marketCap) / (td.toDouble(initState.marketCap) + 1e9)
    result.index shouldBe (initState.index * expectedDilution +- 0.01)
  }

  "EquityMarket.processIssuance" should "return unchanged state for zero amount" in {
    val result = EquityMarket.processIssuance(PLN.Zero, initState)
    result shouldBe initState
  }

  "EquityMarket.processIssuance" should "return unchanged state for negative amount" in {
    val result = EquityMarket.processIssuance(PLN(-100.0), initState)
    result.index shouldBe initState.index
    result.marketCap shouldBe initState.marketCap
  }

  "EquityMarket.processIssuance" should "track lastIssuance amount" in {
    val result = EquityMarket.processIssuance(PLN(5e8), initState)
    result.lastIssuance shouldBe PLN(5e8)
  }

  "EquityMarket.computeDividends" should "return zeros for zero realized profits" in {
    val r = EquityMarket.computeDividends(PLN.Zero, Share(0.67))
    r shouldBe EquityMarket.DividendResultZero
  }

  "EquityMarket.computeDividends" should "compute correct split from realized profits and payout ratio" in {
    val profits          = PLN(1e12)
    val foreignShare     = Share(0.67)
    val r                = EquityMarket.computeDividends(profits, foreignShare)
    val expectedTotal    = 1e12 * 0.57
    val expectedForeign  = expectedTotal * 0.67
    val expectedDomGross = expectedTotal - expectedForeign
    val expectedTax      = expectedDomGross * td.toDouble(p.equity.divTax)
    val expectedNetDom   = expectedDomGross - expectedTax
    td.toDouble(r.foreign) shouldBe (expectedForeign +- 1e8)
    td.toDouble(r.tax) shouldBe (expectedTax +- 1e8)
    td.toDouble(r.netDomestic) shouldBe (expectedNetDom +- 1e8)
    r.foreign should be > r.netDomestic
  }

  it should "return no foreign dividends when foreign share is zero" in {
    val r           = EquityMarket.computeDividends(PLN(1e12), Share.Zero)
    r.foreign shouldBe PLN.Zero
    val total       = 1e12 * 0.57
    val expectedTax = total * td.toDouble(p.equity.divTax)
    td.toDouble(r.netDomestic) shouldBe (total - expectedTax +- 1e8)
    td.toDouble(r.tax) shouldBe (expectedTax +- 1e8)
  }

  it should "return no domestic dividends when foreign share is 1.0" in {
    val r     = EquityMarket.computeDividends(PLN(1e12), Share.One)
    r.netDomestic shouldBe PLN.Zero
    r.tax shouldBe PLN.Zero
    val total = 1e12 * 0.57
    td.toDouble(r.foreign) shouldBe (total +- 1e8)
  }

  it should "apply Belka tax rate correctly" in {
    val profits  = PLN(1e12)
    val r        = EquityMarket.computeDividends(profits, Share(0.50))
    val total    = 1e12 * 0.57
    val domGross = total * 0.50
    td.toDouble(r.tax) shouldBe (domGross * 0.19 +- 1.0)
    td.toDouble(r.netDomestic) shouldBe (domGross * 0.81 +- 1.0)
  }

  it should "compute dividends based on realized profits" in {
    val r = EquityMarket.computeDividends(PLN(1e12), Share(0.67))
    r.netDomestic should be > PLN.Zero
  }

  it should "not depend on market valuation directly" in {
    val lowProfits  = EquityMarket.computeDividends(PLN(1e9), Share(0.67))
    val highProfits = EquityMarket.computeDividends(PLN(1e12), Share(0.67))

    highProfits.tax should be > lowProfits.tax
  }
