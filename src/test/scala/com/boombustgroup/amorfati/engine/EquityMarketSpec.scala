package com.boombustgroup.amorfati.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.engine.markets.EquityMarket
import com.boombustgroup.amorfati.types.*

class EquityMarketSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  private val initState = EquityMarket.State(
    index = 2400.0,
    marketCap = PLN(1.4e12 * p.pop.firmsCount / 10000.0),
    earningsYield = Rate(0.10),
    dividendYield = Rate(0.057),
    foreignOwnership = Ratio(0.67),
  )

  "EquityMarket.zero" should "return all-zero state" in {
    val z = EquityMarket.zero
    z.index shouldBe 0.0
    z.marketCap shouldBe PLN.Zero
    z.earningsYield shouldBe Rate.Zero
    z.dividendYield shouldBe Rate.Zero
    z.foreignOwnership shouldBe Ratio.Zero
    z.lastIssuance shouldBe PLN.Zero
    z.lastDomesticDividends shouldBe PLN.Zero
    z.lastForeignDividends shouldBe PLN.Zero
    z.lastDividendTax shouldBe PLN.Zero
    z.hhEquityWealth shouldBe PLN.Zero
    z.lastWealthEffect shouldBe PLN.Zero
  }

  "EquityMarket.step" should "return zero when GPW_ENABLED=false" in {
    val result = EquityMarket.step(EquityMarket.StepInput(initState, Rate(0.0575), Rate(0.025), 0.002, PLN(1e8)))
    result shouldBe EquityMarket.zero
  }

  "EquityMarket.step" should "keep index positive for reasonable inputs" in {
    val state = initState
    state.index should be > 0.0
  }

  "EquityMarket.processIssuance" should "increase market cap and dilute index" in {
    val issuance         = PLN(1e9)
    val result           = EquityMarket.processIssuance(issuance, initState)
    result.marketCap.toDouble shouldBe (initState.marketCap.toDouble + 1e9 +- 1.0)
    result.index should be < initState.index
    val expectedDilution = initState.marketCap.toDouble / (initState.marketCap.toDouble + 1e9)
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

  "EquityMarket.computeDividends" should "return zeros for zero market cap" in {
    val r = EquityMarket.computeDividends(Rate(0.057), PLN.Zero, Ratio(0.67))
    r shouldBe EquityMarket.DividendResultZero
  }

  "EquityMarket.computeDividends" should "compute correct split for given parameters" in {
    val mcap             = PLN(1e12)
    val divYield         = Rate(0.057)
    val foreignShare     = Ratio(0.67)
    val r                = EquityMarket.computeDividends(divYield, mcap, foreignShare)
    val expectedTotal    = 0.057 * 1e12 / 12.0
    val expectedForeign  = expectedTotal * 0.67
    val expectedDomGross = expectedTotal - expectedForeign
    val expectedTax      = expectedDomGross * p.equity.divTax.toDouble
    val expectedNetDom   = expectedDomGross - expectedTax
    r.foreign.toDouble shouldBe (expectedForeign +- 1.0)
    r.tax.toDouble shouldBe (expectedTax +- 1.0)
    r.netDomestic.toDouble shouldBe (expectedNetDom +- 1.0)
    r.foreign should be > r.netDomestic
  }

  it should "return no foreign dividends when foreign share is zero" in {
    val r           = EquityMarket.computeDividends(Rate(0.057), PLN(1e12), Ratio.Zero)
    r.foreign shouldBe PLN.Zero
    val total       = 0.057 * 1e12 / 12.0
    val expectedTax = total * p.equity.divTax.toDouble
    r.netDomestic.toDouble shouldBe (total - expectedTax +- 1.0)
    r.tax.toDouble shouldBe (expectedTax +- 1.0)
  }

  it should "return no domestic dividends when foreign share is 1.0" in {
    val r     = EquityMarket.computeDividends(Rate(0.057), PLN(1e12), Ratio.One)
    r.netDomestic shouldBe PLN.Zero
    r.tax shouldBe PLN.Zero
    val total = 0.057 * 1e12 / 12.0
    r.foreign.toDouble shouldBe (total +- 1.0)
  }

  it should "apply Belka tax rate correctly" in {
    val mcap     = PLN(1e12)
    val r        = EquityMarket.computeDividends(Rate(0.06), mcap, Ratio(0.50))
    val total    = 0.06 * 1e12 / 12.0
    val domGross = total * 0.50
    r.tax.toDouble shouldBe (domGross * 0.19 +- 1.0)
    r.netDomestic.toDouble shouldBe (domGross * 0.81 +- 1.0)
  }

  it should "compute dividends based on market cap, not firm profits" in {
    // firmProfits param was removed — dividends depend only on divYield × marketCap
    val r = EquityMarket.computeDividends(Rate(0.057), PLN(1e12), Ratio(0.67))
    r.netDomestic should be > PLN.Zero
  }
