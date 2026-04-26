package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.markets.EquityMarket
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EquityMarketSpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  private val initState = EquityMarket.State(
    index = PriceIndex(2400),
    marketCap = plnBD(140000000L * p.pop.firmsCount),
    earningsYield = Rate.decimal(10, 2),
    dividendYield = Rate.decimal(57, 3),
    foreignOwnership = Share.decimal(67, 2),
  )

  "EquityMarket.zero" should "return all-zero state" in {
    val z = EquityMarket.zero
    z.index shouldBe PriceIndex.Zero
    z.marketCap shouldBe PLN.Zero
    z.earningsYield shouldBe Rate.Zero
    z.dividendYield shouldBe Rate.Zero
    z.foreignOwnership shouldBe Share.Zero
    z.lastIssuance shouldBe PLN.Zero
    z.lastDomesticDividends shouldBe PLN.Zero
    z.lastForeignDividends shouldBe PLN.Zero
    z.lastDividendTax shouldBe PLN.Zero
    z.lastWealthEffect shouldBe PLN.Zero
  }

  "EquityMarket.step" should "keep index positive for reasonable inputs" in {
    initState.index should be > PriceIndex.Zero
  }

  "EquityMarket.processIssuance" should "increase market cap and dilute index" in {
    val issuance       = PLN(1000000000)
    val result         = EquityMarket.processIssuance(issuance, initState)
    val dilutionFactor = initState.marketCap.ratioTo(initState.marketCap + issuance).toMultiplier
    result.marketCap shouldBe (initState.marketCap + issuance)
    result.index should be < initState.index
    result.index shouldBe (initState.index * dilutionFactor)
  }

  "EquityMarket.processIssuance" should "return unchanged state for zero amount" in {
    val result = EquityMarket.processIssuance(PLN.Zero, initState)
    result shouldBe initState
  }

  "EquityMarket.processIssuance" should "return unchanged state for negative amount" in {
    val result = EquityMarket.processIssuance(PLN(-100), initState)
    result.index shouldBe initState.index
    result.marketCap shouldBe initState.marketCap
  }

  "EquityMarket.processIssuance" should "track lastIssuance amount" in {
    val result = EquityMarket.processIssuance(PLN(500000000), initState)
    result.lastIssuance shouldBe PLN(500000000)
  }

  "EquityMarket.computeDividends" should "return zeros for zero realized profits" in {
    val r = EquityMarket.computeDividends(PLN.Zero, Share.decimal(67, 2), PLN.Zero, Share.Zero)
    r shouldBe EquityMarket.DividendResultZero
  }

  "EquityMarket.computeDividends" should "compute correct split from realized profits and payout ratio" in {
    val profits          = PLN(1000000000000L)
    val foreignShare     = Share.decimal(67, 2)
    val r                = EquityMarket.computeDividends(profits, foreignShare, PLN.Zero, Share.Zero)
    val expectedTotal    = profits * Share.decimal(57, 2)
    val expectedForeign  = expectedTotal * foreignShare
    val expectedDomGross = expectedTotal - expectedForeign
    val expectedTax      = expectedDomGross * p.equity.divTax
    val expectedNetDom   = expectedDomGross - expectedTax
    r.foreign shouldBe expectedForeign
    r.tax shouldBe expectedTax
    r.netDomestic shouldBe expectedNetDom
    r.foreign should be > r.netDomestic
  }

  it should "return no foreign dividends when foreign share is zero" in {
    val r           = EquityMarket.computeDividends(PLN(1000000000000L), Share.Zero, PLN.Zero, Share.Zero)
    val total       = PLN(1000000000000L) * Share.decimal(57, 2)
    val expectedTax = total * p.equity.divTax
    r.foreign shouldBe PLN.Zero
    r.netDomestic shouldBe (total - expectedTax)
    r.tax shouldBe expectedTax
  }

  it should "return no domestic dividends when foreign share is 1.0" in {
    val r     = EquityMarket.computeDividends(PLN(1000000000000L), Share.One, PLN.Zero, Share.Zero)
    val total = PLN(1000000000000L) * Share.decimal(57, 2)
    r.netDomestic shouldBe PLN.Zero
    r.tax shouldBe PLN.Zero
    r.foreign shouldBe total
  }

  it should "apply Belka tax rate correctly" in {
    val profits  = PLN(1000000000000L)
    val r        = EquityMarket.computeDividends(profits, Share.decimal(50, 2), PLN.Zero, Share.Zero)
    val total    = profits * Share.decimal(57, 2)
    val domGross = total * Share.decimal(50, 2)
    r.tax shouldBe (domGross * p.equity.divTax)
    r.netDomestic shouldBe (domGross - (domGross * p.equity.divTax))
  }

  it should "compute dividends based on realized profits" in {
    val r = EquityMarket.computeDividends(PLN(1000000000000L), Share.decimal(67, 2), PLN.Zero, Share.Zero)
    r.netDomestic should be > PLN.Zero
  }

  it should "add a government SOE dividend leg without changing the baseline split" in {
    val profits      = PLN(1000000000000L)
    val foreignShare = Share.decimal(67, 2)
    val baseline     = EquityMarket.computeDividends(profits, foreignShare, PLN.Zero, Share.Zero)
    val r            = EquityMarket.computeDividends(profits, foreignShare, PLN(200000000000L), Share.decimal(6, 2))

    r.gov should be > PLN.Zero
    r.netDomestic shouldBe baseline.netDomestic
    r.foreign shouldBe baseline.foreign
    r.tax shouldBe baseline.tax
  }

  it should "not depend on market valuation directly" in {
    val lowProfits  = EquityMarket.computeDividends(PLN(1000000000), Share.decimal(67, 2), PLN.Zero, Share.Zero)
    val highProfits = EquityMarket.computeDividends(PLN(1000000000000L), Share.decimal(67, 2), PLN.Zero, Share.Zero)

    highProfits.tax should be > lowProfits.tax
  }
