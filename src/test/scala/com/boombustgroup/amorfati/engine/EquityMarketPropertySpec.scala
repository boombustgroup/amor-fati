package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.Generators.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.markets.EquityMarket
import com.boombustgroup.amorfati.types.*
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class EquityMarketPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given SimParams = SimParams.defaults

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  private def shouldBeClosePln(actual: PLN, expected: PLN, tolerance: PLN): Unit =
    (actual - expected).abs should be <= tolerance

  private val genEquityState: Gen[EquityMarket.State] = for
    index   <- genDecimal("100.0", "10000.0")
    mcap    <- genDecimal("1e9", "1e13")
    ey      <- genDecimal("0.01", "0.50")
    dy      <- genDecimal("0.01", "0.15")
    foreign <- genDecimal("0.0", "1.0")
  yield EquityMarket.State(
    index = priceIndexBD(index),
    marketCap = plnBD(mcap),
    earningsYield = rateBD(ey),
    dividendYield = rateBD(dy),
    foreignOwnership = shareBD(foreign),
  )

  "EquityMarket.processIssuance" should "always increase market cap for positive issuance" in
    forAll(genEquityState, genDecimal("1.0", "1e10")) { (state, amount) =>
      val result = EquityMarket.processIssuance(plnBD(amount), state)
      result.marketCap should be >= state.marketCap
    }

  it should "always decrease or maintain index (dilution)" in
    forAll(genEquityState, genDecimal("1.0", "1e10")) { (state, amount) =>
      val result = EquityMarket.processIssuance(plnBD(amount), state)
      result.index should be <= state.index
    }

  it should "preserve dilution formula exactly in typed algebra" in
    forAll(genEquityState, genDecimal("1.0", "1e10")) { (state, amount) =>
      whenever(state.marketCap > PLN.Zero) {
        val issuance      = plnBD(amount)
        val result        = EquityMarket.processIssuance(issuance, state)
        val expectedIndex = state.index * state.marketCap.ratioTo(state.marketCap + issuance).toMultiplier
        result.index shouldBe expectedIndex
      }
    }

  "EquityMarket.computeDividends" should "have non-negative outputs for positive inputs" in
    forAll(genDecimal("1e6", "1e13"), genFraction) { (profits, foreignShare) =>
      val r = EquityMarket.computeDividends(plnBD(profits), shareBD(foreignShare), PLN.Zero, Share.Zero)
      r.netDomestic should be >= PLN.Zero
      r.foreign should be >= PLN.Zero
      r.tax should be >= PLN.Zero
      r.gov should be >= PLN.Zero
    }

  it should "conserve payout-scaled profits exactly" in
    forAll(genDecimal("1e6", "1e13"), genDecimal("0.0", "1.0")) { (profits, foreignShare) =>
      whenever(foreignShare >= BigDecimal("0.0")) {
        val r             = EquityMarket.computeDividends(plnBD(profits), shareBD(foreignShare), PLN.Zero, Share.Zero)
        val expectedTotal = plnBD(profits) * Share.decimal(57, 2)
        (r.netDomestic + r.tax + r.foreign) shouldBe expectedTotal
      }
    }

  it should "preserve the baseline split while adding a non-negative SOE government leg" in
    forAll(genDecimal("1e8", "1e13"), genFraction, genDecimal("0.01", "1.0"), genDecimal("0.04", "0.20")) { (profits, foreignShare, soeShare, deficitToGdp) =>
      whenever(foreignShare >= BigDecimal("0.0") && foreignShare <= BigDecimal("1.0")) {
        val baseline = EquityMarket.computeDividends(plnBD(profits), shareBD(foreignShare), PLN.Zero, Share.Zero)
        val withGov  = EquityMarket.computeDividends(plnBD(profits), shareBD(foreignShare), plnBD(profits * soeShare), shareBD(deficitToGdp))
        val payout   = plnBD(profits) * Share.decimal(57, 2)

        withGov.netDomestic shouldBe baseline.netDomestic
        withGov.foreign shouldBe baseline.foreign
        withGov.tax shouldBe baseline.tax
        (withGov.netDomestic + withGov.tax + withGov.foreign) shouldBe payout
        withGov.gov should be >= PLN.Zero
      }
    }

  it should "have foreign dividends <= total dividends" in
    forAll(genDecimal("1e6", "1e13"), genFraction) { (profits, foreignShare) =>
      val r     = EquityMarket.computeDividends(plnBD(profits), shareBD(foreignShare), PLN.Zero, Share.Zero)
      val total = plnBD(profits) * Share.decimal(57, 2)
      r.foreign should be <= total
    }

  it should "have dividend tax <= domestic gross" in
    forAll(genDecimal("1e6", "1e13"), genFraction) { (profits, foreignShare) =>
      val gross = plnBD(profits) * Share.decimal(57, 2) * (Share.One - shareBD(foreignShare))
      val r     = EquityMarket.computeDividends(plnBD(profits), shareBD(foreignShare), PLN.Zero, Share.Zero)
      r.tax should be <= gross
    }

  it should "scale dividends linearly with realized profits up to rounding" in
    forAll(genDecimal("1e6", "1e12"), genFraction) { (profits, foreignShare) =>
      val r1 = EquityMarket.computeDividends(plnBD(profits), shareBD(foreignShare), PLN.Zero, Share.Zero)
      val r2 = EquityMarket.computeDividends(plnBD(profits * BigDecimal("2.0")), shareBD(foreignShare), PLN.Zero, Share.Zero)
      whenever(r1.netDomestic > PLN.Zero && r1.foreign > PLN.Zero && r1.tax > PLN.Zero) {
        shouldBeClosePln(r2.netDomestic, r1.netDomestic * 2, PLN(1))
        shouldBeClosePln(r2.foreign, r1.foreign * 2, PLN(1))
        shouldBeClosePln(r2.tax, r1.tax * 2, PLN(1))
      }
    }

  "EquityMarket.zero" should "have all fields equal to zero" in {
    val z = EquityMarket.zero
    z.index shouldBe PriceIndex.Zero
    z.marketCap shouldBe PLN.Zero
    z.earningsYield shouldBe Rate.Zero
    z.dividendYield shouldBe Rate.Zero
    z.foreignOwnership shouldBe Share.Zero
    z.lastIssuance shouldBe PLN.Zero
    z.lastWealthEffect shouldBe PLN.Zero
  }
