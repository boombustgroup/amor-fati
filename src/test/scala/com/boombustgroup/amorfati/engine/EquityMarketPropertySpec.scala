package com.boombustgroup.amorfati.engine

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.boombustgroup.amorfati.Generators.*
import com.boombustgroup.amorfati.engine.markets.EquityMarket
import com.boombustgroup.amorfati.types.*

class EquityMarketPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams = SimParams.defaults
  private val td  = ComputationBoundary

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  private val genEquityState: Gen[EquityMarket.State] = for
    index   <- Gen.choose(100.0, 10000.0)
    mcap    <- Gen.choose(1e9, 1e13)
    ey      <- Gen.choose(0.01, 0.50)
    dy      <- Gen.choose(0.01, 0.15)
    foreign <- Gen.choose(0.0, 1.0)
  yield EquityMarket.State(
    index = index,
    marketCap = PLN(mcap),
    earningsYield = Rate(ey),
    dividendYield = Rate(dy),
    foreignOwnership = Share(foreign),
  )

  // --- processIssuance properties ---

  "EquityMarket.processIssuance" should "always increase market cap for positive issuance" in
    forAll(genEquityState, Gen.choose(1.0, 1e10)) { (state, amount) =>
      val result = EquityMarket.processIssuance(PLN(amount), state)
      td.toDouble(result.marketCap) should be >= td.toDouble(state.marketCap)
    }

  it should "always decrease or maintain index (dilution)" in
    forAll(genEquityState, Gen.choose(1.0, 1e10)) { (state, amount) =>
      val result = EquityMarket.processIssuance(PLN(amount), state)
      result.index should be <= state.index
    }

  it should "preserve index x (1 + amount/mcap) approx old index relationship" in
    forAll(genEquityState, Gen.choose(1.0, 1e10)) { (state, amount) =>
      whenever(state.marketCap > PLN.Zero) {
        val result        = EquityMarket.processIssuance(PLN(amount), state)
        val expectedIndex = state.index * td.toDouble(state.marketCap) / (td.toDouble(state.marketCap) + amount)
        result.index shouldBe (expectedIndex +- 1.0)
      }
    }

  // --- computeDividends properties ---

  "EquityMarket.computeDividends" should "have non-negative outputs for positive inputs" in
    forAll(Gen.choose(1e6, 1e13), genFraction) { (profits, foreignShare) =>
      val r = EquityMarket.computeDividends(PLN(profits), Share(foreignShare))
      r.netDomestic should be >= PLN.Zero
      r.foreign should be >= PLN.Zero
      r.tax should be >= PLN.Zero
    }

  it should "have domestic + foreign + tax approx payout-scaled profits" in
    forAll(Gen.choose(1e6, 1e13), Gen.choose(0.0, 1.0)) { (profits, foreignShare) =>
      whenever(foreignShare >= 0.0) {
        val r             = EquityMarket.computeDividends(PLN(profits), Share(foreignShare))
        val expectedTotal = profits * 0.57
        val tol           = Math.max(1.0, profits * 0.0001)
        td.toDouble(r.netDomestic + r.tax + r.foreign) shouldBe (expectedTotal +- tol)
      }
    }

  // --- Additional dividend properties ---

  it should "have foreign dividends <= total dividends" in
    forAll(Gen.choose(1e6, 1e13), genFraction) { (profits, foreignShare) =>
      val r     = EquityMarket.computeDividends(PLN(profits), Share(foreignShare))
      val total = profits * 0.57
      val tol   = Math.max(1.0, profits * 0.0001)
      td.toDouble(r.foreign) should be <= (total + tol)
    }

  it should "have dividend tax <= domestic gross" in
    forAll(Gen.choose(1e6, 1e13), genFraction) { (profits, foreignShare) =>
      val r        = EquityMarket.computeDividends(PLN(profits), Share(foreignShare))
      val total    = profits * 0.57
      val domGross = total * (1.0 - foreignShare)
      td.toDouble(r.tax) should be <= (domGross + 1.0)
    }

  it should "scale dividends linearly with realized profits" in
    forAll(Gen.choose(1e6, 1e12), genFraction) { (profits, foreignShare) =>
      val r1 = EquityMarket.computeDividends(PLN(profits), Share(foreignShare))
      val r2 = EquityMarket.computeDividends(PLN(profits * 2.0), Share(foreignShare))
      whenever(r1.netDomestic > PLN(1e-6) && r1.foreign > PLN(1e-6) && r1.tax > PLN(1e-6)) {
        (td.toDouble(r2.netDomestic) / td.toDouble(r1.netDomestic)) shouldBe (2.0 +- 1e-6)
        (td.toDouble(r2.foreign) / td.toDouble(r1.foreign)) shouldBe (2.0 +- 1e-6)
        (td.toDouble(r2.tax) / td.toDouble(r1.tax)) shouldBe (2.0 +- 1e-6)
      }
    }

  // --- Zero state invariant ---

  "EquityMarket.zero" should "have all fields equal to zero" in {
    val z = EquityMarket.zero
    z.index shouldBe 0.0
    z.marketCap shouldBe PLN.Zero
    z.earningsYield shouldBe Rate.Zero
    z.dividendYield shouldBe Rate.Zero
    z.foreignOwnership shouldBe Share.Zero
    z.lastIssuance shouldBe PLN.Zero
    z.hhEquityWealth shouldBe PLN.Zero
    z.lastWealthEffect shouldBe PLN.Zero
  }
