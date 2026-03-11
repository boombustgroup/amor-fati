package sfc.engine

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sfc.Generators.*
import sfc.engine.markets.EquityMarket
import sfc.types.*

class EquityMarketPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  import sfc.config.SimParams
  given SimParams = SimParams.defaults

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
    foreignOwnership = Ratio(foreign),
  )

  // --- processIssuance properties ---

  "EquityMarket.processIssuance" should "always increase market cap for positive issuance" in
    forAll(genEquityState, Gen.choose(1.0, 1e10)) { (state, amount) =>
      val result = EquityMarket.processIssuance(PLN(amount), state)
      result.marketCap.toDouble should be >= state.marketCap.toDouble
    }

  it should "always decrease or maintain index (dilution)" in
    forAll(genEquityState, Gen.choose(1.0, 1e10)) { (state, amount) =>
      val result = EquityMarket.processIssuance(PLN(amount), state)
      result.index should be <= state.index
    }

  it should "preserve index × (1 + amount/mcap) ≈ old index relationship" in
    forAll(genEquityState, Gen.choose(1.0, 1e10)) { (state, amount) =>
      whenever(state.marketCap > PLN.Zero) {
        val result        = EquityMarket.processIssuance(PLN(amount), state)
        val expectedIndex = state.index * state.marketCap.toDouble / (state.marketCap.toDouble + amount)
        result.index shouldBe (expectedIndex +- 1.0)
      }
    }

  // --- computeDividends properties ---

  "EquityMarket.computeDividends" should "have non-negative outputs for positive inputs" in
    forAll(Gen.choose(0.01, 0.15), Gen.choose(1e6, 1e13), genFraction) { (divYield, mcap, foreignShare) =>
      val r = EquityMarket.computeDividends(Rate(divYield), PLN(mcap), Ratio(foreignShare))
      r.netDomestic should be >= PLN.Zero
      r.foreign should be >= PLN.Zero
      r.tax should be >= PLN.Zero
    }

  it should "have domestic + foreign + tax ≈ total dividends" in
    forAll(Gen.choose(0.01, 0.15), Gen.choose(1e6, 1e13), Gen.choose(0.0, 1.0)) { (divYield, mcap, foreignShare) =>
      val r             = EquityMarket.computeDividends(Rate(divYield), PLN(mcap), Ratio(foreignShare))
      val expectedTotal = divYield * mcap / 12.0
      (r.netDomestic + r.tax + r.foreign).toDouble shouldBe (expectedTotal +- 1.0)
    }

  // --- Additional dividend properties ---

  it should "have foreign dividends ≤ total dividends" in
    forAll(Gen.choose(0.01, 0.15), Gen.choose(1e6, 1e13), genFraction) { (divYield, mcap, foreignShare) =>
      val r     = EquityMarket.computeDividends(Rate(divYield), PLN(mcap), Ratio(foreignShare))
      val total = divYield * mcap / 12.0
      r.foreign.toDouble should be <= (total + 1.0)
    }

  it should "have dividend tax ≤ domestic gross" in
    forAll(Gen.choose(0.01, 0.15), Gen.choose(1e6, 1e13), genFraction) { (divYield, mcap, foreignShare) =>
      val r        = EquityMarket.computeDividends(Rate(divYield), PLN(mcap), Ratio(foreignShare))
      val total    = divYield * mcap / 12.0
      val domGross = total * (1.0 - foreignShare)
      r.tax.toDouble should be <= (domGross + 1.0)
    }

  it should "scale dividends linearly with market cap" in
    forAll(Gen.choose(1e6, 1e12), Gen.choose(0.01, 0.15), genFraction) { (mcap, divYield, foreignShare) =>
      val r1 = EquityMarket.computeDividends(Rate(divYield), PLN(mcap), Ratio(foreignShare))
      val r2 = EquityMarket.computeDividends(Rate(divYield), PLN(mcap * 2.0), Ratio(foreignShare))
      whenever(r1.netDomestic > PLN(1e-6) && r1.foreign > PLN(1e-6) && r1.tax > PLN(1e-6)) {
        (r2.netDomestic.toDouble / r1.netDomestic.toDouble) shouldBe (2.0 +- 1e-6)
        (r2.foreign.toDouble / r1.foreign.toDouble) shouldBe (2.0 +- 1e-6)
        (r2.tax.toDouble / r1.tax.toDouble) shouldBe (2.0 +- 1e-6)
      }
    }

  // --- Zero state invariant ---

  "EquityMarket.zero" should "have all fields equal to zero" in {
    val z = EquityMarket.zero
    z.index shouldBe 0.0
    z.marketCap shouldBe PLN.Zero
    z.earningsYield shouldBe Rate.Zero
    z.dividendYield shouldBe Rate.Zero
    z.foreignOwnership shouldBe Ratio.Zero
    z.lastIssuance shouldBe PLN.Zero
    z.hhEquityWealth shouldBe PLN.Zero
    z.lastWealthEffect shouldBe PLN.Zero
  }
