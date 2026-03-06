package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen
import sfc.testutil.Generators.*
import sfc.types.*

class EquityMarketPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  private val genEquityState: Gen[EquityMarket.State] = for
    index    <- Gen.choose(100.0, 10000.0)
    mcap     <- Gen.choose(1e9, 1e13)
    ey       <- Gen.choose(0.01, 0.50)
    dy       <- Gen.choose(0.01, 0.15)
    foreign  <- Gen.choose(0.0, 1.0)
  yield EquityMarket.State(index, PLN(mcap), Rate(ey), Rate(dy), Ratio(foreign), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)

  // --- processIssuance properties ---

  "EquityMarket.processIssuance" should "always increase market cap for positive issuance" in {
    forAll(genEquityState, Gen.choose(1.0, 1e10)) { (state, amount) =>
      val result = EquityMarket.processIssuance(amount, state)
      result.marketCap.toDouble should be >= state.marketCap.toDouble
    }
  }

  it should "always decrease or maintain index (dilution)" in {
    forAll(genEquityState, Gen.choose(1.0, 1e10)) { (state, amount) =>
      val result = EquityMarket.processIssuance(amount, state)
      result.index should be <= state.index
    }
  }

  it should "preserve index × (1 + amount/mcap) ≈ old index relationship" in {
    forAll(genEquityState, Gen.choose(1.0, 1e10)) { (state, amount) =>
      whenever(state.marketCap > PLN.Zero) {
        val result = EquityMarket.processIssuance(amount, state)
        val expectedIndex = state.index * state.marketCap.toDouble / (state.marketCap.toDouble + amount)
        result.index shouldBe (expectedIndex +- 1.0)
      }
    }
  }

  // --- computeDividends properties ---

  "EquityMarket.computeDividends" should "have non-negative outputs for positive inputs" in {
    forAll(Gen.choose(0.0, 1e10), Gen.choose(0.01, 0.15), Gen.choose(1e6, 1e13), genFraction) {
      (profits, divYield, mcap, foreignShare) =>
        val (dom, foreign, tax) = EquityMarket.computeDividends(profits, divYield, mcap, foreignShare)
        dom should be >= 0.0
        foreign should be >= 0.0
        tax should be >= 0.0
    }
  }

  it should "have domestic + foreign + tax ≈ total dividends" in {
    forAll(Gen.choose(0.0, 1e10), Gen.choose(0.01, 0.15), Gen.choose(1e6, 1e13), Gen.choose(0.0, 1.0)) {
      (profits, divYield, mcap, foreignShare) =>
        val (netDom, foreign, tax) = EquityMarket.computeDividends(profits, divYield, mcap, foreignShare)
        val expectedTotal = divYield * mcap / 12.0
        // netDom + tax + foreign = total
        (netDom + tax + foreign) shouldBe (expectedTotal +- 1.0)
    }
  }

  // --- Additional dividend properties ---

  it should "have foreign dividends ≤ total dividends" in {
    forAll(Gen.choose(0.0, 1e10), Gen.choose(0.01, 0.15), Gen.choose(1e6, 1e13), genFraction) {
      (profits, divYield, mcap, foreignShare) =>
        val (_, foreign, _) = EquityMarket.computeDividends(profits, divYield, mcap, foreignShare)
        val total = divYield * mcap / 12.0
        foreign should be <= (total + 1.0)
    }
  }

  it should "have dividend tax ≤ domestic gross" in {
    forAll(Gen.choose(0.0, 1e10), Gen.choose(0.01, 0.15), Gen.choose(1e6, 1e13), genFraction) {
      (profits, divYield, mcap, foreignShare) =>
        val (_, _, tax) = EquityMarket.computeDividends(profits, divYield, mcap, foreignShare)
        val total = divYield * mcap / 12.0
        val domGross = total * (1.0 - foreignShare)
        tax should be <= (domGross + 1.0)
    }
  }

  it should "scale dividends linearly with market cap" in {
    forAll(Gen.choose(1e6, 1e12), Gen.choose(0.01, 0.15), genFraction) {
      (mcap, divYield, foreignShare) =>
        val (d1, f1, t1) = EquityMarket.computeDividends(1e8, divYield, mcap, foreignShare)
        val (d2, f2, t2) = EquityMarket.computeDividends(1e8, divYield, mcap * 2.0, foreignShare)
        whenever(d1 > 1e-6 && f1 > 1e-6 && t1 > 1e-6) {
          (d2 / d1) shouldBe (2.0 +- 1e-6)
          (f2 / f1) shouldBe (2.0 +- 1e-6)
          (t2 / t1) shouldBe (2.0 +- 1e-6)
        }
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
