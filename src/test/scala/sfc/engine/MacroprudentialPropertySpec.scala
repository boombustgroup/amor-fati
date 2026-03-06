package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen
import sfc.types.*

/** Property-based tests for macroprudential instruments. */
class MacroprudentialPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:
  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  "CCyB (internal)" should "always be in [0, CcybMax]" in {
    forAll(Gen.choose(0.0, 0.025), Gen.choose(-0.10, 0.10), Gen.choose(0.0, 1.0),
           Gen.choose(0.0, 1e10), Gen.choose(1.0, 1e9)) {
      (prevCcyb, prevGap, prevTrend, totalLoans, gdp) =>
        val prev = Macroprudential.State(Rate(prevCcyb), prevGap, prevTrend)
        val result = Macroprudential.stepInternal(prev, totalLoans, gdp)
        result.ccyb.toDouble should be >= 0.0
        result.ccyb.toDouble should be <= sfc.config.Config.CcybMax
    }
  }

  "effectiveMinCarInternal" should "be >= base MinCar for all banks" in {
    forAll(Gen.choose(0, 6), Gen.choose(0.0, 0.025)) {
      (bankId, ccyb) =>
        val eff = Macroprudential.effectiveMinCarInternal(bankId, ccyb)
        eff should be >= sfc.config.Config.MinCar
    }
  }

  it should "be monotonically increasing in CCyB" in {
    forAll(Gen.choose(0, 6), Gen.choose(0.0, 0.01), Gen.choose(0.005, 0.025)) {
      (bankId, ccyb1, delta) =>
        val ccyb2 = ccyb1 + delta
        val eff1 = Macroprudential.effectiveMinCarInternal(bankId, ccyb1)
        val eff2 = Macroprudential.effectiveMinCarInternal(bankId, ccyb2)
        eff2 should be >= eff1
    }
  }
