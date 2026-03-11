package sfc.engine

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sfc.engine.mechanisms.Macroprudential
import sfc.types.*

/** Property-based tests for macroprudential instruments. */
class MacroprudentialPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  import sfc.config.SimParams
  given SimParams                                                         = SimParams.defaults
  private val p: SimParams                                                = summon[SimParams]
  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  "CCyB (internal)" should "always be in [0, CcybMax]" in
    forAll(
      Gen.choose(0.0, 0.025),
      Gen.choose(-0.10, 0.10),
      Gen.choose(0.0, 1.0),
      Gen.choose(0.0, 1e10),
      Gen.choose(1.0, 1e9),
    ) { (prevCcyb, prevGap, prevTrend, totalLoans, gdp) =>
      val prev   = Macroprudential.State(Rate(prevCcyb), prevGap, prevTrend)
      val result = Macroprudential.stepImpl(prev, totalLoans, gdp)
      result.ccyb.toDouble should be >= 0.0
      result.ccyb.toDouble should be <= p.banking.ccybMax.toDouble
    }

  "effectiveMinCarImpl" should "be >= base MinCar for all banks" in
    forAll(Gen.choose(0, 6), Gen.choose(0.0, 0.025)) { (bankId, ccyb) =>
      val eff = Macroprudential.effectiveMinCarImpl(bankId, ccyb)
      eff should be >= p.banking.minCar.toDouble
    }

  it should "be monotonically increasing in CCyB" in
    forAll(Gen.choose(0, 6), Gen.choose(0.0, 0.01), Gen.choose(0.005, 0.025)) { (bankId, ccyb1, delta) =>
      val ccyb2 = ccyb1 + delta
      val eff1  = Macroprudential.effectiveMinCarImpl(bankId, ccyb1)
      val eff2  = Macroprudential.effectiveMinCarImpl(bankId, ccyb2)
      eff2 should be >= eff1
    }
