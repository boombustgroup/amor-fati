package com.boombustgroup.amorfati.engine

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.boombustgroup.amorfati.engine.mechanisms.Macroprudential
import com.boombustgroup.amorfati.types.*

/** Property-based tests for macroprudential instruments. */
class MacroprudentialPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams                                                         = SimParams.defaults
  private val p: SimParams                                                = summon[SimParams]
  private val td                                                          = ComputationBoundary
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
      val prev   = Macroprudential.State(Multiplier(prevCcyb), Coefficient(prevGap), Multiplier(prevTrend))
      val result = Macroprudential.stepImpl(prev, PLN(totalLoans), PLN(gdp))
      td.toDouble(result.ccyb) should be >= 0.0
      td.toDouble(result.ccyb) should be <= td.toDouble(p.banking.ccybMax)
    }

  "effectiveMinCarImpl" should "be >= base MinCar for all banks" in
    forAll(Gen.choose(0, 6), Gen.choose(0.0, 0.025)) { (bankId, ccyb) =>
      val eff = Macroprudential.effectiveMinCarImpl(bankId, Multiplier(ccyb))
      eff should be >= p.banking.minCar
    }

  it should "be monotonically increasing in CCyB" in
    forAll(Gen.choose(0, 6), Gen.choose(0.0, 0.01), Gen.choose(0.005, 0.025)) { (bankId, ccyb1, delta) =>
      val ccyb2 = ccyb1 + delta
      val eff1  = Macroprudential.effectiveMinCarImpl(bankId, Multiplier(ccyb1))
      val eff2  = Macroprudential.effectiveMinCarImpl(bankId, Multiplier(ccyb2))
      eff2 should be >= eff1
    }
