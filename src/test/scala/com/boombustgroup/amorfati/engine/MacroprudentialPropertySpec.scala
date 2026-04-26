package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.boombustgroup.amorfati.engine.mechanisms.Macroprudential

/** Property-based tests for macroprudential instruments. */
class MacroprudentialPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams                                                         = SimParams.defaults
  private val p: SimParams                                                = summon[SimParams]
  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  "CCyB (internal)" should "always be in [0, CcybMax]" in
    forAll(
      genDecimal("0.0", "0.025"),
      genDecimal("-0.10", "0.10"),
      genDecimal("0.0", "1.0"),
      genDecimal("0.0", "1e10"),
      genDecimal("1.0", "1e9"),
    ) { (prevCcyb, prevGap, prevTrend, totalLoans, gdp) =>
      val prev   = Macroprudential.State(multiplierBD(prevCcyb), coefficientBD(prevGap), multiplierBD(prevTrend))
      val result = Macroprudential.stepImpl(prev, plnBD(totalLoans), plnBD(gdp))
      decimal(result.ccyb) should be >= BigDecimal("0.0")
      decimal(result.ccyb) should be <= decimal(p.banking.ccybMax)
    }

  "effectiveMinCarImpl" should "be >= base MinCar for all banks" in
    forAll(Gen.choose(0, 6), genDecimal("0.0", "0.025")) { (bankId, ccyb) =>
      val eff = Macroprudential.effectiveMinCarImpl(bankId, multiplierBD(ccyb))
      eff should be >= p.banking.minCar
    }

  it should "be monotonically increasing in CCyB" in
    forAll(Gen.choose(0, 6), genDecimal("0.0", "0.01"), genDecimal("0.005", "0.025")) { (bankId, ccyb1, delta) =>
      val ccyb2 = ccyb1 + delta
      val eff1  = Macroprudential.effectiveMinCarImpl(bankId, multiplierBD(ccyb1))
      val eff2  = Macroprudential.effectiveMinCarImpl(bankId, multiplierBD(ccyb2))
      eff2 should be >= eff1
    }
