package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.boombustgroup.amorfati.engine.mechanisms.YieldCurve
import com.boombustgroup.amorfati.types.*

/** Property-based tests for interbank term structure. */
class YieldCurvePropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams                                                         = SimParams.defaults
  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  "YieldCurve" should "always maintain O/N < 1M < 3M < 6M ordering" in
    forAll(genDecimal("0.0", "0.30")) { onRate =>
      val curve = YieldCurve.compute(rateBD(onRate))
      curve.overnight should be < curve.wibor1m
      curve.wibor1m should be < curve.wibor3m
      curve.wibor3m should be < curve.wibor6m
    }

  it should "produce non-negative term rates when O/N >= 0" in
    forAll(genDecimal("0.0", "0.30")) { onRate =>
      val curve = YieldCurve.compute(rateBD(onRate))
      curve.overnight should be >= Rate.Zero
      curve.wibor1m should be >= Rate.Zero
      curve.wibor3m should be >= Rate.Zero
      curve.wibor6m should be >= Rate.Zero
    }

  it should "have base premiums when no stress (nplRatio=0, credibility=1)" in
    forAll(genDecimal("0.0", "0.30")) { onRate =>
      val curve = YieldCurve.compute(rateBD(onRate))
      decimal(curve.wibor1m - curve.overnight) should be(decimal(YieldCurve.BasePremium1M) +- BigDecimal("1e-12"))
      decimal(curve.wibor3m - curve.overnight) should be(decimal(YieldCurve.BasePremium3M) +- BigDecimal("1e-12"))
      decimal(curve.wibor6m - curve.overnight) should be(decimal(YieldCurve.BasePremium6M) +- BigDecimal("1e-12"))
    }

  it should "widen monotonically with NPL" in
    forAll(genDecimal("0.0", "0.30"), genDecimal("0.0", "0.20")) { (onRate, npl) =>
      val calm     = YieldCurve.compute(rateBD(onRate))
      val stressed = YieldCurve.compute(rateBD(onRate), nplRatio = shareBD(npl))
      stressed.wibor3m should be >= calm.wibor3m
    }
