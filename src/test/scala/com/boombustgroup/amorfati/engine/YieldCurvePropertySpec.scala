package com.boombustgroup.amorfati.engine

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.boombustgroup.amorfati.engine.mechanisms.YieldCurve
import com.boombustgroup.amorfati.types.*

/** Property-based tests for interbank term structure. */
class YieldCurvePropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams                                                         = SimParams.defaults
  private val td                                                          = ComputationBoundary
  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  "YieldCurve" should "always maintain O/N < 1M < 3M < 6M ordering" in
    forAll(Gen.choose(0.0, 0.30)) { onRate =>
      val curve = YieldCurve.compute(Rate(onRate))
      curve.overnight should be < curve.wibor1m
      curve.wibor1m should be < curve.wibor3m
      curve.wibor3m should be < curve.wibor6m
    }

  it should "produce non-negative term rates when O/N >= 0" in
    forAll(Gen.choose(0.0, 0.30)) { onRate =>
      val curve = YieldCurve.compute(Rate(onRate))
      curve.overnight should be >= Rate.Zero
      curve.wibor1m should be >= Rate.Zero
      curve.wibor3m should be >= Rate.Zero
      curve.wibor6m should be >= Rate.Zero
    }

  it should "have base premiums when no stress (nplRatio=0, credibility=1)" in
    forAll(Gen.choose(0.0, 0.30)) { onRate =>
      val curve = YieldCurve.compute(Rate(onRate))
      td.toDouble(curve.wibor1m - curve.overnight) should be(td.toDouble(YieldCurve.BasePremium1M) +- 1e-12)
      td.toDouble(curve.wibor3m - curve.overnight) should be(td.toDouble(YieldCurve.BasePremium3M) +- 1e-12)
      td.toDouble(curve.wibor6m - curve.overnight) should be(td.toDouble(YieldCurve.BasePremium6M) +- 1e-12)
    }

  it should "widen monotonically with NPL" in
    forAll(Gen.choose(0.0, 0.30), Gen.choose(0.0, 0.20)) { (onRate, npl) =>
      val calm     = YieldCurve.compute(Rate(onRate))
      val stressed = YieldCurve.compute(Rate(onRate), nplRatio = Share(npl))
      stressed.wibor3m should be >= calm.wibor3m
    }
