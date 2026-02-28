package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen

/** Phase 8A: Property-based tests for interbank term structure. */
class YieldCurvePropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:
  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  "YieldCurve" should "always maintain O/N < 1M < 3M < 6M ordering" in {
    forAll(Gen.choose(0.0, 0.30)) { onRate =>
      val curve = YieldCurve.compute(onRate)
      curve.overnight should be < curve.wibor1m
      curve.wibor1m should be < curve.wibor3m
      curve.wibor3m should be < curve.wibor6m
    }
  }

  it should "produce non-negative term rates when O/N >= 0" in {
    forAll(Gen.choose(0.0, 0.30)) { onRate =>
      val curve = YieldCurve.compute(onRate)
      curve.overnight should be >= 0.0
      curve.wibor1m should be >= 0.0
      curve.wibor3m should be >= 0.0
      curve.wibor6m should be >= 0.0
    }
  }

  it should "have fixed term premiums regardless of O/N rate" in {
    forAll(Gen.choose(0.0, 0.30)) { onRate =>
      val curve = YieldCurve.compute(onRate)
      (curve.wibor1m - curve.overnight) shouldBe (YieldCurve.TermPremium1M +- 1e-12)
      (curve.wibor3m - curve.overnight) shouldBe (YieldCurve.TermPremium3M +- 1e-12)
      (curve.wibor6m - curve.overnight) shouldBe (YieldCurve.TermPremium6M +- 1e-12)
    }
  }
