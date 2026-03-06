package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.agents.Banking
import sfc.types.*

/** Interbank term structure tests. */
class YieldCurveSpec extends AnyFlatSpec with Matchers:

  // =========================================================================
  // Compute
  // =========================================================================

  "YieldCurve.compute" should "produce correct premiums from O/N rate" in {
    val curve = YieldCurve.compute(0.058)  // 5.80% O/N
    curve.overnight.toDouble shouldBe 0.058
    curve.wibor1m.toDouble shouldBe (0.058 + 0.0015 +- 1e-10)
    curve.wibor3m.toDouble shouldBe (0.058 + 0.0040 +- 1e-10)
    curve.wibor6m.toDouble shouldBe (0.058 + 0.0080 +- 1e-10)
  }

  it should "preserve term structure ordering: O/N < 1M < 3M < 6M" in {
    val curve = YieldCurve.compute(0.05)
    curve.overnight.toDouble should be < curve.wibor1m.toDouble
    curve.wibor1m.toDouble should be < curve.wibor3m.toDouble
    curve.wibor3m.toDouble should be < curve.wibor6m.toDouble
  }

  it should "handle zero O/N rate" in {
    val curve = YieldCurve.compute(0.0)
    curve.overnight.toDouble shouldBe 0.0
    curve.wibor1m.toDouble shouldBe YieldCurve.TermPremium1M
    curve.wibor3m.toDouble shouldBe YieldCurve.TermPremium3M
    curve.wibor6m.toDouble shouldBe YieldCurve.TermPremium6M
  }

  it should "handle very low O/N rate" in {
    val curve = YieldCurve.compute(0.001)  // 0.1% floor rate
    curve.wibor3m.toDouble shouldBe (0.001 + 0.0040 +- 1e-10)
  }

  it should "handle high O/N rate" in {
    val curve = YieldCurve.compute(0.25)  // 25% ceiling
    curve.wibor6m.toDouble shouldBe (0.25 + 0.0080 +- 1e-10)
  }

  // =========================================================================
  // Term premiums
  // =========================================================================

  "Term premiums" should "be positive" in {
    YieldCurve.TermPremium1M should be > 0.0
    YieldCurve.TermPremium3M should be > 0.0
    YieldCurve.TermPremium6M should be > 0.0
  }

  it should "be monotonically increasing" in {
    YieldCurve.TermPremium1M should be < YieldCurve.TermPremium3M
    YieldCurve.TermPremium3M should be < YieldCurve.TermPremium6M
  }

  // =========================================================================
  // Banking.State integration
  // =========================================================================

  "Banking.State" should "default to None for interbankCurve" in {
    val bs = Banking.State(Vector.empty, Rate(0.05), Vector.empty)
    bs.interbankCurve shouldBe None
  }

  it should "store curve when provided" in {
    val curve = YieldCurve.compute(0.058)
    val bs = Banking.State(Vector.empty, Rate(0.058), Vector.empty,
      interbankCurve = Some(curve))
    bs.interbankCurve shouldBe defined
    bs.interbankCurve.get.wibor3m.toDouble shouldBe (0.058 + 0.0040 +- 1e-10)
  }
