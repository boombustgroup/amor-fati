package com.boombustgroup.amorfati.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.agents.Banking
import com.boombustgroup.amorfati.engine.mechanisms.YieldCurve
import com.boombustgroup.amorfati.types.*

/** Interbank term structure tests. */
class YieldCurveSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams = SimParams.defaults

  // =========================================================================
  // Compute (base premiums, no stress)
  // =========================================================================

  "YieldCurve.compute" should "produce base premiums from O/N rate" in {
    val curve = YieldCurve.compute(Rate(0.058))
    curve.overnight should be(Rate(0.058))
    curve.wibor1m.toDouble should be(0.058 + YieldCurve.BasePremium1M.toDouble +- 1e-10)
    curve.wibor3m.toDouble should be(0.058 + YieldCurve.BasePremium3M.toDouble +- 1e-10)
    curve.wibor6m.toDouble should be(0.058 + YieldCurve.BasePremium6M.toDouble +- 1e-10)
  }

  it should "preserve term structure ordering: O/N < 1M < 3M < 6M" in {
    val curve = YieldCurve.compute(Rate(0.05))
    curve.overnight should be < curve.wibor1m
    curve.wibor1m should be < curve.wibor3m
    curve.wibor3m should be < curve.wibor6m
  }

  it should "handle zero O/N rate" in {
    val curve = YieldCurve.compute(Rate.Zero)
    curve.overnight should be(Rate.Zero)
    curve.wibor1m should be(YieldCurve.BasePremium1M)
    curve.wibor3m should be(YieldCurve.BasePremium3M)
    curve.wibor6m should be(YieldCurve.BasePremium6M)
  }

  it should "handle very low O/N rate" in {
    val curve = YieldCurve.compute(Rate(0.001))
    curve.wibor3m.toDouble should be(0.001 + YieldCurve.BasePremium3M.toDouble +- 1e-10)
  }

  it should "handle high O/N rate" in {
    val curve = YieldCurve.compute(Rate(0.25))
    curve.wibor6m.toDouble should be(0.25 + YieldCurve.BasePremium6M.toDouble +- 1e-10)
  }

  // =========================================================================
  // Stress components
  // =========================================================================

  it should "widen premium under credit stress (NPL > 0)" in {
    val calm     = YieldCurve.compute(Rate(0.05))
    val stressed = YieldCurve.compute(Rate(0.05), nplRatio = Share(0.10))
    stressed.wibor3m should be > calm.wibor3m
  }

  it should "widen premium when expectations de-anchor (credibility < 1)" in {
    val anchored   = YieldCurve.compute(Rate(0.05))
    val deAnchored = YieldCurve.compute(Rate(0.05), credibility = Share(0.5), expectedInflation = Rate(0.08))
    deAnchored.wibor3m should be > anchored.wibor3m
  }

  it should "have no extra premium when credibility = 1 regardless of expected inflation" in {
    val base = YieldCurve.compute(Rate(0.05))
    val high = YieldCurve.compute(Rate(0.05), credibility = Share.One, expectedInflation = Rate(0.10))
    high.wibor3m should be(base.wibor3m)
  }

  // =========================================================================
  // Base premiums
  // =========================================================================

  "Base premiums" should "be positive" in {
    YieldCurve.BasePremium1M should be > Rate.Zero
    YieldCurve.BasePremium3M should be > Rate.Zero
    YieldCurve.BasePremium6M should be > Rate.Zero
  }

  it should "be monotonically increasing" in {
    YieldCurve.BasePremium1M should be < YieldCurve.BasePremium3M
    YieldCurve.BasePremium3M should be < YieldCurve.BasePremium6M
  }

  // =========================================================================
  // Banking.State integration
  // =========================================================================

  "Banking.State" should "default to None for interbankCurve" in {
    val bs = Banking.State(Vector.empty, Rate(0.05), Vector.empty, None)
    bs.interbankCurve should be(None)
  }

  it should "store curve when provided" in {
    val curve = YieldCurve.compute(Rate(0.058))
    val bs    = Banking.State(Vector.empty, Rate(0.058), Vector.empty, interbankCurve = Some(curve))
    bs.interbankCurve should be(defined)
    bs.interbankCurve.get.wibor3m.toDouble should be(0.058 + YieldCurve.BasePremium3M.toDouble +- 1e-10)
  }
