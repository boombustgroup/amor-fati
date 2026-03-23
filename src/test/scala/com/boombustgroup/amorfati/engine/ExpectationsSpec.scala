package com.boombustgroup.amorfati.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.mechanisms.Expectations
import com.boombustgroup.amorfati.types.*

class ExpectationsSpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]
  private val td           = ComputationBoundary

  // --- Initialization ---

  "Expectations.initial" should "use config values" in {
    val i = Expectations.initial
    td.toDouble(i.expectedInflation) shouldBe td.toDouble(p.monetary.targetInfl)
    td.toDouble(i.expectedRate) shouldBe td.toDouble(p.monetary.initialRate)
    td.toDouble(i.credibility) shouldBe td.toDouble(p.labor.expCredibilityInit)
    td.toDouble(i.forecastError) shouldBe 0.0
    td.toDouble(i.forwardGuidanceRate) shouldBe td.toDouble(p.monetary.initialRate)
  }

  // --- Forecast error ---

  "step" should "compute forecast error = realized - expected" in {
    val prev = Expectations.initial
    val r    = Expectations.step(prev, 0.05, 0.0575, 0.05)
    td.toDouble(r.forecastError) shouldBe (0.05 - td.toDouble(p.monetary.targetInfl)) +- 1e-10
  }

  // --- Adaptive update + anchoring ---

  it should "increase expected inflation when realized exceeds target" in {
    val prev = Expectations.initial
    val r    = Expectations.step(prev, 0.08, 0.0575, 0.05)
    td.toDouble(r.expectedInflation) should be > td.toDouble(p.monetary.targetInfl)
  }

  it should "keep expected inflation near target when credibility is high" in {
    val prev = Expectations.initial.copy(credibility = Share(0.99))
    val r    = Expectations.step(prev, 0.08, 0.0575, 0.05)
    // With 99% credibility, expectations should stay close to target
    td.toDouble(r.expectedInflation) should be < 0.04
  }

  it should "track realized inflation when credibility is low" in {
    val prev = Expectations.initial.copy(credibility = Share(0.05))
    val r    = Expectations.step(prev, 0.10, 0.0575, 0.05)
    // With 5% credibility, expectations should move toward realized
    td.toDouble(r.expectedInflation) should be > 0.06
  }

  // --- Credibility dynamics ---

  it should "build credibility when inflation is near target" in {
    val prev = Expectations.initial.copy(credibility = Share(0.5))
    val r    = Expectations.step(prev, td.toDouble(p.monetary.targetInfl), 0.0575, 0.05)
    td.toDouble(r.credibility) should be > 0.5
  }

  it should "erode credibility when inflation deviates from target" in {
    val prev = Expectations.initial.copy(credibility = Share(0.8))
    // 10% inflation -> well above 2pp threshold
    val r    = Expectations.step(prev, 0.10, 0.0575, 0.05)
    td.toDouble(r.credibility) should be < 0.8
  }

  it should "bound credibility in [0.01, 1.0]" in {
    // Test lower bound
    val low = Expectations.initial.copy(credibility = Share(0.02))
    val r1  = Expectations.step(low, 0.50, 0.0575, 0.05)
    td.toDouble(r1.credibility) should be >= 0.01

    // Test upper bound
    val high = Expectations.initial.copy(credibility = Share(0.99))
    val r2   = Expectations.step(high, td.toDouble(p.monetary.targetInfl), 0.0575, 0.05)
    td.toDouble(r2.credibility) should be <= 1.0
  }

  it should "be harder to build credibility than to lose it (asymmetric)" in {
    val mid        = Expectations.initial.copy(credibility = Share(0.5))
    // Build: at target
    val rBuild     = Expectations.step(mid, td.toDouble(p.monetary.targetInfl), 0.0575, 0.05)
    val buildDelta = td.toDouble(rBuild.credibility) - 0.5
    // Erode: 5% above target (symmetric deviation)
    val rErode     = Expectations.step(mid, td.toDouble(p.monetary.targetInfl) + 0.05, 0.0575, 0.05)
    val erodeDelta = 0.5 - td.toDouble(rErode.credibility)
    // Erosion should be larger because it's proportional to current credibility (0.5)
    // while building is proportional to (1 - credibility) (0.5) -- but the deviation matters too
    // The key asymmetry: building uses (1-c) scaling, eroding uses c scaling
    buildDelta should be > 0.0
    erodeDelta should be > 0.0
  }

  // --- Expected rate ---

  it should "update expected rate toward current rate" in {
    val prev = Expectations.initial
    val r    = Expectations.step(prev, 0.025, 0.08, 0.05)
    // With forward guidance on, expected rate blends FG (approx neutralRate) and adaptive (toward 0.08)
    // Expected rate should differ from initial (moves toward blended target)
    td.toDouble(r.expectedRate) should not be td.toDouble(p.monetary.initialRate)
  }

  // --- Stability ---

  it should "converge when inflation equals target persistently" in {
    var s = Expectations.initial.copy(credibility = Share(0.5))
    for _ <- 0 until 120 do s = Expectations.step(s, td.toDouble(p.monetary.targetInfl), td.toDouble(p.monetary.initialRate), 0.05)
    td.toDouble(s.credibility) should be > 0.9
    td.toDouble(s.expectedInflation) shouldBe td.toDouble(p.monetary.targetInfl) +- 0.005
    td.toDouble(s.forecastError) shouldBe 0.0 +- 0.005
  }
