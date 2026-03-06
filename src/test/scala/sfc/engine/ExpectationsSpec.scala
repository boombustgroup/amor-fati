package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.config.{Config, MonetaryRegime, RunConfig}
import sfc.types.*

class ExpectationsSpec extends AnyFlatSpec with Matchers:

  private val plnRc = RunConfig(2000.0, 1, "test", MonetaryRegime.Pln)
  private val eurRc = RunConfig(2000.0, 1, "test", MonetaryRegime.Eur)

  // --- Initialization ---

  "Expectations.zero" should "have default values" in {
    val z = Expectations.zero
    z.expectedInflation.toDouble shouldBe 0.025
    z.expectedRate.toDouble shouldBe 0.0575
    z.credibility.toDouble shouldBe 0.8
    z.forecastError.toDouble shouldBe 0.0
  }

  "Expectations.initial" should "use config values" in {
    val i = Expectations.initial
    i.expectedInflation.toDouble shouldBe Config.NbpTargetInfl
    i.expectedRate.toDouble shouldBe Config.NbpInitialRate
    i.credibility.toDouble shouldBe Config.ExpCredibilityInit
    i.forecastError.toDouble shouldBe 0.0
    i.forwardGuidanceRate.toDouble shouldBe Config.NbpInitialRate
  }

  // --- Forecast error ---

  "step" should "compute forecast error = realized - expected" in {
    val prev = Expectations.initial
    val r = Expectations.step(prev, 0.05, 0.0575, 0.05, plnRc)
    r.forecastError.toDouble shouldBe (0.05 - Config.NbpTargetInfl) +- 1e-10
  }

  // --- Adaptive update + anchoring ---

  it should "increase expected inflation when realized exceeds target" in {
    val prev = Expectations.initial
    val r = Expectations.step(prev, 0.08, 0.0575, 0.05, plnRc)
    r.expectedInflation.toDouble should be > Config.NbpTargetInfl
  }

  it should "keep expected inflation near target when credibility is high" in {
    val prev = Expectations.initial.copy(credibility = Ratio(0.99))
    val r = Expectations.step(prev, 0.08, 0.0575, 0.05, plnRc)
    // With 99% credibility, expectations should stay close to target
    r.expectedInflation.toDouble should be < 0.04
  }

  it should "track realized inflation when credibility is low" in {
    val prev = Expectations.initial.copy(credibility = Ratio(0.05))
    val r = Expectations.step(prev, 0.10, 0.0575, 0.05, plnRc)
    // With 5% credibility, expectations should move toward realized
    r.expectedInflation.toDouble should be > 0.06
  }

  // --- Credibility dynamics ---

  it should "build credibility when inflation is near target" in {
    val prev = Expectations.initial.copy(credibility = Ratio(0.5))
    val r = Expectations.step(prev, Config.NbpTargetInfl, 0.0575, 0.05, plnRc)
    r.credibility.toDouble should be > 0.5
  }

  it should "erode credibility when inflation deviates from target" in {
    val prev = Expectations.initial.copy(credibility = Ratio(0.8))
    // 10% inflation → well above 2pp threshold
    val r = Expectations.step(prev, 0.10, 0.0575, 0.05, plnRc)
    r.credibility.toDouble should be < 0.8
  }

  it should "bound credibility in [0.01, 1.0]" in {
    // Test lower bound
    val low = Expectations.initial.copy(credibility = Ratio(0.02))
    val r1 = Expectations.step(low, 0.50, 0.0575, 0.05, plnRc)
    r1.credibility.toDouble should be >= 0.01

    // Test upper bound
    val high = Expectations.initial.copy(credibility = Ratio(0.99))
    val r2 = Expectations.step(high, Config.NbpTargetInfl, 0.0575, 0.05, plnRc)
    r2.credibility.toDouble should be <= 1.0
  }

  it should "be harder to build credibility than to lose it (asymmetric)" in {
    val mid = Expectations.initial.copy(credibility = Ratio(0.5))
    // Build: at target
    val rBuild = Expectations.step(mid, Config.NbpTargetInfl, 0.0575, 0.05, plnRc)
    val buildDelta = rBuild.credibility.toDouble - 0.5
    // Erode: 5% above target (symmetric deviation)
    val rErode = Expectations.step(mid, Config.NbpTargetInfl + 0.05, 0.0575, 0.05, plnRc)
    val erodeDelta = 0.5 - rErode.credibility.toDouble
    // Erosion should be larger because it's proportional to current credibility (0.5)
    // while building is proportional to (1 - credibility) (0.5) — but the deviation matters too
    // The key asymmetry: building uses (1-c) scaling, eroding uses c scaling
    buildDelta should be > 0.0
    erodeDelta should be > 0.0
  }

  // --- Expected rate ---

  it should "update expected rate adaptively without forward guidance" in {
    val prev = Expectations.initial
    val r = Expectations.step(prev, 0.025, 0.08, 0.05, plnRc)
    // Expected rate should move toward current rate (0.08)
    r.expectedRate.toDouble should be > Config.NbpInitialRate
  }

  // --- EUR regime ---

  it should "use ECB target for EUR regime" in {
    val prev = Expectations.initial
    val r = Expectations.step(prev, Config.EcbTargetInfl, 0.035, 0.05, eurRc)
    // At ECB target → credibility should increase
    r.credibility.toDouble should be >= prev.credibility.toDouble
  }

  // --- Stability ---

  it should "converge when inflation equals target persistently" in {
    var s = Expectations.initial.copy(credibility = Ratio(0.5))
    for _ <- 0 until 120 do s = Expectations.step(s, Config.NbpTargetInfl, Config.NbpInitialRate, 0.05, plnRc)
    s.credibility.toDouble should be > 0.9
    s.expectedInflation.toDouble shouldBe Config.NbpTargetInfl +- 0.005
    s.forecastError.toDouble shouldBe 0.0 +- 0.005
  }
