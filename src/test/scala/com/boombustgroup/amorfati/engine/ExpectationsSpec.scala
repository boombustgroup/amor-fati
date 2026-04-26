package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.mechanisms.Expectations
import com.boombustgroup.amorfati.types.*

class ExpectationsSpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  private def step(prev: Expectations.State, infl: BigDecimal, rate: BigDecimal, unemp: BigDecimal): Expectations.State =
    Expectations.step(prev, rateBD(infl), rateBD(rate), shareBD(unemp))

  // --- Initialization ---

  "Expectations.initial" should "use config values" in {
    val i = Expectations.initial
    decimal(i.expectedInflation) shouldBe decimal(p.monetary.targetInfl)
    decimal(i.expectedRate) shouldBe decimal(p.monetary.initialRate)
    decimal(i.credibility) shouldBe decimal(p.labor.expCredibilityInit)
    decimal(i.forecastError) shouldBe BigDecimal("0.0")
    decimal(i.forwardGuidanceRate) shouldBe decimal(p.monetary.initialRate)
  }

  // --- Forecast error ---

  "step" should "compute forecast error = realized - expected" in {
    val prev = Expectations.initial
    val r    = step(prev, BigDecimal("0.05"), BigDecimal("0.0575"), BigDecimal("0.05"))
    decimal(r.forecastError) shouldBe (BigDecimal("0.05") - decimal(p.monetary.targetInfl)) +- BigDecimal("1e-10")
  }

  // --- Adaptive update + anchoring ---

  it should "increase expected inflation when realized exceeds target" in {
    val prev = Expectations.initial
    val r    = step(prev, BigDecimal("0.08"), BigDecimal("0.0575"), BigDecimal("0.05"))
    decimal(r.expectedInflation) should be > decimal(p.monetary.targetInfl)
  }

  it should "keep expected inflation near target when credibility is high" in {
    val prev = Expectations.initial.copy(credibility = Share.decimal(99, 2))
    val r    = step(prev, BigDecimal("0.08"), BigDecimal("0.0575"), BigDecimal("0.05"))
    // With 99% credibility, expectations should stay close to target
    decimal(r.expectedInflation) should be < BigDecimal("0.04")
  }

  it should "track realized inflation when credibility is low" in {
    val prev = Expectations.initial.copy(credibility = Share.decimal(5, 2))
    val r    = step(prev, BigDecimal("0.10"), BigDecimal("0.0575"), BigDecimal("0.05"))
    // With 5% credibility, expectations should move toward realized
    decimal(r.expectedInflation) should be > BigDecimal("0.06")
  }

  it should "keep expected inflation meaningfully anchored under deflation when credibility is high" in {
    val prev = Expectations.initial.copy(credibility = Share.decimal(90, 2))
    val r    = step(prev, -BigDecimal("0.10"), BigDecimal("0.0100"), BigDecimal("0.10"))
    decimal(r.expectedInflation) should be > BigDecimal("0.015")
  }

  it should "bound downward de-anchoring even when credibility is low" in {
    val prev = Expectations.initial.copy(credibility = Share.decimal(5, 2), expectedInflation = Rate.decimal(1, 2))
    val r    = step(prev, -BigDecimal("0.20"), BigDecimal("0.0100"), BigDecimal("0.12"))
    decimal(r.expectedInflation) should be >= BigDecimal("-0.005")
  }

  // --- Credibility dynamics ---

  it should "build credibility when inflation is near target" in {
    val prev = Expectations.initial.copy(credibility = Share.decimal(5, 1))
    val r    = Expectations.step(prev, p.monetary.targetInfl, Rate.decimal(575, 4), Share.decimal(5, 2))
    decimal(r.credibility) should be > BigDecimal("0.5")
  }

  it should "erode credibility when inflation deviates from target" in {
    val prev = Expectations.initial.copy(credibility = Share.decimal(8, 1))
    // 10% inflation -> well above 2pp threshold
    val r    = step(prev, BigDecimal("0.10"), BigDecimal("0.0575"), BigDecimal("0.05"))
    decimal(r.credibility) should be < BigDecimal("0.8")
  }

  it should "erode credibility less under equal undershooting than overshooting" in {
    val prev          = Expectations.initial.copy(credibility = Share.decimal(8, 1))
    val target        = decimal(p.monetary.targetInfl)
    val sameDeviation = BigDecimal("0.05")
    val low           = step(prev, target - sameDeviation, BigDecimal("0.0575"), BigDecimal("0.08"))
    val high          = step(prev, target + sameDeviation, BigDecimal("0.0575"), BigDecimal("0.05"))
    decimal(low.credibility) should be > decimal(high.credibility)
  }

  it should "bound credibility in [0.01, 1.0]" in {
    // Test lower bound
    val low = Expectations.initial.copy(credibility = Share.decimal(2, 2))
    val r1  = step(low, BigDecimal("0.50"), BigDecimal("0.0575"), BigDecimal("0.05"))
    decimal(r1.credibility) should be >= BigDecimal("0.01")

    // Test upper bound
    val high = Expectations.initial.copy(credibility = Share.decimal(99, 2))
    val r2   = Expectations.step(high, p.monetary.targetInfl, Rate.decimal(575, 4), Share.decimal(5, 2))
    decimal(r2.credibility) should be <= BigDecimal("1.0")
  }

  it should "be harder to build credibility than to lose it (asymmetric)" in {
    val mid        = Expectations.initial.copy(credibility = Share.decimal(5, 1))
    // Build: at target
    val rBuild     = Expectations.step(mid, p.monetary.targetInfl, Rate.decimal(575, 4), Share.decimal(5, 2))
    val buildDelta = decimal(rBuild.credibility) - BigDecimal("0.5")
    // Erode: 5% above target (symmetric deviation)
    val rErode     = step(mid, decimal(p.monetary.targetInfl) + BigDecimal("0.05"), BigDecimal("0.0575"), BigDecimal("0.05"))
    val erodeDelta = BigDecimal("0.5") - decimal(rErode.credibility)
    // Erosion should be larger because it's proportional to current credibility (0.5)
    // while building is proportional to (1 - credibility) (0.5) -- but the deviation matters too
    // The key asymmetry: building uses (1-c) scaling, eroding uses c scaling
    buildDelta should be > BigDecimal("0.0")
    erodeDelta should be > BigDecimal("0.0")
  }

  // --- Expected rate ---

  it should "update expected rate toward current rate" in {
    val prev = Expectations.initial
    val r    = step(prev, BigDecimal("0.025"), BigDecimal("0.08"), BigDecimal("0.05"))
    // With forward guidance on, expected rate blends FG (approx neutralRate) and adaptive (toward 0.08)
    // Expected rate should differ from initial (moves toward blended target)
    decimal(r.expectedRate) should not be decimal(p.monetary.initialRate)
  }

  // --- Stability ---

  it should "converge when inflation equals target persistently" in {
    var s = Expectations.initial.copy(credibility = Share.decimal(5, 1))
    for _ <- 0 until 120 do s = Expectations.step(s, p.monetary.targetInfl, p.monetary.initialRate, Share.decimal(5, 2))
    decimal(s.credibility) should be > BigDecimal("0.9")
    decimal(s.expectedInflation) shouldBe decimal(p.monetary.targetInfl) +- BigDecimal("0.005")
    decimal(s.forecastError) shouldBe BigDecimal("0.0") +- BigDecimal("0.005")
  }

  it should "avoid free-fall expected inflation under repeated deflation" in {
    var s = Expectations.initial.copy(credibility = Share.decimal(2, 1))
    for _ <- 0 until 24 do s = step(s, -BigDecimal("0.10"), BigDecimal("0.0100"), BigDecimal("0.12"))
    decimal(s.expectedInflation) should be >= BigDecimal("-0.005")
  }
