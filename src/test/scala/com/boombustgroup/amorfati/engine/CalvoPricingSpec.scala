package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.markets.CalvoPricing
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class CalvoPricingSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  "optimalMarkup" should "increase with demand pressure" in {
    val low  = CalvoPricing.optimalMarkup(0.9, 0.0)
    val high = CalvoPricing.optimalMarkup(1.2, 0.0)
    high.toDouble should be > low.toDouble
  }

  it should "increase with wage growth (cost passthrough)" in {
    val noGrowth = CalvoPricing.optimalMarkup(1.0, 0.0)
    val growth   = CalvoPricing.optimalMarkup(1.0, 0.05)
    growth.toDouble should be > noGrowth.toDouble
  }

  it should "be clamped between min and max" in {
    val extreme = CalvoPricing.optimalMarkup(5.0, 1.0)
    extreme.toDouble should be <= summon[SimParams].pricing.maxMarkup.toDouble
    val low     = CalvoPricing.optimalMarkup(-5.0, -1.0)
    low.toDouble should be >= summon[SimParams].pricing.minMarkup.toDouble
  }

  "updateFirmMarkup" should "change markup with high probability (theta)" in {
    // Run 100 times, expect ~15 changes (theta=0.15)
    val rng     = new Random(42)
    val results = (0 until 100).map(_ => CalvoPricing.updateFirmMarkup(Ratio.One, 1.1, 0.01, rng))
    val changed = results.count(_.priceChanged)
    changed should be > 5
    changed should be < 30
  }

  it should "keep markup unchanged when not selected" in {
    val rng    = new Random(42)
    val result = CalvoPricing.updateFirmMarkup(Ratio(1.2), 1.0, 0.0, rng)
    if !result.priceChanged then result.newMarkup.toDouble shouldBe 1.2 +- 0.001
  }

  it should "be deterministic with same seed" in {
    val r1 = CalvoPricing.updateFirmMarkup(Ratio.One, 1.1, 0.01, new Random(42))
    val r2 = CalvoPricing.updateFirmMarkup(Ratio.One, 1.1, 0.01, new Random(42))
    r1.newMarkup.toDouble shouldBe r2.newMarkup.toDouble +- 1e-10
    r1.priceChanged shouldBe r2.priceChanged
  }

  "aggregateMarkupInflation" should "be zero when no firms present" in {
    CalvoPricing.aggregateMarkupInflation(Vector.empty, Vector.empty) shouldBe 0.0
  }
