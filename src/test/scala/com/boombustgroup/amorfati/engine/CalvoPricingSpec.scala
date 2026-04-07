package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.markets.CalvoPricing
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class CalvoPricingSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  "optimalMarkup" should "increase with demand pressure" in {
    val low  = CalvoPricing.optimalMarkup(Multiplier(0.9), Coefficient.Zero)
    val high = CalvoPricing.optimalMarkup(Multiplier(1.2), Coefficient.Zero)
    high should be > low
  }

  it should "increase with wage growth (cost passthrough)" in {
    val noGrowth = CalvoPricing.optimalMarkup(Multiplier.One, Coefficient.Zero)
    val growth   = CalvoPricing.optimalMarkup(Multiplier.One, Coefficient(0.05))
    growth should be > noGrowth
  }

  it should "be clamped between min and max" in {
    val extreme = CalvoPricing.optimalMarkup(Multiplier(5.0), Coefficient(1.0))
    extreme should be <= summon[SimParams].pricing.maxMarkup
    val low     = CalvoPricing.optimalMarkup(Multiplier(-5.0), Coefficient(-1.0))
    low should be >= summon[SimParams].pricing.minMarkup
  }

  "updateFirmMarkup" should "change markup with high probability (theta)" in {
    // Run 100 times, expect ~15 changes (theta=0.15)
    val rng     = new Random(42)
    val results = (0 until 100).map(_ => CalvoPricing.updateFirmMarkup(Multiplier.One, Multiplier(1.1), Coefficient(0.01), rng))
    val changed = results.count(_.priceChanged)
    changed should be > 5
    changed should be < 30
  }

  it should "keep markup unchanged when not selected" in {
    val rng    = new Random(42)
    val result = CalvoPricing.updateFirmMarkup(Multiplier(1.2), Multiplier.One, Coefficient.Zero, rng)
    if !result.priceChanged then result.newMarkup.bd shouldBe BigDecimal("1.2") +- BigDecimal("0.001")
  }

  it should "be deterministic with same seed" in {
    val r1 = CalvoPricing.updateFirmMarkup(Multiplier.One, Multiplier(1.1), Coefficient(0.01), new Random(42))
    val r2 = CalvoPricing.updateFirmMarkup(Multiplier.One, Multiplier(1.1), Coefficient(0.01), new Random(42))
    r1.newMarkup.bd shouldBe r2.newMarkup.bd +- BigDecimal("0.0000000001")
    r1.priceChanged shouldBe r2.priceChanged
  }

  "aggregateMarkupInflation" should "be zero when no firms present" in {
    CalvoPricing.aggregateMarkupInflation(Vector.empty, Vector.empty) shouldBe Rate.Zero
  }
