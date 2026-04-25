package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.mechanisms.Expectations
import com.boombustgroup.amorfati.types.*

class ExpectationsPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  private val inflationGen = genDecimal("-0.10", "0.20")
  private val rateGen      = genDecimal("0.001", "0.25")
  private val credGen      = genDecimal("0.01", "1.0")
  private val unempGen     = genDecimal("0.01", "0.50")

  private def step(prev: Expectations.State, infl: BigDecimal, rate: BigDecimal, unemp: BigDecimal): Expectations.State =
    Expectations.step(prev, Rate(infl), Rate(rate), Share(unemp))

  private def step(prev: Expectations.State, infl: Rate, rate: Rate, unemp: Share): Expectations.State =
    Expectations.step(prev, infl, rate, unemp)

  "Expectations.step" should "always bound credibility in [0.01, 1.0]" in
    forAll(inflationGen, rateGen, credGen, unempGen) { (infl: BigDecimal, rate: BigDecimal, cred: BigDecimal, unemp: BigDecimal) =>
      val prev = Expectations.initial.copy(credibility = Share(cred))
      val r    = step(prev, infl, rate, unemp)
      decimal(r.credibility).should(be >= BigDecimal("0.01"))
      decimal(r.credibility).should(be <= BigDecimal("1.0"))
    }

  it should "produce finite expected inflation" in
    forAll(inflationGen, rateGen, credGen, unempGen) { (infl: BigDecimal, rate: BigDecimal, cred: BigDecimal, unemp: BigDecimal) =>
      val prev = Expectations.initial.copy(credibility = Share(cred))
      val r    = step(prev, infl, rate, unemp)
      decimal(r.expectedInflation).isFinite.shouldBe(true)
    }

  it should "produce finite expected rate" in
    forAll(inflationGen, rateGen, credGen, unempGen) { (infl: BigDecimal, rate: BigDecimal, cred: BigDecimal, unemp: BigDecimal) =>
      val prev = Expectations.initial.copy(credibility = Share(cred))
      val r    = step(prev, infl, rate, unemp)
      decimal(r.expectedRate).isFinite.shouldBe(true)
    }

  it should "compute correct forecast error" in
    forAll(inflationGen, rateGen, unempGen) { (infl: BigDecimal, rate: BigDecimal, unemp: BigDecimal) =>
      val prev = Expectations.initial
      val r    = step(prev, infl, rate, unemp)
      decimal(r.forecastError).shouldBe((infl - decimal(prev.expectedInflation)) +- BigDecimal("1e-3"))
    }

  it should "increase credibility monotonically as deviation decreases (at fixed credibility)" in {
    val prev   = Expectations.initial.copy(credibility = Share("0.5"))
    val target = decimal(p.monetary.targetInfl)
    // Low deviation
    val r1     = step(prev, target + BigDecimal("0.005"), BigDecimal("0.0575"), BigDecimal("0.05"))
    // High deviation
    val r2     = step(prev, target + BigDecimal("0.10"), BigDecimal("0.0575"), BigDecimal("0.05"))
    decimal(r1.credibility).should(be > decimal(r2.credibility))
  }

  it should "keep expected inflation between target and adaptive" in
    forAll(inflationGen, credGen) { (infl: BigDecimal, cred: BigDecimal) =>
      val prev     = Expectations.initial.copy(credibility = Share(cred))
      val r        = step(prev, infl, BigDecimal("0.0575"), BigDecimal("0.05"))
      val target   = decimal(p.monetary.targetInfl)
      val adaptive =
        decimal(prev.expectedInflation) + decimal(p.labor.expLambda) * (infl - decimal(prev.expectedInflation))
      val lo       = DecimalMath.min(target, adaptive)
      val hi       = DecimalMath.max(target, adaptive)
      decimal(r.expectedInflation).should(be >= lo - BigDecimal("1e-10"))
      decimal(r.expectedInflation).should(be <= hi + BigDecimal("1e-10"))
    }

  it should "produce finite forward guidance rate" in
    forAll(inflationGen, rateGen, credGen, unempGen) { (infl: BigDecimal, rate: BigDecimal, cred: BigDecimal, unemp: BigDecimal) =>
      val prev = Expectations.initial.copy(credibility = Share(cred))
      val r    = step(prev, infl, rate, unemp)
      decimal(r.forwardGuidanceRate).isFinite.shouldBe(true)
    }

  "initial" should "have well-defined fields" in {
    val i = Expectations.initial
    decimal(i.expectedInflation).isFinite.shouldBe(true)
    decimal(i.expectedRate).isFinite.shouldBe(true)
    decimal(i.credibility).should(be >= BigDecimal("0.01"))
    decimal(i.credibility).should(be <= BigDecimal("1.0"))
  }

  "step with no change" should "preserve stability at target" in {
    val prev = Expectations.initial
    val r    = step(prev, p.monetary.targetInfl, p.monetary.initialRate, Share("0.05"))
    // Expectations should stay near initial values
    DecimalMath.abs(decimal(r.expectedInflation) - decimal(p.monetary.targetInfl)).should(be < BigDecimal("0.01"))
    decimal(r.credibility).should(be >= decimal(prev.credibility))
  }
