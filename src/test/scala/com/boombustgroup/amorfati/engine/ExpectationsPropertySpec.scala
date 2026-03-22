package com.boombustgroup.amorfati.engine

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.mechanisms.Expectations
import com.boombustgroup.amorfati.types.*

class ExpectationsPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  private val inflationGen = Gen.choose(-0.10, 0.20)
  private val rateGen      = Gen.choose(0.001, 0.25)
  private val credGen      = Gen.choose(0.01, 1.0)
  private val unempGen     = Gen.choose(0.01, 0.50)

  "Expectations.step" should "always bound credibility in [0.01, 1.0]" in
    forAll(inflationGen, rateGen, credGen, unempGen) { (infl: Double, rate: Double, cred: Double, unemp: Double) =>
      val prev = Expectations.initial.copy(credibility = Share(cred))
      val r    = Expectations.step(prev, infl, rate, unemp)
      r.credibility.toDouble should be >= 0.01
      r.credibility.toDouble should be <= 1.0
    }

  it should "produce finite expected inflation" in
    forAll(inflationGen, rateGen, credGen, unempGen) { (infl: Double, rate: Double, cred: Double, unemp: Double) =>
      val prev = Expectations.initial.copy(credibility = Share(cred))
      val r    = Expectations.step(prev, infl, rate, unemp)
      r.expectedInflation.toDouble.isFinite shouldBe true
    }

  it should "produce finite expected rate" in
    forAll(inflationGen, rateGen, credGen, unempGen) { (infl: Double, rate: Double, cred: Double, unemp: Double) =>
      val prev = Expectations.initial.copy(credibility = Share(cred))
      val r    = Expectations.step(prev, infl, rate, unemp)
      r.expectedRate.toDouble.isFinite shouldBe true
    }

  it should "compute correct forecast error" in
    forAll(inflationGen, rateGen, unempGen) { (infl: Double, rate: Double, unemp: Double) =>
      val prev = Expectations.initial
      val r    = Expectations.step(prev, infl, rate, unemp)
      r.forecastError.toDouble shouldBe (infl - prev.expectedInflation.toDouble) +- 1e-3
    }

  it should "increase credibility monotonically as deviation decreases (at fixed credibility)" in {
    val prev   = Expectations.initial.copy(credibility = Share(0.5))
    val target = p.monetary.targetInfl.toDouble
    // Low deviation
    val r1     = Expectations.step(prev, target + 0.005, 0.0575, 0.05)
    // High deviation
    val r2     = Expectations.step(prev, target + 0.10, 0.0575, 0.05)
    r1.credibility.toDouble should be > r2.credibility.toDouble
  }

  it should "keep expected inflation between target and adaptive" in
    forAll(inflationGen, credGen) { (infl: Double, cred: Double) =>
      val prev     = Expectations.initial.copy(credibility = Share(cred))
      val r        = Expectations.step(prev, infl, 0.0575, 0.05)
      val target   = p.monetary.targetInfl.toDouble
      val adaptive =
        prev.expectedInflation.toDouble + p.labor.expLambda.toDouble * (infl - prev.expectedInflation.toDouble)
      val lo       = Math.min(target, adaptive)
      val hi       = Math.max(target, adaptive)
      r.expectedInflation.toDouble should be >= lo - 1e-10
      r.expectedInflation.toDouble should be <= hi + 1e-10
    }

  it should "produce finite forward guidance rate" in
    forAll(inflationGen, rateGen, credGen, unempGen) { (infl: Double, rate: Double, cred: Double, unemp: Double) =>
      val prev = Expectations.initial.copy(credibility = Share(cred))
      val r    = Expectations.step(prev, infl, rate, unemp)
      r.forwardGuidanceRate.toDouble.isFinite shouldBe true
    }

  "initial" should "have well-defined fields" in {
    val i = Expectations.initial
    i.expectedInflation.toDouble.isFinite shouldBe true
    i.expectedRate.toDouble.isFinite shouldBe true
    i.credibility.toDouble should be >= 0.01
    i.credibility.toDouble should be <= 1.0
  }

  "step with no change" should "preserve stability at target" in {
    val prev = Expectations.initial
    val r    = Expectations.step(prev, p.monetary.targetInfl.toDouble, p.monetary.initialRate.toDouble, 0.05)
    // Expectations should stay near initial values
    Math.abs(r.expectedInflation.toDouble - p.monetary.targetInfl.toDouble) should be < 0.01
    r.credibility.toDouble should be >= prev.credibility.toDouble
  }
