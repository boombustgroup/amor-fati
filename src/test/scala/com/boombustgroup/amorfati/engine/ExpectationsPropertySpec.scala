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
  private val td           = ComputationBoundary

  private val inflationGen = Gen.choose(-0.10, 0.20)
  private val rateGen      = Gen.choose(0.001, 0.25)
  private val credGen      = Gen.choose(0.01, 1.0)
  private val unempGen     = Gen.choose(0.01, 0.50)

  private def step(prev: Expectations.State, infl: Double, rate: Double, unemp: Double): Expectations.State =
    Expectations.step(prev, Rate(infl), Rate(rate), Share(unemp))

  "Expectations.step" should "always bound credibility in [0.01, 1.0]" in
    forAll(inflationGen, rateGen, credGen, unempGen) { (infl: Double, rate: Double, cred: Double, unemp: Double) =>
      val prev = Expectations.initial.copy(credibility = Share(cred))
      val r    = step(prev, infl, rate, unemp)
      td.toDouble(r.credibility).should(be >= 0.01)
      td.toDouble(r.credibility).should(be <= 1.0)
    }

  it should "produce finite expected inflation" in
    forAll(inflationGen, rateGen, credGen, unempGen) { (infl: Double, rate: Double, cred: Double, unemp: Double) =>
      val prev = Expectations.initial.copy(credibility = Share(cred))
      val r    = step(prev, infl, rate, unemp)
      td.toDouble(r.expectedInflation).isFinite.shouldBe(true)
    }

  it should "produce finite expected rate" in
    forAll(inflationGen, rateGen, credGen, unempGen) { (infl: Double, rate: Double, cred: Double, unemp: Double) =>
      val prev = Expectations.initial.copy(credibility = Share(cred))
      val r    = step(prev, infl, rate, unemp)
      td.toDouble(r.expectedRate).isFinite.shouldBe(true)
    }

  it should "compute correct forecast error" in
    forAll(inflationGen, rateGen, unempGen) { (infl: Double, rate: Double, unemp: Double) =>
      val prev = Expectations.initial
      val r    = step(prev, infl, rate, unemp)
      td.toDouble(r.forecastError).shouldBe((infl - td.toDouble(prev.expectedInflation)) +- 1e-3)
    }

  it should "increase credibility monotonically as deviation decreases (at fixed credibility)" in {
    val prev   = Expectations.initial.copy(credibility = Share(0.5))
    val target = td.toDouble(p.monetary.targetInfl)
    // Low deviation
    val r1     = step(prev, target + 0.005, 0.0575, 0.05)
    // High deviation
    val r2     = step(prev, target + 0.10, 0.0575, 0.05)
    td.toDouble(r1.credibility).should(be > td.toDouble(r2.credibility))
  }

  it should "keep expected inflation between target and adaptive" in
    forAll(inflationGen, credGen) { (infl: Double, cred: Double) =>
      val prev     = Expectations.initial.copy(credibility = Share(cred))
      val r        = step(prev, infl, 0.0575, 0.05)
      val target   = td.toDouble(p.monetary.targetInfl)
      val adaptive =
        td.toDouble(prev.expectedInflation) + td.toDouble(p.labor.expLambda) * (infl - td.toDouble(prev.expectedInflation))
      val lo       = Math.min(target, adaptive)
      val hi       = Math.max(target, adaptive)
      td.toDouble(r.expectedInflation).should(be >= lo - 1e-10)
      td.toDouble(r.expectedInflation).should(be <= hi + 1e-10)
    }

  it should "produce finite forward guidance rate" in
    forAll(inflationGen, rateGen, credGen, unempGen) { (infl: Double, rate: Double, cred: Double, unemp: Double) =>
      val prev = Expectations.initial.copy(credibility = Share(cred))
      val r    = step(prev, infl, rate, unemp)
      td.toDouble(r.forwardGuidanceRate).isFinite.shouldBe(true)
    }

  "initial" should "have well-defined fields" in {
    val i = Expectations.initial
    td.toDouble(i.expectedInflation).isFinite.shouldBe(true)
    td.toDouble(i.expectedRate).isFinite.shouldBe(true)
    td.toDouble(i.credibility).should(be >= 0.01)
    td.toDouble(i.credibility).should(be <= 1.0)
  }

  "step with no change" should "preserve stability at target" in {
    val prev = Expectations.initial
    val r    = Expectations.step(prev, p.monetary.targetInfl, p.monetary.initialRate, Share(0.05))
    // Expectations should stay near initial values
    Math.abs(td.toDouble(r.expectedInflation) - td.toDouble(p.monetary.targetInfl)).should(be < 0.01)
    td.toDouble(r.credibility).should(be >= td.toDouble(prev.credibility))
  }
