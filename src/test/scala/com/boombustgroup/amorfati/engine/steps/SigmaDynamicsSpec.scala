package com.boombustgroup.amorfati.engine.steps

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.types.*

class SigmaDynamicsSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams = SimParams.defaults

  "PriceEquityStep.evolveSigmas" should "return unchanged sigmas when lambda=0" in {
    val current  = Vector(50.0, 10.0, 5.0, 2.0, 1.0, 3.0).map(Sigma(_))
    val base     = Vector(50.0, 10.0, 5.0, 2.0, 1.0, 3.0)
    val adoption = Vector(0.5, 0.3, 0.2, 0.1, 0.0, 0.1)
    PriceEquityStep.evolveSigmas(current, base, adoption, 0.0, 3.0) shouldBe current
  }

  it should "increase sigma when lambda>0 and adoption>0" in {
    val current  = Vector(Sigma(5.0))
    val base     = Vector(5.0)
    val adoption = Vector(0.5)
    val result   = PriceEquityStep.evolveSigmas(current, base, adoption, 0.02, 3.0)
    result(0).toDouble should be > 5.0
  }

  it should "not change sigma when adoption=0" in {
    val current  = Vector(Sigma(5.0))
    val base     = Vector(5.0)
    val adoption = Vector(0.0)
    val result   = PriceEquityStep.evolveSigmas(current, base, adoption, 0.02, 3.0)
    result(0).toDouble shouldBe 5.0
  }

  it should "cap sigma at base * capMult" in {
    // current very close to cap (5.0 * 3.0 = 15.0)
    val current  = Vector(Sigma(14.99))
    val base     = Vector(5.0)
    val adoption = Vector(1.0)
    val result   = PriceEquityStep.evolveSigmas(current, base, adoption, 10.0, 3.0)
    result(0).toDouble should be <= 15.0
  }

  it should "never decrease sigma (ratchet)" in {
    val current  = Vector(Sigma(5.0))
    val base     = Vector(5.0)
    // Negative adoption is pathological but ratchet should still hold
    val adoption = Vector(-0.5)
    val result   = PriceEquityStep.evolveSigmas(current, base, adoption, 0.02, 3.0)
    result(0).toDouble should be >= 5.0
  }

  it should "evolve multiple sectors independently" in {
    val current  = Vector(50.0, 10.0, 5.0, 2.0, 1.0, 3.0).map(Sigma(_))
    val base     = Vector(50.0, 10.0, 5.0, 2.0, 1.0, 3.0)
    val adoption = Vector(0.5, 0.0, 0.3, 0.0, 0.0, 0.2)
    val result   = PriceEquityStep.evolveSigmas(current, base, adoption, 0.02, 3.0)
    // Sectors with adoption > 0 should increase
    result(0).toDouble should be > 50.0
    result(2).toDouble should be > 5.0
    result(5).toDouble should be > 3.0
    // Sectors with adoption = 0 should stay the same
    result(1).toDouble shouldBe 10.0
    result(3).toDouble shouldBe 2.0
    result(4).toDouble shouldBe 1.0
  }
