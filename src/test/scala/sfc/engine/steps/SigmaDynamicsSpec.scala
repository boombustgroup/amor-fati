package sfc.engine.steps

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SigmaDynamicsSpec extends AnyFlatSpec with Matchers:

  import sfc.config.SimParams
  given SimParams = SimParams.defaults

  "PriceEquityStep.evolveSigmas" should "return unchanged sigmas when lambda=0" in {
    val current  = Vector(50.0, 10.0, 5.0, 2.0, 1.0, 3.0)
    val adoption = Vector(0.5, 0.3, 0.2, 0.1, 0.0, 0.1)
    PriceEquityStep.evolveSigmas(current, current, adoption, 0.0, 3.0) shouldBe current
  }

  it should "increase sigma when lambda>0 and adoption>0" in {
    val current  = Vector(5.0)
    val base     = Vector(5.0)
    val adoption = Vector(0.5)
    val result   = PriceEquityStep.evolveSigmas(current, base, adoption, 0.02, 3.0)
    result(0) should be > 5.0
  }

  it should "not change sigma when adoption=0" in {
    val current  = Vector(5.0)
    val base     = Vector(5.0)
    val adoption = Vector(0.0)
    val result   = PriceEquityStep.evolveSigmas(current, base, adoption, 0.02, 3.0)
    result(0) shouldBe 5.0
  }

  it should "cap sigma at base * capMult" in {
    // current very close to cap (5.0 * 3.0 = 15.0)
    val current  = Vector(14.99)
    val base     = Vector(5.0)
    val adoption = Vector(1.0)
    val result   = PriceEquityStep.evolveSigmas(current, base, adoption, 10.0, 3.0)
    result(0) should be <= 15.0
  }

  it should "never decrease sigma (ratchet)" in {
    val current  = Vector(5.0)
    val base     = Vector(5.0)
    // Negative adoption is pathological but ratchet should still hold
    val adoption = Vector(-0.5)
    val result   = PriceEquityStep.evolveSigmas(current, base, adoption, 0.02, 3.0)
    result(0) should be >= 5.0
  }

  it should "evolve multiple sectors independently" in {
    val current  = Vector(50.0, 10.0, 5.0, 2.0, 1.0, 3.0)
    val base     = current
    val adoption = Vector(0.5, 0.0, 0.3, 0.0, 0.0, 0.2)
    val result   = PriceEquityStep.evolveSigmas(current, base, adoption, 0.02, 3.0)
    // Sectors with adoption > 0 should increase
    result(0) should be > 50.0
    result(2) should be > 5.0
    result(5) should be > 3.0
    // Sectors with adoption = 0 should stay the same
    result(1) shouldBe 10.0
    result(3) shouldBe 2.0
    result(4) shouldBe 1.0
  }
