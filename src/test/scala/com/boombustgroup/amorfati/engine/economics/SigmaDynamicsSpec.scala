package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.types.*

class SigmaDynamicsSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams = SimParams.defaults

  private def evolveSigmas(
      current: Vector[Sigma],
      base: Vector[BigDecimal],
      adoption: Vector[BigDecimal],
      lambda: BigDecimal,
      capMult: BigDecimal,
  ): Vector[Sigma] =
    PriceEquityEconomics.evolveSigmas(current, base.map(sigmaBD(_)), adoption.map(shareBD(_)), coefficientBD(lambda), multiplierBD(capMult))

  "PriceEquityEconomics.evolveSigmas" should "return unchanged sigmas when lambda=0" in {
    val current  = Vector(BigDecimal("50.0"), BigDecimal("10.0"), BigDecimal("5.0"), BigDecimal("2.0"), BigDecimal("1.0"), BigDecimal("3.0")).map(sigmaBD(_))
    val base     = Vector(BigDecimal("50.0"), BigDecimal("10.0"), BigDecimal("5.0"), BigDecimal("2.0"), BigDecimal("1.0"), BigDecimal("3.0"))
    val adoption = Vector(BigDecimal("0.5"), BigDecimal("0.3"), BigDecimal("0.2"), BigDecimal("0.1"), BigDecimal("0.0"), BigDecimal("0.1"))
    evolveSigmas(current, base, adoption, BigDecimal("0.0"), BigDecimal("3.0")) shouldBe current
  }

  it should "increase sigma when lambda>0 and adoption>0" in {
    val current  = Vector(Sigma(5))
    val base     = Vector(BigDecimal("5.0"))
    val adoption = Vector(BigDecimal("0.5"))
    val result   = evolveSigmas(current, base, adoption, BigDecimal("0.02"), BigDecimal("3.0"))
    decimal(result(0)) should be > BigDecimal("5.0")
  }

  it should "not change sigma when adoption=0" in {
    val current  = Vector(Sigma(5))
    val base     = Vector(BigDecimal("5.0"))
    val adoption = Vector(BigDecimal("0.0"))
    val result   = evolveSigmas(current, base, adoption, BigDecimal("0.02"), BigDecimal("3.0"))
    decimal(result(0)) shouldBe BigDecimal("5.0")
  }

  it should "cap sigma at base * capMult" in {
    // current very close to cap (5.0 * 3.0 = 15.0)
    val current  = Vector(Sigma.decimal(1499, 2))
    val base     = Vector(BigDecimal("5.0"))
    val adoption = Vector(BigDecimal("1.0"))
    val result   = evolveSigmas(current, base, adoption, BigDecimal("10.0"), BigDecimal("3.0"))
    decimal(result(0)) should be <= BigDecimal("15.0")
  }

  it should "never decrease sigma (ratchet)" in {
    val current  = Vector(Sigma(5))
    val base     = Vector(BigDecimal("5.0"))
    // Negative adoption is pathological but ratchet should still hold
    val adoption = Vector(-BigDecimal("0.5"))
    val result   = evolveSigmas(current, base, adoption, BigDecimal("0.02"), BigDecimal("3.0"))
    decimal(result(0)) should be >= BigDecimal("5.0")
  }

  it should "evolve multiple sectors independently" in {
    val current  = Vector(BigDecimal("50.0"), BigDecimal("10.0"), BigDecimal("5.0"), BigDecimal("2.0"), BigDecimal("1.0"), BigDecimal("3.0")).map(sigmaBD(_))
    val base     = Vector(BigDecimal("50.0"), BigDecimal("10.0"), BigDecimal("5.0"), BigDecimal("2.0"), BigDecimal("1.0"), BigDecimal("3.0"))
    val adoption = Vector(BigDecimal("0.5"), BigDecimal("0.0"), BigDecimal("0.3"), BigDecimal("0.0"), BigDecimal("0.0"), BigDecimal("0.2"))
    val result   = evolveSigmas(current, base, adoption, BigDecimal("0.02"), BigDecimal("3.0"))
    // Sectors with adoption > 0 should increase
    decimal(result(0)) should be > BigDecimal("50.0")
    decimal(result(2)) should be > BigDecimal("5.0")
    decimal(result(5)) should be > BigDecimal("3.0")
    // Sectors with adoption = 0 should stay the same
    decimal(result(1)) shouldBe BigDecimal("10.0")
    decimal(result(3)) shouldBe BigDecimal("2.0")
    decimal(result(4)) shouldBe BigDecimal("1.0")
  }
