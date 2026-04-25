package com.boombustgroup.amorfati

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.random.RandomStream
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OpaqueTypesSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  private def rateValue(r: Rate): BigDecimal                               = decimal(r)
  private def shareValue(s: Share): BigDecimal                             = decimal(s)
  private def scalarValue(s: Scalar): BigDecimal                           = decimal(s)
  private def multiplierValue(m: Multiplier): BigDecimal                   = decimal(m)
  private def coefficientValue(c: Coefficient): BigDecimal                 = decimal(c)
  private def priceIndexValue(pi: PriceIndex): BigDecimal                  = decimal(pi)
  private def exchangeRateValue(er: ExchangeRate): BigDecimal              = decimal(er)
  private def exchangeRateShockValue(shock: ExchangeRateShock): BigDecimal = decimal(shock)

  "Entity ids" should "wrap and unwrap domain identifiers" in {
    BankId(5).toInt shouldBe 5
    BankId.NoBank.toInt shouldBe -1
    FirmId(42).toInt shouldBe 42
    HhId(7).toInt shouldBe 7
    SectorIdx(3).toInt shouldBe 3
  }

  "PLN" should "support exact same-type arithmetic" in {
    val a = PLN(100)
    val b = PLN(40)

    (a + b) shouldBe PLN(140)
    (a - b) shouldBe PLN(60)
    (-b) shouldBe PLN(-40)
    PLN.decimal(-125, 1).abs shouldBe PLN.decimal(125, 1)
    a.max(b) shouldBe a
    a.min(b) shouldBe b
    PLN(250).clamp(PLN(0), PLN(200)) shouldBe PLN(200)
    PLN.Zero shouldBe PLN(0)
  }

  it should "support typed multiplications and integer scaling" in {
    val amount = PLN(1200)

    (amount * 3) shouldBe PLN(3600)
    (amount * 3L) shouldBe PLN(3600)
    (3 * amount) shouldBe PLN(3600)
    (amount * Share.decimal(25, 2)) shouldBe PLN(300)
    (amount * Rate.decimal(6, 2)) shouldBe PLN(72)
    (amount * Multiplier.decimal(15, 1)) shouldBe PLN(1800)
    (amount * Coefficient.decimal(-2, 1)) shouldBe PLN(-240)
  }

  it should "support division by typed scalars and money ratios" in {
    (PLN(120) / 12L) shouldBe PLN(10)
    (PLN(120) / Share.decimal(25, 2)) shouldBe PLN(480)
    (PLN(120) / Multiplier.decimal(15, 1)) shouldBe PLN(80)
    scalarValue(PLN(75).ratioTo(PLN(100))) shouldBe BigDecimal("0.75") +- BigDecimal("1e-9")
    scalarValue(PLN(0).ratioTo(PLN.Zero)) shouldBe BigDecimal("0.0") +- BigDecimal("1e-9")
  }

  "Rate" should "support same-type arithmetic and normalization" in {
    val rate = Rate.decimal(6, 2)

    rateValue(rate + Rate.decimal(1, 2)) shouldBe BigDecimal("0.07") +- BigDecimal("1e-9")
    rateValue(rate - Rate.decimal(2, 2)) shouldBe BigDecimal("0.04") +- BigDecimal("1e-9")
    rateValue(-rate) shouldBe BigDecimal("-0.06") +- BigDecimal("1e-9")
    rateValue(Rate.decimal(-3, 2).abs) shouldBe BigDecimal("0.03") +- BigDecimal("1e-9")
    rate.max(Rate.decimal(4, 2)) shouldBe rate
    rate.min(Rate.decimal(4, 2)) shouldBe Rate.decimal(4, 2)
    rateValue(rate.monthly) shouldBe BigDecimal("0.005") +- BigDecimal("1e-9")
    rateValue(rate.annualize.monthly) shouldBe BigDecimal("0.06") +- BigDecimal("1e-4")
    multiplierValue(rate.toMultiplier) shouldBe BigDecimal("0.06") +- BigDecimal("1e-9")
  }

  it should "support cross-type multiplication and ratios" in {
    rateValue(Rate.decimal(6, 2) * Multiplier.decimal(5, 1)) shouldBe BigDecimal("0.03") +- BigDecimal("1e-9")
    rateValue(Rate.decimal(6, 2) * Share.decimal(5, 1)) shouldBe BigDecimal("0.03") +- BigDecimal("1e-9")
    rateValue(Rate.decimal(6, 2) * Coefficient(-2)) shouldBe BigDecimal("-0.12") +- BigDecimal("1e-9")
    scalarValue(Rate.decimal(6, 2).ratioTo(Rate.decimal(8, 2))) shouldBe BigDecimal("0.75") +- BigDecimal("1e-9")
  }

  "Share" should "support bounded arithmetic helpers" in {
    shareValue(Share.decimal(3, 1) + Share.decimal(2, 1)) shouldBe BigDecimal("0.5") +- BigDecimal("1e-9")
    shareValue(Share.decimal(8, 1) - Share.decimal(3, 1)) shouldBe BigDecimal("0.5") +- BigDecimal("1e-9")
    shareValue(Share.decimal(5, 1) * Share.decimal(4, 1)) shouldBe BigDecimal("0.2") +- BigDecimal("1e-9")
    shareValue(Share.decimal(81, 2).sqrt) shouldBe BigDecimal("0.9") +- BigDecimal("1e-4")
    shareValue(Share.decimal(24, 2).monthly) shouldBe BigDecimal("0.02") +- BigDecimal("1e-9")
    Share.decimal(12, 1).clamp(Share.Zero, Share.One) shouldBe Share.One
    Share.Zero shouldBe Share(0)
    shareValue(Share.One) shouldBe BigDecimal("1.0") +- BigDecimal("1e-9")
    shareValue(Share.random(RandomStream.seeded(0L))) should (be >= BigDecimal("0.0") and be < BigDecimal("1.0"))
  }

  it should "bridge to other semantic types and counts" in {
    (Share.decimal(25, 2) * PLN(200)) shouldBe PLN(50)
    multiplierValue(Share.decimal(25, 2) * Multiplier(2)) shouldBe BigDecimal("0.5") +- BigDecimal("1e-9")
    coefficientValue(Share.decimal(25, 2) * Coefficient(2)) shouldBe BigDecimal("0.5") +- BigDecimal("1e-9")
    rateValue(Share.decimal(25, 2).toRate) shouldBe BigDecimal("0.25") +- BigDecimal("1e-9")
    multiplierValue(Share.decimal(25, 2).toMultiplier) shouldBe BigDecimal("0.25") +- BigDecimal("1e-9")
    scalarValue(Share.decimal(25, 2).ratioTo(Share.decimal(5, 1))) shouldBe BigDecimal("0.5") +- BigDecimal("1e-9")
    Share.decimal(25, 2).applyTo(200) shouldBe 50
    Share.fraction(1, 4) shouldBe Share.decimal(25, 2)
  }

  "Scalar" should "encode dimensionless ratios and compose with typed values" in {
    val s = PLN(150).ratioTo(PLN(100))

    scalarValue(s) shouldBe BigDecimal("1.5") +- BigDecimal("1e-9")
    scalarValue(s + Scalar.decimal(5, 1)) shouldBe BigDecimal("2.0") +- BigDecimal("1e-9")
    scalarValue(s - Scalar.decimal(5, 1)) shouldBe BigDecimal("1.0") +- BigDecimal("1e-9")
    scalarValue(s * Scalar.decimal(5, 1)) shouldBe BigDecimal("0.75") +- BigDecimal("1e-9")
    scalarValue((-s).abs) shouldBe BigDecimal("1.5") +- BigDecimal("1e-9")
    s.max(Scalar.One) shouldBe s
    Scalar.decimal(25, 1).clamp(Scalar.Zero, Scalar.One) shouldBe Scalar.One
    scalarValue(s / 3) shouldBe BigDecimal("0.5") +- BigDecimal("1e-9")
    scalarValue(Scalar.fraction(3, 4)) shouldBe BigDecimal("0.75") +- BigDecimal("1e-9")
    scalarValue(Scalar(4).reciprocal) shouldBe BigDecimal("0.25") +- BigDecimal("1e-4")
    scalarValue(Scalar(2).ratioTo(Scalar(8))) shouldBe BigDecimal("0.25") +- BigDecimal("1e-9")
    scalarValue(Scalar(100).log10) shouldBe BigDecimal("2.0") +- BigDecimal("1e-4")
    scalarValue(Scalar(9).pow(Scalar.decimal(5, 1))) shouldBe BigDecimal("3.0") +- BigDecimal("1e-4")

    (s * PLN(200)) shouldBe PLN(300)
    rateValue(s * Rate.decimal(1, 1)) shouldBe BigDecimal("0.15") +- BigDecimal("1e-9")
    shareValue(s * Share.decimal(2, 1)) shouldBe BigDecimal("0.3") +- BigDecimal("1e-9")
    multiplierValue(s * Multiplier(2)) shouldBe BigDecimal("3.0") +- BigDecimal("1e-9")
    coefficientValue(s * Coefficient(-2)) shouldBe BigDecimal("-3.0") +- BigDecimal("1e-9")
    shareValue(s.toShare) shouldBe BigDecimal("1.5") +- BigDecimal("1e-9")
    multiplierValue(s.toMultiplier) shouldBe BigDecimal("1.5") +- BigDecimal("1e-9")
    coefficientValue(s.toCoefficient) shouldBe BigDecimal("1.5") +- BigDecimal("1e-9")
  }

  "Multiplier" should "support semantic multiplier operations" in {
    val m = Multiplier.decimal(15, 1)

    multiplierValue(m + Multiplier.decimal(5, 1)) shouldBe BigDecimal("2.0") +- BigDecimal("1e-9")
    multiplierValue(m - Multiplier.decimal(5, 1)) shouldBe BigDecimal("1.0") +- BigDecimal("1e-9")
    multiplierValue(m * Multiplier(2)) shouldBe BigDecimal("3.0") +- BigDecimal("1e-9")
    multiplierValue(Multiplier(9).pow(Scalar.decimal(5, 1))) shouldBe BigDecimal("3.0") +- BigDecimal("1e-4")
    multiplierValue(m * Share.decimal(2, 1)) shouldBe BigDecimal("0.3") +- BigDecimal("1e-9")
    (m * PLN(200)) shouldBe PLN(300)
    rateValue(m.toRate) shouldBe BigDecimal("1.5") +- BigDecimal("1e-9")
    shareValue(m.toShare) shouldBe BigDecimal("1.5") +- BigDecimal("1e-9")
    scalarValue(m.toScalar) shouldBe BigDecimal("1.5") +- BigDecimal("1e-9")
  }

  "Coefficient" should "support signed behavioral arithmetic" in {
    val c = Coefficient(-2)

    coefficientValue(c.abs) shouldBe BigDecimal("2.0") +- BigDecimal("1e-9")
    coefficientValue(c + Coefficient.decimal(5, 1)) shouldBe BigDecimal("-1.5") +- BigDecimal("1e-9")
    coefficientValue(c - Coefficient.decimal(5, 1)) shouldBe BigDecimal("-2.5") +- BigDecimal("1e-9")
    coefficientValue(c * Coefficient.decimal(-5, 1)) shouldBe BigDecimal("1.0") +- BigDecimal("1e-9")
    coefficientValue(c * Share.decimal(5, 1)) shouldBe BigDecimal("-1.0") +- BigDecimal("1e-9")
    shareValue(Coefficient.decimal(5, 1) * Multiplier(2)) shouldBe BigDecimal("1.0") +- BigDecimal("1e-9")
    (c * PLN(100)) shouldBe PLN(-200)
    multiplierValue(c.toMultiplier) shouldBe BigDecimal("-2.0") +- BigDecimal("1e-9")
  }

  "PriceIndex" should "support indexed arithmetic" in {
    priceIndexValue(PriceIndex.Base * Rate.decimal(5, 2)) shouldBe BigDecimal("0.05") +- BigDecimal("1e-9")
    priceIndexValue(PriceIndex.Base * Multiplier.decimal(12, 1)) shouldBe BigDecimal("1.2") +- BigDecimal("1e-9")
    (PriceIndex.Base * PLN(200)) shouldBe PLN(200)
    priceIndexValue(PriceIndex.decimal(11, 1) * PriceIndex.decimal(12, 1)) shouldBe BigDecimal("1.32") +- BigDecimal("1e-4")
  }

  "Sigma" should "remain a typed elasticity token" in {
    val sigma = Sigma(5)

    sigma + Sigma(1) shouldBe Sigma(6)
    sigma > Sigma(4) shouldBe true
    sigma >= Sigma(5) shouldBe true
    sigma < Sigma(6) shouldBe true
    scalarValue(sigma.toScalar) shouldBe BigDecimal("5.0") +- BigDecimal("1e-9")
  }

  "ExchangeRate" should "support relative FX semantics" in {
    val base  = ExchangeRate(4)
    val rate  = ExchangeRate.decimal(44, 1)
    val shock = rate.deviationFrom(base)

    exchangeRateShockValue(shock) shouldBe BigDecimal("0.1") +- BigDecimal("1e-9")
    exchangeRateValue(base.applyShock(shock)) shouldBe BigDecimal("4.4") +- BigDecimal("1e-4")
  }
