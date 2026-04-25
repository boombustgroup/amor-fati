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
    val a = PLN("100.0")
    val b = PLN("40.0")

    (a + b) shouldBe PLN("140.0")
    (a - b) shouldBe PLN("60.0")
    (-b) shouldBe PLN("-40.0")
    PLN("-12.5").abs shouldBe PLN("12.5")
    a.max(b) shouldBe a
    a.min(b) shouldBe b
    PLN("250.0").clamp(PLN("0.0"), PLN("200.0")) shouldBe PLN("200.0")
    PLN.Zero shouldBe PLN("0.0")
  }

  it should "support typed multiplications and integer scaling" in {
    val amount = PLN("1200.0")

    (amount * 3) shouldBe PLN("3600.0")
    (amount * 3L) shouldBe PLN("3600.0")
    (3 * amount) shouldBe PLN("3600.0")
    (amount * Share("0.25")) shouldBe PLN("300.0")
    (amount * Rate("0.06")) shouldBe PLN("72.0")
    (amount * Multiplier("1.5")) shouldBe PLN("1800.0")
    (amount * Coefficient("-0.2")) shouldBe PLN("-240.0")
  }

  it should "support division by typed scalars and money ratios" in {
    (PLN("120.0") / 12L) shouldBe PLN("10.0")
    (PLN("120.0") / Share("0.25")) shouldBe PLN("480.0")
    (PLN("120.0") / Multiplier("1.5")) shouldBe PLN("80.0")
    scalarValue(PLN("75.0").ratioTo(PLN("100.0"))) shouldBe BigDecimal("0.75") +- BigDecimal("1e-9")
    scalarValue(PLN("0.0").ratioTo(PLN.Zero)) shouldBe BigDecimal("0.0") +- BigDecimal("1e-9")
  }

  "Rate" should "support same-type arithmetic and normalization" in {
    val rate = Rate("0.06")

    rateValue(rate + Rate("0.01")) shouldBe BigDecimal("0.07") +- BigDecimal("1e-9")
    rateValue(rate - Rate("0.02")) shouldBe BigDecimal("0.04") +- BigDecimal("1e-9")
    rateValue(-rate) shouldBe BigDecimal("-0.06") +- BigDecimal("1e-9")
    rateValue(Rate("-0.03").abs) shouldBe BigDecimal("0.03") +- BigDecimal("1e-9")
    rate.max(Rate("0.04")) shouldBe rate
    rate.min(Rate("0.04")) shouldBe Rate("0.04")
    rateValue(rate.monthly) shouldBe BigDecimal("0.005") +- BigDecimal("1e-9")
    rateValue(rate.annualize.monthly) shouldBe BigDecimal("0.06") +- BigDecimal("1e-4")
    multiplierValue(rate.toMultiplier) shouldBe BigDecimal("0.06") +- BigDecimal("1e-9")
  }

  it should "support cross-type multiplication and ratios" in {
    rateValue(Rate("0.06") * Multiplier("0.5")) shouldBe BigDecimal("0.03") +- BigDecimal("1e-9")
    rateValue(Rate("0.06") * Share("0.5")) shouldBe BigDecimal("0.03") +- BigDecimal("1e-9")
    rateValue(Rate("0.06") * Coefficient("-2.0")) shouldBe BigDecimal("-0.12") +- BigDecimal("1e-9")
    scalarValue(Rate("0.06").ratioTo(Rate("0.08"))) shouldBe BigDecimal("0.75") +- BigDecimal("1e-9")
  }

  "Share" should "support bounded arithmetic helpers" in {
    shareValue(Share("0.3") + Share("0.2")) shouldBe BigDecimal("0.5") +- BigDecimal("1e-9")
    shareValue(Share("0.8") - Share("0.3")) shouldBe BigDecimal("0.5") +- BigDecimal("1e-9")
    shareValue(Share("0.5") * Share("0.4")) shouldBe BigDecimal("0.2") +- BigDecimal("1e-9")
    shareValue(Share("0.81").sqrt) shouldBe BigDecimal("0.9") +- BigDecimal("1e-4")
    shareValue(Share("0.24").monthly) shouldBe BigDecimal("0.02") +- BigDecimal("1e-9")
    Share("1.2").clamp(Share.Zero, Share.One) shouldBe Share.One
    Share.Zero shouldBe Share("0.0")
    shareValue(Share.One) shouldBe BigDecimal("1.0") +- BigDecimal("1e-9")
    shareValue(Share.random(RandomStream.seeded(0L))) should (be >= BigDecimal("0.0") and be < BigDecimal("1.0"))
  }

  it should "bridge to other semantic types and counts" in {
    (Share("0.25") * PLN("200.0")) shouldBe PLN("50.0")
    multiplierValue(Share("0.25") * Multiplier("2.0")) shouldBe BigDecimal("0.5") +- BigDecimal("1e-9")
    coefficientValue(Share("0.25") * Coefficient("2.0")) shouldBe BigDecimal("0.5") +- BigDecimal("1e-9")
    rateValue(Share("0.25").toRate) shouldBe BigDecimal("0.25") +- BigDecimal("1e-9")
    multiplierValue(Share("0.25").toMultiplier) shouldBe BigDecimal("0.25") +- BigDecimal("1e-9")
    scalarValue(Share("0.25").ratioTo(Share("0.5"))) shouldBe BigDecimal("0.5") +- BigDecimal("1e-9")
    Share("0.25").applyTo(200) shouldBe 50
    Share.fraction(1, 4) shouldBe Share("0.25")
  }

  "Scalar" should "encode dimensionless ratios and compose with typed values" in {
    val s = PLN("150.0").ratioTo(PLN("100.0"))

    scalarValue(s) shouldBe BigDecimal("1.5") +- BigDecimal("1e-9")
    scalarValue(s + Scalar("0.5")) shouldBe BigDecimal("2.0") +- BigDecimal("1e-9")
    scalarValue(s - Scalar("0.5")) shouldBe BigDecimal("1.0") +- BigDecimal("1e-9")
    scalarValue(s * Scalar("0.5")) shouldBe BigDecimal("0.75") +- BigDecimal("1e-9")
    scalarValue((-s).abs) shouldBe BigDecimal("1.5") +- BigDecimal("1e-9")
    s.max(Scalar.One) shouldBe s
    Scalar("2.5").clamp(Scalar.Zero, Scalar.One) shouldBe Scalar.One
    scalarValue(s / 3) shouldBe BigDecimal("0.5") +- BigDecimal("1e-9")
    scalarValue(Scalar.fraction(3, 4)) shouldBe BigDecimal("0.75") +- BigDecimal("1e-9")
    scalarValue(Scalar("4.0").reciprocal) shouldBe BigDecimal("0.25") +- BigDecimal("1e-4")
    scalarValue(Scalar("2.0").ratioTo(Scalar("8.0"))) shouldBe BigDecimal("0.25") +- BigDecimal("1e-9")
    scalarValue(Scalar("100.0").log10) shouldBe BigDecimal("2.0") +- BigDecimal("1e-4")
    scalarValue(Scalar("9.0").pow(Scalar("0.5"))) shouldBe BigDecimal("3.0") +- BigDecimal("1e-4")

    (s * PLN("200.0")) shouldBe PLN("300.0")
    rateValue(s * Rate("0.1")) shouldBe BigDecimal("0.15") +- BigDecimal("1e-9")
    shareValue(s * Share("0.2")) shouldBe BigDecimal("0.3") +- BigDecimal("1e-9")
    multiplierValue(s * Multiplier("2.0")) shouldBe BigDecimal("3.0") +- BigDecimal("1e-9")
    coefficientValue(s * Coefficient("-2.0")) shouldBe BigDecimal("-3.0") +- BigDecimal("1e-9")
    shareValue(s.toShare) shouldBe BigDecimal("1.5") +- BigDecimal("1e-9")
    multiplierValue(s.toMultiplier) shouldBe BigDecimal("1.5") +- BigDecimal("1e-9")
    coefficientValue(s.toCoefficient) shouldBe BigDecimal("1.5") +- BigDecimal("1e-9")
  }

  "Multiplier" should "support semantic multiplier operations" in {
    val m = Multiplier("1.5")

    multiplierValue(m + Multiplier("0.5")) shouldBe BigDecimal("2.0") +- BigDecimal("1e-9")
    multiplierValue(m - Multiplier("0.5")) shouldBe BigDecimal("1.0") +- BigDecimal("1e-9")
    multiplierValue(m * Multiplier("2.0")) shouldBe BigDecimal("3.0") +- BigDecimal("1e-9")
    multiplierValue(Multiplier("9.0").pow(Scalar("0.5"))) shouldBe BigDecimal("3.0") +- BigDecimal("1e-4")
    multiplierValue(m * Share("0.2")) shouldBe BigDecimal("0.3") +- BigDecimal("1e-9")
    (m * PLN("200.0")) shouldBe PLN("300.0")
    rateValue(m.toRate) shouldBe BigDecimal("1.5") +- BigDecimal("1e-9")
    shareValue(m.toShare) shouldBe BigDecimal("1.5") +- BigDecimal("1e-9")
    scalarValue(m.toScalar) shouldBe BigDecimal("1.5") +- BigDecimal("1e-9")
  }

  "Coefficient" should "support signed behavioral arithmetic" in {
    val c = Coefficient("-2.0")

    coefficientValue(c.abs) shouldBe BigDecimal("2.0") +- BigDecimal("1e-9")
    coefficientValue(c + Coefficient("0.5")) shouldBe BigDecimal("-1.5") +- BigDecimal("1e-9")
    coefficientValue(c - Coefficient("0.5")) shouldBe BigDecimal("-2.5") +- BigDecimal("1e-9")
    coefficientValue(c * Coefficient("-0.5")) shouldBe BigDecimal("1.0") +- BigDecimal("1e-9")
    coefficientValue(c * Share("0.5")) shouldBe BigDecimal("-1.0") +- BigDecimal("1e-9")
    shareValue(Coefficient("0.5") * Multiplier("2.0")) shouldBe BigDecimal("1.0") +- BigDecimal("1e-9")
    (c * PLN("100.0")) shouldBe PLN("-200.0")
    multiplierValue(c.toMultiplier) shouldBe BigDecimal("-2.0") +- BigDecimal("1e-9")
  }

  "PriceIndex" should "support indexed arithmetic" in {
    priceIndexValue(PriceIndex.Base * Rate("0.05")) shouldBe BigDecimal("0.05") +- BigDecimal("1e-9")
    priceIndexValue(PriceIndex.Base * Multiplier("1.2")) shouldBe BigDecimal("1.2") +- BigDecimal("1e-9")
    (PriceIndex.Base * PLN("200.0")) shouldBe PLN("200.0")
    priceIndexValue(PriceIndex("1.1") * PriceIndex("1.2")) shouldBe BigDecimal("1.32") +- BigDecimal("1e-4")
  }

  "Sigma" should "remain a typed elasticity token" in {
    val sigma = Sigma("5.0")

    sigma + Sigma("1.0") shouldBe Sigma("6.0")
    sigma > Sigma("4.0") shouldBe true
    sigma >= Sigma("5.0") shouldBe true
    sigma < Sigma("6.0") shouldBe true
    scalarValue(sigma.toScalar) shouldBe BigDecimal("5.0") +- BigDecimal("1e-9")
  }

  "ExchangeRate" should "support relative FX semantics" in {
    val base  = ExchangeRate("4.0")
    val rate  = ExchangeRate("4.4")
    val shock = rate.deviationFrom(base)

    exchangeRateShockValue(shock) shouldBe BigDecimal("0.1") +- BigDecimal("1e-9")
    exchangeRateValue(base.applyShock(shock)) shouldBe BigDecimal("4.4") +- BigDecimal("1e-4")
  }
