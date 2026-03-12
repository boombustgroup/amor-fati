package com.boombustgroup.amorfati.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ReducedVatSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  "VatRates" should "have 6 values" in {
    p.fiscal.vatRates.map(_.toDouble).length shouldBe 6
  }

  it should "have all rates in [0, 0.23]" in
    p.fiscal.vatRates.map(_.toDouble).foreach { r =>
      r should be >= 0.0
      r should be <= 0.23
    }

  "Weighted average" should "approximate real Polish effective VAT (~13%)" in {
    val weightedAvg =
      p.fiscal.fofConsWeights.map(_.toDouble).zip(p.fiscal.vatRates.map(_.toDouble)).map((w, r) => w * r).sum
    weightedAvg should be > 0.10
    weightedAvg should be < 0.18
  }

  it should "match manual computation" in {
    val weights = p.fiscal.fofConsWeights.map(_.toDouble)
    val rates   = p.fiscal.vatRates.map(_.toDouble)
    val manual  = weights(0) * rates(0) + weights(1) * rates(1) + weights(2) * rates(2) +
      weights(3) * rates(3) + weights(4) * rates(4) + weights(5) * rates(5)
    val formula = weights.zip(rates).map((w, r) => w * r).sum
    Math.abs(formula - manual) should be < 1e-10
  }

  "Sector ordering" should "have Healthcare and Agriculture rates below Retail" in {
    // Healthcare (idx 3), Agriculture (idx 5) < Retail (idx 2)
    p.fiscal.vatRates.map(_.toDouble)(3) should be < p.fiscal.vatRates.map(_.toDouble)(2)
    p.fiscal.vatRates.map(_.toDouble)(5) should be < p.fiscal.vatRates.map(_.toDouble)(2)
  }

  it should "have BPO rate at standard 23%" in {
    p.fiscal.vatRates.map(_.toDouble)(0) shouldBe 0.23
  }

  "Sector-weighted VAT" should "be less than flat 23% for same consumption" in {
    val consumption       = 1000000.0
    val sectorWeightedVat = consumption * p.fiscal.fofConsWeights
      .map(_.toDouble)
      .zip(p.fiscal.vatRates.map(_.toDouble))
      .map((w, r) => w * r)
      .sum
    val flatVat           = consumption * 0.23
    sectorWeightedVat should be < flatVat
  }

  "Flat rates" should "reproduce old behavior when all rates are 0.23" in {
    val flatRates   = Vector.fill(6)(0.23)
    val consumption = 1000000.0
    val flatVat     = consumption * p.fiscal.fofConsWeights.map(_.toDouble).zip(flatRates).map((w, r) => w * r).sum
    // Since FofConsWeights sums to ~1.0, flat rate × consumption ≈ 0.23 × consumption
    Math.abs(flatVat - consumption * 0.23) should be < 1.0
  }

  "Zero rates" should "produce zero VAT" in {
    val zeroRates   = Vector.fill(6)(0.0)
    val consumption = 1000000.0
    val vat         = consumption * p.fiscal.fofConsWeights.map(_.toDouble).zip(zeroRates).map((w, r) => w * r).sum
    vat shouldBe 0.0
  }

  "Default effective rate" should "be close to MF 2023 data (~13.9%)" in {
    val effectiveRate =
      p.fiscal.fofConsWeights.map(_.toDouble).zip(p.fiscal.vatRates.map(_.toDouble)).map((w, r) => w * r).sum
    // MF 2023: VAT revenue ~250 bln / consumption ~1.8 trln ≈ 13.9%
    // Model: ~13.1% (slightly lower due to rounding)
    effectiveRate should be > 0.12
    effectiveRate should be < 0.15
  }
