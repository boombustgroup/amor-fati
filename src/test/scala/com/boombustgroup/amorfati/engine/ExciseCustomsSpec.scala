package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ExciseCustomsSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  "ExciseRates" should "have 6 values" in {
    p.fiscal.exciseRates.map(r => decimal(r)).length shouldBe BigDecimal("6")
  }

  it should "have all rates in [0, 0.10]" in
    p.fiscal.exciseRates.map(r => decimal(r)).foreach { r =>
      r should be >= BigDecimal("0.0")
      r should be <= BigDecimal("0.10")
    }

  "Weighted excise" should "be between 2% and 4% effective rate" in {
    val weightedAvg =
      p.fiscal.fofConsWeights.map(w => decimal(w)).zip(p.fiscal.exciseRates.map(r => decimal(r))).map((w, r) => w * r).sum
    weightedAvg should be > BigDecimal("0.02")
    weightedAvg should be < BigDecimal("0.04")
  }

  "CustomsDutyRate" should "be in [0, 0.15]" in {
    decimal(p.fiscal.customsDutyRate) should be >= BigDecimal("0.0")
    decimal(p.fiscal.customsDutyRate) should be <= BigDecimal("0.15")
  }

  "CustomsNonEuShare" should "be in [0, 1]" in {
    decimal(p.fiscal.customsNonEuShare) should be >= BigDecimal("0.0")
    decimal(p.fiscal.customsNonEuShare) should be <= BigDecimal("1.0")
  }

  "Excise" should "always be positive for positive consumption" in {
    val consumption = BigDecimal("1000000.0")
    val excise      = consumption * p.fiscal.fofConsWeights
      .map(w => decimal(w))
      .zip(p.fiscal.exciseRates.map(r => decimal(r)))
      .map((w, r) => w * r)
      .sum
    excise should be > BigDecimal("0.0")
  }

  it should "be less than VAT for same consumption" in {
    val consumption = BigDecimal("1000000.0")
    val excise      = consumption * p.fiscal.fofConsWeights
      .map(w => decimal(w))
      .zip(p.fiscal.exciseRates.map(r => decimal(r)))
      .map((w, r) => w * r)
      .sum
    val vat         = consumption * p.fiscal.fofConsWeights
      .map(w => decimal(w))
      .zip(p.fiscal.vatRates.map(r => decimal(r)))
      .map((w, r) => w * r)
      .sum
    excise should be < vat
  }

  "Zero excise rates" should "produce zero excise" in {
    val zeroRates   = Vector.fill(6)(BigDecimal("0.0"))
    val consumption = BigDecimal("1000000.0")
    val excise      = consumption * p.fiscal.fofConsWeights.map(w => decimal(w)).zip(zeroRates).map((w, r) => w * r).sum
    excise shouldBe BigDecimal("0.0")
  }

  "Manual computation" should "match formula" in {
    val weights = p.fiscal.fofConsWeights.map(w => decimal(w))
    val rates   = p.fiscal.exciseRates.map(r => decimal(r))
    val manual  = weights(0) * rates(0) + weights(1) * rates(1) + weights(2) * rates(2) +
      weights(3) * rates(3) + weights(4) * rates(4) + weights(5) * rates(5)
    val formula = weights.zip(rates).map((w, r) => w * r).sum
    DecimalMath.abs(formula - manual) should be < BigDecimal("1e-10")
  }

  "Combined fiscal plausibility" should "produce ~30 bln PLN/year at baseline" in {
    // Approximate: consumption ~1.8T PLN/year, imports ~600B PLN/year
    val monthlyConsumption = BigDecimal("1.8e12") / BigDecimal("12.0")
    val monthlyImports     = BigDecimal("600e9") / BigDecimal("12.0")
    val monthlyExcise      = monthlyConsumption * p.fiscal.fofConsWeights
      .map(w => decimal(w))
      .zip(p.fiscal.exciseRates.map(r => decimal(r)))
      .map((w, r) => w * r)
      .sum
    val monthlyCustoms     = monthlyImports * decimal(p.fiscal.customsNonEuShare) * decimal(p.fiscal.customsDutyRate)
    val annualCombined     = (monthlyExcise + monthlyCustoms) * BigDecimal("12.0")
    // Should be in ~30-90 bln PLN range
    annualCombined should be > BigDecimal("20e9")
    annualCombined should be < BigDecimal("120e9")
  }

  "Customs" should "apply only to non-EU share" in {
    val imports      = BigDecimal("1000000.0")
    val fullCustoms  = imports * decimal(p.fiscal.customsDutyRate)
    val nonEuCustoms = imports * decimal(p.fiscal.customsNonEuShare) * decimal(p.fiscal.customsDutyRate)
    nonEuCustoms should be < fullCustoms
    nonEuCustoms shouldBe (fullCustoms * decimal(p.fiscal.customsNonEuShare) +- BigDecimal("0.01"))
  }

  "Manufacturing excise" should "be highest sector rate" in {
    // Mfg (idx 1) has highest excise due to fuel content
    p.fiscal.exciseRates.map(r => decimal(r))(1) shouldBe p.fiscal.exciseRates.map(r => decimal(r)).max
  }
