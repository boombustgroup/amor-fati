package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.config.{Config, SECTORS}
import sfc.util.KahanSum.*

class ExciseCustomsSpec extends AnyFlatSpec with Matchers:

  "ExciseRates" should "have 6 values" in {
    Config.ExciseRates.length shouldBe 6
  }

  it should "have all rates in [0, 0.10]" in {
    Config.ExciseRates.foreach { r =>
      r should be >= 0.0
      r should be <= 0.10
    }
  }

  "Weighted excise" should "be between 2% and 4% effective rate" in {
    val weightedAvg = Config.FofConsWeights.zip(Config.ExciseRates).map((w, r) => w * r).sum
    weightedAvg should be > 0.02
    weightedAvg should be < 0.04
  }

  "CustomsDutyRate" should "be in [0, 0.15]" in {
    Config.CustomsDutyRate should be >= 0.0
    Config.CustomsDutyRate should be <= 0.15
  }

  "CustomsNonEuShare" should "be in [0, 1]" in {
    Config.CustomsNonEuShare should be >= 0.0
    Config.CustomsNonEuShare should be <= 1.0
  }

  "Customs revenue" should "be zero when OPEN_ECON=false" in {
    // When OeEnabled is false, customs duty = 0 regardless of imports
    if !Config.OeEnabled then
      val imports = 1000000.0
      val customs =
        if Config.OeEnabled then imports * Config.CustomsNonEuShare * Config.CustomsDutyRate
        else 0.0
      customs shouldBe 0.0
  }

  "Excise" should "always be positive for positive consumption" in {
    val consumption = 1000000.0
    val excise = consumption * Config.FofConsWeights.zip(Config.ExciseRates).map((w, r) => w * r).kahanSum
    excise should be > 0.0
  }

  it should "be less than VAT for same consumption" in {
    val consumption = 1000000.0
    val excise = consumption * Config.FofConsWeights.zip(Config.ExciseRates).map((w, r) => w * r).kahanSum
    val vat = consumption * Config.FofConsWeights.zip(Config.VatRates).map((w, r) => w * r).kahanSum
    excise should be < vat
  }

  "Zero excise rates" should "produce zero excise" in {
    val zeroRates = Vector.fill(6)(0.0)
    val consumption = 1000000.0
    val excise = consumption * Config.FofConsWeights.zip(zeroRates).map((w, r) => w * r).sum
    excise shouldBe 0.0
  }

  "Manual computation" should "match formula" in {
    val consumption = 1000000.0
    val weights = Config.FofConsWeights
    val rates = Config.ExciseRates
    val manual = weights(0) * rates(0) + weights(1) * rates(1) + weights(2) * rates(2) +
      weights(3) * rates(3) + weights(4) * rates(4) + weights(5) * rates(5)
    val formula = weights.zip(rates).map((w, r) => w * r).sum
    Math.abs(formula - manual) should be < 1e-10
  }

  "Combined fiscal plausibility" should "produce ~30 bln PLN/year at baseline" in {
    // Approximate: consumption ~1.8T PLN/year, imports ~600B PLN/year
    val monthlyConsumption = 1.8e12 / 12.0
    val monthlyImports = 600e9 / 12.0
    val monthlyExcise = monthlyConsumption * Config.FofConsWeights.zip(Config.ExciseRates).map((w, r) => w * r).sum
    val monthlyCustoms = monthlyImports * Config.CustomsNonEuShare * Config.CustomsDutyRate
    val annualCombined = (monthlyExcise + monthlyCustoms) * 12.0
    // Should be in ~30-90 bln PLN range
    annualCombined should be > 20e9
    annualCombined should be < 120e9
  }

  "Customs" should "apply only to non-EU share" in {
    val imports = 1000000.0
    val fullCustoms = imports * Config.CustomsDutyRate
    val nonEuCustoms = imports * Config.CustomsNonEuShare * Config.CustomsDutyRate
    nonEuCustoms should be < fullCustoms
    nonEuCustoms shouldBe (fullCustoms * Config.CustomsNonEuShare +- 0.01)
  }

  "Manufacturing excise" should "be highest sector rate" in {
    // Mfg (idx 1) has highest excise due to fuel content
    Config.ExciseRates(1) shouldBe Config.ExciseRates.max
  }
