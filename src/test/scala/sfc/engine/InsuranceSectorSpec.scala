package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.config.Config

class InsuranceSectorSpec extends AnyFlatSpec with Matchers:

  "InsuranceSector.zero" should "return all-zero state" in {
    val z = InsuranceSector.zero
    z.lifeReserves shouldBe 0.0
    z.nonLifeReserves shouldBe 0.0
    z.govBondHoldings shouldBe 0.0
    z.corpBondHoldings shouldBe 0.0
    z.equityHoldings shouldBe 0.0
    z.lastLifePremium shouldBe 0.0
    z.lastNonLifePremium shouldBe 0.0
    z.lastLifeClaims shouldBe 0.0
    z.lastNonLifeClaims shouldBe 0.0
    z.lastInvestmentIncome shouldBe 0.0
    z.lastNetDepositChange shouldBe 0.0
  }

  "InsuranceSector.initial" should "have correct life reserves" in {
    val s = InsuranceSector.initial
    s.lifeReserves shouldBe (Config.InsLifeReserves +- 1.0)
  }

  it should "have correct non-life reserves" in {
    val s = InsuranceSector.initial
    s.nonLifeReserves shouldBe (Config.InsNonLifeReserves +- 1.0)
  }

  it should "have govBondHoldings = totalAssets * govBondShare" in {
    val s = InsuranceSector.initial
    val totalAssets = Config.InsLifeReserves + Config.InsNonLifeReserves
    s.govBondHoldings shouldBe (totalAssets * Config.InsGovBondShare +- 1.0)
  }

  it should "have corpBondHoldings = totalAssets * corpBondShare" in {
    val s = InsuranceSector.initial
    val totalAssets = Config.InsLifeReserves + Config.InsNonLifeReserves
    s.corpBondHoldings shouldBe (totalAssets * Config.InsCorpBondShare +- 1.0)
  }

  it should "have equityHoldings = totalAssets * equityShare" in {
    val s = InsuranceSector.initial
    val totalAssets = Config.InsLifeReserves + Config.InsNonLifeReserves
    s.equityHoldings shouldBe (totalAssets * Config.InsEquityShare +- 1.0)
  }

  it should "have allocation shares summing to < 1.0 (remainder is cash/other)" in {
    val total = Config.InsGovBondShare + Config.InsCorpBondShare + Config.InsEquityShare
    total should be <= 1.0
  }

  "InsuranceSector.step" should "compute life premium proportional to employment and wage" in {
    val prev = InsuranceSector.initial
    val result = InsuranceSector.step(prev, 80000, 8000.0, 1.0, 0.05, 0.06, 0.08, 0.005)
    result.lastLifePremium shouldBe (80000 * 8000.0 * Config.InsLifePremiumRate +- 0.01)
  }

  it should "compute non-life premium scaling with price level" in {
    val prev = InsuranceSector.initial
    val r1 = InsuranceSector.step(prev, 80000, 8000.0, 1.0, 0.05, 0.06, 0.08, 0.005)
    val r2 = InsuranceSector.step(prev, 80000, 8000.0, 1.5, 0.05, 0.06, 0.08, 0.005)
    r2.lastNonLifePremium shouldBe (r1.lastNonLifePremium * 1.5 +- 0.01)
  }

  it should "compute life claims = premium * loss ratio" in {
    val prev = InsuranceSector.initial
    val r = InsuranceSector.step(prev, 80000, 8000.0, 1.0, 0.05, 0.06, 0.08, 0.005)
    r.lastLifeClaims shouldBe (r.lastLifePremium * Config.InsLifeLossRatio +- 0.01)
  }

  it should "widen non-life claims with high unemployment" in {
    val prev = InsuranceSector.initial
    val rLow = InsuranceSector.step(prev, 80000, 8000.0, 1.0, 0.05, 0.06, 0.08, 0.005)
    val rHigh = InsuranceSector.step(prev, 80000, 8000.0, 1.0, 0.15, 0.06, 0.08, 0.005)
    rHigh.lastNonLifeClaims should be > rLow.lastNonLifeClaims
  }

  it should "not widen non-life claims when unemployment is at or below 5%" in {
    val prev = InsuranceSector.initial
    val r = InsuranceSector.step(prev, 80000, 8000.0, 1.0, 0.04, 0.06, 0.08, 0.005)
    val expectedBase = r.lastNonLifePremium * Config.InsNonLifeLossRatio
    r.lastNonLifeClaims shouldBe (expectedBase +- 0.01)
  }

  it should "compute positive investment income with positive yields" in {
    val prev = InsuranceSector.initial
    val r = InsuranceSector.step(prev, 80000, 8000.0, 1.0, 0.05, 0.06, 0.08, 0.005)
    r.lastInvestmentIncome should be > 0.0
  }

  it should "compute zero investment income with zero holdings" in {
    val prev = InsuranceSector.zero
    val r = InsuranceSector.step(prev, 80000, 8000.0, 1.0, 0.05, 0.06, 0.08, 0.005)
    r.lastInvestmentIncome shouldBe 0.0
  }

  it should "have net deposit change = -(premium - claims)" in {
    val prev = InsuranceSector.initial
    val r = InsuranceSector.step(prev, 80000, 8000.0, 1.0, 0.05, 0.06, 0.08, 0.005)
    val totalPrem = r.lastLifePremium + r.lastNonLifePremium
    val totalClaims = r.lastLifeClaims + r.lastNonLifeClaims
    r.lastNetDepositChange shouldBe (-(totalPrem - totalClaims) +- 0.01)
  }

  it should "have negative net deposit change in normal times (premium > claims)" in {
    val prev = InsuranceSector.initial
    val r = InsuranceSector.step(prev, 80000, 8000.0, 1.0, 0.05, 0.06, 0.08, 0.005)
    r.lastNetDepositChange should be < 0.0
  }

  it should "preserve reserves >= 0 under normal conditions" in {
    val prev = InsuranceSector.initial
    val r = InsuranceSector.step(prev, 80000, 8000.0, 1.0, 0.05, 0.06, 0.08, 0.005)
    r.lifeReserves should be >= 0.0
    r.nonLifeReserves should be >= 0.0
  }

  it should "move govBondHoldings towards target allocation" in {
    val prev = InsuranceSector.initial.copy(govBondHoldings = 0.0) // far below target
    val r = InsuranceSector.step(prev, 80000, 8000.0, 1.0, 0.05, 0.06, 0.08, 0.005)
    r.govBondHoldings should be > 0.0
  }

  it should "move equityHoldings towards target allocation" in {
    val prev = InsuranceSector.initial.copy(equityHoldings = 0.0) // far below target
    val r = InsuranceSector.step(prev, 80000, 8000.0, 1.0, 0.05, 0.06, 0.08, 0.005)
    r.equityHoldings should be > 0.0
  }

  it should "be idempotent from zero state with zero employment" in {
    val prev = InsuranceSector.zero
    val r = InsuranceSector.step(prev, 0, 8000.0, 1.0, 0.05, 0.0, 0.0, 0.0)
    r.lastLifePremium shouldBe 0.0
    r.lastNonLifePremium shouldBe 0.0
    r.lastLifeClaims shouldBe 0.0
    r.lastNonLifeClaims shouldBe 0.0
    r.lastInvestmentIncome shouldBe 0.0
    r.lastNetDepositChange shouldBe 0.0
  }

  "Config defaults" should "have InsLifeReserves consistent with KNF 2024 calibration" in {
    // ~110 bln PLN scaled by ScaleFactor (default 1.0)
    Config.InsLifeReserves should be > 0.0
  }

  it should "have InsNonLifeReserves consistent with KNF 2024 calibration" in {
    Config.InsNonLifeReserves should be > 0.0
  }

  it should "have allocation shares that are positive and bounded" in {
    Config.InsGovBondShare should be > 0.0
    Config.InsGovBondShare should be < 1.0
    Config.InsCorpBondShare should be > 0.0
    Config.InsCorpBondShare should be < 1.0
    Config.InsEquityShare should be > 0.0
    Config.InsEquityShare should be < 1.0
  }

  it should "have loss ratios between 0 and 1" in {
    Config.InsLifeLossRatio should be > 0.0
    Config.InsLifeLossRatio should be <= 1.0
    Config.InsNonLifeLossRatio should be > 0.0
    Config.InsNonLifeLossRatio should be <= 1.0
  }

  it should "have InsEnabled default to false" in {
    Config.InsEnabled shouldBe false
  }
