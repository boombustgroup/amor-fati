package sfc.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.types.*

class InsuranceSectorSpec extends AnyFlatSpec with Matchers:

  import sfc.config.SimParams
  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  "Insurance.zero" should "return all-zero state" in {
    val z = Insurance.zero
    z.lifeReserves shouldBe PLN.Zero
    z.nonLifeReserves shouldBe PLN.Zero
    z.govBondHoldings shouldBe PLN.Zero
    z.corpBondHoldings shouldBe PLN.Zero
    z.equityHoldings shouldBe PLN.Zero
    z.lastLifePremium shouldBe PLN.Zero
    z.lastNonLifePremium shouldBe PLN.Zero
    z.lastLifeClaims shouldBe PLN.Zero
    z.lastNonLifeClaims shouldBe PLN.Zero
    z.lastInvestmentIncome shouldBe PLN.Zero
    z.lastNetDepositChange shouldBe PLN.Zero
  }

  "Insurance.initial" should "have correct life reserves" in {
    val s = Insurance.initial
    s.lifeReserves.toDouble shouldBe (p.ins.lifeReserves.toDouble +- 1.0)
  }

  it should "have correct non-life reserves" in {
    val s = Insurance.initial
    s.nonLifeReserves.toDouble shouldBe (p.ins.nonLifeReserves.toDouble +- 1.0)
  }

  it should "have govBondHoldings = totalAssets * govBondShare" in {
    val s           = Insurance.initial
    val totalAssets = p.ins.lifeReserves.toDouble + p.ins.nonLifeReserves.toDouble
    s.govBondHoldings.toDouble shouldBe (totalAssets * p.ins.govBondShare.toDouble +- 1.0)
  }

  it should "have corpBondHoldings = totalAssets * corpBondShare" in {
    val s           = Insurance.initial
    val totalAssets = p.ins.lifeReserves.toDouble + p.ins.nonLifeReserves.toDouble
    s.corpBondHoldings.toDouble shouldBe (totalAssets * p.ins.corpBondShare.toDouble +- 1.0)
  }

  it should "have equityHoldings = totalAssets * equityShare" in {
    val s           = Insurance.initial
    val totalAssets = p.ins.lifeReserves.toDouble + p.ins.nonLifeReserves.toDouble
    s.equityHoldings.toDouble shouldBe (totalAssets * p.ins.equityShare.toDouble +- 1.0)
  }

  it should "have allocation shares summing to < 1.0 (remainder is cash/other)" in {
    val total = p.ins.govBondShare.toDouble + p.ins.corpBondShare.toDouble + p.ins.equityShare.toDouble
    total should be <= 1.0
  }

  "Insurance.step" should "compute life premium proportional to employment and wage" in {
    val prev   = Insurance.initial
    val result = Insurance.step(prev, 80000, 8000.0, 1.0, 0.05, 0.06, 0.08, 0.005)
    result.lastLifePremium.toDouble shouldBe (80000 * 8000.0 * p.ins.lifePremiumRate.toDouble +- 0.01)
  }

  it should "compute non-life premium scaling with price level" in {
    val prev = Insurance.initial
    val r1   = Insurance.step(prev, 80000, 8000.0, 1.0, 0.05, 0.06, 0.08, 0.005)
    val r2   = Insurance.step(prev, 80000, 8000.0, 1.5, 0.05, 0.06, 0.08, 0.005)
    r2.lastNonLifePremium.toDouble shouldBe (r1.lastNonLifePremium.toDouble * 1.5 +- 0.01)
  }

  it should "compute life claims = premium * loss ratio" in {
    val prev = Insurance.initial
    val r    = Insurance.step(prev, 80000, 8000.0, 1.0, 0.05, 0.06, 0.08, 0.005)
    r.lastLifeClaims.toDouble shouldBe (r.lastLifePremium.toDouble * p.ins.lifeLossRatio.toDouble +- 0.01)
  }

  it should "widen non-life claims with high unemployment" in {
    val prev  = Insurance.initial
    val rLow  = Insurance.step(prev, 80000, 8000.0, 1.0, 0.05, 0.06, 0.08, 0.005)
    val rHigh = Insurance.step(prev, 80000, 8000.0, 1.0, 0.15, 0.06, 0.08, 0.005)
    rHigh.lastNonLifeClaims.toDouble should be > rLow.lastNonLifeClaims.toDouble
  }

  it should "not widen non-life claims when unemployment is at or below 5%" in {
    val prev         = Insurance.initial
    val r            = Insurance.step(prev, 80000, 8000.0, 1.0, 0.04, 0.06, 0.08, 0.005)
    val expectedBase = r.lastNonLifePremium.toDouble * p.ins.nonLifeLossRatio.toDouble
    r.lastNonLifeClaims.toDouble shouldBe (expectedBase +- 0.01)
  }

  it should "compute positive investment income with positive yields" in {
    val prev = Insurance.initial
    val r    = Insurance.step(prev, 80000, 8000.0, 1.0, 0.05, 0.06, 0.08, 0.005)
    r.lastInvestmentIncome.toDouble should be > 0.0
  }

  it should "compute zero investment income with zero holdings" in {
    val prev = Insurance.zero
    val r    = Insurance.step(prev, 80000, 8000.0, 1.0, 0.05, 0.06, 0.08, 0.005)
    r.lastInvestmentIncome shouldBe PLN.Zero
  }

  it should "have net deposit change = -(premium - claims)" in {
    val prev        = Insurance.initial
    val r           = Insurance.step(prev, 80000, 8000.0, 1.0, 0.05, 0.06, 0.08, 0.005)
    val totalPrem   = r.lastLifePremium.toDouble + r.lastNonLifePremium.toDouble
    val totalClaims = r.lastLifeClaims.toDouble + r.lastNonLifeClaims.toDouble
    r.lastNetDepositChange.toDouble shouldBe (-(totalPrem - totalClaims) +- 0.01)
  }

  it should "have negative net deposit change in normal times (premium > claims)" in {
    val prev = Insurance.initial
    val r    = Insurance.step(prev, 80000, 8000.0, 1.0, 0.05, 0.06, 0.08, 0.005)
    r.lastNetDepositChange.toDouble should be < 0.0
  }

  it should "preserve reserves >= 0 under normal conditions" in {
    val prev = Insurance.initial
    val r    = Insurance.step(prev, 80000, 8000.0, 1.0, 0.05, 0.06, 0.08, 0.005)
    r.lifeReserves.toDouble should be >= 0.0
    r.nonLifeReserves.toDouble should be >= 0.0
  }

  it should "move govBondHoldings towards target allocation" in {
    val prev = Insurance.initial.copy(govBondHoldings = PLN.Zero) // far below target
    val r    = Insurance.step(prev, 80000, 8000.0, 1.0, 0.05, 0.06, 0.08, 0.005)
    r.govBondHoldings.toDouble should be > 0.0
  }

  it should "move equityHoldings towards target allocation" in {
    val prev = Insurance.initial.copy(equityHoldings = PLN.Zero) // far below target
    val r    = Insurance.step(prev, 80000, 8000.0, 1.0, 0.05, 0.06, 0.08, 0.005)
    r.equityHoldings.toDouble should be > 0.0
  }

  it should "be idempotent from zero state with zero employment" in {
    val prev = Insurance.zero
    val r    = Insurance.step(prev, 0, 8000.0, 1.0, 0.05, 0.0, 0.0, 0.0)
    r.lastLifePremium shouldBe PLN.Zero
    r.lastNonLifePremium shouldBe PLN.Zero
    r.lastLifeClaims shouldBe PLN.Zero
    r.lastNonLifeClaims shouldBe PLN.Zero
    r.lastInvestmentIncome shouldBe PLN.Zero
    r.lastNetDepositChange shouldBe PLN.Zero
  }

  "Config defaults" should "have InsLifeReserves consistent with KNF 2024 calibration" in {
    // ~110 bln PLN scaled by ScaleFactor (default 1.0)
    p.ins.lifeReserves.toDouble should be > 0.0
  }

  it should "have InsNonLifeReserves consistent with KNF 2024 calibration" in {
    p.ins.nonLifeReserves.toDouble should be > 0.0
  }

  it should "have allocation shares that are positive and bounded" in {
    p.ins.govBondShare.toDouble should be > 0.0
    p.ins.govBondShare.toDouble should be < 1.0
    p.ins.corpBondShare.toDouble should be > 0.0
    p.ins.corpBondShare.toDouble should be < 1.0
    p.ins.equityShare.toDouble should be > 0.0
    p.ins.equityShare.toDouble should be < 1.0
  }

  it should "have loss ratios between 0 and 1" in {
    p.ins.lifeLossRatio.toDouble should be > 0.0
    p.ins.lifeLossRatio.toDouble should be <= 1.0
    p.ins.nonLifeLossRatio.toDouble should be > 0.0
    p.ins.nonLifeLossRatio.toDouble should be <= 1.0
  }

  it should "have InsEnabled default to false" in {
    p.flags.insurance shouldBe false
  }
