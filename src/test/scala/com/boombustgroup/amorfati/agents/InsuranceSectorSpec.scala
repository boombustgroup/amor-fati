package com.boombustgroup.amorfati.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.types.*

class InsuranceSectorSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  private def mkStep(
      prev: Insurance.State = Insurance.initial,
      employed: Int = 80000,
      wage: PLN = PLN(8000.0),
      priceLevel: Double = 1.0,
      unempRate: Share = Share(0.05),
      govBondYield: Rate = Rate(0.06),
      corpBondYield: Rate = Rate(0.08),
      equityReturn: Rate = Rate(0.005),
  ): Insurance.State =
    Insurance.step(prev, employed, wage, priceLevel, unempRate, govBondYield, corpBondYield, equityReturn)

  "Insurance.State.zero" should "return all-zero state" in {
    val z = Insurance.State.zero
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
    val result = mkStep()
    result.lastLifePremium.toDouble shouldBe (80000 * 8000.0 * p.ins.lifePremiumRate.toDouble +- 0.01)
  }

  it should "compute non-life premium scaling with price level" in {
    val r1 = mkStep(priceLevel = 1.0)
    val r2 = mkStep(priceLevel = 1.5)
    r2.lastNonLifePremium.toDouble shouldBe (r1.lastNonLifePremium.toDouble * 1.5 +- 0.01)
  }

  it should "compute life claims = premium * loss ratio" in {
    val r = mkStep()
    r.lastLifeClaims.toDouble shouldBe (r.lastLifePremium.toDouble * p.ins.lifeLossRatio.toDouble +- 0.01)
  }

  it should "widen non-life claims with high unemployment" in {
    val rLow  = mkStep(unempRate = Share(0.05))
    val rHigh = mkStep(unempRate = Share(0.15))
    rHigh.lastNonLifeClaims should be > rLow.lastNonLifeClaims
  }

  it should "not widen non-life claims when unemployment is at or below 5%" in {
    val r            = mkStep(unempRate = Share(0.04))
    val expectedBase = r.lastNonLifePremium.toDouble * p.ins.nonLifeLossRatio.toDouble
    r.lastNonLifeClaims.toDouble shouldBe (expectedBase +- 0.01)
  }

  it should "compute positive investment income with positive yields" in {
    val r = mkStep()
    r.lastInvestmentIncome should be > PLN.Zero
  }

  it should "compute zero investment income with zero holdings" in {
    val r = mkStep(prev = Insurance.State.zero)
    r.lastInvestmentIncome shouldBe PLN.Zero
  }

  it should "have net deposit change = -(premium - claims)" in {
    val r           = mkStep()
    val totalPrem   = r.lastLifePremium + r.lastNonLifePremium
    val totalClaims = r.lastLifeClaims + r.lastNonLifeClaims
    r.lastNetDepositChange.toDouble shouldBe (-(totalPrem - totalClaims).toDouble +- 0.01)
  }

  it should "have negative net deposit change in normal times (premium > claims)" in {
    val r = mkStep()
    r.lastNetDepositChange should be < PLN.Zero
  }

  it should "preserve reserves >= 0 under normal conditions" in {
    val r = mkStep()
    r.lifeReserves should be >= PLN.Zero
    r.nonLifeReserves should be >= PLN.Zero
  }

  it should "move govBondHoldings towards target allocation" in {
    val prev = Insurance.initial.copy(govBondHoldings = PLN.Zero)
    val r    = mkStep(prev = prev)
    r.govBondHoldings should be > PLN.Zero
  }

  it should "move equityHoldings towards target allocation" in {
    val prev = Insurance.initial.copy(equityHoldings = PLN.Zero)
    val r    = mkStep(prev = prev)
    r.equityHoldings should be > PLN.Zero
  }

  it should "be idempotent from zero state with zero employment" in {
    val r = mkStep(prev = Insurance.State.zero, employed = 0, govBondYield = Rate.Zero, corpBondYield = Rate.Zero, equityReturn = Rate.Zero)
    r.lastLifePremium shouldBe PLN.Zero
    r.lastNonLifePremium shouldBe PLN.Zero
    r.lastLifeClaims shouldBe PLN.Zero
    r.lastNonLifeClaims shouldBe PLN.Zero
    r.lastInvestmentIncome shouldBe PLN.Zero
    r.lastNetDepositChange shouldBe PLN.Zero
  }

  "Config defaults" should "have InsLifeReserves consistent with KNF 2024 calibration" in {
    p.ins.lifeReserves should be > PLN.Zero
  }

  it should "have InsNonLifeReserves consistent with KNF 2024 calibration" in {
    p.ins.nonLifeReserves should be > PLN.Zero
  }

  it should "have allocation shares that are positive and bounded" in {
    p.ins.govBondShare should be > Share.Zero
    p.ins.govBondShare should be < Share.One
    p.ins.corpBondShare should be > Share.Zero
    p.ins.corpBondShare should be < Share.One
    p.ins.equityShare should be > Share.Zero
    p.ins.equityShare should be < Share.One
  }

  it should "have loss ratios between 0 and 1" in {
    p.ins.lifeLossRatio should be > Share.Zero
    p.ins.lifeLossRatio should be <= Share.One
    p.ins.nonLifeLossRatio should be > Share.Zero
    p.ins.nonLifeLossRatio should be <= Share.One
  }
