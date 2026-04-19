package com.boombustgroup.amorfati.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.fp.ComputationBoundary
import com.boombustgroup.amorfati.types.*

class InsuranceSectorSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams                                       = SimParams.defaults
  private val p: SimParams                              = summon[SimParams]
  private val td                                        = ComputationBoundary
  private def initialCorpBondHoldings: PLN              =
    (p.ins.lifeReserves + p.ins.nonLifeReserves) * p.ins.corpBondShare
  private def initialOpening: Insurance.OpeningBalances =
    val b = Insurance.initialBalances
    Insurance.OpeningBalances(b.lifeReserves, b.nonLifeReserves, b.govBondHoldings, initialCorpBondHoldings, b.equityHoldings)

  private def mkStep(
      opening: Insurance.OpeningBalances = initialOpening,
      employed: Int = 80000,
      wage: PLN = PLN(8000.0),
      unempRate: Share = Share(0.05),
      govBondYield: Rate = Rate(0.06),
      corpBondYield: Rate = Rate(0.08),
      equityReturn: Rate = Rate(0.005),
      prevCorpBondHoldings: PLN = initialCorpBondHoldings,
      corpBondDefaultLoss: PLN = PLN.Zero,
  ): Insurance.StepResult =
    Insurance.step(
      Insurance.StepInput(
        opening = opening.copy(corpBondHoldings = prevCorpBondHoldings),
        employed = employed,
        wage = wage,
        unempRate = unempRate,
        govBondYield = govBondYield,
        corpBondYield = corpBondYield,
        equityReturn = equityReturn,
        corpBondDefaultLoss = corpBondDefaultLoss,
      ),
    )

  "Insurance.State.zero" should "return all-zero monthly state" in {
    val z = Insurance.State.zero
    z.lastLifePremium shouldBe PLN.Zero
    z.lastNonLifePremium shouldBe PLN.Zero
    z.lastLifeClaims shouldBe PLN.Zero
    z.lastNonLifeClaims shouldBe PLN.Zero
    z.lastInvestmentIncome shouldBe PLN.Zero
    z.lastNetDepositChange shouldBe PLN.Zero
  }

  "Insurance.ClosingBalances.zero" should "return all-zero closing balances" in {
    val z = Insurance.ClosingBalances.zero
    z.lifeReserves shouldBe PLN.Zero
    z.nonLifeReserves shouldBe PLN.Zero
    z.govBondHoldings shouldBe PLN.Zero
    z.equityHoldings shouldBe PLN.Zero
  }

  "Insurance.initialBalances" should "have correct life reserves" in {
    val s = Insurance.initialBalances
    td.toDouble(s.lifeReserves) shouldBe (td.toDouble(p.ins.lifeReserves) +- 1.0)
  }

  it should "have correct non-life reserves" in {
    val s = Insurance.initialBalances
    td.toDouble(s.nonLifeReserves) shouldBe (td.toDouble(p.ins.nonLifeReserves) +- 1.0)
  }

  it should "have govBondHoldings = totalAssets * govBondShare" in {
    val s           = Insurance.initialBalances
    val totalAssets = td.toDouble(p.ins.lifeReserves) + td.toDouble(p.ins.nonLifeReserves)
    td.toDouble(s.govBondHoldings) shouldBe (totalAssets * td.toDouble(p.ins.govBondShare) +- 1.0)
  }

  it should "have equityHoldings = totalAssets * equityShare" in {
    val s           = Insurance.initialBalances
    val totalAssets = td.toDouble(p.ins.lifeReserves) + td.toDouble(p.ins.nonLifeReserves)
    td.toDouble(s.equityHoldings) shouldBe (totalAssets * td.toDouble(p.ins.equityShare) +- 1.0)
  }

  it should "have allocation shares summing to < 1.0 (remainder is cash/other)" in {
    val total = td.toDouble(p.ins.govBondShare) + td.toDouble(p.ins.corpBondShare) + td.toDouble(p.ins.equityShare)
    total should be <= 1.0
  }

  "Insurance.step" should "compute life premium proportional to employment and wage" in {
    val result = mkStep()
    td.toDouble(result.state.lastLifePremium) shouldBe (80000 * 8000.0 * td.toDouble(p.ins.lifePremiumRate) +- 0.01)
  }

  it should "compute non-life premium proportional to employment and wage" in {
    val result = mkStep()
    td.toDouble(result.state.lastNonLifePremium) shouldBe (80000 * 8000.0 * td.toDouble(p.ins.nonLifePremiumRate) +- 0.01)
  }

  it should "compute life claims = premium * loss ratio" in {
    val r = mkStep().state
    td.toDouble(r.lastLifeClaims) shouldBe (td.toDouble(r.lastLifePremium) * td.toDouble(p.ins.lifeLossRatio) +- 0.01)
  }

  it should "widen non-life claims with high unemployment" in {
    val rLow  = mkStep(unempRate = Share(0.05)).state
    val rHigh = mkStep(unempRate = Share(0.15)).state
    rHigh.lastNonLifeClaims should be > rLow.lastNonLifeClaims
  }

  it should "not widen non-life claims when unemployment is at or below 5%" in {
    val r            = mkStep(unempRate = Share(0.04)).state
    val expectedBase = td.toDouble(r.lastNonLifePremium) * td.toDouble(p.ins.nonLifeLossRatio)
    td.toDouble(r.lastNonLifeClaims) shouldBe (expectedBase +- 0.01)
  }

  it should "compute positive investment income with positive yields" in {
    val r = mkStep().state
    r.lastInvestmentIncome should be > PLN.Zero
  }

  it should "reduce investment income by corporate bond default losses" in {
    val noDefault = mkStep().state
    val withLoss  = mkStep(corpBondDefaultLoss = PLN(1000.0)).state
    withLoss.lastInvestmentIncome shouldBe noDefault.lastInvestmentIncome - PLN(1000.0)
  }

  it should "include opening ledger corporate bond holdings in investment income" in {
    val withoutCorp = mkStep(prevCorpBondHoldings = PLN.Zero).state
    val withCorp    = mkStep(prevCorpBondHoldings = PLN(120000.0)).state
    withCorp.lastInvestmentIncome shouldBe withoutCorp.lastInvestmentIncome + PLN(120000.0) * Rate(0.08).monthly
  }

  it should "compute zero investment income with zero holdings" in {
    val r = mkStep(
      opening = Insurance.OpeningBalances(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      prevCorpBondHoldings = PLN.Zero,
    ).state
    r.lastInvestmentIncome shouldBe PLN.Zero
  }

  it should "have net deposit change = -(premium - claims)" in {
    val r           = mkStep().state
    val totalPrem   = r.lastLifePremium + r.lastNonLifePremium
    val totalClaims = r.lastLifeClaims + r.lastNonLifeClaims
    td.toDouble(r.lastNetDepositChange) shouldBe (-td.toDouble(totalPrem - totalClaims) +- 0.01)
  }

  it should "have negative net deposit change in normal times (premium > claims)" in {
    val r = mkStep().state
    r.lastNetDepositChange should be < PLN.Zero
  }

  it should "preserve reserves >= 0 under normal conditions" in {
    val r = mkStep().closing
    r.lifeReserves should be >= PLN.Zero
    r.nonLifeReserves should be >= PLN.Zero
  }

  it should "move govBondHoldings towards target allocation" in {
    val prev = initialOpening.copy(govBondHoldings = PLN.Zero)
    val r    = mkStep(opening = prev).closing
    r.govBondHoldings should be > PLN.Zero
  }

  it should "move equityHoldings towards target allocation" in {
    val prev = initialOpening.copy(equityHoldings = PLN.Zero)
    val r    = mkStep(opening = prev).closing
    r.equityHoldings should be > PLN.Zero
  }

  it should "be idempotent from zero state with zero employment" in {
    val r = mkStep(
      opening = Insurance.OpeningBalances(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      employed = 0,
      govBondYield = Rate.Zero,
      corpBondYield = Rate.Zero,
      equityReturn = Rate.Zero,
      prevCorpBondHoldings = PLN.Zero,
    ).state
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
