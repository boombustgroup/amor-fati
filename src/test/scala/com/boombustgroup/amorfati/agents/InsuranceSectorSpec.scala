package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.types.*

class InsuranceSectorSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams                                       = SimParams.defaults
  private val p: SimParams                              = summon[SimParams]
  private def initialCorpBondHoldings: PLN              =
    (p.ins.lifeReserves + p.ins.nonLifeReserves) * p.ins.corpBondShare
  private def initialOpening: Insurance.OpeningBalances =
    val b = Insurance.initialBalances
    Insurance.OpeningBalances(b.lifeReserves, b.nonLifeReserves, b.govBondHoldings, initialCorpBondHoldings, b.equityHoldings)

  private def mkStep(
      opening: Insurance.OpeningBalances = initialOpening,
      employed: Int = 80000,
      wage: PLN = PLN(8000),
      unempRate: Share = Share.decimal(5, 2),
      govBondYield: Rate = Rate.decimal(6, 2),
      corpBondYield: Rate = Rate.decimal(8, 2),
      equityReturn: Rate = Rate.decimal(5, 3),
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
    decimal(s.lifeReserves) shouldBe (decimal(p.ins.lifeReserves) +- BigDecimal("1.0"))
  }

  it should "have correct non-life reserves" in {
    val s = Insurance.initialBalances
    decimal(s.nonLifeReserves) shouldBe (decimal(p.ins.nonLifeReserves) +- BigDecimal("1.0"))
  }

  it should "have govBondHoldings = totalAssets * govBondShare" in {
    val s           = Insurance.initialBalances
    val totalAssets = decimal(p.ins.lifeReserves) + decimal(p.ins.nonLifeReserves)
    decimal(s.govBondHoldings) shouldBe (totalAssets * decimal(p.ins.govBondShare) +- BigDecimal("1.0"))
  }

  it should "have equityHoldings = totalAssets * equityShare" in {
    val s           = Insurance.initialBalances
    val totalAssets = decimal(p.ins.lifeReserves) + decimal(p.ins.nonLifeReserves)
    decimal(s.equityHoldings) shouldBe (totalAssets * decimal(p.ins.equityShare) +- BigDecimal("1.0"))
  }

  it should "have allocation shares summing to < 1.0 (remainder is cash/other)" in {
    val total = decimal(p.ins.govBondShare) + decimal(p.ins.corpBondShare) + decimal(p.ins.equityShare)
    total should be <= BigDecimal("1.0")
  }

  "Insurance.step" should "compute life premium proportional to employment and wage" in {
    val result = mkStep()
    decimal(result.state.lastLifePremium) shouldBe (BigDecimal(80000) * BigDecimal("8000.0") * decimal(p.ins.lifePremiumRate) +- BigDecimal("0.01"))
  }

  it should "compute non-life premium proportional to employment and wage" in {
    val result = mkStep()
    decimal(result.state.lastNonLifePremium) shouldBe (BigDecimal(80000) * BigDecimal("8000.0") * decimal(p.ins.nonLifePremiumRate) +- BigDecimal("0.01"))
  }

  it should "compute life claims = premium * loss ratio" in {
    val r = mkStep().state
    decimal(r.lastLifeClaims) shouldBe (decimal(r.lastLifePremium) * decimal(p.ins.lifeLossRatio) +- BigDecimal("0.01"))
  }

  it should "widen non-life claims with high unemployment" in {
    val rLow  = mkStep(unempRate = Share.decimal(5, 2)).state
    val rHigh = mkStep(unempRate = Share.decimal(15, 2)).state
    rHigh.lastNonLifeClaims should be > rLow.lastNonLifeClaims
  }

  it should "not widen non-life claims when unemployment is at or below 5%" in {
    val r            = mkStep(unempRate = Share.decimal(4, 2)).state
    val expectedBase = decimal(r.lastNonLifePremium) * decimal(p.ins.nonLifeLossRatio)
    decimal(r.lastNonLifeClaims) shouldBe (expectedBase +- BigDecimal("0.01"))
  }

  it should "compute positive investment income with positive yields" in {
    val r = mkStep().state
    r.lastInvestmentIncome should be > PLN.Zero
  }

  it should "reduce investment income by corporate bond default losses" in {
    val noDefault = mkStep().state
    val withLoss  = mkStep(corpBondDefaultLoss = PLN(1000)).state
    withLoss.lastInvestmentIncome shouldBe noDefault.lastInvestmentIncome - PLN(1000)
  }

  it should "include opening ledger corporate bond holdings in investment income" in {
    val withoutCorp = mkStep(prevCorpBondHoldings = PLN.Zero).state
    val withCorp    = mkStep(prevCorpBondHoldings = PLN(120000)).state
    withCorp.lastInvestmentIncome shouldBe withoutCorp.lastInvestmentIncome + PLN(120000) * Rate.decimal(8, 2).monthly
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
    decimal(r.lastNetDepositChange) shouldBe (-decimal(totalPrem - totalClaims) +- BigDecimal("0.01"))
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
