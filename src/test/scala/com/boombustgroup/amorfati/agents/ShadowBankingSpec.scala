package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.types.*

class ShadowBankingSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams                                  = SimParams.defaults
  private val p: SimParams                         = summon[SimParams]
  private def initialCorpBondHoldings: PLN         =
    p.nbfi.tfiInitAum * p.nbfi.tfiCorpBondShare
  private def initialOpening: Nbfi.OpeningBalances =
    val b = Nbfi.initialBalances
    Nbfi.OpeningBalances(b.tfiAum, b.tfiGovBondHoldings, initialCorpBondHoldings, b.tfiEquityHoldings, b.nbfiLoanStock)

  private def mkStep(
      opening: Nbfi.OpeningBalances = initialOpening,
      employed: Int = 50000,
      wage: PLN = PLN("8000.0"),
      priceLevel: PriceIndex = PriceIndex.Base,
      unempRate: Share = Share("0.05"),
      bankNplRatio: Share = Share("0.02"),
      govBondYield: Rate = Rate("0.05"),
      corpBondYield: Rate = Rate("0.07"),
      equityReturn: Rate = Rate("0.005"),
      depositRate: Rate = Rate("0.03"),
      domesticCons: PLN = PLN("1e8"),
      prevCorpBondHoldings: PLN = initialCorpBondHoldings,
      corpBondDefaultLoss: PLN = PLN.Zero,
  ): Nbfi.StepResult =
    Nbfi.step(
      Nbfi.StepInput(
        opening = opening.copy(corpBondHoldings = prevCorpBondHoldings),
        employed = employed,
        wage = wage,
        priceLevel = priceLevel,
        unempRate = unempRate,
        bankNplRatio = bankNplRatio,
        govBondYield = govBondYield,
        corpBondYield = corpBondYield,
        equityReturn = equityReturn,
        depositRate = depositRate,
        domesticCons = domesticCons,
        corpBondDefaultLoss = corpBondDefaultLoss,
      ),
    )

  // ---- zero / initial ----

  "Nbfi.State.zero" should "have all monthly fields at zero" in {
    val z = Nbfi.State.zero
    z.lastTfiNetInflow shouldBe PLN.Zero
    z.lastNbfiOrigination shouldBe PLN.Zero
    z.lastNbfiRepayment shouldBe PLN.Zero
    z.lastNbfiDefaultAmount shouldBe PLN.Zero
    z.lastBankTightness shouldBe Share.Zero
    z.lastDepositDrain shouldBe PLN.Zero
  }

  "Nbfi.ClosingBalances.zero" should "have all closing fields at zero" in {
    val z = Nbfi.ClosingBalances.zero
    z.tfiAum shouldBe PLN.Zero
    z.tfiGovBondHoldings shouldBe PLN.Zero
    z.tfiEquityHoldings shouldBe PLN.Zero
    z.nbfiLoanStock shouldBe PLN.Zero
  }

  "Nbfi.initialBalances" should "have correct AUM" in {
    val init = Nbfi.initialBalances
    decimal(init.tfiAum) shouldBe decimal(p.nbfi.tfiInitAum) +- BigDecimal("1.0")
  }

  it should "allocate gov bonds at target share" in {
    val init = Nbfi.initialBalances
    decimal(init.tfiGovBondHoldings) shouldBe (decimal(p.nbfi.tfiInitAum) * decimal(p.nbfi.tfiGovBondShare)) +- BigDecimal("1.0")
  }

  it should "allocate equities at target share" in {
    val init = Nbfi.initialBalances
    decimal(init.tfiEquityHoldings) shouldBe (decimal(p.nbfi.tfiInitAum) * decimal(p.nbfi.tfiEquityShare)) +- BigDecimal("1.0")
  }

  it should "have correct initial loan stock" in {
    val init = Nbfi.initialBalances
    decimal(init.nbfiLoanStock) shouldBe decimal(p.nbfi.creditInitStock) +- BigDecimal("1.0")
  }

  // ---- bankTightness ----

  "Nbfi.bankTightness" should "be 0 at NPL <= 3%" in {
    Nbfi.bankTightness(Share("0.01")) shouldBe Share.Zero
    Nbfi.bankTightness(Share("0.03")) shouldBe Share.Zero
  }

  it should "be positive at NPL > 3%" in {
    Nbfi.bankTightness(Share("0.04")) should be > Share.Zero
  }

  it should "be 1.0 at NPL = 6%" in {
    decimal(Nbfi.bankTightness(Share("0.06"))) shouldBe BigDecimal("1.0") +- BigDecimal("0.001")
  }

  it should "be capped at 1.0 for NPL > 6%" in {
    Nbfi.bankTightness(Share("0.10")) shouldBe Share.One
  }

  it should "be 0.5 at NPL = 4.5%" in {
    decimal(Nbfi.bankTightness(Share("0.045"))) shouldBe BigDecimal("0.5") +- BigDecimal("0.001")
  }

  // ---- tfiInflow ----

  "Nbfi.tfiInflow" should "be proportional to employment and wage" in {
    val i1 = Nbfi.tfiInflow(1000, PLN("8000.0"), Rate("0.0"), Rate("0.05"), Rate("0.03"))
    val i2 = Nbfi.tfiInflow(2000, PLN("8000.0"), Rate("0.0"), Rate("0.05"), Rate("0.03"))
    i2 should be > i1
    // Approximately double (modulated by returns, but base scales linearly)
    decimal(i2 / i1) shouldBe BigDecimal("2.0") +- BigDecimal("0.5")
  }

  it should "increase with excess returns" in {
    val low  = Nbfi.tfiInflow(1000, PLN("8000.0"), Rate("0.0"), Rate("0.03"), Rate("0.05")) // fund < deposit
    val high = Nbfi.tfiInflow(1000, PLN("8000.0"), Rate("0.0"), Rate("0.08"), Rate("0.02")) // fund > deposit
    high should be > low
  }

  // ---- nbfiOrigination ----

  "Nbfi.nbfiOrigination" should "be proportional to consumption" in {
    val o1 = Nbfi.nbfiOrigination(PLN("1000000.0"), Share("0.02"))
    val o2 = Nbfi.nbfiOrigination(PLN("2000000.0"), Share("0.02"))
    decimal(o2 / o1) shouldBe BigDecimal("2.0") +- BigDecimal("0.01")
  }

  it should "be counter-cyclical (increase with bank tightness)" in {
    val normal = Nbfi.nbfiOrigination(PLN("1000000.0"), Share("0.02")) // NPL 2% → tightness 0
    val tight  = Nbfi.nbfiOrigination(PLN("1000000.0"), Share("0.06")) // NPL 6% → tightness 1
    tight should be > normal
  }

  it should "equal base at zero tightness" in {
    val base = Nbfi.nbfiOrigination(PLN("1000000.0"), Share("0.03")) // NPL 3% → tightness 0
    decimal(base) shouldBe (BigDecimal("1000000.0") * decimal(p.nbfi.creditBaseRate)) +- BigDecimal("1.0")
  }

  // ---- nbfiRepayment ----

  "Nbfi.nbfiRepayment" should "equal stock / maturity" in {
    decimal(Nbfi.nbfiRepayment(PLN("360000.0"))) shouldBe (BigDecimal("360000.0") / decimal(p.nbfi.creditMaturity)) +- BigDecimal("0.01")
  }

  it should "be zero for zero stock" in {
    Nbfi.nbfiRepayment(PLN.Zero) shouldBe PLN.Zero
  }

  // ---- nbfiDefaults ----

  "Nbfi.nbfiDefaults" should "use base rate at 5% unemployment" in {
    val d = Nbfi.nbfiDefaults(PLN("100000.0"), Share("0.05"))
    decimal(d) shouldBe (BigDecimal("100000.0") * decimal(p.nbfi.defaultBase)) +- BigDecimal("0.01")
  }

  it should "increase with unemployment above 5%" in {
    val low  = Nbfi.nbfiDefaults(PLN("100000.0"), Share("0.05"))
    val high = Nbfi.nbfiDefaults(PLN("100000.0"), Share("0.10"))
    high should be > low
  }

  it should "be zero for zero stock" in {
    Nbfi.nbfiDefaults(PLN.Zero, Share("0.10")) shouldBe PLN.Zero
  }

  it should "be sensitive to unemployment (sensitivity 3.0)" in {
    val d5  = Nbfi.nbfiDefaults(PLN("100000.0"), Share("0.05"))
    val d10 = Nbfi.nbfiDefaults(PLN("100000.0"), Share("0.10"))
    // At 10%, excess = 5%, sensitivity = 3.0: factor = 1 + 3.0 * 0.05 = 1.15
    decimal(d10 / d5) shouldBe BigDecimal("1.15") +- BigDecimal("0.01")
  }

  // ---- step ----

  "Nbfi.step" should "grow AUM with positive inflow" in {
    val init   = Nbfi.initialBalances
    val result = mkStep().closing
    result.tfiAum should be > init.tfiAum
  }

  it should "reduce AUM by corporate bond default losses" in {
    val noDefault = mkStep().closing
    val withLoss  = mkStep(corpBondDefaultLoss = PLN("1000.0")).closing
    withLoss.tfiAum shouldBe noDefault.tfiAum - PLN("1000.0")
  }

  it should "include opening ledger corporate bond holdings in AUM return" in {
    val withoutCorp = mkStep(prevCorpBondHoldings = PLN.Zero).closing
    val withCorp    = mkStep(prevCorpBondHoldings = PLN("120000.0")).closing
    withCorp.tfiAum shouldBe withoutCorp.tfiAum + PLN("120000.0") * Rate("0.07").monthly
  }

  it should "produce deposit drain equal to negative inflow" in {
    val result = mkStep().state
    decimal(result.lastDepositDrain) shouldBe -decimal(result.lastTfiNetInflow) +- BigDecimal("0.01")
  }

  it should "maintain Identity 13 (NBFI credit stock)" in {
    val init           = Nbfi.initialBalances
    val result         = mkStep()
    val expectedChange = decimal(result.state.lastNbfiOrigination - result.state.lastNbfiRepayment - result.state.lastNbfiDefaultAmount)
    val actualChange   = decimal(result.closing.nbfiLoanStock - init.nbfiLoanStock)
    actualChange shouldBe expectedChange +- BigDecimal("0.01")
  }

  it should "rebalance TFI portfolio towards targets" in {
    val offTarget = Nbfi.OpeningBalances(
      tfiAum = PLN("1000000.0"),
      tfiGovBondHoldings = PLN.Zero,
      corpBondHoldings = PLN.Zero,
      tfiEquityHoldings = PLN.Zero,
      nbfiLoanStock = PLN("100000.0"),
    )
    val result    = mkStep(opening = offTarget, prevCorpBondHoldings = PLN.Zero).closing
    result.tfiGovBondHoldings should be > PLN.Zero
  }

  it should "increase origination when bank NPL is high (counter-cyclical)" in {
    val normal = mkStep(bankNplRatio = Share("0.02")).state
    val tight  = mkStep(bankNplRatio = Share("0.06")).state
    tight.lastNbfiOrigination should be > normal.lastNbfiOrigination
    tight.lastBankTightness should be > normal.lastBankTightness
  }

  it should "produce positive interest income from loan stock" in {
    val init   = Nbfi.initialBalances
    val result = mkStep().state
    if init.nbfiLoanStock > PLN.Zero then result.lastNbfiInterestIncome should be > PLN.Zero
  }

  // ---- Config defaults ----

  "Config" should "have correct TFI allocation shares" in {
    p.nbfi.tfiGovBondShare shouldBe Share("0.40")
    p.nbfi.tfiCorpBondShare shouldBe Share("0.10")
    p.nbfi.tfiEquityShare shouldBe Share("0.10")
  }
