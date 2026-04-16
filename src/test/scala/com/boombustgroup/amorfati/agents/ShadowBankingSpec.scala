package com.boombustgroup.amorfati.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.types.*

class ShadowBankingSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]
  private val td           = ComputationBoundary

  private def mkStep(
      prev: Nbfi.State = Nbfi.initial,
      employed: Int = 50000,
      wage: PLN = PLN(8000.0),
      priceLevel: PriceIndex = PriceIndex.Base,
      unempRate: Share = Share(0.05),
      bankNplRatio: Share = Share(0.02),
      govBondYield: Rate = Rate(0.05),
      corpBondYield: Rate = Rate(0.07),
      equityReturn: Rate = Rate(0.005),
      depositRate: Rate = Rate(0.03),
      domesticCons: PLN = PLN(1e8),
      settledCorpBondHoldings: Option[PLN] = None,
  ): Nbfi.State =
    Nbfi.step(
      prev,
      employed,
      wage,
      priceLevel,
      unempRate,
      bankNplRatio,
      govBondYield,
      corpBondYield,
      equityReturn,
      depositRate,
      domesticCons,
      settledCorpBondHoldings.getOrElse(prev.tfiCorpBondHoldings),
    )

  // ---- zero / initial ----

  "Nbfi.State.zero" should "have all fields at zero" in {
    val z = Nbfi.State.zero
    z.tfiAum shouldBe PLN.Zero
    z.tfiGovBondHoldings shouldBe PLN.Zero
    z.tfiCorpBondHoldings shouldBe PLN.Zero
    z.tfiEquityHoldings shouldBe PLN.Zero
    z.tfiCashHoldings shouldBe PLN.Zero
    z.nbfiLoanStock shouldBe PLN.Zero
    z.lastTfiNetInflow shouldBe PLN.Zero
    z.lastNbfiOrigination shouldBe PLN.Zero
    z.lastNbfiRepayment shouldBe PLN.Zero
    z.lastNbfiDefaultAmount shouldBe PLN.Zero
    z.lastBankTightness shouldBe Share.Zero
    z.lastDepositDrain shouldBe PLN.Zero
  }

  "Nbfi.initial" should "have correct AUM" in {
    val init = Nbfi.initial
    td.toDouble(init.tfiAum) shouldBe td.toDouble(p.nbfi.tfiInitAum) +- 1.0
  }

  it should "allocate gov bonds at target share" in {
    val init = Nbfi.initial
    td.toDouble(init.tfiGovBondHoldings) shouldBe (td.toDouble(p.nbfi.tfiInitAum) * td.toDouble(p.nbfi.tfiGovBondShare)) +- 1.0
  }

  it should "allocate corp bonds at target share" in {
    val init = Nbfi.initial
    td.toDouble(init.tfiCorpBondHoldings) shouldBe (td.toDouble(p.nbfi.tfiInitAum) * td.toDouble(p.nbfi.tfiCorpBondShare)) +- 1.0
  }

  it should "allocate equities at target share" in {
    val init = Nbfi.initial
    td.toDouble(init.tfiEquityHoldings) shouldBe (td.toDouble(p.nbfi.tfiInitAum) * td.toDouble(p.nbfi.tfiEquityShare)) +- 1.0
  }

  it should "allocate residual to cash" in {
    val init         = Nbfi.initial
    val expectedCash = td.toDouble(p.nbfi.tfiInitAum) *
      (1.0 - td.toDouble(p.nbfi.tfiGovBondShare) - td.toDouble(p.nbfi.tfiCorpBondShare) - td.toDouble(p.nbfi.tfiEquityShare))
    td.toDouble(init.tfiCashHoldings) shouldBe expectedCash +- 1.0
  }

  it should "have correct initial loan stock" in {
    val init = Nbfi.initial
    td.toDouble(init.nbfiLoanStock) shouldBe td.toDouble(p.nbfi.creditInitStock) +- 1.0
  }

  // ---- bankTightness ----

  "Nbfi.bankTightness" should "be 0 at NPL <= 3%" in {
    Nbfi.bankTightness(Share(0.01)) shouldBe Share.Zero
    Nbfi.bankTightness(Share(0.03)) shouldBe Share.Zero
  }

  it should "be positive at NPL > 3%" in {
    Nbfi.bankTightness(Share(0.04)) should be > Share.Zero
  }

  it should "be 1.0 at NPL = 6%" in {
    td.toDouble(Nbfi.bankTightness(Share(0.06))) shouldBe 1.0 +- 0.001
  }

  it should "be capped at 1.0 for NPL > 6%" in {
    Nbfi.bankTightness(Share(0.10)) shouldBe Share.One
  }

  it should "be 0.5 at NPL = 4.5%" in {
    td.toDouble(Nbfi.bankTightness(Share(0.045))) shouldBe 0.5 +- 0.001
  }

  // ---- tfiInflow ----

  "Nbfi.tfiInflow" should "be proportional to employment and wage" in {
    val i1 = Nbfi.tfiInflow(1000, PLN(8000.0), Rate(0.0), Rate(0.05), Rate(0.03))
    val i2 = Nbfi.tfiInflow(2000, PLN(8000.0), Rate(0.0), Rate(0.05), Rate(0.03))
    i2 should be > i1
    // Approximately double (modulated by returns, but base scales linearly)
    (i2 / i1) shouldBe 2.0 +- 0.5
  }

  it should "increase with excess returns" in {
    val low  = Nbfi.tfiInflow(1000, PLN(8000.0), Rate(0.0), Rate(0.03), Rate(0.05)) // fund < deposit
    val high = Nbfi.tfiInflow(1000, PLN(8000.0), Rate(0.0), Rate(0.08), Rate(0.02)) // fund > deposit
    high should be > low
  }

  // ---- nbfiOrigination ----

  "Nbfi.nbfiOrigination" should "be proportional to consumption" in {
    val o1 = Nbfi.nbfiOrigination(PLN(1000000.0), Share(0.02))
    val o2 = Nbfi.nbfiOrigination(PLN(2000000.0), Share(0.02))
    (o2 / o1) shouldBe 2.0 +- 0.01
  }

  it should "be counter-cyclical (increase with bank tightness)" in {
    val normal = Nbfi.nbfiOrigination(PLN(1000000.0), Share(0.02)) // NPL 2% → tightness 0
    val tight  = Nbfi.nbfiOrigination(PLN(1000000.0), Share(0.06)) // NPL 6% → tightness 1
    tight should be > normal
  }

  it should "equal base at zero tightness" in {
    val base = Nbfi.nbfiOrigination(PLN(1000000.0), Share(0.03)) // NPL 3% → tightness 0
    td.toDouble(base) shouldBe (1000000.0 * td.toDouble(p.nbfi.creditBaseRate)) +- 1.0
  }

  // ---- nbfiRepayment ----

  "Nbfi.nbfiRepayment" should "equal stock / maturity" in {
    td.toDouble(Nbfi.nbfiRepayment(PLN(360000.0))) shouldBe (360000.0 / td.toDouble(p.nbfi.creditMaturity)) +- 0.01
  }

  it should "be zero for zero stock" in {
    Nbfi.nbfiRepayment(PLN.Zero) shouldBe PLN.Zero
  }

  // ---- nbfiDefaults ----

  "Nbfi.nbfiDefaults" should "use base rate at 5% unemployment" in {
    val d = Nbfi.nbfiDefaults(PLN(100000.0), Share(0.05))
    td.toDouble(d) shouldBe (100000.0 * td.toDouble(p.nbfi.defaultBase)) +- 0.01
  }

  it should "increase with unemployment above 5%" in {
    val low  = Nbfi.nbfiDefaults(PLN(100000.0), Share(0.05))
    val high = Nbfi.nbfiDefaults(PLN(100000.0), Share(0.10))
    high should be > low
  }

  it should "be zero for zero stock" in {
    Nbfi.nbfiDefaults(PLN.Zero, Share(0.10)) shouldBe PLN.Zero
  }

  it should "be sensitive to unemployment (sensitivity 3.0)" in {
    val d5  = Nbfi.nbfiDefaults(PLN(100000.0), Share(0.05))
    val d10 = Nbfi.nbfiDefaults(PLN(100000.0), Share(0.10))
    // At 10%, excess = 5%, sensitivity = 3.0: factor = 1 + 3.0 * 0.05 = 1.15
    (d10 / d5) shouldBe 1.15 +- 0.01
  }

  // ---- step ----

  "Nbfi.step" should "grow AUM with positive inflow" in {
    val init   = Nbfi.initial
    val result = mkStep(prev = init)
    result.tfiAum should be > init.tfiAum
  }

  it should "produce deposit drain equal to negative inflow" in {
    val result = mkStep()
    td.toDouble(result.lastDepositDrain) shouldBe -td.toDouble(result.lastTfiNetInflow) +- 0.01
  }

  it should "maintain Identity 13 (NBFI credit stock)" in {
    val init           = Nbfi.initial
    val result         = mkStep(prev = init)
    val expectedChange = td.toDouble(result.lastNbfiOrigination - result.lastNbfiRepayment - result.lastNbfiDefaultAmount)
    val actualChange   = td.toDouble(result.nbfiLoanStock - init.nbfiLoanStock)
    actualChange shouldBe expectedChange +- 0.01
  }

  it should "rebalance TFI portfolio towards targets" in {
    val offTarget = Nbfi.State(
      tfiAum = PLN(1000000.0),
      tfiGovBondHoldings = PLN.Zero,
      tfiCorpBondHoldings = PLN.Zero,
      tfiEquityHoldings = PLN.Zero,
      tfiCashHoldings = PLN(1000000.0),
      nbfiLoanStock = PLN(100000.0),
      lastTfiNetInflow = PLN.Zero,
      lastNbfiOrigination = PLN.Zero,
      lastNbfiRepayment = PLN.Zero,
      lastNbfiDefaultAmount = PLN.Zero,
      lastNbfiInterestIncome = PLN.Zero,
      lastBankTightness = Share.Zero,
      lastDepositDrain = PLN.Zero,
    )
    val result    = mkStep(prev = offTarget)
    result.tfiGovBondHoldings should be > PLN.Zero
  }

  it should "take TFI corpBondHoldings from corporate bond market settlement" in {
    val settled = PLN(123456.0)
    val result  = mkStep(settledCorpBondHoldings = Some(settled))
    result.tfiCorpBondHoldings shouldBe settled
  }

  it should "increase origination when bank NPL is high (counter-cyclical)" in {
    val normal = mkStep(bankNplRatio = Share(0.02))
    val tight  = mkStep(bankNplRatio = Share(0.06))
    tight.lastNbfiOrigination should be > normal.lastNbfiOrigination
    tight.lastBankTightness should be > normal.lastBankTightness
  }

  it should "produce positive interest income from loan stock" in {
    val init   = Nbfi.initial
    val result = mkStep(prev = init)
    if init.nbfiLoanStock > PLN.Zero then result.lastNbfiInterestIncome should be > PLN.Zero
  }

  // ---- Config defaults ----

  "Config" should "have correct TFI allocation shares" in {
    p.nbfi.tfiGovBondShare shouldBe Share(0.40)
    p.nbfi.tfiCorpBondShare shouldBe Share(0.10)
    p.nbfi.tfiEquityShare shouldBe Share(0.10)
  }
