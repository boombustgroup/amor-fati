package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.config.Config

class ShadowBankingSpec extends AnyFlatSpec with Matchers:

  // ---- zero / initial ----

  "ShadowBanking.zero" should "have all fields at zero" in {
    val z = ShadowBanking.zero
    z.tfiAum shouldBe 0.0
    z.tfiGovBondHoldings shouldBe 0.0
    z.tfiCorpBondHoldings shouldBe 0.0
    z.tfiEquityHoldings shouldBe 0.0
    z.tfiCashHoldings shouldBe 0.0
    z.nbfiLoanStock shouldBe 0.0
    z.lastTfiNetInflow shouldBe 0.0
    z.lastNbfiOrigination shouldBe 0.0
    z.lastNbfiRepayment shouldBe 0.0
    z.lastNbfiDefaultAmount shouldBe 0.0
    z.lastBankTightness shouldBe 0.0
    z.lastDepositDrain shouldBe 0.0
  }

  "ShadowBanking.initial" should "have correct AUM" in {
    val init = ShadowBanking.initial
    init.tfiAum shouldBe Config.NbfiTfiInitAum +- 1.0
  }

  it should "allocate gov bonds at target share" in {
    val init = ShadowBanking.initial
    init.tfiGovBondHoldings shouldBe (Config.NbfiTfiInitAum * Config.NbfiTfiGovBondShare) +- 1.0
  }

  it should "allocate corp bonds at target share" in {
    val init = ShadowBanking.initial
    init.tfiCorpBondHoldings shouldBe (Config.NbfiTfiInitAum * Config.NbfiTfiCorpBondShare) +- 1.0
  }

  it should "allocate equities at target share" in {
    val init = ShadowBanking.initial
    init.tfiEquityHoldings shouldBe (Config.NbfiTfiInitAum * Config.NbfiTfiEquityShare) +- 1.0
  }

  it should "allocate residual to cash" in {
    val init = ShadowBanking.initial
    val expectedCash = Config.NbfiTfiInitAum *
      (1.0 - Config.NbfiTfiGovBondShare - Config.NbfiTfiCorpBondShare - Config.NbfiTfiEquityShare)
    init.tfiCashHoldings shouldBe expectedCash +- 1.0
  }

  it should "have correct initial loan stock" in {
    val init = ShadowBanking.initial
    init.nbfiLoanStock shouldBe Config.NbfiCreditInitStock +- 1.0
  }

  // ---- bankTightness ----

  "ShadowBanking.bankTightness" should "be 0 at NPL <= 3%" in {
    ShadowBanking.bankTightness(0.01) shouldBe 0.0
    ShadowBanking.bankTightness(0.03) shouldBe 0.0
  }

  it should "be positive at NPL > 3%" in {
    ShadowBanking.bankTightness(0.04) should be > 0.0
  }

  it should "be 1.0 at NPL = 6%" in {
    ShadowBanking.bankTightness(0.06) shouldBe 1.0 +- 0.001
  }

  it should "be capped at 1.0 for NPL > 6%" in {
    ShadowBanking.bankTightness(0.10) shouldBe 1.0
  }

  it should "be 0.5 at NPL = 4.5%" in {
    ShadowBanking.bankTightness(0.045) shouldBe 0.5 +- 0.001
  }

  // ---- tfiInflow ----

  "ShadowBanking.tfiInflow" should "be proportional to employment and wage" in {
    val i1 = ShadowBanking.tfiInflow(1000, 8000.0, 0.0, 0.05, 0.03)
    val i2 = ShadowBanking.tfiInflow(2000, 8000.0, 0.0, 0.05, 0.03)
    i2 should be > i1
    // Approximately double (modulated by returns, but base scales linearly)
    (i2 / i1) shouldBe 2.0 +- 0.5
  }

  it should "increase with excess returns" in {
    val low = ShadowBanking.tfiInflow(1000, 8000.0, 0.0, 0.03, 0.05)  // fund < deposit
    val high = ShadowBanking.tfiInflow(1000, 8000.0, 0.0, 0.08, 0.02) // fund > deposit
    high should be > low
  }

  // ---- nbfiOrigination ----

  "ShadowBanking.nbfiOrigination" should "be proportional to consumption" in {
    val o1 = ShadowBanking.nbfiOrigination(1000000.0, 0.02)
    val o2 = ShadowBanking.nbfiOrigination(2000000.0, 0.02)
    (o2 / o1) shouldBe 2.0 +- 0.01
  }

  it should "be counter-cyclical (increase with bank tightness)" in {
    val normal = ShadowBanking.nbfiOrigination(1000000.0, 0.02)  // NPL 2% → tightness 0
    val tight = ShadowBanking.nbfiOrigination(1000000.0, 0.06)   // NPL 6% → tightness 1
    tight should be > normal
  }

  it should "equal base at zero tightness" in {
    val base = ShadowBanking.nbfiOrigination(1000000.0, 0.03)  // NPL 3% → tightness 0
    base shouldBe (1000000.0 * Config.NbfiCreditBaseRate) +- 1.0
  }

  // ---- nbfiRepayment ----

  "ShadowBanking.nbfiRepayment" should "equal stock / maturity" in {
    ShadowBanking.nbfiRepayment(360000.0) shouldBe (360000.0 / Config.NbfiCreditMaturity) +- 0.01
  }

  it should "be zero for zero stock" in {
    ShadowBanking.nbfiRepayment(0.0) shouldBe 0.0
  }

  // ---- nbfiDefaults ----

  "ShadowBanking.nbfiDefaults" should "use base rate at 5% unemployment" in {
    val d = ShadowBanking.nbfiDefaults(100000.0, 0.05)
    d shouldBe (100000.0 * Config.NbfiDefaultBase) +- 0.01
  }

  it should "increase with unemployment above 5%" in {
    val low = ShadowBanking.nbfiDefaults(100000.0, 0.05)
    val high = ShadowBanking.nbfiDefaults(100000.0, 0.10)
    high should be > low
  }

  it should "be zero for zero stock" in {
    ShadowBanking.nbfiDefaults(0.0, 0.10) shouldBe 0.0
  }

  it should "be sensitive to unemployment (sensitivity 3.0)" in {
    val d5 = ShadowBanking.nbfiDefaults(100000.0, 0.05)
    val d10 = ShadowBanking.nbfiDefaults(100000.0, 0.10)
    // At 10%, excess = 5%, sensitivity = 3.0: factor = 1 + 3.0 * 0.05 = 1.15
    (d10 / d5) shouldBe 1.15 +- 0.01
  }

  // ---- step ----

  "ShadowBanking.step" should "grow AUM with positive inflow" in {
    val init = ShadowBanking.initial
    val result = ShadowBanking.step(init, 50000, 8000.0, 1.0, 0.05, 0.02,
      0.05, 0.07, 0.005, 0.03, 1e8)
    result.tfiAum should be > init.tfiAum
  }

  it should "produce deposit drain equal to negative inflow" in {
    val init = ShadowBanking.initial
    val result = ShadowBanking.step(init, 50000, 8000.0, 1.0, 0.05, 0.02,
      0.05, 0.07, 0.005, 0.03, 1e8)
    result.lastDepositDrain shouldBe -result.lastTfiNetInflow +- 0.01
  }

  it should "maintain Identity 13 (NBFI credit stock)" in {
    val init = ShadowBanking.initial
    val result = ShadowBanking.step(init, 50000, 8000.0, 1.0, 0.05, 0.02,
      0.05, 0.07, 0.005, 0.03, 1e8)
    val expectedChange = result.lastNbfiOrigination - result.lastNbfiRepayment - result.lastNbfiDefaultAmount
    val actualChange = result.nbfiLoanStock - init.nbfiLoanStock
    actualChange shouldBe expectedChange +- 0.01
  }

  it should "rebalance TFI portfolio towards targets" in {
    // Start with all in cash (off-target)
    val offTarget = NbfiState(
      tfiAum = 1000000.0,
      tfiGovBondHoldings = 0.0,
      tfiCorpBondHoldings = 0.0,
      tfiEquityHoldings = 0.0,
      tfiCashHoldings = 1000000.0,
      nbfiLoanStock = 100000.0
    )
    val result = ShadowBanking.step(offTarget, 50000, 8000.0, 1.0, 0.05, 0.02,
      0.05, 0.07, 0.005, 0.03, 1e8)
    // Gov bond holdings should increase towards target
    result.tfiGovBondHoldings should be > 0.0
  }

  it should "increase origination when bank NPL is high (counter-cyclical)" in {
    val init = ShadowBanking.initial
    val normal = ShadowBanking.step(init, 50000, 8000.0, 1.0, 0.05, 0.02,
      0.05, 0.07, 0.005, 0.03, 1e8)
    val tight = ShadowBanking.step(init, 50000, 8000.0, 1.0, 0.05, 0.06,
      0.05, 0.07, 0.005, 0.03, 1e8)
    tight.lastNbfiOrigination should be > normal.lastNbfiOrigination
    tight.lastBankTightness should be > normal.lastBankTightness
  }

  it should "produce positive interest income from loan stock" in {
    val init = ShadowBanking.initial
    val result = ShadowBanking.step(init, 50000, 8000.0, 1.0, 0.05, 0.02,
      0.05, 0.07, 0.005, 0.03, 1e8)
    if init.nbfiLoanStock > 0 then
      result.lastNbfiInterestIncome should be > 0.0
  }

  // ---- Config defaults ----

  "Config" should "have NbfiEnabled=false by default" in {
    Config.NbfiEnabled shouldBe false
  }

  it should "have correct TFI allocation shares" in {
    Config.NbfiTfiGovBondShare shouldBe 0.40
    Config.NbfiTfiCorpBondShare shouldBe 0.10
    Config.NbfiTfiEquityShare shouldBe 0.10
  }
