package sfc.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.config.Config
import sfc.types.*

class ShadowBankingSpec extends AnyFlatSpec with Matchers:

  // ---- zero / initial ----

  "Nbfi.zero" should "have all fields at zero" in {
    val z = Nbfi.zero
    z.tfiAum.toDouble shouldBe 0.0
    z.tfiGovBondHoldings.toDouble shouldBe 0.0
    z.tfiCorpBondHoldings.toDouble shouldBe 0.0
    z.tfiEquityHoldings.toDouble shouldBe 0.0
    z.tfiCashHoldings.toDouble shouldBe 0.0
    z.nbfiLoanStock.toDouble shouldBe 0.0
    z.lastTfiNetInflow.toDouble shouldBe 0.0
    z.lastNbfiOrigination.toDouble shouldBe 0.0
    z.lastNbfiRepayment.toDouble shouldBe 0.0
    z.lastNbfiDefaultAmount.toDouble shouldBe 0.0
    z.lastBankTightness shouldBe Ratio.Zero
    z.lastDepositDrain.toDouble shouldBe 0.0
  }

  "Nbfi.initial" should "have correct AUM" in {
    val init = Nbfi.initial
    init.tfiAum.toDouble shouldBe Config.NbfiTfiInitAum +- 1.0
  }

  it should "allocate gov bonds at target share" in {
    val init = Nbfi.initial
    init.tfiGovBondHoldings.toDouble shouldBe (Config.NbfiTfiInitAum * Config.NbfiTfiGovBondShare) +- 1.0
  }

  it should "allocate corp bonds at target share" in {
    val init = Nbfi.initial
    init.tfiCorpBondHoldings.toDouble shouldBe (Config.NbfiTfiInitAum * Config.NbfiTfiCorpBondShare) +- 1.0
  }

  it should "allocate equities at target share" in {
    val init = Nbfi.initial
    init.tfiEquityHoldings.toDouble shouldBe (Config.NbfiTfiInitAum * Config.NbfiTfiEquityShare) +- 1.0
  }

  it should "allocate residual to cash" in {
    val init = Nbfi.initial
    val expectedCash = Config.NbfiTfiInitAum *
      (1.0 - Config.NbfiTfiGovBondShare - Config.NbfiTfiCorpBondShare - Config.NbfiTfiEquityShare)
    init.tfiCashHoldings.toDouble shouldBe expectedCash +- 1.0
  }

  it should "have correct initial loan stock" in {
    val init = Nbfi.initial
    init.nbfiLoanStock.toDouble shouldBe Config.NbfiCreditInitStock +- 1.0
  }

  // ---- bankTightness ----

  "Nbfi.bankTightness" should "be 0 at NPL <= 3%" in {
    Nbfi.bankTightness(0.01) shouldBe 0.0
    Nbfi.bankTightness(0.03) shouldBe 0.0
  }

  it should "be positive at NPL > 3%" in {
    Nbfi.bankTightness(0.04) should be > 0.0
  }

  it should "be 1.0 at NPL = 6%" in {
    Nbfi.bankTightness(0.06) shouldBe 1.0 +- 0.001
  }

  it should "be capped at 1.0 for NPL > 6%" in {
    Nbfi.bankTightness(0.10) shouldBe 1.0
  }

  it should "be 0.5 at NPL = 4.5%" in {
    Nbfi.bankTightness(0.045) shouldBe 0.5 +- 0.001
  }

  // ---- tfiInflow ----

  "Nbfi.tfiInflow" should "be proportional to employment and wage" in {
    val i1 = Nbfi.tfiInflow(1000, 8000.0, 0.0, 0.05, 0.03)
    val i2 = Nbfi.tfiInflow(2000, 8000.0, 0.0, 0.05, 0.03)
    i2 should be > i1
    // Approximately double (modulated by returns, but base scales linearly)
    (i2 / i1) shouldBe 2.0 +- 0.5
  }

  it should "increase with excess returns" in {
    val low = Nbfi.tfiInflow(1000, 8000.0, 0.0, 0.03, 0.05) // fund < deposit
    val high = Nbfi.tfiInflow(1000, 8000.0, 0.0, 0.08, 0.02) // fund > deposit
    high should be > low
  }

  // ---- nbfiOrigination ----

  "Nbfi.nbfiOrigination" should "be proportional to consumption" in {
    val o1 = Nbfi.nbfiOrigination(1000000.0, 0.02)
    val o2 = Nbfi.nbfiOrigination(2000000.0, 0.02)
    (o2 / o1) shouldBe 2.0 +- 0.01
  }

  it should "be counter-cyclical (increase with bank tightness)" in {
    val normal = Nbfi.nbfiOrigination(1000000.0, 0.02) // NPL 2% → tightness 0
    val tight = Nbfi.nbfiOrigination(1000000.0, 0.06) // NPL 6% → tightness 1
    tight should be > normal
  }

  it should "equal base at zero tightness" in {
    val base = Nbfi.nbfiOrigination(1000000.0, 0.03) // NPL 3% → tightness 0
    base shouldBe (1000000.0 * Config.NbfiCreditBaseRate) +- 1.0
  }

  // ---- nbfiRepayment ----

  "Nbfi.nbfiRepayment" should "equal stock / maturity" in {
    Nbfi.nbfiRepayment(360000.0) shouldBe (360000.0 / Config.NbfiCreditMaturity) +- 0.01
  }

  it should "be zero for zero stock" in {
    Nbfi.nbfiRepayment(0.0) shouldBe 0.0
  }

  // ---- nbfiDefaults ----

  "Nbfi.nbfiDefaults" should "use base rate at 5% unemployment" in {
    val d = Nbfi.nbfiDefaults(100000.0, 0.05)
    d shouldBe (100000.0 * Config.NbfiDefaultBase) +- 0.01
  }

  it should "increase with unemployment above 5%" in {
    val low = Nbfi.nbfiDefaults(100000.0, 0.05)
    val high = Nbfi.nbfiDefaults(100000.0, 0.10)
    high should be > low
  }

  it should "be zero for zero stock" in {
    Nbfi.nbfiDefaults(0.0, 0.10) shouldBe 0.0
  }

  it should "be sensitive to unemployment (sensitivity 3.0)" in {
    val d5 = Nbfi.nbfiDefaults(100000.0, 0.05)
    val d10 = Nbfi.nbfiDefaults(100000.0, 0.10)
    // At 10%, excess = 5%, sensitivity = 3.0: factor = 1 + 3.0 * 0.05 = 1.15
    (d10 / d5) shouldBe 1.15 +- 0.01
  }

  // ---- step ----

  "Nbfi.step" should "grow AUM with positive inflow" in {
    val init = Nbfi.initial
    val result = Nbfi.step(init, 50000, 8000.0, 1.0, 0.05, 0.02, 0.05, 0.07, 0.005, 0.03, 1e8)
    result.tfiAum > init.tfiAum shouldBe true
  }

  it should "produce deposit drain equal to negative inflow" in {
    val init = Nbfi.initial
    val result = Nbfi.step(init, 50000, 8000.0, 1.0, 0.05, 0.02, 0.05, 0.07, 0.005, 0.03, 1e8)
    result.lastDepositDrain.toDouble shouldBe -result.lastTfiNetInflow.toDouble +- 0.01
  }

  it should "maintain Identity 13 (NBFI credit stock)" in {
    val init = Nbfi.initial
    val result = Nbfi.step(init, 50000, 8000.0, 1.0, 0.05, 0.02, 0.05, 0.07, 0.005, 0.03, 1e8)
    val expectedChange = (result.lastNbfiOrigination - result.lastNbfiRepayment - result.lastNbfiDefaultAmount).toDouble
    val actualChange = (result.nbfiLoanStock - init.nbfiLoanStock).toDouble
    actualChange shouldBe expectedChange +- 0.01
  }

  it should "rebalance TFI portfolio towards targets" in {
    // Start with all in cash (off-target)
    val offTarget = Nbfi.State(
      tfiAum = PLN(1000000.0),
      tfiGovBondHoldings = PLN.Zero,
      tfiCorpBondHoldings = PLN.Zero,
      tfiEquityHoldings = PLN.Zero,
      tfiCashHoldings = PLN(1000000.0),
      nbfiLoanStock = PLN(100000.0),
    )
    val result = Nbfi.step(offTarget, 50000, 8000.0, 1.0, 0.05, 0.02, 0.05, 0.07, 0.005, 0.03, 1e8)
    // Gov bond holdings should increase towards target
    result.tfiGovBondHoldings > PLN.Zero shouldBe true
  }

  it should "increase origination when bank NPL is high (counter-cyclical)" in {
    val init = Nbfi.initial
    val normal = Nbfi.step(init, 50000, 8000.0, 1.0, 0.05, 0.02, 0.05, 0.07, 0.005, 0.03, 1e8)
    val tight = Nbfi.step(init, 50000, 8000.0, 1.0, 0.05, 0.06, 0.05, 0.07, 0.005, 0.03, 1e8)
    tight.lastNbfiOrigination > normal.lastNbfiOrigination shouldBe true
    tight.lastBankTightness.toDouble should be > normal.lastBankTightness.toDouble
  }

  it should "produce positive interest income from loan stock" in {
    val init = Nbfi.initial
    val result = Nbfi.step(init, 50000, 8000.0, 1.0, 0.05, 0.02, 0.05, 0.07, 0.005, 0.03, 1e8)
    if init.nbfiLoanStock > PLN.Zero then result.lastNbfiInterestIncome > PLN.Zero shouldBe true
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
