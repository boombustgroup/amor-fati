package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.engine.markets.CorporateBondMarket
import sfc.types.*

class CorporateBondSpec extends AnyFlatSpec with Matchers:

  import sfc.config.SimParams
  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  private val initState = CorporateBondMarket.initial

  "CorporateBondMarket.zero" should "return all-zero state" in {
    val z = CorporateBondMarket.zero
    z.outstanding shouldBe PLN.Zero
    z.bankHoldings shouldBe PLN.Zero
    z.ppkHoldings shouldBe PLN.Zero
    z.otherHoldings shouldBe PLN.Zero
    z.corpBondYield shouldBe Rate.Zero
    z.lastIssuance shouldBe PLN.Zero
    z.lastAmortization shouldBe PLN.Zero
    z.lastCouponIncome shouldBe PLN.Zero
    z.lastDefaultLoss shouldBe PLN.Zero
    z.lastDefaultAmount shouldBe PLN.Zero
  }

  "CorporateBondMarket.initial" should "have stock = CorpBondInitStock" in {
    initState.outstanding.toDouble shouldBe (p.corpBond.initStock.toDouble +- 1.0)
  }

  it should "allocate holders summing to 100%" in {
    val total = initState.bankHoldings + initState.ppkHoldings + initState.otherHoldings
    total.toDouble shouldBe (initState.outstanding.toDouble +- 1.0)
  }

  it should "have bank holdings = stock * CorpBondBankShare" in {
    initState.bankHoldings.toDouble shouldBe (initState.outstanding.toDouble * p.corpBond.bankShare.toDouble +- 1.0)
  }

  it should "have ppk holdings = stock * CorpBondPpkShare" in {
    initState.ppkHoldings.toDouble shouldBe (initState.outstanding.toDouble * p.corpBond.ppkShare.toDouble +- 1.0)
  }

  "computeYield" should "equal govYield + spread when nplRatio = 0" in {
    val y = CorporateBondMarket.computeYield(Rate(0.06), Ratio.Zero)
    y.toDouble shouldBe (0.06 + p.corpBond.spread.toDouble +- 0.001)
  }

  it should "widen with NPL ratio" in {
    val y0 = CorporateBondMarket.computeYield(Rate(0.06), Ratio.Zero)
    val y1 = CorporateBondMarket.computeYield(Rate(0.06), Ratio(0.05))
    y1 should be > y0
  }

  it should "cap spread at 10%" in {
    val y = CorporateBondMarket.computeYield(Rate(0.06), Ratio.One)
    y.toDouble should be <= (0.06 + 0.10 + 0.001)
  }

  "computeCoupon" should "be proportional to holdings" in {
    val coupon = CorporateBondMarket.computeCoupon(initState)
    coupon.total should be > PLN.Zero
    coupon.bank.toDouble shouldBe (initState.bankHoldings.toDouble * initState.corpBondYield.toDouble / 12.0 +- 0.01)
    coupon.ppk.toDouble shouldBe (initState.ppkHoldings.toDouble * initState.corpBondYield.toDouble / 12.0 +- 0.01)
  }

  it should "return zeros for zero outstanding" in {
    val coupon = CorporateBondMarket.computeCoupon(CorporateBondMarket.zero)
    coupon.total shouldBe PLN.Zero
    coupon.bank shouldBe PLN.Zero
    coupon.ppk shouldBe PLN.Zero
  }

  "amortization" should "equal outstanding / maturity" in {
    val a = CorporateBondMarket.amortization(initState)
    a.toDouble shouldBe (initState.outstanding.toDouble / p.corpBond.maturity +- 0.01)
  }

  "processDefaults" should "return zeros when no defaults" in {
    val r = CorporateBondMarket.processDefaults(initState, PLN.Zero)
    r.grossDefault shouldBe PLN.Zero
    r.lossAfterRecovery shouldBe PLN.Zero
    r.bankLoss shouldBe PLN.Zero
    r.ppkLoss shouldBe PLN.Zero
  }

  it should "allocate defaults proportionally to holdings" in {
    val defaultAmt = PLN(1000.0)
    val r          = CorporateBondMarket.processDefaults(initState, defaultAmt)
    r.grossDefault shouldBe defaultAmt
    r.lossAfterRecovery.toDouble shouldBe (1000.0 * (1.0 - p.corpBond.recovery.toDouble) +- 0.01)
    val bankFrac   = initState.bankHoldings / initState.outstanding
    r.bankLoss.toDouble shouldBe (1000.0 * bankFrac * (1.0 - p.corpBond.recovery.toDouble) +- 0.01)
  }

  "processIssuance" should "increase all holder buckets" in {
    val issuance = PLN(5000.0)
    val result   = CorporateBondMarket.processIssuance(initState, issuance)
    result.outstanding.toDouble shouldBe (initState.outstanding.toDouble + 5000.0 +- 0.01)
    result.bankHoldings.toDouble shouldBe (initState.bankHoldings.toDouble + 5000.0 * p.corpBond.bankShare.toDouble +- 0.01)
    result.ppkHoldings.toDouble shouldBe (initState.ppkHoldings.toDouble + 5000.0 * p.corpBond.ppkShare.toDouble +- 0.01)
    result.lastIssuance shouldBe issuance
  }

  it should "not change state for zero issuance" in {
    val result = CorporateBondMarket.processIssuance(initState, PLN.Zero)
    result.outstanding shouldBe initState.outstanding
    result.lastIssuance shouldBe PLN.Zero
  }

  "step" should "reduce outstanding by amortization + defaults" in {
    val prevOutstanding = initState.outstanding
    val result          = CorporateBondMarket.step(CorporateBondMarket.StepInput(initState, Rate(0.06), Ratio.Zero, PLN.Zero, PLN.Zero))
    val amort           = prevOutstanding.toDouble / p.corpBond.maturity
    result.outstanding.toDouble shouldBe (prevOutstanding.toDouble - amort +- 1.0)
  }

  it should "satisfy SFC Identity 12: delta = issuance - amort - default" in {
    val issuance       = PLN(2000.0)
    val default        = PLN(500.0)
    val result         = CorporateBondMarket.step(CorporateBondMarket.StepInput(initState, Rate(0.06), Ratio(0.02), default, issuance))
    val amort          = initState.outstanding.toDouble / p.corpBond.maturity
    val expectedChange = 2000.0 - amort - 500.0
    val actualChange   = result.outstanding.toDouble - initState.outstanding.toDouble
    actualChange shouldBe (expectedChange +- 1.0)
  }

  "p.corpBond.spread.toDouble" should "be 250 bps by default" in {
    p.corpBond.spread.toDouble shouldBe 0.025
  }

  "p.corpBond.recovery.toDouble" should "be 30% by default" in {
    p.corpBond.recovery.toDouble shouldBe 0.30
  }

  "p.corpBond.maturity" should "be 60 months by default" in {
    p.corpBond.maturity shouldBe 60.0
  }

  "p.corpBond.minSize" should "be 50 workers by default" in {
    p.corpBond.minSize shouldBe 50
  }

  "computeAbsorption" should "return 1.0 when spread = base and CAR comfortable" in {
    val state      = initState.copy(creditSpread = Rate(p.corpBond.spread.toDouble))
    val absorption = CorporateBondMarket.computeAbsorption(state, PLN(1000.0), Ratio(0.15), Ratio(0.08))
    absorption shouldBe Ratio.One
  }

  it should "return 1.0 for zero issuance" in {
    val absorption = CorporateBondMarket.computeAbsorption(initState, PLN.Zero, Ratio(0.05), Ratio(0.08))
    absorption shouldBe Ratio.One
  }

  it should "decrease with widening spread" in {
    val normalState = initState.copy(creditSpread = Rate(p.corpBond.spread.toDouble))
    val wideState   = initState.copy(creditSpread = Rate(p.corpBond.spread.toDouble + 0.05))
    val a1          = CorporateBondMarket.computeAbsorption(normalState, PLN(1000.0), Ratio(0.15), Ratio(0.08))
    val a2          = CorporateBondMarket.computeAbsorption(wideState, PLN(1000.0), Ratio(0.15), Ratio(0.08))
    a2 should be < a1
  }

  it should "return 0.3 floor at extreme spread" in {
    val extremeState = initState.copy(creditSpread = Rate(p.corpBond.spread.toDouble + 0.15))
    val absorption   = CorporateBondMarket.computeAbsorption(extremeState, PLN(1000.0), Ratio(0.15), Ratio(0.08))
    absorption.toDouble shouldBe (0.3 +- 0.001)
  }

  it should "decrease when CAR near minimum" in {
    val state = initState.copy(creditSpread = Rate(p.corpBond.spread.toDouble))
    val a1    = CorporateBondMarket.computeAbsorption(state, PLN(1000.0), Ratio(0.15), Ratio(0.08))
    val a2    = CorporateBondMarket.computeAbsorption(state, PLN(1000.0), Ratio(0.09), Ratio(0.08))
    a2 should be < a1
  }

  it should "return 0.3 when CAR at or below minimum" in {
    val state      = initState.copy(creditSpread = Rate(p.corpBond.spread.toDouble))
    val absorption = CorporateBondMarket.computeAbsorption(state, PLN(1000.0), Ratio(0.08), Ratio(0.08))
    absorption.toDouble shouldBe (0.3 +- 0.001)
  }

  "BankingAggregate.car" should "include corpBondHoldings at 50% risk weight" in {
    import sfc.accounting.BankingAggregate
    val bank = BankingAggregate(
      totalLoans = PLN(1000.0),
      nplAmount = PLN(0.0),
      capital = PLN(200.0),
      deposits = PLN(5000.0),
      govBondHoldings = PLN.Zero,
      consumerLoans = PLN.Zero,
      consumerNpl = PLN.Zero,
      corpBondHoldings = PLN(400.0),
    )
    // RWA = 1000 + 400 * 0.5 = 1200; CAR = 200 / 1200 = 0.1667
    bank.car.toDouble shouldBe (200.0 / 1200.0 +- 0.001)
  }
