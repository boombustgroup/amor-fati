package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.markets.CorporateBondMarket
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CorporateBondSpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  private val onePln                                                                                         = PLN.fromLong(1)
  private val fiveKPln                                                                                       = PLN.fromLong(5000)
  private def pln(units: Long): PLN                                                                          = PLN.fromLong(units)
  private def rateBps(bps: Long): Rate                                                                       = Rate.fromRaw(bps)
  private def shareBps(bps: Long): Share                                                                     = Share.fromRaw(bps)
  private def multBps(bps: Long): Multiplier                                                                 = Multiplier.fromRaw(bps)
  private def shouldBeClosePln(actual: PLN, expected: PLN, tolerance: PLN): Unit                             = (actual - expected).abs should be <= tolerance
  private def shouldBeCloseRate(actual: Rate, expected: Rate, tolerance: Rate): Unit                         = (actual - expected).abs should be <= tolerance
  private def shouldBeCloseMultiplier(actual: Multiplier, expected: Multiplier, tolerance: Multiplier): Unit = (actual - expected).abs should be <= tolerance

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

  "CorporateBondMarket.initial" should "have stock = CorpBondInitStock" in
    shouldBeClosePln(initState.outstanding, p.corpBond.initStock, onePln)

  it should "allocate holders summing to 100%" in {
    val total = initState.bankHoldings + initState.ppkHoldings + initState.otherHoldings
    shouldBeClosePln(total, initState.outstanding, onePln)
  }

  it should "have bank holdings = stock * CorpBondBankShare" in
    shouldBeClosePln(initState.bankHoldings, initState.outstanding * p.corpBond.bankShare, onePln)

  it should "have ppk holdings = stock * CorpBondPpkShare" in
    shouldBeClosePln(initState.ppkHoldings, initState.outstanding * p.corpBond.ppkShare, onePln)

  "computeYield" should "equal govYield + spread when nplRatio = 0" in {
    val y = CorporateBondMarket.computeYield(rateBps(600), Share.Zero)
    shouldBeCloseRate(y, rateBps(600) + p.corpBond.spread, Rate.fromRaw(10))
  }

  it should "widen with NPL ratio" in {
    val y0 = CorporateBondMarket.computeYield(Rate(0.06), Share.Zero)
    val y1 = CorporateBondMarket.computeYield(Rate(0.06), Share(0.05))
    y1 should be > y0
  }

  it should "cap spread at 10%" in {
    val y = CorporateBondMarket.computeYield(rateBps(600), Share.One)
    y should be <= rateBps(1600)
  }

  "computeCoupon" should "be proportional to holdings" in {
    val coupon = CorporateBondMarket.computeCoupon(initState)
    coupon.total should be > PLN.Zero
    coupon.bank shouldBe (initState.bankHoldings * initState.corpBondYield.monthly)
    coupon.ppk shouldBe (initState.ppkHoldings * initState.corpBondYield.monthly)
  }

  it should "return zeros for zero outstanding" in {
    val coupon = CorporateBondMarket.computeCoupon(CorporateBondMarket.zero)
    coupon.total shouldBe PLN.Zero
    coupon.bank shouldBe PLN.Zero
    coupon.ppk shouldBe PLN.Zero
  }

  "amortization" should "equal outstanding / maturity" in {
    val a = CorporateBondMarket.amortization(initState)
    a shouldBe (initState.outstanding / p.corpBond.maturity)
  }

  "processDefaults" should "return zeros when no defaults" in {
    val r = CorporateBondMarket.processDefaults(initState, PLN.Zero)
    r.grossDefault shouldBe PLN.Zero
    r.lossAfterRecovery shouldBe PLN.Zero
    r.bankLoss shouldBe PLN.Zero
    r.ppkLoss shouldBe PLN.Zero
  }

  it should "allocate defaults proportionally to holdings" in {
    val defaultAmt = pln(1_000_000)
    val r          = CorporateBondMarket.processDefaults(initState, defaultAmt)
    r.grossDefault shouldBe defaultAmt
    shouldBeClosePln(r.lossAfterRecovery, defaultAmt * (Share.One - p.corpBond.recovery), pln(5_000))
    val bankFrac   = initState.bankHoldings.ratioTo(initState.outstanding).toShare
    shouldBeClosePln(r.bankLoss, defaultAmt * bankFrac * (Share.One - p.corpBond.recovery), pln(5_000))
  }

  "processIssuance" should "increase all holder buckets" in {
    val issuance = fiveKPln
    val result   = CorporateBondMarket.processIssuance(initState, issuance)
    result.outstanding shouldBe initState.outstanding + issuance
    shouldBeClosePln(result.bankHoldings, initState.bankHoldings + issuance * p.corpBond.bankShare, onePln)
    shouldBeClosePln(result.ppkHoldings, initState.ppkHoldings + issuance * p.corpBond.ppkShare, onePln)
    result.lastIssuance shouldBe issuance
  }

  it should "allocate issuance with exact holder sums under residual rounding" in {
    val issuance = pln(2)
    val result   = CorporateBondMarket.processIssuance(initState, issuance)
    val deltaSum =
      (result.bankHoldings - initState.bankHoldings) +
        (result.ppkHoldings - initState.ppkHoldings) +
        (result.otherHoldings - initState.otherHoldings)
    deltaSum shouldBe issuance
  }

  it should "not change state for zero issuance" in {
    val result = CorporateBondMarket.processIssuance(initState, PLN.Zero)
    result.outstanding shouldBe initState.outstanding
    result.lastIssuance shouldBe PLN.Zero
  }

  "step" should "reduce outstanding by amortization + defaults" in {
    val prevOutstanding = initState.outstanding
    val result          = CorporateBondMarket.step(CorporateBondMarket.StepInput(initState, rateBps(600), Share.Zero, PLN.Zero, PLN.Zero))
    result.outstanding shouldBe (prevOutstanding - (prevOutstanding / p.corpBond.maturity))
  }

  it should "preserve exact holder reduction sum under residual rounding" in {
    val prev            = initState.copy(
      outstanding = pln(3000),
      bankHoldings = pln(1000),
      ppkHoldings = pln(1000),
      otherHoldings = pln(1000),
    )
    val result          = CorporateBondMarket.step(CorporateBondMarket.StepInput(prev, rateBps(600), Share.Zero, PLN.Zero, PLN.Zero))
    val holderReduction =
      (prev.bankHoldings - result.bankHoldings) +
        (prev.ppkHoldings - result.ppkHoldings) +
        (prev.otherHoldings - result.otherHoldings)
    holderReduction shouldBe (prev.outstanding - result.outstanding)
  }

  it should "satisfy SFC Identity 12: delta = issuance - amort - default" in {
    val issuance = pln(2000)
    val default  = pln(500)
    val result   = CorporateBondMarket.step(CorporateBondMarket.StepInput(initState, rateBps(600), shareBps(200), default, issuance))
    val amort    = initState.outstanding / p.corpBond.maturity
    result.outstanding - initState.outstanding shouldBe (issuance - amort - default)
  }

  "p.corpBond.spread" should "be 250 bps by default" in {
    p.corpBond.spread shouldBe rateBps(250)
  }

  "p.corpBond.recovery" should "be 30% by default" in {
    p.corpBond.recovery shouldBe shareBps(3000)
  }

  "p.corpBond.maturity" should "be 60 months by default" in {
    p.corpBond.maturity shouldBe 60
  }

  "p.corpBond.minSize" should "be 50 workers by default" in {
    p.corpBond.minSize shouldBe 50
  }

  "computeAbsorption" should "return 1.0 when spread = base and CAR comfortable" in {
    val state      = initState.copy(creditSpread = p.corpBond.spread)
    val absorption = CorporateBondMarket.computeAbsorption(state, pln(1000), multBps(1500), multBps(800))
    absorption shouldBe Share.One
  }

  it should "return 1.0 for zero issuance" in {
    val absorption = CorporateBondMarket.computeAbsorption(initState, PLN.Zero, multBps(500), multBps(800))
    absorption shouldBe Share.One
  }

  it should "decrease with widening spread" in {
    val normalState = initState.copy(creditSpread = p.corpBond.spread)
    val wideState   = initState.copy(creditSpread = p.corpBond.spread + rateBps(500))
    val a1          = CorporateBondMarket.computeAbsorption(normalState, pln(1000), multBps(1500), multBps(800))
    val a2          = CorporateBondMarket.computeAbsorption(wideState, pln(1000), multBps(1500), multBps(800))
    a2 should be < a1
  }

  it should "return 0.3 floor at extreme spread" in {
    val extremeState = initState.copy(creditSpread = p.corpBond.spread + rateBps(1500))
    val absorption   = CorporateBondMarket.computeAbsorption(extremeState, pln(1000), multBps(1500), multBps(800))
    absorption shouldBe shareBps(3000)
  }

  it should "decrease when CAR near minimum" in {
    val state = initState.copy(creditSpread = p.corpBond.spread)
    val a1    = CorporateBondMarket.computeAbsorption(state, pln(1000), multBps(1500), multBps(800))
    val a2    = CorporateBondMarket.computeAbsorption(state, pln(1000), multBps(900), multBps(800))
    a2 should be < a1
  }

  it should "return 0.3 when CAR at or below minimum" in {
    val state      = initState.copy(creditSpread = p.corpBond.spread)
    val absorption = CorporateBondMarket.computeAbsorption(state, pln(1000), multBps(800), multBps(800))
    absorption shouldBe shareBps(3000)
  }

  "BankingAggregate.car" should "include corpBondHoldings at 50% risk weight" in {
    import com.boombustgroup.amorfati.agents.Banking
    val bank = Banking.Aggregate(
      totalLoans = PLN(1000.0),
      nplAmount = PLN(0.0),
      capital = PLN(200.0),
      deposits = PLN(5000.0),
      afsBonds = PLN.Zero,
      htmBonds = PLN.Zero,
      consumerLoans = PLN.Zero,
      consumerNpl = PLN.Zero,
      corpBondHoldings = PLN(400.0),
    )
    // RWA = 1000 + 400 * 0.5 = 1200; CAR = 200 / 1200 = 0.1667
    shouldBeCloseMultiplier(bank.car, Multiplier(200.0 / 1200.0), Multiplier(0.001))
  }
