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
  private val initStock = CorporateBondMarket.initialStock

  "CorporateBondMarket.zero" should "return all-zero state" in {
    val z = CorporateBondMarket.zero
    z.corpBondYield shouldBe Rate.Zero
    z.lastIssuance shouldBe PLN.Zero
    z.lastAmortization shouldBe PLN.Zero
    z.lastCouponIncome shouldBe PLN.Zero
    z.lastDefaultLoss shouldBe PLN.Zero
    z.lastDefaultAmount shouldBe PLN.Zero
  }

  "CorporateBondMarket.StockState.zero" should "return all-zero stock" in {
    val z = CorporateBondMarket.StockState.zero
    z.outstanding shouldBe PLN.Zero
    z.bankHoldings shouldBe PLN.Zero
    z.ppkHoldings shouldBe PLN.Zero
    z.otherHoldings shouldBe PLN.Zero
    z.insuranceHoldings shouldBe PLN.Zero
    z.nbfiHoldings shouldBe PLN.Zero
  }

  "CorporateBondMarket.initialStock" should "have stock = CorpBondInitStock" in
    shouldBeClosePln(initStock.outstanding, p.corpBond.initStock, onePln)

  it should "allocate holders summing to 100%" in
    shouldBeClosePln(initStock.holderTotal, initStock.outstanding, onePln)

  it should "allocate insurance and NBFI holder buckets explicitly" in {
    initStock.insuranceHoldings should be > PLN.Zero
    initStock.nbfiHoldings should be > PLN.Zero
  }

  it should "have bank holdings = stock * CorpBondBankShare" in
    shouldBeClosePln(initStock.bankHoldings, initStock.outstanding * p.corpBond.bankShare, onePln)

  it should "have ppk holdings = stock * CorpBondPpkShare" in
    shouldBeClosePln(initStock.ppkHoldings, initStock.outstanding * p.corpBond.ppkShare, onePln)

  "computeYield" should "equal govYield + spread when nplRatio = 0" in {
    val y = CorporateBondMarket.computeYield(rateBps(600), Share.Zero)
    shouldBeCloseRate(y, rateBps(600) + p.corpBond.spread, Rate.fromRaw(10))
  }

  it should "widen with NPL ratio" in {
    val y0 = CorporateBondMarket.computeYield(Rate("0.06"), Share.Zero)
    val y1 = CorporateBondMarket.computeYield(Rate("0.06"), Share("0.05"))
    y1 should be > y0
  }

  it should "cap spread at 10%" in {
    val y = CorporateBondMarket.computeYield(rateBps(600), Share.One)
    y should be <= rateBps(1600)
  }

  "computeCoupon" should "be proportional to holdings" in {
    val coupon = CorporateBondMarket.computeCoupon(initState, initStock)
    coupon.total should be > PLN.Zero
    coupon.bank shouldBe (initStock.bankHoldings * initState.corpBondYield.monthly)
    coupon.ppk shouldBe (initStock.ppkHoldings * initState.corpBondYield.monthly)
    coupon.insurance shouldBe (initStock.insuranceHoldings * initState.corpBondYield.monthly)
    coupon.nbfi shouldBe (initStock.nbfiHoldings * initState.corpBondYield.monthly)
    coupon.total shouldBe coupon.holderTotal
  }

  it should "use holder-resolved coupon total when fixed-point rounding differs from outstanding coupon" in {
    val stock = CorporateBondMarket.StockState(
      outstanding = PLN.fromRaw(3L),
      bankHoldings = PLN.fromRaw(1L),
      ppkHoldings = PLN.fromRaw(1L),
      otherHoldings = PLN.fromRaw(1L),
      insuranceHoldings = PLN.Zero,
      nbfiHoldings = PLN.Zero,
    )
    val state = CorporateBondMarket.zero.copy(corpBondYield = Rate.fromRaw(60000L))

    val coupon = CorporateBondMarket.computeCoupon(state, stock)

    coupon.total shouldBe coupon.holderTotal
    coupon.total should not be (stock.outstanding * state.corpBondYield.monthly)
  }

  it should "return zeros for zero outstanding" in {
    val coupon = CorporateBondMarket.computeCoupon(CorporateBondMarket.zero, CorporateBondMarket.StockState.zero)
    coupon.total shouldBe PLN.Zero
    coupon.bank shouldBe PLN.Zero
    coupon.ppk shouldBe PLN.Zero
    coupon.other shouldBe PLN.Zero
    coupon.insurance shouldBe PLN.Zero
    coupon.nbfi shouldBe PLN.Zero
  }

  "amortization" should "equal outstanding / maturity" in {
    val a = CorporateBondMarket.amortization(initStock)
    a shouldBe (initStock.outstanding / p.corpBond.maturity)
  }

  "processDefaults" should "return zeros when no defaults" in {
    val r = CorporateBondMarket.processDefaults(initStock, PLN.Zero)
    r.grossDefault shouldBe PLN.Zero
    r.lossAfterRecovery shouldBe PLN.Zero
    r.bankLoss shouldBe PLN.Zero
    r.ppkLoss shouldBe PLN.Zero
    r.otherLoss shouldBe PLN.Zero
    r.insuranceLoss shouldBe PLN.Zero
    r.nbfiLoss shouldBe PLN.Zero
  }

  it should "allocate defaults proportionally to holdings" in {
    val defaultAmt = pln(1_000_000)
    val r          = CorporateBondMarket.processDefaults(initStock, defaultAmt)
    r.grossDefault shouldBe defaultAmt
    shouldBeClosePln(r.lossAfterRecovery, defaultAmt * (Share.One - p.corpBond.recovery), pln(5_000))
    val bankFrac   = initStock.bankHoldings.ratioTo(initStock.outstanding).toShare
    shouldBeClosePln(r.bankLoss, defaultAmt * bankFrac * (Share.One - p.corpBond.recovery), pln(5_000))
    val nbfiFrac   = initStock.nbfiHoldings.ratioTo(initStock.outstanding).toShare
    shouldBeClosePln(r.nbfiLoss, defaultAmt * nbfiFrac * (Share.One - p.corpBond.recovery), pln(5_000))
    shouldBeClosePln(r.holderLossTotal, r.lossAfterRecovery, pln(5_000))
  }

  "processIssuance" should "increase outstanding and fixed bank/PPK buckets" in {
    val issuance = fiveKPln
    val result   = CorporateBondMarket.processIssuance(initStock, issuance)
    result.outstanding shouldBe initStock.outstanding + issuance
    shouldBeClosePln(result.bankHoldings, initStock.bankHoldings + issuance * p.corpBond.bankShare, onePln)
    shouldBeClosePln(result.ppkHoldings, initStock.ppkHoldings + issuance * p.corpBond.ppkShare, onePln)
  }

  it should "allocate issuance with exact holder sums under residual rounding" in {
    val issuance = pln(2)
    val result   = CorporateBondMarket.processIssuance(initStock, issuance)
    val deltaSum =
      (result.bankHoldings - initStock.bankHoldings) +
        (result.ppkHoldings - initStock.ppkHoldings) +
        (result.otherHoldings - initStock.otherHoldings) +
        (result.insuranceHoldings - initStock.insuranceHoldings) +
        (result.nbfiHoldings - initStock.nbfiHoldings)
    deltaSum shouldBe issuance
  }

  it should "not change state for zero issuance" in {
    val result = CorporateBondMarket.processIssuance(initStock, PLN.Zero)
    result.outstanding shouldBe initStock.outstanding
  }

  "step" should "reduce outstanding by amortization + defaults" in {
    val prevOutstanding = initStock.outstanding
    val result          = CorporateBondMarket.step(
      CorporateBondMarket.StepInput(initState, initStock, rateBps(600), Share.Zero, PLN.Zero, PLN.Zero),
    )
    result.stock.outstanding shouldBe (prevOutstanding - (prevOutstanding / p.corpBond.maturity))
  }

  it should "preserve exact holder reduction sum under residual rounding" in {
    val prevStock       = initStock.copy(
      outstanding = pln(3000),
      bankHoldings = pln(1000),
      ppkHoldings = pln(1000),
      otherHoldings = pln(500),
      insuranceHoldings = pln(300),
      nbfiHoldings = pln(200),
    )
    val result          = CorporateBondMarket.step(
      CorporateBondMarket.StepInput(initState, prevStock, rateBps(600), Share.Zero, PLN.Zero, PLN.Zero),
    )
    val holderReduction =
      (prevStock.bankHoldings - result.stock.bankHoldings) +
        (prevStock.ppkHoldings - result.stock.ppkHoldings) +
        (prevStock.otherHoldings - result.stock.otherHoldings) +
        (prevStock.insuranceHoldings - result.stock.insuranceHoldings) +
        (prevStock.nbfiHoldings - result.stock.nbfiHoldings)
    holderReduction shouldBe (prevStock.outstanding - result.stock.outstanding)
  }

  it should "satisfy SFC Identity 12: delta = issuance - amort - default" in {
    val issuance = pln(2000)
    val default  = pln(500)
    val result   = CorporateBondMarket.step(CorporateBondMarket.StepInput(initState, initStock, rateBps(600), shareBps(200), default, issuance))
    val amort    = initStock.outstanding / p.corpBond.maturity
    result.stock.outstanding - initStock.outstanding shouldBe (issuance - amort - default)
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
      totalLoans = PLN("1000.0"),
      nplAmount = PLN("0.0"),
      capital = PLN("200.0"),
      deposits = PLN("5000.0"),
      afsBonds = PLN.Zero,
      htmBonds = PLN.Zero,
      consumerLoans = PLN.Zero,
      consumerNpl = PLN.Zero,
      corpBondHoldings = PLN("400.0"),
    )
    // RWA = 1000 + 400 * 0.5 = 1200; CAR = 200 / 1200 = 0.1667
    shouldBeCloseMultiplier(bank.car, Multiplier(BigDecimal("200.0") / BigDecimal("1200.0")), Multiplier("0.001"))
  }
