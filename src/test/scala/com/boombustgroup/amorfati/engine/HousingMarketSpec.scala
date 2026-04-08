package com.boombustgroup.amorfati.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.engine.markets.HousingMarket
import com.boombustgroup.amorfati.types.*

class HousingMarketSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]
  private val onePln       = PLN.fromLong(1)

  private def pln(units: Long): PLN = PLN.fromLong(units)
  private def rateBps(bps: Long): Rate = Rate.fromRaw(bps)
  private def priceIndex(points: Long): PriceIndex = PriceIndex.fromRaw(points * 10000L)
  private def shouldBeClosePln(actual: PLN, expected: PLN, tolerance: PLN): Unit =
    (actual - expected).abs should be <= tolerance
  private def shouldBeCloseRate(actual: Rate, expected: Rate, tolerance: Rate): Unit =
    (actual - expected).abs should be <= tolerance

  private val initState = HousingMarket.State(
    priceIndex = priceIndex(100),
    totalValue = p.housing.initValue,
    mortgageStock = p.housing.initMortgage,
    avgMortgageRate = p.monetary.initialRate + p.housing.mortgageSpread,
    hhHousingWealth = p.housing.initValue - p.housing.initMortgage,
    lastOrigination = PLN.Zero,
    lastRepayment = PLN.Zero,
    lastDefault = PLN.Zero,
    lastWealthEffect = PLN.Zero,
    monthlyReturn = Rate.Zero,
    mortgageInterestIncome = PLN.Zero,
  )

  @annotation.nowarn("msg=unused private member") // defaults used by callers
  private def mkFlows(
      interest: PLN = PLN.Zero,
      principal: PLN = PLN.Zero,
      defaultAmount: PLN = PLN.Zero,
      defaultLoss: PLN = PLN.Zero,
  ): HousingMarket.MortgageFlows =
    HousingMarket.MortgageFlows(interest, principal, defaultAmount, defaultLoss)

  "HousingMarket.zero" should "return all-zero state" in {
    val z = HousingMarket.zero
    z.priceIndex shouldBe PriceIndex.Zero
    z.totalValue shouldBe PLN.Zero
    z.mortgageStock shouldBe PLN.Zero
    z.avgMortgageRate shouldBe Rate.Zero
    z.hhHousingWealth shouldBe PLN.Zero
    z.lastOrigination shouldBe PLN.Zero
    z.lastRepayment shouldBe PLN.Zero
    z.lastDefault shouldBe PLN.Zero
    z.lastWealthEffect shouldBe PLN.Zero
    z.monthlyReturn shouldBe Rate.Zero
    z.mortgageInterestIncome shouldBe PLN.Zero
    z.regions shouldBe None
  }

  "HousingMarket.processMortgageFlows" should "return zeros for zero mortgage stock" in {
    val zeroStock = initState.copy(mortgageStock = PLN.Zero)
    val flows     = HousingMarket.processMortgageFlows(zeroStock, rateBps(825), Share.fraction(1, 20))
    flows.interest shouldBe PLN.Zero
    flows.principal shouldBe PLN.Zero
    flows.defaultLoss shouldBe PLN.Zero
  }

  "HousingMarket.applyFlows" should "reduce mortgage stock by principal + defaults" in {
    val stock0 = pln(1_000_000)
    val state  = initState.copy(mortgageStock = stock0, totalValue = pln(2_000_000))
    val flows  = mkFlows(interest = pln(6_000), principal = pln(5_000), defaultAmount = pln(2_000))
    val result = HousingMarket.applyFlows(state, flows)
    result.mortgageStock shouldBe stock0 - pln(5_000) - pln(2_000)
    result.lastRepayment shouldBe pln(5_000)
    result.lastDefault shouldBe pln(2_000)
    result.mortgageInterestIncome shouldBe pln(6_000)
  }

  it should "floor mortgage stock at zero" in {
    val state  = initState.copy(mortgageStock = pln(100), totalValue = pln(2_000_000))
    val result = HousingMarket.applyFlows(state, mkFlows(principal = pln(50), defaultAmount = pln(60)))
    result.mortgageStock shouldBe PLN.Zero
  }

  it should "compute housing wealth as value minus mortgage" in {
    val state  = initState.copy(
      mortgageStock = pln(400_000),
      totalValue = pln(1_000_000),
      hhHousingWealth = pln(600_000),
    )
    val result = HousingMarket.applyFlows(state, mkFlows(principal = pln(10_000)))
    result.hhHousingWealth shouldBe pln(610_000)
  }

  "HousingMarket.processMortgageFlows" should "compute interest as stock × rate / 12" in {
    val stock = pln(1_000_000_000)
    val rate  = rateBps(800)
    val state = initState.copy(mortgageStock = stock)
    val flows = HousingMarket.processMortgageFlows(state, rate, Share.fraction(1, 20))
    shouldBeClosePln(flows.interest, stock * rate / 12, pln(100_000))
  }

  it should "increase default rate with unemployment" in {
    val state  = initState.copy(mortgageStock = pln(1_000_000_000))
    val flows1 = HousingMarket.processMortgageFlows(state, rateBps(800), Share.fromRaw(400))
    val flows2 = HousingMarket.processMortgageFlows(state, rateBps(800), Share.fromRaw(1500))
    flows2.defaultLoss should be > flows1.defaultLoss
  }

  "HousingMarket.initial" should "have calibrated Polish values" in {
    val init = HousingMarket.initial
    init.priceIndex shouldBe priceIndex(100)
    shouldBeClosePln(init.totalValue, p.housing.initValue, onePln)
    shouldBeClosePln(init.mortgageStock, p.housing.initMortgage, onePln)
    shouldBeCloseRate(init.avgMortgageRate, p.monetary.initialRate + p.housing.mortgageSpread, Rate.fromRaw(10))
    shouldBeClosePln(init.hhHousingWealth, p.housing.initValue - p.housing.initMortgage, onePln)
  }

  it should "have regions when RE_REGIONAL is true" in {
    val init = HousingMarket.initial
    // RE_REGIONAL is true by default
    init.regions shouldBe defined
  }

  "Mortgage stock identity" should "hold: Δstock = origination - principal - default" in {
    val stock0         = pln(500_000)
    val state          = initState.copy(mortgageStock = stock0, totalValue = pln(1_000_000))
    val origination    = pln(20_000)
    val stateAfterOrig = state.copy(
      mortgageStock = stock0 + origination,
      lastOrigination = origination,
    )
    val principal      = pln(3_000)
    val defaultAmt     = pln(1_000)
    val result         = HousingMarket.applyFlows(stateAfterOrig, mkFlows(interest = pln(5_000), principal = principal, defaultAmount = defaultAmt))
    result.mortgageStock shouldBe stock0 + origination - principal - defaultAmt
  }

  // --- Regional Housing Market tests ---

  private def makeRegionalState(aggValue: PLN, aggMortgage: PLN): HousingMarket.State =
    val valueShares    = Vector(Share.fraction(25, 100), Share.fraction(8, 100), Share.fraction(7, 100), Share.fraction(8, 100), Share.fraction(4, 100), Share.fraction(5, 100), Share.fraction(43, 100))
    val mortgageShares = Vector(Share.fraction(30, 100), Share.fraction(10, 100), Share.fraction(8, 100), Share.fraction(9, 100), Share.fraction(4, 100), Share.fraction(6, 100), Share.fraction(33, 100))
    val hpis           = Vector(230L, 190L, 170L, 175L, 110L, 140L, 100L)
    val regions        = (0 until 7).map { r =>
      HousingMarket.RegionalState(
        priceIndex = priceIndex(hpis(r)),
        totalValue = valueShares(r) * aggValue,
        mortgageStock = mortgageShares(r) * aggMortgage,
        lastOrigination = PLN.Zero,
        lastRepayment = PLN.Zero,
        lastDefault = PLN.Zero,
        monthlyReturn = Rate.Zero,
      )
    }.toVector
    HousingMarket.State(
      priceIndex = priceIndex(100),
      totalValue = aggValue,
      mortgageStock = aggMortgage,
      avgMortgageRate = rateBps(800),
      hhHousingWealth = aggValue - aggMortgage,
      lastOrigination = PLN.Zero,
      lastRepayment = PLN.Zero,
      lastDefault = PLN.Zero,
      lastWealthEffect = PLN.Zero,
      monthlyReturn = Rate.Zero,
      mortgageInterestIncome = PLN.Zero,
      regions = Some(regions),
    )

  "HousingMarket.RegionalState" should "have 7 entries" in {
    val state = makeRegionalState(pln(1_000_000_000), pln(400_000_000))
    state.regions.get.length shouldBe 7
  }

  it should "have regional values summing to aggregate" in {
    val aggValue = pln(1_000_000_000)
    val state    = makeRegionalState(aggValue, pln(400_000_000))
    state.regions.get.map(_.totalValue).sum shouldBe aggValue
  }

  it should "have regional mortgages summing to aggregate" in {
    val aggMortgage = pln(400_000_000)
    val state       = makeRegionalState(pln(1_000_000_000), aggMortgage)
    state.regions.get.map(_.mortgageStock).sum shouldBe aggMortgage
  }

  it should "have Warszawa HPI > Rest HPI" in {
    val state = makeRegionalState(pln(1_000_000_000), pln(400_000_000))
    val regs  = state.regions.get
    regs(0).priceIndex should be > regs(6).priceIndex
  }

  "applyFlows with regions" should "distribute flows proportionally to mortgage stock" in {
    val state  = makeRegionalState(pln(1_000_000_000), pln(400_000_000))
    val result = HousingMarket.applyFlows(state, mkFlows(interest = pln(3_000), principal = pln(10_000), defaultAmount = pln(5_000)))
    shouldBeClosePln(result.regions.get.map(_.mortgageStock).sum, result.mortgageStock, onePln)
  }

  it should "reduce each region's stock proportionally" in {
    val state  = makeRegionalState(pln(1_000_000_000), pln(400_000_000))
    val result = HousingMarket.applyFlows(state, mkFlows(interest = pln(3_000), principal = pln(10_000), defaultAmount = pln(5_000)))

    val wawReduction  = state.regions.get(0).mortgageStock - result.regions.get(0).mortgageStock
    val restReduction = state.regions.get(6).mortgageStock - result.regions.get(6).mortgageStock
    wawReduction should be > PLN.Zero
    restReduction should be > PLN.Zero
  }

  it should "preserve regional repayment and default tracking" in {
    val state  = makeRegionalState(pln(1_000_000_000), pln(400_000_000))
    val result = HousingMarket.applyFlows(state, mkFlows(interest = pln(3_000), principal = pln(10_000), defaultAmount = pln(5_000)))
    result.regions.get.map(_.lastRepayment).sum shouldBe pln(10_000)
    result.regions.get.map(_.lastDefault).sum shouldBe pln(5_000)
  }

  it should "preserve exact regional repayment and default sums under residual rounding" in {
    val state         = makeRegionalState(pln(1_000_000_000), pln(400_000_000))
    val principal     = pln(2)
    val defaultAmount = pln(2)
    val result        = HousingMarket.applyFlows(state, HousingMarket.MortgageFlows(PLN.Zero, principal, defaultAmount, PLN.Zero))

    result.regions.get.map(_.lastRepayment).sum shouldBe principal
    result.regions.get.map(_.lastDefault).sum shouldBe defaultAmount
  }

  "Mortgage stock identity with regions" should "hold per-region" in {
    val state          = makeRegionalState(pln(1_000_000_000), pln(400_000_000))
    val origAmount     = pln(5_000)
    val shares         = Vector(Share.fraction(25, 100), Share.fraction(8, 100), Share.fraction(7, 100), Share.fraction(8, 100), Share.fraction(4, 100), Share.fraction(5, 100), Share.fraction(43, 100))
    val updatedRegs    = state.regions.get.zipWithIndex.map { (r, i) =>
      val rOrig = shares(i) * origAmount
      r.copy(mortgageStock = r.mortgageStock + rOrig, lastOrigination = rOrig)
    }
    val stateAfterOrig = state.copy(
      mortgageStock = state.mortgageStock + origAmount,
      lastOrigination = origAmount,
      regions = Some(updatedRegs),
    )
    val principal      = pln(2_000)
    val defaultAmt     = pln(1_000)
    val result         = HousingMarket.applyFlows(stateAfterOrig, mkFlows(interest = pln(1_000), principal = principal, defaultAmount = defaultAmt))
    result.mortgageStock shouldBe state.mortgageStock + origAmount - principal - defaultAmt
    shouldBeClosePln(result.regions.get.map(_.mortgageStock).sum, result.mortgageStock, onePln)
  }

  "step with regions" should "preserve exact regional origination sum under residual rounding" in {
    val state  = makeRegionalState(pln(1_000_000_000), pln(400_000_000))
    val result = HousingMarket.step(
      HousingMarket.StepInput(
        prev = state,
        mortgageRate = rateBps(800),
        inflation = Rate.Zero,
        incomeGrowth = Rate.Zero,
        employed = 1,
        prevMortgageRate = rateBps(800),
      ),
    )

    result.regions.get.map(_.lastOrigination).sum shouldBe result.lastOrigination
  }
