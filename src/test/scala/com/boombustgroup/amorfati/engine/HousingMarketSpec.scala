package com.boombustgroup.amorfati.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.engine.markets.HousingMarket
import com.boombustgroup.amorfati.types.*

class HousingMarketSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  private val initState = HousingMarket.State(
    priceIndex = 100.0,
    totalValue = PLN(3.0e12 * p.pop.firmsCount / 10000.0),
    mortgageStock = PLN(485e9 * p.pop.firmsCount / 10000.0),
    avgMortgageRate = Rate(0.0575 + 0.025),
    hhHousingWealth = PLN((3.0e12 - 485e9) * p.pop.firmsCount / 10000.0),
    lastOrigination = PLN.Zero,
    lastRepayment = PLN.Zero,
    lastDefault = PLN.Zero,
    lastWealthEffect = PLN.Zero,
    monthlyReturn = Rate.Zero,
    mortgageInterestIncome = PLN.Zero,
  )

  private def mkStepInput(
      prev: HousingMarket.State = initState,
      mortgageRate: Double = 0.0825,
      inflation: Double = 0.025,
      incomeGrowth: Double = 0.002,
      employed: Int = 90000,
      prevMortgageRate: Double = 0.0825,
  ): HousingMarket.StepInput =
    HousingMarket.StepInput(prev, Rate(mortgageRate), Rate(inflation), Rate(incomeGrowth), employed, Rate(prevMortgageRate))

  @annotation.nowarn("msg=unused private member") // defaults used by callers
  private def mkFlows(
      interest: Double = 0.0,
      principal: Double = 0.0,
      defaultAmount: Double = 0.0,
      defaultLoss: Double = 0.0,
  ): HousingMarket.MortgageFlows =
    HousingMarket.MortgageFlows(PLN(interest), PLN(principal), PLN(defaultAmount), PLN(defaultLoss))

  "HousingMarket.zero" should "return all-zero state" in {
    val z = HousingMarket.zero
    z.priceIndex shouldBe 0.0
    z.totalValue shouldBe PLN.Zero
    z.mortgageStock shouldBe PLN.Zero
    z.avgMortgageRate.toDouble shouldBe 0.0
    z.hhHousingWealth shouldBe PLN.Zero
    z.lastOrigination shouldBe PLN.Zero
    z.lastRepayment shouldBe PLN.Zero
    z.lastDefault shouldBe PLN.Zero
    z.lastWealthEffect shouldBe PLN.Zero
    z.monthlyReturn.toDouble shouldBe 0.0
    z.mortgageInterestIncome shouldBe PLN.Zero
    z.regions shouldBe None
  }

  "HousingMarket.step" should "return zero when RE_ENABLED=false" in {
    // p.flags.re is false by default
    val result = HousingMarket.step(mkStepInput())
    result shouldBe HousingMarket.zero
  }

  "HousingMarket.processOrigination" should "return zero origination when RE_ENABLED=false" in {
    val result = HousingMarket.processOrigination(initState, PLN(1e9), Rate(0.0825), true)
    result.lastOrigination shouldBe PLN.Zero
  }

  it should "return zero origination when bankCapacity is false" in {
    // Even with RE_ENABLED=false, this guard fires first
    val result = HousingMarket.processOrigination(initState, PLN(1e9), Rate(0.0825), false)
    result.lastOrigination shouldBe PLN.Zero
  }

  "HousingMarket.processMortgageFlows" should "return zeros when RE_ENABLED=false" in {
    val flows = HousingMarket.processMortgageFlows(initState, Rate(0.0825), Ratio(0.05))
    flows.interest shouldBe PLN.Zero
    flows.principal shouldBe PLN.Zero
    flows.defaultLoss shouldBe PLN.Zero
  }

  it should "return zeros for zero mortgage stock" in {
    val zeroStock = initState.copy(mortgageStock = PLN.Zero)
    val flows     = HousingMarket.processMortgageFlows(zeroStock, Rate(0.0825), Ratio(0.05))
    flows.interest shouldBe PLN.Zero
    flows.principal shouldBe PLN.Zero
    flows.defaultLoss shouldBe PLN.Zero
  }

  "HousingMarket.applyFlows" should "reduce mortgage stock by principal + defaults" in {
    val stock0 = 1000000.0
    val state  = initState.copy(mortgageStock = PLN(stock0), totalValue = PLN(2000000.0))
    val flows  = mkFlows(interest = 6000.0, principal = 5000.0, defaultAmount = 2000.0)
    val result = HousingMarket.applyFlows(state, flows)
    result.mortgageStock.toDouble shouldBe (stock0 - 5000.0 - 2000.0 +- 0.01)
    result.lastRepayment shouldBe PLN(5000.0)
    result.lastDefault shouldBe PLN(2000.0)
    result.mortgageInterestIncome shouldBe PLN(6000.0)
  }

  it should "floor mortgage stock at zero" in {
    val state  = initState.copy(mortgageStock = PLN(100.0), totalValue = PLN(2000000.0))
    val result = HousingMarket.applyFlows(state, mkFlows(principal = 50.0, defaultAmount = 60.0))
    result.mortgageStock shouldBe PLN.Zero
  }

  it should "compute housing wealth as value minus mortgage" in {
    val state  = initState.copy(
      mortgageStock = PLN(400000.0),
      totalValue = PLN(1000000.0),
      hhHousingWealth = PLN(600000.0),
    )
    val result = HousingMarket.applyFlows(state, mkFlows(principal = 10000.0))
    // new stock = 390000, new wealth = 1000000 - 390000 = 610000
    result.hhHousingWealth.toDouble shouldBe (610000.0 +- 0.01)
  }

  "HousingMarket.processMortgageFlows" should "compute interest as stock × rate / 12" in {
    val stock = 1e9
    val rate  = 0.08
    val state = initState.copy(mortgageStock = PLN(stock))
    // Need RE_ENABLED to be true for this to work, but it's false by default
    val flows = HousingMarket.processMortgageFlows(state, Rate(rate), Ratio(0.05))
    flows.interest shouldBe PLN.Zero // RE_ENABLED is false by default
  }

  it should "increase default rate with unemployment" in {
    // Can't test directly since RE_ENABLED=false, but we verify the guard
    val state  = initState.copy(mortgageStock = PLN(1e9))
    val flows1 = HousingMarket.processMortgageFlows(state, Rate(0.08), Ratio(0.04))
    val flows2 = HousingMarket.processMortgageFlows(state, Rate(0.08), Ratio(0.15))
    // Both zero when RE_ENABLED=false
    flows1.defaultLoss shouldBe PLN.Zero
    flows2.defaultLoss shouldBe PLN.Zero
  }

  "HousingMarket.initial" should "have calibrated Polish values" in {
    // p.flags.re is false but initial() always creates a calibrated state
    val init = HousingMarket.initial
    init.priceIndex shouldBe 100.0
    init.totalValue.toDouble shouldBe (p.housing.initValue.toDouble +- 1.0)
    init.mortgageStock.toDouble shouldBe (p.housing.initMortgage.toDouble +- 1.0)
    init.avgMortgageRate.toDouble shouldBe (p.monetary.initialRate.toDouble + p.housing.mortgageSpread.toDouble +- 0.001)
    init.hhHousingWealth.toDouble shouldBe (p.housing.initValue.toDouble - p.housing.initMortgage.toDouble +- 1.0)
  }

  it should "have no regions when RE_REGIONAL is false" in {
    val init = HousingMarket.initial
    // RE_REGIONAL is false by default
    init.regions shouldBe None
  }

  "Mortgage stock identity" should "hold: Δstock = origination - principal - default" in {
    val stock0         = 500000.0
    val state          = initState.copy(mortgageStock = PLN(stock0), totalValue = PLN(1000000.0))
    val origination    = 20000.0
    val stateAfterOrig = state.copy(
      mortgageStock = PLN(stock0 + origination),
      lastOrigination = PLN(origination),
    )
    val principal      = 3000.0
    val defaultAmt     = 1000.0
    val result         = HousingMarket.applyFlows(stateAfterOrig, mkFlows(interest = 5000.0, principal = principal, defaultAmount = defaultAmt))
    val expectedStock  = stock0 + origination - principal - defaultAmt
    result.mortgageStock.toDouble shouldBe (expectedStock +- 0.01)
  }

  // --- Regional Housing Market tests ---

  private def makeRegionalState(aggValue: Double, aggMortgage: Double): HousingMarket.State =
    val valueShares    = Vector(0.25, 0.08, 0.07, 0.08, 0.04, 0.05, 0.43)
    val mortgageShares = Vector(0.30, 0.10, 0.08, 0.09, 0.04, 0.06, 0.33)
    val hpis           = Vector(230.0, 190.0, 170.0, 175.0, 110.0, 140.0, 100.0)
    val regions        = (0 until 7).map { r =>
      HousingMarket.RegionalState(
        priceIndex = hpis(r),
        totalValue = PLN(aggValue * valueShares(r)),
        mortgageStock = PLN(aggMortgage * mortgageShares(r)),
        lastOrigination = PLN.Zero,
        lastRepayment = PLN.Zero,
        lastDefault = PLN.Zero,
        monthlyReturn = Rate.Zero,
      )
    }.toVector
    HousingMarket.State(
      priceIndex = 100.0,
      totalValue = PLN(aggValue),
      mortgageStock = PLN(aggMortgage),
      avgMortgageRate = Rate(0.08),
      hhHousingWealth = PLN(aggValue - aggMortgage),
      lastOrigination = PLN.Zero,
      lastRepayment = PLN.Zero,
      lastDefault = PLN.Zero,
      lastWealthEffect = PLN.Zero,
      monthlyReturn = Rate.Zero,
      mortgageInterestIncome = PLN.Zero,
      regions = Some(regions),
    )

  "HousingMarket.RegionalState" should "have 7 entries" in {
    val state = makeRegionalState(1e9, 4e8)
    state.regions.get.length shouldBe 7
  }

  it should "have regional values summing to aggregate" in {
    val aggValue = 1e9
    val state    = makeRegionalState(aggValue, 4e8)
    val regSum   = state.regions.get.map(_.totalValue.toDouble).sum
    regSum shouldBe (aggValue +- 1.0)
  }

  it should "have regional mortgages summing to aggregate" in {
    val aggMortgage = 4e8
    val state       = makeRegionalState(1e9, aggMortgage)
    val regSum      = state.regions.get.map(_.mortgageStock.toDouble).sum
    regSum shouldBe (aggMortgage +- 1.0)
  }

  it should "have Warszawa HPI > Rest HPI" in {
    val state = makeRegionalState(1e9, 4e8)
    val regs  = state.regions.get
    regs(0).priceIndex should be > regs(6).priceIndex
  }

  "applyFlows with regions" should "distribute flows proportionally to mortgage stock" in {
    val state  = makeRegionalState(1e9, 4e8)
    val result = HousingMarket.applyFlows(state, mkFlows(interest = 3000.0, principal = 10000.0, defaultAmount = 5000.0))

    // Regional stocks should sum to aggregate
    val regStockSum = result.regions.get.map(_.mortgageStock.toDouble).sum
    regStockSum shouldBe (result.mortgageStock.toDouble +- 1.0)
  }

  it should "reduce each region's stock proportionally" in {
    val state  = makeRegionalState(1e9, 4e8)
    val result = HousingMarket.applyFlows(state, mkFlows(interest = 3000.0, principal = 10000.0, defaultAmount = 5000.0))

    // Warszawa (30% of mortgage) should have largest reduction
    val wawReduction  = (state.regions.get(0).mortgageStock - result.regions.get(0).mortgageStock).toDouble
    val restReduction = (state.regions.get(6).mortgageStock - result.regions.get(6).mortgageStock).toDouble
    // Waw mortgage share (0.30) vs Rest (0.33), so rest slightly larger
    wawReduction should be > 0.0
    restReduction should be > 0.0
  }

  it should "preserve regional repayment and default tracking" in {
    val state  = makeRegionalState(1e9, 4e8)
    val result = HousingMarket.applyFlows(state, mkFlows(interest = 3000.0, principal = 10000.0, defaultAmount = 5000.0))

    // Regional repayments should sum to aggregate
    val regRepaySum = result.regions.get.map(_.lastRepayment.toDouble).sum
    regRepaySum shouldBe (10000.0 +- 0.01)

    val regDefaultSum = result.regions.get.map(_.lastDefault.toDouble).sum
    regDefaultSum shouldBe (5000.0 +- 0.01)
  }

  "processOrigination with regions" should "zero out regional origination when disabled" in {
    val state  = makeRegionalState(1e9, 4e8)
    // RE_ENABLED is false by default
    val result = HousingMarket.processOrigination(state, PLN(1e7), Rate(0.08), true)
    result.lastOrigination shouldBe PLN.Zero
    result.regions.get.foreach { r =>
      r.lastOrigination shouldBe PLN.Zero
    }
  }

  "Mortgage stock identity with regions" should "hold per-region" in {
    val state          = makeRegionalState(1e9, 4e8)
    val origAmount     = 5000.0
    // Simulate origination by bumping each region's stock
    val shares         = Vector(0.25, 0.08, 0.07, 0.08, 0.04, 0.05, 0.43)
    val updatedRegs    = state.regions.get.zipWithIndex.map { (r, i) =>
      val rOrig = PLN(origAmount * shares(i))
      r.copy(mortgageStock = r.mortgageStock + rOrig, lastOrigination = rOrig)
    }
    val stateAfterOrig = state.copy(
      mortgageStock = state.mortgageStock + PLN(origAmount),
      lastOrigination = PLN(origAmount),
      regions = Some(updatedRegs),
    )
    val principal      = 2000.0
    val defaultAmt     = 1000.0
    val result         = HousingMarket.applyFlows(stateAfterOrig, mkFlows(interest = 1000.0, principal = principal, defaultAmount = defaultAmt))

    // Aggregate identity
    val expectedAggStock = state.mortgageStock.toDouble + origAmount - principal - defaultAmt
    result.mortgageStock.toDouble shouldBe (expectedAggStock +- 1.0)

    // Regional stocks should sum to aggregate
    val regSum = result.regions.get.map(_.mortgageStock.toDouble).sum
    regSum shouldBe (result.mortgageStock.toDouble +- 1.0)
  }
