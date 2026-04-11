package com.boombustgroup.amorfati.engine

import org.scalatest.flatspec.AnyFlatSpec
import com.boombustgroup.amorfati.Generators
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.markets.{FiscalBudget, OpenEconomy}
import com.boombustgroup.amorfati.types.*

class DiasporaRemittanceSpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]
  private val td           = ComputationBoundary
  private val baseEr       = td.toDouble(p.forex.baseExRate)

  // ==========================================================================
  // Config defaults
  // ==========================================================================

  "RemittancePerCapita" should "default to 40.0" in {
    td.toDouble(p.remittance.perCapita) shouldBe 40.0
  }

  "RemittanceErElasticity" should "default to 0.5" in {
    td.toDouble(p.remittance.erElasticity) shouldBe 0.5
  }

  "RemittanceGrowthRate" should "default to 0.02" in {
    td.toDouble(p.remittance.growthRate) shouldBe 0.02
  }

  "RemittanceCyclicalSens" should "default to 0.3" in {
    td.toDouble(p.remittance.cyclicalSens) shouldBe 0.3
  }

  // ==========================================================================
  // Per-capita base
  // ==========================================================================

  "Per-capita base" should "be positive for positive WAP" in {
    val wap  = 1000
    val base = td.toDouble(p.remittance.perCapita) * wap.toDouble
    base should be > 0.0
  }

  // ==========================================================================
  // ER adjustment
  // ==========================================================================

  "ER adjustment" should "increase inflow when PLN weakens" in {
    val weakerER = baseEr * 1.2 // PLN weaker → higher exchange rate number
    val erAdj    = Math.pow(weakerER / baseEr, td.toDouble(p.remittance.erElasticity))
    erAdj should be > 1.0
  }

  it should "decrease inflow when PLN strengthens" in {
    val strongerER = baseEr * 0.8
    val erAdj      = Math.pow(strongerER / baseEr, td.toDouble(p.remittance.erElasticity))
    erAdj should be < 1.0
  }

  it should "apply partial pass-through (exponent = 0.5)" in {
    // 20% depreciation → sqrt(1.2) ≈ 1.095 (not full 1.2)
    val weakerER = baseEr * 1.2
    val erAdj    = Math.pow(weakerER / baseEr, 0.5)
    erAdj should be > 1.0
    erAdj should be < 1.2
    erAdj shouldBe Math.sqrt(1.2) +- 1e-10
  }

  // ==========================================================================
  // Trend adjustment
  // ==========================================================================

  "Trend adjustment" should "equal 1.0 at month 0" in {
    val trendAdj = Math.pow(1.0 + td.toDouble(p.remittance.growthRate) / 12.0, 0.0)
    trendAdj shouldBe 1.0
  }

  it should "grow over time" in {
    val trend12 = Math.pow(1.0 + td.toDouble(p.remittance.growthRate) / 12.0, 12.0)
    trend12 should be > 1.0
    // ~2% annual growth
    trend12 shouldBe (1.0 + td.toDouble(p.remittance.growthRate)) +- 0.001
  }

  // ==========================================================================
  // Cyclical adjustment
  // ==========================================================================

  "Cyclical adjustment" should "increase with unemployment above 5%" in {
    val highUnemp = 0.10
    val adj       = 1.0 + td.toDouble(p.remittance.cyclicalSens) * Math.max(0.0, highUnemp - 0.05)
    adj should be > 1.0
  }

  it should "be neutral at unemployment = 5%" in {
    val adj = 1.0 + td.toDouble(p.remittance.cyclicalSens) * Math.max(0.0, 0.05 - 0.05)
    adj shouldBe 1.0
  }

  it should "be neutral at unemployment < 5%" in {
    val lowUnemp = 0.03
    val adj      = 1.0 + td.toDouble(p.remittance.cyclicalSens) * Math.max(0.0, lowUnemp - 0.05)
    adj shouldBe 1.0
  }

  // ==========================================================================
  // Full formula
  // ==========================================================================

  "Full formula" should "combine all components correctly" in {
    val wap   = 1000
    val month = 12
    val unemp = 0.08
    val er    = baseEr * 1.1

    val base        = td.toDouble(p.remittance.perCapita) * wap.toDouble
    val erAdj       = Math.pow(er / baseEr, td.toDouble(p.remittance.erElasticity))
    val trendAdj    = Math.pow(1.0 + td.toDouble(p.remittance.growthRate) / 12.0, month.toDouble)
    val cyclicalAdj = 1.0 + td.toDouble(p.remittance.cyclicalSens) * Math.max(0.0, unemp - 0.05)
    val result      = base * erAdj * trendAdj * cyclicalAdj

    result should be > 0.0
    // base = 40 * 1000 = 40000
    base shouldBe 40000.0
    // erAdj > 1 (weaker PLN)
    erAdj should be > 1.0
    // trendAdj > 1 (12 months)
    trendAdj should be > 1.0
    // cyclicalAdj > 1 (8% > 5%)
    cyclicalAdj should be > 1.0
    // Result should be greater than base
    result should be > base
  }

  // ==========================================================================
  // OpenEconomy secondaryIncome
  // ==========================================================================

  "secondaryIncome" should "include diasporaInflow as credit" in {
    val prevBop   = OpenEconomy.BopState.zero
    val prevForex = OpenEconomy.ForexState(p.forex.baseExRate, PLN.Zero, p.openEcon.exportBase, PLN.Zero, PLN.Zero)

    val base          = OpenEconomy.StepInput(
      prevBop = prevBop,
      prevForex = prevForex,
      importCons = PLN.Zero,
      techImports = PLN.Zero,
      autoRatio = Share.Zero,
      domesticRate = Rate(0.05),
      gdp = PLN(1e9),
      priceLevel = PriceIndex.Base,
      sectorOutputs = Vector.fill(6)(PLN(1e8)),
      month = ExecutionMonth(1),
      nbpFxReserves = prevBop.reserves,
    )
    val resultWith    = OpenEconomy.step(base.copy(diasporaInflow = PLN(1000.0)))
    val resultWithout = OpenEconomy.step(base.copy(diasporaInflow = PLN.Zero))

    resultWith.bop.secondaryIncome.shouldBe(resultWithout.bop.secondaryIncome + PLN(1000.0))
  }

  it should "net outflow and inflow" in {
    val prevBop   = OpenEconomy.BopState.zero
    val prevForex = OpenEconomy.ForexState(p.forex.baseExRate, PLN.Zero, p.openEcon.exportBase, PLN.Zero, PLN.Zero)

    val base   = OpenEconomy.StepInput(
      prevBop = prevBop,
      prevForex = prevForex,
      importCons = PLN.Zero,
      techImports = PLN.Zero,
      autoRatio = Share.Zero,
      domesticRate = Rate(0.05),
      gdp = PLN(1e9),
      priceLevel = PriceIndex.Base,
      sectorOutputs = Vector.fill(6)(PLN(1e8)),
      month = ExecutionMonth(1),
      nbpFxReserves = prevBop.reserves,
    )
    val result = OpenEconomy.step(base.copy(remittanceOutflow = PLN(500.0), diasporaInflow = PLN(800.0)))

    // secondaryIncome = euFunds(0) - outflow(500) + inflow(800) = 300
    result.bop.secondaryIncome.shouldBe(PLN(300.0))
  }

  // ==========================================================================
  // Net remittances
  // ==========================================================================

  "Net remittances" should "be inflow minus outflow" in {
    val inflow  = 1000.0
    val outflow = 400.0
    (inflow - outflow).shouldBe(600.0)
  }

  // ==========================================================================
  // World defaults
  // ==========================================================================

  "World" should "default diasporaRemittanceInflow to 0.0" in {
    val w = World(
      inflation = Rate(0.02),
      priceLevel = 1.0,
      gdpProxy = 1e9,
      currentSigmas = Vector.fill(6)(Sigma(0.1)),
      totalPopulation = 100,
      gov = FiscalBudget.GovState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      nbp = com.boombustgroup.amorfati.agents.Nbp.State(Rate(0.05), PLN.Zero, false, PLN.Zero, PLN.Zero, PLN.Zero),
      bankingSector = Generators.testBankingSector().marketState,
      forex = OpenEconomy.ForexState(p.forex.baseExRate, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      hhAgg = com.boombustgroup.amorfati.agents.Household.Aggregates(
        employed = 100,
        unemployed = 0,
        retraining = 0,
        bankrupt = 0,
        totalIncome = PLN.Zero,
        consumption = PLN.Zero,
        domesticConsumption = PLN.Zero,
        importConsumption = PLN.Zero,
        marketWage = PLN(5000),
        reservationWage = PLN(4000),
        giniIndividual = Share.Zero,
        giniWealth = Share.Zero,
        meanSavings = PLN.Zero,
        medianSavings = PLN.Zero,
        povertyRate50 = Share.Zero,
        bankruptcyRate = Share.Zero,
        meanSkill = Share.Zero,
        meanHealthPenalty = Share.Zero,
        retrainingAttempts = 0,
        retrainingSuccesses = 0,
        consumptionP10 = PLN.Zero,
        consumptionP50 = PLN.Zero,
        consumptionP90 = PLN.Zero,
        meanMonthsToRuin = Scalar.Zero,
        povertyRate30 = Share.Zero,
        totalRent = PLN.Zero,
        totalDebtService = PLN.Zero,
        totalUnempBenefits = PLN.Zero,
        totalDepositInterest = PLN.Zero,
        crossSectorHires = 0,
        voluntaryQuits = 0,
        sectorMobilityRate = Share.Zero,
        totalRemittances = PLN.Zero,
        totalPit = PLN.Zero,
        totalSocialTransfers = PLN.Zero,
        totalConsumerDebtService = PLN.Zero,
        totalConsumerOrigination = PLN.Zero,
        totalConsumerDefault = PLN.Zero,
        totalConsumerPrincipal = PLN.Zero,
      ),
      social = SocialState.zero,
      financial = FinancialMarketsState.zero,
      external = ExternalState.zero,
      real = RealState.zero,
      mechanisms = MechanismsState.zero,
      plumbing = MonetaryPlumbingState.zero,
      flows = FlowState.zero,
    )
    w.flows.diasporaRemittanceInflow.shouldBe(PLN.Zero)
  }
