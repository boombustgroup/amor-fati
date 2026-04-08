package com.boombustgroup.amorfati.engine

import org.scalatest.flatspec.AnyFlatSpec
import com.boombustgroup.amorfati.Generators
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.engine.markets.{FiscalBudget, OpenEconomy}
import com.boombustgroup.amorfati.types.*

class TourismSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]
  private val td           = ComputationBoundary

  // ==========================================================================
  // Config defaults (10 tests)
  // ==========================================================================

  "TourismInboundShare" should "default to 0.05" in {
    td.toDouble(p.tourism.inboundShare) shouldBe 0.05
  }

  "TourismOutboundShare" should "default to 0.03" in {
    td.toDouble(p.tourism.outboundShare) shouldBe 0.03
  }

  "TourismErElasticity" should "default to 0.6" in {
    td.toDouble(p.tourism.erElasticity) shouldBe 0.6
  }

  "TourismSeasonality" should "default to 0.40" in {
    td.toDouble(p.tourism.seasonality) shouldBe 0.40
  }

  "TourismPeakMonth" should "default to 7" in {
    p.tourism.peakMonth shouldBe 7
  }

  "TourismGrowthRate" should "default to 0.03" in {
    td.toDouble(p.tourism.growthRate) shouldBe 0.03
  }

  "TourismShockMonth" should "default to 0" in {
    p.tourism.shockMonth shouldBe 0
  }

  "TourismShockSize" should "default to 0.80" in {
    td.toDouble(p.tourism.shockSize) shouldBe 0.80
  }

  "TourismShockRecovery" should "default to 0.03" in {
    td.toDouble(p.tourism.shockRecovery) shouldBe 0.03
  }

  // ==========================================================================
  // Seasonal factor (3 tests)
  // ==========================================================================

  "Seasonal factor" should "peak in July (month 7) above 1.0" in {
    val monthInYear = 7
    val factor      = 1.0 + td.toDouble(p.tourism.seasonality) *
      Math.cos(2 * Math.PI * (monthInYear - p.tourism.peakMonth) / 12.0)
    factor shouldBe 1.0 + td.toDouble(p.tourism.seasonality) // cos(0) = 1
    factor should be > 1.0
  }

  it should "trough in January below 1.0" in {
    val monthInYear = 1
    val factor      = 1.0 + td.toDouble(p.tourism.seasonality) *
      Math.cos(2 * Math.PI * (monthInYear - p.tourism.peakMonth) / 12.0)
    factor should be < 1.0
  }

  it should "average approximately 1.0 over 12 months" in {
    val factors = (1 to 12).map { m =>
      1.0 + td.toDouble(p.tourism.seasonality) *
        Math.cos(2 * Math.PI * (m - p.tourism.peakMonth) / 12.0)
    }
    val avg     = factors.sum / 12.0
    avg shouldBe 1.0 +- 1e-10
  }

  // ==========================================================================
  // ER adjustment (3 tests)
  // ==========================================================================

  "ER adjustment" should "increase inbound tourism when PLN weakens" in {
    val weakerER     = p.forex.baseExRate * 1.2
    val inboundErAdj = Math.pow(weakerER / p.forex.baseExRate, td.toDouble(p.tourism.erElasticity))
    inboundErAdj should be > 1.0
  }

  it should "decrease outbound tourism when PLN weakens" in {
    val weakerER      = p.forex.baseExRate * 1.2
    val outboundErAdj = Math.pow(p.forex.baseExRate / weakerER, td.toDouble(p.tourism.erElasticity))
    outboundErAdj should be < 1.0
  }

  it should "apply partial pass-through (exponent = 0.6)" in {
    val weakerER     = p.forex.baseExRate * 1.2
    val inboundErAdj = Math.pow(weakerER / p.forex.baseExRate, 0.6)
    inboundErAdj should be > 1.0
    inboundErAdj should be < 1.2 // partial, not full pass-through
  }

  // ==========================================================================
  // Trend adjustment (2 tests)
  // ==========================================================================

  "Trend adjustment" should "equal 1.0 at month 0" in {
    val trendAdj = Math.pow(1.0 + td.toDouble(p.tourism.growthRate) / 12.0, 0.0)
    trendAdj shouldBe 1.0
  }

  it should "grow over 12 months" in {
    val trend12 = Math.pow(1.0 + td.toDouble(p.tourism.growthRate) / 12.0, 12.0)
    trend12 should be > 1.0
    trend12 shouldBe (1.0 + td.toDouble(p.tourism.growthRate)) +- 0.001
  }

  // ==========================================================================
  // COVID shock (3 tests)
  // ==========================================================================

  "COVID shock" should "have no disruption when shock month is 0" in {
    val disruption =
      if 0 > 0 && 10 >= 0 then td.toDouble(p.tourism.shockSize) * Math.pow(1.0 - td.toDouble(p.tourism.shockRecovery), 10.0)
      else 0.0
    disruption shouldBe 0.0
  }

  it should "apply disruption at shock trigger month" in {
    val shockMonth  = 24
    val m           = 24
    val disruption  =
      if shockMonth > 0 && m >= shockMonth then
        td.toDouble(p.tourism.shockSize) * Math.pow(1.0 - td.toDouble(p.tourism.shockRecovery), (m - shockMonth).toDouble)
      else 0.0
    // At trigger month: disruption = 0.80 * (0.97)^0 = 0.80
    disruption shouldBe 0.80
    val shockFactor = 1.0 - disruption
    shockFactor shouldBe 0.20 +- 1e-10
  }

  it should "recover gradually after shock" in {
    val shockMonth  = 24
    val m           = 36 // 12 months after shock
    val disruption  =
      td.toDouble(p.tourism.shockSize) * Math.pow(1.0 - td.toDouble(p.tourism.shockRecovery), (m - shockMonth).toDouble)
    disruption should be < td.toDouble(p.tourism.shockSize)
    disruption should be > 0.0
    val shockFactor = 1.0 - disruption
    shockFactor should be > 0.20 // Better than at trigger
    shockFactor should be < 1.0 // Not fully recovered
  }

  // ==========================================================================
  // Full formula (1 test)
  // ==========================================================================

  "Full formula" should "combine all components multiplicatively" in {
    val baseGdp     = 1e9
    val monthInYear = 7 // peak month
    val m           = 12
    val er          = p.forex.baseExRate * 1.1

    val seasonalFactor = 1.0 + td.toDouble(p.tourism.seasonality) *
      Math.cos(2 * Math.PI * (monthInYear - p.tourism.peakMonth) / 12.0)
    val inboundErAdj   = Math.pow(er / p.forex.baseExRate, td.toDouble(p.tourism.erElasticity))
    val outboundErAdj  = Math.pow(p.forex.baseExRate / er, td.toDouble(p.tourism.erElasticity))
    val trendAdj       = Math.pow(1.0 + td.toDouble(p.tourism.growthRate) / 12.0, m.toDouble)
    val shockFactor    = 1.0 // no shock

    val inbound  = baseGdp * td.toDouble(p.tourism.inboundShare) *
      seasonalFactor * inboundErAdj * trendAdj * shockFactor
    val outbound = baseGdp * td.toDouble(p.tourism.outboundShare) *
      seasonalFactor * outboundErAdj * trendAdj * shockFactor

    inbound should be > 0.0
    outbound should be > 0.0
    // Inbound > outbound because: higher share (5% vs 3%) AND weaker PLN boosts inbound
    inbound should be > outbound
    // Seasonal peak → factor = 1.4
    seasonalFactor shouldBe 1.4 +- 1e-10
    // ER: weaker PLN → inbound ↑, outbound ↓
    inboundErAdj should be > 1.0
    outboundErAdj should be < 1.0
  }

  // ==========================================================================
  // OpenEconomy integration (2 tests)
  // ==========================================================================

  "OpenEconomy exports" should "include tourismExport" in {
    val prevBop   = OpenEconomy.BopState.zero
    val prevForex = OpenEconomy.ForexState(ExchangeRate(p.forex.baseExRate), PLN.Zero, p.openEcon.exportBase, PLN.Zero, PLN.Zero)

    val base          = OpenEconomy.StepInput(
      prevBop = prevBop,
      prevForex = prevForex,
      importCons = PLN.Zero,
      techImports = PLN.Zero,
      autoRatio = Share.Zero,
      domesticRate = Rate(0.05),
      gdp = PLN(1e9),
      priceLevel = PriceIndex.Base,
      sectorOutputs = Vector.fill(p.sectorDefs.length)(PLN(1e8)),
      month = 1,
      nbpFxReserves = prevBop.reserves,
    )
    val resultWith    = OpenEconomy.step(base.copy(tourismExport = PLN(1000.0)))
    val resultWithout = OpenEconomy.step(base.copy(tourismExport = PLN.Zero))

    resultWith.bop.exports.shouldBe(resultWithout.bop.exports + PLN(1000.0))
  }

  "OpenEconomy imports" should "include tourismImport" in {
    val prevBop   = OpenEconomy.BopState.zero
    val prevForex = OpenEconomy.ForexState(ExchangeRate(p.forex.baseExRate), PLN.Zero, p.openEcon.exportBase, PLN.Zero, PLN.Zero)

    val base          = OpenEconomy.StepInput(
      prevBop = prevBop,
      prevForex = prevForex,
      importCons = PLN.Zero,
      techImports = PLN.Zero,
      autoRatio = Share.Zero,
      domesticRate = Rate(0.05),
      gdp = PLN(1e9),
      priceLevel = PriceIndex.Base,
      sectorOutputs = Vector.fill(p.sectorDefs.length)(PLN(1e8)),
      month = 1,
      nbpFxReserves = prevBop.reserves,
    )
    val resultWith    = OpenEconomy.step(base.copy(tourismImport = PLN(500.0)))
    val resultWithout = OpenEconomy.step(base.copy(tourismImport = PLN.Zero))

    resultWith.bop.totalImports.shouldBe(resultWithout.bop.totalImports + PLN(500.0))
  }

  // ==========================================================================
  // World defaults (1 test)
  // ==========================================================================

  "World" should "default tourismExport and tourismImport to 0.0" in {
    val w = World(
      month = 0,
      inflation = Rate(0.02),
      priceLevel = PriceIndex.Base,
      gdpProxy = 1e9,
      currentSigmas = Vector.fill(p.sectorDefs.length)(Sigma(0.1)),
      totalPopulation = 100,
      gov = FiscalBudget.GovState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      nbp = com.boombustgroup.amorfati.agents.Nbp.State(Rate(0.05), PLN.Zero, false, PLN.Zero, PLN.Zero, PLN.Zero),
      bankingSector = Generators.testBankingSector().marketState,
      forex = OpenEconomy.ForexState(ExchangeRate(p.forex.baseExRate), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
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
    w.flows.tourismExport.shouldBe(PLN.Zero)
    w.flows.tourismImport.shouldBe(PLN.Zero)
  }
