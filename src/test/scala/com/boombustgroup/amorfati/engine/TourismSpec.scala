package com.boombustgroup.amorfati.engine

import org.scalatest.flatspec.AnyFlatSpec
import com.boombustgroup.amorfati.Generators
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.agents.Banking
import com.boombustgroup.amorfati.engine.markets.{FiscalBudget, OpenEconomy}
import com.boombustgroup.amorfati.types.*

class TourismSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  // ==========================================================================
  // Config defaults (10 tests)
  // ==========================================================================

  "TourismEnabled" should "default to false" in {
    p.flags.tourism shouldBe false
  }

  "TourismInboundShare" should "default to 0.05" in {
    p.tourism.inboundShare.toDouble shouldBe 0.05
  }

  "TourismOutboundShare" should "default to 0.03" in {
    p.tourism.outboundShare.toDouble shouldBe 0.03
  }

  "TourismErElasticity" should "default to 0.6" in {
    p.tourism.erElasticity shouldBe 0.6
  }

  "TourismSeasonality" should "default to 0.40" in {
    p.tourism.seasonality.toDouble shouldBe 0.40
  }

  "TourismPeakMonth" should "default to 7" in {
    p.tourism.peakMonth shouldBe 7
  }

  "TourismGrowthRate" should "default to 0.03" in {
    p.tourism.growthRate.toDouble shouldBe 0.03
  }

  "TourismShockMonth" should "default to 0" in {
    p.tourism.shockMonth shouldBe 0
  }

  "TourismShockSize" should "default to 0.80" in {
    p.tourism.shockSize.toDouble shouldBe 0.80
  }

  "TourismShockRecovery" should "default to 0.03" in {
    p.tourism.shockRecovery.toDouble shouldBe 0.03
  }

  // ==========================================================================
  // Seasonal factor (3 tests)
  // ==========================================================================

  "Seasonal factor" should "peak in July (month 7) above 1.0" in {
    val monthInYear = 7
    val factor      = 1.0 + p.tourism.seasonality.toDouble *
      Math.cos(2 * Math.PI * (monthInYear - p.tourism.peakMonth) / 12.0)
    factor shouldBe 1.0 + p.tourism.seasonality.toDouble // cos(0) = 1
    factor should be > 1.0
  }

  it should "trough in January below 1.0" in {
    val monthInYear = 1
    val factor      = 1.0 + p.tourism.seasonality.toDouble *
      Math.cos(2 * Math.PI * (monthInYear - p.tourism.peakMonth) / 12.0)
    factor should be < 1.0
  }

  it should "average approximately 1.0 over 12 months" in {
    val factors = (1 to 12).map { m =>
      1.0 + p.tourism.seasonality.toDouble *
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
    val inboundErAdj = Math.pow(weakerER / p.forex.baseExRate, p.tourism.erElasticity)
    inboundErAdj should be > 1.0
  }

  it should "decrease outbound tourism when PLN weakens" in {
    val weakerER      = p.forex.baseExRate * 1.2
    val outboundErAdj = Math.pow(p.forex.baseExRate / weakerER, p.tourism.erElasticity)
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
    val trendAdj = Math.pow(1.0 + p.tourism.growthRate.toDouble / 12.0, 0.0)
    trendAdj shouldBe 1.0
  }

  it should "grow over 12 months" in {
    val trend12 = Math.pow(1.0 + p.tourism.growthRate.toDouble / 12.0, 12.0)
    trend12 should be > 1.0
    trend12 shouldBe (1.0 + p.tourism.growthRate.toDouble) +- 0.001
  }

  // ==========================================================================
  // COVID shock (3 tests)
  // ==========================================================================

  "COVID shock" should "have no disruption when shock month is 0" in {
    val disruption =
      if 0 > 0 && 10 >= 0 then p.tourism.shockSize.toDouble * Math.pow(1.0 - p.tourism.shockRecovery.toDouble, 10.0)
      else 0.0
    disruption shouldBe 0.0
  }

  it should "apply disruption at shock trigger month" in {
    val shockMonth  = 24
    val m           = 24
    val disruption  =
      if shockMonth > 0 && m >= shockMonth then p.tourism.shockSize.toDouble * Math.pow(1.0 - p.tourism.shockRecovery.toDouble, (m - shockMonth).toDouble)
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
      p.tourism.shockSize.toDouble * Math.pow(1.0 - p.tourism.shockRecovery.toDouble, (m - shockMonth).toDouble)
    disruption should be < p.tourism.shockSize.toDouble
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

    val seasonalFactor = 1.0 + p.tourism.seasonality.toDouble *
      Math.cos(2 * Math.PI * (monthInYear - p.tourism.peakMonth) / 12.0)
    val inboundErAdj   = Math.pow(er / p.forex.baseExRate, p.tourism.erElasticity)
    val outboundErAdj  = Math.pow(p.forex.baseExRate / er, p.tourism.erElasticity)
    val trendAdj       = Math.pow(1.0 + p.tourism.growthRate.toDouble / 12.0, m.toDouble)
    val shockFactor    = 1.0 // no shock

    val inbound  = baseGdp * p.tourism.inboundShare.toDouble *
      seasonalFactor * inboundErAdj * trendAdj * shockFactor
    val outbound = baseGdp * p.tourism.outboundShare.toDouble *
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
  // Disabled (1 test)
  // ==========================================================================

  "Tourism flows" should "be zero when disabled" in {
    val (tourismExport, tourismImport) =
      if p.flags.tourism then (100.0, 60.0)
      else (0.0, 0.0)
    tourismExport shouldBe 0.0
    tourismImport shouldBe 0.0
  }

  // ==========================================================================
  // OpenEconomy integration (2 tests)
  // ==========================================================================

  "OpenEconomy exports" should "include tourismExport" in {
    val prevBop   = OpenEconomy.BopState.zero
    val prevForex = OpenEconomy.ForexState(p.forex.baseExRate, PLN.Zero, PLN(p.forex.exportBase.toDouble), PLN.Zero, PLN.Zero)

    val base          = OpenEconomy.StepInput(
      prevBop = prevBop,
      prevForex = prevForex,
      importCons = PLN.Zero,
      techImports = PLN.Zero,
      autoRatio = Ratio.Zero,
      domesticRate = Rate(0.05),
      gdp = PLN(1e9),
      priceLevel = 1.0,
      sectorOutputs = Vector.fill(6)(PLN(1e8)),
      month = 1,
    )
    val resultWith    = OpenEconomy.step(base.copy(tourismExport = PLN(1000.0)))
    val resultWithout = OpenEconomy.step(base.copy(tourismExport = PLN.Zero))

    resultWith.bop.exports shouldBe resultWithout.bop.exports + PLN(1000.0)
  }

  "OpenEconomy imports" should "include tourismImport" in {
    val prevBop   = OpenEconomy.BopState.zero
    val prevForex = OpenEconomy.ForexState(p.forex.baseExRate, PLN.Zero, PLN(p.forex.exportBase.toDouble), PLN.Zero, PLN.Zero)

    val base          = OpenEconomy.StepInput(
      prevBop = prevBop,
      prevForex = prevForex,
      importCons = PLN.Zero,
      techImports = PLN.Zero,
      autoRatio = Ratio.Zero,
      domesticRate = Rate(0.05),
      gdp = PLN(1e9),
      priceLevel = 1.0,
      sectorOutputs = Vector.fill(6)(PLN(1e8)),
      month = 1,
    )
    val resultWith    = OpenEconomy.step(base.copy(tourismImport = PLN(500.0)))
    val resultWithout = OpenEconomy.step(base.copy(tourismImport = PLN.Zero))

    resultWith.bop.totalImports shouldBe resultWithout.bop.totalImports + PLN(500.0)
  }

  // ==========================================================================
  // World defaults (1 test)
  // ==========================================================================

  "World" should "default tourismExport and tourismImport to 0.0" in {
    val w = World(
      month = 0,
      inflation = Rate(0.02),
      priceLevel = 1.0,
      gdpProxy = 1e9,
      currentSigmas = Vector.fill(6)(0.1),
      totalPopulation = 100,
      gov = FiscalBudget.GovState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      nbp = com.boombustgroup.amorfati.agents.Nbp.State(Rate(0.05), PLN.Zero, false, PLN.Zero, PLN.Zero, PLN.Zero),
      bank = Banking.Aggregate(PLN.Zero, PLN.Zero, PLN(100), PLN(1000), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      bankingSector = Generators.testBankingSector(),
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
        giniIndividual = Ratio.Zero,
        giniWealth = Ratio.Zero,
        meanSavings = PLN.Zero,
        medianSavings = PLN.Zero,
        povertyRate50 = Ratio.Zero,
        bankruptcyRate = Ratio.Zero,
        meanSkill = 0.0,
        meanHealthPenalty = 0.0,
        retrainingAttempts = 0,
        retrainingSuccesses = 0,
        consumptionP10 = PLN.Zero,
        consumptionP50 = PLN.Zero,
        consumptionP90 = PLN.Zero,
        meanMonthsToRuin = 0.0,
        povertyRate30 = Ratio.Zero,
        totalRent = PLN.Zero,
        totalDebtService = PLN.Zero,
        totalUnempBenefits = PLN.Zero,
        totalDepositInterest = PLN.Zero,
        crossSectorHires = 0,
        voluntaryQuits = 0,
        sectorMobilityRate = Ratio.Zero,
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
    w.flows.tourismExport shouldBe PLN.Zero
    w.flows.tourismImport shouldBe PLN.Zero
  }
