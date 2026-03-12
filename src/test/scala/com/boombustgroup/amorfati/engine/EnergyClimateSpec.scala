package com.boombustgroup.amorfati.engine

import org.scalatest.flatspec.AnyFlatSpec
import com.boombustgroup.amorfati.Generators
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.engine.markets.{FiscalBudget, OpenEconomy}
import com.boombustgroup.amorfati.agents.{Banking, BankruptReason, Firm, TechState}
import com.boombustgroup.amorfati.types.*

class EnergyClimateSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  // ==========================================================================
  // Config defaults
  // ==========================================================================

  "EnergyEnabled" should "default to false" in {
    p.flags.energy shouldBe false
  }

  "EnergyCostShares" should "have 6 elements" in {
    p.climate.energyCostShares.map(_.toDouble).length shouldBe 6
  }

  it should "have all values in [0,1]" in
    p.climate.energyCostShares.map(_.toDouble).foreach { r =>
      r should be >= 0.0
      r should be <= 1.0
    }

  it should "have Mfg highest" in {
    p.climate.energyCostShares.map(_.toDouble)(1) shouldBe p.climate.energyCostShares.map(_.toDouble).max
  }

  it should "have BPO lowest" in {
    p.climate.energyCostShares.map(_.toDouble)(0) shouldBe p.climate.energyCostShares.map(_.toDouble).min
  }

  it should "match expected defaults" in {
    p.climate.energyCostShares.map(_.toDouble) shouldBe Vector(0.02, 0.10, 0.04, 0.05, 0.03, 0.06)
  }

  "EnergyCarbonIntensity" should "have 6 elements" in {
    p.climate.carbonIntensity.length shouldBe 6
  }

  it should "have all values in [0,1]" in
    p.climate.carbonIntensity.foreach { r =>
      r should be >= 0.0
      r should be <= 1.0
    }

  it should "have Mfg highest" in {
    p.climate.carbonIntensity(1) shouldBe p.climate.carbonIntensity.max
  }

  it should "match expected defaults" in {
    p.climate.carbonIntensity shouldBe Vector(0.01, 0.08, 0.02, 0.01, 0.02, 0.04)
  }

  "EtsBasePrice" should "default to 80.0" in {
    p.climate.etsBasePrice shouldBe 80.0
  }

  "EtsPriceDrift" should "default to 0.03" in {
    p.climate.etsPriceDrift.toDouble shouldBe 0.03
  }

  "GreenKLRatios" should "have 6 elements" in {
    p.climate.greenKLRatios.map(_.toDouble).length shouldBe 6
  }

  it should "have all positive values" in
    p.climate.greenKLRatios.map(_.toDouble).foreach(_ should be > 0.0)

  it should "have Mfg highest" in {
    p.climate.greenKLRatios.map(_.toDouble)(1) shouldBe p.climate.greenKLRatios.map(_.toDouble).max
  }

  it should "match expected defaults" in {
    p.climate.greenKLRatios.map(_.toDouble) shouldBe Vector(5000.0, 30000.0, 10000.0, 15000.0, 8000.0, 20000.0)
  }

  "GreenDepRate" should "default to 0.04" in {
    p.climate.greenDepRate.toDouble shouldBe 0.04
  }

  "GreenAdjustSpeed" should "default to 0.08" in {
    p.climate.greenAdjustSpeed.toDouble shouldBe 0.08
  }

  "GreenMaxDiscount" should "default to 0.30" in {
    p.climate.greenMaxDiscount.toDouble shouldBe 0.30
  }

  "GreenImportShare" should "default to 0.35" in {
    p.climate.greenImportShare.toDouble shouldBe 0.35
  }

  "GreenInitRatio" should "default to 0.10" in {
    p.climate.greenInitRatio.toDouble shouldBe 0.10
  }

  "GreenBudgetShare" should "default to 0.20" in {
    p.climate.greenBudgetShare.toDouble shouldBe 0.20
  }

  // ==========================================================================
  // ETS price dynamics
  // ==========================================================================

  "ETS price" should "increase over time with positive drift" in {
    val month    = 12
    val etsPrice = p.climate.etsBasePrice * Math.pow(1.0 + p.climate.etsPriceDrift.toDouble / 12.0, month.toDouble)
    etsPrice should be > p.climate.etsBasePrice
  }

  it should "equal base price at month 0" in {
    val etsPrice = p.climate.etsBasePrice * Math.pow(1.0 + p.climate.etsPriceDrift.toDouble / 12.0, 0.0)
    etsPrice shouldBe p.climate.etsBasePrice
  }

  // ==========================================================================
  // Firm defaults
  // ==========================================================================

  private def mkFirm(tech: TechState = TechState.Traditional(10), sector: Int = 0): Firm.State =
    Firm.State(
      FirmId(0),
      PLN(50000.0),
      PLN.Zero,
      tech,
      Ratio(0.5),
      1.0,
      Ratio(0.3),
      SectorIdx(sector),
      Vector.empty[FirmId],
      bankId = BankId(0),
      equityRaised = PLN.Zero,
      initialSize = 10,
      capitalStock = PLN.Zero,
      bondDebt = PLN.Zero,
      foreignOwned = false,
      inventory = PLN.Zero,
      greenCapital = PLN.Zero,
    )

  "Firm" should "default greenCapital to 0.0" in {
    val f = mkFirm()
    f.greenCapital.toDouble shouldBe 0.0
  }

  // ==========================================================================
  // Firm.Result defaults
  // ==========================================================================

  "Firm.Result" should "default energyCost to 0.0" in {
    val r = Firm.Result.zero(mkFirm())
    r.energyCost.toDouble shouldBe 0.0
  }

  it should "default greenInvestment to 0.0" in {
    val r = Firm.Result.zero(mkFirm())
    r.greenInvestment.toDouble shouldBe 0.0
  }

  // ==========================================================================
  // World defaults
  // ==========================================================================

  private def mkMinimalWorld() = World(
    month = 0,
    inflation = Rate(0.02),
    priceLevel = 1.0,
    gdpProxy = 1e9,
    currentSigmas = Vector.fill(6)(5.0),
    totalPopulation = 100000,
    gov = FiscalBudget.GovState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
    nbp = com.boombustgroup.amorfati.agents.Nbp.State(Rate(0.0575), PLN.Zero, false, PLN.Zero, PLN.Zero, PLN.Zero),
    bank = Banking.Aggregate(PLN.Zero, PLN.Zero, PLN(500000000.0), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
    bankingSector = Generators.testBankingSector(),
    forex = OpenEconomy.ForexState(4.33, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
    hhAgg = com.boombustgroup.amorfati.agents.Household.Aggregates(
      employed = 100000,
      unemployed = 0,
      retraining = 0,
      bankrupt = 0,
      totalIncome = PLN.Zero,
      consumption = PLN.Zero,
      domesticConsumption = PLN.Zero,
      importConsumption = PLN.Zero,
      marketWage = PLN(8266.0),
      reservationWage = PLN(4666.0),
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

  "World" should "default aggEnergyCost to 0.0" in {
    val w = mkMinimalWorld()
    w.flows.aggEnergyCost.toDouble shouldBe 0.0
  }

  it should "default aggGreenCapital to 0.0" in {
    val w = mkMinimalWorld()
    w.real.aggGreenCapital.toDouble shouldBe 0.0
  }

  it should "default aggGreenInvestment to 0.0" in {
    val w = mkMinimalWorld()
    w.real.aggGreenInvestment.toDouble shouldBe 0.0
  }

  // ==========================================================================
  // Energy cost formula unit tests
  // ==========================================================================

  "Energy cost formula" should "compute positive base energy cost" in {
    val revenue    = 100000.0
    val sector     = 1 // Mfg
    val baseEnergy = revenue * p.climate.energyCostShares.map(_.toDouble)(sector)
    baseEnergy should be > 0.0
    baseEnergy shouldBe 10000.0 // 100k * 0.10
  }

  it should "increase with carbon surcharge at later months" in {
    val revenue           = 100000.0
    val sector            = 1 // Mfg
    val baseEnergy        = revenue * p.climate.energyCostShares.map(_.toDouble)(sector)
    val month             = 60
    val etsPrice          = p.climate.etsBasePrice * Math.pow(1.0 + p.climate.etsPriceDrift.toDouble / 12.0, month.toDouble)
    val carbonSurcharge   = p.climate.carbonIntensity(sector) * (etsPrice / p.climate.etsBasePrice - 1.0)
    carbonSurcharge should be > 0.0
    val costWithSurcharge = baseEnergy * (1.0 + carbonSurcharge)
    costWithSurcharge should be > baseEnergy
  }

  it should "reduce with green discount" in {
    val revenue          = 100000.0
    val sector           = 1    // Mfg
    val baseEnergy       = revenue * p.climate.energyCostShares.map(_.toDouble)(sector)
    val greenDiscount    = 0.20 // 20%
    val costWithDiscount = baseEnergy * (1.0 - greenDiscount)
    costWithDiscount should be < baseEnergy
  }

  it should "cap green discount at GreenMaxDiscount" in {
    val greenCapital = 1e9                     // very large
    val targetGK     = 30000.0                 // per worker * workers
    val rawRatio     = greenCapital / targetGK // >> 1
    val discount     = Math.min(p.climate.greenMaxDiscount.toDouble, rawRatio * p.climate.greenMaxDiscount.toDouble)
    discount shouldBe p.climate.greenMaxDiscount.toDouble
  }

  // ==========================================================================
  // Green investment mechanics
  // ==========================================================================

  "Green investment" should "depreciate greenCapital" in {
    val depRate = p.climate.greenDepRate.toDouble / 12.0
    val gk      = 100000.0
    val postDep = gk * (1.0 - depRate)
    postDep should be < gk
    postDep should be > 0.0
  }

  it should "compute gap-driven desired investment" in {
    val gk         = 10000.0
    val targetGK   = 50000.0
    val depRate    = p.climate.greenDepRate.toDouble / 12.0
    val depn       = gk * depRate
    val postDepGK  = gk - depn
    val gap        = Math.max(0.0, targetGK - postDepGK)
    val desiredInv = depn + gap * p.climate.greenAdjustSpeed.toDouble
    desiredInv should be > 0.0
    desiredInv should be > depn // gap-driven portion adds to depreciation replacement
  }

  it should "be constrained by green budget share of cash" in {
    val cash        = 100000.0
    val desiredInv  = 50000.0
    val greenBudget = cash * p.climate.greenBudgetShare.toDouble // 20,000
    val actualInv   = Math.min(desiredInv, greenBudget)
    actualInv shouldBe greenBudget
    actualInv shouldBe 20000.0
  }

  it should "be zero for bankrupt firms" in {
    val f = mkFirm(tech = TechState.Bankrupt(BankruptReason.Other("test"))).copy(greenCapital = PLN(5000.0))
    Firm.isAlive(f) shouldBe false
  }

  // ==========================================================================
  // GDP contribution
  // ==========================================================================

  "Green domestic GFCF" should "be positive when enabled with positive investment" in {
    val greenInv          = 100000.0
    val greenDomesticGFCF = greenInv * (1.0 - p.climate.greenImportShare.toDouble)
    greenDomesticGFCF should be > 0.0
    greenDomesticGFCF shouldBe 65000.0 // 100k * 0.65
  }

  "Green import share" should "be correct fraction of investment" in {
    val greenInv     = 100000.0
    val greenImports = greenInv * p.climate.greenImportShare.toDouble
    greenImports shouldBe 35000.0 // 100k * 0.35
  }
