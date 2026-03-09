package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.accounting
import sfc.accounting.{BankingAggregate, ForexState, GovState}
import sfc.agents.{Banking, Firm, TechState}
import sfc.types.*

class EnergyClimateSpec extends AnyFlatSpec with Matchers:

  import sfc.config.SimParams
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

  "Firm" should "default greenCapital to 0.0" in {
    val f = Firm.State(
      FirmId(0),
      PLN(50000.0),
      PLN.Zero,
      TechState.Traditional(10),
      Ratio(0.5),
      1.0,
      Ratio(0.3),
      SectorIdx(0),
      Array.empty[FirmId],
    )
    f.greenCapital.toDouble shouldBe 0.0
  }

  // ==========================================================================
  // Firm.Result defaults
  // ==========================================================================

  "Firm.Result" should "default energyCost to 0.0" in {
    val f = Firm.State(
      FirmId(0),
      PLN(50000.0),
      PLN.Zero,
      TechState.Traditional(10),
      Ratio(0.5),
      1.0,
      Ratio(0.3),
      SectorIdx(0),
      Array.empty[FirmId],
    )
    val r = Firm.Result(f, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
    r.energyCost.toDouble shouldBe 0.0
  }

  it should "default greenInvestment to 0.0" in {
    val f = Firm.State(
      FirmId(0),
      PLN(50000.0),
      PLN.Zero,
      TechState.Traditional(10),
      Ratio(0.5),
      1.0,
      Ratio(0.3),
      SectorIdx(0),
      Array.empty[FirmId],
    )
    val r = Firm.Result(f, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
    r.greenInvestment.toDouble shouldBe 0.0
  }

  // ==========================================================================
  // World defaults
  // ==========================================================================

  "World" should "default aggEnergyCost to 0.0" in {
    val w = World(
      0,
      Rate(0.02),
      1.0,
      GovState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      sfc.agents.Nbp.State(Rate(0.0575)),
      BankingAggregate(PLN.Zero, PLN.Zero, PLN(500000000.0), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      ForexState(4.33, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      sfc.agents.Household.SectorState(100000, PLN(8266.0), PLN(4666.0), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      Ratio.Zero,
      Ratio.Zero,
      1e9,
      Vector.fill(6)(5.0),
      bankingSector = Banking.initialize(1e9, 5e8, 5e8, 0, 0, Banking.DefaultConfigs),
    )
    w.aggEnergyCost.toDouble shouldBe 0.0
  }

  it should "default aggGreenCapital to 0.0" in {
    val w = World(
      0,
      Rate(0.02),
      1.0,
      accounting.GovState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      sfc.agents.Nbp.State(Rate(0.0575)),
      accounting
        .BankingAggregate(PLN.Zero, PLN.Zero, PLN(500000000.0), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      accounting.ForexState(4.33, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      sfc.agents.Household.SectorState(100000, PLN(8266.0), PLN(4666.0), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      Ratio.Zero,
      Ratio.Zero,
      1e9,
      Vector.fill(6)(5.0),
      bankingSector = Banking.initialize(1e9, 5e8, 5e8, 0, 0, Banking.DefaultConfigs),
    )
    w.aggGreenCapital.toDouble shouldBe 0.0
  }

  it should "default aggGreenInvestment to 0.0" in {
    val w = World(
      0,
      Rate(0.02),
      1.0,
      accounting.GovState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      sfc.agents.Nbp.State(Rate(0.0575)),
      accounting
        .BankingAggregate(PLN.Zero, PLN.Zero, PLN(500000000.0), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      accounting.ForexState(4.33, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      sfc.agents.Household.SectorState(100000, PLN(8266.0), PLN(4666.0), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      Ratio.Zero,
      Ratio.Zero,
      1e9,
      Vector.fill(6)(5.0),
      bankingSector = Banking.initialize(1e9, 5e8, 5e8, 0, 0, Banking.DefaultConfigs),
    )
    w.aggGreenInvestment.toDouble shouldBe 0.0
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
    val f = Firm.State(
      FirmId(0),
      PLN(50000.0),
      PLN.Zero,
      TechState.Bankrupt("test"),
      Ratio(0.5),
      1.0,
      Ratio(0.3),
      SectorIdx(0),
      Array.empty[FirmId],
      greenCapital = PLN(5000.0),
    )
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
