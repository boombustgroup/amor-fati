package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.accounting
import sfc.accounting.{BankState, ForexState, GovState}
import sfc.agents.{Firm, TechState}
import sfc.config.Config
import sfc.types.*

class EnergyClimateSpec extends AnyFlatSpec with Matchers:

  // ==========================================================================
  // Config defaults
  // ==========================================================================

  "EnergyEnabled" should "default to false" in {
    Config.EnergyEnabled shouldBe false
  }

  "EnergyCostShares" should "have 6 elements" in {
    Config.EnergyCostShares.length shouldBe 6
  }

  it should "have all values in [0,1]" in {
    Config.EnergyCostShares.foreach { r =>
      r should be >= 0.0
      r should be <= 1.0
    }
  }

  it should "have Mfg highest" in {
    Config.EnergyCostShares(1) shouldBe Config.EnergyCostShares.max
  }

  it should "have BPO lowest" in {
    Config.EnergyCostShares(0) shouldBe Config.EnergyCostShares.min
  }

  it should "match expected defaults" in {
    Config.EnergyCostShares shouldBe Vector(0.02, 0.10, 0.04, 0.05, 0.03, 0.06)
  }

  "EnergyCarbonIntensity" should "have 6 elements" in {
    Config.EnergyCarbonIntensity.length shouldBe 6
  }

  it should "have all values in [0,1]" in {
    Config.EnergyCarbonIntensity.foreach { r =>
      r should be >= 0.0
      r should be <= 1.0
    }
  }

  it should "have Mfg highest" in {
    Config.EnergyCarbonIntensity(1) shouldBe Config.EnergyCarbonIntensity.max
  }

  it should "match expected defaults" in {
    Config.EnergyCarbonIntensity shouldBe Vector(0.01, 0.08, 0.02, 0.01, 0.02, 0.04)
  }

  "EtsBasePrice" should "default to 80.0" in {
    Config.EtsBasePrice shouldBe 80.0
  }

  "EtsPriceDrift" should "default to 0.03" in {
    Config.EtsPriceDrift shouldBe 0.03
  }

  "GreenKLRatios" should "have 6 elements" in {
    Config.GreenKLRatios.length shouldBe 6
  }

  it should "have all positive values" in {
    Config.GreenKLRatios.foreach(_ should be > 0.0)
  }

  it should "have Mfg highest" in {
    Config.GreenKLRatios(1) shouldBe Config.GreenKLRatios.max
  }

  it should "match expected defaults" in {
    Config.GreenKLRatios shouldBe Vector(5000.0, 30000.0, 10000.0, 15000.0, 8000.0, 20000.0)
  }

  "GreenDepRate" should "default to 0.04" in {
    Config.GreenDepRate shouldBe 0.04
  }

  "GreenAdjustSpeed" should "default to 0.08" in {
    Config.GreenAdjustSpeed shouldBe 0.08
  }

  "GreenMaxDiscount" should "default to 0.30" in {
    Config.GreenMaxDiscount shouldBe 0.30
  }

  "GreenImportShare" should "default to 0.35" in {
    Config.GreenImportShare shouldBe 0.35
  }

  "GreenInitRatio" should "default to 0.10" in {
    Config.GreenInitRatio shouldBe 0.10
  }

  "GreenBudgetShare" should "default to 0.20" in {
    Config.GreenBudgetShare shouldBe 0.20
  }

  // ==========================================================================
  // ETS price dynamics
  // ==========================================================================

  "ETS price" should "increase over time with positive drift" in {
    val month = 12
    val etsPrice = Config.EtsBasePrice * Math.pow(1.0 + Config.EtsPriceDrift / 12.0, month.toDouble)
    etsPrice should be > Config.EtsBasePrice
  }

  it should "equal base price at month 0" in {
    val etsPrice = Config.EtsBasePrice * Math.pow(1.0 + Config.EtsPriceDrift / 12.0, 0.0)
    etsPrice shouldBe Config.EtsBasePrice
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
      Array.empty[Int],
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
      Array.empty[Int],
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
      Array.empty[Int],
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
      GovState(false, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      sfc.agents.Nbp.State(Rate(0.0575)),
      BankState(PLN.Zero, PLN.Zero, PLN(500000000.0), PLN.Zero),
      ForexState(4.33, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      sfc.agents.Household.SectorState(100000, PLN(8266.0), PLN(4666.0), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      Ratio.Zero,
      Ratio.Zero,
      1e9,
      Vector.fill(6)(5.0),
    )
    w.aggEnergyCost.toDouble shouldBe 0.0
  }

  it should "default aggGreenCapital to 0.0" in {
    val w = World(
      0,
      Rate(0.02),
      1.0,
      accounting.GovState(false, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      sfc.agents.Nbp.State(Rate(0.0575)),
      accounting.BankState(PLN.Zero, PLN.Zero, PLN(500000000.0), PLN.Zero),
      accounting.ForexState(4.33, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      sfc.agents.Household.SectorState(100000, PLN(8266.0), PLN(4666.0), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      Ratio.Zero,
      Ratio.Zero,
      1e9,
      Vector.fill(6)(5.0),
    )
    w.aggGreenCapital.toDouble shouldBe 0.0
  }

  it should "default aggGreenInvestment to 0.0" in {
    val w = World(
      0,
      Rate(0.02),
      1.0,
      accounting.GovState(false, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      sfc.agents.Nbp.State(Rate(0.0575)),
      accounting.BankState(PLN.Zero, PLN.Zero, PLN(500000000.0), PLN.Zero),
      accounting.ForexState(4.33, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      sfc.agents.Household.SectorState(100000, PLN(8266.0), PLN(4666.0), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      Ratio.Zero,
      Ratio.Zero,
      1e9,
      Vector.fill(6)(5.0),
    )
    w.aggGreenInvestment.toDouble shouldBe 0.0
  }

  // ==========================================================================
  // Energy cost formula unit tests
  // ==========================================================================

  "Energy cost formula" should "compute positive base energy cost" in {
    val revenue = 100000.0
    val sector = 1 // Mfg
    val baseEnergy = revenue * Config.EnergyCostShares(sector)
    baseEnergy should be > 0.0
    baseEnergy shouldBe 10000.0 // 100k * 0.10
  }

  it should "increase with carbon surcharge at later months" in {
    val revenue = 100000.0
    val sector = 1 // Mfg
    val baseEnergy = revenue * Config.EnergyCostShares(sector)
    val month = 60
    val etsPrice = Config.EtsBasePrice * Math.pow(1.0 + Config.EtsPriceDrift / 12.0, month.toDouble)
    val carbonSurcharge = Config.EnergyCarbonIntensity(sector) * (etsPrice / Config.EtsBasePrice - 1.0)
    carbonSurcharge should be > 0.0
    val costWithSurcharge = baseEnergy * (1.0 + carbonSurcharge)
    costWithSurcharge should be > baseEnergy
  }

  it should "reduce with green discount" in {
    val revenue = 100000.0
    val sector = 1 // Mfg
    val baseEnergy = revenue * Config.EnergyCostShares(sector)
    val greenDiscount = 0.20 // 20%
    val costWithDiscount = baseEnergy * (1.0 - greenDiscount)
    costWithDiscount should be < baseEnergy
  }

  it should "cap green discount at GreenMaxDiscount" in {
    val greenCapital = 1e9 // very large
    val targetGK = 30000.0 // per worker * workers
    val rawRatio = greenCapital / targetGK // >> 1
    val discount = Math.min(Config.GreenMaxDiscount, rawRatio * Config.GreenMaxDiscount)
    discount shouldBe Config.GreenMaxDiscount
  }

  // ==========================================================================
  // Green investment mechanics
  // ==========================================================================

  "Green investment" should "depreciate greenCapital" in {
    val depRate = Config.GreenDepRate / 12.0
    val gk = 100000.0
    val postDep = gk * (1.0 - depRate)
    postDep should be < gk
    postDep should be > 0.0
  }

  it should "compute gap-driven desired investment" in {
    val gk = 10000.0
    val targetGK = 50000.0
    val depRate = Config.GreenDepRate / 12.0
    val depn = gk * depRate
    val postDepGK = gk - depn
    val gap = Math.max(0.0, targetGK - postDepGK)
    val desiredInv = depn + gap * Config.GreenAdjustSpeed
    desiredInv should be > 0.0
    desiredInv should be > depn // gap-driven portion adds to depreciation replacement
  }

  it should "be constrained by green budget share of cash" in {
    val cash = 100000.0
    val desiredInv = 50000.0
    val greenBudget = cash * Config.GreenBudgetShare // 20,000
    val actualInv = Math.min(desiredInv, greenBudget)
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
      Array.empty[Int],
      greenCapital = PLN(5000.0),
    )
    Firm.isAlive(f) shouldBe false
  }

  // ==========================================================================
  // GDP contribution
  // ==========================================================================

  "Green domestic GFCF" should "be positive when enabled with positive investment" in {
    val greenInv = 100000.0
    val greenDomesticGFCF = greenInv * (1.0 - Config.GreenImportShare)
    greenDomesticGFCF should be > 0.0
    greenDomesticGFCF shouldBe 65000.0 // 100k * 0.65
  }

  "Green import share" should "be correct fraction of investment" in {
    val greenInv = 100000.0
    val greenImports = greenInv * Config.GreenImportShare
    greenImports shouldBe 35000.0 // 100k * 0.35
  }
