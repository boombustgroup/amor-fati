package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.TestFirmState

import org.scalatest.flatspec.AnyFlatSpec
import com.boombustgroup.amorfati.Generators
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.agents.{BankruptReason, Firm, TechState}
import com.boombustgroup.amorfati.types.*

class EnergyClimateSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  // ==========================================================================
  // Config defaults
  // ==========================================================================

  "EnergyCostShares" should "have 6 elements" in {
    p.climate.energyCostShares.map(decimal(_)).length shouldBe BigDecimal("6")
  }

  it should "have all values in [0,1]" in
    p.climate.energyCostShares.map(decimal(_)).foreach { r =>
      r should be >= BigDecimal("0.0")
      r should be <= BigDecimal("1.0")
    }

  it should "have Mfg highest" in {
    p.climate.energyCostShares.map(decimal(_))(1) shouldBe p.climate.energyCostShares.map(decimal(_)).max
  }

  it should "have BPO lowest" in {
    p.climate.energyCostShares.map(decimal(_))(0) shouldBe p.climate.energyCostShares.map(decimal(_)).min
  }

  it should "match expected defaults" in {
    p.climate.energyCostShares
      .map(decimal(_)) shouldBe Vector(BigDecimal("0.02"), BigDecimal("0.10"), BigDecimal("0.04"), BigDecimal("0.05"), BigDecimal("0.03"), BigDecimal("0.06"))
  }

  "EnergyCarbonIntensity" should "have 6 elements" in {
    p.climate.carbonIntensity.length shouldBe 6
  }

  it should "have all values in [0,1]" in
    p.climate.carbonIntensity.map(decimal(_)).foreach { r =>
      r should be >= BigDecimal("0.0")
      r should be <= BigDecimal("1.0")
    }

  it should "have Mfg highest" in {
    p.climate.carbonIntensity.map(decimal(_))(1) shouldBe p.climate.carbonIntensity.map(decimal(_)).max
  }

  it should "match expected defaults" in {
    p.climate.carbonIntensity
      .map(decimal(_)) shouldBe Vector(BigDecimal("0.01"), BigDecimal("0.08"), BigDecimal("0.02"), BigDecimal("0.01"), BigDecimal("0.02"), BigDecimal("0.04"))
  }

  "EtsBasePrice" should "default to 80.0" in {
    decimal(p.climate.etsBasePrice) shouldBe BigDecimal("80.0")
  }

  "EtsPriceDrift" should "default to 0.03" in {
    decimal(p.climate.etsPriceDrift) shouldBe BigDecimal("0.03")
  }

  "GreenKLRatios" should "have 6 elements" in {
    p.climate.greenKLRatios.map(decimal(_)).length shouldBe BigDecimal("6")
  }

  it should "have all positive values" in
    p.climate.greenKLRatios.map(decimal(_)).foreach(_ should be > BigDecimal("0.0"))

  it should "have Mfg highest" in {
    p.climate.greenKLRatios.map(decimal(_))(1) shouldBe p.climate.greenKLRatios.map(decimal(_)).max
  }

  it should "match expected defaults" in {
    p.climate.greenKLRatios.map(decimal(_)) shouldBe Vector(
      BigDecimal("5000.0"),
      BigDecimal("30000.0"),
      BigDecimal("10000.0"),
      BigDecimal("15000.0"),
      BigDecimal("8000.0"),
      BigDecimal("20000.0"),
    )
  }

  "GreenDepRate" should "default to 0.04" in {
    decimal(p.climate.greenDepRate) shouldBe BigDecimal("0.04")
  }

  "GreenAdjustSpeed" should "default to 0.08" in {
    decimal(p.climate.greenAdjustSpeed) shouldBe BigDecimal("0.08")
  }

  "GreenMaxDiscount" should "default to 0.30" in {
    decimal(p.climate.greenMaxDiscount) shouldBe BigDecimal("0.30")
  }

  "GreenImportShare" should "default to 0.35" in {
    decimal(p.climate.greenImportShare) shouldBe BigDecimal("0.35")
  }

  "GreenInitRatio" should "default to 0.10" in {
    decimal(p.climate.greenInitRatio) shouldBe BigDecimal("0.10")
  }

  "GreenBudgetShare" should "default to 0.20" in {
    decimal(p.climate.greenBudgetShare) shouldBe BigDecimal("0.20")
  }

  // ==========================================================================
  // ETS price dynamics
  // ==========================================================================

  "ETS price" should "increase over time with positive drift" in {
    val month    = 12
    val etsPrice = decimal(p.climate.etsBasePrice) * powDecimal(BigDecimal(1) + decimal(p.climate.etsPriceDrift) / BigDecimal(12), month)
    etsPrice should be > decimal(p.climate.etsBasePrice)
  }

  it should "equal base price at month 0" in {
    val etsPrice = decimal(p.climate.etsBasePrice) * powDecimal(BigDecimal(1) + decimal(p.climate.etsPriceDrift) / BigDecimal(12), 0)
    etsPrice shouldBe decimal(p.climate.etsBasePrice)
  }

  // ==========================================================================
  // Firm defaults
  // ==========================================================================

  private def mkFirm(tech: TechState = TechState.Traditional(10), sector: Int = 0): Firm.State =
    TestFirmState(
      FirmId(0),
      PLN("50000.0"),
      PLN.Zero,
      tech,
      Share("0.5"),
      Multiplier.One,
      Share("0.3"),
      SectorIdx(sector),
      Vector.empty[FirmId],
      bankId = BankId(0),
      equityRaised = PLN.Zero,
      initialSize = 10,
      capitalStock = PLN.Zero,
      foreignOwned = false,
      inventory = PLN.Zero,
      greenCapital = PLN.Zero,
      accumulatedLoss = PLN.Zero,
    )

  "Firm" should "default greenCapital to 0.0" in {
    val f = mkFirm()
    decimal(f.greenCapital) shouldBe BigDecimal("0.0")
  }

  // ==========================================================================
  // Firm.Result defaults
  // ==========================================================================

  "Firm.Result" should "default energyCost to 0.0" in {
    val r = Firm.Result.zero(mkFirm())
    decimal(r.energyCost) shouldBe BigDecimal("0.0")
  }

  it should "default greenInvestment to 0.0" in {
    val r = Firm.Result.zero(mkFirm())
    decimal(r.greenInvestment) shouldBe BigDecimal("0.0")
  }

  // ==========================================================================
  // World defaults
  // ==========================================================================

  private def mkMinimalWorld() = Generators.testWorld(
    currentSigmas = Vector.fill(6)(Sigma("5.0")),
    totalPopulation = 100000,
    employed = 100000,
    marketWage = PLN("8266.0"),
    reservationWage = PLN("4666.0"),
  )

  "World" should "default aggEnergyCost to 0.0" in {
    val w = mkMinimalWorld()
    decimal(w.flows.aggEnergyCost) shouldBe BigDecimal("0.0")
  }

  it should "default aggGreenCapital to 0.0" in {
    val w = mkMinimalWorld()
    decimal(w.real.aggGreenCapital) shouldBe BigDecimal("0.0")
  }

  it should "default aggGreenInvestment to 0.0" in {
    val w = mkMinimalWorld()
    decimal(w.real.aggGreenInvestment) shouldBe BigDecimal("0.0")
  }

  // ==========================================================================
  // Energy cost formula unit tests
  // ==========================================================================

  "Energy cost formula" should "compute positive base energy cost" in {
    val revenue    = BigDecimal("100000.0")
    val sector     = 1 // Mfg
    val baseEnergy = revenue * decimal(p.climate.energyCostShares(sector))
    baseEnergy should be > BigDecimal("0.0")
    baseEnergy shouldBe BigDecimal("10000.0") // 100k * 0.10
  }

  it should "increase with carbon surcharge at later months" in {
    val revenue           = BigDecimal("100000.0")
    val sector            = 1 // Mfg
    val baseEnergy        = revenue * decimal(p.climate.energyCostShares(sector))
    val month             = 60
    val etsPrice          = decimal(p.climate.etsBasePrice) * powDecimal(BigDecimal(1) + decimal(p.climate.etsPriceDrift) / BigDecimal(12), month)
    val carbonSurcharge   = decimal(p.climate.carbonIntensity(sector)) * (etsPrice / decimal(p.climate.etsBasePrice) - BigDecimal(1))
    carbonSurcharge should be > BigDecimal("0.0")
    val costWithSurcharge = baseEnergy * (BigDecimal(1) + carbonSurcharge)
    costWithSurcharge should be > baseEnergy
  }

  it should "reduce with green discount" in {
    val revenue          = BigDecimal("100000.0")
    val sector           = 1                  // Mfg
    val baseEnergy       = revenue * decimal(p.climate.energyCostShares(sector))
    val greenDiscount    = BigDecimal("0.20") // 20%
    val costWithDiscount = baseEnergy * (BigDecimal(1) - greenDiscount)
    costWithDiscount should be < baseEnergy
  }

  it should "cap green discount at GreenMaxDiscount" in {
    val greenCapital = BigDecimal("1000000000.0") // very large
    val targetGK     = BigDecimal("30000.0")      // per worker * workers
    val rawRatio     = greenCapital / targetGK    // >> 1
    val discount     = decimal(p.climate.greenMaxDiscount).min(rawRatio * decimal(p.climate.greenMaxDiscount))
    discount shouldBe decimal(p.climate.greenMaxDiscount)
  }

  // ==========================================================================
  // Green investment mechanics
  // ==========================================================================

  "Green investment" should "depreciate greenCapital" in {
    val depRate = decimal(p.climate.greenDepRate) / BigDecimal(12)
    val gk      = BigDecimal("100000.0")
    val postDep = gk * (BigDecimal(1) - depRate)
    postDep should be < gk
    postDep should be > BigDecimal("0.0")
  }

  it should "compute gap-driven desired investment" in {
    val gk         = BigDecimal("10000.0")
    val targetGK   = BigDecimal("50000.0")
    val depRate    = decimal(p.climate.greenDepRate) / BigDecimal(12)
    val depn       = gk * depRate
    val postDepGK  = gk - depn
    val gap        = (targetGK - postDepGK).max(BigDecimal(0))
    val desiredInv = depn + gap * decimal(p.climate.greenAdjustSpeed)
    desiredInv should be > BigDecimal("0.0")
    desiredInv should be > depn // gap-driven portion adds to depreciation replacement
  }

  it should "be constrained by green budget share of cash" in {
    val cash        = BigDecimal("100000.0")
    val desiredInv  = BigDecimal("50000.0")
    val greenBudget = cash * decimal(p.climate.greenBudgetShare) // 20,000
    val actualInv   = desiredInv.min(greenBudget)
    actualInv shouldBe greenBudget
    actualInv shouldBe BigDecimal("20000.0")
  }

  it should "be zero for bankrupt firms" in {
    val f = mkFirm(tech = TechState.Bankrupt(BankruptReason.Other("test"))).copy(greenCapital = PLN("5000.0"))
    Firm.isAlive(f) shouldBe false
  }

  // ==========================================================================
  // GDP contribution
  // ==========================================================================

  "Green domestic GFCF" should "be positive when enabled with positive investment" in {
    val greenInv          = BigDecimal("100000.0")
    val greenDomesticGFCF = greenInv * (BigDecimal(1) - decimal(p.climate.greenImportShare))
    greenDomesticGFCF should be > BigDecimal("0.0")
    greenDomesticGFCF shouldBe BigDecimal("65000.0") // 100k * 0.65
  }

  "Green import share" should "be correct fraction of investment" in {
    val greenInv     = BigDecimal("100000.0")
    val greenImports = greenInv * decimal(p.climate.greenImportShare)
    greenImports shouldBe BigDecimal("35000.0") // 100k * 0.35
  }
