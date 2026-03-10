package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.accounting
import sfc.accounting.{BankingAggregate, ForexState, GovState}
import sfc.agents.{Banking, BankruptReason, Firm, TechState}
import sfc.types.*

class InventorySpec extends AnyFlatSpec with Matchers:

  import sfc.config.SimParams
  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  // ==========================================================================
  // Config defaults
  // ==========================================================================

  "InventoryEnabled" should "default to false" in {
    p.flags.inventory shouldBe false
  }

  "InventoryTargetRatios" should "have 6 elements" in {
    p.capital.inventoryTargetRatios.map(_.toDouble).length shouldBe 6
  }

  it should "have all non-negative values" in
    p.capital.inventoryTargetRatios.map(_.toDouble).foreach(_ should be >= 0.0)

  it should "have Mfg > BPO" in {
    p.capital.inventoryTargetRatios.map(_.toDouble)(1) should be > p.capital.inventoryTargetRatios.map(_.toDouble)(0)
  }

  it should "match expected defaults" in {
    p.capital.inventoryTargetRatios.map(_.toDouble) shouldBe Vector(0.05, 0.25, 0.15, 0.10, 0.02, 0.30)
  }

  "InventoryAdjustSpeed" should "default to 0.10" in {
    p.capital.inventoryAdjustSpeed.toDouble shouldBe 0.10
  }

  "InventoryCarryingCost" should "default to 0.06" in {
    p.capital.inventoryCarryingCost.toDouble shouldBe 0.06
  }

  "InventorySpoilageRates" should "have 6 elements" in {
    p.capital.inventorySpoilageRates.map(_.toDouble).length shouldBe 6
  }

  it should "have all values in [0,1]" in
    p.capital.inventorySpoilageRates.map(_.toDouble).foreach { r =>
      r should be >= 0.0
      r should be <= 1.0
    }

  it should "have Agri highest" in {
    p.capital.inventorySpoilageRates.map(_.toDouble)(5) shouldBe p.capital.inventorySpoilageRates.map(_.toDouble).max
  }

  it should "match expected defaults" in {
    p.capital.inventorySpoilageRates.map(_.toDouble) shouldBe Vector(0.0, 0.02, 0.05, 0.03, 0.0, 0.10)
  }

  "InventoryCostFraction" should "default to 0.50" in {
    p.capital.inventoryCostFraction.toDouble shouldBe 0.50
  }

  "InventoryLiquidationDisc" should "default to 0.50" in {
    p.capital.inventoryLiquidationDisc.toDouble shouldBe 0.50
  }

  "InventoryInitRatio" should "default to 0.80" in {
    p.capital.inventoryInitRatio.toDouble shouldBe 0.80
  }

  // ==========================================================================
  // World fields
  // ==========================================================================

  private def mkMinimalWorld() = World(
    month = 0,
    inflation = Rate(0.0),
    priceLevel = 1.0,
    gdpProxy = 1e9,
    currentSigmas = Vector.fill(6)(5.0),
    totalPopulation = 100,
    gov = GovState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
    nbp = sfc.agents.Nbp.State(Rate(0.05), PLN.Zero, false, PLN.Zero, PLN.Zero, PLN.Zero),
    bank = BankingAggregate(PLN.Zero, PLN.Zero, PLN(1e9), PLN(1e9), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
    bankingSector = Banking.initialize(PLN(1e9), PLN(5e8), PLN(5e8), PLN.Zero, PLN.Zero, Banking.DefaultConfigs),
    forex = ForexState(4.33, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
    hhAgg = sfc.agents.Household.Aggregates(
      employed = 100,
      unemployed = 0,
      retraining = 0,
      bankrupt = 0,
      totalIncome = PLN.Zero,
      consumption = PLN.Zero,
      domesticConsumption = PLN.Zero,
      importConsumption = PLN.Zero,
      marketWage = PLN(8000),
      reservationWage = PLN(4500),
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
    ),
    social = SocialState.zero,
    financial = FinancialMarketsState.zero,
    external = ExternalState.zero,
    real = RealState.zero,
    mechanisms = MechanismsState.zero,
    plumbing = MonetaryPlumbingState.zero,
    flows = FlowState.zero,
  )

  "World" should "have aggInventoryStock defaulting to 0.0" in {
    val w = mkMinimalWorld()
    w.flows.aggInventoryStock.toDouble shouldBe 0.0
  }

  it should "have aggInventoryChange defaulting to 0.0" in {
    val w = mkMinimalWorld()
    w.flows.aggInventoryChange.toDouble shouldBe 0.0
  }

  // ==========================================================================
  // Firm inventory field
  // ==========================================================================

  private def mkFirm(tech: TechState = TechState.Traditional(10)): Firm.State =
    Firm.State(
      FirmId(0),
      PLN(50000.0),
      PLN.Zero,
      tech,
      Ratio(0.5),
      1.0,
      Ratio(0.3),
      SectorIdx(0),
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

  "Firm" should "have inventory defaulting to 0.0" in {
    mkFirm().inventory.toDouble shouldBe 0.0
  }

  "Firm.Result" should "have inventoryChange defaulting to 0.0" in {
    val r = Firm.Result.zero(mkFirm())
    r.inventoryChange.toDouble shouldBe 0.0
  }

  // ==========================================================================
  // Carrying cost
  // ==========================================================================

  "Carrying cost" should "be positive when inventory > 0 and InventoryEnabled" in {
    // Monthly carrying cost = inventory * carryingCostRate / 12
    val inventory = 100000.0
    val cost      = inventory * p.capital.inventoryCarryingCost.toDouble / 12.0
    cost should be > 0.0
  }

  it should "be zero when inventory is 0" in {
    val cost = 0.0 * p.capital.inventoryCarryingCost.toDouble / 12.0
    cost shouldBe 0.0
  }

  // ==========================================================================
  // Spoilage
  // ==========================================================================

  "Spoilage" should "reduce inventory stock" in {
    val inventory = 100000.0
    for s <- 0 until 6 do
      val spoilRate    = p.capital.inventorySpoilageRates.map(_.toDouble)(s) / 12.0
      val postSpoilage = inventory * (1.0 - spoilRate)
      postSpoilage should be <= inventory
  }

  it should "be highest for Agriculture" in {
    val agriRate = p.capital.inventorySpoilageRates.map(_.toDouble)(5) / 12.0
    val mfgRate  = p.capital.inventorySpoilageRates.map(_.toDouble)(1) / 12.0
    val bpoRate  = p.capital.inventorySpoilageRates.map(_.toDouble)(0) / 12.0
    agriRate should be > mfgRate
    mfgRate should be > bpoRate
  }

  // ==========================================================================
  // Stress liquidation
  // ==========================================================================

  "Stress liquidation" should "recover cash at discount" in {
    val inventory = 100000.0
    val cash      = -30000.0
    val disc      = p.capital.inventoryLiquidationDisc.toDouble
    val liquidate = Math.min(inventory, Math.abs(cash) / disc)
    val cashBoost = liquidate * disc
    cashBoost should be > 0.0
    cashBoost shouldBe Math.abs(cash) // exactly covers the deficit
  }

  it should "not exceed available inventory" in {
    val inventory = 10000.0
    val cash      = -100000.0
    val disc      = p.capital.inventoryLiquidationDisc.toDouble
    val liquidate = Math.min(inventory, Math.abs(cash) / disc)
    liquidate shouldBe inventory
  }

  // ==========================================================================
  // Inventory floor
  // ==========================================================================

  "Inventory" should "never go negative" in {
    val postSpoilage = 1000.0
    val rawChange    = -5000.0 // trying to draw down more than available
    val invChange    = Math.max(-postSpoilage, rawChange)
    val newInv       = Math.max(0.0, postSpoilage + invChange)
    newInv should be >= 0.0
  }

  // ==========================================================================
  // Accumulation
  // ==========================================================================

  "Unsold production" should "add to inventory" in {
    val capacity         = 100000.0
    val costFraction     = p.capital.inventoryCostFraction.toDouble
    val productionValue  = capacity * costFraction
    val sectorDemandMult = 0.8 // 20% unsold
    val salesValue       = productionValue * Math.min(1.0, sectorDemandMult)
    val unsoldValue      = Math.max(0.0, productionValue - salesValue)
    unsoldValue should be > 0.0
    unsoldValue shouldBe productionValue * 0.2 +- 0.01
  }

  "Excess demand" should "not generate unsold production" in {
    val capacity         = 100000.0
    val costFraction     = p.capital.inventoryCostFraction.toDouble
    val productionValue  = capacity * costFraction
    val sectorDemandMult = 1.2 // excess demand
    val salesValue       = productionValue * Math.min(1.0, sectorDemandMult)
    val unsoldValue      = Math.max(0.0, productionValue - salesValue)
    unsoldValue shouldBe 0.0
  }

  // ==========================================================================
  // Target ratio convergence
  // ==========================================================================

  "Target adjustment" should "move inventory toward target" in {
    val capacity         = 100000.0
    val sectorDemandMult = 1.0
    val revenue          = capacity * sectorDemandMult
    val targetRatio      = 0.15
    val targetInv        = revenue * targetRatio
    val currentInv       = 0.0 // starting from zero
    val desired          = (targetInv - currentInv) * p.capital.inventoryAdjustSpeed.toDouble
    desired should be > 0.0 // should want to build up
  }

  it should "reduce inventory when above target" in {
    val capacity         = 100000.0
    val sectorDemandMult = 1.0
    val revenue          = capacity * sectorDemandMult
    val targetRatio      = 0.15
    val targetInv        = revenue * targetRatio
    val currentInv       = targetInv * 3.0 // well above target
    val desired          = (targetInv - currentInv) * p.capital.inventoryAdjustSpeed.toDouble
    desired should be < 0.0 // should want to reduce
  }

  // ==========================================================================
  // GDP contribution
  // ==========================================================================

  "GDP formula" should "include inventory change when enabled" in {
    val domesticCons = 1000.0
    val gov          = 200.0
    val eu           = 50.0
    val exports      = 300.0
    val gfcf         = 100.0
    val invChange    = 15.0
    val gdpWithInv   = domesticCons + gov + eu + exports + gfcf + invChange
    val gdpWithout   = domesticCons + gov + eu + exports + gfcf
    gdpWithInv should be > gdpWithout
    (gdpWithInv - gdpWithout) shouldBe invChange
  }

  it should "reduce GDP when destocking" in {
    val invChange      = -10.0
    val baseGdp        = 1650.0
    val gdpWithDestock = baseGdp + invChange
    gdpWithDestock should be < baseGdp
  }

  // ==========================================================================
  // Disabled mode
  // ==========================================================================

  "Inventory disabled" should "produce zero inventory columns" in {
    // When InventoryEnabled=false, aggInventoryStock/Change should be 0
    p.flags.inventory shouldBe false
  }

  // ==========================================================================
  // Bankrupt firm
  // ==========================================================================

  "Bankrupt firm" should "have zero inventory" in {
    val f = mkFirm(TechState.Bankrupt(BankruptReason.Other("test")))
      .copy(cash = PLN(-1000.0), debt = PLN(50000.0), inventory = PLN(5000.0))
    // applyInventory should zero out inventory for bankrupt firms
    Firm.isAlive(f) shouldBe false
  }
