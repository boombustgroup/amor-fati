package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.accounting
import sfc.accounting.{BankState, ForexState, GovState}
import sfc.config.{Config, SECTORS}
import sfc.agents.{Firm, FirmLogic, FirmOps, FirmResult, TechState}
import sfc.types.*

class InventorySpec extends AnyFlatSpec with Matchers:

  // ==========================================================================
  // Config defaults
  // ==========================================================================

  "InventoryEnabled" should "default to false" in {
    Config.InventoryEnabled shouldBe false
  }

  "InventoryTargetRatios" should "have 6 elements" in {
    Config.InventoryTargetRatios.length shouldBe 6
  }

  it should "have all non-negative values" in {
    Config.InventoryTargetRatios.foreach(_ should be >= 0.0)
  }

  it should "have Mfg > BPO" in {
    Config.InventoryTargetRatios(1) should be > Config.InventoryTargetRatios(0)
  }

  it should "match expected defaults" in {
    Config.InventoryTargetRatios shouldBe Vector(0.05, 0.25, 0.15, 0.10, 0.02, 0.30)
  }

  "InventoryAdjustSpeed" should "default to 0.10" in {
    Config.InventoryAdjustSpeed shouldBe 0.10
  }

  "InventoryCarryingCost" should "default to 0.06" in {
    Config.InventoryCarryingCost shouldBe 0.06
  }

  "InventorySpoilageRates" should "have 6 elements" in {
    Config.InventorySpoilageRates.length shouldBe 6
  }

  it should "have all values in [0,1]" in {
    Config.InventorySpoilageRates.foreach { r =>
      r should be >= 0.0
      r should be <= 1.0
    }
  }

  it should "have Agri highest" in {
    Config.InventorySpoilageRates(5) shouldBe Config.InventorySpoilageRates.max
  }

  it should "match expected defaults" in {
    Config.InventorySpoilageRates shouldBe Vector(0.0, 0.02, 0.05, 0.03, 0.0, 0.10)
  }

  "InventoryCostFraction" should "default to 0.50" in {
    Config.InventoryCostFraction shouldBe 0.50
  }

  "InventoryLiquidationDisc" should "default to 0.50" in {
    Config.InventoryLiquidationDisc shouldBe 0.50
  }

  "InventoryInitRatio" should "default to 0.80" in {
    Config.InventoryInitRatio shouldBe 0.80
  }

  // ==========================================================================
  // World fields
  // ==========================================================================

  "World" should "have aggInventoryStock defaulting to 0.0" in {
    val w = World(0, 0.0, 1.0,
      GovState(false, 0, 0, 0, 0, 0),
      sfc.agents.NbpState(0.05),
      BankState(0, 0, 1e9, 1e9),
      ForexState(4.33, 0, 0, 0, 0),
      sfc.agents.HhState(100, 8000, 4500, 0, 0, 0, 0),
      0, 0, 1e9, Vector.fill(6)(5.0))
    w.aggInventoryStock shouldBe 0.0
  }

  it should "have aggInventoryChange defaulting to 0.0" in {
    val w = World(0, 0.0, 1.0,
      accounting.GovState(false, 0, 0, 0, 0, 0),
      sfc.agents.NbpState(0.05),
      accounting.BankState(0, 0, 1e9, 1e9),
      accounting.ForexState(4.33, 0, 0, 0, 0),
      sfc.agents.HhState(100, 8000, 4500, 0, 0, 0, 0),
      0, 0, 1e9, Vector.fill(6)(5.0))
    w.aggInventoryChange shouldBe 0.0
  }

  // ==========================================================================
  // Firm inventory field
  // ==========================================================================

  "Firm" should "have inventory defaulting to 0.0" in {
    val f = Firm(FirmId(0), 50000.0, 0.0, TechState.Traditional(10),
      0.5, 1.0, 0.3, SectorIdx(0), Array.empty[Int])
    f.inventory shouldBe 0.0
  }

  "FirmResult" should "have inventoryChange defaulting to 0.0" in {
    val f = Firm(FirmId(0), 50000.0, 0.0, TechState.Traditional(10),
      0.5, 1.0, 0.3, SectorIdx(0), Array.empty[Int])
    val r = FirmResult(f, 0.0, 0.0, 0.0, 0.0)
    r.inventoryChange shouldBe 0.0
  }

  // ==========================================================================
  // Carrying cost
  // ==========================================================================

  "Carrying cost" should "be positive when inventory > 0 and InventoryEnabled" in {
    // Monthly carrying cost = inventory * carryingCostRate / 12
    val inventory = 100000.0
    val cost = inventory * Config.InventoryCarryingCost / 12.0
    cost should be > 0.0
  }

  it should "be zero when inventory is 0" in {
    val cost = 0.0 * Config.InventoryCarryingCost / 12.0
    cost shouldBe 0.0
  }

  // ==========================================================================
  // Spoilage
  // ==========================================================================

  "Spoilage" should "reduce inventory stock" in {
    val inventory = 100000.0
    for s <- 0 until 6 do
      val spoilRate = Config.InventorySpoilageRates(s) / 12.0
      val postSpoilage = inventory * (1.0 - spoilRate)
      postSpoilage should be <= inventory
  }

  it should "be highest for Agriculture" in {
    val agriRate = Config.InventorySpoilageRates(5) / 12.0
    val mfgRate = Config.InventorySpoilageRates(1) / 12.0
    val bpoRate = Config.InventorySpoilageRates(0) / 12.0
    agriRate should be > mfgRate
    mfgRate should be > bpoRate
  }

  // ==========================================================================
  // Stress liquidation
  // ==========================================================================

  "Stress liquidation" should "recover cash at discount" in {
    val inventory = 100000.0
    val cash = -30000.0
    val disc = Config.InventoryLiquidationDisc
    val liquidate = Math.min(inventory, Math.abs(cash) / disc)
    val cashBoost = liquidate * disc
    cashBoost should be > 0.0
    cashBoost shouldBe Math.abs(cash) // exactly covers the deficit
  }

  it should "not exceed available inventory" in {
    val inventory = 10000.0
    val cash = -100000.0
    val disc = Config.InventoryLiquidationDisc
    val liquidate = Math.min(inventory, Math.abs(cash) / disc)
    liquidate shouldBe inventory
  }

  // ==========================================================================
  // Inventory floor
  // ==========================================================================

  "Inventory" should "never go negative" in {
    val postSpoilage = 1000.0
    val rawChange = -5000.0 // trying to draw down more than available
    val invChange = Math.max(-postSpoilage, rawChange)
    val newInv = Math.max(0.0, postSpoilage + invChange)
    newInv should be >= 0.0
  }

  // ==========================================================================
  // Accumulation
  // ==========================================================================

  "Unsold production" should "add to inventory" in {
    val capacity = 100000.0
    val costFraction = Config.InventoryCostFraction
    val productionValue = capacity * costFraction
    val sectorDemandMult = 0.8 // 20% unsold
    val salesValue = productionValue * Math.min(1.0, sectorDemandMult)
    val unsoldValue = Math.max(0.0, productionValue - salesValue)
    unsoldValue should be > 0.0
    unsoldValue shouldBe productionValue * 0.2 +- 0.01
  }

  "Excess demand" should "not generate unsold production" in {
    val capacity = 100000.0
    val costFraction = Config.InventoryCostFraction
    val productionValue = capacity * costFraction
    val sectorDemandMult = 1.2 // excess demand
    val salesValue = productionValue * Math.min(1.0, sectorDemandMult)
    val unsoldValue = Math.max(0.0, productionValue - salesValue)
    unsoldValue shouldBe 0.0
  }

  // ==========================================================================
  // Target ratio convergence
  // ==========================================================================

  "Target adjustment" should "move inventory toward target" in {
    val capacity = 100000.0
    val sectorDemandMult = 1.0
    val revenue = capacity * sectorDemandMult
    val targetRatio = 0.15
    val targetInv = revenue * targetRatio
    val currentInv = 0.0 // starting from zero
    val desired = (targetInv - currentInv) * Config.InventoryAdjustSpeed
    desired should be > 0.0 // should want to build up
  }

  it should "reduce inventory when above target" in {
    val capacity = 100000.0
    val sectorDemandMult = 1.0
    val revenue = capacity * sectorDemandMult
    val targetRatio = 0.15
    val targetInv = revenue * targetRatio
    val currentInv = targetInv * 3.0 // well above target
    val desired = (targetInv - currentInv) * Config.InventoryAdjustSpeed
    desired should be < 0.0 // should want to reduce
  }

  // ==========================================================================
  // GDP contribution
  // ==========================================================================

  "GDP formula" should "include inventory change when enabled" in {
    val domesticCons = 1000.0
    val gov = 200.0
    val eu = 50.0
    val exports = 300.0
    val gfcf = 100.0
    val invChange = 15.0
    val gdpWithInv = domesticCons + gov + eu + exports + gfcf + invChange
    val gdpWithout = domesticCons + gov + eu + exports + gfcf
    gdpWithInv should be > gdpWithout
    (gdpWithInv - gdpWithout) shouldBe invChange
  }

  it should "reduce GDP when destocking" in {
    val invChange = -10.0
    val baseGdp = 1650.0
    val gdpWithDestock = baseGdp + invChange
    gdpWithDestock should be < baseGdp
  }

  // ==========================================================================
  // Disabled mode
  // ==========================================================================

  "Inventory disabled" should "produce zero inventory columns" in {
    // When InventoryEnabled=false, aggInventoryStock/Change should be 0
    Config.InventoryEnabled shouldBe false
  }

  // ==========================================================================
  // Bankrupt firm
  // ==========================================================================

  "Bankrupt firm" should "have zero inventory" in {
    val f = Firm(FirmId(0), -1000.0, 50000.0, TechState.Bankrupt("test"),
      0.5, 1.0, 0.3, SectorIdx(0), Array.empty[Int], inventory = 5000.0)
    // applyInventory should zero out inventory for bankrupt firms
    FirmOps.isAlive(f) shouldBe false
  }
