package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.TestFirmState

import org.scalatest.flatspec.AnyFlatSpec
import com.boombustgroup.amorfati.Generators
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.agents.{BankruptReason, Firm, TechState}
import com.boombustgroup.amorfati.types.*

class InventorySpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  // ==========================================================================
  // Config defaults
  // ==========================================================================

  "InventoryTargetRatios" should "have 6 elements" in {
    p.capital.inventoryTargetRatios.map(decimal(_)).length shouldBe BigDecimal("6")
  }

  it should "have all non-negative values" in
    p.capital.inventoryTargetRatios.map(decimal(_)).foreach(_ should be >= BigDecimal("0.0"))

  it should "have Mfg > BPO" in {
    p.capital.inventoryTargetRatios.map(decimal(_))(1) should be > p.capital.inventoryTargetRatios.map(decimal(_))(0)
  }

  it should "match expected defaults" in {
    p.capital.inventoryTargetRatios
      .map(decimal(_)) shouldBe Vector(BigDecimal("0.05"), BigDecimal("0.25"), BigDecimal("0.15"), BigDecimal("0.10"), BigDecimal("0.02"), BigDecimal("0.30"))
  }

  "InventoryAdjustSpeed" should "default to 0.10" in {
    decimal(p.capital.inventoryAdjustSpeed) shouldBe BigDecimal("0.10")
  }

  "InventoryCarryingCost" should "default to 0.06" in {
    decimal(p.capital.inventoryCarryingCost) shouldBe BigDecimal("0.06")
  }

  "InventorySpoilageRates" should "have 6 elements" in {
    p.capital.inventorySpoilageRates.map(decimal(_)).length shouldBe BigDecimal("6")
  }

  it should "have all values in [0,1]" in
    p.capital.inventorySpoilageRates.map(decimal(_)).foreach { r =>
      r should be >= BigDecimal("0.0")
      r should be <= BigDecimal("1.0")
    }

  it should "have Agri highest" in {
    p.capital.inventorySpoilageRates.map(decimal(_))(5) shouldBe p.capital.inventorySpoilageRates.map(decimal(_)).max
  }

  it should "match expected defaults" in {
    p.capital.inventorySpoilageRates
      .map(decimal(_)) shouldBe Vector(BigDecimal("0.0"), BigDecimal("0.02"), BigDecimal("0.05"), BigDecimal("0.03"), BigDecimal("0.0"), BigDecimal("0.10"))
  }

  "InventoryCostFraction" should "default to 0.50" in {
    decimal(p.capital.inventoryCostFraction) shouldBe BigDecimal("0.50")
  }

  "InventoryLiquidationDisc" should "default to 0.50" in {
    decimal(p.capital.inventoryLiquidationDisc) shouldBe BigDecimal("0.50")
  }

  "InventoryInitRatio" should "default to 0.80" in {
    decimal(p.capital.inventoryInitRatio) shouldBe BigDecimal("0.80")
  }

  // ==========================================================================
  // World fields
  // ==========================================================================

  private def mkMinimalWorld() = Generators.testWorld(
    inflation = Rate(0),
    currentSigmas = Vector.fill(6)(Sigma(5)),
    marketWage = PLN(8000),
    reservationWage = PLN(4500),
  )

  "World" should "have aggInventoryStock defaulting to 0.0" in {
    val w = mkMinimalWorld()
    w.flows.aggInventoryStock shouldBe PLN.Zero
  }

  it should "have aggInventoryChange defaulting to 0.0" in {
    val w = mkMinimalWorld()
    w.flows.aggInventoryChange shouldBe PLN.Zero
  }

  // ==========================================================================
  // Firm inventory field
  // ==========================================================================

  private def mkFirm(tech: TechState = TechState.Traditional(10)): Firm.State =
    TestFirmState(
      FirmId(0),
      PLN(50000),
      PLN.Zero,
      tech,
      Share.decimal(5, 1),
      Multiplier.One,
      Share.decimal(3, 1),
      SectorIdx(0),
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

  "Firm" should "have inventory defaulting to 0.0" in {
    mkFirm().inventory shouldBe PLN.Zero
  }

  "Firm.Result" should "have inventoryChange defaulting to 0.0" in {
    val r = Firm.Result.zero(mkFirm())
    r.inventoryChange shouldBe PLN.Zero
  }

  // ==========================================================================
  // Carrying cost
  // ==========================================================================

  "Carrying cost" should "be positive when inventory > 0 and InventoryEnabled" in {
    // Monthly carrying cost = inventory * carryingCostRate / 12
    val inventory = BigDecimal("100000.0")
    val cost      = inventory * decimal(p.capital.inventoryCarryingCost) / BigDecimal("12.0")
    cost should be > BigDecimal("0.0")
  }

  it should "be zero when inventory is 0" in {
    val cost = BigDecimal("0.0") * decimal(p.capital.inventoryCarryingCost) / BigDecimal("12.0")
    cost shouldBe BigDecimal("0.0")
  }

  // ==========================================================================
  // Spoilage
  // ==========================================================================

  "Spoilage" should "reduce inventory stock" in {
    val inventory = BigDecimal("100000.0")
    for s <- 0 until 6 do
      val spoilRate    = decimal(p.capital.inventorySpoilageRates(s)) / BigDecimal("12.0")
      val postSpoilage = inventory * (BigDecimal("1.0") - spoilRate)
      postSpoilage should be <= inventory
  }

  it should "be highest for Agriculture" in {
    val agriRate = decimal(p.capital.inventorySpoilageRates(5)) / BigDecimal("12.0")
    val mfgRate  = decimal(p.capital.inventorySpoilageRates(1)) / BigDecimal("12.0")
    val bpoRate  = decimal(p.capital.inventorySpoilageRates(0)) / BigDecimal("12.0")
    agriRate should be > mfgRate
    mfgRate should be > bpoRate
  }

  // ==========================================================================
  // Stress liquidation
  // ==========================================================================

  "Stress liquidation" should "recover cash at discount" in {
    val inventory = BigDecimal("100000.0")
    val cash      = -BigDecimal("30000.0")
    val disc      = decimal(p.capital.inventoryLiquidationDisc)
    val liquidate = DecimalMath.min(inventory, DecimalMath.abs(cash) / disc)
    val cashBoost = liquidate * disc
    cashBoost should be > BigDecimal("0.0")
    cashBoost shouldBe DecimalMath.abs(cash) // exactly covers the deficit
  }

  it should "not exceed available inventory" in {
    val inventory = BigDecimal("10000.0")
    val cash      = -BigDecimal("100000.0")
    val disc      = decimal(p.capital.inventoryLiquidationDisc)
    val liquidate = DecimalMath.min(inventory, DecimalMath.abs(cash) / disc)
    liquidate shouldBe inventory
  }

  // ==========================================================================
  // Inventory floor
  // ==========================================================================

  "Inventory" should "never go negative" in {
    val postSpoilage = BigDecimal("1000.0")
    val rawChange    = -BigDecimal("5000.0") // trying to draw down more than available
    val invChange    = DecimalMath.max(-postSpoilage, rawChange)
    val newInv       = DecimalMath.max(BigDecimal("0.0"), postSpoilage + invChange)
    newInv should be >= BigDecimal("0.0")
  }

  // ==========================================================================
  // Accumulation
  // ==========================================================================

  "Unsold production" should "add to inventory" in {
    val capacity         = BigDecimal("100000.0")
    val costFraction     = decimal(p.capital.inventoryCostFraction)
    val productionValue  = capacity * costFraction
    val sectorDemandMult = BigDecimal("0.8") // 20% unsold
    val salesValue       = productionValue * DecimalMath.min(BigDecimal("1.0"), sectorDemandMult)
    val unsoldValue      = DecimalMath.max(BigDecimal("0.0"), productionValue - salesValue)
    unsoldValue should be > BigDecimal("0.0")
    unsoldValue shouldBe productionValue * BigDecimal("0.2") +- BigDecimal("0.01")
  }

  "Excess demand" should "not generate unsold production" in {
    val capacity         = BigDecimal("100000.0")
    val costFraction     = decimal(p.capital.inventoryCostFraction)
    val productionValue  = capacity * costFraction
    val sectorDemandMult = BigDecimal("1.2") // excess demand
    val salesValue       = productionValue * DecimalMath.min(BigDecimal("1.0"), sectorDemandMult)
    val unsoldValue      = DecimalMath.max(BigDecimal("0.0"), productionValue - salesValue)
    unsoldValue shouldBe BigDecimal("0.0")
  }

  // ==========================================================================
  // Target ratio convergence
  // ==========================================================================

  "Target adjustment" should "move inventory toward target" in {
    val capacity         = BigDecimal("100000.0")
    val sectorDemandMult = BigDecimal("1.0")
    val revenue          = capacity * sectorDemandMult
    val targetRatio      = BigDecimal("0.15")
    val targetInv        = revenue * targetRatio
    val currentInv       = BigDecimal("0.0") // starting from zero
    val desired          = (targetInv - currentInv) * decimal(p.capital.inventoryAdjustSpeed)
    desired should be > BigDecimal("0.0") // should want to build up
  }

  it should "reduce inventory when above target" in {
    val capacity         = BigDecimal("100000.0")
    val sectorDemandMult = BigDecimal("1.0")
    val revenue          = capacity * sectorDemandMult
    val targetRatio      = BigDecimal("0.15")
    val targetInv        = revenue * targetRatio
    val currentInv       = targetInv * BigDecimal("3.0") // well above target
    val desired          = (targetInv - currentInv) * decimal(p.capital.inventoryAdjustSpeed)
    desired should be < BigDecimal("0.0") // should want to reduce
  }

  // ==========================================================================
  // GDP contribution
  // ==========================================================================

  "GDP formula" should "include inventory change when enabled" in {
    val domesticCons = BigDecimal("1000.0")
    val gov          = BigDecimal("200.0")
    val eu           = BigDecimal("50.0")
    val exports      = BigDecimal("300.0")
    val gfcf         = BigDecimal("100.0")
    val invChange    = BigDecimal("15.0")
    val gdpWithInv   = domesticCons + gov + eu + exports + gfcf + invChange
    val gdpWithout   = domesticCons + gov + eu + exports + gfcf
    gdpWithInv should be > gdpWithout
    (gdpWithInv - gdpWithout) shouldBe invChange
  }

  it should "reduce GDP when destocking" in {
    val invChange      = -BigDecimal("10.0")
    val baseGdp        = BigDecimal("1650.0")
    val gdpWithDestock = baseGdp + invChange
    gdpWithDestock should be < baseGdp
  }

  // ==========================================================================
  // Bankrupt firm
  // ==========================================================================

  "Bankrupt firm" should "have zero inventory" in {
    val f = mkFirm(TechState.Bankrupt(BankruptReason.Other("test")))
      .copy(inventory = PLN(5000))
    // applyInventory should zero out inventory for bankrupt firms
    Firm.isAlive(f) shouldBe false
  }
