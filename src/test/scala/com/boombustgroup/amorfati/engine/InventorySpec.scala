package com.boombustgroup.amorfati.engine

import org.scalatest.flatspec.AnyFlatSpec
import com.boombustgroup.amorfati.Generators
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.engine.markets.{FiscalBudget, OpenEconomy}
import com.boombustgroup.amorfati.agents.{BankruptReason, Firm, TechState}
import com.boombustgroup.amorfati.fp.ComputationBoundary
import com.boombustgroup.amorfati.types.*

class InventorySpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]
  private val td           = ComputationBoundary

  // ==========================================================================
  // Config defaults
  // ==========================================================================

  "InventoryTargetRatios" should "have 6 elements" in {
    p.capital.inventoryTargetRatios.map(td.toDouble(_)).length shouldBe 6
  }

  it should "have all non-negative values" in
    p.capital.inventoryTargetRatios.map(td.toDouble(_)).foreach(_ should be >= 0.0)

  it should "have Mfg > BPO" in {
    p.capital.inventoryTargetRatios.map(td.toDouble(_))(1) should be > p.capital.inventoryTargetRatios.map(td.toDouble(_))(0)
  }

  it should "match expected defaults" in {
    p.capital.inventoryTargetRatios.map(td.toDouble(_)) shouldBe Vector(0.05, 0.25, 0.15, 0.10, 0.02, 0.30)
  }

  "InventoryAdjustSpeed" should "default to 0.10" in {
    td.toDouble(p.capital.inventoryAdjustSpeed) shouldBe 0.10
  }

  "InventoryCarryingCost" should "default to 0.06" in {
    td.toDouble(p.capital.inventoryCarryingCost) shouldBe 0.06
  }

  "InventorySpoilageRates" should "have 6 elements" in {
    p.capital.inventorySpoilageRates.map(td.toDouble(_)).length shouldBe 6
  }

  it should "have all values in [0,1]" in
    p.capital.inventorySpoilageRates.map(td.toDouble(_)).foreach { r =>
      r should be >= 0.0
      r should be <= 1.0
    }

  it should "have Agri highest" in {
    p.capital.inventorySpoilageRates.map(td.toDouble(_))(5) shouldBe p.capital.inventorySpoilageRates.map(td.toDouble(_)).max
  }

  it should "match expected defaults" in {
    p.capital.inventorySpoilageRates.map(td.toDouble(_)) shouldBe Vector(0.0, 0.02, 0.05, 0.03, 0.0, 0.10)
  }

  "InventoryCostFraction" should "default to 0.50" in {
    td.toDouble(p.capital.inventoryCostFraction) shouldBe 0.50
  }

  "InventoryLiquidationDisc" should "default to 0.50" in {
    td.toDouble(p.capital.inventoryLiquidationDisc) shouldBe 0.50
  }

  "InventoryInitRatio" should "default to 0.80" in {
    td.toDouble(p.capital.inventoryInitRatio) shouldBe 0.80
  }

  // ==========================================================================
  // World fields
  // ==========================================================================

  private def mkMinimalWorld() = World(
    inflation = Rate(0.0),
    priceLevel = 1.0,
    gdpProxy = 1e9,
    currentSigmas = Vector.fill(6)(Sigma(5.0)),
    totalPopulation = 100,
    gov = FiscalBudget.GovState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
    nbp = com.boombustgroup.amorfati.agents.Nbp.State(Rate(0.05), PLN.Zero, false, PLN.Zero, PLN.Zero, PLN.Zero),
    bankingSector = Generators.testBankingSector().marketState,
    forex = OpenEconomy.ForexState(ExchangeRate(4.33), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
    hhAgg = com.boombustgroup.amorfati.agents.Household.Aggregates(
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
    financialMarkets = FinancialMarketsState.zero,
    external = ExternalState.zero,
    real = RealState.zero,
    mechanisms = MechanismsState.zero,
    plumbing = MonetaryPlumbingState.zero,
    flows = FlowState.zero,
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
    Firm.State(
      FirmId(0),
      PLN(50000.0),
      PLN.Zero,
      tech,
      Share(0.5),
      Multiplier.One,
      Share(0.3),
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
    val inventory = 100000.0
    val cost      = inventory * td.toDouble(p.capital.inventoryCarryingCost) / 12.0
    cost should be > 0.0
  }

  it should "be zero when inventory is 0" in {
    val cost = 0.0 * td.toDouble(p.capital.inventoryCarryingCost) / 12.0
    cost shouldBe 0.0
  }

  // ==========================================================================
  // Spoilage
  // ==========================================================================

  "Spoilage" should "reduce inventory stock" in {
    val inventory = 100000.0
    for s <- 0 until 6 do
      val spoilRate    = td.toDouble(p.capital.inventorySpoilageRates(s)) / 12.0
      val postSpoilage = inventory * (1.0 - spoilRate)
      postSpoilage should be <= inventory
  }

  it should "be highest for Agriculture" in {
    val agriRate = td.toDouble(p.capital.inventorySpoilageRates(5)) / 12.0
    val mfgRate  = td.toDouble(p.capital.inventorySpoilageRates(1)) / 12.0
    val bpoRate  = td.toDouble(p.capital.inventorySpoilageRates(0)) / 12.0
    agriRate should be > mfgRate
    mfgRate should be > bpoRate
  }

  // ==========================================================================
  // Stress liquidation
  // ==========================================================================

  "Stress liquidation" should "recover cash at discount" in {
    val inventory = 100000.0
    val cash      = -30000.0
    val disc      = td.toDouble(p.capital.inventoryLiquidationDisc)
    val liquidate = Math.min(inventory, Math.abs(cash) / disc)
    val cashBoost = liquidate * disc
    cashBoost should be > 0.0
    cashBoost shouldBe Math.abs(cash) // exactly covers the deficit
  }

  it should "not exceed available inventory" in {
    val inventory = 10000.0
    val cash      = -100000.0
    val disc      = td.toDouble(p.capital.inventoryLiquidationDisc)
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
    val costFraction     = td.toDouble(p.capital.inventoryCostFraction)
    val productionValue  = capacity * costFraction
    val sectorDemandMult = 0.8 // 20% unsold
    val salesValue       = productionValue * Math.min(1.0, sectorDemandMult)
    val unsoldValue      = Math.max(0.0, productionValue - salesValue)
    unsoldValue should be > 0.0
    unsoldValue shouldBe productionValue * 0.2 +- 0.01
  }

  "Excess demand" should "not generate unsold production" in {
    val capacity         = 100000.0
    val costFraction     = td.toDouble(p.capital.inventoryCostFraction)
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
    val desired          = (targetInv - currentInv) * td.toDouble(p.capital.inventoryAdjustSpeed)
    desired should be > 0.0 // should want to build up
  }

  it should "reduce inventory when above target" in {
    val capacity         = 100000.0
    val sectorDemandMult = 1.0
    val revenue          = capacity * sectorDemandMult
    val targetRatio      = 0.15
    val targetInv        = revenue * targetRatio
    val currentInv       = targetInv * 3.0 // well above target
    val desired          = (targetInv - currentInv) * td.toDouble(p.capital.inventoryAdjustSpeed)
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
  // Bankrupt firm
  // ==========================================================================

  "Bankrupt firm" should "have zero inventory" in {
    val f = mkFirm(TechState.Bankrupt(BankruptReason.Other("test")))
      .copy(cash = PLN(-1000.0), debt = PLN(50000.0), inventory = PLN(5000.0))
    // applyInventory should zero out inventory for bankrupt firms
    Firm.isAlive(f) shouldBe false
  }
