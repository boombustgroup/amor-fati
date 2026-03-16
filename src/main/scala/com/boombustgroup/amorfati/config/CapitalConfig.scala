package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.types.*

/** Physical capital and inventory accumulation at the firm level.
  *
  * Two mechanisms: (1) physical capital with per-sector capital-labor ratios,
  * Cobb-Douglas production, and vintage depreciation; (2) firm-level
  * inventories with per-sector target ratios, spoilage, carrying costs, and
  * stress liquidation. GDP accounting includes inventory investment (SNA 2008:
  * GDP += delta-inventories).
  *
  * Calibrated to GUS F-01 2024 (capital) and GUS 2024 industry data
  * (inventories).
  *
  * @param klRatios
  *   per-sector capital-labor ratio in PLN per worker (6 sectors, GUS F-01
  *   2024)
  * @param depRates
  *   per-sector annual capital depreciation rate (6 sectors)
  * @param importShare
  *   share of capital investment that is imported (import leakage)
  * @param adjustSpeed
  *   monthly speed of capital stock adjustment toward target
  * @param prodElast
  *   capital share α in CES production function (also Cobb-Douglas when σ≈1)
  * @param costReplace
  *   replacement cost of capital as fraction of original value
  * @param inventoryTargetRatios
  *   per-sector target inventory-to-revenue ratio (6 sectors, GUS 2024)
  * @param inventoryAdjustSpeed
  *   monthly speed of inventory adjustment toward target
  * @param inventoryCarryingCost
  *   annual carrying cost as fraction of inventory value
  * @param inventorySpoilageRates
  *   per-sector monthly spoilage rate (6 sectors, highest for Agriculture)
  * @param inventoryCostFraction
  *   fraction of revenue allocated to inventory replenishment
  * @param inventoryLiquidationDisc
  *   discount applied during stress inventory liquidation
  * @param inventoryInitRatio
  *   initial inventory as fraction of target (0-1)
  * @param inventoryCostReplace
  *   cost of replacing spoiled inventory as fraction of revenue
  */
case class CapitalConfig(
    // Physical capital (GUS F-01 2024)
    klRatios: Vector[PLN] = Vector(PLN(120000.0), PLN(250000.0), PLN(80000.0), PLN(200000.0), PLN(150000.0), PLN(180000.0)),
    depRates: Vector[Rate] = Vector(Rate(0.15), Rate(0.08), Rate(0.10), Rate(0.07), Rate(0.05), Rate(0.08)),
    importShare: Ratio = Ratio(0.35),
    adjustSpeed: Ratio = Ratio(0.10),
    prodElast: Ratio = Ratio(0.30),
    costReplace: Ratio = Ratio(0.50),
    // Inventories (GUS 2024)
    inventoryTargetRatios: Vector[Ratio] = Vector(Ratio(0.05), Ratio(0.25), Ratio(0.15), Ratio(0.10), Ratio(0.02), Ratio(0.30)),
    inventoryAdjustSpeed: Ratio = Ratio(0.10),
    inventoryCarryingCost: Rate = Rate(0.06),
    inventorySpoilageRates: Vector[Rate] = Vector(Rate(0.0), Rate(0.02), Rate(0.05), Rate(0.03), Rate(0.0), Rate(0.10)),
    inventoryCostFraction: Ratio = Ratio(0.50),
    inventoryLiquidationDisc: Ratio = Ratio(0.50),
    inventoryInitRatio: Ratio = Ratio(0.80),
    inventoryCostReplace: Ratio = Ratio(0.10),
):
  require(klRatios.length == 6, s"klRatios must have 6 sectors: ${klRatios.length}")
  require(depRates.length == 6, s"depRates must have 6 sectors: ${depRates.length}")
  require(
    inventoryTargetRatios.length == 6,
    s"inventoryTargetRatios must have 6 sectors: ${inventoryTargetRatios.length}",
  )
  require(
    inventorySpoilageRates.length == 6,
    s"inventorySpoilageRates must have 6 sectors: ${inventorySpoilageRates.length}",
  )
