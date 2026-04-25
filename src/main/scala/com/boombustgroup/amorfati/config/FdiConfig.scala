package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.types.*

/** Foreign Direct Investment composition: per-sector foreign ownership, profit
  * outflows, and M&A.
  *
  * Models the two-channel FDI outflow (profit shifting + repatriation)
  * calibrated to NBP IIP 2024 and GUS 2024 data on foreign-owned firms in
  * Poland. Cumulative FDI stock ~1.1 bln PLN (~50% GDP). Affects SFC Identity 4
  * (external balance).
  *
  * @param foreignShares
  *   per-sector share of firms that are foreign-owned (6 sectors, GUS/NBP 2024)
  * @param profitShiftRate
  *   fraction of foreign firms' profits shifted via transfer pricing
  * @param repatriationRate
  *   fraction of declared profits repatriated to parent (dividends)
  * @param maProb
  *   monthly probability of M&A conversion (domestic firm acquired by foreign
  *   entity)
  * @param maSizeMin
  *   minimum firm size (employees) eligible for M&A
  */
case class FdiConfig(
    foreignShares: Vector[Share] = Vector(Share.decimal(15, 2), Share.decimal(30, 2), Share.decimal(10, 2), Share.decimal(3, 2), Share(0), Share.decimal(5, 2)),
    profitShiftRate: Share = Share.decimal(15, 2),
    repatriationRate: Share = Share.decimal(70, 2),
    maProb: Share = Share.decimal(1, 3),
    maSizeMin: Int = 50,
):
  require(foreignShares.length == 6, s"foreignShares must have 6 sectors: ${foreignShares.length}")
