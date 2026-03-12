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
    foreignShares: Vector[Ratio] = Vector(Ratio(0.15), Ratio(0.30), Ratio(0.10), Ratio(0.03), Ratio(0.00), Ratio(0.05)),
    profitShiftRate: Ratio = Ratio(0.15),
    repatriationRate: Ratio = Ratio(0.70),
    maProb: Ratio = Ratio(0.001),
    maSizeMin: Int = 50,
):
  require(foreignShares.length == 6, s"foreignShares must have 6 sectors: ${foreignShares.length}")
