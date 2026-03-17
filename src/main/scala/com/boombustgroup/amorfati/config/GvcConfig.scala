package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.types.*

/** Global Value Chain integration: sectoral trade, ER pass-through, demand
  * shocks, and commodity prices.
  *
  * Models Poland's deep integration into EU/global supply chains (WIOD/OECD
  * ICIO), with per-sector export shares, GVC depth (backward linkages),
  * differentiated EUR/non-EUR exchange rate pass-through (Campa & Goldberg
  * 2005), foreign demand shocks by sector, supply chain disruption recovery,
  * and exogenous commodity price dynamics (GBM + shock). Commodity prices feed
  * into importCostIndex and energy costs — Poland imports ~95% of oil/gas. EU
  * trade share ~70% of total (GUS/NBP 2024).
  *
  * @param euTradeShare
  *   share of total trade with EU partners (GUS/NBP 2024: ~70%)
  * @param exportShares
  *   per-sector share of total exports (6 sectors, GUS 2024)
  * @param depth
  *   per-sector GVC depth / backward linkage ratio (WIOD/OECD ICIO)
  * @param foreignInflation
  *   annual foreign (trading partner) inflation rate (ECB/IMF)
  * @param foreignGdpGrowth
  *   annual foreign GDP growth rate (ECB/IMF projections)
  * @param erPassthrough
  *   exchange rate pass-through to non-EU import prices (Campa & Goldberg 2005)
  * @param euErPassthrough
  *   exchange rate pass-through to EU import prices (lower due to EUR
  *   invoicing)
  * @param demandShockMonth
  *   simulation month when external demand shock hits (0 = no shock)
  * @param demandShockSize
  *   magnitude of demand shock (fraction of export demand affected)
  * @param demandShockSectors
  *   set of sector indices affected by demand shock
  * @param disruptionRecovery
  *   monthly recovery rate from supply chain disruption
  * @param commodityDrift
  *   annual commodity price drift (long-run trend, IMF commodity outlook)
  * @param commodityVolatility
  *   monthly GBM standard deviation (σ) for commodity price noise
  * @param commodityShockMonth
  *   simulation month when commodity price shock hits (0 = no shock)
  * @param commodityShockMag
  *   one-time shock magnitude as multiplicative increment (e.g., Shock(3.0) =
  *   +300% gas price, 2022-style). Unbounded — can exceed 1.0.
  */
case class GvcConfig(
    euTradeShare: Ratio = Ratio(0.70),
    exportShares: Vector[Ratio] = Vector(Ratio(0.05), Ratio(0.55), Ratio(0.15), Ratio(0.03), Ratio(0.02), Ratio(0.20)),
    depth: Vector[Ratio] = Vector(Ratio(0.35), Ratio(0.75), Ratio(0.30), Ratio(0.40), Ratio(0.10), Ratio(0.45)),
    foreignInflation: Rate = Rate(0.02),
    foreignGdpGrowth: Rate = Rate(0.015),
    erPassthrough: Ratio = Ratio(0.60),
    euErPassthrough: Ratio = Ratio(0.15),
    demandShockMonth: Int = 0,
    demandShockSize: Ratio = Ratio(0.0),
    demandShockSectors: Set[Int] = Set.empty,
    disruptionRecovery: Ratio = Ratio(0.05),
    // Commodity prices (Poland imports ~95% of oil/gas)
    commodityDrift: Rate = Rate(0.02),
    commodityVolatility: Sigma = Sigma(0.03),
    commodityShockMonth: Int = 0,
    commodityShockMag: Shock = Shock.Zero,
):
  require(exportShares.length == 6, s"exportShares must have 6 sectors: ${exportShares.length}")
  require(depth.length == 6, s"depth must have 6 sectors: ${depth.length}")
