package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.types.*

/** Open economy: trade elasticities, import content, net foreign assets, EU
  * transfers, and FDI base.
  *
  * Extends the basic ForexConfig with detailed trade dynamics: per-sector
  * import content, Marshall-Lerner elasticities, exchange rate bands, ULC-based
  * competitiveness, net foreign asset returns, EU structural transfers, and FDI
  * base flow. Calibrated to GUS/NBP 2024 balance of payments data.
  *
  * Stock values (`exportBase`, `euTransfers`, `fdiBase`) are in raw PLN —
  * scaled by `gdpRatio` in `SimParams.defaults`.
  *
  * @param importContent
  *   per-sector import content of production (6 sectors, GUS supply-use tables
  *   2024)
  * @param erFloor
  *   exchange rate floor (PLN/EUR) — structural lower bound
  * @param erCeiling
  *   exchange rate ceiling (PLN/EUR) — structural upper bound
  * @param exportBase
  *   monthly export base in raw PLN (scaled by gdpRatio, NBP BoP 2024: ~138.5
  *   mld)
  * @param importPushCap
  *   maximum monthly import growth from demand pressure
  * @param foreignGdpGrowth
  *   annual foreign (EU) GDP growth rate (ECB/IMF projections)
  * @param exportPriceElasticity
  *   price elasticity of export demand (Marshall-Lerner, Campa & Goldberg 2005)
  * @param importPriceElasticity
  *   price elasticity of import demand
  * @param erElasticity
  *   exchange rate elasticity of trade flows
  * @param ulcExportBoost
  *   export boost from unit labor cost competitiveness improvement
  * @param nfaReturnRate
  *   annual return rate on net foreign assets
  * @param euTransfers
  *   monthly EU structural transfers in raw PLN (scaled by gdpRatio, MFiPR
  *   2024: ~1.458 mld)
  * @param fdiBase
  *   monthly FDI base inflow in raw PLN (scaled by gdpRatio, NBP IIP 2024: ~583
  *   mln)
  * @param portfolioSensitivity
  *   sensitivity of portfolio flows to interest rate differential
  * @param riskPremiumSensitivity
  *   sensitivity of portfolio flows to risk premium changes
  */
case class OpenEconConfig(
    importContent: Vector[Share] = Vector(Share(0.15), Share(0.50), Share(0.20), Share(0.15), Share(0.05), Share(0.12)),
    erFloor: Double = 2.5,
    erCeiling: Double = 10.0,
    exportBase: PLN = PLN(138.5e9),           // raw — scaled by gdpRatio
    importPushCap: Share = Share(0.03),
    foreignGdpGrowth: Rate = Rate(0.015),
    exportPriceElasticity: Coefficient = Coefficient(0.8),
    importPriceElasticity: Coefficient = Coefficient(0.6),
    erElasticity: Coefficient = Coefficient(0.5),
    ulcExportBoost: Coefficient = Coefficient(0.15),
    nfaReturnRate: Rate = Rate(0.03),
    euTransfers: PLN = PLN(1.458e9),          // raw — scaled by gdpRatio
    fdiBase: PLN = PLN(583.1e6),              // raw — scaled by gdpRatio
    portfolioSensitivity: Coefficient = Coefficient(0.20),
    riskPremiumSensitivity: Coefficient = Coefficient(0.10),
    pppSpeed: Coefficient = Coefficient(0.10), // annual convergence speed toward PPP equilibrium (Rogoff 1996: 3-5yr half-life)
):
  require(erFloor > 0, s"erFloor must be positive: $erFloor")
  require(erFloor < erCeiling, s"erFloor ($erFloor) must be < erCeiling ($erCeiling)")
  require(importContent.length == 6, s"importContent must have 6 sectors: ${importContent.length}")
