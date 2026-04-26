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
    importContent: Vector[Share] =
      Vector(Share.decimal(15, 2), Share.decimal(50, 2), Share.decimal(20, 2), Share.decimal(15, 2), Share.decimal(5, 2), Share.decimal(12, 2)),
    erFloor: ExchangeRate = ExchangeRate.decimal(25, 1),
    erCeiling: ExchangeRate = ExchangeRate(10),
    exportBase: PLN = PLN(138500000000L),              // raw — scaled by gdpRatio
    importPushCap: Share = Share.decimal(3, 2),
    foreignGdpGrowth: Rate = Rate.decimal(15, 3),
    exportPriceElasticity: Coefficient = Coefficient.decimal(8, 1),
    importPriceElasticity: Coefficient = Coefficient.decimal(6, 1),
    erElasticity: Coefficient = Coefficient.decimal(5, 1),
    ulcExportBoost: Coefficient = Coefficient.decimal(15, 2),
    nfaReturnRate: Rate = Rate.decimal(3, 2),
    euTransfers: PLN = PLN(1458000000),                // raw — scaled by gdpRatio
    fdiBase: PLN = PLN(583100000),                     // raw — scaled by gdpRatio
    portfolioSensitivity: Coefficient = Coefficient.decimal(20, 2),
    riskPremiumSensitivity: Coefficient = Coefficient.decimal(10, 2),
    pppSpeed: Coefficient = Coefficient.decimal(10, 2), // annual convergence speed toward PPP equilibrium (Rogoff 1996: 3-5yr half-life)
):
  require(erFloor > ExchangeRate.decimal(1, 4), s"erFloor must be positive: $erFloor")
  require(erFloor < erCeiling, s"erFloor ($erFloor) must be < erCeiling ($erCeiling)")
  require(importContent.length == 6, s"importContent must have 6 sectors: ${importContent.length}")
