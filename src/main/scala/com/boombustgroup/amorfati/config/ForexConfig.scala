package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.types.*

/** Foreign exchange market: PLN/EUR base rate, trade propensities, and interest
  * rate parity.
  *
  * Models the bilateral PLN/EUR exchange rate with interest rate parity (IRP)
  * adjustment, import propensity, technology import channel, and export
  * automation boost. Provides the exchange rate foundation used by
  * OpenEconConfig, GvcConfig, TourismConfig, and RemittanceConfig.
  *
  * `exportBase` is in raw PLN — scaled by `gdpRatio` in `SimParams.defaults`.
  *
  * @param baseExRate
  *   PLN/EUR exchange rate at simulation start (NBP 2024: ~4.33)
  * @param foreignRate
  *   foreign (ECB) reference interest rate for IRP
  * @param importPropensity
  *   aggregate import-to-GDP ratio (GUS/NBP 2024: ~22%)
  * @param exportBase
  *   monthly export volume in raw PLN (scaled by gdpRatio)
  * @param techImportShare
  *   share of imports classified as technology/capital goods
  * @param irpSensitivity
  *   sensitivity of exchange rate to interest rate differential (IRP channel)
  * @param exRateAdjSpeed
  *   monthly exchange rate adjustment speed toward equilibrium
  * @param exportAutoBoost
  *   export productivity boost from firm automation
  * @param riskOffShockMonth
  *   simulation month when global risk-off event hits (0 = no shock)
  * @param riskOffMagnitude
  *   capital outflow as fraction of monthly GDP on risk-off (2020/2022: ~10%)
  * @param riskOffDurationMonths
  *   months of elevated risk-off (carry unwind persists)
  * @param carryThreshold
  *   yield spread above which carry trade positions accumulate
  * @param carryAccumulationRate
  *   speed of carry trade stock buildup
  * @param carryUnwindSpeed
  *   monthly fraction of carry stock unwound during risk-off
  * @param auctionConfidenceThreshold
  *   bid-to-cover below which foreign confidence erodes
  * @param auctionOutflowSensitivity
  *   portfolio outflow sensitivity to auction undersubscription
  */
case class ForexConfig(
    baseExRate: Double = 4.33,
    foreignRate: Rate = Rate(0.04),
    importPropensity: Ratio = Ratio(0.22),
    exportBase: PLN = PLN(55.4e9), // raw — scaled by gdpRatio
    techImportShare: Ratio = Ratio(0.40),
    irpSensitivity: Double = 0.15,
    exRateAdjSpeed: Ratio = Ratio(0.02),
    exportAutoBoost: Ratio = Ratio(0.15),
    // Capital flight (risk-off, carry trade)
    riskOffShockMonth: Int = 0,
    riskOffMagnitude: Ratio = Ratio(0.10),
    riskOffDurationMonths: Int = 6,
    carryThreshold: Rate = Rate(0.03),
    carryAccumulationRate: Double = 0.5,
    carryUnwindSpeed: Ratio = Ratio(0.30),
    auctionConfidenceThreshold: Ratio = Ratio(0.90),
    auctionOutflowSensitivity: Double = 2.0,
):
  require(baseExRate > 0, s"baseExRate must be positive: $baseExRate")
  require(
    importPropensity >= Ratio.Zero && importPropensity <= Ratio.One,
    s"importPropensity must be in [0,1]: $importPropensity",
  )
