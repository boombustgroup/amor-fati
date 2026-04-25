package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.types.*

/** Foreign exchange market: PLN/EUR base rate, trade propensities, and interest
  * rate parity.
  *
  * Models the bilateral PLN/EUR exchange rate with interest rate parity (IRP)
  * adjustment, import propensity, and technology import channel. Provides the
  * exchange rate foundation used by OpenEconConfig, GvcConfig, TourismConfig,
  * and RemittanceConfig.
  *
  * @param baseExRate
  *   PLN/EUR exchange rate at simulation start (NBP 2024: ~4.33)
  * @param foreignRate
  *   foreign (ECB) reference interest rate for IRP
  * @param importPropensity
  *   aggregate import-to-GDP ratio (GUS/NBP 2024: ~22%)
  * @param techImportShare
  *   share of imports classified as technology/capital goods
  * @param irpSensitivity
  *   sensitivity of exchange rate to interest rate differential (IRP channel)
  * @param exRateAdjSpeed
  *   monthly exchange rate adjustment speed toward equilibrium
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
    baseExRate: ExchangeRate = ExchangeRate.decimal(433, 2),
    foreignRate: Rate = Rate.decimal(4, 2),
    importPropensity: Share = Share.decimal(22, 2),
    techImportShare: Share = Share.decimal(40, 2),
    irpSensitivity: Coefficient = Coefficient.decimal(15, 2),
    exRateAdjSpeed: Coefficient = Coefficient.decimal(2, 2),
    // Capital flight (risk-off, carry trade)
    riskOffShockMonth: Int = 0,
    riskOffMagnitude: Share = Share.decimal(10, 2),
    riskOffDurationMonths: Int = 6,
    carryThreshold: Rate = Rate.decimal(3, 2),
    carryAccumulationRate: Coefficient = Coefficient.decimal(5, 1),
    carryUnwindSpeed: Share = Share.decimal(30, 2),
    auctionConfidenceThreshold: Share = Share.decimal(90, 2),
    auctionOutflowSensitivity: Coefficient = Coefficient(2),
):
  require(baseExRate > ExchangeRate.decimal(1, 4), s"baseExRate must be positive: $baseExRate")
  require(
    importPropensity >= Share.Zero && importPropensity <= Share.One,
    s"importPropensity must be in [0,1]: $importPropensity",
  )
