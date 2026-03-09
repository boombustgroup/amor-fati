package sfc.config

import sfc.types.*

/** Tourism sector: inbound and outbound flows, seasonality, and COVID-type
  * shock capability.
  *
  * Models bilateral tourism flows calibrated to GUS Tourism Satellite Account
  * 2023 and NBP BoP 2023. Inbound tourism (~5% GDP) generates domestic demand
  * and FX inflow; outbound (~3% GDP) is an import-side leakage. Features
  * seasonal pattern (peak July, +/-40%), exchange rate sensitivity, secular
  * growth trend, and a configurable COVID-type demand shock with gradual
  * recovery. Affects SFC Identities 2 and 4.
  *
  * @param inboundShare
  *   inbound tourism receipts as share of GDP (GUS TSA 2023: ~5%)
  * @param outboundShare
  *   outbound tourism expenditure as share of GDP (NBP BoP 2023: ~3%)
  * @param erElasticity
  *   exchange rate elasticity of tourism flows (weaker PLN boosts inbound)
  * @param seasonality
  *   seasonal amplitude (+/- fraction of base, GUS TSA: ~40%)
  * @param peakMonth
  *   month of peak tourism (1-12, July = 7)
  * @param growthRate
  *   annual secular growth rate of tourism volume (~3%)
  * @param shockMonth
  *   simulation month when tourism shock hits (0 = no shock)
  * @param shockSize
  *   fraction of tourism demand destroyed by shock (e.g. 0.80 = 80% drop)
  * @param shockRecovery
  *   monthly recovery rate after shock (fraction of lost demand restored)
  */
case class TourismConfig(
    inboundShare: Ratio = Ratio(0.05),
    outboundShare: Ratio = Ratio(0.03),
    erElasticity: Double = 0.6,
    seasonality: Ratio = Ratio(0.40),
    peakMonth: Int = 7,
    growthRate: Rate = Rate(0.03),
    shockMonth: Int = 0,
    shockSize: Ratio = Ratio(0.80),
    shockRecovery: Rate = Rate(0.03),
)
