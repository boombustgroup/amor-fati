package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.types.*

/** State-owned enterprise configuration.
  *
  * @param baseDividendMultiplier
  *   baseline SOE dividend payout multiplier vs private firms (MF: ~1.3×)
  * @param dividendFiscalThreshold
  *   deficit/GDP above which government demands extra SOE dividends
  * @param dividendFiscalSensitivity
  *   extra dividend multiplier per unit of deficit/GDP above threshold
  * @param firingReduction
  *   fraction of normal firing that SOE actually executes (political buffer)
  * @param investmentMultiplier
  *   SOE investment rate vs private sector (directed investment)
  * @param energyPassthrough
  *   fraction of commodity price shock SOE energy firms pass to consumers
  */
case class SoeConfig(
    baseDividendMultiplier: Multiplier = Multiplier.decimal(13, 1),
    dividendFiscalThreshold: Share = Share.decimal(3, 2),
    dividendFiscalSensitivity: Coefficient = Coefficient(5),
    firingReduction: Share = Share.decimal(70, 2),
    investmentMultiplier: Multiplier = Multiplier.decimal(12, 1),
    energyPassthrough: Share = Share.decimal(60, 2),
)
