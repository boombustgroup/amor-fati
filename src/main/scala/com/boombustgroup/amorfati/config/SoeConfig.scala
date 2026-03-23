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
    baseDividendMultiplier: Multiplier = Multiplier(1.3),
    dividendFiscalThreshold: Share = Share(0.03),
    dividendFiscalSensitivity: Coefficient = Coefficient(5.0),
    firingReduction: Share = Share(0.70),
    investmentMultiplier: Multiplier = Multiplier(1.2),
    energyPassthrough: Share = Share(0.60),
)
