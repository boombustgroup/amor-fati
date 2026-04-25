package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.types.*

/** Diaspora remittance inflows from Polish workers abroad.
  *
  * Models the inbound remittance channel calibrated to NBP BoP 2024 (~18 mld
  * PLN/year). Remittances enter as household income, with exchange rate
  * pass-through and counter-cyclical sensitivity to excess unemployment.
  * Affects SFC Identities 2 (bank balance sheet) and 4 (external balance).
  *
  * @param perCapita
  *   monthly remittance per diaspora member (PLN, NBP BoP 2024: ~40
  *   PLN/os/month)
  * @param erElasticity
  *   exchange rate pass-through elasticity (weaker PLN = higher PLN-denominated
  *   inflow)
  * @param growthRate
  *   annual secular growth rate of remittance volume (NBP trend: ~2%)
  * @param cyclicalSens
  *   sensitivity of remittances to excess domestic unemployment
  *   (counter-cyclical buffer)
  */
case class RemittanceConfig(
    perCapita: PLN = PLN(40),
    erElasticity: Coefficient = Coefficient.decimal(5, 1),
    growthRate: Rate = Rate.decimal(2, 2),
    cyclicalSens: Coefficient = Coefficient.decimal(3, 1),
)
