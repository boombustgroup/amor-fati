package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.types.*

/** Insurance sector: life and non-life reserves, asset allocation, and
  * counter-cyclical claims.
  *
  * Models the Polish insurance industry with separate life (~110 mld PLN) and
  * non-life (~90 mld PLN) reserve pools (KNF 2024). Three-asset allocation
  * across government bonds, corporate bonds, and equities. Non-life claims are
  * counter-cyclical (unemployment-sensitive). Affects SFC Identities 2 and 5.
  *
  * Reserve values are in raw PLN — scaled by `gdpRatio` in
  * `SimParams.defaults`.
  *
  * @param lifeReserves
  *   initial life insurance reserves in raw PLN (KNF 2024: ~110 mld, scaled by
  *   gdpRatio)
  * @param nonLifeReserves
  *   initial non-life insurance reserves in raw PLN (KNF 2024: ~90 mld, scaled
  *   by gdpRatio)
  * @param govBondShare
  *   share of reserves invested in government bonds
  * @param corpBondShare
  *   share of reserves invested in corporate bonds
  * @param equityShare
  *   share of reserves invested in equities (GPW)
  * @param lifePremiumRate
  *   monthly life premium as fraction of household income
  * @param nonLifePremiumRate
  *   monthly non-life premium as fraction of household income
  * @param lifeLossRatio
  *   life insurance loss ratio (claims / premiums)
  * @param nonLifeLossRatio
  *   non-life base loss ratio (before cyclical adjustment)
  * @param nonLifeUnempSens
  *   sensitivity of non-life claims to unemployment (counter-cyclical)
  * @param rebalanceSpeed
  *   monthly portfolio rebalancing speed toward target allocation
  */
case class InsuranceConfig(
    lifeReserves: PLN = PLN("110e9"),   // raw — scaled by gdpRatio
    nonLifeReserves: PLN = PLN("90e9"), // raw — scaled by gdpRatio
    govBondShare: Share = Share("0.35"),
    corpBondShare: Share = Share("0.08"),
    equityShare: Share = Share("0.12"),
    lifePremiumRate: Share = Share("0.003"),
    nonLifePremiumRate: Share = Share("0.0025"),
    lifeLossRatio: Share = Share("0.85"),
    nonLifeLossRatio: Share = Share("0.70"),
    nonLifeUnempSens: Coefficient = Coefficient("0.5"),
    rebalanceSpeed: Coefficient = Coefficient("0.05"),
)
