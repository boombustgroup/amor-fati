package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.types.*

/** Calvo staggered pricing configuration.
  *
  * @param calvoTheta
  *   monthly probability of price reset (Alvarez et al. 2006: 0.15 → avg price
  *   duration ~6.7 months for EU)
  * @param baseMarkup
  *   steady-state markup over marginal cost (micro data: ~1.15 for Poland)
  * @param demandSensitivity
  *   markup elasticity to demand/capacity gap
  * @param costPassthrough
  *   fraction of wage growth passed to markup on reset
  * @param minMarkup
  *   minimum markup (floor, prevents below-cost pricing)
  * @param maxMarkup
  *   maximum markup (ceiling, prevents monopolistic extremes)
  */
case class PricingConfig(
    calvoTheta: Share = Share("0.15"),
    baseMarkup: Multiplier = Multiplier("1.15"),
    demandSensitivity: Coefficient = Coefficient("0.5"),
    costPassthrough: Coefficient = Coefficient("0.4"),
    minMarkup: Multiplier = Multiplier("0.95"),
    maxMarkup: Multiplier = Multiplier("1.50"),
)
