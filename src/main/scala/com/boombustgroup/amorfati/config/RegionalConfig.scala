package com.boombustgroup.amorfati.config

/** Regional labor market configuration.
  *
  * @param baseMigrationRate
  *   baseline monthly probability of internal migration for unemployed workers
  *   (GUS 2024: ~0.5% of unemployed migrate per month)
  * @param housingBarrierThreshold
  *   housing cost ratio above which migration is blocked. At 1.0, destination
  *   housing must be ≤ origin housing cost. At 0.5, destination can be 2×.
  */
case class RegionalConfig(
    baseMigrationRate: Double = 0.005,
    housingBarrierThreshold: Double = 0.7,
)
