package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.types.*

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
    baseMigrationRate: Share = Share.decimal(5, 3),
    housingBarrierThreshold: Share = Share.decimal(7, 1),
)
