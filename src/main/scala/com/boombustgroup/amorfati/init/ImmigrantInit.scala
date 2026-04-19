package com.boombustgroup.amorfati.init

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams

/** Factory for initial immigrant stock. */
object ImmigrantInit:

  /** Spawn initial immigrants when ImmigEnabled && ImmigInitStock > 0. Uses
    * households.length as startId internally. Returns households with aligned
    * ledger financial stocks.
    */
  def create(randomness: InitRandomness.ImmigrationStreams, population: Household.Population)(using p: SimParams): Household.Population =
    if p.immigration.initStock > 0 then
      val immigrants = Immigration.spawnImmigrantPopulation(p.immigration.initStock, population.households.length, randomness.initialStock)
      Household.Population(
        households = population.households ++ immigrants.households,
        financialStocks = population.financialStocks ++ immigrants.financialStocks,
      )
    else population
