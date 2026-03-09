package sfc.init

import sfc.agents.*
import sfc.config.SimParams

import scala.util.Random

/** Factory for initial immigrant stock. */
object ImmigrantInit:

  /** Spawn initial immigrants when ImmigEnabled && ImmigInitStock > 0. Returns
    * (updatedHouseholds, populationIncrease) — caller handles totalPopulation.
    */
  def create(
      rng: Random,
      households: Vector[Household.State],
      startId: Int,
  )(using p: SimParams): (Vector[Household.State], Int) =
    if p.flags.immigration && p.immigration.initStock > 0 then
      val immigrants = Immigration.spawnImmigrants(p.immigration.initStock, startId, rng)
      (households ++ immigrants, p.immigration.initStock)
    else (households, 0)
