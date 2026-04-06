package com.boombustgroup.amorfati.init

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams

import scala.util.Random

/** Factory for initial immigrant stock. */
object ImmigrantInit:

  /** Spawn initial immigrants when ImmigEnabled && ImmigInitStock > 0. Uses
    * households.length as startId internally. Returns updated household vector.
    */
  def create(rng: Random, households: Vector[Household.State])(using p: SimParams): Vector[Household.State] =
    if true && p.immigration.initStock > 0 then
      val immigrants = Immigration.spawnImmigrants(p.immigration.initStock, households.length, rng)
      households ++ immigrants
    else households
