package sfc.init

import sfc.agents.*
import sfc.config.Config

import scala.util.Random

/** Factory for initial immigrant stock. */
object ImmigrantInit:

  /** Spawn initial immigrants when ImmigEnabled && ImmigInitStock > 0 && individual mode. Returns (updatedHouseholds,
    * populationIncrease) — caller handles Config.setTotalPopulation.
    */
  def create(
    rng: Random,
    households: Option[Vector[Household.State]],
    startId: Int,
  ): (Option[Vector[Household.State]], Int) =
    if Config.ImmigEnabled && Config.ImmigInitStock > 0 && households.isDefined then
      val immigrants = Immigration.spawnImmigrants(Config.ImmigInitStock, startId, rng)
      (households.map(hhs => hhs ++ immigrants), Config.ImmigInitStock)
    else (households, 0)
