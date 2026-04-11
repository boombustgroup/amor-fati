package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

import com.boombustgroup.amorfati.random.RandomStream

/** Internal migration between NUTS-1 macroregions.
  *
  * Workers migrate from high-unemployment / low-wage regions to high-wage /
  * low-unemployment regions, subject to:
  *   - Geographic friction (distance, social ties)
  *   - Housing cost barrier (Warsaw is 1.8× national average)
  *   - Wage differential pull (only migrate if destination wages justify)
  *
  * This generates persistent regional inequality: Eastern Poland hemorrhages
  * workers to Central/South, but housing costs in Warsaw limit absorption.
  * Structural unemployment = national NAIRU + regional mismatch component.
  *
  * Pure function — no mutable state. Only unemployed HH consider migration.
  *
  * Calibration: GUS internal migration statistics 2024, NBP residential price
  * survey.
  */
object RegionalMigration:

  /** Result: households with potentially updated regions. */
  case class Result(households: Vector[Household.State])

  /** Monthly migration step: unemployed HH may relocate to a better region.
    * Employed workers are tied to their firm's region and do not migrate.
    */
  def apply(households: Vector[Household.State], regionalWages: Map[Region, PLN], rng: RandomStream)(using p: SimParams): Result =
    Result(households.map(considerMigration(_, regionalWages, rng)))

  /** Evaluate migration for a single household. Only unemployed workers
    * consider relocation.
    */
  private def considerMigration(hh: Household.State, regionalWages: Map[Region, PLN], rng: RandomStream)(using p: SimParams) =
    hh.status match
      case HhStatus.Unemployed(_) => tryRelocate(hh, regionalWages, rng)
      case _                      => hh

  /** Attempt relocation for an unemployed household. Finds the best target
    * region (highest migration probability), then rolls the dice to decide
    * whether to move.
    */
  private def tryRelocate(hh: Household.State, regionalWages: Map[Region, PLN], rng: RandomStream)(using p: SimParams) =
    val currentWage = regionalWages.getOrElse(hh.region, PLN.Zero)
    findBestTarget(hh.region, currentWage, regionalWages).fold(hh): (target, targetScore) =>
      val moveProbability = targetScore * p.regional.baseMigrationRate
      if moveProbability.sampleBelow(rng) then hh.copy(region = target)
      else hh

  /** Find the destination region with highest migration probability. Returns
    * None if no viable target exists (all blocked by housing barrier or wage
    * differential).
    */
  private def findBestTarget(origin: Region, originWage: PLN, regionalWages: Map[Region, PLN])(using p: SimParams) =
    candidateTargets(origin, originWage, regionalWages).maxByOption(_._2.toLong)

  /** Score all viable destination regions for a migrant from the origin region.
    */
  private def candidateTargets(origin: Region, originWage: PLN, regionalWages: Map[Region, PLN])(using p: SimParams) =
    Region.all
      .filter(_ != origin)
      .map(target => (target, destinationScore(origin, target, originWage, regionalWages)))
      .filter((_, score) => score > Share.Zero)

  /** Compute migration score from origin to a specific target. Wraps
    * Region.migrationProbability with wage ratio computation.
    */
  private def destinationScore(origin: Region, target: Region, originWage: PLN, regionalWages: Map[Region, PLN])(using p: SimParams) =
    val targetWage = regionalWages.getOrElse(target, PLN.Zero)
    val wageRatio  = if originWage > PLN.Zero then targetWage.ratioTo(originWage).toMultiplier else Multiplier.One
    Region.migrationProbability(origin, target, wageRatio, p.regional.housingBarrierThreshold)
