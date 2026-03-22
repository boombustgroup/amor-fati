package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

import scala.util.Random

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
  def apply(
      households: Vector[Household.State],
      regionalWages: Map[Region, PLN],
      rng: Random,
  )(using p: SimParams): Result =
    Result(households.map(considerMigration(_, regionalWages, rng)))

  /** Evaluate migration for a single household. Only unemployed workers
    * consider relocation.
    */
  private def considerMigration(
      hh: Household.State,
      regionalWages: Map[Region, PLN],
      rng: Random,
  )(using p: SimParams): Household.State =
    hh.status match
      case HhStatus.Unemployed(_) => tryRelocate(hh, regionalWages, rng)
      case _                      => hh

  /** Attempt relocation for an unemployed household. Finds the best target
    * region (highest migration probability), then rolls the dice to decide
    * whether to move.
    */
  private def tryRelocate(
      hh: Household.State,
      regionalWages: Map[Region, PLN],
      rng: Random,
  )(using p: SimParams): Household.State =
    val currentWage = regionalWages.getOrElse(hh.region, PLN.Zero)
    findBestTarget(hh.region, currentWage, regionalWages) match
      case Some((target, prob)) =>
        val effectiveProb = prob * p.regional.baseMigrationRate
        if effectiveProb.sampleBelow(rng) then hh.copy(region = target)
        else hh
      case None                 => hh

  /** Find the destination region with highest migration probability. Returns
    * None if no viable target exists (all blocked by housing barrier or wage
    * differential).
    */
  private def findBestTarget(
      origin: Region,
      originWage: PLN,
      regionalWages: Map[Region, PLN],
  )(using p: SimParams): Option[(Region, Share)] =
    Region.all
      .filter(_ != origin)
      .map(target => (target, migrationProb(origin, target, originWage, regionalWages)))
      .filter(_._2 > Share.Zero)
      .maxByOption(_._2.toLong)

  /** Compute migration probability from origin to a specific target. Wraps
    * Region.migrationProbability with wage ratio computation.
    */
  private def migrationProb(
      origin: Region,
      target: Region,
      originWage: PLN,
      regionalWages: Map[Region, PLN],
  )(using p: SimParams): Share =
    val targetWage = regionalWages.getOrElse(target, PLN.Zero)
    val wageRatio  = if originWage > PLN.Zero then Multiplier(targetWage / originWage) else Multiplier.One
    Region.migrationProbability(origin, target, wageRatio, p.regional.housingBarrierThreshold)
