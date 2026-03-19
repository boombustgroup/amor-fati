package com.boombustgroup.amorfati.engine.markets

import com.boombustgroup.amorfati.agents.{Firm, Household, Region}
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.amorfati.util.KahanSum.*

/** Per-region labor market clearing with geographic friction.
  *
  * Replaces single national clearing with 6 independent NUTS-1 clearings. Each
  * region has its own wage (Phillips curve × regional multiplier), employment
  * count, and labor supply. Firms hire from their own region; cross-region
  * matching is possible but penalized by friction.
  *
  * National wage and employment are population-weighted aggregates of regional
  * outcomes — not inputs to clearing.
  *
  * Pure functions, no mutable state.
  *
  * Calibration: GUS BAEL 2024 (regional unemployment), GUS average wages by
  * voivodeship 2024.
  */
object RegionalClearing:

  /** Per-region clearing result. */
  case class RegionalResult(
      region: Region,
      wage: PLN,
      employed: Int,
      laborDemand: Int,
      population: Int,
  ):
    def unemploymentRate: Ratio =
      if population > 0 then Ratio(1.0 - employed.toDouble / population) else Ratio.Zero

  /** Aggregate result across all regions. */
  case class Result(
      regionalResults: Vector[RegionalResult],
      nationalWage: PLN,
      nationalEmployed: Int,
      nationalLaborDemand: Int,
  )

  /** Run per-region labor clearing and aggregate.
    *
    * Groups firms and HH by region once, then runs Phillips curve per region.
    * National wage = population-weighted average (Kahan summation).
    */
  def clear(
      prevNationalWage: PLN,
      resWage: PLN,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
  )(using p: SimParams): Result =
    val livingFirms   = firms.filter(Firm.isAlive)
    val firmsByRegion = livingFirms.groupBy(_.region)
    val hhByRegion    = households.groupBy(_.region)

    val regional = Region.all.map: region =>
      clearRegion(
        region,
        prevNationalWage,
        resWage,
        firmsByRegion.getOrElse(region, Vector.empty),
        hhByRegion.getOrElse(region, Vector.empty),
      )

    val totalPop     = regional.map(_.population).sum
    val nationalWage =
      if totalPop > 0 then PLN(regional.kahanSumBy(r => r.wage.toDouble * r.population) / totalPop)
      else prevNationalWage

    Result(
      regionalResults = regional,
      nationalWage = nationalWage,
      nationalEmployed = regional.map(_.employed).sum,
      nationalLaborDemand = regional.map(_.laborDemand).sum,
    )

  /** Clear a single region's labor market.
    *
    * Regional wage = Phillips curve on regional labor demand/supply, anchored
    * to national wage × regional multiplier.
    */
  private def clearRegion(
      region: Region,
      prevNationalWage: PLN,
      resWage: PLN,
      regionFirms: Vector[Firm.State],
      regionHH: Vector[Household.State],
  )(using p: SimParams): RegionalResult =
    val regionPop    = regionHH.length
    val regionDemand = regionFirms.map(Firm.workerCount).sum

    val regionalPrevWage = prevNationalWage * region.wageMultiplier
    val regionalResWage  = resWage * region.wageMultiplier
    val popForClearing   = if regionPop > 0 then regionPop else 1

    val result = LaborMarket.updateLaborMarket(regionalPrevWage, regionalResWage, regionDemand, popForClearing)

    RegionalResult(region, result.wage, result.employed, regionDemand, regionPop)
