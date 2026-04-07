package com.boombustgroup.amorfati.engine.markets

import com.boombustgroup.amorfati.agents.Region
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

/** Regional labor market clearing: 6 independent Phillips curves (one per
  * NUTS-1 macroregion), producing per-region wages. National wage =
  * population-weighted aggregate (exact Long summation for numerical
  * stability).
  *
  * Each region clears its own labor market using the same Phillips curve
  * mechanism as the national model, but with regional wage multipliers and base
  * unemployment rates.
  */
object RegionalClearing:

  case class Result(
      regionalWages: Map[Region, PLN], // per-region market-clearing wage
      nationalWage: PLN,               // population-weighted aggregate wage
  )

  /** Clear 6 regional labor markets and produce a national aggregate wage.
    *
    * Each region's wage = national base wage × regional multiplier, adjusted by
    * regional excess demand via the Phillips curve. The national wage is the
    * population-weighted mean of regional wages (exact Long summation).
    */
  def clear(
      prevRegionalWages: Map[Region, PLN],
      resWage: PLN,
      laborDemand: Int,
      totalPopulation: Int,
  )(using p: SimParams): Result =
    val regionalWages: Map[Region, PLN] = Region.all
      .map: region =>
        val regWageMult: Multiplier = Region.normalizedWageMultiplier(region)
        val prevWage: PLN           = prevRegionalWages.getOrElse(region, resWage * regWageMult)
        val regDemand: Int          = region.populationShare.applyTo(laborDemand)
        val regPop: Int             = region.populationShare.applyTo(totalPopulation).max(1)
        val regResWage: PLN         = resWage * regWageMult
        val clearResult             = LaborMarket.updateLaborMarket(prevWage, regResWage, regDemand, regPop)
        region -> clearResult.wage
      .toMap

    val nationalWage: PLN =
      Region.all.foldLeft(PLN.Zero): (acc, region) =>
        acc + (regionalWages(region) * region.populationShare)

    Result(regionalWages, nationalWage)
