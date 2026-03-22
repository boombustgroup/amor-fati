package com.boombustgroup.amorfati.engine.markets

import com.boombustgroup.amorfati.agents.Region
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

/** Regional labor market clearing: 6 independent Phillips curves (one per
  * NUTS-1 macroregion), producing per-region wages. National wage =
  * population-weighted aggregate (Kahan summation for numerical stability).
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
    * population-weighted mean of regional wages (Kahan summation).
    */
  def clear(
      prevRegionalWages: Map[Region, PLN],
      resWage: PLN,
      laborDemand: Int,
      totalPopulation: Int,
  )(using p: SimParams): Result =
    val regionalWages = Region.all
      .map: region =>
        val prevWage    = prevRegionalWages.getOrElse(region, resWage * region.wageMultiplier)
        val regDemand   = (laborDemand * region.populationShare.toDouble).toInt
        val regPop      = (totalPopulation * region.populationShare.toDouble).toInt.max(1)
        val regResWage  = resWage * region.wageMultiplier
        val clearResult = LaborMarket.updateLaborMarket(prevWage, regResWage, regDemand, regPop)
        region -> clearResult.wage
      .toMap

    val nationalWage = PLN(
      Region.all.kahanSumBy(r => regionalWages(r).toDouble * r.populationShare.toDouble),
    )

    Result(regionalWages, nationalWage)
