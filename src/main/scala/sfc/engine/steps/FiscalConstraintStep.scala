package sfc.engine.steps

import sfc.config.{Config, RunConfig}
import sfc.engine.World
import sfc.engine.mechanisms.YieldCurve
import sfc.types.*

object FiscalConstraintStep:

  case class Input(
    w: World,
    rc: RunConfig,
  )

  case class Output(
    m: Int,
    bdpActive: Boolean,
    bdp: Double,
    baseMinWage: Double,
    updatedMinWagePriceLevel: Double,
    resWage: Double,
    lendingBaseRate: Double,
  )

  def run(in: Input): Output =
    val w = in.w
    val m = w.month + 1
    val bdpActive = m >= Config.ShockMonth

    val bdp = if bdpActive then in.rc.bdpAmount else 0.0

    val (baseMinWage, updatedMinWagePriceLevel) = if Config.MinWageEnabled then
      val isAdjustMonth = m > 0 && m % Config.MinWageAdjustMonths == 0
      if isAdjustMonth then
        val cumInfl =
          if Config.MinWageInflationIndex && w.hh.minWagePriceLevel > 0 then w.priceLevel / w.hh.minWagePriceLevel - 1.0
          else 0.0
        val inflIndexed = w.hh.minWageLevel.toDouble * (1.0 + Math.max(0.0, cumInfl))
        val target = w.hh.marketWage.toDouble * Config.MinWageTargetRatio
        val gap = target - inflIndexed
        val adjusted =
          if gap > 0 then inflIndexed + gap * Config.MinWageConvergenceSpeed
          else inflIndexed
        (Math.max(w.hh.minWageLevel.toDouble, adjusted), w.priceLevel)
      else (w.hh.minWageLevel.toDouble, w.hh.minWagePriceLevel)
    else (Config.BaseReservationWage, w.hh.minWagePriceLevel)

    val resWage = baseMinWage + bdp * Config.ReservationBdpMult

    val rawLendingBaseRate: Double =
      if Config.InterbankTermStructure then YieldCurve.compute(w.bankingSector.interbankRate.toDouble).wibor3m.toDouble
      else w.nbp.referenceRate.toDouble

    val lendingBaseRate =
      if Config.ExpEnabled then 0.5 * rawLendingBaseRate + 0.5 * w.expectations.expectedRate.toDouble
      else rawLendingBaseRate

    Output(m, bdpActive, bdp, baseMinWage, updatedMinWagePriceLevel, resWage, lendingBaseRate)
