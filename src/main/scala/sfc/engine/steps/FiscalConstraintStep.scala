package sfc.engine.steps

import sfc.config.{RunConfig, SimParams}
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
      baseMinWage: Double,
      updatedMinWagePriceLevel: Double,
      resWage: Double,
      lendingBaseRate: Double,
  )

  def run(in: Input)(using p: SimParams): Output =
    val w = in.w
    val m = w.month + 1

    val (baseMinWage, updatedMinWagePriceLevel) = if p.flags.minWage then
      val isAdjustMonth = m > 0 && m % p.fiscal.minWageAdjustMonths == 0
      if isAdjustMonth then
        val cumInfl     =
          if p.fiscal.minWageInflationIndex && w.hh.minWagePriceLevel > 0 then w.priceLevel / w.hh.minWagePriceLevel - 1.0
          else 0.0
        val inflIndexed = w.hh.minWageLevel.toDouble * (1.0 + Math.max(0.0, cumInfl))
        val target      = w.hh.marketWage.toDouble * p.fiscal.minWageTargetRatio.toDouble
        val gap         = target - inflIndexed
        val adjusted    =
          if gap > 0 then inflIndexed + gap * p.fiscal.minWageConvergenceSpeed.toDouble
          else inflIndexed
        (Math.max(w.hh.minWageLevel.toDouble, adjusted), w.priceLevel)
      else (w.hh.minWageLevel.toDouble, w.hh.minWagePriceLevel)
    else (p.household.baseReservationWage.toDouble, w.hh.minWagePriceLevel)

    val resWage = baseMinWage

    val rawLendingBaseRate: Double =
      if p.flags.interbankTermStructure then YieldCurve.compute(w.bankingSector.interbankRate.toDouble).wibor3m.toDouble
      else w.nbp.referenceRate.toDouble

    val lendingBaseRate =
      if p.flags.expectations then 0.5 * rawLendingBaseRate + 0.5 * w.expectations.expectedRate.toDouble
      else rawLendingBaseRate

    Output(m, baseMinWage, updatedMinWagePriceLevel, resWage, lendingBaseRate)
