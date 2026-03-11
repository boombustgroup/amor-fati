package sfc.engine.steps

import sfc.config.SimParams
import sfc.engine.World
import sfc.engine.mechanisms.YieldCurve
import sfc.types.*

/** Fiscal constraint initialization: minimum wage indexation, reservation wage,
  * and lending base rate computation. The lending base rate blends the
  * interbank term structure (or reference rate) with expected rates when the
  * expectations mechanism is active, smoothing out monetary policy
  * transmission.
  */
object FiscalConstraintStep:

  // ---- Calibration constants ----
  private val ExpectationsBlendWeight = 0.5 // weight on raw vs expected rate in lending base rate blend

  case class Input(w: World) // current world state

  case class Output(
      m: Int,
      baseMinWage: PLN,
      updatedMinWagePriceLevel: Double,
      resWage: PLN,
      lendingBaseRate: Rate,
  )

  def run(in: Input)(using p: SimParams): Output =
    val w = in.w
    val m = w.month + 1

    val (baseMinWage, updatedMinWagePriceLevel) = if p.flags.minWage then
      val isAdjustMonth = m > 0 && m % p.fiscal.minWageAdjustMonths == 0
      if isAdjustMonth then
        val cumInfl     =
          if p.fiscal.minWageInflationIndex && w.gov.minWagePriceLevel > 0 then w.priceLevel / w.gov.minWagePriceLevel - 1.0
          else 0.0
        val inflIndexed = w.gov.minWageLevel.toDouble * (1.0 + Math.max(0.0, cumInfl))
        val target      = w.hhAgg.marketWage.toDouble * p.fiscal.minWageTargetRatio.toDouble
        val gap         = target - inflIndexed
        val adjusted    =
          if gap > 0 then inflIndexed + gap * p.fiscal.minWageConvergenceSpeed.toDouble
          else inflIndexed
        (Math.max(w.gov.minWageLevel.toDouble, adjusted), w.priceLevel)
      else (w.gov.minWageLevel.toDouble, w.gov.minWagePriceLevel)
    else (p.household.baseReservationWage.toDouble, w.gov.minWagePriceLevel)

    val resWage = baseMinWage

    val rawLendingBaseRate: Double =
      if p.flags.interbankTermStructure then YieldCurve.compute(w.bankingSector.interbankRate.toDouble).wibor3m.toDouble
      else w.nbp.referenceRate.toDouble

    val lendingBaseRate =
      if p.flags.expectations then
        ExpectationsBlendWeight * rawLendingBaseRate + (1.0 - ExpectationsBlendWeight) * w.mechanisms.expectations.expectedRate.toDouble
      else rawLendingBaseRate

    Output(m, PLN(baseMinWage), updatedMinWagePriceLevel, PLN(resWage), Rate(lendingBaseRate))
