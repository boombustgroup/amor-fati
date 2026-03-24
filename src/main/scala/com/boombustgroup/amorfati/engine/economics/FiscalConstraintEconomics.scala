package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.engine.mechanisms.YieldCurve
import com.boombustgroup.amorfati.types.*

/** Pure economic logic for fiscal constraints — no state mutation, no flows.
  *
  * Computes minimum wage indexation, reservation wage, and lending base rate.
  * Extracted from FiscalConstraintStep (Calculus vs Accounting split). This
  * step has no Accounting — it's 100% Calculus.
  */
object FiscalConstraintEconomics:

  private val ExpectationsBlendWeight = 0.5

  case class Result(
      month: Int,
      baseMinWage: PLN,
      updatedMinWagePriceLevel: Double,
      resWage: PLN,
      lendingBaseRate: Rate,
  )

  @boundaryEscape
  def compute(w: World)(using p: SimParams): Result =
    import ComputationBoundary.toDouble
    val m = w.month + 1

    val (baseMinWage, updatedMinWagePriceLevel) = if p.flags.minWage then
      val isAdjustMonth = m > 0 && m % p.fiscal.minWageAdjustMonths == 0
      if isAdjustMonth then
        val cumInfl     =
          if p.fiscal.minWageInflationIndex && w.gov.minWagePriceLevel > 0 then w.priceLevel / w.gov.minWagePriceLevel - 1.0
          else 0.0
        val inflIndexed = toDouble(w.gov.minWageLevel) * (1.0 + Math.max(0.0, cumInfl))
        val target      = toDouble(w.hhAgg.marketWage) * toDouble(p.fiscal.minWageTargetRatio)
        val gap         = target - inflIndexed
        val adjusted    =
          if gap > 0 then inflIndexed + gap * toDouble(p.fiscal.minWageConvergenceSpeed)
          else inflIndexed
        (Math.max(toDouble(w.gov.minWageLevel), adjusted), w.priceLevel)
      else (toDouble(w.gov.minWageLevel), w.gov.minWagePriceLevel)
    else (toDouble(p.household.baseReservationWage), w.gov.minWagePriceLevel)

    val resWage = baseMinWage

    val rawLendingBaseRate: Double =
      if p.flags.interbankTermStructure then
        val exp = w.mechanisms.expectations
        toDouble(
          YieldCurve
            .compute(w.bankingSector.interbankRate, w.bank.nplRatio, exp.credibility, exp.expectedInflation, p.monetary.targetInfl)
            .wibor3m,
        )
      else toDouble(w.nbp.referenceRate)

    val lendingBaseRate =
      if p.flags.expectations then
        ExpectationsBlendWeight * rawLendingBaseRate + (1.0 - ExpectationsBlendWeight) * toDouble(w.mechanisms.expectations.expectedRate)
      else rawLendingBaseRate

    Result(m, PLN(baseMinWage), updatedMinWagePriceLevel, PLN(resWage), Rate(lendingBaseRate))
