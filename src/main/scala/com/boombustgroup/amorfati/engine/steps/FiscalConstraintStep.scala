package com.boombustgroup.amorfati.engine.steps

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.engine.mechanisms.YieldCurve
import com.boombustgroup.amorfati.types.*

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
      m: Int,                           // current simulation month (1-based)
      baseMinWage: PLN,                 // inflation-indexed minimum wage level
      updatedMinWagePriceLevel: Double, // price level at last minimum wage adjustment
      resWage: PLN,                     // reservation wage (floor for labor market clearing)
      lendingBaseRate: Rate,            // blended base rate for bank lending (interbank + expectations)
  )

  @computationBoundary
  def run(in: Input)(using p: SimParams): Output =
    import ComputationBoundary.toDouble
    val w = in.w
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
        val yc  = YieldCurve
          .compute(
            w.bankingSector.interbankRate,
            nplRatio = w.bank.nplRatio,
            credibility = exp.credibility,
            expectedInflation = exp.expectedInflation,
            targetInflation = p.monetary.targetInfl,
          )
          .wibor3m
        toDouble(yc)
      else toDouble(w.nbp.referenceRate)

    val lendingBaseRate =
      if p.flags.expectations then
        ExpectationsBlendWeight * rawLendingBaseRate + (1.0 - ExpectationsBlendWeight) * toDouble(w.mechanisms.expectations.expectedRate)
      else rawLendingBaseRate

    Output(m, PLN(baseMinWage), updatedMinWagePriceLevel, PLN(resWage), Rate(lendingBaseRate))
