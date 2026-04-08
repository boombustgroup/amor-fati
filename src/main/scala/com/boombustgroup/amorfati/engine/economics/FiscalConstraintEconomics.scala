package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.agents.Banking
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
      updatedMinWagePriceLevel: PriceIndex,
      resWage: PLN,
      lendingBaseRate: Rate,
  )

  /** Bridge type — same fields as the deleted FiscalConstraintStep.Output. */
  case class Output(
      m: Int,
      baseMinWage: PLN,
      updatedMinWagePriceLevel: PriceIndex,
      resWage: PLN,
      lendingBaseRate: Rate,
  )

  def toOutput(r: Result): Output =
    Output(r.month, r.baseMinWage, r.updatedMinWagePriceLevel, r.resWage, r.lendingBaseRate)

  @boundaryEscape
  def compute(w: World, banks: Vector[Banking.BankState])(using p: SimParams): Result =
    import ComputationBoundary.toDouble
    val m       = w.month + 1
    val bankAgg = Banking.aggregateFromBanks(banks)

    val (baseMinWage, updatedMinWagePriceLevel) =
      val isAdjustMonth = m > 0 && m % p.fiscal.minWageAdjustMonths == 0
      if isAdjustMonth then
        val cumInfl     =
          if p.fiscal.minWageInflationIndex && w.gov.minWagePriceLevel > PriceIndex.Zero then toDouble(w.priceLevel) / toDouble(w.gov.minWagePriceLevel) - 1.0
          else 0.0
        val inflIndexed = toDouble(w.gov.minWageLevel) * (1.0 + Math.max(0.0, cumInfl))
        val target      = toDouble(w.householdMarket.marketWage) * toDouble(p.fiscal.minWageTargetRatio)
        val gap         = target - inflIndexed
        val adjusted    =
          if gap > 0 then inflIndexed + gap * toDouble(p.fiscal.minWageConvergenceSpeed)
          else inflIndexed
        (Math.max(toDouble(w.gov.minWageLevel), adjusted), w.priceLevel)
      else (toDouble(w.gov.minWageLevel), w.gov.minWagePriceLevel)

    val resWage = baseMinWage

    val rawLendingBaseRate: Double =
      val exp = w.mechanisms.expectations
      toDouble(
        YieldCurve
          .compute(w.bankingSector.interbankRate, bankAgg.nplRatio, exp.credibility, exp.expectedInflation, p.monetary.targetInfl)
          .wibor3m,
      )

    val lendingBaseRate =
      ExpectationsBlendWeight * rawLendingBaseRate + (1.0 - ExpectationsBlendWeight) * toDouble(w.mechanisms.expectations.expectedRate)

    Result(m, PLN(baseMinWage), updatedMinWagePriceLevel, PLN(resWage), Rate(lendingBaseRate))
