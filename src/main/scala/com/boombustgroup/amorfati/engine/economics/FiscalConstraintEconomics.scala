package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.agents.Banking
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.engine.ledger.LedgerFinancialState
import com.boombustgroup.amorfati.engine.mechanisms.YieldCurve
import com.boombustgroup.amorfati.types.*

/** Pure economic logic for fiscal constraints — no state mutation, no flows.
  *
  * Computes minimum wage indexation, reservation wage, and lending base rate.
  * Extracted from FiscalConstraintStep (Calculus vs Accounting split). This
  * step has no Accounting — it's 100% Calculus.
  */
object FiscalConstraintEconomics:

  private val ExpectationsBlendWeight = Share.decimal(5, 1)

  case class Output(
      month: ExecutionMonth,
      baseMinWage: PLN,
      updatedMinWagePriceLevel: PriceIndex,
      resWage: PLN,
      lendingBaseRate: Rate,
  ):
    def m: ExecutionMonth = month

  def compute(w: World, banks: Vector[Banking.BankState], ledgerFinancialState: LedgerFinancialState, month: ExecutionMonth)(using p: SimParams): Output =
    val bankAgg = Banking.aggregateFromBankStocks(banks, ledgerFinancialState.banks.map(LedgerFinancialState.projectBankFinancialStocks))

    val (baseMinWage, updatedMinWagePriceLevel) =
      val isAdjustMonth = month.toInt % p.fiscal.minWageAdjustMonths == 0
      if isAdjustMonth then
        val cumInfl     =
          if p.fiscal.minWageInflationIndex && w.gov.minWagePriceLevel > PriceIndex.Zero then
            (w.priceLevel.ratioTo(w.gov.minWagePriceLevel) - Scalar.One).max(Scalar.Zero)
          else Scalar.Zero
        val inflIndexed = w.gov.minWageLevel * cumInfl.toCoefficient.growthMultiplier
        val target      = w.householdMarket.marketWage * p.fiscal.minWageTargetRatio
        val gap         = target - inflIndexed
        val adjusted    =
          if gap > PLN.Zero then inflIndexed + gap * p.fiscal.minWageConvergenceSpeed
          else inflIndexed
        (w.gov.minWageLevel.max(adjusted), w.priceLevel)
      else (w.gov.minWageLevel, w.gov.minWagePriceLevel)

    val resWage = baseMinWage

    val rawLendingBaseRate =
      val exp = w.mechanisms.expectations
      YieldCurve
        .compute(w.bankingSector.interbankRate, bankAgg.nplRatio, exp.credibility, exp.expectedInflation, p.monetary.targetInfl)
        .wibor3m

    val lendingBaseRate =
      rawLendingBaseRate * ExpectationsBlendWeight + w.mechanisms.expectations.expectedRate * (Share.One - ExpectationsBlendWeight)

    Output(month, baseMinWage, updatedMinWagePriceLevel, resWage, lendingBaseRate)
