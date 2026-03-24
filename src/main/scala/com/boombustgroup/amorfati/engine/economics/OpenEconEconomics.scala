package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.steps.*
import com.boombustgroup.amorfati.types.*

import scala.util.Random

/** Pure economic results from open economy — no state mutation, no flows.
  *
  * Wraps OpenEconomyStep.run() and extracts aggregate BoP values needed by flow
  * mechanisms. Exchange rate model, trade, FDI, portfolio flows.
  */
object OpenEconEconomics:

  case class Result(
      exports: PLN,
      totalImports: PLN,
      tourismExport: PLN,
      tourismImport: PLN,
      fdi: PLN,
      portfolioFlows: PLN,
      primaryIncome: PLN,
      euFunds: PLN,
      diasporaInflow: PLN,
  )

  def compute(
      w: com.boombustgroup.amorfati.engine.World,
      s1: FiscalConstraintStep.Output,
      s2: LaborDemographicsStep.Output,
      s3: HouseholdIncomeStep.Output,
      s4: DemandStep.Output,
      s5: FirmProcessingStep.Output,
      s6: HouseholdFinancialStep.Output,
      s7: PriceEquityStep.Output,
      rng: Random,
  )(using SimParams): Result =
    val s8 = OpenEconomyStep.run(OpenEconomyStep.Input(w, s1, s2, s3, s4, s5, s6, s7, rng))
    Result(
      exports = s8.external.newBop.exports,
      totalImports = s8.external.newBop.totalImports,
      tourismExport = s6.tourismExport,
      tourismImport = s6.tourismImport,
      fdi = s8.external.newBop.fdi,
      portfolioFlows = s8.external.newBop.portfolioFlows,
      primaryIncome = s8.external.newBop.primaryIncome,
      euFunds = s8.external.newBop.euFundsMonthly,
      diasporaInflow = s6.diasporaInflow,
    )
