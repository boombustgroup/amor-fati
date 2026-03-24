package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.steps.*
import com.boombustgroup.amorfati.types.*

import scala.util.Random

/** Pure economic results from firm processing — no state mutation, no flows.
  *
  * Wraps FirmProcessingStep.run() and extracts the aggregate monetary values
  * needed by flow mechanisms. The per-firm state updates (tech, debt, etc.)
  * stay in the step — they're StateUpdates, not monetary flows.
  *
  * Extracted from FirmProcessingStep (Calculus vs Accounting split).
  */
object FirmEconomics:

  case class Result(
      // Aggregate monetary flows
      tax: PLN,
      capex: PLN,
      newLoans: PLN,
      equityIssuance: PLN,
      grossInvestment: PLN,
      bondIssuance: PLN,
      actualBondIssuance: PLN,
      profitShifting: PLN,
      fdiRepatriation: PLN,
      ioPayments: PLN,
      nplLoss: PLN,
      intIncome: PLN,
      firmPrincipal: PLN,
      greenInvestment: PLN,
      energyCost: PLN,
      // Non-monetary
      firmDeaths: Int,
      nplNew: PLN,
      totalBondDefault: PLN,
      corpBondAbsorption: Share,
  )

  /** Run firm processing via old step, extract economics. */
  def compute(
      w: com.boombustgroup.amorfati.engine.World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      s1: FiscalConstraintStep.Output,
      s2: LaborDemographicsStep.Output,
      s3: HouseholdIncomeStep.Output,
      s4: DemandStep.Output,
      rng: Random,
  )(using SimParams): Result =
    val s5 = FirmProcessingStep.run(FirmProcessingStep.Input(w, firms, households, s1, s2, s3, s4), rng)
    Result(
      tax = s5.sumTax,
      capex = s5.sumCapex,
      newLoans = s5.sumNewLoans,
      equityIssuance = s5.sumEquityIssuance,
      grossInvestment = s5.sumGrossInvestment,
      bondIssuance = s5.sumBondIssuance,
      actualBondIssuance = s5.actualBondIssuance,
      profitShifting = s5.sumProfitShifting,
      fdiRepatriation = s5.sumFdiRepatriation,
      ioPayments = s5.totalIoPaid,
      nplLoss = s5.nplLoss,
      intIncome = s5.intIncome,
      firmPrincipal = s5.sumFirmPrincipal,
      greenInvestment = s5.sumGreenInvestment,
      energyCost = s5.sumEnergyCost,
      firmDeaths = s5.firmDeaths,
      nplNew = s5.nplNew,
      totalBondDefault = s5.totalBondDefault,
      corpBondAbsorption = s5.corpBondAbsorption,
    )
