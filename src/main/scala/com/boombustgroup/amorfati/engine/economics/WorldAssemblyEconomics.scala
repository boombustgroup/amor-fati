package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.engine.steps.*
import com.boombustgroup.amorfati.types.*

import scala.util.Random

/** WorldAssembly economics: aggregation, informal economy, observables.
  *
  * Own Input takes raw values where possible, Step.Output types where
  * unavoidable. Returns assembled World + updated agents.
  *
  * Full decoupling happens when World is replaced by MutableWorldState (#131).
  */
object WorldAssemblyEconomics:

  case class Input(
      w: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      // Raw values
      month: Int,
      lendingBaseRate: Rate,
      resWage: PLN,
      baseMinWage: PLN,
      minWagePriceLevel: Double,
      govPurchases: PLN,
      sectorMults: Vector[Double],
      avgDemandMult: Double,
      sectorCap: Vector[Double],
      laggedInvestDemand: PLN,
      fiscalRuleStatus: com.boombustgroup.amorfati.engine.markets.FiscalRules.RuleStatus,
      // Step outputs (too complex to decompose)
      laborOutput: LaborDemographicsStep.Output,
      hhOutput: HouseholdIncomeStep.Output,
      firmOutput: FirmProcessingStep.Output,
      hhFinancialOutput: HouseholdFinancialStep.Output,
      priceEquityOutput: PriceEquityStep.Output,
      openEconOutput: OpenEconomyStep.Output,
      bankOutput: BankUpdateStep.Output,
      rng: Random,
      migRng: Random,
  )

  case class Result(
      world: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
  )

  def compute(in: Input)(using SimParams): Result =
    val s1 = FiscalConstraintStep.Output(in.month, in.baseMinWage, in.minWagePriceLevel, in.resWage, in.lendingBaseRate)
    val s4 = DemandStep.Output(in.govPurchases, in.sectorMults, in.avgDemandMult, in.sectorCap, in.laggedInvestDemand, in.fiscalRuleStatus)

    val s10 = WorldAssemblyStep.run(
      WorldAssemblyStep.Input(
        in.w,
        in.firms,
        in.households,
        s1,
        in.laborOutput,
        in.hhOutput,
        s4,
        in.firmOutput,
        in.hhFinancialOutput,
        in.priceEquityOutput,
        in.openEconOutput,
        in.bankOutput,
      ),
      in.rng,
      in.migRng,
    )

    Result(s10.newWorld, s10.finalFirms, s10.reassignedHouseholds)
