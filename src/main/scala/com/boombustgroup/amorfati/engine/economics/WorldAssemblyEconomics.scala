package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.engine.steps.*

import scala.util.Random

/** WorldAssembly economics: aggregation, informal economy, observables.
  *
  * Wraps WorldAssemblyStep.run() and returns the assembled World. In the new
  * pipeline, this becomes the final stage that converts MutableWorldState back
  * to World for output/SFC validation.
  */
object WorldAssemblyEconomics:

  case class Result(
      world: World,
      firms: Vector[com.boombustgroup.amorfati.agents.Firm.State],
      households: Vector[com.boombustgroup.amorfati.agents.Household.State],
  )

  def compute(
      w: World,
      firms: Vector[com.boombustgroup.amorfati.agents.Firm.State],
      households: Vector[com.boombustgroup.amorfati.agents.Household.State],
      s1: FiscalConstraintStep.Output,
      s2: LaborDemographicsStep.Output,
      s3: HouseholdIncomeStep.Output,
      s4: DemandStep.Output,
      s5: FirmProcessingStep.Output,
      s6: HouseholdFinancialStep.Output,
      s7: PriceEquityStep.Output,
      s8: OpenEconomyStep.Output,
      s9: BankUpdateStep.Output,
      rng: Random,
      migRng: Random,
  )(using SimParams): Result =
    val s10 = WorldAssemblyStep.run(WorldAssemblyStep.Input(w, firms, households, s1, s2, s3, s4, s5, s6, s7, s8, s9), rng, migRng)
    Result(s10.newWorld, s10.finalFirms, s10.reassignedHouseholds)
