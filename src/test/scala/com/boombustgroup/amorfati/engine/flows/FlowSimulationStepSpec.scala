package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.accounting.Sfc
import com.boombustgroup.amorfati.agents.Household
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.{DecisionSignals, MonthTimingEnvelopeKey, MonthTimingPayload, MonthTraceStage}
import com.boombustgroup.amorfati.init.WorldInit
import com.boombustgroup.amorfati.tags.Heavy
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.{ImperativeInterpreter, Interpreter}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

@Heavy
class FlowSimulationStepSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  private def canonicalHouseholds(households: Vector[Household.State]): Vector[Vector[Any]] =
    households.map: hh =>
      hh.productIterator
        .map:
          case arr: Array[?] => arr.toIndexedSeq
          case value         => value
        .toVector

  "FlowSimulation.step" should "produce SFC == 0L on real World" in {
    val init   = WorldInit.initialize(42L)
    val state  = FlowSimulation.SimState.fromInit(init)
    val rng    = new scala.util.Random(42)
    val result = FlowSimulation.step(state, rng)
    result.execution.totalWealth shouldBe 0L
    result.sfcResult shouldBe Right(())
    result.calculus.employed should be > 0
  }

  it should "expose the month boundary as SimState -> StepOutput -> (trace, nextState)" in {
    val init        = WorldInit.initialize(42L)
    val state       = FlowSimulation.SimState.fromInit(init)
    val stateRng    = new scala.util.Random(42L)
    val rerunRng    = new scala.util.Random(42L)
    val stateResult = FlowSimulation.step(state, stateRng)
    val repeated    = FlowSimulation.step(state, rerunRng)

    stateResult.stateIn shouldBe state
    stateResult.transition shouldBe ((stateResult.trace, stateResult.nextState))
    stateResult.nextState.world shouldBe repeated.nextState.world
    stateResult.nextState.firms shouldBe repeated.nextState.firms
    canonicalHouseholds(stateResult.nextState.households) shouldBe canonicalHouseholds(repeated.nextState.households)
    stateResult.nextState.banks shouldBe repeated.nextState.banks
    stateResult.nextState.householdAggregates shouldBe repeated.nextState.householdAggregates
    stateResult.trace shouldBe repeated.trace
  }

  it should "produce SFC == 0L across 12 months (autonomous driving)" in {
    val init  = WorldInit.initialize(42L)
    var state = FlowSimulation.SimState.fromInit(init)

    (1 to 12).foreach { month =>
      val rng    = new scala.util.Random(42L * 1000 + month)
      val result = FlowSimulation.step(state, rng)
      withClue(s"Month $month: ") {
        result.execution.totalWealth shouldBe 0L
        result.sfcResult shouldBe Right(())
      }
      state = result.nextState
    }
  }

  it should "propagate informal-economy pressure into fiscal outputs without breaking SFC" in {
    val init          = WorldInit.initialize(42L)
    val lowShadowW    = init.world.copy(mechanisms = init.world.mechanisms.copy(informalCyclicalAdj = 0.0))
    val highShadowW   = init.world.copy(mechanisms = init.world.mechanisms.copy(informalCyclicalAdj = 0.4))
    val lowShadowRun  = FlowSimulation.step(
      FlowSimulation.SimState(lowShadowW, init.firms, init.households, init.banks, init.householdAggregates),
      new scala.util.Random(42L),
    )
    val highShadowRun = FlowSimulation.step(
      FlowSimulation.SimState(highShadowW, init.firms, init.households, init.banks, init.householdAggregates),
      new scala.util.Random(42L),
    )

    lowShadowRun.execution.totalWealth shouldBe 0L
    highShadowRun.execution.totalWealth shouldBe 0L
    lowShadowRun.sfcResult shouldBe Right(())
    highShadowRun.sfcResult shouldBe Right(())

    highShadowRun.nextState.world.flows.realizedTaxShadowShare should be > lowShadowRun.nextState.world.flows.realizedTaxShadowShare
    highShadowRun.nextState.world.flows.taxEvasionLoss should be > lowShadowRun.nextState.world.flows.taxEvasionLoss
    highShadowRun.nextState.world.gov.taxRevenue should be < lowShadowRun.nextState.world.gov.taxRevenue
    highShadowRun.nextState.world.gov.deficit should be > lowShadowRun.nextState.world.gov.deficit
  }

  it should "produce 30+ mechanism IDs" in {
    val init   = WorldInit.initialize(42L)
    val state  = FlowSimulation.SimState.fromInit(init)
    val rng    = new scala.util.Random(42)
    val result = FlowSimulation.step(state, rng)
    result.flows.map(_.mechanism).toSet.size should be > 30
  }

  it should "emit a typed MonthTrace with stable boundaries and typed timing envelopes" in {
    val init   = WorldInit.initialize(42L)
    val state  = FlowSimulation.SimState.fromInit(init)
    val rng    = new scala.util.Random(42L)
    val result = FlowSimulation.step(state, rng)
    val trace  = result.trace

    val demandSignals = trace.timing.requirePayload[MonthTimingPayload.DemandSignals](MonthTimingEnvelopeKey.DemandSignals)

    trace.month shouldBe result.nextState.world.month
    trace.seedTransition.seedIn shouldBe init.world.seedIn
    trace.seedTransition.seedOut shouldBe result.nextState.world.seedIn
    trace.boundary.startSnapshot.stock shouldBe Sfc.snapshot(init.world, init.firms, init.households, init.banks)
    trace.boundary.endSnapshot.stock shouldBe Sfc.snapshot(
      result.nextState.world,
      result.nextState.firms,
      result.nextState.households,
      result.nextState.banks,
    )
    trace.boundary.startSnapshot.inflation shouldBe init.world.inflation
    trace.boundary.endSnapshot.inflation shouldBe result.nextState.world.inflation
    trace.timing.envelopes.map(_.key).toSet shouldBe Set(
      MonthTimingEnvelopeKey.LaborSignals,
      MonthTimingEnvelopeKey.DemandSignals,
      MonthTimingEnvelopeKey.NominalSignals,
      MonthTimingEnvelopeKey.FirmDynamics,
    )
    trace.timing.laborSignals.operationalHiringSlack shouldBe result.nextState.world.pipeline.operationalHiringSlack
    demandSignals.sectorDemandMult shouldBe result.nextState.world.seedIn.sectorDemandMult
    demandSignals.sectorDemandPressure shouldBe result.nextState.world.seedIn.sectorDemandPressure
    demandSignals.sectorHiringSignal shouldBe result.nextState.world.seedIn.sectorHiringSignal
    trace.timing.nominalSignals.realizedInflation shouldBe result.nextState.world.inflation
    trace.timing.nominalSignals.expectedInflation shouldBe result.nextState.world.mechanisms.expectations.expectedInflation
    trace.timing.firmDynamics.startupAbsorptionRate shouldBe result.nextState.world.seedIn.startupAbsorptionRate
    trace.timing.firmDynamics.firmBirths shouldBe result.nextState.world.flows.firmBirths
    trace.timing.firmDynamics.firmDeaths shouldBe result.nextState.world.flows.firmDeaths
    trace.timing.firmDynamics.netFirmBirths shouldBe result.nextState.world.flows.netFirmBirths
    trace.executedFlows.totalIncome shouldBe result.calculus.totalIncome
    trace.executedFlows.currentAccount shouldBe result.nextState.world.bop.currentAccount
    trace.executedFlows.fofResidual shouldBe result.nextState.world.plumbing.fofResidual
    trace.seedTransition.provenance.unemploymentRate.stage shouldBe MonthTraceStage.WorldAssemblyEconomics
    trace.seedTransition.provenance.inflation.stage shouldBe MonthTraceStage.PriceEquityEconomics
    trace.seedTransition.provenance.expectedInflation.stage shouldBe MonthTraceStage.OpenEconEconomics
    trace.seedTransition.provenance.laggedHiringSlack.stage shouldBe MonthTraceStage.LaborEconomics
    trace.seedTransition.provenance.startupAbsorptionRate.stage shouldBe MonthTraceStage.StartupStaffing
    trace.seedTransition.provenance.sectorDemandMult.stage shouldBe MonthTraceStage.DemandEconomics
    trace.seedTransition.provenance.sectorDemandPressure.stage shouldBe MonthTraceStage.DemandEconomics
    trace.seedTransition.provenance.sectorHiringSignal.stage shouldBe MonthTraceStage.DemandEconomics
    trace.seedTransition.provenance.laggedHiringSlack.value shouldBe result.nextState.world.seedIn.laggedHiringSlack
    trace.seedTransition.provenance.startupAbsorptionRate.value shouldBe result.nextState.world.seedIn.startupAbsorptionRate
    trace.validations should have size 1
    trace.validations.head.passed shouldBe true
    trace.validations.head.failures shouldBe empty
    result.sfcResult shouldBe Right(())
  }

  it should "keep MonthTrace seed transitions consistent with timing envelopes and end-of-month boundary data" in {
    val init   = WorldInit.initialize(42L)
    val state  = FlowSimulation.SimState.fromInit(init)
    val rng    = new scala.util.Random(42L)
    val result = FlowSimulation.step(state, rng)
    val trace  = result.trace

    trace.seedTransition.seedOut shouldBe DecisionSignals(
      unemploymentRate = trace.boundary.endSnapshot.unemploymentRate,
      inflation = trace.timing.nominalSignals.realizedInflation,
      expectedInflation = trace.timing.nominalSignals.expectedInflation,
      laggedHiringSlack = trace.timing.laborSignals.operationalHiringSlack,
      startupAbsorptionRate = trace.timing.firmDynamics.startupAbsorptionRate,
      sectorDemandMult = trace.timing.demandSignals.sectorDemandMult,
      sectorDemandPressure = trace.timing.demandSignals.sectorDemandPressure,
      sectorHiringSignal = trace.timing.demandSignals.sectorHiringSignal,
    )
    trace.seedTransition.provenance.unemploymentRate.value shouldBe trace.boundary.endSnapshot.unemploymentRate
    trace.seedTransition.provenance.inflation.value shouldBe trace.timing.nominalSignals.realizedInflation
    trace.seedTransition.provenance.expectedInflation.value shouldBe trace.timing.nominalSignals.expectedInflation
    trace.seedTransition.provenance.laggedHiringSlack.value shouldBe trace.timing.laborSignals.operationalHiringSlack
    trace.seedTransition.provenance.startupAbsorptionRate.value shouldBe trace.timing.firmDynamics.startupAbsorptionRate
    trace.seedTransition.provenance.sectorDemandMult.value shouldBe trace.timing.demandSignals.sectorDemandMult
    trace.seedTransition.provenance.sectorDemandPressure.value shouldBe trace.timing.demandSignals.sectorDemandPressure
    trace.seedTransition.provenance.sectorHiringSignal.value shouldBe trace.timing.demandSignals.sectorHiringSignal
  }

  it should "match the legacy pure interpreter on aggregate execution balances" in {
    val init      = WorldInit.initialize(42L)
    val initState = FlowSimulation.SimState.fromInit(init)
    val rng       = new scala.util.Random(42L)
    val result    = FlowSimulation.step(initState, rng)
    val state     = AggregateBatchContract.emptyExecutionState()

    ImperativeInterpreter.planAndApplyAll(state, result.flows) shouldBe Right(())
    AggregateBatchContract.totalWealth(state) shouldBe Interpreter.totalWealth(
      Interpreter.applyAll(Map.empty[Int, Long], AggregateBatchContract.toLegacyFlows(result.flows)),
    )
  }
