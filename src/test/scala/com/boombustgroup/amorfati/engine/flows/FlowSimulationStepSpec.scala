package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.accounting.Sfc
import com.boombustgroup.amorfati.agents.Household
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.{
  DecisionSignals,
  MonthDriver,
  MonthRandomness,
  MonthSemantics,
  MonthTimingEnvelopeKey,
  MonthTimingPayload,
  MonthTraceStage,
}
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
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
    val init   = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state  = FlowSimulation.SimState.fromInit(init)
    val result = FlowSimulation.step(state, MonthRandomness.Contract.fromSeed(42L))
    result.execution.totalWealth shouldBe 0L
    result.sfcResult shouldBe Right(())
    result.calculus.employed should be > 0
  }

  it should "expose the month boundary as SimState -> StepOutput -> (nextState, trace)" in {
    val init        = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state       = FlowSimulation.SimState.fromInit(init)
    val contract    = MonthRandomness.Contract.fromSeed(42L)
    val stateResult = FlowSimulation.step(state, contract)
    val repeated    = FlowSimulation.step(state, contract)

    stateResult.stateIn shouldBe state
    stateResult.transition shouldBe ((stateResult.nextState, stateResult.trace))
    stateResult.nextState.world shouldBe repeated.nextState.world
    stateResult.nextState.firms shouldBe repeated.nextState.firms
    canonicalHouseholds(stateResult.nextState.households) shouldBe canonicalHouseholds(repeated.nextState.households)
    stateResult.nextState.banks shouldBe repeated.nextState.banks
    stateResult.nextState.householdAggregates shouldBe repeated.nextState.householdAggregates
    stateResult.trace shouldBe repeated.trace
  }

  it should "keep explicit pre and next-pre seed wrappers aligned with step outputs" in {
    val init    = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state   = FlowSimulation.SimState.fromInit(init)
    val seedIn  = MonthSemantics.At[DecisionSignals, MonthSemantics.Pre](state.world.seedIn)
    val result  = FlowSimulation.step(state, MonthRandomness.Contract.fromSeed(42L))
    val seedOut = MonthSemantics.At[com.boombustgroup.amorfati.engine.SignalExtraction.Output, MonthSemantics.NextPre](result.signalExtraction)

    seedIn.value shouldBe state.world.seedIn
    seedOut.value shouldBe result.signalExtraction
    seedOut.value.seedOut shouldBe result.nextState.world.seedIn
    result.trace.seedTransition.seedIn shouldBe seedIn.value
    result.trace.seedTransition.seedOut shouldBe seedOut.value.seedOut
  }

  it should "accept an explicit month-step randomness contract with named stage and assembly streams" in {
    val init       = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state      = FlowSimulation.SimState.fromInit(init)
    val contract   = MonthRandomness.Contract.fromSeed(1234L)
    val fromSeed   = FlowSimulation.step(state, contract)
    val fromReplay = FlowSimulation.step(state, MonthRandomness.Contract.fromSeed(1234L))

    fromSeed.randomness shouldBe contract
    fromSeed.trace.randomness shouldBe contract
    fromSeed.trace.randomness.all.map(_.key).toSet shouldBe Set(
      MonthRandomness.StreamKey.HouseholdIncomeEconomics,
      MonthRandomness.StreamKey.FirmEconomics,
      MonthRandomness.StreamKey.HouseholdFinancialEconomics,
      MonthRandomness.StreamKey.PriceEquityEconomics,
      MonthRandomness.StreamKey.OpenEconEconomics,
      MonthRandomness.StreamKey.BankingEconomics,
      MonthRandomness.StreamKey.FdiMa,
      MonthRandomness.StreamKey.FirmEntry,
      MonthRandomness.StreamKey.StartupStaffing,
      MonthRandomness.StreamKey.RegionalMigration,
    )
    fromSeed.trace.randomness.all.map(_.seed).distinct should have size fromSeed.trace.randomness.all.size
    fromSeed.nextState.world shouldBe fromReplay.nextState.world
    fromSeed.nextState.firms shouldBe fromReplay.nextState.firms
    canonicalHouseholds(fromSeed.nextState.households) shouldBe canonicalHouseholds(fromReplay.nextState.households)
    fromSeed.nextState.banks shouldBe fromReplay.nextState.banks
    fromSeed.trace shouldBe fromReplay.trace
  }

  it should "expose a first-class unfold driver over the monthly step boundary" in {
    val init  = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state = FlowSimulation.SimState.fromInit(init)
    val steps = MonthDriver
      .unfoldSteps(state): current =>
        Some(MonthRandomness.Contract.fromSeed(42L * 1000L + current.world.month.toLong + 1L))
      .take(3)
      .toVector

    steps should have size 3
    steps.map(_.stateIn.world.month) shouldBe Vector(0, 1, 2)
    steps.map(_.nextState.world.month) shouldBe Vector(1, 2, 3)
    steps.foreach(_.sfcResult shouldBe Right(()))
  }

  it should "stop unfolding when the caller closes the randomness schedule" in {
    val init    = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state   = FlowSimulation.SimState.fromInit(init)
    val results = MonthDriver
      .unfoldSteps(state): current =>
        Option.when(current.world.month < 2)(MonthRandomness.Contract.fromSeed(42L * 1000L + current.world.month.toLong + 1L))
      .toVector

    results should have size 2
    results.map(_.nextState.world.month) shouldBe Vector(1, 2)
  }

  it should "produce SFC == 0L across 12 months (autonomous driving)" in {
    val init    = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state   = FlowSimulation.SimState.fromInit(init)
    val results = MonthDriver
      .unfoldSteps(state): current =>
        Some(MonthRandomness.Contract.fromSeed(42L * 1000L + current.world.month.toLong + 1L))
      .take(12)
      .toVector

    results should have size 12

    results.foreach { result =>
      val month = result.nextState.world.month
      withClue(s"Month $month: ") {
        result.execution.totalWealth shouldBe 0L
        result.sfcResult shouldBe Right(())
      }
    }
  }

  it should "propagate informal-economy pressure into fiscal outputs without breaking SFC" in {
    val init          = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val lowShadowW    = init.world.copy(mechanisms = init.world.mechanisms.copy(informalCyclicalAdj = 0.0))
    val highShadowW   = init.world.copy(mechanisms = init.world.mechanisms.copy(informalCyclicalAdj = 0.4))
    val lowShadowRun  = FlowSimulation.step(
      FlowSimulation.SimState(lowShadowW, init.firms, init.households, init.banks, init.householdAggregates),
      MonthRandomness.Contract.fromSeed(42L),
    )
    val highShadowRun = FlowSimulation.step(
      FlowSimulation.SimState(highShadowW, init.firms, init.households, init.banks, init.householdAggregates),
      MonthRandomness.Contract.fromSeed(42L),
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
    val init   = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state  = FlowSimulation.SimState.fromInit(init)
    val result = FlowSimulation.step(state, MonthRandomness.Contract.fromSeed(42L))
    result.flows.map(_.mechanism).toSet.size should be > 30
  }

  it should "emit a typed MonthTrace with stable boundaries and typed timing envelopes" in {
    val init   = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state  = FlowSimulation.SimState.fromInit(init)
    val result = FlowSimulation.step(state, MonthRandomness.Contract.fromSeed(42L))
    val trace  = result.trace

    val demandSignals = trace.timing.requirePayload[MonthTimingPayload.DemandSignals](MonthTimingEnvelopeKey.DemandSignals)

    trace.month shouldBe result.nextState.world.month
    trace.seedTransition.seedIn shouldBe init.world.seedIn
    trace.seedTransition.seedOut shouldBe result.nextState.world.seedIn
    trace.randomness shouldBe result.randomness
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
    val init   = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state  = FlowSimulation.SimState.fromInit(init)
    val result = FlowSimulation.step(state, MonthRandomness.Contract.fromSeed(42L))
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
    val init      = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val initState = FlowSimulation.SimState.fromInit(init)
    val result    = FlowSimulation.step(initState, MonthRandomness.Contract.fromSeed(42L))
    val state     = AggregateBatchContract.emptyExecutionState()

    ImperativeInterpreter.planAndApplyAll(state, result.flows) shouldBe Right(())
    AggregateBatchContract.totalWealth(state) shouldBe Interpreter.totalWealth(
      Interpreter.applyAll(Map.empty[Int, Long], AggregateBatchContract.toLegacyFlows(result.flows)),
    )
  }
