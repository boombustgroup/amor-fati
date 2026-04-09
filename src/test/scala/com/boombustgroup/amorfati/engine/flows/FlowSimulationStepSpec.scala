package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.accounting.Sfc
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

  "FlowSimulation.step" should "produce SFC == 0L on real World" in {
    val init   = WorldInit.initialize(42L)
    val rng    = new scala.util.Random(42)
    val result = FlowSimulation.step(init.world, init.firms, init.households, init.banks, rng)
    result.execution.totalWealth shouldBe 0L
    result.sfcResult shouldBe Right(())
    result.calculus.employed should be > 0
  }

  it should "produce SFC == 0L across 12 months (autonomous driving)" in {
    val init  = WorldInit.initialize(42L)
    var w     = init.world
    var firms = init.firms
    var hh    = init.households
    var banks = init.banks

    (1 to 12).foreach { month =>
      val rng    = new scala.util.Random(42L * 1000 + month)
      val result = FlowSimulation.step(w, firms, hh, banks, rng)
      withClue(s"Month $month: ") {
        result.execution.totalWealth shouldBe 0L
        result.sfcResult shouldBe Right(())
      }
      w = result.newWorld
      firms = result.newFirms
      hh = result.newHouseholds
      banks = result.newBanks
    }
  }

  it should "propagate informal-economy pressure into fiscal outputs without breaking SFC" in {
    val init          = WorldInit.initialize(42L)
    val lowShadowW    = init.world.copy(mechanisms = init.world.mechanisms.copy(informalCyclicalAdj = 0.0))
    val highShadowW   = init.world.copy(mechanisms = init.world.mechanisms.copy(informalCyclicalAdj = 0.4))
    val lowShadowRun  = FlowSimulation.step(lowShadowW, init.firms, init.households, init.banks, new scala.util.Random(42L))
    val highShadowRun = FlowSimulation.step(highShadowW, init.firms, init.households, init.banks, new scala.util.Random(42L))

    lowShadowRun.execution.totalWealth shouldBe 0L
    highShadowRun.execution.totalWealth shouldBe 0L
    lowShadowRun.sfcResult shouldBe Right(())
    highShadowRun.sfcResult shouldBe Right(())

    highShadowRun.newWorld.flows.realizedTaxShadowShare should be > lowShadowRun.newWorld.flows.realizedTaxShadowShare
    highShadowRun.newWorld.flows.taxEvasionLoss should be > lowShadowRun.newWorld.flows.taxEvasionLoss
    highShadowRun.newWorld.gov.taxRevenue should be < lowShadowRun.newWorld.gov.taxRevenue
    highShadowRun.newWorld.gov.deficit should be > lowShadowRun.newWorld.gov.deficit
  }

  it should "produce 30+ mechanism IDs" in {
    val init   = WorldInit.initialize(42L)
    val rng    = new scala.util.Random(42)
    val result = FlowSimulation.step(init.world, init.firms, init.households, init.banks, rng)
    result.flows.map(_.mechanism).toSet.size should be > 30
  }

  it should "emit a typed MonthTrace with stable boundaries and typed timing envelopes" in {
    val init   = WorldInit.initialize(42L)
    val rng    = new scala.util.Random(42L)
    val result = FlowSimulation.step(init.world, init.firms, init.households, init.banks, rng)
    val trace  = result.monthTrace

    val demandSignals = trace.timing.requirePayload[MonthTimingPayload.DemandSignals](MonthTimingEnvelopeKey.DemandSignals)

    trace.month shouldBe result.newWorld.month
    trace.seedTransition.seedIn shouldBe init.world.seedIn
    trace.seedTransition.seedOut shouldBe result.newWorld.seedIn
    trace.boundary.startSnapshot.stock shouldBe Sfc.snapshot(init.world, init.firms, init.households, init.banks)
    trace.boundary.endSnapshot.stock shouldBe Sfc.snapshot(result.newWorld, result.newFirms, result.newHouseholds, result.newBanks)
    trace.boundary.startSnapshot.inflation shouldBe init.world.inflation
    trace.boundary.endSnapshot.inflation shouldBe result.newWorld.inflation
    trace.timing.envelopes.map(_.key).toSet shouldBe Set(
      MonthTimingEnvelopeKey.LaborSignals,
      MonthTimingEnvelopeKey.DemandSignals,
      MonthTimingEnvelopeKey.NominalSignals,
      MonthTimingEnvelopeKey.FirmDynamics,
    )
    trace.timing.laborSignals.operationalHiringSlack shouldBe result.newWorld.pipeline.operationalHiringSlack
    demandSignals.sectorDemandMult shouldBe result.newWorld.seedIn.sectorDemandMult
    demandSignals.sectorDemandPressure shouldBe result.newWorld.seedIn.sectorDemandPressure
    demandSignals.sectorHiringSignal shouldBe result.newWorld.seedIn.sectorHiringSignal
    trace.timing.nominalSignals.realizedInflation shouldBe result.newWorld.inflation
    trace.timing.nominalSignals.expectedInflation shouldBe result.newWorld.mechanisms.expectations.expectedInflation
    trace.timing.firmDynamics.startupAbsorptionRate shouldBe result.newWorld.seedIn.startupAbsorptionRate
    trace.timing.firmDynamics.firmBirths shouldBe result.newWorld.flows.firmBirths
    trace.timing.firmDynamics.firmDeaths shouldBe result.newWorld.flows.firmDeaths
    trace.timing.firmDynamics.netFirmBirths shouldBe result.newWorld.flows.netFirmBirths
    trace.executedFlows.totalIncome shouldBe result.calculus.totalIncome
    trace.executedFlows.currentAccount shouldBe result.newWorld.bop.currentAccount
    trace.executedFlows.fofResidual shouldBe result.newWorld.plumbing.fofResidual
    trace.seedTransition.provenance.unemploymentRate.stage shouldBe MonthTraceStage.WorldAssemblyEconomics
    trace.seedTransition.provenance.inflation.stage shouldBe MonthTraceStage.PriceEquityEconomics
    trace.seedTransition.provenance.expectedInflation.stage shouldBe MonthTraceStage.OpenEconEconomics
    trace.seedTransition.provenance.laggedHiringSlack.stage shouldBe MonthTraceStage.LaborEconomics
    trace.seedTransition.provenance.startupAbsorptionRate.stage shouldBe MonthTraceStage.StartupStaffing
    trace.seedTransition.provenance.sectorDemandMult.stage shouldBe MonthTraceStage.DemandEconomics
    trace.seedTransition.provenance.sectorDemandPressure.stage shouldBe MonthTraceStage.DemandEconomics
    trace.seedTransition.provenance.sectorHiringSignal.stage shouldBe MonthTraceStage.DemandEconomics
    trace.seedTransition.provenance.laggedHiringSlack.value shouldBe result.newWorld.seedIn.laggedHiringSlack
    trace.seedTransition.provenance.startupAbsorptionRate.value shouldBe result.newWorld.seedIn.startupAbsorptionRate
    trace.validations should have size 1
    trace.validations.head.passed shouldBe true
    trace.validations.head.failures shouldBe empty
    result.sfcResult shouldBe Right(())
  }

  it should "keep MonthTrace seed transitions consistent with timing envelopes and end-of-month boundary data" in {
    val init   = WorldInit.initialize(42L)
    val rng    = new scala.util.Random(42L)
    val result = FlowSimulation.step(init.world, init.firms, init.households, init.banks, rng)
    val trace  = result.monthTrace

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
    val init   = WorldInit.initialize(42L)
    val rng    = new scala.util.Random(42L)
    val result = FlowSimulation.step(init.world, init.firms, init.households, init.banks, rng)
    val state  = AggregateBatchContract.emptyExecutionState()

    ImperativeInterpreter.planAndApplyAll(state, result.flows) shouldBe Right(())
    AggregateBatchContract.totalWealth(state) shouldBe Interpreter.totalWealth(
      Interpreter.applyAll(Map.empty[Int, Long], AggregateBatchContract.toLegacyFlows(result.flows)),
    )
  }
