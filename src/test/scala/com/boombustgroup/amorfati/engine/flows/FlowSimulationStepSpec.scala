package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.accounting.Sfc
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.MonthTraceStage
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

  it should "emit a typed MonthTrace with boundary snapshots, stage outputs, and seed provenance" in {
    val init   = WorldInit.initialize(42L)
    val rng    = new scala.util.Random(42L)
    val result = FlowSimulation.step(init.world, init.firms, init.households, init.banks, rng)
    val trace  = result.monthTrace

    trace.month shouldBe result.newWorld.month
    trace.seedIn shouldBe init.world.seedIn
    trace.seedOut shouldBe result.newWorld.seedIn
    trace.startSnapshot.stock shouldBe Sfc.snapshot(init.world, init.firms, init.households, init.banks)
    trace.endSnapshot.stock shouldBe Sfc.snapshot(result.newWorld, result.newFirms, result.newHouseholds, result.newBanks)
    trace.startSnapshot.inflation shouldBe init.world.inflation
    trace.endSnapshot.inflation shouldBe result.newWorld.inflation
    trace.stages.operationalHiringSlack shouldBe result.newWorld.pipeline.operationalHiringSlack
    trace.stages.sectorDemandMult shouldBe result.newWorld.seedIn.sectorDemandMult
    trace.stages.sectorDemandPressure shouldBe result.newWorld.seedIn.sectorDemandPressure
    trace.stages.sectorHiringSignal shouldBe result.newWorld.seedIn.sectorHiringSignal
    trace.stages.realizedInflation shouldBe result.newWorld.inflation
    trace.stages.expectedInflation shouldBe result.newWorld.mechanisms.expectations.expectedInflation
    trace.stages.startupAbsorptionRate shouldBe result.newWorld.seedIn.startupAbsorptionRate
    trace.stages.firmBirths shouldBe result.newWorld.flows.firmBirths
    trace.stages.firmDeaths shouldBe result.newWorld.flows.firmDeaths
    trace.stages.netFirmBirths shouldBe result.newWorld.flows.netFirmBirths
    trace.executedFlows.totalIncome shouldBe result.calculus.totalIncome
    trace.executedFlows.currentAccount shouldBe result.newWorld.bop.currentAccount
    trace.executedFlows.fofResidual shouldBe result.newWorld.plumbing.fofResidual
    trace.seedOutProvenance.unemploymentRate.stage shouldBe MonthTraceStage.WorldAssemblyEconomics
    trace.seedOutProvenance.inflation.stage shouldBe MonthTraceStage.PriceEquityEconomics
    trace.seedOutProvenance.expectedInflation.stage shouldBe MonthTraceStage.OpenEconEconomics
    trace.seedOutProvenance.laggedHiringSlack.stage shouldBe MonthTraceStage.LaborEconomics
    trace.seedOutProvenance.startupAbsorptionRate.stage shouldBe MonthTraceStage.StartupStaffing
    trace.seedOutProvenance.sectorDemandMult.stage shouldBe MonthTraceStage.DemandEconomics
    trace.seedOutProvenance.sectorDemandPressure.stage shouldBe MonthTraceStage.DemandEconomics
    trace.seedOutProvenance.sectorHiringSignal.stage shouldBe MonthTraceStage.DemandEconomics
    trace.seedOutProvenance.laggedHiringSlack.value shouldBe result.newWorld.seedIn.laggedHiringSlack
    trace.seedOutProvenance.startupAbsorptionRate.value shouldBe result.newWorld.seedIn.startupAbsorptionRate
    trace.validations should have size 1
    trace.validations.head.passed shouldBe true
    trace.validations.head.failures shouldBe empty
    result.sfcResult shouldBe Right(())
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
