package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.agents.SocialSecurity
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.MonthRandomness
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.{AssetType, BatchedFlow, Flow, MechanismId}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FlowSimulationNfzRuntimeSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  private def cashMechanismTotal(batches: Vector[BatchedFlow], mechanism: MechanismId): PLN =
    val selected = batches.filter(_.mechanism == mechanism)
    selected.map(_.asset).toSet shouldBe Set(AssetType.Cash)
    PLN.fromRaw(
      selected.iterator
        .map(RuntimeLedgerTopology.totalTransferred)
        .sum,
    )

  private def flowTotal(flows: Vector[Flow], mechanism: MechanismId): PLN =
    PLN.fromRaw(
      flows.iterator
        .filter(_.mechanism == mechanism.toInt)
        .map(_.amount)
        .sum,
    )

  "FlowSimulation.step" should "align NFZ runtime emission with direct NfzFlows emission in default CI" in {
    val init        = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state       = FlowSimulation.SimState.fromInit(init)
    val result      = FlowSimulation.step(state, MonthRandomness.Contract.fromSeed(42L))
    val nfz         = result.nextState.world.social.nfz
    val expectedNfz = SocialSecurity.nfzStep(result.calculus.employed, result.calculus.wage, result.calculus.workingAgePop, result.calculus.retirees)
    val directFlows = NfzFlows.emit(NfzFlows.NfzInput(nfz))

    val emittedContributions = cashMechanismTotal(result.flows, FlowMechanism.NfzContribution)
    val emittedSpending      = cashMechanismTotal(result.flows, FlowMechanism.NfzSpending)
    val emittedSubvention    = cashMechanismTotal(result.flows, FlowMechanism.NfzGovSubvention)

    result.sfcResult shouldBe Right(())
    expectedNfz shouldBe nfz
    withClue("SimParams.defaults with seed 42 should exercise the nfzStep positive govSubvention path: ") {
      nfz.govSubvention should be > PLN.Zero
      emittedSubvention should be > PLN.Zero
    }
    emittedContributions shouldBe nfz.contributions
    emittedSpending shouldBe nfz.spending
    emittedSubvention shouldBe nfz.govSubvention

    emittedContributions shouldBe flowTotal(directFlows, FlowMechanism.NfzContribution)
    emittedSpending shouldBe flowTotal(directFlows, FlowMechanism.NfzSpending)
    emittedSubvention shouldBe flowTotal(directFlows, FlowMechanism.NfzGovSubvention)
  }
