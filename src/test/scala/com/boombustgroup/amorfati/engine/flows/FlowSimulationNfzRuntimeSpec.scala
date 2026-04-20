package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.MonthRandomness
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.{AssetType, BatchedFlow, Flow, MechanismId}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FlowSimulationNfzRuntimeSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  private def mechanismBatches(batches: Vector[BatchedFlow], mechanism: MechanismId): Vector[BatchedFlow] =
    batches.filter(_.mechanism == mechanism)

  private def assertAllCash(selected: Vector[BatchedFlow], mechanism: MechanismId): Unit =
    withClue(s"Mechanism ${mechanism.toInt} should emit only cash batches: ") {
      selected.map(_.asset).toSet shouldBe Set(AssetType.Cash)
    }

  private def cashMechanismTotal(selected: Vector[BatchedFlow]): PLN =
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
    val directFlows = NfzFlows.emit(NfzFlows.NfzInput(nfz))

    val contributionBatches = mechanismBatches(result.flows, FlowMechanism.NfzContribution)
    val spendingBatches     = mechanismBatches(result.flows, FlowMechanism.NfzSpending)
    val subventionBatches   = mechanismBatches(result.flows, FlowMechanism.NfzGovSubvention)

    assertAllCash(contributionBatches, FlowMechanism.NfzContribution)
    assertAllCash(spendingBatches, FlowMechanism.NfzSpending)
    assertAllCash(subventionBatches, FlowMechanism.NfzGovSubvention)

    val emittedContributions = cashMechanismTotal(contributionBatches)
    val emittedSpending      = cashMechanismTotal(spendingBatches)
    val emittedSubvention    = cashMechanismTotal(subventionBatches)

    result.sfcResult shouldBe Right(())
    withClue("SimParams.defaults with seed 42 should exercise a positive NFZ deficit path: ") {
      nfz.spending should be > nfz.contributions
      nfz.govSubvention shouldBe (nfz.spending - nfz.contributions)
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
