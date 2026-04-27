package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FlowSimulationNfzRuntimeSpec extends AnyFlatSpec with Matchers:
  import RuntimeFlowsTestSupport.*

  private given p: SimParams = SimParams.defaults

  "FlowSimulation.step" should "align NFZ runtime emission with direct NFZ emission in default CI" in {
    val result      = stepFromSeed()
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

    result.trace.executedFlows.nfzContributions shouldBe emittedContributions
    result.trace.executedFlows.nfzSpending shouldBe emittedSpending
    result.trace.executedFlows.nfzGovSubvention shouldBe emittedSubvention
  }
