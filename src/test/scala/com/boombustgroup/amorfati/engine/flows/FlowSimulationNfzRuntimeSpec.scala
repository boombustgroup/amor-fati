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

  private def mechanismTotal(batches: Vector[BatchedFlow], mechanism: MechanismId): PLN =
    PLN.fromRaw(
      batches.iterator
        .filter(_.mechanism == mechanism)
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

  "FlowSimulation.step" should "align NFZ runtime emission and executed semantic flow evidence in default CI" in {
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

    result.trace.executedFlows.nfzContributions shouldBe emittedContributions
    result.trace.executedFlows.nfzSpending shouldBe emittedSpending
    result.trace.executedFlows.nfzGovSubvention shouldBe emittedSubvention

    val emittedGovSpending = Vector(
      FlowMechanism.GovPurchases,
      FlowMechanism.GovDebtService,
      FlowMechanism.GovUnempBenefit,
      FlowMechanism.GovSocialTransfer,
      FlowMechanism.GovEuCofin,
      FlowMechanism.GovCapitalInvestment,
      FlowMechanism.ZusGovSubvention,
      FlowMechanism.NfzGovSubvention,
      FlowMechanism.FpGovSubvention,
      FlowMechanism.PfronGovSubvention,
      FlowMechanism.FgspGovSubvention,
    ).map(mechanismTotal(result.flows, _)).foldLeft(PLN.Zero)(_ + _)

    result.trace.executedFlows.govSpending shouldBe emittedGovSpending
    result.trace.executedFlows.totalIncome shouldBe mechanismTotal(result.flows, FlowMechanism.HhTotalIncome)
    result.trace.executedFlows.jstDepositChange shouldBe
      mechanismTotal(result.flows, FlowMechanism.JstRevenue) - mechanismTotal(result.flows, FlowMechanism.JstSpending)
    result.trace.executedFlows.dividendIncome shouldBe mechanismTotal(result.flows, FlowMechanism.EquityDomDividend)
    result.trace.executedFlows.foreignDividendOutflow shouldBe mechanismTotal(result.flows, FlowMechanism.EquityForDividend)
    result.trace.executedFlows.dividendTax shouldBe mechanismTotal(result.flows, FlowMechanism.EquityDividendTax)

    val insurancePremiums = mechanismTotal(result.flows, FlowMechanism.InsLifePremium) +
      mechanismTotal(result.flows, FlowMechanism.InsNonLifePremium)
    val insuranceClaims   = mechanismTotal(result.flows, FlowMechanism.InsLifeClaim) +
      mechanismTotal(result.flows, FlowMechanism.InsNonLifeClaim)

    result.trace.executedFlows.insNetDepositChange shouldBe insuranceClaims - insurancePremiums
  }
