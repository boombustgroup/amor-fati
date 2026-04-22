package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.MonthRandomness
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.EntitySector
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FlowSimulationExecutedEvidenceSpec extends AnyFlatSpec with Matchers:
  import RuntimeFlowsTestSupport.*

  private given p: SimParams = SimParams.defaults

  "FlowSimulation.step" should "source executed semantic flow evidence from emitted mechanisms in default CI" in {
    val init   = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state  = FlowSimulation.SimState.fromInit(init)
    val result = FlowSimulation.step(state, MonthRandomness.Contract.fromSeed(42L))

    val expectedCentralGovernmentSpendingMechanisms = Vector(
      FlowMechanism.GovPurchases,
      FlowMechanism.GovDebtService,
      FlowMechanism.GovUnempBenefit,
      FlowMechanism.GovSocialTransfer,
      FlowMechanism.GovEuCofin,
      FlowMechanism.GovCapitalInvestment,
      FlowMechanism.JstGovSubvention,
    )
    val expectedSocialFundGovSubventionMechanisms   = Vector(
      FlowMechanism.ZusGovSubvention,
      FlowMechanism.NfzGovSubvention,
      FlowMechanism.FpGovSubvention,
      FlowMechanism.PfronGovSubvention,
      FlowMechanism.FgspGovSubvention,
    )
    val expectedJstRevenueMechanisms                =
      Vector(FlowMechanism.JstRevenue, FlowMechanism.JstGovSubvention)

    FlowSimulation.ExecutedFlowEvidence.CentralGovernmentSpendingMechanisms shouldBe expectedCentralGovernmentSpendingMechanisms
    FlowSimulation.ExecutedFlowEvidence.SocialFundGovSubventionMechanisms shouldBe expectedSocialFundGovSubventionMechanisms
    FlowSimulation.ExecutedFlowEvidence.JstRevenueMechanisms shouldBe expectedJstRevenueMechanisms

    val govSpendingMechanisms       = expectedCentralGovernmentSpendingMechanisms ++ expectedSocialFundGovSubventionMechanisms
    val emittedGovSpending          =
      govSpendingMechanisms.map(mechanismTotal(result.flows, _)).foldLeft(PLN.Zero)(_ + _)
    val emittedJstRevenue           =
      expectedJstRevenueMechanisms.map(mechanismTotal(result.flows, _)).foldLeft(PLN.Zero)(_ + _)
    val emittedInvestNetDepositFlow =
      signedMechanismTotal(result.flows, FlowMechanism.InvestNetDepositFlow, EntitySector.Banks, EntitySector.Firms)

    result.sfcResult shouldBe Right(())
    result.trace.executedFlows.govSpending shouldBe emittedGovSpending
    result.trace.executedFlows.totalIncome shouldBe mechanismTotal(result.flows, FlowMechanism.HhTotalIncome)
    result.trace.executedFlows.jstRevenue shouldBe emittedJstRevenue
    result.trace.executedFlows.jstSpending shouldBe mechanismTotal(result.flows, FlowMechanism.JstSpending)
    result.trace.executedFlows.jstDepositChange shouldBe emittedJstRevenue - mechanismTotal(result.flows, FlowMechanism.JstSpending)
    result.trace.executedFlows.dividendIncome shouldBe mechanismTotal(result.flows, FlowMechanism.EquityDomDividend)
    result.trace.executedFlows.foreignDividendOutflow shouldBe mechanismTotal(result.flows, FlowMechanism.EquityForDividend)
    result.trace.executedFlows.dividendTax shouldBe mechanismTotal(result.flows, FlowMechanism.EquityDividendTax)
    result.trace.executedFlows.investNetDepositFlow shouldBe emittedInvestNetDepositFlow
    result.trace.executedFlows.investNetDepositFlow shouldBe result.calculus.investNetDepositFlow

    val insurancePremiums = mechanismTotal(result.flows, FlowMechanism.InsLifePremium) +
      mechanismTotal(result.flows, FlowMechanism.InsNonLifePremium)
    val insuranceClaims   = mechanismTotal(result.flows, FlowMechanism.InsLifeClaim) +
      mechanismTotal(result.flows, FlowMechanism.InsNonLifeClaim)

    result.trace.executedFlows.insNetDepositChange shouldBe insuranceClaims - insurancePremiums
  }
