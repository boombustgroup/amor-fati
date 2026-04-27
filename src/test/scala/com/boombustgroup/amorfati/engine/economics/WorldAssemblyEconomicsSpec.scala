package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.MonthRandomness
import com.boombustgroup.amorfati.engine.SimulationMonth.CompletedMonth
import com.boombustgroup.amorfati.engine.flows.FlowSimulation
import com.boombustgroup.amorfati.engine.ledger.LedgerFinancialState
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WorldAssemblyEconomicsSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  private lazy val deterministicStep: FlowSimulation.StepOutput =
    val init  = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state = FlowSimulation.SimState.fromInit(init)
    FlowSimulation.step(state, MonthRandomness.Contract.fromSeed(42L))

  "WorldAssemblyEconomics" should "produce valid world after simulation step" in {
    val result = deterministicStep
    val w      = result.nextState.world

    result.nextState.completedMonth shouldBe CompletedMonth(1)
    w.derivedTotalPopulation.should(be > 0)
    result.nextState.householdAggregates.employed.should(be > 0)
    w.external.tourismSeasonalFactor.should(not be Multiplier.Zero)
  }

  it should "keep ETS observables at the base price in the first execution month" in {
    val result = deterministicStep
    val w      = result.nextState.world

    decimal(w.real.etsPrice).shouldBe(decimal(p.climate.etsBasePrice) +- BigDecimal("1e-10"))
  }

  it should "preserve public-spending semantic aggregates on the assembled world" in {
    val result = deterministicStep
    val w      = result.nextState.world

    w.gov.domesticBudgetDemand shouldBe (w.gov.govCurrentSpend + w.gov.govCapitalSpend)
    w.gov.domesticBudgetOutlays shouldBe (
      w.gov.unempBenefitSpend
        + w.gov.socialTransferSpend
        + w.gov.govCurrentSpend
        + w.gov.govCapitalSpend
        + w.gov.debtServiceSpend
        + w.gov.euCofinancing
    )

    w.gov.govCurrentSpend shouldBe result.calculus.govCurrentSpend
    w.gov.domesticBudgetDemand shouldBe (result.calculus.govCurrentSpend + result.calculus.govCapitalSpend)
    w.gov.domesticBudgetOutlays.should(be >= w.gov.domesticBudgetDemand)
  }

  it should "carry supported financial stocks through stage-owned ledger updates" in {
    val nextState = deterministicStep.nextState
    val ledger    = nextState.ledgerFinancialState

    ledger.households.length shouldBe nextState.households.length
    nextState.households.foreach: household =>
      ledger.households.isDefinedAt(household.id.toInt) shouldBe true

    nextState.firms.foreach: firm =>
      ledger.firms.isDefinedAt(firm.id.toInt) shouldBe true

    nextState.banks.foreach: bank =>
      val balances = ledger.banks(bank.id.toInt)
      LedgerFinancialState.bankBalances(
        LedgerFinancialState.projectBankFinancialStocks(balances),
        balances.corpBond,
      ) shouldBe balances
  }
