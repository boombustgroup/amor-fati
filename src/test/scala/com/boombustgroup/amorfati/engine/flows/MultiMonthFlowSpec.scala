package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.Simulation
import com.boombustgroup.amorfati.init.WorldInit
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Multi-month flow verification: old pipeline drives state, new flows verify
  * accounting.
  *
  * Runs 12 months of full simulation via old Simulation.step(). At each month,
  * extracts flow mechanism inputs from the World state and verifies SFC == 0L.
  * Proves the flow-based accounting produces correct results on real evolving
  * state.
  */
class MultiMonthFlowSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  private def extractFlows(state: Simulation.SimState): Vector[Flow] =
    val w   = state.world
    val agg = w.hhAgg

    Vector.concat(
      // Tier 1: Social funds
      ZusFlows.emit(ZusFlows.ZusInput(agg.employed, agg.marketWage, w.social.demographics.retirees)),
      NfzFlows.emit(NfzFlows.NfzInput(agg.employed, agg.marketWage, w.social.demographics.workingAgePop, w.social.demographics.retirees)),
      PpkFlows.emit(PpkFlows.PpkInput(agg.employed, agg.marketWage)),
      EarmarkedFlows.emit(EarmarkedFlows.Input(agg.employed, agg.marketWage, agg.totalUnempBenefits, 0, 0)),
      JstFlows.emit(
        JstFlows.Input(
          w.gov.taxRevenue,
          agg.totalIncome,
          PLN(w.gdpProxy),
          state.firms.count(f => com.boombustgroup.amorfati.agents.Firm.isAlive(f)),
          agg.totalPit,
        ),
      ),
      // Tier 2: Agents
      HouseholdFlows.emit(StateAdapter.hhInput(agg)),
      GovBudgetFlows.emit(
        GovBudgetFlows.Input(
          w.gov.taxRevenue,
          w.gov.govCurrentSpend,
          w.gov.debtServiceSpend,
          agg.totalUnempBenefits,
          agg.totalSocialTransfers,
          w.gov.euCofinancing,
          w.gov.govCapitalSpend,
        ),
      ),
      InsuranceFlows.emit(
        InsuranceFlows.Input(
          agg.employed,
          agg.marketWage,
          Share.One - Share.fraction(agg.employed, w.totalPopulation.max(1)),
          w.financial.insurance.govBondHoldings,
          w.financial.insurance.corpBondHoldings,
          w.financial.insurance.equityHoldings,
          w.gov.bondYield,
          w.financial.corporateBonds.corpBondYield,
          w.financial.equity.monthlyReturn,
        ),
      ),
      // Tier 3: Financial markets + external
      EquityFlows.emit(
        EquityFlows.Input(
          w.financial.equity.lastDomesticDividends,
          w.financial.equity.lastForeignDividends,
          w.financial.equity.lastDividendTax,
          w.financial.equity.lastIssuance,
        ),
      ),
      CorpBondFlows.emit(
        CorpBondFlows.Input(
          w.financial.corporateBonds.lastCouponIncome,
          w.financial.corporateBonds.lastDefaultLoss,
          w.financial.corporateBonds.lastIssuance,
          w.financial.corporateBonds.lastAmortization,
        ),
      ),
      MortgageFlows.emit(
        MortgageFlows.Input(
          w.real.housing.lastOrigination,
          w.real.housing.lastRepayment,
          w.real.housing.mortgageInterestIncome,
          w.real.housing.lastDefault,
        ),
      ),
      OpenEconFlows.emit(
        OpenEconFlows.Input(
          w.bop.exports,
          w.bop.totalImports,
          w.flows.tourismExport,
          w.flows.tourismImport,
          w.bop.fdi,
          w.bop.portfolioFlows,
          w.bop.primaryIncome,
          w.bop.euFundsMonthly,
          w.flows.diasporaRemittanceInflow,
          PLN.Zero,
        ),
      ),
      BankingFlows.emit(
        BankingFlows.Input(
          w.bank.govBondHoldings * w.gov.bondYield.monthly,
          w.plumbing.reserveInterestTotal,
          w.plumbing.standingFacilityNet,
          w.plumbing.interbankInterestNet,
          PLN.Zero,
          PLN.Zero,
          PLN.Zero,
          PLN.Zero,
        ),
      ),
    )

  "Multi-month flow verification (12 months)" should "preserve SFC at 0L every month" in {
    val init  = WorldInit.initialize(42L)
    var state = Simulation.SimState(init.world, init.firms, init.households)

    (1 to 12).foreach { month =>
      val result = Simulation.step(state, 42L, month)
      state = result.state

      val flows    = extractFlows(state)
      val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)

      withClue(s"SFC violated at month $month: ") {
        Interpreter.totalWealth(balances) shouldBe 0L
      }
    }
  }

  it should "produce increasing total flow volume over time" in {
    val init    = WorldInit.initialize(42L)
    var state   = Simulation.SimState(init.world, init.firms, init.households)
    var volumes = Vector.empty[Long]

    (1 to 12).foreach { month =>
      val result = Simulation.step(state, 42L, month)
      state = result.state
      val flows  = extractFlows(state)
      volumes = volumes :+ flows.map(_.amount).sum
    }

    volumes.last should be > volumes.head
  }
