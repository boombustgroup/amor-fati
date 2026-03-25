package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.Simulation
import com.boombustgroup.amorfati.init.WorldInit
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Tests FlowSimulation.emitAllFlows with MonthlyCalculus extracted from real
  * World state.
  *
  * Proves that the Contract-First pipeline design (MonthlyCalculus →
  * emitAllFlows → Interpreter) closes at SFC == 0L when fed with real
  * simulation data.
  */
class FlowSimulationSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  /** Extract MonthlyCalculus from a World state (bridge from old pipeline). */
  private def extractCalculus(w: com.boombustgroup.amorfati.engine.World): FlowSimulation.MonthlyCalculus =
    val agg = w.hhAgg
    val ins = w.financial.insurance
    val eq  = w.financial.equity
    val cb  = w.financial.corporateBonds
    val h   = w.real.housing

    FlowSimulation.MonthlyCalculus(
      month = w.month,
      resWage = agg.reservationWage,
      lendingBaseRate = w.nbp.referenceRate,
      baseMinWage = w.gov.minWageLevel,
      minWagePriceLevel = w.gov.minWagePriceLevel,
      wage = agg.marketWage,
      employed = agg.employed,
      laborDemand = agg.employed,
      retirees = w.social.demographics.retirees,
      workingAgePop = w.social.demographics.workingAgePop,
      nBankruptFirms = agg.bankrupt,
      avgFirmWorkers = 10,
      totalIncome = agg.totalIncome,
      consumption = agg.consumption,
      domesticConsumption = agg.domesticConsumption,
      importConsumption = agg.importConsumption,
      totalRent = agg.totalRent,
      totalPit = agg.totalPit,
      totalDebtService = agg.totalDebtService,
      totalDepositInterest = agg.totalDepositInterest,
      totalRemittances = agg.totalRemittances,
      totalUnempBenefits = agg.totalUnempBenefits,
      totalSocialTransfers = agg.totalSocialTransfers,
      totalCcOrigination = agg.totalConsumerOrigination,
      totalCcDebtService = agg.totalConsumerDebtService,
      totalCcDefault = agg.totalConsumerDefault,
      govPurchases = w.gov.govCurrentSpend,
      firmTax = w.gov.taxRevenue,
      firmNewLoans = PLN.Zero,
      firmPrincipal = PLN.Zero,
      firmInterestIncome = PLN.Zero,
      firmCapex = PLN.Zero,
      firmEquityIssuance = eq.lastIssuance,
      firmBondIssuance = cb.lastIssuance,
      firmIoPayments = w.flows.ioFlows,
      firmNplLoss = PLN.Zero,
      firmProfitShifting = PLN.Zero,
      firmFdiRepatriation = PLN.Zero,
      firmGrossInvestment = w.real.grossInvestment,
      gdp = PLN(w.gdpProxy),
      inflation = w.inflation,
      equityDomDividends = eq.lastDomesticDividends,
      equityForDividends = eq.lastForeignDividends,
      equityDivTax = eq.lastDividendTax,
      equityIssuance = eq.lastIssuance,
      equityReturn = eq.monthlyReturn,
      exports = w.bop.exports,
      totalImports = w.bop.totalImports,
      tourismExport = w.flows.tourismExport,
      tourismImport = w.flows.tourismImport,
      fdi = w.bop.fdi,
      portfolioFlows = w.bop.portfolioFlows,
      primaryIncome = w.bop.primaryIncome,
      euFunds = w.bop.euFundsMonthly,
      diasporaInflow = w.flows.diasporaRemittanceInflow,
      corpBondCoupon = cb.lastCouponIncome,
      corpBondDefaultLoss = cb.lastDefaultLoss,
      corpBondIssuance = cb.lastIssuance,
      corpBondAmortization = cb.lastAmortization,
      mortgageOrigination = h.lastOrigination,
      mortgageRepayment = h.lastRepayment,
      mortgageInterest = h.mortgageInterestIncome,
      mortgageDefault = h.lastDefault,
      bankGovBondIncome = w.bank.govBondHoldings * w.gov.bondYield.monthly,
      bankReserveInterest = w.plumbing.reserveInterestTotal,
      bankStandingFacility = w.plumbing.standingFacilityNet,
      bankInterbankInterest = w.plumbing.interbankInterestNet,
      bankBfgLevy = PLN.Zero,
      bankUnrealizedLoss = PLN.Zero,
      bankBailIn = PLN.Zero,
      bankNbpRemittance = PLN.Zero,
      govTaxRevenue = w.gov.taxRevenue,
      govDebtService = w.gov.debtServiceSpend,
      govEuCofinancing = w.gov.euCofinancing,
      govCapitalSpend = w.gov.govCapitalSpend,
      insurancePrevGovBonds = ins.govBondHoldings,
      insurancePrevCorpBonds = ins.corpBondHoldings,
      insurancePrevEquity = ins.equityHoldings,
      govBondYield = w.gov.bondYield,
      corpBondYield = cb.corpBondYield,
    )

  "FlowSimulation.emitAllFlows" should "preserve SFC at 0L" in {
    val init  = WorldInit.initialize(42L)
    val step  = Simulation.step(Simulation.SimState(init.world, init.firms, init.households), 42L, 1)
    val calc  = extractCalculus(step.state.world)
    val flows = FlowSimulation.emitAllFlows(calc)

    Interpreter.totalWealth(Interpreter.applyAll(Map.empty[Int, Long], flows)) shouldBe 0L
  }

  it should "preserve SFC across 12 months" in {
    val init  = WorldInit.initialize(42L)
    var state = Simulation.SimState(init.world, init.firms, init.households)

    (1 to 12).foreach { month =>
      state = Simulation.step(state, 42L, month).state
      val calc  = extractCalculus(state.world)
      val flows = FlowSimulation.emitAllFlows(calc)

      withClue(s"Month $month: ") {
        Interpreter.totalWealth(Interpreter.applyAll(Map.empty[Int, Long], flows)) shouldBe 0L
      }
    }
  }

  it should "emit 30+ mechanism IDs" in {
    val init  = WorldInit.initialize(42L)
    val step  = Simulation.step(Simulation.SimState(init.world, init.firms, init.households), 42L, 1)
    val flows = FlowSimulation.emitAllFlows(extractCalculus(step.state.world))

    flows.map(_.mechanism).toSet.size should be > 30
  }
