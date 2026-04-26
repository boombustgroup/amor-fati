package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.flows.*
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import com.boombustgroup.amorfati.random.RandomStream
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Tests OpenEconEconomics produces self-consistent results. */
class OpenEconEconomicsSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults
  private val TestSeed       = 42L

  private val init                     = WorldInit.initialize(InitRandomness.Contract.fromSeed(TestSeed))
  private val w                        = init.world
  private val baseLedgerFinancialState = FlowSimulation.SimState.fromInit(init).ledgerFinancialState
  private val rng                      = RandomStream.seeded(TestSeed)

  // Run pipeline through Economics objects
  private val s1 = FiscalConstraintEconomics.compute(w, init.banks, baseLedgerFinancialState, ExecutionMonth.First)
  private val s2 = LaborEconomics.compute(w, init.firms, init.households, s1)
  private val s3 =
    HouseholdIncomeEconomics.compute(w, init.firms, init.households, init.banks, baseLedgerFinancialState, s1.lendingBaseRate, s1.resWage, s2.newWage, rng)
  private val s4 = DemandEconomics.compute(w, s2.employed, s2.living, s3.domesticCons)
  private val s5 = FirmEconomics.runStep(w, init.firms, init.households, init.banks, baseLedgerFinancialState, s1, s2, s3, s4, rng)
  private val s6 = HouseholdFinancialEconomics.compute(w, s1.m, s2.employed, s3.hhAgg, rng)
  private val s7 = PriceEquityEconomics.compute(
    w = w,
    month = s1.m,
    wageGrowth = s2.wageGrowth,
    domesticCons = s3.domesticCons,
    govPurchases = s4.govPurchases,
    avgDemandMult = s4.avgDemandMult,
    totalSystemLoans = baseLedgerFinancialState.banks.map(_.firmLoan).sumPln,
    firmStep = s5,
  )

  private def runOpenEcon(world: World): OpenEconEconomics.StepOutput =
    OpenEconEconomics.runStep(
      OpenEconEconomics.StepInput(
        w = world,
        ledgerFinancialState = baseLedgerFinancialState,
        s1 = s1,
        s2 = s2,
        s3 = s3,
        s4 = s4,
        s5 = s5,
        s6 = s6,
        s7 = s7,
        banks = init.banks,
        commodityRng = RandomStream.seeded(TestSeed),
      ),
    )

  private val result = runOpenEcon(w)

  "OpenEconEconomics (self-contained)" should "produce a valid reference rate" in {
    decimal(result.monetary.newRefRate) should be >= BigDecimal("0.0")
  }

  it should "keep diaspora inflow growth at baseline in the first execution month" in {

    val exchangeRate  = decimal(w.forex.exchangeRate)
    val wap           = w.social.demographics.workingAgePop
    val base          = decimal(p.remittance.perCapita) * decimal(wap)
    val erAdj         = powDecimal(exchangeRate / decimal(p.forex.baseExRate), decimal(p.remittance.erElasticity))
    val unempForRemit = decimal(w.unemploymentRate(s2.employed))
    val cyclicalAdj   = BigDecimal(1) + decimal(p.remittance.cyclicalSens) * (unempForRemit - BigDecimal("0.05")).max(BigDecimal(0))
    val expected      = plnBD(base * erAdj * cyclicalAdj)

    s6.diasporaInflow shouldBe expected
  }

  it should "keep tourism growth at baseline in the first execution month" in {

    val exchangeRate   = decimal(w.forex.exchangeRate)
    val monthInYear    = s1.m.monthInYear
    val seasonalFactor = BigDecimal(1) + decimal(p.tourism.seasonality) * cosTurns(monthInYear - p.tourism.peakMonth, 12)
    val inboundErAdj   = powDecimal(exchangeRate / decimal(p.forex.baseExRate), decimal(p.tourism.erElasticity))
    val outboundErAdj  = powDecimal(decimal(p.forex.baseExRate) / exchangeRate, decimal(p.tourism.erElasticity))
    val baseGdp        = decimal(w.cachedMonthlyGdpProxy).max(BigDecimal(0))
    val expectedExport = plnBD(baseGdp * decimal(p.tourism.inboundShare) * seasonalFactor * inboundErAdj)
    val expectedImport = plnBD(baseGdp * decimal(p.tourism.outboundShare) * seasonalFactor * outboundErAdj)

    s6.tourismExport shouldBe expectedExport
    s6.tourismImport shouldBe expectedImport
  }

  it should "produce a valid bond yield" in {
    decimal(result.monetary.newBondYield) should be >= BigDecimal("0.0")
  }

  it should "return corporate bond projection separately from market memory in runStep" in {
    val aligned = runOpenEcon(w)

    aligned.corpBonds.newCorpBonds.corpBondYield should be > Rate.Zero
    aligned.corpBonds.closingCorpBondProjection.outstanding should be > PLN.Zero
    aligned.corpBonds.closingCorpBondProjection.bankHoldings should be > PLN.Zero
    aligned.corpBonds.closingCorpBondProjection.ppkHoldings should be > PLN.Zero
    aligned.corpBonds.closingCorpBondProjection.otherHoldings should be >= PLN.Zero
    aligned.corpBonds.closingCorpBondProjection.insuranceHoldings should be > PLN.Zero
    aligned.corpBonds.closingCorpBondProjection.nbfiHoldings should be > PLN.Zero
  }

  it should "produce non-negative interbank flows" in {
    result.banking.totalReserveInterest should be >= PLN.Zero
  }

  it should "produce flows that close at SFC == 0L" in {
    val flowBop = result.external.flowBop
    val flows   = OpenEconFlows.emit(
      OpenEconFlows.Input(
        exports = flowBop.exports,
        imports = flowBop.totalImports,
        tourismExport = s6.tourismExport,
        tourismImport = s6.tourismImport,
        fdi = flowBop.fdi,
        portfolioFlows = flowBop.portfolioFlows,
        carryTradeFlow = flowBop.carryTradeFlow,
        primaryIncome = flowBop.primaryIncome,
        euFunds = flowBop.euFundsMonthly,
        diasporaInflow = s6.diasporaInflow,
        capitalFlightOutflow = flowBop.capitalFlightOutflow,
      ),
    )
    Interpreter.totalWealth(Interpreter.applyAll(Map.empty[Int, Long], flows)) shouldBe 0L
  }
