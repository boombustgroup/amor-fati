package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.{MonthRandomness, World}
import com.boombustgroup.amorfati.engine.flows.*
import com.boombustgroup.amorfati.engine.ledger.{CorporateBondOwnership, LedgerFinancialState}
import com.boombustgroup.amorfati.engine.markets.FiscalBudget
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BankingEconomicsSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults
  private val TestSeed       = 42L

  "BankingEconomics.runStep" should "produce flows that close at SFC == 0L" in {
    val prepared    = preparedBankingStep()
    val s9          = prepared.run()
    val prevBankAgg = Banking.aggregateFromBankStocks(
      prepared.banks,
      prepared.ledgerFinancialState.banks.map(LedgerFinancialState.projectBankFinancialStocks),
      bankId => CorporateBondOwnership.bankHolderFor(prepared.ledgerFinancialState, bankId),
    )

    s9.ledgerFinancialState.government.govBondOutstanding shouldBe FiscalBudget.nextGovBondOutstanding(
      prepared.ledgerFinancialState.government.govBondOutstanding,
      s9.newGovWithYield.deficit,
    )
    s9.ledgerFinancialState.insurance.govBondHoldings shouldBe s9.finalInsuranceBalances.govBondHoldings
    s9.finalPpk shouldBe prepared.s2.newPpk
    s9.ledgerFinancialState.funds.ppkGovBondHoldings should be >= prepared.ledgerFinancialState.funds.ppkGovBondHoldings
    s9.ledgerFinancialState.funds.nbfi.tfiUnit shouldBe s9.finalNbfiBalances.tfiAum
    s9.ledgerFinancialState.funds.quasiFiscal.bondsOutstanding should be >= PLN.Zero
    s9.ledgerFinancialState.funds.quasiFiscal.loanPortfolio should be >= PLN.Zero

    val flows = BankingFlows.emit(
      BankingFlows.Input(
        firmInterestIncome = prepared.s5.intIncome,
        firmNplLoss = prepared.s5.nplLoss,
        mortgageNplLoss = s9.mortgageDefaultLoss,
        consumerNplLoss = prepared.s6.consumerNplLoss,
        govBondIncome = prevBankAgg.govBondHoldings * prepared.s8.monetary.newBondYield.monthly,
        reserveInterest = prepared.s8.banking.totalReserveInterest,
        standingFacilityIncome = prepared.s8.banking.totalStandingFacilityIncome,
        interbankInterest = prepared.s8.banking.totalInterbankInterest,
        corpBondCoupon = prepared.s8.corpBonds.corpBondBankCoupon,
        corpBondDefaultLoss = prepared.s8.corpBonds.corpBondBankDefaultLoss,
        bfgLevy = s9.bfgLevy,
        unrealizedBondLoss = s9.unrealizedBondLoss,
        bailInLoss = s9.bailInLoss,
        nbpRemittance = prepared.s8.banking.nbpRemittance,
        fxReserveSettlement = PLN.Zero,
        standingFacilityBackstop = s9.standingFacilityBackstop,
      ),
    )

    Interpreter.totalWealth(Interpreter.applyAll(Map.empty[Int, Long], flows)).shouldBe(0L)
  }

  it should "read JST cash opening stocks from LedgerFinancialState" in {
    val prepared = preparedBankingStep()
    val aligned  = prepared.run()

    val shiftedLedger = prepared.ledgerFinancialState.copy(
      funds = prepared.ledgerFinancialState.funds.copy(
        jstCash = prepared.ledgerFinancialState.funds.jstCash + PLN(555e9),
      ),
    )
    val fromLedger    = prepared.run(ledgerFinancialStateOverride = shiftedLedger)

    fromLedger.ledgerFinancialState.government.govBondOutstanding shouldBe aligned.ledgerFinancialState.government.govBondOutstanding
    fromLedger.newJst shouldBe aligned.newJst
    fromLedger.ledgerFinancialState.funds.jstCash shouldBe aligned.ledgerFinancialState.funds.jstCash + PLN(555e9)
  }

  private case class PreparedBankingStep(
      world: World,
      ledgerFinancialState: LedgerFinancialState,
      banks: Vector[Banking.BankState],
      s1: FiscalConstraintEconomics.Output,
      s2: LaborEconomics.Output,
      s3: HouseholdIncomeEconomics.Output,
      s4: DemandEconomics.Output,
      s5: FirmEconomics.StepOutput,
      s6: HouseholdFinancialEconomics.Output,
      s7: PriceEquityEconomics.Output,
      s8: OpenEconEconomics.StepOutput,
  ):
    def run(
        worldOverride: World = world,
        ledgerFinancialStateOverride: LedgerFinancialState = ledgerFinancialState,
        banksOverride: Vector[Banking.BankState] = banks,
    ): BankingEconomics.StepOutput =
      val bankingRng = MonthRandomness.Contract.fromSeed(TestSeed).stages.newStreams().bankingEconomics
      BankingEconomics.runStep(
        BankingEconomics.StepInput(
          worldOverride,
          ledgerFinancialStateOverride,
          s1,
          s2,
          s3,
          s4,
          s5,
          s6,
          s7,
          s8,
          banksOverride,
          bankingRng,
        ),
      )

  private def preparedBankingStep(): PreparedBankingStep =
    val init                 = WorldInit.initialize(InitRandomness.Contract.fromSeed(TestSeed))
    val w                    = init.world
    val ledgerFinancialState = init.ledgerFinancialState
    val stageRandomness      = MonthRandomness.Contract.fromSeed(TestSeed).stages.newStreams()

    val s1 = FiscalConstraintEconomics.compute(w, init.banks, ledgerFinancialState, ExecutionMonth.First)
    val s2 = LaborEconomics.compute(w, init.firms, init.households, s1)
    val s3 =
      HouseholdIncomeEconomics.compute(
        w,
        init.firms,
        init.households,
        init.banks,
        ledgerFinancialState,
        s1.lendingBaseRate,
        s1.resWage,
        s2.newWage,
        stageRandomness.householdIncomeEconomics,
      )
    val s4 = DemandEconomics.compute(w, s2.employed, s2.living, s3.domesticCons)
    val s5 = FirmEconomics.runStep(w, init.firms, init.households, init.banks, ledgerFinancialState, s1, s2, s3, s4, stageRandomness.firmEconomics)
    val s6 = HouseholdFinancialEconomics.compute(w, s1.m, s2.employed, s3.hhAgg, stageRandomness.householdFinancialEconomics)
    val s7 = PriceEquityEconomics.compute(
      w = w,
      month = s1.m,
      wageGrowth = s2.wageGrowth,
      domesticCons = s3.domesticCons,
      govPurchases = s4.govPurchases,
      avgDemandMult = s4.avgDemandMult,
      totalSystemLoans = ledgerFinancialState.banks.map(_.firmLoan).sum,
      firmStep = s5,
    )
    val s8 = OpenEconEconomics.runStep(
      OpenEconEconomics.StepInput(
        w,
        ledgerFinancialState,
        s1,
        s2,
        s3,
        s4,
        s5,
        s6,
        s7,
        init.banks,
        stageRandomness.openEconEconomics,
      ),
    )

    PreparedBankingStep(w, ledgerFinancialState, init.banks, s1, s2, s3, s4, s5, s6, s7, s8)

  "BankingEconomics.distributeFxInjection" should "distribute exact positive injection by non-negative deposit weights" in {
    val rows   = Vector(
      initBank(0, deposits = PLN(600.0), reserves = PLN(10.0)),
      initBank(1, deposits = PLN(400.0), reserves = PLN(20.0)),
    )
    val result = BankingEconomics.distributeFxInjection(rows.map(_._1), rows.map(_._2), PLN(101.0))
    val delta0 = result.financialStocks(0).reserve - rows(0)._2.reserve
    val delta1 = result.financialStocks(1).reserve - rows(1)._2.reserve
    result.standingFacilityBackstop shouldBe PLN.Zero
    result.residual shouldBe PLN.Zero
    delta0.toLong + delta1.toLong shouldBe PLN(101.0).toLong
    delta0 shouldBe PLN(60.6)
    delta1 shouldBe PLN(40.4)
  }

  it should "treat negative deposits as zero weight during PLN drain" in {
    val rows   = Vector(
      initBank(0, deposits = PLN(-50.0), reserves = PLN(30.0)),
      initBank(1, deposits = PLN(100.0), reserves = PLN(50.0)),
      initBank(2, deposits = PLN(300.0), reserves = PLN(90.0)),
    )
    val result = BankingEconomics.distributeFxInjection(rows.map(_._1), rows.map(_._2), PLN(-40.0))
    val delta0 = result.financialStocks(0).reserve - rows(0)._2.reserve
    val delta1 = result.financialStocks(1).reserve - rows(1)._2.reserve
    val delta2 = result.financialStocks(2).reserve - rows(2)._2.reserve
    result.standingFacilityBackstop shouldBe PLN.Zero
    result.residual shouldBe PLN.Zero
    delta0 shouldBe PLN.Zero
    delta1 + delta2 shouldBe PLN(-40.0)
    delta1 shouldBe PLN(-10.0)
    delta2 shouldBe PLN(-30.0)
  }

  it should "surface the full residual when no bank has positive deposit weight" in {
    val rows   = Vector(
      initBank(0, deposits = PLN.Zero, reserves = PLN(30.0)),
      initBank(1, deposits = PLN(-10.0), reserves = PLN(40.0)),
    )
    val result = BankingEconomics.distributeFxInjection(rows.map(_._1), rows.map(_._2), PLN(-25.0))

    result.banks shouldBe rows.map(_._1)
    result.financialStocks shouldBe rows.map(_._2)
    result.standingFacilityBackstop shouldBe PLN.Zero
    result.residual shouldBe PLN(-25.0)
  }

  "BankingEconomics.applyNbpReserveSettlement" should "apply reserve-side monetary settlement and FX injection to reserves" in {
    val rows    = Vector(
      initBank(0, deposits = PLN(600.0), reserves = PLN(10.0)),
      initBank(1, deposits = PLN(400.0), reserves = PLN(20.0)),
    )
    val reserve = Banking.PerBankAmounts(Vector(PLN(6.0), PLN(4.0)), PLN(10.0))
    val sf      = Banking.PerBankAmounts(Vector(PLN(3.0), PLN(-1.0)), PLN(2.0))
    val ib      = Banking.PerBankAmounts(Vector(PLN(-2.0), PLN(2.0)), PLN.Zero)
    val result  = BankingEconomics.applyNbpReserveSettlement(rows.map(_._1), rows.map(_._2), reserve, sf, ib, PLN(100.0))

    val delta0 = result.financialStocks(0).reserve - rows(0)._2.reserve
    val delta1 = result.financialStocks(1).reserve - rows(1)._2.reserve

    result.standingFacilityBackstop shouldBe PLN.Zero
    result.residual shouldBe PLN.Zero
    delta0 shouldBe PLN(67.0)
    delta1 shouldBe PLN(45.0)
  }

  it should "convert reserve drain shortfall into explicit standing-facility backstop" in {
    val rows   = Vector(initBank(0, deposits = PLN(100.0), reserves = PLN(10.0)))
    val zeros  = Banking.PerBankAmounts(Vector(PLN.Zero), PLN.Zero)
    val drain  = Banking.PerBankAmounts(Vector(PLN(-15.0)), PLN(-15.0))
    val result = BankingEconomics.applyNbpReserveSettlement(rows.map(_._1), rows.map(_._2), zeros, drain, zeros, PLN.Zero)

    result.financialStocks.head.reserve shouldBe PLN.Zero
    result.standingFacilityBackstop shouldBe PLN(5.0)
    result.residual shouldBe PLN.Zero
  }

  private def initBank(id: Int, deposits: PLN, reserves: PLN): (Banking.BankState, Banking.BankFinancialStocks) =
    (
      Banking.BankState(
        id = BankId(id),
        capital = PLN(100.0),
        nplAmount = PLN.Zero,
        htmBookYield = Rate.Zero,
        status = Banking.BankStatus.Active(0),
        loansShort = PLN.Zero,
        loansMedium = PLN.Zero,
        loansLong = PLN.Zero,
        consumerNpl = PLN.Zero,
      ),
      Banking.BankFinancialStocks(
        totalDeposits = deposits,
        firmLoan = PLN.Zero,
        govBondAfs = PLN.Zero,
        govBondHtm = PLN.Zero,
        reserve = reserves,
        interbankLoan = PLN.Zero,
        demandDeposit = deposits.max(PLN.Zero),
        termDeposit = PLN.Zero,
        consumerLoan = PLN.Zero,
      ),
    )
