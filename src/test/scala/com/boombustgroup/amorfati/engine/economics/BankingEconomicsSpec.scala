package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.{MonthRandomness, OperationalSignals}
import com.boombustgroup.amorfati.engine.flows.*
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BankingEconomicsSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults
  private val TestSeed       = 42L

  "BankingEconomics (own Input)" should "produce flows that close at SFC == 0L" in {
    val init                 = WorldInit.initialize(InitRandomness.Contract.fromSeed(TestSeed))
    val w                    = init.world
    val ledgerFinancialState = init.ledgerFinancialState
    val stageRandomness      = MonthRandomness.Contract.fromSeed(TestSeed).stages.newStreams()

    val fiscal = FiscalConstraintEconomics.compute(w, init.banks, ExecutionMonth.First)
    val s1     = FiscalConstraintEconomics.toOutput(fiscal)
    val labor  = LaborEconomics.compute(w, init.firms, init.households, s1)
    val s2     = LaborEconomics.Output(
      labor.wage,
      labor.employed,
      labor.laborDemand,
      labor.wageGrowth,
      labor.operationalHiringSlack,
      labor.immigration,
      labor.netMigration,
      labor.demographics,
      SocialSecurity.ZusState.zero,
      SocialSecurity.NfzState.zero,
      SocialSecurity.PpkState.zero,
      PLN.Zero,
      EarmarkedFunds.State.zero,
      labor.living,
      labor.regionalWages,
    )
    val s3     =
      HouseholdIncomeEconomics.compute(
        w,
        init.firms,
        init.households,
        init.banks,
        s1.lendingBaseRate,
        s1.resWage,
        s2.newWage,
        stageRandomness.householdIncomeEconomics,
      )
    val s4     = DemandEconomics.compute(DemandEconomics.Input(w, s2.employed, s2.living, s3.domesticCons))
    val s5     = FirmEconomics.runStep(w, init.firms, init.households, init.banks, ledgerFinancialState, s1, s2, s3, s4, stageRandomness.firmEconomics)
    val s6     = HouseholdFinancialEconomics.compute(w, s1.m, s2.employed, s3.hhAgg, stageRandomness.householdFinancialEconomics)
    val s7     = PriceEquityEconomics.compute(
      PriceEquityEconomics.Input(
        w,
        s1.m,
        s2.newWage,
        s2.employed,
        s2.wageGrowth,
        s3.domesticCons,
        s4.govPurchases,
        s4.avgDemandMult,
        s4.sectorMults,
        init.banks,
        s5,
      ),
      stageRandomness.priceEquityEconomics,
    )
    val s8     = OpenEconEconomics.runStep(
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

    val bankingInput = BankingEconomics.Input(
      w = w,
      ledgerFinancialState = ledgerFinancialState,
      month = s1.m,
      lendingBaseRate = s1.lendingBaseRate,
      resWage = s1.resWage,
      baseMinWage = s1.baseMinWage,
      minWagePriceLevel = s1.updatedMinWagePriceLevel,
      employed = s2.employed,
      newWage = s2.newWage,
      laborDemand = s2.laborDemand,
      wageGrowth = s2.wageGrowth,
      govPurchases = s4.govPurchases,
      avgDemandMult = s4.avgDemandMult,
      sectorCapReal = s4.sectorCapReal,
      laggedInvestDemand = s4.laggedInvestDemand,
      fiscalRuleStatus = s4.fiscalRuleStatus,
      laborOutput = s2,
      operationalSignals = OperationalSignals(
        sectorDemandMult = s4.sectorMults,
        sectorDemandPressure = s4.sectorDemandPressure,
        sectorHiringSignal = s4.sectorHiringSignal,
        operationalHiringSlack = s2.operationalHiringSlack,
      ),
      hhOutput = s3,
      firmOutput = s5,
      hhFinancialOutput = s6,
      priceEquityOutput = s7,
      openEconOutput = s8,
      banks = init.banks,
      depositRng = stageRandomness.bankingEconomics,
    )
    val s9           = BankingEconomics.runStep(
      BankingEconomics.StepInput(
        w,
        bankingInput.ledgerFinancialState,
        s1,
        s2,
        s3,
        s4,
        s5,
        s6,
        s7,
        s8,
        init.banks,
        stageRandomness.bankingEconomics,
      ),
    )
    val res          = BankingEconomics.toResult(s9, bankingInput)

    s9.ledgerFinancialState.government.govBondOutstanding shouldBe s9.newGovWithYield.bondsOutstanding
    s9.ledgerFinancialState.foreign.govBondHoldings shouldBe s9.newGovWithYield.foreignBondHoldings
    s9.ledgerFinancialState.nbp.govBondHoldings shouldBe s9.finalNbp.govBondHoldings
    s9.ledgerFinancialState.insurance.govBondHoldings shouldBe s9.finalInsuranceStock.govBondHoldings
    s9.ledgerFinancialState.funds.ppkGovBondHoldings shouldBe s9.finalPpk.bondHoldings
    s9.ledgerFinancialState.funds.nbfi.tfiUnit shouldBe s9.finalNbfiStock.tfiAum
    s9.ledgerFinancialState.funds.quasiFiscal.bondsOutstanding should be >= PLN.Zero
    s9.ledgerFinancialState.funds.quasiFiscal.loanPortfolio should be >= PLN.Zero

    val flows = BankingFlows.emit(
      BankingFlows.Input(
        res.govBondIncome,
        res.reserveInterest,
        res.standingFacilityIncome,
        res.interbankInterest,
        s8.corpBonds.corpBondBankCoupon,
        s8.corpBonds.corpBondBankDefaultLoss,
        res.bfgLevy,
        res.unrealizedBondLoss,
        res.bailInLoss,
        res.nbpRemittance,
        PLN.Zero,
        res.standingFacilityBackstop,
      ),
    )

    Interpreter.totalWealth(Interpreter.applyAll(Map.empty[Int, Long], flows)).shouldBe(0L)
  }

  "BankingEconomics.distributeFxInjection" should "distribute exact positive injection by non-negative deposit weights" in {
    val banks  = Vector(
      initBank(0, deposits = PLN(600.0), reserves = PLN(10.0)),
      initBank(1, deposits = PLN(400.0), reserves = PLN(20.0)),
    )
    val result = BankingEconomics.distributeFxInjection(banks, PLN(101.0))
    val delta0 = result.banks(0).reservesAtNbp - banks(0).reservesAtNbp
    val delta1 = result.banks(1).reservesAtNbp - banks(1).reservesAtNbp
    result.standingFacilityBackstop shouldBe PLN.Zero
    result.residual shouldBe PLN.Zero
    delta0.toLong + delta1.toLong shouldBe PLN(101.0).toLong
    delta0 shouldBe PLN(60.6)
    delta1 shouldBe PLN(40.4)
  }

  it should "treat negative deposits as zero weight during PLN drain" in {
    val banks  = Vector(
      initBank(0, deposits = PLN(-50.0), reserves = PLN(30.0)),
      initBank(1, deposits = PLN(100.0), reserves = PLN(50.0)),
      initBank(2, deposits = PLN(300.0), reserves = PLN(90.0)),
    )
    val result = BankingEconomics.distributeFxInjection(banks, PLN(-40.0))
    val delta0 = result.banks(0).reservesAtNbp - banks(0).reservesAtNbp
    val delta1 = result.banks(1).reservesAtNbp - banks(1).reservesAtNbp
    val delta2 = result.banks(2).reservesAtNbp - banks(2).reservesAtNbp
    result.standingFacilityBackstop shouldBe PLN.Zero
    result.residual shouldBe PLN.Zero
    delta0 shouldBe PLN.Zero
    delta1 + delta2 shouldBe PLN(-40.0)
    delta1 shouldBe PLN(-10.0)
    delta2 shouldBe PLN(-30.0)
  }

  it should "surface the full residual when no bank has positive deposit weight" in {
    val banks  = Vector(
      initBank(0, deposits = PLN.Zero, reserves = PLN(30.0)),
      initBank(1, deposits = PLN(-10.0), reserves = PLN(40.0)),
    )
    val result = BankingEconomics.distributeFxInjection(banks, PLN(-25.0))

    result.banks shouldBe banks
    result.standingFacilityBackstop shouldBe PLN.Zero
    result.residual shouldBe PLN(-25.0)
  }

  "BankingEconomics.applyNbpReserveSettlement" should "apply reserve-side monetary settlement and FX injection to reserves" in {
    val banks   = Vector(
      initBank(0, deposits = PLN(600.0), reserves = PLN(10.0)),
      initBank(1, deposits = PLN(400.0), reserves = PLN(20.0)),
    )
    val reserve = Banking.PerBankAmounts(Vector(PLN(6.0), PLN(4.0)), PLN(10.0))
    val sf      = Banking.PerBankAmounts(Vector(PLN(3.0), PLN(-1.0)), PLN(2.0))
    val ib      = Banking.PerBankAmounts(Vector(PLN(-2.0), PLN(2.0)), PLN.Zero)
    val result  = BankingEconomics.applyNbpReserveSettlement(banks, reserve, sf, ib, PLN(100.0))

    val delta0 = result.banks(0).reservesAtNbp - banks(0).reservesAtNbp
    val delta1 = result.banks(1).reservesAtNbp - banks(1).reservesAtNbp

    result.standingFacilityBackstop shouldBe PLN.Zero
    result.residual shouldBe PLN.Zero
    delta0 shouldBe PLN(67.0)
    delta1 shouldBe PLN(45.0)
  }

  it should "convert reserve drain shortfall into explicit standing-facility backstop" in {
    val banks  = Vector(initBank(0, deposits = PLN(100.0), reserves = PLN(10.0)))
    val zeros  = Banking.PerBankAmounts(Vector(PLN.Zero), PLN.Zero)
    val drain  = Banking.PerBankAmounts(Vector(PLN(-15.0)), PLN(-15.0))
    val result = BankingEconomics.applyNbpReserveSettlement(banks, zeros, drain, zeros, PLN.Zero)

    result.banks.head.reservesAtNbp shouldBe PLN.Zero
    result.standingFacilityBackstop shouldBe PLN(5.0)
    result.residual shouldBe PLN.Zero
  }

  private def initBank(id: Int, deposits: PLN, reserves: PLN): Banking.BankState =
    Banking.BankState(
      id = BankId(id),
      deposits = deposits,
      loans = PLN.Zero,
      capital = PLN(100.0),
      nplAmount = PLN.Zero,
      afsBonds = PLN.Zero,
      htmBonds = PLN.Zero,
      htmBookYield = Rate.Zero,
      reservesAtNbp = reserves,
      interbankNet = PLN.Zero,
      status = Banking.BankStatus.Active(0),
      demandDeposits = deposits.max(PLN.Zero),
      termDeposits = PLN.Zero,
      loansShort = PLN.Zero,
      loansMedium = PLN.Zero,
      loansLong = PLN.Zero,
      consumerLoans = PLN.Zero,
      consumerNpl = PLN.Zero,
      corpBondHoldings = PLN.Zero,
    )
