package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.accounting.Sfc
import com.boombustgroup.amorfati.agents.{ContractType, HhStatus, Household, SocialSecurity}
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.{
  DecisionSignals,
  MonthDriver,
  MonthRandomness,
  MonthSemantics,
  MonthTimingEnvelopeKey,
  MonthTimingPayload,
  MonthTraceStage,
  SimulationMonth,
}
import com.boombustgroup.amorfati.engine.ledger.CorporateBondOwnership
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import com.boombustgroup.amorfati.tags.Heavy
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.{AssetType, BatchedFlow, EntitySector, ImperativeInterpreter, Interpreter}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

@Heavy
class FlowSimulationStepSpec extends AnyFlatSpec with Matchers:
  import RuntimeFlowsTestSupport.*

  private given p: SimParams = SimParams.defaults

  private def canonicalHouseholds(households: Vector[Household.State]): Vector[Vector[Any]] =
    households.map: hh =>
      hh.productIterator
        .map:
          case arr: Array[?] => arr.toIndexedSeq
          case value         => value
        .toVector

  "FlowSimulation.step" should "produce SFC == 0L on real World" in {
    val init   = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state  = FlowSimulation.SimState.fromInit(init)
    val result = FlowSimulation.step(state, MonthRandomness.Contract.fromSeed(42L))
    result.execution.netDelta shouldBe 0L
    result.sfcResult shouldBe Right(())
    result.calculus.employed should be > 0
  }

  it should "emit equity issuance once from current-month firm financing" in {
    val init          = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val baseState     = FlowSimulation.SimState.fromInit(init)
    val staleIssuance = PLN(987654321.0)
    val state         = baseState.copy(
      world = baseState.world.copy(
        financialMarkets = baseState.world.financialMarkets.copy(
          equity = baseState.world.financialMarkets.equity.copy(lastIssuance = staleIssuance),
        ),
      ),
    )

    val result                = FlowSimulation.step(state, MonthRandomness.Contract.fromSeed(42L))
    val firmIssuance          = mechanismTotal(result.flows, FlowMechanism.FirmEquityIssuance)
    val equityIssuanceBatches = result.flows.filter(batch => batch.asset == AssetType.Equity && batch.to == EntitySector.Firms)

    equityIssuanceBatches.map(_.mechanism).toSet shouldBe Set(FlowMechanism.FirmEquityIssuance)
    firmIssuance shouldBe result.calculus.firmEquityIssuance
    firmIssuance shouldBe result.nextState.world.financialMarkets.equity.lastIssuance
    firmIssuance should not equal staleIssuance
  }

  it should "emit equity cash legs from current-month dividends instead of boundary last values" in {
    val init             = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val baseState        = FlowSimulation.SimState.fromInit(init)
    val staleDomestic    = PLN(987654321.0)
    val staleForeign     = PLN(876543210.0)
    val staleDividendTax = PLN(765432109.0)
    val staleGovDividend = PLN(654321098.0)
    val state            = baseState.copy(
      world = baseState.world.copy(
        financialMarkets = baseState.world.financialMarkets.copy(
          equity = baseState.world.financialMarkets.equity.copy(
            lastDomesticDividends = staleDomestic,
            lastForeignDividends = staleForeign,
            lastDividendTax = staleDividendTax,
          ),
        ),
        gov = baseState.world.gov.copy(
          monthly = baseState.world.gov.monthly.copy(
            govDividendRevenue = staleGovDividend,
          ),
        ),
      ),
    )

    val result = FlowSimulation.step(state, MonthRandomness.Contract.fromSeed(42L))
    val equity = result.nextState.world.financialMarkets.equity

    result.calculus.equityDomDividends shouldBe equity.lastDomesticDividends
    result.calculus.equityForDividends shouldBe equity.lastForeignDividends
    result.calculus.equityDivTax shouldBe equity.lastDividendTax
    result.calculus.equityGovDividends shouldBe result.nextState.world.gov.govDividendRevenue

    result.calculus.equityDomDividends should not equal staleDomestic
    result.calculus.equityForDividends should not equal staleForeign
    result.calculus.equityDivTax should not equal staleDividendTax
    result.calculus.equityGovDividends should not equal staleGovDividend

    mechanismTotal(result.flows, FlowMechanism.EquityDomDividend) shouldBe result.calculus.equityDomDividends
    mechanismTotal(result.flows, FlowMechanism.EquityForDividend) shouldBe result.calculus.equityForDividends
    mechanismTotal(result.flows, FlowMechanism.EquityDividendTax) shouldBe result.calculus.equityDivTax
    mechanismTotal(result.flows, FlowMechanism.EquityGovDividend) shouldBe result.calculus.equityGovDividends

    result.trace.executedFlows.dividendIncome shouldBe mechanismTotal(result.flows, FlowMechanism.EquityDomDividend)
    result.trace.executedFlows.foreignDividendOutflow shouldBe mechanismTotal(result.flows, FlowMechanism.EquityForDividend)
    result.trace.executedFlows.dividendTax shouldBe mechanismTotal(result.flows, FlowMechanism.EquityDividendTax)
  }

  it should "emit insurance investment income from same-month equity return instead of boundary market memory" in {
    val init              = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val baseState         = FlowSimulation.SimState.fromInit(init)
    val staleEquityReturn = Rate(0.42)
    val state             = baseState.copy(
      world = baseState.world.copy(
        financialMarkets = baseState.world.financialMarkets.copy(
          equity = baseState.world.financialMarkets.equity.copy(monthlyReturn = staleEquityReturn),
        ),
      ),
    )

    val result = FlowSimulation.step(state, MonthRandomness.Contract.fromSeed(42L))

    result.calculus.equityReturn shouldBe result.nextState.world.financialMarkets.equity.monthlyReturn
    result.calculus.equityReturn should not equal staleEquityReturn

    val insuranceInvestmentBatches = mechanismBatches(result.flows, FlowMechanism.InsInvestmentIncome)
    insuranceInvestmentBatches should not be empty
    result.calculus.insurancePrevEquity should be > PLN.Zero

    val sameMonthInvestmentIncome     =
      result.calculus.insurancePrevGovBonds * result.calculus.govBondYield.monthly +
        result.calculus.insurancePrevCorpBonds * result.calculus.corpBondYield.monthly +
        result.calculus.insurancePrevEquity * result.calculus.equityReturn -
        result.calculus.insuranceCorpBondDefaultLoss
    val staleBoundaryInvestmentIncome =
      result.calculus.insurancePrevGovBonds * result.calculus.govBondYield.monthly +
        result.calculus.insurancePrevCorpBonds * result.calculus.corpBondYield.monthly +
        result.calculus.insurancePrevEquity * staleEquityReturn -
        result.calculus.insuranceCorpBondDefaultLoss

    totalTransferred(insuranceInvestmentBatches) shouldBe sameMonthInvestmentIncome.abs
    totalTransferred(insuranceInvestmentBatches) should not equal staleBoundaryInvestmentIncome.abs
  }

  it should "emit government and JST flows from current-month fiscal state instead of boundary fields" in {
    val init               = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val baseState          = FlowSimulation.SimState.fromInit(init)
    val staleDebtService   = PLN(987654321.0)
    val staleEuCofinancing = PLN(876543210.0)
    val staleCapitalSpend  = PLN(765432109.0)
    val staleJstRevenue    = PLN(654321098.0)
    val staleJstSpending   = PLN(543210987.0)
    val state              = baseState.copy(
      world = baseState.world.copy(
        gov = baseState.world.gov.copy(
          monthly = baseState.world.gov.monthly.copy(
            debtServiceSpend = staleDebtService,
            euCofinancing = staleEuCofinancing,
            govCapitalSpend = staleCapitalSpend,
          ),
        ),
        social = baseState.world.social.copy(
          jst = baseState.world.social.jst.copy(
            revenue = staleJstRevenue,
            spending = staleJstSpending,
          ),
        ),
      ),
    )

    val result  = FlowSimulation.step(state, MonthRandomness.Contract.fromSeed(42L))
    val nextGov = result.nextState.world.gov
    val nextJst = result.nextState.world.social.jst

    result.sfcResult shouldBe Right(())
    result.calculus.govCurrentSpend shouldBe nextGov.govCurrentSpend
    result.calculus.govDebtService shouldBe nextGov.debtServiceSpend
    result.calculus.govEuCofinancing shouldBe nextGov.euCofinancing
    result.calculus.govCapitalSpend shouldBe nextGov.govCapitalSpend

    result.calculus.govDebtService should not equal staleDebtService
    result.calculus.govEuCofinancing should not equal staleEuCofinancing
    result.calculus.govCapitalSpend should not equal staleCapitalSpend

    mechanismTotal(result.flows, FlowMechanism.GovPurchases) shouldBe nextGov.govCurrentSpend
    mechanismTotal(result.flows, FlowMechanism.GovDebtService) shouldBe result.calculus.govDebtService
    mechanismTotal(result.flows, FlowMechanism.GovEuCofin) shouldBe result.calculus.govEuCofinancing
    mechanismTotal(result.flows, FlowMechanism.GovCapitalInvestment) shouldBe result.calculus.govCapitalSpend
    mechanismTotal(result.flows, FlowMechanism.GovUnempBenefit) shouldBe result.calculus.totalUnempBenefits
    mechanismTotal(result.flows, FlowMechanism.GovSocialTransfer) shouldBe result.calculus.totalSocialTransfers

    val emittedJstRevenue =
      FlowSimulation.ExecutedFlowEvidence.JstRevenueMechanisms.map(mechanismTotal(result.flows, _)).foldLeft(PLN.Zero)(_ + _)
    emittedJstRevenue shouldBe nextJst.revenue
    mechanismTotal(result.flows, FlowMechanism.JstSpending) shouldBe nextJst.spending
    emittedJstRevenue should not equal staleJstRevenue
    mechanismTotal(result.flows, FlowMechanism.JstSpending) should not equal staleJstSpending

    val emittedGovSpending =
      (FlowSimulation.ExecutedFlowEvidence.CentralGovernmentSpendingMechanisms ++
        FlowSimulation.ExecutedFlowEvidence.SocialFundGovSubventionMechanisms)
        .map(mechanismTotal(result.flows, _))
        .foldLeft(PLN.Zero)(_ + _)

    result.trace.executedFlows.govSpending shouldBe emittedGovSpending
    result.trace.executedFlows.totalIncome shouldBe mechanismTotal(result.flows, FlowMechanism.HhTotalIncome)
    result.trace.executedFlows.jstRevenue shouldBe emittedJstRevenue
    result.trace.executedFlows.jstSpending shouldBe mechanismTotal(result.flows, FlowMechanism.JstSpending)
    result.trace.executedFlows.jstDepositChange shouldBe emittedJstRevenue - mechanismTotal(result.flows, FlowMechanism.JstSpending)
  }

  it should "align NFZ runtime subvention emission with semantic current-month state" in {
    val init   = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state  = FlowSimulation.SimState.fromInit(init)
    val result = FlowSimulation.step(state, MonthRandomness.Contract.fromSeed(42L))
    val nfz    = result.nextState.world.social.nfz

    val emittedContributions = mechanismTotal(result.flows, FlowMechanism.NfzContribution)
    val emittedSpending      = mechanismTotal(result.flows, FlowMechanism.NfzSpending)
    val emittedSubvention    = mechanismTotal(result.flows, FlowMechanism.NfzGovSubvention)

    result.sfcResult shouldBe Right(())
    emittedSubvention should be > PLN.Zero
    emittedContributions shouldBe nfz.contributions
    emittedSpending shouldBe nfz.spending
    emittedSubvention shouldBe nfz.govSubvention

    result.trace.executedFlows.nfzContributions shouldBe nfz.contributions
    result.trace.executedFlows.nfzSpending shouldBe nfz.spending
    result.trace.executedFlows.nfzGovSubvention shouldBe nfz.govSubvention

    val expectedGovSpending =
      (FlowSimulation.ExecutedFlowEvidence.CentralGovernmentSpendingMechanisms ++
        FlowSimulation.ExecutedFlowEvidence.SocialFundGovSubventionMechanisms)
        .map(mechanismTotal(result.flows, _))
        .foldLeft(PLN.Zero)(_ + _)

    result.trace.executedFlows.govSpending shouldBe expectedGovSpending
  }

  it should "anchor same-month social payroll before month-end hiring contract changes" in {
    val init      = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val baseState = FlowSimulation.SimState.fromInit(init)
    val state     = baseState.copy(
      households = baseState.households.zipWithIndex.map:
        case (hh, idx) if idx < 24 =>
          hh.copy(status = HhStatus.Unemployed(0), contractType = ContractType.Permanent)
        case (hh, _)               => hh,
    )

    val result               = FlowSimulation.step(state, MonthRandomness.Contract.fromSeed(42L))
    val finalPayroll         = SocialSecurity.payrollBase(result.nextState.households)
    val hiredWithNewContract = state.households
      .zip(result.nextState.households)
      .exists:
        case (before, after) =>
          before.status == HhStatus.Unemployed(0) &&
          after.status.isInstanceOf[HhStatus.Employed] &&
          after.contractType != before.contractType

    hiredWithNewContract shouldBe true
    result.calculus.zus.contributions shouldBe result.calculus.payroll.zusContributions
    result.calculus.nfz.contributions shouldBe result.calculus.payroll.nfzContributions
    result.calculus.ppk.contributions shouldBe result.calculus.payroll.ppkContributions
    mechanismTotal(result.flows, FlowMechanism.ZusContribution) shouldBe result.calculus.payroll.zusContributions
    mechanismTotal(result.flows, FlowMechanism.NfzContribution) shouldBe result.calculus.payroll.nfzContributions
    mechanismTotal(result.flows, FlowMechanism.PpkContribution) shouldBe result.calculus.payroll.ppkContributions
    result.calculus.payroll.zusContributions should not equal finalPayroll.zusContributions
  }

  it should "keep corporate bond outstanding ledger-owned across month boundaries" in {
    val init        = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state       = FlowSimulation.SimState.fromInit(init)
    val initialDebt = CorporateBondOwnership.issuerOutstanding(state.ledgerFinancialState)

    initialDebt shouldBe CorporateBondOwnership.issuerOutstanding(state.ledgerFinancialState.firms)
    initialDebt shouldBe CorporateBondOwnership.holderOutstanding(state.ledgerFinancialState)

    val result   = FlowSimulation.step(state, MonthRandomness.Contract.fromSeed(42L))
    val nextDebt = CorporateBondOwnership.issuerOutstanding(result.nextState.ledgerFinancialState)

    nextDebt shouldBe CorporateBondOwnership.issuerOutstanding(result.nextState.ledgerFinancialState.firms)
    nextDebt shouldBe CorporateBondOwnership.holderOutstanding(result.nextState.ledgerFinancialState)
  }

  it should "derive runtime ledger topology from actual populations plus explicit shell slots" in {
    val init     = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state    = FlowSimulation.SimState.fromInit(init)
    val result   = FlowSimulation.step(state, MonthRandomness.Contract.fromSeed(42L))
    val topology = result.execution.topology

    val expectedHouseholdSectorSize = state.households.size + 5
    val expectedFirmSectorSize      = state.firms.size + 5
    val expectedBankSectorSize      = state.banks.size + 1

    topology.households.aggregate shouldBe state.households.size
    topology.households.mortgagePrincipalSettlement shouldBe state.households.size + 4
    topology.households.sectorSize shouldBe expectedHouseholdSectorSize
    topology.firms.aggregate shouldBe state.firms.size
    topology.firms.sectorSize shouldBe expectedFirmSectorSize
    topology.banks.aggregate shouldBe state.banks.size
    topology.banks.sectorSize shouldBe expectedBankSectorSize
    topology.government.sovereignIssuer shouldBe 0
    topology.government.treasuryBudgetSettlement shouldBe 1
    topology.government.taxpayerCollection shouldBe 2
    topology.nbp.persistedOwner shouldBe 0
    topology.nbp.reserveSettlement shouldBe 1
    topology.sectorSizes shouldBe Map(
      EntitySector.Households -> expectedHouseholdSectorSize,
      EntitySector.Firms      -> expectedFirmSectorSize,
      EntitySector.Banks      -> expectedBankSectorSize,
      EntitySector.Government -> 3,
      EntitySector.NBP        -> 2,
      EntitySector.Insurance  -> 2,
      EntitySector.Funds      -> 12,
      EntitySector.Foreign    -> 5,
    )

    result.flows.foreach {
      case scatter: BatchedFlow.Scatter     =>
        scatter.amounts.length shouldBe topology.sectorSizes(scatter.from)
        scatter.amounts.length shouldBe scatter.targetIndices.length
        all(scatter.targetIndices.map(index => index >= 0 && index < topology.sectorSizes(scatter.to))) shouldBe true
      case broadcast: BatchedFlow.Broadcast =>
        broadcast.amounts.length shouldBe broadcast.targetIndices.length
        broadcast.fromIndex should (be >= 0 and be < topology.sectorSizes(broadcast.from))
        all(broadcast.targetIndices.map(index => index >= 0 && index < topology.sectorSizes(broadcast.to))) shouldBe true
    }
  }

  it should "avoid cash assets on bank capital and insurance investment channels" in {
    val init   = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state  = FlowSimulation.SimState.fromInit(init)
    val result = FlowSimulation.step(state, MonthRandomness.Contract.fromSeed(42L))

    val bankCapitalMechanisms = Set(
      FlowMechanism.BankGovBondIncome,
      FlowMechanism.BankCorpBondCoupon,
      FlowMechanism.BankCorpBondLoss,
      FlowMechanism.BankBfgLevy,
      FlowMechanism.BankUnrealizedLoss,
      FlowMechanism.BankNbpRemittance,
    )

    val bankCapitalBatches = result.flows.filter(batch => bankCapitalMechanisms.contains(batch.mechanism))
    bankCapitalBatches should not be empty
    all(bankCapitalBatches.map(_.asset)) shouldBe AssetType.Capital

    val insuranceIncomeBatches = result.flows.filter(_.mechanism == FlowMechanism.InsInvestmentIncome)
    insuranceIncomeBatches should not be empty
    insuranceIncomeBatches.map(_.asset).toSet shouldBe Set(AssetType.LifeReserve, AssetType.NonLifeReserve)
    all(insuranceIncomeBatches.map(_.from)) shouldBe EntitySector.Insurance
    all(insuranceIncomeBatches.map(_.to)) shouldBe EntitySector.Insurance

    val insuranceReserveBatches = result.flows.filter(batch => batch.asset == AssetType.LifeReserve || batch.asset == AssetType.NonLifeReserve)
    insuranceReserveBatches should not be empty
    all(insuranceReserveBatches.map(_.from)) shouldBe EntitySector.Insurance
    all(insuranceReserveBatches.map(_.to)) shouldBe EntitySector.Insurance

    val insurancePremiums = mechanismTotal(result.flows, FlowMechanism.InsLifePremium) +
      mechanismTotal(result.flows, FlowMechanism.InsNonLifePremium)
    val insuranceClaims   = mechanismTotal(result.flows, FlowMechanism.InsLifeClaim) +
      mechanismTotal(result.flows, FlowMechanism.InsNonLifeClaim)

    result.trace.executedFlows.insNetDepositChange shouldBe insuranceClaims - insurancePremiums
    result.trace.executedFlows.insNetDepositChange shouldBe result.nextState.world.financialMarkets.insurance.lastNetDepositChange
  }

  it should "read insurance flow inputs from LedgerFinancialState instead of World market memory" in {
    val init            = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val baseState       = FlowSimulation.SimState.fromInit(init)
    val ledgerInsurance = baseState.ledgerFinancialState.insurance.copy(
      lifeReserve = PLN(21),
      nonLifeReserve = PLN(22),
      govBondHoldings = PLN(23),
      corpBondHoldings = PLN(24),
      equityHoldings = PLN(25),
    )
    val state           = baseState.copy(
      ledgerFinancialState = baseState.ledgerFinancialState.copy(
        insurance = ledgerInsurance,
      ),
    )

    val calculus = FlowSimulation.computeCalculus(state, MonthRandomness.Contract.fromSeed(42L))

    calculus.insuranceCurrentLifeReserves shouldBe ledgerInsurance.lifeReserve
    calculus.insuranceCurrentNonLifeReserves shouldBe ledgerInsurance.nonLifeReserve
    calculus.insurancePrevGovBonds shouldBe ledgerInsurance.govBondHoldings
    calculus.insurancePrevCorpBonds shouldBe ledgerInsurance.corpBondHoldings
    calculus.insurancePrevEquity shouldBe ledgerInsurance.equityHoldings
  }

  it should "expose the month boundary as SimState -> StepOutput -> (nextState, trace)" in {
    val init        = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state       = FlowSimulation.SimState.fromInit(init)
    val contract    = MonthRandomness.Contract.fromSeed(42L)
    val stateResult = FlowSimulation.step(state, contract)
    val repeated    = FlowSimulation.step(state, contract)

    stateResult.stateIn shouldBe state
    stateResult.transition shouldBe ((stateResult.nextState, stateResult.trace))
    stateResult.nextState.world shouldBe repeated.nextState.world
    stateResult.nextState.firms shouldBe repeated.nextState.firms
    canonicalHouseholds(stateResult.nextState.households) shouldBe canonicalHouseholds(repeated.nextState.households)
    stateResult.nextState.banks shouldBe repeated.nextState.banks
    stateResult.nextState.householdAggregates shouldBe repeated.nextState.householdAggregates
    stateResult.trace shouldBe repeated.trace
  }

  it should "keep explicit pre and next-pre seed wrappers aligned with step outputs" in {
    val init    = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state   = FlowSimulation.SimState.fromInit(init)
    val seedIn  = MonthSemantics.seedIn(state.world.seedIn)
    val result  = FlowSimulation.step(state, MonthRandomness.Contract.fromSeed(42L))
    val seedOut = MonthSemantics.seedOut(result.signalExtraction)

    seedIn.decisionSignals shouldBe state.world.seedIn
    seedOut.signalExtraction shouldBe result.signalExtraction
    seedOut.nextSeed shouldBe result.nextState.world.seedIn
    result.trace.seedTransition.seedIn shouldBe seedIn.decisionSignals
    result.trace.seedTransition.seedOut shouldBe seedOut.nextSeed
  }

  it should "accept an explicit month-step randomness contract with named stage and assembly streams" in {
    val init       = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state      = FlowSimulation.SimState.fromInit(init)
    val contract   = MonthRandomness.Contract.fromSeed(1234L)
    val fromSeed   = FlowSimulation.step(state, contract)
    val fromReplay = FlowSimulation.step(state, MonthRandomness.Contract.fromSeed(1234L))

    fromSeed.randomness shouldBe contract
    fromSeed.trace.randomness shouldBe contract
    fromSeed.trace.randomness.all.map(_.key).toSet shouldBe Set(
      MonthRandomness.StreamKey.HouseholdIncomeEconomics,
      MonthRandomness.StreamKey.FirmEconomics,
      MonthRandomness.StreamKey.HouseholdFinancialEconomics,
      MonthRandomness.StreamKey.OpenEconEconomics,
      MonthRandomness.StreamKey.BankingEconomics,
      MonthRandomness.StreamKey.FdiMa,
      MonthRandomness.StreamKey.FirmEntry,
      MonthRandomness.StreamKey.StartupStaffing,
      MonthRandomness.StreamKey.RegionalMigration,
    )
    fromSeed.trace.randomness.all.map(_.seed).distinct should have size fromSeed.trace.randomness.all.size
    fromSeed.nextState.world shouldBe fromReplay.nextState.world
    fromSeed.nextState.firms shouldBe fromReplay.nextState.firms
    canonicalHouseholds(fromSeed.nextState.households) shouldBe canonicalHouseholds(fromReplay.nextState.households)
    fromSeed.nextState.banks shouldBe fromReplay.nextState.banks
    fromSeed.trace shouldBe fromReplay.trace
  }

  it should "expose a first-class unfold driver over the monthly step boundary" in {
    val init  = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state = FlowSimulation.SimState.fromInit(init)
    val steps = MonthDriver
      .unfoldSteps(state): current =>
        Some(MonthRandomness.Contract.fromSeed(42L * 1000L + current.completedMonth.toLong + 1L))
      .take(3)
      .toVector

    steps should have size 3
    steps.map(_.stateIn.completedMonth.toInt) shouldBe Vector(0, 1, 2)
    steps.map(_.executionMonth.toInt) shouldBe Vector(1, 2, 3)
    steps.map(_.nextState.completedMonth.toInt) shouldBe Vector(1, 2, 3)
    steps.foreach(_.sfcResult shouldBe Right(()))
  }

  it should "stop unfolding when the caller closes the randomness schedule" in {
    val init    = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state   = FlowSimulation.SimState.fromInit(init)
    val results = MonthDriver
      .unfoldSteps(state): current =>
        Option.when(current.completedMonth.toInt < 2)(MonthRandomness.Contract.fromSeed(42L * 1000L + current.completedMonth.toLong + 1L))
      .toVector

    results should have size 2
    results.map(_.nextState.completedMonth.toInt) shouldBe Vector(1, 2)
  }

  it should "produce SFC == 0L across 12 months (autonomous driving)" in {
    val init    = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state   = FlowSimulation.SimState.fromInit(init)
    val results = MonthDriver
      .unfoldSteps(state): current =>
        Some(MonthRandomness.Contract.fromSeed(42L * 1000L + current.completedMonth.toLong + 1L))
      .take(12)
      .toVector

    results should have size 12

    results.foreach { result =>
      val month = result.executionMonth.toInt
      withClue(s"Month $month: ") {
        result.execution.netDelta shouldBe 0L
        result.sfcResult shouldBe Right(())
      }
    }
  }

  it should "propagate informal-economy pressure into fiscal outputs without breaking SFC" in {
    val init          = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val baseState     = FlowSimulation.SimState.fromInit(init)
    val lowShadowW    = init.world.copy(mechanisms = init.world.mechanisms.copy(informalCyclicalAdj = 0.0))
    val highShadowW   = init.world.copy(mechanisms = init.world.mechanisms.copy(informalCyclicalAdj = 0.4))
    val lowShadowRun  = FlowSimulation.step(
      baseState.copy(world = lowShadowW),
      MonthRandomness.Contract.fromSeed(42L),
    )
    val highShadowRun = FlowSimulation.step(
      baseState.copy(world = highShadowW),
      MonthRandomness.Contract.fromSeed(42L),
    )

    lowShadowRun.execution.netDelta shouldBe 0L
    highShadowRun.execution.netDelta shouldBe 0L
    lowShadowRun.sfcResult shouldBe Right(())
    highShadowRun.sfcResult shouldBe Right(())

    highShadowRun.nextState.world.flows.realizedTaxShadowShare should be > lowShadowRun.nextState.world.flows.realizedTaxShadowShare
    highShadowRun.nextState.world.flows.taxEvasionLoss should be > lowShadowRun.nextState.world.flows.taxEvasionLoss
    highShadowRun.nextState.world.gov.taxRevenue should be < lowShadowRun.nextState.world.gov.taxRevenue
    highShadowRun.nextState.world.gov.deficit should be > lowShadowRun.nextState.world.gov.deficit
  }

  it should "align semantic gov revenue with the emitted current-month SOE dividend batch" in {
    val init                    = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val staleBoundaryDividend   = PLN(25e6)
    val baseState               = FlowSimulation.SimState.fromInit(init)
    val state                   = baseState.copy(
      world = baseState.world.copy(
        gov = baseState.world.gov.copy(
          monthly = baseState.world.gov.monthly.copy(govDividendRevenue = staleBoundaryDividend),
        ),
      ),
    )
    val result                  = FlowSimulation.step(state, MonthRandomness.Contract.fromSeed(42L))
    val currentMonthGovDividend = mechanismTotal(result.flows, FlowMechanism.EquityGovDividend)
    val emittedGovRevenue       = GovBudgetFlows.CentralGovernmentRevenueMechanisms
      .map(mechanismTotal(result.flows, _))
      .foldLeft(PLN.Zero)(_ + _)

    result.sfcResult shouldBe Right(())
    currentMonthGovDividend shouldBe result.calculus.equityGovDividends
    currentMonthGovDividend should not equal staleBoundaryDividend
    result.trace.executedFlows.govRevenue shouldBe emittedGovRevenue
  }

  it should "produce 30+ mechanism IDs" in {
    val init   = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state  = FlowSimulation.SimState.fromInit(init)
    val result = FlowSimulation.step(state, MonthRandomness.Contract.fromSeed(42L))
    result.flows.map(_.mechanism).toSet.size should be > 30
  }

  it should "emit a typed MonthTrace with stable boundaries and typed timing envelopes" in {
    val init   = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state  = FlowSimulation.SimState.fromInit(init)
    val result = FlowSimulation.step(state, MonthRandomness.Contract.fromSeed(42L))
    val trace  = result.trace

    val demandSignals = trace.timing.requirePayload[MonthTimingPayload.DemandSignals](MonthTimingEnvelopeKey.Demand)

    trace.executionMonth shouldBe result.executionMonth
    trace.seedTransition.seedIn shouldBe init.world.seedIn
    trace.seedTransition.seedOut shouldBe result.nextState.world.seedIn
    trace.randomness shouldBe result.randomness
    trace.boundary.startSnapshot.stock shouldBe Sfc.snapshot(init.world, init.firms, init.households, init.banks, state.ledgerFinancialState)
    trace.boundary.endSnapshot.stock shouldBe Sfc.snapshot(
      result.nextState.world,
      result.nextState.firms,
      result.nextState.households,
      result.nextState.banks,
      result.nextState.ledgerFinancialState,
    )
    trace.boundary.startSnapshot.inflation shouldBe init.world.inflation
    trace.boundary.endSnapshot.inflation shouldBe result.nextState.world.inflation
    trace.timing.envelopes.map(_.key).toSet shouldBe Set(
      MonthTimingEnvelopeKey.Labor,
      MonthTimingEnvelopeKey.Demand,
      MonthTimingEnvelopeKey.Nominal,
      MonthTimingEnvelopeKey.Firm,
    )
    trace.timing.laborSignals.operationalHiringSlack shouldBe result.nextState.world.pipeline.operationalHiringSlack
    demandSignals.sectorDemandMult shouldBe result.nextState.world.seedIn.sectorDemandMult
    demandSignals.sectorDemandPressure shouldBe result.nextState.world.seedIn.sectorDemandPressure
    demandSignals.sectorHiringSignal shouldBe result.nextState.world.seedIn.sectorHiringSignal
    trace.timing.nominalSignals.realizedInflation shouldBe result.nextState.world.inflation
    trace.timing.nominalSignals.expectedInflation shouldBe result.nextState.world.mechanisms.expectations.expectedInflation
    trace.timing.firmDynamics.startupAbsorptionRate shouldBe result.nextState.world.seedIn.startupAbsorptionRate
    trace.timing.firmDynamics.firmBirths shouldBe result.nextState.world.flows.firmBirths
    trace.timing.firmDynamics.firmDeaths shouldBe result.nextState.world.flows.firmDeaths
    trace.timing.firmDynamics.netFirmBirths shouldBe result.nextState.world.flows.netFirmBirths
    trace.executedFlows.totalIncome shouldBe result.calculus.totalIncome
    trace.executedFlows.currentAccount shouldBe result.nextState.world.bop.currentAccount
    trace.executedFlows.fofResidual shouldBe result.nextState.world.plumbing.fofResidual
    trace.seedTransition.provenance.unemploymentRate.stage shouldBe MonthTraceStage.WorldAssemblyEconomics
    trace.seedTransition.provenance.inflation.stage shouldBe MonthTraceStage.PriceEquityEconomics
    trace.seedTransition.provenance.expectedInflation.stage shouldBe MonthTraceStage.OpenEconEconomics
    trace.seedTransition.provenance.laggedHiringSlack.stage shouldBe MonthTraceStage.LaborEconomics
    trace.seedTransition.provenance.startupAbsorptionRate.stage shouldBe MonthTraceStage.StartupStaffing
    trace.seedTransition.provenance.sectorDemandMult.stage shouldBe MonthTraceStage.DemandEconomics
    trace.seedTransition.provenance.sectorDemandPressure.stage shouldBe MonthTraceStage.DemandEconomics
    trace.seedTransition.provenance.sectorHiringSignal.stage shouldBe MonthTraceStage.DemandEconomics
    trace.seedTransition.provenance.laggedHiringSlack.value shouldBe result.nextState.world.seedIn.laggedHiringSlack
    trace.seedTransition.provenance.startupAbsorptionRate.value shouldBe result.nextState.world.seedIn.startupAbsorptionRate
    trace.validations should have size 1
    trace.validations.head.passed shouldBe true
    trace.validations.head.failures shouldBe empty
    result.sfcResult shouldBe Right(())
  }

  it should "keep MonthTrace seed transitions consistent with timing envelopes and end-of-month boundary data" in {
    val init   = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val state  = FlowSimulation.SimState.fromInit(init)
    val result = FlowSimulation.step(state, MonthRandomness.Contract.fromSeed(42L))
    val trace  = result.trace

    trace.seedTransition.seedOut shouldBe DecisionSignals(
      unemploymentRate = trace.boundary.endSnapshot.unemploymentRate,
      inflation = trace.timing.nominalSignals.realizedInflation,
      expectedInflation = trace.timing.nominalSignals.expectedInflation,
      laggedHiringSlack = trace.timing.laborSignals.operationalHiringSlack,
      startupAbsorptionRate = trace.timing.firmDynamics.startupAbsorptionRate,
      sectorDemandMult = trace.timing.demandSignals.sectorDemandMult,
      sectorDemandPressure = trace.timing.demandSignals.sectorDemandPressure,
      sectorHiringSignal = trace.timing.demandSignals.sectorHiringSignal,
    )
    trace.seedTransition.provenance.unemploymentRate.value shouldBe trace.boundary.endSnapshot.unemploymentRate
    trace.seedTransition.provenance.inflation.value shouldBe trace.timing.nominalSignals.realizedInflation
    trace.seedTransition.provenance.expectedInflation.value shouldBe trace.timing.nominalSignals.expectedInflation
    trace.seedTransition.provenance.laggedHiringSlack.value shouldBe trace.timing.laborSignals.operationalHiringSlack
    trace.seedTransition.provenance.startupAbsorptionRate.value shouldBe trace.timing.firmDynamics.startupAbsorptionRate
    trace.seedTransition.provenance.sectorDemandMult.value shouldBe trace.timing.demandSignals.sectorDemandMult
    trace.seedTransition.provenance.sectorDemandPressure.value shouldBe trace.timing.demandSignals.sectorDemandPressure
    trace.seedTransition.provenance.sectorHiringSignal.value shouldBe trace.timing.demandSignals.sectorHiringSignal
  }

  it should "match the flat reference interpreter on aggregate execution deltas" in {
    val init      = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
    val initState = FlowSimulation.SimState.fromInit(init)
    val result    = FlowSimulation.step(initState, MonthRandomness.Contract.fromSeed(42L))
    val state     = result.execution.topology.emptyExecutionState()

    ImperativeInterpreter.planAndApplyAll(state, result.flows) shouldBe Right(())
    result.execution.topology.netDelta(state) shouldBe Interpreter.totalWealth(
      Interpreter.applyAll(Map.empty[Int, Long], result.execution.topology.toFlatFlows(result.flows)),
    )
  }
