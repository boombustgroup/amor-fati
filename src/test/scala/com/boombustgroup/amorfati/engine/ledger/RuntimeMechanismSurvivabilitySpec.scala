package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.config.{SimParams, SimParamsTestOverrides}
import com.boombustgroup.amorfati.engine.economics.BankingEconomics
import com.boombustgroup.amorfati.engine.flows.*
import com.boombustgroup.amorfati.engine.ledger.RuntimeMechanismSurvivability.*
import com.boombustgroup.amorfati.engine.ledger.RuntimeMechanismSurvivability.Classification.*
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RuntimeMechanismSurvivabilitySpec extends AnyFlatSpec with Matchers:

  private given SimParams = SimParams.defaults

  private val topology = RuntimeLedgerTopology.nonZeroPopulation

  private def mechanismIds(mechanisms: Set[MechanismId]): Vector[Int] =
    mechanisms.toVector.map(_.toInt).sorted

  private def representativeBatches(using RuntimeLedgerTopology): Vector[BatchedFlow] =
    val bankingInput = BankingFlows.Input(
      firmInterestIncome = PLN(130000),
      firmNplLoss = PLN(20000),
      mortgageNplLoss = PLN(30000),
      consumerNplLoss = PLN(10000),
      govBondIncome = PLN(600000),
      reserveInterest = PLN(200000),
      standingFacilityIncome = PLN(100000),
      interbankInterest = PLN(50000),
      corpBondCoupon = PLN(25000),
      corpBondDefaultLoss = PLN(45000),
      bfgLevy = PLN(80000),
      unrealizedBondLoss = PLN(40000),
      bailInLoss = PLN(30000),
      nbpRemittance = PLN(50000),
      fxReserveSettlement = PLN(70000),
      standingFacilityBackstop = PLN(15000),
    )

    val openEconInput = OpenEconFlows.Input(
      exports = PLN(1000000),
      imports = PLN(700000),
      tourismExport = PLN(250000),
      tourismImport = PLN(200000),
      fdi = PLN(300000),
      portfolioFlows = PLN(50000),
      carryTradeFlow = PLN(25000),
      primaryIncome = PLN(20000),
      euFunds = PLN(400000),
      diasporaInflow = PLN(100000),
      capitalFlightOutflow = PLN(150000),
    )

    val insuranceInput = InsuranceFlows.Input(
      employed = 80000,
      wage = PLN(7000),
      unempRate = Share.decimal(8, 2),
      currentLifeReserves = PLN(90000000),
      currentNonLifeReserves = PLN(30000000),
      prevGovBondHoldings = PLN(100000000),
      prevCorpBondHoldings = PLN(50000000),
      corpBondDefaultLoss = PLN.Zero,
      prevEquityHoldings = PLN(40000000),
      govBondYield = Rate.decimal(6, 2),
      corpBondYield = Rate.decimal(8, 2),
      equityReturn = Rate.decimal(3, 2),
    )

    Vector.concat(
      ZusFlows.emitBatches(ZusFlows.ZusInput(1000, PLN(1000), 100000)),
      NfzFlows.emitBatches(NfzFlows.NfzInput.fromDrivers(1000, PLN(1000), 200000, 100000)),
      PpkFlows.emitBatches(PpkFlows.PpkInput(10000, PLN(1000))),
      GovBondFlows.emitBatches(
        BankingEconomics.GovBondRuntimeMovements(
          primaryByBank = Vector(PLN(100000), PLN(50000)),
          foreignPurchaseByBank = Vector(PLN(10000), PLN(5000)),
          nbpQePurchaseByBank = Vector(PLN(20000), PLN(10000)),
          ppkPurchaseByBank = Vector(PLN(30000), PLN(15000)),
          insurancePurchaseByBank = Vector(PLN(40000), PLN(20000)),
          tfiPurchaseByBank = Vector(PLN(50000), PLN(25000)),
        ),
      ),
      EarmarkedFlows.emitBatches(EarmarkedFlows.Input(1000, PLN(1000), PLN(100000000), 100000, 100)),
      EarmarkedFlows.emitBatches(EarmarkedFlows.Input(0, PLN.Zero, PLN.Zero, 0, 0)(using SimParamsTestOverrides.pfronDeficit))(using
        summon[SimParams],
        summon[RuntimeLedgerTopology],
      ),
      JstFlows.emitBatches(JstFlows.Input(PLN(5000000), PLN(50000000), PLN(100000000), 9000, PLN(3000000))),
      HouseholdFlows.emitBatches(
        HouseholdFlows.Input(
          PLN(40000000),
          PLN(8000000),
          PLN(5000000),
          PLN(3000000),
          PLN(1000000),
          PLN(500000),
          PLN(2000000),
          PLN(1500000),
          PLN(200000),
        ),
      ),
      FirmFlows.emitBatches(
        FirmFlows.Input(
          PLN(50000000),
          PLN(8000000),
          PLN(4000000),
          PLN(6000000),
          PLN(3000000),
          PLN(5000000),
          PLN(1000000),
          PLN(7000000),
          PLN(300000),
          PLN(800000),
          PLN(600000),
          PLN(5000000),
        ),
      ),
      InvestmentDepositSettlementFlows.emitBatches(InvestmentDepositSettlementFlows.Input(PLN(2500000))),
      InvestmentDepositSettlementFlows.emitBatches(InvestmentDepositSettlementFlows.Input(PLN(-1500000))),
      NbfiFlows.emitBatches(NbfiFlows.Input(PLN(-1200000), PLN(900000), PLN(400000), PLN(100000))),
      NbfiFlows.emitBatches(NbfiFlows.Input(PLN(800000), PLN.Zero, PLN.Zero, PLN.Zero)),
      QuasiFiscalFlows.emitBatches(
        QuasiFiscalFlows.Input(
          bankBondIssuance = PLN(700000),
          nbpBondAbsorption = PLN(300000),
          bankBondAmortization = PLN(200000),
          nbpBondAmortization = PLN(100000),
          lending = PLN(500000),
          repayment = PLN(150000),
        ),
      ),
      GovBudgetFlows.emitBatches(
        GovBudgetFlows.Input(
          vatRevenue = PLN(9000000),
          exciseRevenue = PLN(3500000),
          customsDutyRevenue = PLN(2500000),
          govCurrentSpend = PLN(10000000),
          debtService = PLN(4000000),
          unempBenefitSpend = PLN(2000000),
          socialTransferSpend = PLN(3000000),
          euCofinancing = PLN(1500000),
          govCapitalSpend = PLN(2500000),
          debtServiceRecipients = Some(
            GovBudgetFlows.DebtServiceRecipients(
              banks = PLN(4000000),
              foreign = PLN.Zero,
              nbp = PLN.Zero,
              insurance = PLN.Zero,
              ppk = PLN.Zero,
              tfi = PLN.Zero,
              banksByBank = Vector(PLN(2500000), PLN(1500000)),
            ),
          ),
        ),
      ),
      InsuranceFlows.emitBatches(insuranceInput),
      InsuranceFlows.emitBatches(insuranceInput.copy(corpBondDefaultLoss = PLN(10000000))),
      EquityFlows.emitBatches(EquityFlows.Input(PLN(500000), PLN(200000), PLN(100000), PLN(50000))),
      CorpBondFlows.emitBatches(CorpBondFlows.Input(PLN(300000), PLN(50000), PLN(1000000), PLN(200000))),
      MortgageFlows.emitBatches(MortgageFlows.Input(PLN(5000000), PLN(2000000), PLN(1500000), PLN(300000))),
      OpenEconFlows.emitBatches(openEconInput),
      OpenEconFlows.emitBatches(
        openEconInput.copy(portfolioFlows = PLN(-50000), carryTradeFlow = PLN(-25000), primaryIncome = PLN(-30000)),
      ),
      BankingFlows.emitBatches(bankingInput),
      BankingFlows.emitBatches(
        bankingInput.copy(
          standingFacilityIncome = PLN(-100000),
          interbankInterest = PLN(-50000),
          fxReserveSettlement = PLN(-70000),
        ),
      ),
    )

  private lazy val emittedBatches: Vector[BatchedFlow] =
    representativeBatches(using topology)

  private lazy val emittedMechanisms: Set[MechanismId] =
    FlowMechanism.emittedRuntimeMechanisms

  private lazy val representativeMechanisms: Set[MechanismId] =
    emittedBatches.map(_.mechanism).toSet

  "RuntimeMechanismSurvivability" should "declare every runtime emitted FlowMechanism" in
    withClue(s"Missing declarations: ${mechanismIds(missingDeclarations(emittedMechanisms)).mkString(",")}") {
      missingDeclarations(emittedMechanisms) shouldBe empty
    }

  it should "avoid stale declarations for mechanisms that are not runtime emitted" in {
    val declaredOnly = declaredMechanisms.diff(emittedMechanisms)
    val emittedOnly  = emittedMechanisms.diff(declaredMechanisms)

    withClue(s"Declared-only: ${mechanismIds(declaredOnly).mkString(",")}; emitted-only: ${mechanismIds(emittedOnly).mkString(",")}") {
      declaredMechanisms shouldBe emittedMechanisms
    }
  }

  it should "exercise every runtime emitted FlowMechanism in representativeBatches" in {
    val fixtureOnly        = representativeMechanisms.diff(emittedMechanisms)
    val missingFromFixture = emittedMechanisms.diff(representativeMechanisms)

    withClue(s"Fixture-only: ${mechanismIds(fixtureOnly).mkString(",")}; missing from fixture: ${mechanismIds(missingFromFixture).mkString(",")}") {
      representativeMechanisms shouldBe emittedMechanisms
    }
  }

  it should "match each declaration to the concrete emitted batch shape" in {
    val observedByMechanism = emittedBatches
      .groupMap(_.mechanism)(observedClassification(_, topology))
      .view
      .mapValues(_.toSet)
      .toMap

    val expectedByMechanism =
      declarationsByMechanism.view.mapValues(row => Set(row.classification)).toMap

    observedByMechanism shouldBe expectedByMechanism
  }

  it should "not classify aggregate runtime shells as round-trippable stock owners" in {
    val aggregateCash = BatchedFlow.Broadcast(
      from = EntitySector.Firms,
      fromIndex = topology.firms.aggregate,
      to = EntitySector.Funds,
      amounts = Array(1L),
      targetIndices = Array(topology.funds.jst),
      asset = AssetType.Cash,
      mechanism = FlowMechanism.JstRevenue,
    )

    observedClassification(aggregateCash, topology) shouldBe ExecutionDeltaOnly
    AssetOwnershipContract.isSupportedPersistedPair(topology, EntitySector.Firms, AssetType.Cash, topology.firms.aggregate) shouldBe false
  }

end RuntimeMechanismSurvivabilitySpec
