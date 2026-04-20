package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.config.{SimParams, SimParamsTestOverrides}
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
      firmInterestIncome = PLN(130000.0),
      firmNplLoss = PLN(20000.0),
      mortgageNplLoss = PLN(30000.0),
      consumerNplLoss = PLN(10000.0),
      govBondIncome = PLN(600000.0),
      reserveInterest = PLN(200000.0),
      standingFacilityIncome = PLN(100000.0),
      interbankInterest = PLN(50000.0),
      corpBondCoupon = PLN(25000.0),
      corpBondDefaultLoss = PLN(45000.0),
      bfgLevy = PLN(80000.0),
      unrealizedBondLoss = PLN(40000.0),
      bailInLoss = PLN(30000.0),
      nbpRemittance = PLN(50000.0),
      fxReserveSettlement = PLN(70000.0),
      standingFacilityBackstop = PLN(15000.0),
    )

    val openEconInput = OpenEconFlows.Input(
      exports = PLN(1000000.0),
      imports = PLN(700000.0),
      tourismExport = PLN(250000.0),
      tourismImport = PLN(200000.0),
      fdi = PLN(300000.0),
      portfolioFlows = PLN(50000.0),
      carryTradeFlow = PLN(25000.0),
      primaryIncome = PLN(20000.0),
      euFunds = PLN(400000.0),
      diasporaInflow = PLN(100000.0),
      capitalFlightOutflow = PLN(150000.0),
    )

    val insuranceInput = InsuranceFlows.Input(
      employed = 80000,
      wage = PLN(7000.0),
      unempRate = Share(0.08),
      currentLifeReserves = PLN(90000000.0),
      currentNonLifeReserves = PLN(30000000.0),
      prevGovBondHoldings = PLN(100000000.0),
      prevCorpBondHoldings = PLN(50000000.0),
      corpBondDefaultLoss = PLN.Zero,
      prevEquityHoldings = PLN(40000000.0),
      govBondYield = Rate(0.06),
      corpBondYield = Rate(0.08),
      equityReturn = Rate(0.03),
    )

    Vector.concat(
      ZusFlows.emitBatches(ZusFlows.ZusInput(1000, PLN(1000.0), 100000)),
      NfzFlows.emitBatches(NfzFlows.NfzInput(1000, PLN(1000.0), 200000, 100000)),
      PpkFlows.emitBatches(PpkFlows.PpkInput(10000, PLN(1000.0))),
      EarmarkedFlows.emitBatches(EarmarkedFlows.Input(1000, PLN(1000.0), PLN(100000000.0), 100000, 100)),
      EarmarkedFlows.emitBatches(EarmarkedFlows.Input(0, PLN.Zero, PLN.Zero, 0, 0))(using SimParamsTestOverrides.pfronDeficit, summon[RuntimeLedgerTopology]),
      JstFlows.emitBatches(JstFlows.Input(PLN(5000000.0), PLN(50000000.0), PLN(100000000.0), 9000, PLN(3000000.0))),
      HouseholdFlows.emitBatches(
        HouseholdFlows.Input(
          PLN(40000000.0),
          PLN(8000000.0),
          PLN(5000000.0),
          PLN(3000000.0),
          PLN(1000000.0),
          PLN(500000.0),
          PLN(2000000.0),
          PLN(1500000.0),
          PLN(200000.0),
        ),
      ),
      FirmFlows.emitBatches(
        FirmFlows.Input(
          PLN(50000000.0),
          PLN(8000000.0),
          PLN(4000000.0),
          PLN(6000000.0),
          PLN(3000000.0),
          PLN(5000000.0),
          PLN(1000000.0),
          PLN(7000000.0),
          PLN(300000.0),
          PLN(800000.0),
          PLN(600000.0),
          PLN(5000000.0),
        ),
      ),
      GovBudgetFlows.emitBatches(
        GovBudgetFlows.Input(
          vatRevenue = PLN(9000000.0),
          exciseRevenue = PLN(3500000.0),
          customsDutyRevenue = PLN(2500000.0),
          govPurchases = PLN(10000000.0),
          debtService = PLN(4000000.0),
          unempBenefitSpend = PLN(2000000.0),
          socialTransferSpend = PLN(3000000.0),
          euCofinancing = PLN(1500000.0),
          govCapitalSpend = PLN(2500000.0),
        ),
      ),
      InsuranceFlows.emitBatches(insuranceInput),
      InsuranceFlows.emitBatches(insuranceInput.copy(corpBondDefaultLoss = PLN(10000000.0))),
      EquityFlows.emitBatches(EquityFlows.Input(PLN(500000.0), PLN(200000.0), PLN(100000.0), PLN(50000.0))),
      CorpBondFlows.emitBatches(CorpBondFlows.Input(PLN(300000.0), PLN(50000.0), PLN(1000000.0), PLN(200000.0))),
      MortgageFlows.emitBatches(MortgageFlows.Input(PLN(5000000.0), PLN(2000000.0), PLN(1500000.0), PLN(300000.0))),
      OpenEconFlows.emitBatches(openEconInput),
      OpenEconFlows.emitBatches(openEconInput.copy(portfolioFlows = PLN(-50000.0), primaryIncome = PLN(-30000.0))),
      BankingFlows.emitBatches(bankingInput),
      BankingFlows.emitBatches(
        bankingInput.copy(
          standingFacilityIncome = PLN(-100000.0),
          interbankInterest = PLN(-50000.0),
          fxReserveSettlement = PLN(-70000.0),
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
