package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BatchedEmissionContractSpec extends AnyFlatSpec with Matchers:
  import RuntimeFlowsTestSupport.*

  private given SimParams             = SimParams.defaults
  private given RuntimeLedgerTopology = RuntimeLedgerTopology.zeroPopulation

  private val govDebtRecipients = GovBudgetFlows.DebtServiceRecipients(
    banks = PLN(4000000),
    foreign = PLN.Zero,
    nbp = PLN.Zero,
    insurance = PLN.Zero,
    ppk = PLN.Zero,
    tfi = PLN.Zero,
  )

  private def flatMechanismTotals(flows: Vector[Flow]): Map[Int, Long] =
    flows.groupMapReduce(_.mechanism)(_.amount)(_ + _)

  private def batchedMechanismTotals(batches: Vector[BatchedFlow]): Map[Int, Long] =
    batches.groupMapReduce(_.mechanism.toInt)(RuntimeLedgerTopology.totalTransferred)(_ + _)

  private case class EmissionPair(flat: Vector[Flow], batched: Vector[BatchedFlow])

  "emitBatches" should "preserve mechanism totals across migrated emitters" in {
    val zusInput       = ZusFlows.ZusInput(80000, PLN(7000), 1000)
    val nfzInput       = NfzFlows.NfzInput.fromDrivers(80000, PLN(7000), 90000, 1000)
    val ppkInput       = PpkFlows.PpkInput(80000, PLN(7000))
    val earmarkedInput = EarmarkedFlows.Input(80000, PLN(7000), PLN(1000000), 10, 15)
    val jstInput       = JstFlows.Input(PLN(5000000), PLN(50000000), PLN(100000000), 9000, PLN(3000000))
    val householdInput = HouseholdFlows.Input(
      PLN(40000000),
      PLN(8000000),
      PLN(5000000),
      PLN(3000000),
      PLN(1000000),
      PLN(500000),
      PLN(2000000),
      PLN(1500000),
      PLN(200000),
    )
    val firmInput      = FirmFlows.Input(
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
    )
    val govInput       = GovBudgetFlows.Input(
      vatRevenue = PLN(9000000),
      exciseRevenue = PLN(3500000),
      customsDutyRevenue = PLN(2500000),
      govCurrentSpend = PLN(10000000),
      debtService = PLN(4000000),
      unempBenefitSpend = PLN(2000000),
      socialTransferSpend = PLN(3000000),
      euCofinancing = PLN(1500000),
      govCapitalSpend = PLN(2500000),
      debtServiceRecipients = Some(govDebtRecipients),
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
    val equityInput    = EquityFlows.Input(PLN(500000), PLN(200000), PLN(100000), PLN(50000))
    val corpBondInput  = CorpBondFlows.Input(PLN(300000), PLN(50000), PLN(1000000), PLN(200000))
    val mortgageInput  = MortgageFlows.Input(PLN(5000000), PLN(2000000), PLN(1500000), PLN(300000))
    val openEconInput  = OpenEconFlows.Input(
      exports = PLN(1000000),
      imports = PLN(700000),
      tourismExport = PLN(250000),
      tourismImport = PLN(200000),
      fdi = PLN(300000),
      portfolioFlows = PLN(50000),
      carryTradeFlow = PLN(10000),
      primaryIncome = PLN(-20000),
      euFunds = PLN(400000),
      diasporaInflow = PLN(100000),
      capitalFlightOutflow = PLN(150000),
    )
    val bankingInput   = BankingFlows.Input(
      firmInterestIncome = PLN(130000),
      firmNplLoss = PLN(20000),
      mortgageNplLoss = PLN(30000),
      consumerNplLoss = PLN(10000),
      govBondIncome = PLN(600000),
      reserveInterest = PLN(200000),
      standingFacilityIncome = PLN(100000),
      interbankInterest = PLN(-50000),
      corpBondCoupon = PLN(25000),
      corpBondDefaultLoss = PLN(45000),
      bfgLevy = PLN(80000),
      unrealizedBondLoss = PLN(40000),
      bailInLoss = PLN(30000),
      nbpRemittance = PLN(50000),
      fxReserveSettlement = PLN.Zero,
      standingFacilityBackstop = PLN.Zero,
    )

    val emissions = Vector(
      EmissionPair(ZusFlows.emit(zusInput), ZusFlows.emitBatches(zusInput)),
      EmissionPair(NfzFlows.emit(nfzInput), NfzFlows.emitBatches(nfzInput)),
      EmissionPair(PpkFlows.emit(ppkInput), PpkFlows.emitBatches(ppkInput)),
      EmissionPair(EarmarkedFlows.emit(earmarkedInput), EarmarkedFlows.emitBatches(earmarkedInput)),
      EmissionPair(JstFlows.emit(jstInput), JstFlows.emitBatches(jstInput)),
      EmissionPair(HouseholdFlows.emit(householdInput), HouseholdFlows.emitBatches(householdInput)),
      EmissionPair(FirmFlows.emit(firmInput), FirmFlows.emitBatches(firmInput)),
      EmissionPair(GovBudgetFlows.emit(govInput), GovBudgetFlows.emitBatches(govInput)),
      EmissionPair(InsuranceFlows.emit(insuranceInput), InsuranceFlows.emitBatches(insuranceInput)),
      EmissionPair(EquityFlows.emit(equityInput), EquityFlows.emitBatches(equityInput)),
      EmissionPair(CorpBondFlows.emit(corpBondInput), CorpBondFlows.emitBatches(corpBondInput)),
      EmissionPair(MortgageFlows.emit(mortgageInput), MortgageFlows.emitBatches(mortgageInput)),
      EmissionPair(OpenEconFlows.emit(openEconInput), OpenEconFlows.emitBatches(openEconInput)),
      EmissionPair(BankingFlows.emit(bankingInput), BankingFlows.emitBatches(bankingInput)),
    )

    val flatFlows    = emissions.flatMap(_.flat)
    val batchedFlows = emissions.flatMap(_.batched)

    batchedMechanismTotals(batchedFlows) shouldBe flatMechanismTotals(flatFlows)
    batchedFlows.foreach(_ shouldBe a[BatchedFlow.Broadcast])
  }

  it should "drive FlowSimulation main path through BatchedFlow" in {
    val result = stepFromSeed()
    val flat   = result.execution.topology.toFlatFlows(result.flows)

    result.flows should not be empty
    result.flows.forall(_.isInstanceOf[BatchedFlow]) shouldBe true
    batchedMechanismTotals(result.flows) shouldBe flatMechanismTotals(flat)
    result.execution.netDelta shouldBe 0L
  }
