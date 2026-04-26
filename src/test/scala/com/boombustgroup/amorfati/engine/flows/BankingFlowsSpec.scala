package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.engine.ledger.NbpRuntimeContract
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BankingFlowsSpec extends AnyFlatSpec with Matchers:

  private val runtimeTopologies = Vector(
    "zeroPopulation"    -> RuntimeLedgerTopology.zeroPopulation,
    "nonZeroPopulation" -> RuntimeLedgerTopology.nonZeroPopulation,
  )

  private val baseInput = BankingFlows.Input(
    firmInterestIncome = PLN(900000),
    firmNplLoss = PLN(120000),
    mortgageNplLoss = PLN(110000),
    consumerNplLoss = PLN(60000),
    govBondIncome = PLN(3000000),
    reserveInterest = PLN(500000),
    standingFacilityIncome = PLN(100000),
    interbankInterest = PLN(200000),
    corpBondCoupon = PLN(70000),
    corpBondDefaultLoss = PLN(50000),
    bfgLevy = PLN(400000),
    unrealizedBondLoss = PLN(150000),
    bailInLoss = PLN.Zero,
    nbpRemittance = PLN(800000),
    fxReserveSettlement = PLN.Zero,
    standingFacilityBackstop = PLN.Zero,
  )

  "BankingFlows" should "preserve total wealth at exactly 0L" in {
    val flows    = BankingFlows.emit(baseInput)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    Interpreter.totalWealth(balances) shouldBe 0L
  }

  it should "have correct bank balance (income - outflows)" in {
    val flows    = BankingFlows.emit(baseInput)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)

    val income   =
      baseInput.firmInterestIncome + baseInput.govBondIncome + baseInput.reserveInterest + baseInput.standingFacilityIncome + baseInput.interbankInterest +
        baseInput.corpBondCoupon
    val outflows =
      baseInput.firmNplLoss + baseInput.mortgageNplLoss + baseInput.consumerNplLoss + baseInput.corpBondDefaultLoss + baseInput.bfgLevy +
        baseInput.unrealizedBondLoss + baseInput.nbpRemittance
    val bailIn   = baseInput.bailInLoss

    balances(BankingFlows.BANK_ACCOUNT) shouldBe (income - outflows + bailIn).toLong
  }

  it should "handle negative interbank interest (net cost)" in {
    val netCost  = baseInput.copy(interbankInterest = PLN(-300000))
    val flows    = BankingFlows.emit(netCost)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    Interpreter.totalWealth(balances) shouldBe 0L

    val ibFlow = flows.filter(_.mechanism == FlowMechanism.BankInterbankInterest.toInt).head
    ibFlow.from shouldBe BankingFlows.BANK_ACCOUNT
  }

  it should "handle negative standing facility income (net cost)" in {
    val netCost  = baseInput.copy(standingFacilityIncome = PLN(-250000))
    val flows    = BankingFlows.emit(netCost)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    Interpreter.totalWealth(balances) shouldBe 0L

    val sfFlow = flows.filter(_.mechanism == FlowMechanism.BankStandingFacility.toInt).head
    sfFlow.from shouldBe BankingFlows.BANK_ACCOUNT
  }

  it should "handle bail-in (depositor loss)" in {
    val withBailIn = baseInput.copy(bailInLoss = PLN(5000000))
    val flows      = BankingFlows.emit(withBailIn)
    val balances   = Interpreter.applyAll(Map.empty[Int, Long], flows)
    Interpreter.totalWealth(balances) shouldBe 0L

    balances(BankingFlows.DEPOSITOR_ACCOUNT) should be < 0L
  }

  it should "route flat corporate-bond P&L through firms like the batched emitter" in {
    val flows         = BankingFlows.emit(baseInput)
    val coupon        = flows.find(_.mechanism == FlowMechanism.BankCorpBondCoupon.toInt).get
    val loss          = flows.find(_.mechanism == FlowMechanism.BankCorpBondLoss.toInt).get
    val batched       = BankingFlows.emitBatches(baseInput)(using RuntimeLedgerTopology.nonZeroPopulation)
    val batchedCoupon = batched.find(_.mechanism == FlowMechanism.BankCorpBondCoupon).get
    val batchedLoss   = batched.find(_.mechanism == FlowMechanism.BankCorpBondLoss).get

    coupon.from shouldBe BankingFlows.FIRM_ACCOUNT
    coupon.to shouldBe BankingFlows.BANK_ACCOUNT
    loss.from shouldBe BankingFlows.BANK_ACCOUNT
    loss.to shouldBe BankingFlows.FIRM_ACCOUNT
    batchedCoupon.from shouldBe EntitySector.Firms
    batchedCoupon.to shouldBe EntitySector.Banks
    batchedLoss.from shouldBe EntitySector.Banks
    batchedLoss.to shouldBe EntitySector.Firms
  }

  it should "emit NBP reserve-side settlement on the reserve asset contract" in {
    val nbpReserveMechanisms = Set(
      FlowMechanism.BankReserveInterest,
      FlowMechanism.BankStandingFacility,
      FlowMechanism.BankInterbankInterest,
    )

    runtimeTopologies.foreach:
      case (label, topology) =>
        withClue(s"$label: ") {
          val batches        = BankingFlows.emitBatches(baseInput)(using topology)
          val reserveBatches = batches.filter(batch => nbpReserveMechanisms.contains(batch.mechanism))
          reserveBatches should not be empty
          all(reserveBatches.map(_.asset)) shouldBe NbpRuntimeContract.ReserveSettlementLiability.asset
          all(
            reserveBatches.collect { case broadcast: BatchedFlow.Broadcast => broadcast.fromIndex },
          ) shouldBe NbpRuntimeContract.ReserveSettlementLiability.index
        }
  }

  it should "emit signed FX reserve settlement through the same NBP shell" in
    runtimeTopologies.foreach:
      case (label, topology) =>
        withClue(s"$label: ") {
          val injection = BankingFlows
            .emitBatches(baseInput.copy(fxReserveSettlement = PLN(125000)))(using topology)
            .find(_.mechanism == FlowMechanism.NbpFxSettlement)
            .get
          val drain     = BankingFlows
            .emitBatches(baseInput.copy(fxReserveSettlement = PLN(-90000)))(using topology)
            .find(_.mechanism == FlowMechanism.NbpFxSettlement)
            .get

          injection.asset shouldBe NbpRuntimeContract.ReserveSettlementLiability.asset
          injection.from shouldBe EntitySector.NBP
          injection.to shouldBe EntitySector.Banks

          drain.asset shouldBe NbpRuntimeContract.ReserveSettlementLiability.asset
          drain.from shouldBe EntitySector.Banks
          drain.to shouldBe EntitySector.NBP
        }

  it should "emit explicit standing-facility backstop on the dedicated NBP contract" in
    runtimeTopologies.foreach:
      case (label, topology) =>
        withClue(s"$label: ") {
          val backstop = BankingFlows
            .emitBatches(baseInput.copy(standingFacilityBackstop = PLN(175000)))(using topology)
            .find(_.mechanism == FlowMechanism.BankStandingFacilityBackstop)
            .get

          backstop.asset shouldBe NbpRuntimeContract.StandingFacilityBackstop.asset
          backstop.from shouldBe EntitySector.NBP
          backstop.to shouldBe EntitySector.Banks
        }

  it should "avoid cash assets on bank capital P&L channels" in
    runtimeTopologies.foreach:
      case (label, topology) =>
        withClue(s"$label: ") {
          val batches           = BankingFlows.emitBatches(baseInput)(using topology)
          val capitalMechanisms = Set(
            FlowMechanism.BankFirmInterest,
            FlowMechanism.BankNplLoss,
            FlowMechanism.BankMortgageNplLoss,
            FlowMechanism.BankCcNplLoss,
            FlowMechanism.BankGovBondIncome,
            FlowMechanism.BankCorpBondCoupon,
            FlowMechanism.BankCorpBondLoss,
            FlowMechanism.BankBfgLevy,
            FlowMechanism.BankUnrealizedLoss,
            FlowMechanism.BankNbpRemittance,
          )

          val capitalBatches = batches.filter(batch => capitalMechanisms.contains(batch.mechanism))
          capitalBatches should not be empty
          all(capitalBatches.map(_.asset)) shouldBe AssetType.Capital
          all(capitalBatches.map(batch => batch.from == EntitySector.Banks || batch.to == EntitySector.Banks)) shouldBe true
        }

  it should "wire all bank-specific P&L audit mechanisms into emitted batches" in
    runtimeTopologies.foreach:
      case (label, topology) =>
        withClue(s"$label: ") {
          val mechanisms = BankingFlows.emitBatches(baseInput)(using topology).map(_.mechanism).toSet

          mechanisms should contain allOf (
            FlowMechanism.BankFirmInterest,
            FlowMechanism.BankNplLoss,
            FlowMechanism.BankMortgageNplLoss,
            FlowMechanism.BankCcNplLoss,
            FlowMechanism.BankCorpBondLoss,
          )
        }

  it should "preserve SFC across 120 months" in {
    var balances = Map.empty[Int, Long]
    (1 to 120).foreach { _ =>
      balances = Interpreter.applyAll(balances, BankingFlows.emit(baseInput))
      Interpreter.totalWealth(balances) shouldBe 0L
    }
  }
