package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NbfiFlowsSpec extends AnyFlatSpec with Matchers:
  import RuntimeFlowsTestSupport.*

  private given RuntimeLedgerTopology = RuntimeLedgerTopology.nonZeroPopulation

  private def onlyBroadcast(batches: Vector[BatchedFlow]): BatchedFlow.Broadcast =
    batches should have size 1
    batches.head match
      case broadcast: BatchedFlow.Broadcast => broadcast
      case other                            => fail(s"Expected Broadcast, got $other")

  "NbfiFlows" should "preserve total wealth at exactly 0L" in {
    val flows = NbfiFlows.emit(NbfiFlows.Input(PLN(-125000.0), PLN(300000.0), PLN(100000.0), PLN(50000.0)))

    Interpreter.totalWealth(Interpreter.applyAll(Map.empty[Int, Long], flows)) shouldBe 0L
  }

  it should "emit legacy flat flows with explicit directions" in {
    val flows = NbfiFlows.emit(NbfiFlows.Input(PLN(-125000.0), PLN(300000.0), PLN(100000.0), PLN(50000.0)))

    val tfiDrain = flows.filter(_.mechanism == FlowMechanism.TfiDepositDrain.toInt)
    tfiDrain should have size 1
    tfiDrain.head.from shouldBe NbfiFlows.HH_ACCOUNT
    tfiDrain.head.to shouldBe NbfiFlows.BANK_ACCOUNT

    val origination = flows.filter(_.mechanism == FlowMechanism.NbfiOrigination.toInt)
    origination should have size 1
    origination.head.from shouldBe NbfiFlows.NBFI_ACCOUNT
    origination.head.to shouldBe NbfiFlows.HH_ACCOUNT

    val repayment = flows.filter(_.mechanism == FlowMechanism.NbfiRepayment.toInt)
    repayment should have size 1
    repayment.head.from shouldBe NbfiFlows.HH_ACCOUNT
    repayment.head.to shouldBe NbfiFlows.NBFI_ACCOUNT

    val defaultAmount = flows.filter(_.mechanism == FlowMechanism.NbfiDefault.toInt)
    defaultAmount should have size 1
    defaultAmount.head.from shouldBe NbfiFlows.HH_ACCOUNT
    defaultAmount.head.to shouldBe NbfiFlows.NBFI_ACCOUNT

    val positiveDrain = NbfiFlows.emit(NbfiFlows.Input(PLN(80000.0), PLN.Zero, PLN.Zero, PLN.Zero))
    positiveDrain should have size 1
    positiveDrain.head.from shouldBe NbfiFlows.BANK_ACCOUNT
    positiveDrain.head.to shouldBe NbfiFlows.HH_ACCOUNT
  }

  it should "emit negative TFI deposit drain as household deposit drawdown" in {
    val batches = NbfiFlows.emitBatches(NbfiFlows.Input(PLN(-125000.0), PLN.Zero, PLN.Zero, PLN.Zero))
    val batch   = onlyBroadcast(batches)

    batch.mechanism shouldBe FlowMechanism.TfiDepositDrain
    batch.asset shouldBe AssetType.DemandDeposit
    batch.from shouldBe EntitySector.Households
    batch.fromIndex shouldBe summon[RuntimeLedgerTopology].households.investors
    batch.to shouldBe EntitySector.Banks
    batch.targetIndices.toVector shouldBe Vector(summon[RuntimeLedgerTopology].banks.aggregate)
    totalTransferred(batches) shouldBe PLN(125000.0)

    val evidence = FlowSimulation.ExecutedFlowEvidence.from(batches)
    evidence.nbfiDepositDrain shouldBe PLN(-125000.0)
  }

  it should "emit positive TFI deposit drain as bank-issued household deposits" in {
    val batches = NbfiFlows.emitBatches(NbfiFlows.Input(PLN(80000.0), PLN.Zero, PLN.Zero, PLN.Zero))
    val batch   = onlyBroadcast(batches)

    batch.mechanism shouldBe FlowMechanism.TfiDepositDrain
    batch.asset shouldBe AssetType.DemandDeposit
    batch.from shouldBe EntitySector.Banks
    batch.fromIndex shouldBe summon[RuntimeLedgerTopology].banks.aggregate
    batch.to shouldBe EntitySector.Households
    batch.targetIndices.toVector shouldBe Vector(summon[RuntimeLedgerTopology].households.investors)
    totalTransferred(batches) shouldBe PLN(80000.0)

    val evidence = FlowSimulation.ExecutedFlowEvidence.from(batches)
    evidence.nbfiDepositDrain shouldBe PLN(80000.0)
  }

  it should "emit NBFI credit stock movements with explicit mechanisms" in {
    val batches = NbfiFlows.emitBatches(NbfiFlows.Input(PLN.Zero, PLN(300000.0), PLN(100000.0), PLN(50000.0)))

    mechanismBatches(batches, FlowMechanism.NbfiOrigination) should have size 1
    mechanismBatches(batches, FlowMechanism.NbfiRepayment) should have size 1
    mechanismBatches(batches, FlowMechanism.NbfiDefault) should have size 1

    val origination = onlyBroadcast(mechanismBatches(batches, FlowMechanism.NbfiOrigination))
    origination.asset shouldBe AssetType.NbfiLoan
    origination.from shouldBe EntitySector.Funds
    origination.fromIndex shouldBe summon[RuntimeLedgerTopology].funds.nbfi
    origination.to shouldBe EntitySector.Households
    origination.targetIndices.toVector shouldBe Vector(summon[RuntimeLedgerTopology].households.aggregate)

    val repayment = onlyBroadcast(mechanismBatches(batches, FlowMechanism.NbfiRepayment))
    repayment.asset shouldBe AssetType.NbfiLoan
    repayment.from shouldBe EntitySector.Households
    repayment.fromIndex shouldBe summon[RuntimeLedgerTopology].households.aggregate
    repayment.to shouldBe EntitySector.Funds
    repayment.targetIndices.toVector shouldBe Vector(summon[RuntimeLedgerTopology].funds.nbfi)

    val defaultAmount = onlyBroadcast(mechanismBatches(batches, FlowMechanism.NbfiDefault))
    defaultAmount.asset shouldBe AssetType.NbfiLoan
    defaultAmount.from shouldBe EntitySector.Households
    defaultAmount.fromIndex shouldBe summon[RuntimeLedgerTopology].households.aggregate
    defaultAmount.to shouldBe EntitySector.Funds
    defaultAmount.targetIndices.toVector shouldBe Vector(summon[RuntimeLedgerTopology].funds.nbfi)

    totalTransferred(mechanismBatches(batches, FlowMechanism.NbfiOrigination)) shouldBe PLN(300000.0)
    totalTransferred(mechanismBatches(batches, FlowMechanism.NbfiRepayment)) shouldBe PLN(100000.0)
    totalTransferred(mechanismBatches(batches, FlowMechanism.NbfiDefault)) shouldBe PLN(50000.0)
  }

  it should "emit no batch for zero monetary channels" in {
    NbfiFlows.emitBatches(NbfiFlows.Input(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)) shouldBe empty
    NbfiFlows.emit(NbfiFlows.Input(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)) shouldBe empty
  }

end NbfiFlowsSpec
