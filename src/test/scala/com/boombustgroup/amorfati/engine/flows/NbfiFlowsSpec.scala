package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NbfiFlowsSpec extends AnyFlatSpec with Matchers:
  import RuntimeFlowsTestSupport.*

  private given RuntimeLedgerTopology = RuntimeLedgerTopology.nonZeroPopulation

  "NbfiFlows" should "preserve total wealth at exactly 0L" in {
    val flows = NbfiFlows.emit(NbfiFlows.Input(PLN(-125000.0), PLN(300000.0), PLN(100000.0), PLN(50000.0)))

    Interpreter.totalWealth(Interpreter.applyAll(Map.empty[Int, Long], flows)) shouldBe 0L
  }

  it should "emit negative TFI deposit drain as household deposit drawdown" in {
    val batches = NbfiFlows.emitBatches(NbfiFlows.Input(PLN(-125000.0), PLN.Zero, PLN.Zero, PLN.Zero))
    val batch   = batches.head

    batches should have size 1
    batch.mechanism shouldBe FlowMechanism.TfiDepositDrain
    batch.asset shouldBe AssetType.DemandDeposit
    batch.from shouldBe EntitySector.Households
    batch.to shouldBe EntitySector.Banks
    totalTransferred(batches) shouldBe PLN(125000.0)

    val evidence = FlowSimulation.ExecutedFlowEvidence.from(batches)
    evidence.nbfiDepositDrain shouldBe PLN(-125000.0)
  }

  it should "emit positive TFI deposit drain as bank-issued household deposits" in {
    val batches = NbfiFlows.emitBatches(NbfiFlows.Input(PLN(80000.0), PLN.Zero, PLN.Zero, PLN.Zero))
    val batch   = batches.head

    batches should have size 1
    batch.mechanism shouldBe FlowMechanism.TfiDepositDrain
    batch.asset shouldBe AssetType.DemandDeposit
    batch.from shouldBe EntitySector.Banks
    batch.to shouldBe EntitySector.Households
    totalTransferred(batches) shouldBe PLN(80000.0)

    val evidence = FlowSimulation.ExecutedFlowEvidence.from(batches)
    evidence.nbfiDepositDrain shouldBe PLN(80000.0)
  }

  it should "emit NBFI credit stock movements with explicit mechanisms" in {
    val batches = NbfiFlows.emitBatches(NbfiFlows.Input(PLN.Zero, PLN(300000.0), PLN(100000.0), PLN(50000.0)))

    mechanismBatches(batches, FlowMechanism.NbfiOrigination) should have size 1
    mechanismBatches(batches, FlowMechanism.NbfiRepayment) should have size 1
    mechanismBatches(batches, FlowMechanism.NbfiDefault) should have size 1

    val origination = mechanismBatches(batches, FlowMechanism.NbfiOrigination).head
    origination.asset shouldBe AssetType.NbfiLoan
    origination.from shouldBe EntitySector.Funds
    origination.to shouldBe EntitySector.Households

    val repayment = mechanismBatches(batches, FlowMechanism.NbfiRepayment).head
    repayment.asset shouldBe AssetType.NbfiLoan
    repayment.from shouldBe EntitySector.Households
    repayment.to shouldBe EntitySector.Funds

    val defaultAmount = mechanismBatches(batches, FlowMechanism.NbfiDefault).head
    defaultAmount.asset shouldBe AssetType.NbfiLoan
    defaultAmount.from shouldBe EntitySector.Households
    defaultAmount.to shouldBe EntitySector.Funds

    totalTransferred(mechanismBatches(batches, FlowMechanism.NbfiOrigination)) shouldBe PLN(300000.0)
    totalTransferred(mechanismBatches(batches, FlowMechanism.NbfiRepayment)) shouldBe PLN(100000.0)
    totalTransferred(mechanismBatches(batches, FlowMechanism.NbfiDefault)) shouldBe PLN(50000.0)
  }

  it should "emit no batch for zero monetary channels" in {
    NbfiFlows.emitBatches(NbfiFlows.Input(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)) shouldBe empty
    NbfiFlows.emit(NbfiFlows.Input(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)) shouldBe empty
  }

end NbfiFlowsSpec
