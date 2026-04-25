package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InvestmentDepositSettlementFlowsSpec extends AnyFlatSpec with Matchers:
  import RuntimeFlowsTestSupport.*

  private given RuntimeLedgerTopology = RuntimeLedgerTopology.nonZeroPopulation

  "InvestmentDepositSettlementFlows" should "preserve total wealth at exactly 0L" in {
    val flows    = InvestmentDepositSettlementFlows.emit(InvestmentDepositSettlementFlows.Input(PLN("250000.0")))
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)

    Interpreter.totalWealth(balances) shouldBe 0L
  }

  it should "emit positive investment net deposits as bank-issued firm deposits" in {
    val batches = InvestmentDepositSettlementFlows.emitBatches(InvestmentDepositSettlementFlows.Input(PLN("250000.0")))
    val batch   = batches.head

    batches should have size 1
    batch.mechanism shouldBe FlowMechanism.InvestmentDepositSettlement
    batch.asset shouldBe AssetType.DemandDeposit
    batch.from shouldBe EntitySector.Banks
    batch.to shouldBe EntitySector.Firms
    totalTransferred(batches) shouldBe PLN("250000.0")

    val evidence = FlowSimulation.ExecutedFlowEvidence.from(batches)
    evidence.investNetDepositFlow shouldBe PLN("250000.0")
  }

  it should "emit negative investment net deposits as firm deposit drawdown" in {
    val batches = InvestmentDepositSettlementFlows.emitBatches(InvestmentDepositSettlementFlows.Input(PLN("-125000.0")))
    val batch   = batches.head

    batches should have size 1
    batch.mechanism shouldBe FlowMechanism.InvestmentDepositSettlement
    batch.asset shouldBe AssetType.DemandDeposit
    batch.from shouldBe EntitySector.Firms
    batch.to shouldBe EntitySector.Banks
    totalTransferred(batches) shouldBe PLN("125000.0")

    val evidence = FlowSimulation.ExecutedFlowEvidence.from(batches)
    evidence.investNetDepositFlow shouldBe PLN("-125000.0")
  }

  it should "emit no batch for a zero investment net deposit flow" in {
    InvestmentDepositSettlementFlows.emitBatches(InvestmentDepositSettlementFlows.Input(PLN.Zero)) shouldBe empty
    InvestmentDepositSettlementFlows.emit(InvestmentDepositSettlementFlows.Input(PLN.Zero)) shouldBe empty
  }

end InvestmentDepositSettlementFlowsSpec
