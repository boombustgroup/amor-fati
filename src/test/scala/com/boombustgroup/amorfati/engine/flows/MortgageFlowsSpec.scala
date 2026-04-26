package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.engine.ledger.MortgageRuntimeContract
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MortgageFlowsSpec extends AnyFlatSpec with Matchers:

  private given RuntimeLedgerTopology = RuntimeLedgerTopology.nonZeroPopulation

  "MortgageFlows" should "preserve total wealth at exactly 0L" in {
    val flows    = MortgageFlows.emit(MortgageFlows.Input(PLN(5000000), PLN(2000000), PLN(1500000), PLN(300000)))
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    Interpreter.totalWealth(balances) shouldBe 0L
  }

  it should "have bank balance = repayment + interest + default - origination" in {
    val input    = MortgageFlows.Input(PLN(5000000), PLN(2000000), PLN(1500000), PLN(300000))
    val flows    = MortgageFlows.emit(input)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    balances(MortgageFlows.BANK_ACCOUNT) shouldBe (input.principalRepayment + input.interest + input.defaultAmount - input.origination).toLong
  }

  it should "preserve SFC across 120 months" in {
    val input    = MortgageFlows.Input(PLN(5000000), PLN(2000000), PLN(1500000), PLN(300000))
    var balances = Map.empty[Int, Long]
    (1 to 120).foreach { _ =>
      balances = Interpreter.applyAll(balances, MortgageFlows.emit(input))
      Interpreter.totalWealth(balances) shouldBe 0L
    }
  }

  it should "route batched mortgage principal through the household settlement shell" in {
    val topology       = summon[RuntimeLedgerTopology]
    val input          = MortgageFlows.Input(PLN(5000000), PLN(2000000), PLN(1500000), PLN(300000))
    val batches        = MortgageFlows.emitBatches(input)
    val principalShell = MortgageRuntimeContract.principalSettlement(topology)
    val principal      = batches.filter(_.asset == AssetType.MortgageLoan)

    principal.map(_.mechanism).toSet shouldBe Set(
      FlowMechanism.MortgageOrigination,
      FlowMechanism.MortgageRepayment,
      FlowMechanism.MortgageDefault,
    )
    all(principal.map(_.from)) shouldBe EntitySector.Households
    all(principal.map(_.to)) shouldBe EntitySector.Households

    val principalBroadcasts = principal.collect { case broadcast: BatchedFlow.Broadcast => broadcast }
    withClue(s"Expected mortgage principal batches to be Broadcasts, got ${principal.map(_.getClass.getSimpleName)}"):
      principalBroadcasts should have size principal.size

    val byMechanism = principalBroadcasts.map(batch => batch.mechanism -> batch).toMap
    byMechanism(FlowMechanism.MortgageOrigination).fromIndex shouldBe principalShell.index
    byMechanism(FlowMechanism.MortgageOrigination).targetIndices.toVector shouldBe Vector(topology.households.aggregate)
    byMechanism(FlowMechanism.MortgageRepayment).fromIndex shouldBe topology.households.aggregate
    byMechanism(FlowMechanism.MortgageRepayment).targetIndices.toVector shouldBe Vector(principalShell.index)
    byMechanism(FlowMechanism.MortgageDefault).fromIndex shouldBe topology.households.aggregate
    byMechanism(FlowMechanism.MortgageDefault).targetIndices.toVector shouldBe Vector(principalShell.index)
  }

  it should "execute batched mortgage principal without bank-side MortgageLoan deltas" in {
    val topology       = summon[RuntimeLedgerTopology]
    val input          = MortgageFlows.Input(PLN(5000000), PLN(2000000), PLN(1500000), PLN(300000))
    val batches        = MortgageFlows.emitBatches(input)
    val state          = topology.emptyExecutionState()
    val principalShell = MortgageRuntimeContract.principalSettlement(topology)

    ImperativeInterpreter.planAndApplyAll(state, batches) shouldBe Right(())
    val snapshot            = state.snapshot
    val expected            = (input.origination - input.principalRepayment - input.defaultAmount).toLong
    val bankMortgageEntries = snapshot.filter { case ((sector, asset, _), _) => sector == EntitySector.Banks && asset == AssetType.MortgageLoan }

    withClue(s"Unexpected bank-side MortgageLoan deltas: $bankMortgageEntries"):
      bankMortgageEntries shouldBe Map.empty

    // Sign convention: shell records the negative side of outstanding principal.
    snapshot.getOrElse((EntitySector.Households, AssetType.MortgageLoan, topology.households.aggregate), 0L) shouldBe expected
    snapshot.getOrElse((EntitySector.Households, AssetType.MortgageLoan, principalShell.index), 0L) shouldBe -expected
  }
