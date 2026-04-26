package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class QuasiFiscalFlowsSpec extends AnyFlatSpec with Matchers:
  import RuntimeFlowsTestSupport.*

  private given RuntimeLedgerTopology = RuntimeLedgerTopology.nonZeroPopulation

  private def onlyBroadcast(batches: Vector[BatchedFlow]): BatchedFlow.Broadcast =
    batches should have size 1
    batches.head match
      case broadcast: BatchedFlow.Broadcast => broadcast
      case other                            => fail(s"Expected Broadcast, got $other")

  private def onlyScatter(batches: Vector[BatchedFlow]): BatchedFlow.Scatter =
    batches should have size 1
    batches.head match
      case scatter: BatchedFlow.Scatter => scatter
      case other                        => fail(s"Expected Scatter, got $other")

  private val input = QuasiFiscalFlows.Input(
    bankBondIssuance = PLN(700000),
    nbpBondAbsorption = PLN(300000),
    bankBondAmortization = PLN(200000),
    nbpBondAmortization = PLN(100000),
    lending = PLN(500000),
    repayment = PLN(150000),
  )

  "QuasiFiscalFlows" should "emit quasi-fiscal bond issuance and amortization through supported holders" in {
    val batches = QuasiFiscalFlows.emitBatches(input)

    val bankIssuance = onlyBroadcast(mechanismBatches(batches, FlowMechanism.QuasiFiscalBondIssuance))
    bankIssuance.from shouldBe EntitySector.Funds
    bankIssuance.fromIndex shouldBe summon[RuntimeLedgerTopology].funds.quasiFiscal
    bankIssuance.to shouldBe EntitySector.Banks
    bankIssuance.asset shouldBe AssetType.QuasiFiscalBond
    bankIssuance.targetIndices.toVector shouldBe Vector(0, 1)
    totalTransferred(Vector(bankIssuance)) shouldBe input.bankBondIssuance

    val nbpAbsorption = onlyBroadcast(mechanismBatches(batches, FlowMechanism.QuasiFiscalNbpAbsorption))
    nbpAbsorption.from shouldBe EntitySector.Funds
    nbpAbsorption.fromIndex shouldBe summon[RuntimeLedgerTopology].funds.quasiFiscal
    nbpAbsorption.to shouldBe EntitySector.NBP
    nbpAbsorption.targetIndices.toVector shouldBe Vector(summon[RuntimeLedgerTopology].nbp.persistedOwner)
    nbpAbsorption.asset shouldBe AssetType.QuasiFiscalBond
    totalTransferred(Vector(nbpAbsorption)) shouldBe input.nbpBondAbsorption

    val bankAmortization = onlyScatter(mechanismBatches(batches, FlowMechanism.QuasiFiscalBondAmortization))
    bankAmortization.from shouldBe EntitySector.Banks
    bankAmortization.to shouldBe EntitySector.Funds
    bankAmortization.asset shouldBe AssetType.QuasiFiscalBond
    bankAmortization.targetIndices.toVector.distinct shouldBe Vector(summon[RuntimeLedgerTopology].funds.quasiFiscal)
    bankAmortization.amounts.toVector.drop(summon[RuntimeLedgerTopology].banks.persistedCount) shouldBe Vector(0L)
    totalTransferred(Vector(bankAmortization)) shouldBe input.bankBondAmortization

    val nbpAmortization = onlyBroadcast(mechanismBatches(batches, FlowMechanism.QuasiFiscalNbpBondAmortization))
    nbpAmortization.from shouldBe EntitySector.NBP
    nbpAmortization.fromIndex shouldBe summon[RuntimeLedgerTopology].nbp.persistedOwner
    nbpAmortization.to shouldBe EntitySector.Funds
    nbpAmortization.targetIndices.toVector shouldBe Vector(summon[RuntimeLedgerTopology].funds.quasiFiscal)
    nbpAmortization.asset shouldBe AssetType.QuasiFiscalBond
    totalTransferred(Vector(nbpAmortization)) shouldBe input.nbpBondAmortization
  }

  it should "emit explicit lending portfolio and deposit creation legs" in {
    val batches = QuasiFiscalFlows.emitBatches(input)

    val lending = onlyBroadcast(mechanismBatches(batches, FlowMechanism.QuasiFiscalLending))
    lending.from shouldBe EntitySector.Funds
    lending.fromIndex shouldBe summon[RuntimeLedgerTopology].funds.quasiFiscal
    lending.to shouldBe EntitySector.Firms
    lending.targetIndices.toVector shouldBe Vector(summon[RuntimeLedgerTopology].firms.aggregate)
    lending.asset shouldBe AssetType.NbfiLoan
    totalTransferred(Vector(lending)) shouldBe input.lending

    val repayment = onlyBroadcast(mechanismBatches(batches, FlowMechanism.QuasiFiscalRepayment))
    repayment.from shouldBe EntitySector.Firms
    repayment.fromIndex shouldBe summon[RuntimeLedgerTopology].firms.aggregate
    repayment.to shouldBe EntitySector.Funds
    repayment.targetIndices.toVector shouldBe Vector(summon[RuntimeLedgerTopology].funds.quasiFiscal)
    repayment.asset shouldBe AssetType.NbfiLoan
    totalTransferred(Vector(repayment)) shouldBe input.repayment

    val lendingDeposit = onlyBroadcast(mechanismBatches(batches, FlowMechanism.QuasiFiscalLendingDeposit))
    lendingDeposit.from shouldBe EntitySector.Banks
    lendingDeposit.fromIndex shouldBe summon[RuntimeLedgerTopology].banks.aggregate
    lendingDeposit.to shouldBe EntitySector.Firms
    lendingDeposit.targetIndices.toVector shouldBe Vector(summon[RuntimeLedgerTopology].firms.aggregate)
    lendingDeposit.asset shouldBe AssetType.DemandDeposit

    val repaymentDeposit = onlyBroadcast(mechanismBatches(batches, FlowMechanism.QuasiFiscalRepaymentDeposit))
    repaymentDeposit.from shouldBe EntitySector.Firms
    repaymentDeposit.fromIndex shouldBe summon[RuntimeLedgerTopology].firms.aggregate
    repaymentDeposit.to shouldBe EntitySector.Banks
    repaymentDeposit.targetIndices.toVector shouldBe Vector(summon[RuntimeLedgerTopology].banks.aggregate)
    repaymentDeposit.asset shouldBe AssetType.DemandDeposit

    val evidence = FlowSimulation.ExecutedFlowEvidence.from(batches)
    evidence.quasiFiscalBondIssuance shouldBe input.bankBondIssuance + input.nbpBondAbsorption
    evidence.quasiFiscalBondAmortization shouldBe input.bankBondAmortization + input.nbpBondAmortization
    evidence.quasiFiscalNbpAbsorption shouldBe input.nbpBondAbsorption
    evidence.quasiFiscalNbpBondAmortization shouldBe input.nbpBondAmortization
    evidence.quasiFiscalLending shouldBe input.lending
    evidence.quasiFiscalRepayment shouldBe input.repayment
    evidence.quasiFiscalDepositChange shouldBe input.lending - input.repayment
  }

  it should "emit no batch for zero monetary channels" in {
    QuasiFiscalFlows.emitBatches(QuasiFiscalFlows.Input(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)) shouldBe empty
  }

end QuasiFiscalFlowsSpec
