package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.agents.Banking
import com.boombustgroup.amorfati.agents.Banking.BankStatus
import com.boombustgroup.amorfati.engine.economics.BankingEconomics
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GovBondFlowsSpec extends AnyFlatSpec with Matchers:

  private val topology = RuntimeLedgerTopology.nonZeroPopulation

  private def bank(id: Int): Banking.BankState =
    Banking.BankState(
      id = BankId(id),
      capital = PLN("1000.0"),
      nplAmount = PLN.Zero,
      htmBookYield = Rate("0.05"),
      status = BankStatus.Active(0),
      loansShort = PLN.Zero,
      loansMedium = PLN.Zero,
      loansLong = PLN.Zero,
      consumerNpl = PLN.Zero,
    )

  private def stocks(afs: PLN, htm: PLN): Banking.BankFinancialStocks =
    Banking.BankFinancialStocks(
      totalDeposits = PLN("1000.0"),
      demandDeposit = PLN.Zero,
      termDeposit = PLN.Zero,
      firmLoan = PLN.Zero,
      consumerLoan = PLN.Zero,
      govBondAfs = afs,
      govBondHtm = htm,
      reserve = PLN.Zero,
      interbankLoan = PLN.Zero,
    )

  private def movements(
      primaryByBank: Vector[PLN] = Vector.fill(topology.banks.persistedCount)(PLN.Zero),
      ppkPurchaseByBank: Vector[PLN] = Vector.fill(topology.banks.persistedCount)(PLN.Zero),
  ): BankingEconomics.GovBondRuntimeMovements =
    BankingEconomics.GovBondRuntimeMovements(
      primaryByBank = primaryByBank,
      foreignPurchaseByBank = Vector.fill(topology.banks.persistedCount)(PLN.Zero),
      nbpQePurchaseByBank = Vector.fill(topology.banks.persistedCount)(PLN.Zero),
      ppkPurchaseByBank = ppkPurchaseByBank,
      insurancePurchaseByBank = Vector.fill(topology.banks.persistedCount)(PLN.Zero),
      tfiPurchaseByBank = Vector.fill(topology.banks.persistedCount)(PLN.Zero),
    )

  "GovBondFlows" should "emit PPK purchases from actual partial-fill sales instead of requested amounts" in {
    val banks      = Vector(bank(0), bank(1))
    val bankStocks = Vector(stocks(PLN("100.0"), PLN("50.0")), stocks(PLN("80.0"), PLN("70.0")))
    val requested  = PLN("1000.0")
    val sale       = Banking.sellToBuyer(banks, bankStocks, requested)

    sale.actualSold shouldBe PLN("300.0")
    sale.actualSold should be < requested
    sale.soldByBank.sumPln shouldBe sale.actualSold

    val batches = GovBondFlows.emitBatches(movements(ppkPurchaseByBank = sale.soldByBank))(using topology)

    batches should have size 1
    val ppkBatch = batches.head
    ppkBatch.mechanism shouldBe FlowMechanism.PpkBondPurchase
    ppkBatch.from shouldBe EntitySector.Banks
    ppkBatch.to shouldBe EntitySector.Funds
    RuntimeLedgerTopology.totalTransferred(ppkBatch) shouldBe sale.actualSold.toLong

    ppkBatch match
      case scatter: BatchedFlow.Scatter =>
        scatter.amounts.toVector.take(sale.soldByBank.length) shouldBe sale.soldByBank.map(_.toLong)
        scatter.amounts.last shouldBe 0L
        scatter.targetIndices.toSet shouldBe Set(topology.funds.ppk)
      case other                        => fail(s"Expected bank scatter for PPK purchase, got $other")
  }

  it should "reject primary-market vectors that target non-persisted bank slots" in {
    val malformedPrimary = Vector.fill(topology.banks.persistedCount + 1)(PLN("1.0"))

    val thrown = the[IllegalArgumentException] thrownBy GovBondFlows.emitBatches(
      movements(primaryByBank = malformedPrimary),
    )(using topology)

    thrown.getMessage should include("primaryByBank")
  }

  it should "name malformed bank-sale movement vectors in validation errors" in {
    val malformedPpkPurchase = Vector.fill(topology.banks.persistedCount + 1)(PLN("1.0"))

    val thrown = the[IllegalArgumentException] thrownBy GovBondFlows.emitBatches(
      movements(ppkPurchaseByBank = malformedPpkPurchase),
    )(using topology)

    thrown.getMessage should include("ppkPurchaseByBank")
  }

  it should "allow primary-market issuance to the first persisted bank" in {
    val batches = GovBondFlows.emitBatches(
      movements(primaryByBank = Vector(PLN("10.0"), PLN.Zero)),
    )(using topology)

    batches should have size 1
    val primaryBatch = batches.head
    primaryBatch.mechanism shouldBe FlowMechanism.GovBondPrimaryMarket
    primaryBatch.from shouldBe EntitySector.Government
    primaryBatch.to shouldBe EntitySector.Banks

    primaryBatch match
      case broadcast: BatchedFlow.Broadcast =>
        broadcast.targetIndices.toVector shouldBe Vector(0)
        broadcast.amounts.toVector shouldBe Vector(PLN("10.0").toLong)
      case other                            => fail(s"Expected government-to-bank broadcast, got $other")
  }

end GovBondFlowsSpec
