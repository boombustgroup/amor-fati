package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CorpBondFlowsSpec extends AnyFlatSpec with Matchers:

  private def baseInput: CorpBondFlows.Input =
    CorpBondFlows.Input(
      coupon = PLN(300000),
      defaultAmount = PLN(50000),
      issuance = PLN(1000000),
      amortization = PLN(200000),
    )

  "CorpBondFlows" should "preserve total wealth at exactly 0L" in {
    val flows    = CorpBondFlows.emit(CorpBondFlows.Input(PLN(300000), PLN(50000), PLN(1000000), PLN(200000)))
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    Interpreter.totalWealth(balances) shouldBe 0L
  }

  it should "have firm balance = issuance - coupon - default amount - amortization" in {
    val input    = CorpBondFlows.Input(PLN(300000), PLN(50000), PLN(1000000), PLN(200000))
    val flows    = CorpBondFlows.emit(input)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    balances(CorpBondFlows.FIRM_ACCOUNT) shouldBe (input.issuance - input.coupon - input.defaultAmount - input.amortization).toLong
  }

  it should "reject explicit holder breakdowns that do not match input amounts" in {
    given RuntimeLedgerTopology = RuntimeLedgerTopology.nonZeroPopulation

    val mismatches = Vector(
      "couponRecipients"       -> baseInput.copy(couponRecipients = Some(CorpBondFlows.HolderBreakdown.copyToOther(PLN(299999)))),
      "defaultRecipients"      -> baseInput.copy(defaultRecipients = Some(CorpBondFlows.HolderBreakdown.copyToOther(PLN(49999)))),
      "issuanceRecipients"     -> baseInput.copy(issuanceRecipients = Some(CorpBondFlows.HolderBreakdown.copyToOther(PLN(999999)))),
      "amortizationRecipients" -> baseInput.copy(amortizationRecipients = Some(CorpBondFlows.HolderBreakdown.copyToOther(PLN(199999)))),
    )

    mismatches.foreach: (fieldName, input) =>
      val thrown = the[IllegalArgumentException] thrownBy CorpBondFlows.emitBatches(input)
      thrown.getMessage should include(fieldName)
  }
