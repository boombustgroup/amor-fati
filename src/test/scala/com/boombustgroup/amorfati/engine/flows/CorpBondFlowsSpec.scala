package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CorpBondFlowsSpec extends AnyFlatSpec with Matchers:

  "CorpBondFlows" should "preserve total wealth at exactly 0L" in {
    val flows    = CorpBondFlows.emit(CorpBondFlows.Input(PLN(300000.0), PLN(50000.0), PLN(1000000.0), PLN(200000.0)))
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    Interpreter.totalWealth(balances) shouldBe 0L
  }

  it should "have firm balance = issuance - coupon - default amount - amortization" in {
    val input    = CorpBondFlows.Input(PLN(300000.0), PLN(50000.0), PLN(1000000.0), PLN(200000.0))
    val flows    = CorpBondFlows.emit(input)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    balances(CorpBondFlows.FIRM_ACCOUNT) shouldBe (input.issuance - input.coupon - input.defaultAmount - input.amortization).toLong
  }

  it should "preserve SFC across 120 months" in {
    val input    = CorpBondFlows.Input(PLN(300000.0), PLN(50000.0), PLN(1000000.0), PLN(200000.0))
    var balances = Map.empty[Int, Long]
    (1 to 120).foreach { _ =>
      balances = Interpreter.applyAll(balances, CorpBondFlows.emit(input))
      Interpreter.totalWealth(balances) shouldBe 0L
    }
  }
