package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MortgageFlowsSpec extends AnyFlatSpec with Matchers:

  "MortgageFlows" should "preserve total wealth at exactly 0L" in {
    val flows    = MortgageFlows.emit(MortgageFlows.Input(PLN(5000000.0), PLN(2000000.0), PLN(1500000.0), PLN(300000.0)))
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    Interpreter.totalWealth(balances) shouldBe 0L
  }

  it should "have bank balance = repayment + interest + default - origination" in {
    val input    = MortgageFlows.Input(PLN(5000000.0), PLN(2000000.0), PLN(1500000.0), PLN(300000.0))
    val flows    = MortgageFlows.emit(input)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    balances(MortgageFlows.BANK_ACCOUNT) shouldBe (input.principalRepayment + input.interest + input.defaultAmount - input.origination).toLong
  }

  it should "preserve SFC across 120 months" in {
    val input    = MortgageFlows.Input(PLN(5000000.0), PLN(2000000.0), PLN(1500000.0), PLN(300000.0))
    var balances = Map.empty[Int, Long]
    (1 to 120).foreach { _ =>
      balances = Interpreter.applyAll(balances, MortgageFlows.emit(input))
      Interpreter.totalWealth(balances) shouldBe 0L
    }
  }
