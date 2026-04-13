package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EquityFlowsSpec extends AnyFlatSpec with Matchers:

  "EquityFlows" should "preserve total wealth at exactly 0L" in {
    val flows    = EquityFlows.emit(EquityFlows.Input(PLN(500000.0), PLN(200000.0), PLN(100000.0), PLN.Zero, PLN(1000000.0)))
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    Interpreter.totalWealth(balances) shouldBe 0L
  }

  it should "have firm balance = -dividends + issuance" in {
    val input    = EquityFlows.Input(PLN(500000.0), PLN(200000.0), PLN(100000.0), PLN.Zero, PLN(1000000.0))
    val flows    = EquityFlows.emit(input)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    balances(EquityFlows.FIRM_ACCOUNT) shouldBe (input.issuance - input.netDomesticDividends - input.foreignDividends).toLong
  }

  it should "preserve SFC across 120 months" in {
    val input    = EquityFlows.Input(PLN(500000.0), PLN(200000.0), PLN(100000.0), PLN.Zero, PLN(1000000.0))
    var balances = Map.empty[Int, Long]
    (1 to 120).foreach { _ =>
      balances = Interpreter.applyAll(balances, EquityFlows.emit(input))
      Interpreter.totalWealth(balances) shouldBe 0L
    }
  }

  it should "emit a dedicated firm-to-government SOE dividend flow when government extraction is positive" in {
    val input = EquityFlows.Input(PLN.Zero, PLN.Zero, PLN.Zero, PLN(250000.0), PLN.Zero)
    val flows = EquityFlows.emit(input)

    flows.exists(flow =>
      flow.from == EquityFlows.FIRM_ACCOUNT &&
        flow.to == EquityFlows.GOV_ACCOUNT &&
        flow.amount == input.govDividends.toLong &&
        flow.mechanism == FlowMechanism.EquityGovDividend.toInt,
    ) shouldBe true
  }
