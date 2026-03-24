package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class OpenEconFlowsSpec extends AnyFlatSpec with Matchers:

  private val baseInput = OpenEconFlows.Input(
    exports = PLN(20000000.0),
    imports = PLN(18000000.0),
    tourismExport = PLN(1000000.0),
    tourismImport = PLN(800000.0),
    fdi = PLN(2000000.0),
    portfolioFlows = PLN(500000.0),
    primaryIncome = PLN(-300000.0),
    euFunds = PLN(1500000.0),
    diasporaInflow = PLN(400000.0),
    capitalFlightOutflow = PLN(200000.0),
  )

  "OpenEconFlows" should "preserve total wealth at exactly 0L" in {
    val flows    = OpenEconFlows.emit(baseInput)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    Interpreter.totalWealth(balances) shouldBe 0L
  }

  it should "have correct domestic balance (CA + KA)" in {
    val flows    = OpenEconFlows.emit(baseInput)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)

    val inflows  = baseInput.exports + baseInput.tourismExport + baseInput.fdi +
      baseInput.portfolioFlows + baseInput.euFunds + baseInput.diasporaInflow
    val outflows = baseInput.imports + baseInput.tourismImport +
      (-baseInput.primaryIncome) + baseInput.capitalFlightOutflow

    balances(OpenEconFlows.DOMESTIC_ACCOUNT) shouldBe (inflows - outflows).toLong
  }

  it should "handle negative portfolio flows (outflow)" in {
    val outflow  = baseInput.copy(portfolioFlows = PLN(-1000000.0))
    val flows    = OpenEconFlows.emit(outflow)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    Interpreter.totalWealth(balances) shouldBe 0L
    // Portfolio outflow debits domestic
    flows.filter(_.mechanism == FlowMechanism.PortfolioFlow.toInt).head.from shouldBe OpenEconFlows.DOMESTIC_ACCOUNT
  }

  it should "handle negative primary income (NFA payment)" in {
    val payment  = baseInput.copy(primaryIncome = PLN(-500000.0))
    val flows    = OpenEconFlows.emit(payment)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    Interpreter.totalWealth(balances) shouldBe 0L
  }

  it should "preserve SFC across 120 months" in {
    var balances = Map.empty[Int, Long]
    (1 to 120).foreach { _ =>
      balances = Interpreter.applyAll(balances, OpenEconFlows.emit(baseInput))
      Interpreter.totalWealth(balances) shouldBe 0L
    }
  }
