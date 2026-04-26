package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class JstFlowsSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  "JstFlows" should "preserve total wealth at exactly 0L" in {
    val flows    = JstFlows.emit(JstFlows.Input(PLN(5000000), PLN(50000000), PLN(100000000), 9000, PLN(3000000)))
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    Interpreter.totalWealth(balances) shouldBe 0L
  }

  it should "match old Jst.step revenue and spending" in {
    val centralCitRevenue = PLN(5000000); val wageIncome = PLN(50000000)
    val gdp               = PLN(100000000); val nFirms   = 9000; val pit = PLN(3000000)

    val oldJst = com.boombustgroup.amorfati.agents.Jst.step(
      com.boombustgroup.amorfati.agents.Jst.State.zero,
      PLN.Zero,
      centralCitRevenue,
      wageIncome,
      gdp,
      nFirms,
      pit,
    )
    val flows  = JstFlows.emit(JstFlows.Input(centralCitRevenue, wageIncome, gdp, nFirms, pit))

    val newTaxRevenue   = flows.filter(_.mechanism == FlowMechanism.JstRevenue.toInt).map(_.amount).sum
    val newGovSubsidies = flows.filter(_.mechanism == FlowMechanism.JstGovSubvention.toInt).map(_.amount).sum
    val newSpending     = flows.filter(_.mechanism == FlowMechanism.JstSpending.toInt).map(_.amount).sum

    newTaxRevenue + newGovSubsidies shouldBe oldJst.state.revenue.toLong
    newGovSubsidies should be > 0L
    newSpending shouldBe oldJst.state.spending.toLong
  }

  it should "preserve SFC across 120 months" in {
    val input    = JstFlows.Input(PLN(5000000), PLN(50000000), PLN(100000000), 9000, PLN(3000000))
    var balances = Map.empty[Int, Long]
    (1 to 120).foreach { _ =>
      balances = Interpreter.applyAll(balances, JstFlows.emit(input))
      Interpreter.totalWealth(balances) shouldBe 0L
    }
  }
