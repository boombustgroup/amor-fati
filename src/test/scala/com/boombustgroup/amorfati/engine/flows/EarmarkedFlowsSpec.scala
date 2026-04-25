package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EarmarkedFlowsSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  "EarmarkedFlows" should "preserve total wealth at exactly 0L" in {
    val flows    = EarmarkedFlows.emit(EarmarkedFlows.Input(80000, PLN("7000.0"), PLN("1000000.0"), 10, 15))
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    Interpreter.totalWealth(balances) shouldBe 0L
  }

  it should "match old EarmarkedFunds.step amounts" in {
    val employed     = 80000; val wage                 = PLN("7000.0")
    val unempBenefit = PLN("1000000.0"); val nBankrupt = 10; val avgWorkers = 15

    val oldState = com.boombustgroup.amorfati.agents.EarmarkedFunds.step(
      employed,
      wage,
      unempBenefit,
      nBankrupt,
      avgWorkers,
    )
    val flows    = EarmarkedFlows.emit(EarmarkedFlows.Input(employed, wage, unempBenefit, nBankrupt, avgWorkers))

    val fp    = flows.filter(_.mechanism == FlowMechanism.FpContribution.toInt).map(_.amount).sum
    val pfron = flows.filter(_.mechanism == FlowMechanism.PfronContribution.toInt).map(_.amount).sum
    val fgsp  = flows.filter(_.mechanism == FlowMechanism.FgspContribution.toInt).map(_.amount).sum

    fp shouldBe oldState.fpContributions.toLong
    pfron shouldBe oldState.pfronContributions.toLong
    fgsp shouldBe oldState.fgspContributions.toLong
  }

  it should "preserve SFC across 120 months" in {
    val input    = EarmarkedFlows.Input(80000, PLN("7000.0"), PLN("1000000.0"), 5, 10)
    var balances = Map.empty[Int, Long]
    (1 to 120).foreach { _ =>
      balances = Interpreter.applyAll(balances, EarmarkedFlows.emit(input))
      Interpreter.totalWealth(balances) shouldBe 0L
    }
  }
