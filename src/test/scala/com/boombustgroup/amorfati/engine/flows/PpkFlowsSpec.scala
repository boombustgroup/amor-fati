package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PpkFlowsSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  "PpkFlows" should "preserve total wealth at exactly 0L" in {
    val flows    = PpkFlows.emit(PpkFlows.PpkInput(employed = 80000, wage = PLN(7000)))
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    Interpreter.totalWealth(balances) shouldBe 0L
  }

  it should "match SocialSecurity.ppkStep contribution amounts exactly" in {
    val employed = 80000; val wage = PLN(7000)
    val oldPpk   = com.boombustgroup.amorfati.agents.SocialSecurity.ppkStep(employed, wage)
    val flows    = PpkFlows.emit(PpkFlows.PpkInput(employed, wage))

    val newContribs = flows.filter(_.mechanism == FlowMechanism.PpkContribution.toInt).map(_.amount).sum
    val newBond     = flows.filter(_.mechanism == FlowMechanism.PpkBondPurchase.toInt).map(_.amount).sum

    newContribs shouldBe oldPpk.contributions.toLong
    newBond shouldBe 0L
  }
