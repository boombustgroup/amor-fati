package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** ZUS mechanism through flow interpreter — SFC proof of concept.
  *
  * Verifies that ZUS contributions, pensions, and gov subvention close at
  * exactly 0L when applied through the verified interpreter. No tolerance, no
  * rounding residual.
  */
class ZusFlowsSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  "ZusFlows" should "emit contribution + pension flows with correct amounts" in {
    val input = ZusFlows.ZusInput(employed = 80000, wage = PLN("7000.0"), nRetirees = 1000)
    val flows = ZusFlows.emit(input)

    flows should not be empty

    val contribFlow = flows.find(_.mechanism == FlowMechanism.ZusContribution.toInt).get
    contribFlow.from shouldBe ZusFlows.HH_ACCOUNT
    contribFlow.to shouldBe ZusFlows.FUS_ACCOUNT
    contribFlow.amount should be > 0L

    val pensionFlow = flows.find(_.mechanism == FlowMechanism.ZusPension.toInt).get
    pensionFlow.from shouldBe ZusFlows.FUS_ACCOUNT
    pensionFlow.to shouldBe ZusFlows.HH_ACCOUNT
    pensionFlow.amount should be > 0L
  }

  it should "preserve total wealth at exactly 0L (SFC by construction)" in {
    val input    = ZusFlows.ZusInput(employed = 80000, wage = PLN("7000.0"), nRetirees = 1000)
    val flows    = ZusFlows.emit(input)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)

    // SFC: total wealth is exactly zero (started from empty, all flows are transfers)
    Interpreter.totalWealth(balances) shouldBe 0L
  }

  it should "have FUS balance = contributions - pensions + govSubvention" in {
    val input    = ZusFlows.ZusInput(employed = 80000, wage = PLN("7000.0"), nRetirees = 1000)
    val flows    = ZusFlows.emit(input)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)

    val contribs   = flows.filter(_.mechanism == FlowMechanism.ZusContribution.toInt).map(_.amount).sum
    val pensions   = flows.filter(_.mechanism == FlowMechanism.ZusPension.toInt).map(_.amount).sum
    val subvention = flows.filter(_.mechanism == FlowMechanism.ZusGovSubvention.toInt).map(_.amount).sum

    // FUS receives contributions + subvention, pays out pensions
    balances(ZusFlows.FUS_ACCOUNT) shouldBe (contribs - pensions + subvention)
  }

  it should "emit gov subvention only when pensions exceed contributions" in {
    // High retirees, low employed → deficit
    val deficit = ZusFlows.ZusInput(employed = 1000, wage = PLN("5000.0"), nRetirees = 5000)
    val flows1  = ZusFlows.emit(deficit)
    flows1.exists(_.mechanism == FlowMechanism.ZusGovSubvention.toInt) shouldBe true

    // High employed, low retirees → surplus
    val surplus = ZusFlows.ZusInput(employed = 80000, wage = PLN("7000.0"), nRetirees = 100)
    val flows2  = ZusFlows.emit(surplus)
    flows2.exists(_.mechanism == FlowMechanism.ZusGovSubvention.toInt) shouldBe false
  }

  it should "match old SocialSecurity.zusStep amounts exactly" in {
    val employed  = 80000
    val wage      = PLN("7000.0")
    val nRetirees = 1000

    // Old path
    val oldZus = com.boombustgroup.amorfati.agents.SocialSecurity.zusStep(employed, wage, nRetirees)

    // New path
    val flows = ZusFlows.emit(ZusFlows.ZusInput(employed, wage, nRetirees))

    val newContribs   = flows.filter(_.mechanism == FlowMechanism.ZusContribution.toInt).map(_.amount).sum
    val newPensions   = flows.filter(_.mechanism == FlowMechanism.ZusPension.toInt).map(_.amount).sum
    val newSubvention = flows.filter(_.mechanism == FlowMechanism.ZusGovSubvention.toInt).map(_.amount).sum

    // Amounts must match bit-for-bit (same formulas, same Long arithmetic)
    newContribs shouldBe oldZus.contributions.toLong
    newPensions shouldBe oldZus.pensionPayments.toLong
    newSubvention shouldBe oldZus.govSubvention.toLong
  }

  it should "preserve SFC across multiple months" in {
    val input = ZusFlows.ZusInput(employed = 80000, wage = PLN("7000.0"), nRetirees = 1000)

    var balances = Map.empty[Int, Long]
    (1 to 120).foreach { _ =>
      val flows = ZusFlows.emit(input)
      balances = Interpreter.applyAll(balances, flows)
      Interpreter.totalWealth(balances) shouldBe 0L
    }
  }
