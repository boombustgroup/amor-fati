package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NfzFlowsSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  "NfzFlows" should "preserve total wealth at exactly 0L" in {
    val flows    = NfzFlows.emit(NfzFlows.NfzInput.fromDrivers(employed = 80000, wage = PLN("7000.0"), workingAge = 90000, nRetirees = 1000))
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    Interpreter.totalWealth(balances) shouldBe 0L
  }

  it should "have NFZ balance = contributions - spending + govSubvention" in {
    val flows    = NfzFlows.emit(NfzFlows.NfzInput.fromDrivers(employed = 80000, wage = PLN("7000.0"), workingAge = 90000, nRetirees = 1000))
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)

    val contribs   = flows.filter(_.mechanism == FlowMechanism.NfzContribution.toInt).map(_.amount).sum
    val spending   = flows.filter(_.mechanism == FlowMechanism.NfzSpending.toInt).map(_.amount).sum
    val subvention = flows.filter(_.mechanism == FlowMechanism.NfzGovSubvention.toInt).map(_.amount).sum

    balances(NfzFlows.NFZ_ACCOUNT) shouldBe (contribs - spending + subvention)
  }

  it should "emit gov subvention only when spending exceeds contributions" in {
    // High retirees = high spending (aging elasticity 2.5x)
    val deficit = NfzFlows.emit(NfzFlows.NfzInput.fromDrivers(employed = 1000, wage = PLN("5000.0"), workingAge = 5000, nRetirees = 5000))
    deficit.exists(_.mechanism == FlowMechanism.NfzGovSubvention.toInt) shouldBe true

    // Very high wages, tiny population = surplus
    val surplus = NfzFlows.emit(NfzFlows.NfzInput.fromDrivers(employed = 80000, wage = PLN("50000.0"), workingAge = 1000, nRetirees = 10))
    surplus.exists(_.mechanism == FlowMechanism.NfzGovSubvention.toInt) shouldBe false
  }

  it should "match old SocialSecurity.nfzStep amounts exactly" in {
    val employed = 80000; val wage = PLN("7000.0"); val workingAge = 90000; val nRetirees = 1000

    val oldNfz = com.boombustgroup.amorfati.agents.SocialSecurity.nfzStep(employed, wage, workingAge, nRetirees)
    val flows  = NfzFlows.emit(NfzFlows.NfzInput(oldNfz))

    val newContribs   = flows.filter(_.mechanism == FlowMechanism.NfzContribution.toInt).map(_.amount).sum
    val newSpending   = flows.filter(_.mechanism == FlowMechanism.NfzSpending.toInt).map(_.amount).sum
    val newSubvention = flows.filter(_.mechanism == FlowMechanism.NfzGovSubvention.toInt).map(_.amount).sum

    newContribs shouldBe oldNfz.contributions.toLong
    newSpending shouldBe oldNfz.spending.toLong
    newSubvention shouldBe oldNfz.govSubvention.toLong
  }

  it should "preserve SFC across 120 months" in {
    val input    = NfzFlows.NfzInput.fromDrivers(employed = 80000, wage = PLN("7000.0"), workingAge = 90000, nRetirees = 1000)
    var balances = Map.empty[Int, Long]
    (1 to 120).foreach { _ =>
      balances = Interpreter.applyAll(balances, NfzFlows.emit(input))
      Interpreter.totalWealth(balances) shouldBe 0L
    }
  }
