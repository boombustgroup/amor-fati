package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GovBudgetFlowsSpec extends AnyFlatSpec with Matchers:

  private val baseInput = GovBudgetFlows.Input(
    vatRevenue = PLN(3000000.0),
    exciseRevenue = PLN(1200000.0),
    customsDutyRevenue = PLN(800000.0),
    govPurchases = PLN(2000000.0),
    debtService = PLN(500000.0),
    unempBenefitSpend = PLN(800000.0),
    socialTransferSpend = PLN(1200000.0),
    euCofinancing = PLN(300000.0),
    govCapitalSpend = PLN(400000.0),
  )

  "GovBudgetFlows" should "preserve total wealth at exactly 0L" in {
    val flows    = GovBudgetFlows.emit(baseInput)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    Interpreter.totalWealth(balances) shouldBe 0L
  }

  it should "emit non-overlapping VAT, excise, and customs revenue legs" in {
    val mechanisms = GovBudgetFlows.emit(baseInput).take(3).map(_.mechanism)

    mechanisms shouldBe Vector(
      FlowMechanism.GovVatRevenue.toInt,
      FlowMechanism.GovExciseRevenue.toInt,
      FlowMechanism.GovCustomsDutyRevenue.toInt,
    )
  }

  it should "have GOV balance = revenue - total spending" in {
    val flows    = GovBudgetFlows.emit(baseInput)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)

    val totalRevenue  = baseInput.vatRevenue + baseInput.exciseRevenue + baseInput.customsDutyRevenue
    val totalSpending = baseInput.govPurchases + baseInput.debtService +
      baseInput.unempBenefitSpend + baseInput.socialTransferSpend +
      baseInput.euCofinancing + baseInput.govCapitalSpend

    val expectedGovBalance = totalRevenue - totalSpending
    balances(GovBudgetFlows.GOV_ACCOUNT) shouldBe expectedGovBalance.toLong
  }

  it should "have deficit when spending > revenue" in {
    val flows    = GovBudgetFlows.emit(baseInput)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    balances(GovBudgetFlows.GOV_ACCOUNT) should be < 0L
  }

  it should "have surplus when revenue > spending" in {
    val surplus  = baseInput.copy(vatRevenue = PLN(47000000.0))
    val flows    = GovBudgetFlows.emit(surplus)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    balances(GovBudgetFlows.GOV_ACCOUNT) should be > 0L
  }

  it should "skip zero-amount flows" in {
    val minimal = GovBudgetFlows.Input(
      vatRevenue = PLN(1000000.0),
      exciseRevenue = PLN.Zero,
      customsDutyRevenue = PLN.Zero,
      govPurchases = PLN.Zero,
      debtService = PLN.Zero,
      unempBenefitSpend = PLN.Zero,
      socialTransferSpend = PLN.Zero,
      euCofinancing = PLN.Zero,
      govCapitalSpend = PLN.Zero,
    )
    val flows   = GovBudgetFlows.emit(minimal)
    flows.length shouldBe 1
    flows.head.mechanism shouldBe FlowMechanism.GovVatRevenue.toInt
  }

  it should "preserve SFC across 120 months" in {
    var balances = Map.empty[Int, Long]
    (1 to 120).foreach { _ =>
      balances = Interpreter.applyAll(balances, GovBudgetFlows.emit(baseInput))
      Interpreter.totalWealth(balances) shouldBe 0L
    }
  }
