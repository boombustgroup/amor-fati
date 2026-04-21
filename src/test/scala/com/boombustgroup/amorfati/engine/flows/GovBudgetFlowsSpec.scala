package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.engine.ledger.GovernmentBondCircuit
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GovBudgetFlowsSpec extends AnyFlatSpec with Matchers:

  private def debtRecipients(amount: PLN): GovBudgetFlows.DebtServiceRecipients =
    GovBudgetFlows.DebtServiceRecipients(
      banks = amount,
      foreign = PLN.Zero,
      nbp = PLN.Zero,
      insurance = PLN.Zero,
      ppk = PLN.Zero,
      tfi = PLN.Zero,
    )

  private val baseInput = GovBudgetFlows.Input(
    vatRevenue = PLN(3000000.0),
    exciseRevenue = PLN(1200000.0),
    customsDutyRevenue = PLN(800000.0),
    govCurrentSpend = PLN(2000000.0),
    debtService = PLN(500000.0),
    unempBenefitSpend = PLN(800000.0),
    socialTransferSpend = PLN(1200000.0),
    euCofinancing = PLN(300000.0),
    govCapitalSpend = PLN(400000.0),
    debtServiceRecipients = Some(debtRecipients(PLN(500000.0))),
  )

  "GovBudgetFlows" should "preserve total wealth at exactly 0L" in {
    val flows    = GovBudgetFlows.emit(baseInput)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    Interpreter.totalWealth(balances) shouldBe 0L
  }

  it should "emit non-overlapping VAT, excise, and customs revenue legs" in {
    val emittedMechanisms         = GovBudgetFlows.emit(baseInput).map(_.mechanism).toVector
    val expectedRevenueMechanisms =
      GovBudgetFlows.DirectTreasuryRevenueMechanisms.map(_.toInt).toSet
    val revenueMechanisms         =
      emittedMechanisms.filter(expectedRevenueMechanisms.contains)

    revenueMechanisms.toSet shouldBe expectedRevenueMechanisms
    emittedMechanisms should contain allElementsOf expectedRevenueMechanisms
  }

  it should "have GOV balance = revenue - total spending" in {
    val flows    = GovBudgetFlows.emit(baseInput)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)

    val totalRevenue  = baseInput.vatRevenue + baseInput.exciseRevenue + baseInput.customsDutyRevenue
    val totalSpending = baseInput.govCurrentSpend + baseInput.debtService +
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
      govCurrentSpend = PLN.Zero,
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

  it should "require explicit debt-service recipients for positive debt service" in {
    val thrown = the[IllegalArgumentException] thrownBy GovBudgetFlows.emit(
      baseInput.copy(debtServiceRecipients = None),
    )

    thrown.getMessage should include("debtServiceRecipients")
  }

  it should "reject debt-service allocation when the bond circuit has no holders" in {
    val emptyCircuit = GovernmentBondCircuit(
      outstanding = PLN.Zero,
      bankHoldings = PLN.Zero,
      foreignHoldings = PLN.Zero,
      nbpHoldings = PLN.Zero,
      insuranceHoldings = PLN.Zero,
      ppkHoldings = PLN.Zero,
      tfiHoldings = PLN.Zero,
    )

    val thrown = the[IllegalArgumentException] thrownBy GovBudgetFlows.DebtServiceRecipients.fromCircuit(
      emptyCircuit,
      PLN(1.0),
    )

    thrown.getMessage should include("positive holder weights")
  }

  it should "preserve SFC across 120 months" in {
    var balances = Map.empty[Int, Long]
    (1 to 120).foreach { _ =>
      balances = Interpreter.applyAll(balances, GovBudgetFlows.emit(baseInput))
      Interpreter.totalWealth(balances) shouldBe 0L
    }
  }
