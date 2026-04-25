package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FirmFlowsSpec extends AnyFlatSpec with Matchers:

  private val baseInput = FirmFlows.Input(
    householdIncome = PLN("50000000.0"),
    cit = PLN("5000000.0"),
    loanRepayment = PLN("3000000.0"),
    newLoans = PLN("4000000.0"),
    interestPaid = PLN("2000000.0"),
    capex = PLN("1500000.0"),
    equityIssuance = PLN("1000000.0"),
    ioPayments = PLN("10000000.0"),
    nplDefault = PLN("500000.0"),
    profitShifting = PLN("300000.0"),
    fdiRepatriation = PLN("200000.0"),
    grossInvestment = PLN("6000000.0"),
  )

  "FirmFlows" should "preserve total wealth at exactly 0L" in {
    val flows    = FirmFlows.emit(baseInput)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    Interpreter.totalWealth(balances) shouldBe 0L
  }

  it should "have correct firm balance (inflows - outflows)" in {
    val flows    = FirmFlows.emit(baseInput)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)

    val inflows  = baseInput.newLoans + baseInput.equityIssuance
    val outflows = baseInput.householdIncome + baseInput.cit + baseInput.loanRepayment +
      baseInput.interestPaid + baseInput.capex + baseInput.ioPayments +
      baseInput.nplDefault + baseInput.profitShifting + baseInput.fdiRepatriation +
      baseInput.grossInvestment

    balances(FirmFlows.FIRM_ACCOUNT) shouldBe (inflows - outflows).toLong
  }

  it should "have bank receiving loan repayment + interest + NPL - new loans" in {
    val flows    = FirmFlows.emit(baseInput)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)

    val bankNet = baseInput.loanRepayment + baseInput.interestPaid + baseInput.nplDefault - baseInput.newLoans
    balances(FirmFlows.BANK_ACCOUNT) shouldBe bankNet.toLong
  }

  it should "skip zero-amount flows" in {
    val minimal =
      FirmFlows.Input(
        householdIncome = PLN("1000000.0"),
        cit = PLN.Zero,
        loanRepayment = PLN.Zero,
        newLoans = PLN.Zero,
        interestPaid = PLN.Zero,
        capex = PLN.Zero,
        equityIssuance = PLN.Zero,
        ioPayments = PLN.Zero,
        nplDefault = PLN.Zero,
        profitShifting = PLN.Zero,
        fdiRepatriation = PLN.Zero,
        grossInvestment = PLN.Zero,
      )
    val flows   = FirmFlows.emit(minimal)
    flows.length shouldBe 1
    flows.head.mechanism shouldBe FlowMechanism.HhTotalIncome.toInt
  }

  it should "preserve SFC across 120 months" in {
    var balances = Map.empty[Int, Long]
    (1 to 120).foreach { _ =>
      balances = Interpreter.applyAll(balances, FirmFlows.emit(baseInput))
      Interpreter.totalWealth(balances) shouldBe 0L
    }
  }
