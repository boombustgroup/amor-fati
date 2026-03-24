package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BankingFlowsSpec extends AnyFlatSpec with Matchers:

  private val baseInput = BankingFlows.Input(
    govBondIncome = PLN(3000000.0),
    reserveInterest = PLN(500000.0),
    standingFacilityIncome = PLN(100000.0),
    interbankInterest = PLN(200000.0),
    bfgLevy = PLN(400000.0),
    unrealizedBondLoss = PLN(150000.0),
    bailInLoss = PLN.Zero,
    nbpRemittance = PLN(800000.0),
  )

  "BankingFlows" should "preserve total wealth at exactly 0L" in {
    val flows    = BankingFlows.emit(baseInput)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    Interpreter.totalWealth(balances) shouldBe 0L
  }

  it should "have correct bank balance (income - outflows)" in {
    val flows    = BankingFlows.emit(baseInput)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)

    val income   = baseInput.govBondIncome + baseInput.reserveInterest + baseInput.standingFacilityIncome + baseInput.interbankInterest
    val outflows = baseInput.bfgLevy + baseInput.unrealizedBondLoss + baseInput.nbpRemittance
    val bailIn   = baseInput.bailInLoss

    balances(BankingFlows.BANK_ACCOUNT) shouldBe (income - outflows + bailIn).toLong
  }

  it should "handle negative interbank interest (net cost)" in {
    val netCost  = baseInput.copy(interbankInterest = PLN(-300000.0))
    val flows    = BankingFlows.emit(netCost)
    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    Interpreter.totalWealth(balances) shouldBe 0L

    val ibFlow = flows.filter(_.mechanism == FlowMechanism.BankInterbankInterest.toInt).head
    ibFlow.from shouldBe BankingFlows.BANK_ACCOUNT
  }

  it should "handle bail-in (depositor loss)" in {
    val withBailIn = baseInput.copy(bailInLoss = PLN(5000000.0))
    val flows      = BankingFlows.emit(withBailIn)
    val balances   = Interpreter.applyAll(Map.empty[Int, Long], flows)
    Interpreter.totalWealth(balances) shouldBe 0L

    balances(BankingFlows.DEPOSITOR_ACCOUNT) should be < 0L
  }

  it should "preserve SFC across 120 months" in {
    var balances = Map.empty[Int, Long]
    (1 to 120).foreach { _ =>
      balances = Interpreter.applyAll(balances, BankingFlows.emit(baseInput))
      Interpreter.totalWealth(balances) shouldBe 0L
    }
  }
