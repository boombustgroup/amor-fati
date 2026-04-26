package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EarmarkedFundsSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  private val wage     = PLN(8000)
  private val employed = 80000

  "EarmarkedFunds.step" should "compute FP contributions from payroll" in {
    val result = EarmarkedFunds.step(employed, wage, PLN.Zero, 0, 0)
    result.fpContributions should be > PLN.Zero
  }

  it should "compute PFRON revenue" in {
    val result = EarmarkedFunds.step(employed, wage, PLN.Zero, 0, 0)
    result.pfronContributions should be > PLN.Zero
  }

  it should "compute FGSP contributions from payroll" in {
    val result = EarmarkedFunds.step(employed, wage, PLN.Zero, 0, 0)
    result.fgspContributions should be > PLN.Zero
  }

  it should "increase FGSP spending with more bankruptcies" in {
    val noBankrupt   = EarmarkedFunds.step(employed, wage, PLN.Zero, 0, 0)
    val manyBankrupt = EarmarkedFunds.step(employed, wage, PLN.Zero, 50, 10)
    manyBankrupt.fgspSpending should be > noBankrupt.fgspSpending
  }

  it should "produce gov subvention when funds in deficit" in {
    // Large unemployment benefit spend -> FP deficit -> subvention
    val result = EarmarkedFunds.step(employed, wage, PLN(50000000000L), 100, 20)
    result.totalGovSubvention should be > PLN.Zero
  }

  it should "accumulate balances across months" in {
    val m1        = EarmarkedFunds.step(employed, wage, PLN.Zero, 0, 0)
    val m1Balance = EarmarkedFunds.fpCashAfter(PLN.Zero, m1)
    val m2        = EarmarkedFunds.step(employed, wage, PLN.Zero, 0, 0)
    val m2Balance = EarmarkedFunds.fpCashAfter(m1Balance, m2)
    // FP balance should grow (contributions > ALMP with zero unemp benefits)
    m2Balance should be > m1Balance
  }
