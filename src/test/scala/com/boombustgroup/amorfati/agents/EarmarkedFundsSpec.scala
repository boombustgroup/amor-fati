package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EarmarkedFundsSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  private val wage     = PLN(8000.0)
  private val employed = 80000

  "EarmarkedFunds.step" should "compute FP contributions from payroll" in {
    val result = EarmarkedFunds.step(EarmarkedFunds.State.zero, employed, wage, PLN.Zero, 0, 0)
    result.fpContributions.toDouble should be > 0.0
  }

  it should "compute PFRON revenue" in {
    val result = EarmarkedFunds.step(EarmarkedFunds.State.zero, employed, wage, PLN.Zero, 0, 0)
    result.pfronContributions.toDouble should be > 0.0
  }

  it should "compute FGŚP contributions from payroll" in {
    val result = EarmarkedFunds.step(EarmarkedFunds.State.zero, employed, wage, PLN.Zero, 0, 0)
    result.fgspContributions.toDouble should be > 0.0
  }

  it should "increase FGŚP spending with more bankruptcies" in {
    val noBankrupt   = EarmarkedFunds.step(EarmarkedFunds.State.zero, employed, wage, PLN.Zero, 0, 0)
    val manyBankrupt = EarmarkedFunds.step(EarmarkedFunds.State.zero, employed, wage, PLN.Zero, 50, 10)
    manyBankrupt.fgspSpending.toDouble should be > noBankrupt.fgspSpending.toDouble
  }

  it should "produce gov subvention when funds in deficit" in {
    // Large unemployment benefit spend → FP deficit → subvention
    val result = EarmarkedFunds.step(EarmarkedFunds.State.zero, employed, wage, PLN(50e9), 100, 20)
    result.totalGovSubvention.toDouble should be > 0.0
  }

  it should "accumulate balances across months" in {
    val m1 = EarmarkedFunds.step(EarmarkedFunds.State.zero, employed, wage, PLN.Zero, 0, 0)
    val m2 = EarmarkedFunds.step(m1, employed, wage, PLN.Zero, 0, 0)
    // FP balance should grow (contributions > ALMP with zero unemp benefits)
    m2.fpBalance.toDouble should be > m1.fpBalance.toDouble
  }
