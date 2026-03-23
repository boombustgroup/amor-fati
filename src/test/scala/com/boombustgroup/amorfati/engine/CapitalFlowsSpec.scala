package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.markets.CapitalFlows
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CapitalFlowsSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  private val td    = ComputationBoundary
  private val gdp   = PLN(100e9)
  private val carry = CapitalFlows.CarryState.zero

  "CapitalFlows.compute" should "produce zero adjustment when no shock and spread below threshold" in {
    val result = CapitalFlows.compute(1, Rate(0.02), Multiplier(2.0), carry, gdp)
    td.toDouble(result.totalAdjustment) shouldBe 0.0 +- 1.0
  }

  it should "accumulate carry trade when spread exceeds threshold" in {
    val highSpread = Rate(0.08) // well above carryThreshold (3%)
    val result     = CapitalFlows.compute(1, highSpread, Multiplier(2.0), carry, gdp)
    td.toDouble(result.newCarryState.stock) should be > 0.0
    td.toDouble(result.carryTradeFlow) should be > 0.0
  }

  it should "not accumulate carry when spread below threshold" in {
    val lowSpread = Rate(0.02) // below carryThreshold (3%)
    val result    = CapitalFlows.compute(1, lowSpread, Multiplier(2.0), carry, gdp)
    result.newCarryState.stock shouldBe PLN.Zero
  }

  it should "trigger auction outflow when bid-to-cover is low" in {
    val lowBtc = Multiplier(0.80) // below auctionConfidenceThreshold (0.90)
    val result = CapitalFlows.compute(1, Rate(0.05), lowBtc, carry, gdp)
    td.toDouble(result.auctionSignal) should be < 0.0
  }

  it should "not trigger auction outflow when bid-to-cover is healthy" in {
    val highBtc = Multiplier(1.50)
    val result  = CapitalFlows.compute(1, Rate(0.05), highBtc, carry, gdp)
    result.auctionSignal shouldBe PLN.Zero
  }

  it should "preserve carry stock when no risk-off" in {
    val withCarry = CapitalFlows.CarryState(PLN(10e9))
    val result    = CapitalFlows.compute(1, Rate(0.05), Multiplier(2.0), withCarry, gdp)
    // Stock should grow (accumulation) or stay (no unwind without risk-off)
    td.toDouble(result.newCarryState.stock) should be >= 10e9
  }
