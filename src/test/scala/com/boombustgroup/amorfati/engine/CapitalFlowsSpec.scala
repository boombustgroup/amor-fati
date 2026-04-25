package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.markets.CapitalFlows
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CapitalFlowsSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  private val gdp    = PLN(100000000000L)
  private val carry  = CapitalFlows.CarryState.zero
  private val Month1 = ExecutionMonth.First

  "CapitalFlows.compute" should "produce zero adjustment when no shock and spread below threshold" in {
    val result = CapitalFlows.compute(Month1, Rate.decimal(2, 2), Multiplier(2), carry, gdp)
    decimal(result.totalAdjustment) shouldBe BigDecimal("0.0") +- BigDecimal("1.0")
  }

  it should "accumulate carry trade when spread exceeds threshold" in {
    val highSpread = Rate.decimal(8, 2) // well above carryThreshold (3%)
    val result     = CapitalFlows.compute(Month1, highSpread, Multiplier(2), carry, gdp)
    decimal(result.newCarryState.stock) should be > BigDecimal("0.0")
    decimal(result.carryTradeFlow) should be > BigDecimal("0.0")
  }

  it should "not accumulate carry when spread below threshold" in {
    val lowSpread = Rate.decimal(2, 2) // below carryThreshold (3%)
    val result    = CapitalFlows.compute(Month1, lowSpread, Multiplier(2), carry, gdp)
    result.newCarryState.stock shouldBe PLN.Zero
  }

  it should "trigger auction outflow when bid-to-cover is low" in {
    val lowBtc = Multiplier.decimal(80, 2) // below auctionConfidenceThreshold (0.90)
    val result = CapitalFlows.compute(Month1, Rate.decimal(5, 2), lowBtc, carry, gdp)
    decimal(result.auctionSignal) should be < BigDecimal("0.0")
  }

  it should "not trigger auction outflow when bid-to-cover is healthy" in {
    val highBtc = Multiplier.decimal(150, 2)
    val result  = CapitalFlows.compute(Month1, Rate.decimal(5, 2), highBtc, carry, gdp)
    result.auctionSignal shouldBe PLN.Zero
  }

  it should "preserve carry stock when no risk-off" in {
    val withCarry = CapitalFlows.CarryState(PLN(10000000000L))
    val result    = CapitalFlows.compute(Month1, Rate.decimal(5, 2), Multiplier(2), withCarry, gdp)
    // Stock should grow (accumulation) or stay (no unwind without risk-off)
    decimal(result.newCarryState.stock) should be >= BigDecimal("10e9")
  }
