package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StateOwnedSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  "dividendMultiplier" should "be above 1.0 at baseline" in {
    decimal(StateOwned.dividendMultiplier(Share.decimal(2, 2))) should be >= BigDecimal("1.0")
  }

  it should "increase with higher deficit/GDP" in {
    val low  = StateOwned.dividendMultiplier(Share.decimal(2, 2))
    val high = StateOwned.dividendMultiplier(Share.decimal(6, 2))
    high should be > low
  }

  "firingReduction" should "be less than 1.0 (SOEs fire less)" in {
    decimal(StateOwned.firingReduction) should be < BigDecimal("1.0")
    decimal(StateOwned.firingReduction) should be > BigDecimal("0.0")
  }

  "investmentMultiplier" should "be above 1.0 (SOEs invest more)" in {
    StateOwned.investmentMultiplier should be > Multiplier.One
  }

  "energyPassthrough" should "be less than 1.0 (SOEs absorb shocks)" in {
    decimal(StateOwned.energyPassthrough) should be < BigDecimal("1.0")
    decimal(StateOwned.energyPassthrough) should be > BigDecimal("0.0")
  }

  "sectorSoeShare" should "be highest in Public sector" in {
    decimal(StateOwned.sectorSoeShare(4)) should be > BigDecimal("0.5")
  }

  it should "be low in BPO" in {
    decimal(StateOwned.sectorSoeShare(0)) should be < BigDecimal("0.10")
  }

  it should "be in [0, 1] for all sectors" in {
    for s <- 0 until 6 do
      decimal(StateOwned.sectorSoeShare(s)) should be >= BigDecimal("0.0")
      decimal(StateOwned.sectorSoeShare(s)) should be <= BigDecimal("1.0")
  }
