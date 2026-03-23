package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StateOwnedSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults
  private val td  = ComputationBoundary

  "dividendMultiplier" should "be above 1.0 at baseline" in {
    td.toDouble(StateOwned.dividendMultiplier(Share(0.02))) should be >= 1.0
  }

  it should "increase with higher deficit/GDP" in {
    val low  = StateOwned.dividendMultiplier(Share(0.02))
    val high = StateOwned.dividendMultiplier(Share(0.06))
    high should be > low
  }

  "firingReduction" should "be less than 1.0 (SOEs fire less)" in {
    td.toDouble(StateOwned.firingReduction) should be < 1.0
    td.toDouble(StateOwned.firingReduction) should be > 0.0
  }

  "investmentMultiplier" should "be above 1.0 (SOEs invest more)" in {
    StateOwned.investmentMultiplier should be > Multiplier.One
  }

  "energyPassthrough" should "be less than 1.0 (SOEs absorb shocks)" in {
    td.toDouble(StateOwned.energyPassthrough) should be < 1.0
    td.toDouble(StateOwned.energyPassthrough) should be > 0.0
  }

  "sectorSoeShare" should "be highest in Public sector" in {
    td.toDouble(StateOwned.sectorSoeShare(4)) should be > 0.5
  }

  it should "be low in BPO" in {
    td.toDouble(StateOwned.sectorSoeShare(0)) should be < 0.10
  }

  it should "be in [0, 1] for all sectors" in {
    for s <- 0 until 6 do
      td.toDouble(StateOwned.sectorSoeShare(s)) should be >= 0.0
      td.toDouble(StateOwned.sectorSoeShare(s)) should be <= 1.0
  }
