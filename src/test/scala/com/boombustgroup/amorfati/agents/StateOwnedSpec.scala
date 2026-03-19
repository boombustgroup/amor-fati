package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StateOwnedSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  "dividendMultiplier" should "be above 1.0 at baseline" in {
    StateOwned.dividendMultiplier(Ratio(0.02)).toDouble should be >= 1.0
  }

  it should "increase with higher deficit/GDP" in {
    val low  = StateOwned.dividendMultiplier(Ratio(0.02))
    val high = StateOwned.dividendMultiplier(Ratio(0.06))
    high should be > low
  }

  "firingReduction" should "be less than 1.0 (SOEs fire less)" in {
    StateOwned.firingReduction.toDouble should be < 1.0
    StateOwned.firingReduction.toDouble should be > 0.0
  }

  "investmentMultiplier" should "be above 1.0 (SOEs invest more)" in {
    StateOwned.investmentMultiplier should be > 1.0
  }

  "energyPassthrough" should "be less than 1.0 (SOEs absorb shocks)" in {
    StateOwned.energyPassthrough.toDouble should be < 1.0
    StateOwned.energyPassthrough.toDouble should be > 0.0
  }

  "sectorSoeShare" should "be highest in Public sector" in {
    StateOwned.sectorSoeShare(4).toDouble should be > 0.5
  }

  it should "be low in BPO" in {
    StateOwned.sectorSoeShare(0).toDouble should be < 0.10
  }

  it should "be in [0, 1] for all sectors" in {
    for s <- 0 until 6 do
      StateOwned.sectorSoeShare(s).toDouble should be >= 0.0
      StateOwned.sectorSoeShare(s).toDouble should be <= 1.0
  }
