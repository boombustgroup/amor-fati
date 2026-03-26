package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.flows.FlowSimulation
import com.boombustgroup.amorfati.init.WorldInit
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class WorldAssemblyEconomicsSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  "WorldAssemblyEconomics (own Input)" should "produce valid world after simulation step" in {
    val init   = WorldInit.initialize(42L)
    val rng    = new scala.util.Random(42)
    val result = FlowSimulation.step(init.world, init.firms, init.households, rng)
    val w      = result.newWorld

    w.month.shouldBe(1)
    w.totalPopulation should be > 0
    w.hhAgg.employed should be > 0
    w.external.tourismSeasonalFactor should not be 0.0
  }
