package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.Simulation
import com.boombustgroup.amorfati.init.WorldInit
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Multi-seed validation: FlowSimulation.step() across 10 seeds × 12 months.
  *
  * Acceptance test for the new pipeline. Proves SFC == 0L and economic sanity
  * across diverse random trajectories.
  */
class MultiSeedValidationSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  private val seeds  = (1L to 10L).toVector
  private val months = 12

  "FlowSimulation.step (multi-seed)" should "produce SFC == 0L for all seeds × months" in
    seeds.foreach { seed =>
      val init  = WorldInit.initialize(seed)
      var state = Simulation.SimState(init.world, init.firms, init.households)

      (1 to months).foreach { month =>
        state = Simulation.step(state, seed, month).state
        val rng    = new scala.util.Random(seed * 1000 + month)
        val result = FlowSimulation.step(state.world, state.firms, state.households, rng)
        val wealth = Interpreter.totalWealth(Interpreter.applyAll(Map.empty[Int, Long], result.flows))
        withClue(s"Seed $seed, month $month: ") {
          wealth.shouldBe(0L)
        }
      }
    }

  it should "produce realistic employment (3-97%) for all seeds at month 12" in
    seeds.foreach { seed =>
      val init  = WorldInit.initialize(seed)
      var state = Simulation.SimState(init.world, init.firms, init.households)
      (1 to months).foreach(m => state = Simulation.step(state, seed, m).state)

      val unemp = state.world.hhAgg.unemploymentRate(state.world.totalPopulation)
      withClue(s"Seed $seed unemployment=$unemp: ") {
        unemp should be >= 0.03
        unemp should be <= 0.97
      }
    }

  it should "produce positive GDP for all seeds" in
    seeds.foreach { seed =>
      val init  = WorldInit.initialize(seed)
      var state = Simulation.SimState(init.world, init.firms, init.households)
      (1 to months).foreach(m => state = Simulation.step(state, seed, m).state)

      withClue(s"Seed $seed GDP: ") {
        state.world.gdpProxy should be > 0.0
      }
    }

  it should "emit 30+ mechanism IDs for all seeds" in
    seeds.foreach { seed =>
      val init   = WorldInit.initialize(seed)
      val state  = Simulation.step(Simulation.SimState(init.world, init.firms, init.households), seed, 1).state
      val rng    = new scala.util.Random(seed)
      val result = FlowSimulation.step(state.world, state.firms, state.households, rng)

      withClue(s"Seed $seed mechanisms: ") {
        result.flows.map(_.mechanism).toSet.size should be > 30
      }
    }
