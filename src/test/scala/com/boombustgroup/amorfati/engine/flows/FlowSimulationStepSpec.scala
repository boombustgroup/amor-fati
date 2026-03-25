package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.Simulation
import com.boombustgroup.amorfati.init.WorldInit
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FlowSimulationStepSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  "FlowSimulation.step" should "produce SFC == 0L on real World" in {
    val init   = WorldInit.initialize(42L)
    val rng    = new scala.util.Random(42)
    val result = FlowSimulation.step(init.world, init.firms, init.households, rng)
    Interpreter.totalWealth(Interpreter.applyAll(Map.empty[Int, Long], result.flows)).shouldBe(0L)
    result.calculus.employed should be > 0
  }

  it should "produce SFC == 0L across 12 months (old pipeline drives state)" in {
    val init  = WorldInit.initialize(42L)
    var state = Simulation.SimState(init.world, init.firms, init.households)

    (1 to 12).foreach { month =>
      state = Simulation.step(state, 42L, month).state
      val rng    = new scala.util.Random(42L + month)
      val result = FlowSimulation.step(state.world, state.firms, state.households, rng)
      withClue(s"Month $month: ") {
        Interpreter.totalWealth(Interpreter.applyAll(Map.empty[Int, Long], result.flows)).shouldBe(0L)
      }
    }
  }

  it should "produce 30+ mechanism IDs" in {
    val init   = WorldInit.initialize(42L)
    val rng    = new scala.util.Random(42)
    val result = FlowSimulation.step(init.world, init.firms, init.households, rng)
    result.flows.map(_.mechanism).toSet.size should be > 30
  }
