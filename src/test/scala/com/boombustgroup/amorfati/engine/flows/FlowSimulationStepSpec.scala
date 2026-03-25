package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.Simulation
import com.boombustgroup.amorfati.init.WorldInit
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Tests FlowSimulation.step() — the new pipeline entry point.
  *
  * Economics chain → MonthlyCalculus → emitAllFlows → SFC == 0L. Old
  * Simulation.step() drives state evolution for multi-month tests.
  */
class FlowSimulationStepSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  "FlowSimulation.step" should "produce SFC == 0L on real World" in {
    val init          = WorldInit.initialize(42L)
    val rng           = new scala.util.Random(42)
    val (calc, flows) = FlowSimulation.step(init.world, init.firms, init.households, rng)
    Interpreter.totalWealth(Interpreter.applyAll(Map.empty[Int, Long], flows)).shouldBe(0L)
    calc.employed should be > 0
  }

  it should "produce SFC == 0L across 12 months (old pipeline drives state)" in {
    val init  = WorldInit.initialize(42L)
    var state = Simulation.SimState(init.world, init.firms, init.households)

    (1 to 12).foreach { month =>
      state = Simulation.step(state, 42L, month).state
      val rng        = new scala.util.Random(42L + month)
      val (_, flows) = FlowSimulation.step(state.world, state.firms, state.households, rng)
      val balances   = Interpreter.applyAll(Map.empty[Int, Long], flows)
      withClue(s"Month $month: ") {
        Interpreter.totalWealth(balances).shouldBe(0L)
      }
    }
  }

  it should "produce 30+ mechanism IDs" in {
    val init       = WorldInit.initialize(42L)
    val rng        = new scala.util.Random(42)
    val (_, flows) = FlowSimulation.step(init.world, init.firms, init.households, rng)
    flows.map(_.mechanism).toSet.size should be > 30
  }
