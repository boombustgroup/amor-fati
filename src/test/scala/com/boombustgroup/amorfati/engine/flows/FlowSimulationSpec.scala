package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.init.WorldInit
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Tests FlowSimulation.emitAllFlows with MonthlyCalculus from
  * FlowSimulation.computeCalculus.
  *
  * Proves that the Contract-First pipeline design (MonthlyCalculus →
  * emitAllFlows → Interpreter) closes at SFC == 0L when fed with real
  * simulation data.
  */
class FlowSimulationSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  "FlowSimulation.emitAllFlows" should "preserve SFC at 0L" in {
    val init   = WorldInit.initialize(42L)
    val rng    = new scala.util.Random(42)
    val result = FlowSimulation.step(init.world, init.firms, init.households, init.banks, rng)
    val flows  = FlowSimulation.emitAllFlows(result.calculus)

    Interpreter.totalWealth(Interpreter.applyAll(Map.empty[Int, Long], flows)) shouldBe 0L
  }

  it should "preserve SFC across 12 months" in {
    val init  = WorldInit.initialize(42L)
    var w     = init.world
    var firms = init.firms
    var hh    = init.households
    var banks = init.banks

    (1 to 12).foreach { month =>
      val rng    = new scala.util.Random(42L * 1000 + month)
      val result = FlowSimulation.step(w, firms, hh, banks, rng)
      val flows  = FlowSimulation.emitAllFlows(result.calculus)

      withClue(s"Month $month: ") {
        Interpreter.totalWealth(Interpreter.applyAll(Map.empty[Int, Long], flows)) shouldBe 0L
      }

      w = result.newWorld
      firms = result.newFirms
      hh = result.newHouseholds
      banks = result.newBanks
    }
  }

  it should "emit 30+ mechanism IDs" in {
    val init   = WorldInit.initialize(42L)
    val rng    = new scala.util.Random(42)
    val result = FlowSimulation.step(init.world, init.firms, init.households, init.banks, rng)
    val flows  = FlowSimulation.emitAllFlows(result.calculus)

    flows.map(_.mechanism).toSet.size should be > 30
  }
