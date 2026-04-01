package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.init.WorldInit
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Autonomous pipeline: FlowSimulation.step() drives its own state.
  *
  * No old Simulation.step() in the loop. FlowSimulation produces new World,
  * which feeds into next month's FlowSimulation.step().
  */
class AutonomousPipelineSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  "FlowSimulation (autonomous)" should "run 12 months with SFC == 0L" in {
    val init  = WorldInit.initialize(42L)
    var w     = init.world
    var firms = init.firms
    var hh    = init.households
    var banks = init.banks

    (1 to 12).foreach { month =>
      val rng    = new scala.util.Random(42L * 1000 + month)
      val result = FlowSimulation.step(w, firms, hh, banks, rng)
      val wealth = Interpreter.totalWealth(Interpreter.applyAll(Map.empty[Int, Long], result.flows))

      withClue(s"Month $month: ") {
        wealth.shouldBe(0L)
      }

      w = result.newWorld
      firms = result.newFirms
      hh = result.newHouseholds
      banks = result.newBanks
    }
  }

  it should "produce evolving economy (GDP changes)" in {
    val init  = WorldInit.initialize(42L)
    var w     = init.world
    var firms = init.firms
    var hh    = init.households
    var banks = init.banks
    val gdps  = scala.collection.mutable.ArrayBuffer[Double]()

    (1 to 12).foreach { month =>
      val rng    = new scala.util.Random(42L * 1000 + month)
      val result = FlowSimulation.step(w, firms, hh, banks, rng)
      w = result.newWorld
      firms = result.newFirms
      hh = result.newHouseholds
      banks = result.newBanks
      gdps += w.gdpProxy
    }

    gdps.last should not be gdps.head
    gdps.forall(_ > 0) shouldBe true
  }

  it should "maintain positive employment throughout" in {
    val init  = WorldInit.initialize(42L)
    var w     = init.world
    var firms = init.firms
    var hh    = init.households
    var banks = init.banks

    (1 to 12).foreach { month =>
      val rng    = new scala.util.Random(42L * 1000 + month)
      val result = FlowSimulation.step(w, firms, hh, banks, rng)
      w = result.newWorld
      firms = result.newFirms
      hh = result.newHouseholds
      banks = result.newBanks

      withClue(s"Month $month: ") {
        w.hhAgg.employed should be > 0
      }
    }
  }
