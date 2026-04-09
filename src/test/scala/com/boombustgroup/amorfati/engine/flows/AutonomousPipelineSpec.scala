package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.init.WorldInit
import com.boombustgroup.amorfati.fp.ComputationBoundary
import com.boombustgroup.amorfati.tags.Heavy
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Autonomous pipeline: FlowSimulation.step() drives its own state.
  *
  * No old Simulation.step() in the loop. FlowSimulation produces new World,
  * which feeds into next month's FlowSimulation.step().
  */
@Heavy
class AutonomousPipelineSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults
  private val td             = ComputationBoundary

  "FlowSimulation (autonomous)" should "run 12 months with SFC == 0L" in {
    val init  = WorldInit.initialize(42L)
    var state = FlowSimulation.SimState(init.world, init.firms, init.households, init.banks, init.householdAggregates)

    (1 to 12).foreach { month =>
      val rng    = new scala.util.Random(42L * 1000 + month)
      val result = FlowSimulation.step(state, rng)

      withClue(s"Month $month: ") {
        result.execution.totalWealth.shouldBe(0L)
      }

      state = result.nextState
    }
  }

  it should "produce evolving economy (GDP changes)" in {
    val init  = WorldInit.initialize(42L)
    var state = FlowSimulation.SimState(init.world, init.firms, init.households, init.banks, init.householdAggregates)
    val gdps  = scala.collection.mutable.ArrayBuffer[Double]()

    (1 to 12).foreach { month =>
      val rng    = new scala.util.Random(42L * 1000 + month)
      val result = FlowSimulation.step(state, rng)
      state = result.nextState
      gdps += td.toDouble(state.world.cachedMonthlyGdpProxy)
    }

    gdps.last should not be gdps.head
    gdps.forall(_ > 0) shouldBe true
  }

  it should "maintain positive employment throughout" in {
    val init  = WorldInit.initialize(42L)
    var state = FlowSimulation.SimState(init.world, init.firms, init.households, init.banks, init.householdAggregates)

    (1 to 12).foreach { month =>
      val rng    = new scala.util.Random(42L * 1000 + month)
      val result = FlowSimulation.step(state, rng)
      state = result.nextState

      withClue(s"Month $month: ") {
        result.householdAggregates.employed should be > 0
      }
    }
  }
