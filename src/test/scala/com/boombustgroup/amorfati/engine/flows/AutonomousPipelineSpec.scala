package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.tags.Heavy
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Autonomous pipeline: FlowSimulation.step() drives its own state.
  *
  * No old Simulation.step() in the loop. FlowSimulation produces nextState,
  * which feeds into next month's FlowSimulation.step().
  */
@Heavy
class AutonomousPipelineSpec extends AnyFlatSpec with Matchers:
  import RuntimeFlowsTestSupport.*

  private given p: SimParams = SimParams.defaults

  "FlowSimulation (autonomous)" should "run 12 months with SFC == 0L" in {
    var state = stateFromSeed()

    (1 to 12).foreach { month =>
      val result = stepWithSeed(state, 42L * 1000 + month)

      withClue(s"Month $month: ") {
        result.execution.netDelta.shouldBe(0L)
      }

      state = result.nextState
    }
  }

  it should "produce evolving economy (GDP changes)" in {
    var state = stateFromSeed()
    val gdps  = scala.collection.mutable.ArrayBuffer[BigDecimal]()

    (1 to 12).foreach { month =>
      val result = stepWithSeed(state, 42L * 1000 + month)
      state = result.nextState
      gdps += decimal(state.world.cachedMonthlyGdpProxy)
    }

    gdps.last should not be gdps.head
    gdps.forall(_ > 0) shouldBe true
  }

  it should "maintain positive employment throughout" in {
    var state = stateFromSeed()

    (1 to 12).foreach { month =>
      val result = stepWithSeed(state, 42L * 1000 + month)
      state = result.nextState

      withClue(s"Month $month: ") {
        result.nextState.householdAggregates.employed should be > 0
      }
    }
  }
