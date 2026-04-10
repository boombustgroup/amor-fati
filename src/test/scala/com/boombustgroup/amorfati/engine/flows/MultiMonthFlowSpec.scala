package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.init.WorldInit
import com.boombustgroup.amorfati.tags.Heavy
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Multi-month flow verification: FlowSimulation.step() drives state
  * autonomously.
  *
  * Runs 120 months via FlowSimulation.step(). At each month verifies SFC == 0L.
  */
@Heavy
class MultiMonthFlowSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  "Multi-month flow verification (120 months)" should "preserve SFC at 0L every month" in {
    val init  = WorldInit.initialize(42L)
    var state = FlowSimulation.SimState.fromInit(init)

    (1 to 120).foreach { month =>
      val rng    = new scala.util.Random(42L * 1000 + month)
      val result = FlowSimulation.step(state, rng)

      withClue(s"SFC violated at month $month: ") {
        result.execution.totalWealth shouldBe 0L
      }

      state = result.nextState
    }
  }

  it should "produce increasing total flow volume over time" in {
    val init    = WorldInit.initialize(42L)
    var state   = FlowSimulation.SimState.fromInit(init)
    var volumes = Vector.empty[Long]

    (1 to 120).foreach { month =>
      val rng    = new scala.util.Random(42L * 1000 + month)
      val result = FlowSimulation.step(state, rng)
      volumes = volumes :+ AggregateBatchContract.totalTransferred(result.flows)

      state = result.nextState
    }

    volumes.last should be > volumes.head
  }
