package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.tags.Heavy
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Multi-month flow verification: FlowSimulation.step() drives state
  * autonomously.
  *
  * Runs 120 months via FlowSimulation.step(). At each month verifies SFC == 0L.
  */
@Heavy
class MultiMonthFlowSpec extends AnyFlatSpec with Matchers:
  import RuntimeFlowsTestSupport.*

  private given p: SimParams = SimParams.defaults

  "Multi-month flow verification (120 months)" should "preserve SFC at 0L every month" in {
    var state = stateFromSeed()

    (1 to 120).foreach { month =>
      val result = stepWithSeed(state, 42L * 1000 + month)

      withClue(s"SFC violated at month $month: ") {
        result.execution.netDelta shouldBe 0L
      }

      state = result.nextState
    }
  }

  it should "produce increasing total flow volume over time" in {
    var state   = stateFromSeed()
    var volumes = Vector.empty[PLN]

    (1 to 120).foreach { month =>
      val result = stepWithSeed(state, 42L * 1000 + month)
      volumes = volumes :+ totalTransferred(result.flows)

      state = result.nextState
    }

    volumes.last should be > volumes.head
  }
