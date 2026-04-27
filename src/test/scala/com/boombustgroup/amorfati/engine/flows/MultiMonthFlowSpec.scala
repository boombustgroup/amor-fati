package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.tags.Heavy
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Multi-month flow verification: FlowSimulation.step() drives state
  * autonomously across a long horizon.
  */
@Heavy
class MultiMonthFlowSpec extends AnyFlatSpec with Matchers:
  import RuntimeFlowsTestSupport.*

  private given p: SimParams = SimParams.defaults

  "Multi-month flow verification (120 months)" should "preserve SFC and basic dynamics every month" in {
    var state   = stateFromSeed()
    var gdps    = Vector.empty[PLN]
    var volumes = Vector.empty[PLN]

    (1 to 120).foreach { month =>
      val result = stepWithSeed(state, 42L * 1000 + month)

      withClue(s"SFC violated at month $month: ") {
        result.execution.netDelta shouldBe 0L
      }
      withClue(s"Employment violated at month $month: ") {
        result.nextState.householdAggregates.employed should be > 0
      }

      gdps = gdps :+ result.nextState.world.cachedMonthlyGdpProxy
      volumes = volumes :+ totalTransferred(result.flows)
      state = result.nextState
    }

    gdps.last should not be gdps.head
    gdps.forall(_ > PLN.Zero) shouldBe true
    volumes.last should be > volumes.head
  }
