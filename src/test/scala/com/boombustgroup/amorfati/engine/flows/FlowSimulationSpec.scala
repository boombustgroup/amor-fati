package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.amorfati.tags.Heavy
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Tests FlowSimulation.emitAllBatches with MonthlyCalculus from
  * FlowSimulation.computeCalculus.
  *
  * Proves that the Contract-First pipeline design (MonthlyCalculus →
  * emitAllBatches → runtime topology → Interpreter) closes at SFC == 0L when
  * fed with real simulation data.
  */
@Heavy
class FlowSimulationSpec extends AnyFlatSpec with Matchers:
  import RuntimeFlowsTestSupport.*

  private given p: SimParams = SimParams.defaults

  "FlowSimulation.emitAllBatches" should "preserve SFC at 0L" in {
    val state                   = stateFromSeed()
    val result                  = stepWithSeed(state)
    given RuntimeLedgerTopology = result.execution.topology
    val flows                   = FlowSimulation.emitAllBatches(result.calculus)

    result.execution.netDelta shouldBe 0L
    totalTransferred(flows) should be > PLN.Zero
  }

  it should "preserve SFC across 12 months" in {
    var state = stateFromSeed()

    (1 to 12).foreach { month =>
      val result                  = stepWithSeed(state, 42L * 1000 + month)
      given RuntimeLedgerTopology = result.execution.topology
      val flows                   = FlowSimulation.emitAllBatches(result.calculus)

      withClue(s"Month $month: ") {
        result.execution.netDelta shouldBe 0L
        totalTransferred(flows) should be > PLN.Zero
      }

      state = result.nextState
    }
  }

  it should "emit 30+ mechanism IDs" in {
    val result                  = stepFromSeed()
    given RuntimeLedgerTopology = result.execution.topology
    val flows                   = FlowSimulation.emitAllBatches(result.calculus)

    result.execution.netDelta shouldBe 0L
    flows.map(_.mechanism).toSet.size should be > 30
  }
