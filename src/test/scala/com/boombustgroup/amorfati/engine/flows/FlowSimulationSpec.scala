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

  "FlowSimulation.emitAllBatches" should "match the step batch plan for computed calculus" in {
    val state                   = stateFromSeed()
    val result                  = stepWithSeed(state)
    given RuntimeLedgerTopology = result.execution.topology
    val flows                   = FlowSimulation.emitAllBatches(result.calculus)

    canonicalBatches(flows) shouldBe canonicalBatches(result.flows)
    result.execution.netDelta shouldBe 0L
    totalTransferred(flows) should be > PLN.Zero
  }
