package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.economics.*
import com.boombustgroup.amorfati.init.WorldInit
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Flow Pipeline integration test: real World -> Economics -> Flows ->
  * Interpreter.
  */
class FlowPipelineSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults
  private val init           = WorldInit.initialize(42L)
  private val w              = init.world

  /** Run one month through the new flow pipeline (partial -- Steps 1-2
    * economics + all fund flows).
    */
  private def runOneMonth: (Map[Int, Long], LaborEconomics.Result) =
    // Step 1: Fiscal constraints (pure calculus)
    val fiscal = FiscalConstraintEconomics.compute(w)

    // Step 2: Labor economics (pure calculus)
    val s1    = FiscalConstraintEconomics.toOutput(fiscal)
    val labor = LaborEconomics.compute(w, init.firms, init.households, s1)

    // Emit flows from fund mechanisms using adapter
    val flows = Vector.concat(
      ZusFlows.emit(StateAdapter.zusInput(labor)),
      NfzFlows.emit(StateAdapter.nfzInput(labor)),
      PpkFlows.emit(StateAdapter.ppkInput(labor)),
      EarmarkedFlows.emit(StateAdapter.earmarkedInput(labor)),
      InsuranceFlows.emit(StateAdapter.insuranceInput(w, labor)),
    )

    val balances = Interpreter.applyAll(Map.empty[Int, Long], flows)
    (balances, labor)

  "Flow pipeline (real World)" should "preserve SFC at 0L" in {
    val (balances, _) = runOneMonth
    Interpreter.totalWealth(balances) shouldBe 0L
  }

  it should "produce realistic wage" in {
    val (_, labor) = runOneMonth
    val td         = ComputationBoundary
    td.toDouble(labor.wage) should be > 3000.0
    td.toDouble(labor.wage) should be < 20000.0
  }

  it should "produce realistic employment" in {
    val (_, labor) = runOneMonth
    labor.employed should be > 50000
    labor.employed should be < 120000
  }

  it should "emit non-trivial fund flows" in {
    val fiscal = FiscalConstraintEconomics.compute(w)
    val s1     = FiscalConstraintEconomics.toOutput(fiscal)
    val labor  = LaborEconomics.compute(w, init.firms, init.households, s1)

    val flows = Vector.concat(
      ZusFlows.emit(StateAdapter.zusInput(labor)),
      NfzFlows.emit(StateAdapter.nfzInput(labor)),
      PpkFlows.emit(StateAdapter.ppkInput(labor)),
      EarmarkedFlows.emit(StateAdapter.earmarkedInput(labor)),
    )

    flows.length should be > 5
    flows.map(_.amount).sum should be > 0L
  }
