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
    var w     = init.world
    var firms = init.firms
    var hh    = init.households
    var banks = init.banks

    (1 to 12).foreach { month =>
      val rng    = new scala.util.Random(42L * 1000 + month)
      val result = FlowSimulation.step(w, firms, hh, banks, rng)

      withClue(s"Month $month: ") {
        result.execution.totalWealth.shouldBe(0L)
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
      gdps += td.toDouble(w.cachedMonthlyGdpProxy)
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
        result.householdAggregates.employed should be > 0
      }
    }
  }
