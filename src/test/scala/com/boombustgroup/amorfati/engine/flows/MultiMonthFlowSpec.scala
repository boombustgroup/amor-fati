package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.init.WorldInit
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Multi-month flow verification: FlowSimulation.step() drives state
  * autonomously.
  *
  * Runs 120 months via FlowSimulation.step(). At each month verifies SFC == 0L.
  */
class MultiMonthFlowSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  "Multi-month flow verification (120 months)" should "preserve SFC at 0L every month" in {
    val init  = WorldInit.initialize(42L)
    var w     = init.world
    var firms = init.firms
    var hh    = init.households

    (1 to 120).foreach { month =>
      val rng    = new scala.util.Random(42L * 1000 + month)
      val result = FlowSimulation.step(w, firms, hh, rng)
      val wealth = Interpreter.totalWealth(Interpreter.applyAll(Map.empty[Int, Long], result.flows))

      withClue(s"SFC violated at month $month: ") {
        wealth shouldBe 0L
      }

      w = result.newWorld
      firms = result.newFirms
      hh = result.newHouseholds
    }
  }

  it should "produce increasing total flow volume over time" in {
    val init    = WorldInit.initialize(42L)
    var w       = init.world
    var firms   = init.firms
    var hh      = init.households
    var volumes = Vector.empty[Long]

    (1 to 120).foreach { month =>
      val rng    = new scala.util.Random(42L * 1000 + month)
      val result = FlowSimulation.step(w, firms, hh, rng)
      volumes = volumes :+ result.flows.map(_.amount).sum

      w = result.newWorld
      firms = result.newFirms
      hh = result.newHouseholds
    }

    volumes.last should be > volumes.head
  }
