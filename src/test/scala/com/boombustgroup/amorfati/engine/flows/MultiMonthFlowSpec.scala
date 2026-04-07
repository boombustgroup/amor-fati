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
    var w     = init.world
    var firms = init.firms
    var hh    = init.households
    var banks = init.banks

    (1 to 120).foreach { month =>
      val rng    = new scala.util.Random(42L * 1000 + month)
      val result = FlowSimulation.step(w, firms, hh, banks, rng)

      withClue(s"SFC violated at month $month: ") {
        result.execution.totalWealth shouldBe 0L
      }

      w = result.newWorld
      firms = result.newFirms
      hh = result.newHouseholds
      banks = result.newBanks
    }
  }

  it should "produce increasing total flow volume over time" in {
    val init    = WorldInit.initialize(42L)
    var w       = init.world
    var firms   = init.firms
    var hh      = init.households
    var banks   = init.banks
    var volumes = Vector.empty[Long]

    (1 to 120).foreach { month =>
      val rng    = new scala.util.Random(42L * 1000 + month)
      val result = FlowSimulation.step(w, firms, hh, banks, rng)
      volumes = volumes :+ AggregateBatchContract.totalTransferred(result.flows)

      w = result.newWorld
      firms = result.newFirms
      hh = result.newHouseholds
      banks = result.newBanks
    }

    volumes.last should be > volumes.head
  }
