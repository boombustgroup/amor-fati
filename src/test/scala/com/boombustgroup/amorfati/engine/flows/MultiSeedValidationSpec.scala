package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.init.WorldInit
import com.boombustgroup.ledger.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Multi-seed validation: FlowSimulation.step() across 10 seeds × 12 months.
  *
  * Acceptance test for the pipeline. Proves SFC == 0L and economic sanity
  * across diverse random trajectories.
  */
class MultiSeedValidationSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  private val seeds  = (1L to 10L).toVector
  private val months = 12

  "FlowSimulation.step (multi-seed)" should "produce SFC == 0L for all seeds × months" in
    seeds.foreach { seed =>
      val init  = WorldInit.initialize(seed)
      var w     = init.world
      var firms = init.firms
      var hh    = init.households
      var banks = init.banks

      (1 to months).foreach { month =>
        val rng    = new scala.util.Random(seed * 1000 + month)
        val result = FlowSimulation.step(w, firms, hh, banks, rng)
        val wealth = Interpreter.totalWealth(Interpreter.applyAll(Map.empty[Int, Long], AggregateBatchContract.toLegacyFlows(result.flows)))
        withClue(s"Seed $seed, month $month: ") {
          wealth.shouldBe(0L)
        }
        w = result.newWorld
        firms = result.newFirms
        hh = result.newHouseholds
        banks = result.newBanks
      }
    }

  it should "produce realistic employment (3-97%) for all seeds at month 12" in
    seeds.foreach { seed =>
      val init  = WorldInit.initialize(seed)
      var w     = init.world
      var firms = init.firms
      var hh    = init.households
      var banks = init.banks
      (1 to months).foreach { m =>
        val rng    = new scala.util.Random(seed * 1000 + m)
        val result = FlowSimulation.step(w, firms, hh, banks, rng)
        w = result.newWorld
        firms = result.newFirms
        hh = result.newHouseholds
        banks = result.newBanks
      }

      val employed = hh.count(_.status.isInstanceOf[com.boombustgroup.amorfati.agents.HhStatus.Employed])
      val unemp    = if w.derivedTotalPopulation > 0 then 1.0 - employed.toDouble / w.derivedTotalPopulation else 0.0
      withClue(s"Seed $seed unemployment=$unemp: ") {
        unemp should be >= 0.03
        unemp should be <= 0.97
      }
    }

  it should "produce positive GDP for all seeds" in
    seeds.foreach { seed =>
      val init  = WorldInit.initialize(seed)
      var w     = init.world
      var firms = init.firms
      var hh    = init.households
      var banks = init.banks
      (1 to months).foreach { m =>
        val rng    = new scala.util.Random(seed * 1000 + m)
        val result = FlowSimulation.step(w, firms, hh, banks, rng)
        w = result.newWorld
        firms = result.newFirms
        hh = result.newHouseholds
        banks = result.newBanks
      }

      withClue(s"Seed $seed GDP: ") {
        w.cachedMonthlyGdpProxy should be > 0.0
      }
    }

  it should "emit 30+ mechanism IDs for all seeds" in
    seeds.foreach { seed =>
      val init   = WorldInit.initialize(seed)
      val rng    = new scala.util.Random(seed)
      val result = FlowSimulation.step(init.world, init.firms, init.households, init.banks, rng)

      withClue(s"Seed $seed mechanisms: ") {
        result.flows.map(_.mechanism).toSet.size should be > 30
      }
    }
