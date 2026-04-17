package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.{CompletedMonth, ExecutionMonth}
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FiscalConstraintEconomicsSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  private val init  = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
  private val world = init.world

  "FiscalConstraintEconomics.compute" should "produce fiscal constraint output" in {
    val output = FiscalConstraintEconomics.compute(world, init.banks, ExecutionMonth.First)

    output.m shouldBe ExecutionMonth.First
    output.baseMinWage should be > PLN.Zero
    output.resWage shouldBe output.baseMinWage
    output.lendingBaseRate should be >= Rate.Zero
    output.updatedMinWagePriceLevel shouldBe world.gov.minWagePriceLevel
  }

  it should "advance month by 1" in {
    val completedMonth = CompletedMonth.Zero
    val result         = FiscalConstraintEconomics.compute(world, init.banks, completedMonth.next)
    result.month shouldBe completedMonth.next
  }
