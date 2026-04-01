package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.init.WorldInit
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FiscalConstraintEconomicsSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  private val init  = WorldInit.initialize(42L)
  private val world = init.world

  "FiscalConstraintEconomics.compute" should "produce consistent result and output" in {
    val result = FiscalConstraintEconomics.compute(world, init.banks)
    val output = FiscalConstraintEconomics.toOutput(result)

    output.m shouldBe result.month
    output.baseMinWage shouldBe result.baseMinWage
    output.resWage shouldBe result.resWage
    output.lendingBaseRate shouldBe result.lendingBaseRate
    output.updatedMinWagePriceLevel shouldBe result.updatedMinWagePriceLevel
  }

  it should "advance month by 1" in {
    val result = FiscalConstraintEconomics.compute(world, init.banks)
    result.month shouldBe world.month + 1
  }
