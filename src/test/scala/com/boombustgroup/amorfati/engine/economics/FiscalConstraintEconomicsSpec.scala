package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.steps.FiscalConstraintStep
import com.boombustgroup.amorfati.init.WorldInit
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FiscalConstraintEconomicsSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  private val world = WorldInit.initialize(42L).world

  "FiscalConstraintEconomics.compute" should "match old FiscalConstraintStep" in {
    val oldResult = FiscalConstraintStep.run(FiscalConstraintStep.Input(world))
    val newResult = FiscalConstraintEconomics.compute(world)

    newResult.month shouldBe oldResult.m
    newResult.baseMinWage shouldBe oldResult.baseMinWage
    newResult.resWage shouldBe oldResult.resWage
    newResult.lendingBaseRate shouldBe oldResult.lendingBaseRate
    newResult.updatedMinWagePriceLevel shouldBe oldResult.updatedMinWagePriceLevel
  }
