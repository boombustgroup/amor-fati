package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.init.WorldInit
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Tests LaborEconomics.compute() produces reasonable results. */
class LaborEconomicsSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  private val initResult = WorldInit.initialize(42L)
  private val world      = initResult.world
  private val firms      = initResult.firms

  private val s1 = FiscalConstraintEconomics.Output(
    m = 1,
    lendingBaseRate = world.nbp.referenceRate,
    resWage = world.hhAgg.reservationWage,
    baseMinWage = world.gov.minWageLevel,
    updatedMinWagePriceLevel = world.priceLevel,
  )

  "LaborEconomics.compute" should "produce positive wage" in {
    val result = LaborEconomics.compute(world, firms, world.households, s1)
    ComputationBoundary.toDouble(result.wage) should be > 0.0
  }

  it should "produce positive employment" in {
    val result = LaborEconomics.compute(world, firms, world.households, s1)
    result.employed should be > 0
  }

  it should "produce demographics with positive working-age pop" in {
    val result = LaborEconomics.compute(world, firms, world.households, s1)
    result.demographics.workingAgePop should be > 0
  }

  it should "produce consistent wage growth" in {
    val result = LaborEconomics.compute(world, firms, world.households, s1)
    // Wage growth should be a finite number
    ComputationBoundary.toDouble(result.wageGrowth).isNaN shouldBe false
  }
