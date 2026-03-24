package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.steps.{FiscalConstraintStep, LaborDemographicsStep}
import com.boombustgroup.amorfati.init.WorldInit
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Proves LaborEconomics.compute() produces identical results to old
  * LaborDemographicsStep.run().
  *
  * The Calculus (economic logic) must match bit-for-bit. The Accounting
  * (ZUS/NFZ/PPK/Earmarked) is already migrated to flow mechanisms — we don't
  * compare those here.
  */
class LaborEconomicsSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  private val initResult = WorldInit.initialize(42L)
  private val world      = initResult.world
  private val firms      = initResult.firms

  private val s1 = FiscalConstraintStep.Output(
    m = 1,
    lendingBaseRate = world.nbp.referenceRate,
    resWage = world.hhAgg.reservationWage,
    baseMinWage = world.gov.minWageLevel,
    updatedMinWagePriceLevel = world.priceLevel,
  )

  "LaborEconomics.compute" should "match old LaborDemographicsStep wage" in {
    val oldResult = LaborDemographicsStep.run(LaborDemographicsStep.Input(world, firms, world.households, s1))
    val newResult = LaborEconomics.compute(world, firms, world.households, s1)

    newResult.wage shouldBe oldResult.newWage
  }

  it should "match old employment count" in {
    val oldResult = LaborDemographicsStep.run(LaborDemographicsStep.Input(world, firms, world.households, s1))
    val newResult = LaborEconomics.compute(world, firms, world.households, s1)

    newResult.employed shouldBe oldResult.employed
  }

  it should "match old demographics" in {
    val oldResult = LaborDemographicsStep.run(LaborDemographicsStep.Input(world, firms, world.households, s1))
    val newResult = LaborEconomics.compute(world, firms, world.households, s1)

    newResult.demographics.retirees shouldBe oldResult.newDemographics.retirees
    newResult.demographics.workingAgePop shouldBe oldResult.newDemographics.workingAgePop
  }

  it should "match old wage growth" in {
    val oldResult = LaborDemographicsStep.run(LaborDemographicsStep.Input(world, firms, world.households, s1))
    val newResult = LaborEconomics.compute(world, firms, world.households, s1)

    newResult.wageGrowth shouldBe oldResult.wageGrowth
  }
