package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Tests LaborEconomics.compute() produces reasonable results. */
class LaborEconomicsSpec extends AnyFlatSpec with Matchers:

  private given p: SimParams = SimParams.defaults

  private val initResult = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
  private val world      = initResult.world
  private val firms      = initResult.firms
  private val households = initResult.households

  private val s1 = FiscalConstraintEconomics.Output(
    month = ExecutionMonth.First,
    lendingBaseRate = world.nbp.referenceRate,
    resWage = world.householdMarket.reservationWage,
    baseMinWage = world.gov.minWageLevel,
    updatedMinWagePriceLevel = world.priceLevel,
  )

  "LaborEconomics.compute" should "produce positive wage" in {
    val result = LaborEconomics.compute(world, firms, households, s1)
    decimal(result.newWage) should be > BigDecimal("0.0")
  }

  it should "produce positive employment" in {
    val result = LaborEconomics.compute(world, firms, households, s1)
    result.employed should be > 0
  }

  it should "produce demographics with positive working-age pop" in {
    val result = LaborEconomics.compute(world, firms, households, s1)
    result.newDemographics.workingAgePop should be > 0
  }

  it should "produce consistent wage growth" in {
    val result = LaborEconomics.compute(world, firms, households, s1)
    // Wage growth should be a finite number
    decimal(result.wageGrowth).isNaN shouldBe false
  }

  it should "compress aggregate hiring plans when labor demand exceeds available labor" in {
    LaborEconomics.operationalHiringSlackFactor(laborDemand = 120000, availableLabor = 80000) should be < Share.One
  }

  it should "leave hiring plans unchanged when labor demand fits available labor" in {
    LaborEconomics.operationalHiringSlackFactor(laborDemand = 60000, availableLabor = 80000) shouldBe Share.One
  }

  it should "reconcile post-firm labor demand and realized employment from post-step state" in {
    val s2Pre = LaborEconomics.compute(world, firms, households, s1)

    val postLiving = firms.take(10).filter(Firm.isAlive)
    val postHh     = households.map(_.copy(status = HhStatus.Unemployed(0)))
    val post       = LaborEconomics.reconcilePostFirmStep(world, s1, s2Pre, postLiving, postHh)

    post.laborDemand shouldBe postLiving.map(Firm.workerCount).sum
    post.employed shouldBe 0
    decimal(post.newWage).should(be >= decimal(s1.resWage))
  }
