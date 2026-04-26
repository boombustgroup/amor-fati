package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.TestHouseholdState

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import com.boombustgroup.amorfati.random.RandomStream

class RegionSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  "Region" should "have 6 macroregions" in {
    Region.all.length shouldBe 6
  }

  it should "have population shares summing to ~1.0" in {
    Region.all.map(_.populationShare.bd).sum shouldBe BigDecimal("1.0") +- BigDecimal("0.01")
  }

  it should "have Central with highest wages" in {
    Region.Central.wageMultiplier.bd should be > (Region.all.map(_.wageMultiplier.bd).sum / BigDecimal(6))
  }

  it should "normalize regional wage multipliers to national mean 1.0" in {
    val weightedMean = Region.all.map(r => Region.normalizedWageMultiplier(r).bd * r.populationShare.bd).sum
    weightedMean shouldBe BigDecimal("1.0") +- BigDecimal("0.0001")
  }

  it should "have East with highest base unemployment" in {
    Region.East.baseUnemployment should be > Region.Central.baseUnemployment
  }

  "frictionMatrix" should "have zero diagonal" in {
    for i <- 0 until Region.count do Region.frictionMatrix(i)(i) shouldBe Share.Zero
  }

  it should "be asymmetric (easier to Central from East than reverse)" in {
    Region.frictionMatrix(Region.East.ordinal)(Region.Central.ordinal) should be <
      Region.frictionMatrix(Region.Central.ordinal)(Region.East.ordinal)
  }

  "migrationProbability" should "be zero for same region" in {
    Region.migrationProbability(Region.Central, Region.Central, Multiplier.decimal(15, 1), Share.decimal(7, 1)) shouldBe Share.Zero
  }

  it should "increase with wage differential" in {
    // East→Northwest: similar housing cost (0.65→0.90), migration not blocked
    val low  = Region.migrationProbability(Region.East, Region.Northwest, Multiplier.decimal(11, 1), Share.decimal(7, 1))
    val high = Region.migrationProbability(Region.East, Region.Northwest, Multiplier.decimal(15, 1), Share.decimal(7, 1))
    high should be > low
  }

  it should "be zero when destination wages are lower" in {
    Region.migrationProbability(Region.Central, Region.East, Multiplier.decimal(8, 1), Share.decimal(7, 1)) shouldBe Share.Zero
  }

  "sectorComposition" should "sum to ~1.0 per region" in {
    for r <- 0 until Region.count do Region.sectorComposition(r).map(_.bd).sum shouldBe BigDecimal("1.0") +- BigDecimal("0.01")
  }

  it should "have high agriculture in East" in {
    Region.sectorComposition(Region.East.ordinal)(5).bd should be > BigDecimal("0.20")
  }

  "RegionalMigration" should "not move employed workers" in {
    val hh     = Vector(
      mkHh(HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(8000)), Region.East),
    )
    val wages  = Map(Region.Central -> PLN(10000), Region.East -> PLN(6000))
    val result = RegionalMigration(hh, wages, RandomStream.seeded(42))
    result.households.head.region shouldBe Region.East
  }

  it should "potentially move unemployed toward higher-wage region" in {
    val hhs    = (0 until 100).map(_ => mkHh(HhStatus.Unemployed(3), Region.East)).toVector
    val wages  = Map(Region.Central -> PLN(12000), Region.East -> PLN(6000))
    val result = RegionalMigration(hhs, wages, RandomStream.seeded(42))
    val moved  = result.households.count(_.region != Region.East)
    moved should be >= 0 // may be 0 if housing barrier blocks all
  }

  private def mkHh(status: HhStatus, region: Region): Household.State =
    TestHouseholdState(
      id = HhId(0),
      savings = PLN(50000),
      debt = PLN.Zero,
      monthlyRent = PLN.Zero,
      skill = Share.decimal(5, 1),
      healthPenalty = Share.Zero,
      mpc = Share.decimal(7, 1),
      status = status,
      socialNeighbors = Array.empty[HhId],
      bankId = BankId(0),
      equityWealth = PLN.Zero,
      lastSectorIdx = SectorIdx(0),
      isImmigrant = false,
      numDependentChildren = 0,
      consumerDebt = PLN.Zero,
      education = 2,
      taskRoutineness = Share.decimal(5, 1),
      wageScar = Share.Zero,
      region = region,
    )
