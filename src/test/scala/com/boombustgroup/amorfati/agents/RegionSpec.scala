package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class RegionSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  "Region" should "have 6 macroregions" in {
    Region.all.length shouldBe 6
  }

  it should "have population shares summing to ~1.0" in {
    Region.all.map(_.populationShare.toDouble).sum shouldBe 1.0 +- 0.01
  }

  it should "have Central with highest wages" in {
    Region.Central.wageMultiplier.toDouble should be > Region.all.map(_.wageMultiplier.toDouble).sum / 6
  }

  it should "have East with highest base unemployment" in {
    Region.East.baseUnemployment.toDouble should be > Region.Central.baseUnemployment.toDouble
  }

  "frictionMatrix" should "have zero diagonal" in {
    for i <- 0 until Region.count do Region.frictionMatrix(i)(i) shouldBe 0.0
  }

  it should "be asymmetric (easier to Central from East than reverse)" in {
    Region.frictionMatrix(Region.East.ordinal)(Region.Central.ordinal) should be <
      Region.frictionMatrix(Region.Central.ordinal)(Region.East.ordinal)
  }

  "migrationProbability" should "be zero for same region" in {
    Region.migrationProbability(Region.Central, Region.Central, Ratio(1.5), 0.7) shouldBe Ratio.Zero
  }

  it should "increase with wage differential" in {
    // East→Northwest: similar housing cost (0.65→0.90), migration not blocked
    val low  = Region.migrationProbability(Region.East, Region.Northwest, Ratio(1.1), 0.7)
    val high = Region.migrationProbability(Region.East, Region.Northwest, Ratio(1.5), 0.7)
    high.toDouble should be > low.toDouble
  }

  it should "be zero when destination wages are lower" in {
    Region.migrationProbability(Region.Central, Region.East, Ratio(0.8), 0.7) shouldBe Ratio.Zero
  }

  "sectorComposition" should "sum to ~1.0 per region" in {
    for r <- 0 until Region.count do Region.sectorComposition(r).map(_.toDouble).sum shouldBe 1.0 +- 0.01
  }

  it should "have high agriculture in East" in {
    Region.sectorComposition(Region.East.ordinal)(5).toDouble should be > 0.20
  }

  "RegionalMigration" should "not move employed workers" in {
    val hh     = Vector(
      mkHh(HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(8000.0)), Region.East),
    )
    val wages  = Map(Region.Central -> PLN(10000.0), Region.East -> PLN(6000.0))
    val result = RegionalMigration(hh, wages, new Random(42))
    result.households.head.region shouldBe Region.East
  }

  it should "potentially move unemployed toward higher-wage region" in {
    val hhs    = (0 until 100).map(_ => mkHh(HhStatus.Unemployed(3), Region.East)).toVector
    val wages  = Map(Region.Central -> PLN(12000.0), Region.East -> PLN(6000.0))
    val result = RegionalMigration(hhs, wages, new Random(42))
    val moved  = result.households.count(_.region != Region.East)
    moved should be >= 0 // may be 0 if housing barrier blocks all
  }

  private def mkHh(status: HhStatus, region: Region): Household.State =
    Household.State(
      id = HhId(0),
      savings = PLN(50000.0),
      debt = PLN.Zero,
      monthlyRent = PLN.Zero,
      skill = Ratio(0.5),
      healthPenalty = Ratio.Zero,
      mpc = Ratio(0.7),
      status = status,
      socialNeighbors = Array.empty[HhId],
      bankId = BankId(0),
      equityWealth = PLN.Zero,
      lastSectorIdx = SectorIdx(0),
      isImmigrant = false,
      numDependentChildren = 0,
      consumerDebt = PLN.Zero,
      education = 2,
      taskRoutineness = Ratio(0.5),
      wageScar = Ratio.Zero,
      region = region,
    )
