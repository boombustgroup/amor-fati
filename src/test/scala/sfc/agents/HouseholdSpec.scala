package sfc.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.config.{Config, SECTORS}
import sfc.engine.World
import sfc.sfc.{GovState, BankState, ForexState}

import scala.util.Random

class HouseholdSpec extends AnyFlatSpec with Matchers:

  // --- HouseholdInit ---

  "HouseholdInit.initialize" should "create correct number of households" in {
    val rng = new Random(42)
    val firms = mkFirms(100)
    val network = Array.fill(1000)(Array.empty[Int])
    val hhs = HouseholdInit.initialize(1000, 100, firms, network, rng)
    hhs.length shouldBe 1000
  }

  it should "start all households as Employed" in {
    val rng = new Random(42)
    val firms = mkFirms(100)
    val network = Array.fill(500)(Array.empty[Int])
    val hhs = HouseholdInit.initialize(500, 100, firms, network, rng)
    hhs.foreach { hh =>
      hh.status shouldBe a[HhStatus.Employed]
    }
  }

  it should "assign positive savings to all households" in {
    val rng = new Random(42)
    val firms = mkFirms(50)
    val network = Array.fill(200)(Array.empty[Int])
    val hhs = HouseholdInit.initialize(200, 50, firms, network, rng)
    hhs.foreach(_.savings should be > 0.0)
  }

  it should "have MPC in [0.5, 0.98]" in {
    val rng = new Random(42)
    val firms = mkFirms(50)
    val network = Array.fill(500)(Array.empty[Int])
    val hhs = HouseholdInit.initialize(500, 50, firms, network, rng)
    hhs.foreach { hh =>
      hh.mpc should be >= 0.5
      hh.mpc should be <= 0.98
    }
  }

  it should "have skill in [0.3, 1.0]" in {
    val rng = new Random(42)
    val firms = mkFirms(50)
    val network = Array.fill(500)(Array.empty[Int])
    val hhs = HouseholdInit.initialize(500, 50, firms, network, rng)
    hhs.foreach { hh =>
      hh.skill should be >= 0.3
      hh.skill should be <= 1.0
    }
  }

  it should "have rent >= floor" in {
    val rng = new Random(42)
    val firms = mkFirms(50)
    val network = Array.fill(500)(Array.empty[Int])
    val hhs = HouseholdInit.initialize(500, 50, firms, network, rng)
    hhs.foreach(_.monthlyRent should be >= Config.HhRentFloor)
  }

  // --- HouseholdLogic.step ---

  "HouseholdLogic.step" should "not change bankrupt households" in {
    val rng = new Random(42)
    val hhs = Vector(
      mkHousehold(0, HhStatus.Bankrupt, savings = 0.0),
      mkHousehold(1, HhStatus.Employed(0, 2, 8000.0), savings = 50000.0)
    )
    val (updated, _) = HouseholdLogic.step(hhs, mkWorld(), 0.0, 8000.0, 4666.0, 0.4, rng)
    updated(0).status shouldBe HhStatus.Bankrupt
  }

  it should "increase unemployment months for unemployed" in {
    val rng = new Random(42)
    val hhs = Vector(
      mkHousehold(0, HhStatus.Unemployed(3), savings = 50000.0)
    )
    val (updated, _) = HouseholdLogic.step(hhs, mkWorld(), 2000.0, 8000.0, 4666.0, 0.4, rng)
    updated(0).status match
      case HhStatus.Unemployed(m) => m should be >= 4
      case HhStatus.Retraining(_, _, _) => succeed  // may enter retraining
      case HhStatus.Bankrupt => succeed  // may go bankrupt
      case other => fail(s"Unexpected status: $other")
  }

  it should "apply skill decay after scarring onset" in {
    val rng = new Random(42)
    val hh = mkHousehold(0, HhStatus.Unemployed(5), savings = 100000.0, skill = 0.8)
    val (updated, _) = HouseholdLogic.step(Vector(hh), mkWorld(), 2000.0, 8000.0, 4666.0, 0.4, rng)
    updated(0).skill should be < 0.8
  }

  it should "not decay skill before scarring onset" in {
    val rng = new Random(42)
    val hh = mkHousehold(0, HhStatus.Unemployed(1), savings = 100000.0, skill = 0.8)
    val (updated, _) = HouseholdLogic.step(Vector(hh), mkWorld(), 2000.0, 8000.0, 4666.0, 0.4, rng)
    updated(0).skill shouldBe 0.8
  }

  it should "apply health scarring after onset" in {
    val rng = new Random(42)
    val hh = mkHousehold(0, HhStatus.Unemployed(5), savings = 100000.0, healthPenalty = 0.0)
    val (updated, _) = HouseholdLogic.step(Vector(hh), mkWorld(), 2000.0, 8000.0, 4666.0, 0.4, rng)
    updated(0).healthPenalty should be > 0.0
  }

  it should "bankrupt household when savings fall below threshold" in {
    val rng = new Random(42)
    val hh = mkHousehold(0, HhStatus.Unemployed(1), savings = -10000.0, rent = 1800.0)
    val (updated, _) = HouseholdLogic.step(Vector(hh), mkWorld(), 0.0, 8000.0, 4666.0, 0.4, rng)
    updated(0).status shouldBe HhStatus.Bankrupt
  }

  // --- HouseholdLogic.gini ---

  "HouseholdLogic.gini" should "return 0 for equal values" in {
    HouseholdLogic.gini(Array(100.0, 100.0, 100.0, 100.0)) shouldBe 0.0 +- 0.001
  }

  it should "return 0 for single element" in {
    HouseholdLogic.gini(Array(42.0)) shouldBe 0.0
  }

  it should "return value in [0, 1] for typical distribution" in {
    val values = Array(1000.0, 2000.0, 3000.0, 5000.0, 10000.0, 50000.0)
    val g = HouseholdLogic.gini(values)
    g should be >= 0.0
    g should be <= 1.0
  }

  it should "increase with more inequality" in {
    val equal = Array(1000.0, 1000.0, 1000.0, 1000.0)
    val unequal = Array(0.0, 0.0, 0.0, 4000.0)
    HouseholdLogic.gini(unequal) should be > HouseholdLogic.gini(equal)
  }

  // --- HouseholdLogic.computeAggregates ---

  "HouseholdLogic.computeAggregates" should "count statuses correctly" in {
    val hhs = Vector(
      mkHousehold(0, HhStatus.Employed(0, 2, 8000.0)),
      mkHousehold(1, HhStatus.Employed(1, 2, 7000.0)),
      mkHousehold(2, HhStatus.Unemployed(3)),
      mkHousehold(3, HhStatus.Retraining(4, 1, 5000.0)),
      mkHousehold(4, HhStatus.Bankrupt)
    )
    val agg = HouseholdLogic.computeAggregates(hhs, 8000.0, 4666.0, 0.4, 0, 0)
    agg.employed shouldBe 2
    agg.unemployed shouldBe 1
    agg.retraining shouldBe 1
    agg.bankrupt shouldBe 1
    agg.bankruptcyRate shouldBe 0.2 +- 0.001
  }

  // --- helpers ---

  private def mkFirms(n: Int): Array[Firm] =
    (0 until n).map { i =>
      Firm(i, 50000.0, 0.0, TechState.Traditional(10), 0.5, 1.0, 0.5,
        i % SECTORS.length, Array.empty)
    }.toArray

  private def mkHousehold(id: Int, status: HhStatus,
                          savings: Double = 20000.0, debt: Double = 0.0,
                          rent: Double = 1800.0, skill: Double = 0.7,
                          healthPenalty: Double = 0.0, mpc: Double = 0.82): Household =
    Household(id, savings, debt, rent, skill, healthPenalty, mpc, status, Array.empty)

  private def mkWorld(): World =
    World(
      month = 31,
      inflation = 0.02,
      priceLevel = 1.0,
      demandMultiplier = 1.0,
      gov = GovState(false, 0, 0, 0, 0),
      nbp = NbpState(0.0575),
      bank = BankState(1000000, 10000, 500000, 1000000),
      forex = ForexState(4.33, 0, 190000000, 0, 0),
      hh = HhState(100000, Config.BaseWage, Config.BaseReservationWage, 0, 0, 0, 0),
      automationRatio = 0.0,
      hybridRatio = 0.0,
      gdpProxy = 1e9,
      currentSigmas = SECTORS.map(_.sigma).toVector
    )
