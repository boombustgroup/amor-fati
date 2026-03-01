package sfc.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class ImmigrationSpec extends AnyFlatSpec with Matchers:

  // ---- computeInflow ----

  "ImmigrationLogic.computeInflow" should "return 0 when disabled" in {
    // Config.ImmigEnabled is false by default
    ImmigrationLogic.computeInflow(100000, 8000.0, 0.05, 1) shouldBe 0
  }

  // ---- computeOutflow ----

  "ImmigrationLogic.computeOutflow" should "return 0 when disabled" in {
    ImmigrationLogic.computeOutflow(5000) shouldBe 0
  }

  // ---- computeRemittances ----

  "ImmigrationLogic.computeRemittances" should "return 0 when disabled" in {
    val hhs = Vector(
      Household(0, 5000.0, 0, 1800.0, 0.5, 0.0, 0.85,
        HhStatus.Employed(0, 1, 6000.0), Array.empty, isImmigrant = true)
    )
    ImmigrationLogic.computeRemittances(hhs) shouldBe 0.0
  }

  it should "return 0 for non-immigrant households" in {
    val hhs = Vector(
      Household(0, 5000.0, 0, 1800.0, 0.5, 0.0, 0.85,
        HhStatus.Employed(0, 1, 6000.0), Array.empty, isImmigrant = false)
    )
    ImmigrationLogic.computeRemittances(hhs) shouldBe 0.0
  }

  // ---- computeRemittancesAggregate ----

  "ImmigrationLogic.computeRemittancesAggregate" should "return 0 when disabled" in {
    ImmigrationLogic.computeRemittancesAggregate(5000, 8000.0, 0.05) shouldBe 0.0
  }

  // ---- chooseSector ----

  "ImmigrationLogic.chooseSector" should "return valid sector index (0-5)" in {
    val rng = new Random(42)
    for _ <- 0 until 100 do
      val sector = ImmigrationLogic.chooseSector(rng)
      sector should be >= 0
      sector should be < 6
  }

  // ---- spawnImmigrants ----

  "ImmigrationLogic.spawnImmigrants" should "create correct number of immigrants" in {
    val rng = new Random(42)
    val immigrants = ImmigrationLogic.spawnImmigrants(50, 1000, rng)
    immigrants.length shouldBe 50
  }

  it should "set isImmigrant=true on all spawned HH" in {
    val rng = new Random(42)
    val immigrants = ImmigrationLogic.spawnImmigrants(20, 500, rng)
    immigrants.foreach(_.isImmigrant shouldBe true)
  }

  it should "assign sequential IDs starting from startId" in {
    val rng = new Random(42)
    val immigrants = ImmigrationLogic.spawnImmigrants(5, 100, rng)
    immigrants.map(_.id) shouldBe Vector(100, 101, 102, 103, 104)
  }

  it should "start all immigrants as Unemployed(0)" in {
    val rng = new Random(42)
    val immigrants = ImmigrationLogic.spawnImmigrants(10, 0, rng)
    immigrants.foreach(_.status shouldBe HhStatus.Unemployed(0))
  }

  it should "clamp skill within valid range" in {
    val rng = new Random(42)
    val immigrants = ImmigrationLogic.spawnImmigrants(100, 0, rng)
    immigrants.foreach { h =>
      h.skill should be >= 0.15
      h.skill should be <= 0.95
    }
  }

  it should "clamp MPC within valid range" in {
    val rng = new Random(42)
    val immigrants = ImmigrationLogic.spawnImmigrants(100, 0, rng)
    immigrants.foreach { h =>
      h.mpc should be >= 0.7
      h.mpc should be <= 0.98
    }
  }

  it should "set lastSectorIdx to a valid sector" in {
    val rng = new Random(42)
    val immigrants = ImmigrationLogic.spawnImmigrants(100, 0, rng)
    immigrants.foreach { h =>
      h.lastSectorIdx should be >= 0
      h.lastSectorIdx should be < 6
    }
  }

  it should "produce zero immigrants when count is 0" in {
    val rng = new Random(42)
    val immigrants = ImmigrationLogic.spawnImmigrants(0, 0, rng)
    immigrants shouldBe empty
  }

  // ---- removeReturnMigrants ----

  "ImmigrationLogic.removeReturnMigrants" should "remove oldest immigrants first" in {
    val hhs = Vector(
      Household(0, 1000.0, 0, 1800.0, 0.5, 0.0, 0.85,
        HhStatus.Employed(0, 0, 6000.0), Array.empty, isImmigrant = false),
      Household(1, 1000.0, 0, 1800.0, 0.5, 0.0, 0.85,
        HhStatus.Employed(1, 0, 5000.0), Array.empty, isImmigrant = true),
      Household(2, 1000.0, 0, 1800.0, 0.5, 0.0, 0.85,
        HhStatus.Employed(2, 0, 5000.0), Array.empty, isImmigrant = true),
      Household(3, 1000.0, 0, 1800.0, 0.5, 0.0, 0.85,
        HhStatus.Employed(3, 0, 5000.0), Array.empty, isImmigrant = true)
    )
    val result = ImmigrationLogic.removeReturnMigrants(hhs, 2)
    result.length shouldBe 2
    result.map(_.id) should contain(0)   // native stays
    result.map(_.id) should contain(3)   // newest immigrant stays
    result.map(_.id) should not contain(1)  // oldest immigrant removed
    result.map(_.id) should not contain(2)  // second oldest removed
  }

  it should "not remove natives" in {
    val hhs = Vector(
      Household(0, 1000.0, 0, 1800.0, 0.5, 0.0, 0.85,
        HhStatus.Employed(0, 0, 6000.0), Array.empty, isImmigrant = false),
      Household(1, 1000.0, 0, 1800.0, 0.5, 0.0, 0.85,
        HhStatus.Employed(1, 0, 6000.0), Array.empty, isImmigrant = false)
    )
    val result = ImmigrationLogic.removeReturnMigrants(hhs, 5)
    result.length shouldBe 2  // no immigrants to remove
  }

  it should "return unchanged households when count is 0" in {
    val hhs = Vector(
      Household(0, 1000.0, 0, 1800.0, 0.5, 0.0, 0.85,
        HhStatus.Employed(0, 0, 6000.0), Array.empty, isImmigrant = true)
    )
    ImmigrationLogic.removeReturnMigrants(hhs, 0) shouldBe hhs
  }

  // ---- step ----

  "ImmigrationLogic.step" should "return zero state when disabled" in {
    val prev = ImmigrationState(100, 0, 0, 0.0)
    val result = ImmigrationLogic.step(prev, None, 8000.0, 0.05, 100000, 1)
    result.monthlyInflow shouldBe 0
    result.monthlyOutflow shouldBe 0
    result.remittanceOutflow shouldBe 0.0
    result.immigrantStock shouldBe 100  // stock preserved (no inflow/outflow when disabled)
  }

  it should "maintain non-negative immigrant stock" in {
    // Even with large outflow, stock should not go negative
    val prev = ImmigrationState(2, 0, 0, 0.0)
    val result = ImmigrationLogic.step(prev, None, 8000.0, 0.05, 100000, 1)
    result.immigrantStock should be >= 0
  }

  // ---- ImmigrationState.zero ----

  "ImmigrationState.zero" should "have all fields at zero" in {
    ImmigrationState.zero.immigrantStock shouldBe 0
    ImmigrationState.zero.monthlyInflow shouldBe 0
    ImmigrationState.zero.monthlyOutflow shouldBe 0
    ImmigrationState.zero.remittanceOutflow shouldBe 0.0
  }
