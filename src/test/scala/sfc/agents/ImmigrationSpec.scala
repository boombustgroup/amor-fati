package sfc.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.types.*

import scala.util.Random

class ImmigrationSpec extends AnyFlatSpec with Matchers:

  // ---- computeInflow ----

  "Immigration.computeInflow" should "return 0 when disabled" in {
    // Config.ImmigEnabled is false by default
    Immigration.computeInflow(100000, 8000.0, 0.05, 1) shouldBe 0
  }

  // ---- computeOutflow ----

  "Immigration.computeOutflow" should "return 0 when disabled" in {
    Immigration.computeOutflow(5000) shouldBe 0
  }

  // ---- computeRemittances ----

  "Immigration.computeRemittances" should "return 0 when disabled" in {
    val hhs = Vector(
      Household.State(
        0,
        PLN(5000.0),
        PLN(0.0),
        PLN(1800.0),
        Ratio(0.5),
        Ratio(0.0),
        Ratio(0.85),
        HhStatus.Employed(FirmId(0), SectorIdx(1), PLN(6000.0)),
        Array.empty[Int],
        isImmigrant = true,
      ),
    )
    Immigration.computeRemittances(hhs) shouldBe 0.0
  }

  it should "return 0 for non-immigrant households" in {
    val hhs = Vector(
      Household.State(
        0,
        PLN(5000.0),
        PLN(0.0),
        PLN(1800.0),
        Ratio(0.5),
        Ratio(0.0),
        Ratio(0.85),
        HhStatus.Employed(FirmId(0), SectorIdx(1), PLN(6000.0)),
        Array.empty[Int],
        isImmigrant = false,
      ),
    )
    Immigration.computeRemittances(hhs) shouldBe 0.0
  }

  // ---- computeRemittancesAggregate ----

  "Immigration.computeRemittancesAggregate" should "return 0 when disabled" in {
    Immigration.computeRemittancesAggregate(5000, 8000.0, 0.05) shouldBe 0.0
  }

  // ---- chooseSector ----

  "Immigration.chooseSector" should "return valid sector index (0-5)" in {
    val rng = new Random(42)
    for _ <- 0 until 100 do
      val sector = Immigration.chooseSector(rng)
      sector should be >= 0
      sector should be < 6
  }

  // ---- spawnImmigrants ----

  "Immigration.spawnImmigrants" should "create correct number of immigrants" in {
    val rng = new Random(42)
    val immigrants = Immigration.spawnImmigrants(50, 1000, rng)
    immigrants.length shouldBe 50
  }

  it should "set isImmigrant=true on all spawned HH" in {
    val rng = new Random(42)
    val immigrants = Immigration.spawnImmigrants(20, 500, rng)
    immigrants.foreach(_.isImmigrant shouldBe true)
  }

  it should "assign sequential IDs starting from startId" in {
    val rng = new Random(42)
    val immigrants = Immigration.spawnImmigrants(5, 100, rng)
    immigrants.map(_.id) shouldBe Vector(100, 101, 102, 103, 104)
  }

  it should "start all immigrants as Unemployed(0)" in {
    val rng = new Random(42)
    val immigrants = Immigration.spawnImmigrants(10, 0, rng)
    immigrants.foreach(_.status shouldBe HhStatus.Unemployed(0))
  }

  it should "clamp skill within valid range" in {
    val rng = new Random(42)
    val immigrants = Immigration.spawnImmigrants(100, 0, rng)
    immigrants.foreach { h =>
      h.skill.toDouble should be >= 0.15
      h.skill.toDouble should be <= 0.95
    }
  }

  it should "clamp MPC within valid range" in {
    val rng = new Random(42)
    val immigrants = Immigration.spawnImmigrants(100, 0, rng)
    immigrants.foreach { h =>
      h.mpc.toDouble should be >= 0.7
      h.mpc.toDouble should be <= 0.98
    }
  }

  it should "set lastSectorIdx to a valid sector" in {
    val rng = new Random(42)
    val immigrants = Immigration.spawnImmigrants(100, 0, rng)
    immigrants.foreach { h =>
      h.lastSectorIdx.toInt should be >= 0
      h.lastSectorIdx.toInt should be < 6
    }
  }

  it should "produce zero immigrants when count is 0" in {
    val rng = new Random(42)
    val immigrants = Immigration.spawnImmigrants(0, 0, rng)
    immigrants shouldBe empty
  }

  // ---- removeReturnMigrants ----

  "Immigration.removeReturnMigrants" should "remove oldest immigrants first" in {
    val hhs = Vector(
      Household.State(
        0,
        PLN(1000.0),
        PLN(0.0),
        PLN(1800.0),
        Ratio(0.5),
        Ratio(0.0),
        Ratio(0.85),
        HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(6000.0)),
        Array.empty[Int],
        isImmigrant = false,
      ),
      Household.State(
        1,
        PLN(1000.0),
        PLN(0.0),
        PLN(1800.0),
        Ratio(0.5),
        Ratio(0.0),
        Ratio(0.85),
        HhStatus.Employed(FirmId(1), SectorIdx(0), PLN(5000.0)),
        Array.empty[Int],
        isImmigrant = true,
      ),
      Household.State(
        2,
        PLN(1000.0),
        PLN(0.0),
        PLN(1800.0),
        Ratio(0.5),
        Ratio(0.0),
        Ratio(0.85),
        HhStatus.Employed(FirmId(2), SectorIdx(0), PLN(5000.0)),
        Array.empty[Int],
        isImmigrant = true,
      ),
      Household.State(
        3,
        PLN(1000.0),
        PLN(0.0),
        PLN(1800.0),
        Ratio(0.5),
        Ratio(0.0),
        Ratio(0.85),
        HhStatus.Employed(FirmId(3), SectorIdx(0), PLN(5000.0)),
        Array.empty[Int],
        isImmigrant = true,
      ),
    )
    val result = Immigration.removeReturnMigrants(hhs, 2)
    result.length shouldBe 2
    result.map(_.id) should contain(0) // native stays
    result.map(_.id) should contain(3) // newest immigrant stays
    result.map(_.id) should not contain (1) // oldest immigrant removed
    result.map(_.id) should not contain (2) // second oldest removed
  }

  it should "not remove natives" in {
    val hhs = Vector(
      Household.State(
        0,
        PLN(1000.0),
        PLN(0.0),
        PLN(1800.0),
        Ratio(0.5),
        Ratio(0.0),
        Ratio(0.85),
        HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(6000.0)),
        Array.empty[Int],
        isImmigrant = false,
      ),
      Household.State(
        1,
        PLN(1000.0),
        PLN(0.0),
        PLN(1800.0),
        Ratio(0.5),
        Ratio(0.0),
        Ratio(0.85),
        HhStatus.Employed(FirmId(1), SectorIdx(0), PLN(6000.0)),
        Array.empty[Int],
        isImmigrant = false,
      ),
    )
    val result = Immigration.removeReturnMigrants(hhs, 5)
    result.length shouldBe 2 // no immigrants to remove
  }

  it should "return unchanged households when count is 0" in {
    val hhs = Vector(
      Household.State(
        0,
        PLN(1000.0),
        PLN(0.0),
        PLN(1800.0),
        Ratio(0.5),
        Ratio(0.0),
        Ratio(0.85),
        HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(6000.0)),
        Array.empty[Int],
        isImmigrant = true,
      ),
    )
    Immigration.removeReturnMigrants(hhs, 0) shouldBe hhs
  }

  // ---- step ----

  "Immigration.step" should "return zero state when disabled" in {
    val prev = Immigration.State(100, 0, 0, 0.0)
    val result = Immigration.step(prev, None, 8000.0, 0.05, 100000, 1)
    result.monthlyInflow shouldBe 0
    result.monthlyOutflow shouldBe 0
    result.remittanceOutflow shouldBe 0.0
    result.immigrantStock shouldBe 100 // stock preserved (no inflow/outflow when disabled)
  }

  it should "maintain non-negative immigrant stock" in {
    // Even with large outflow, stock should not go negative
    val prev = Immigration.State(2, 0, 0, 0.0)
    val result = Immigration.step(prev, None, 8000.0, 0.05, 100000, 1)
    result.immigrantStock should be >= 0
  }

  // ---- Immigration.State.zero ----

  "Immigration.State.zero" should "have all fields at zero" in {
    Immigration.State.zero.immigrantStock shouldBe 0
    Immigration.State.zero.monthlyInflow shouldBe 0
    Immigration.State.zero.monthlyOutflow shouldBe 0
    Immigration.State.zero.remittanceOutflow shouldBe 0.0
  }
