package com.boombustgroup.amorfati.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.types.*

import scala.util.Random

class ImmigrationSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams = SimParams.defaults

  // ---- computeInflow ----

  "Immigration.computeRemittances" should "return 0 for non-immigrant households" in {
    val hhs = Vector(
      Household.State(
        HhId(0),
        PLN(5000.0),
        PLN(0.0),
        PLN(1800.0),
        Share(0.5),
        Share(0.0),
        Share(0.85),
        HhStatus.Employed(FirmId(0), SectorIdx(1), PLN(6000.0)),
        Array.empty[HhId],
        bankId = BankId(0),
        equityWealth = PLN.Zero,
        lastSectorIdx = SectorIdx(-1),
        isImmigrant = false,
        numDependentChildren = 0,
        consumerDebt = PLN.Zero,
        education = 2,
        taskRoutineness = Share(0.5),
        wageScar = Share.Zero,
      ),
    )
    Immigration.computeRemittances(hhs) shouldBe PLN.Zero
  }

  // ---- chooseSector ----

  "Immigration.chooseSector" should "return valid sector index (0-5)" in {
    val rng = new Random(42)
    for _ <- 0 until 100 do
      val sector = Immigration.chooseSector(rng)
      sector.toInt should be >= 0
      sector.toInt should be < 6
  }

  // ---- spawnImmigrants ----

  "Immigration.spawnImmigrants" should "create correct number of immigrants" in {
    val rng        = new Random(42)
    val immigrants = Immigration.spawnImmigrants(50, 1000, rng)
    immigrants.length shouldBe 50
  }

  it should "set isImmigrant=true on all spawned HH" in {
    val rng        = new Random(42)
    val immigrants = Immigration.spawnImmigrants(20, 500, rng)
    immigrants.foreach(_.isImmigrant shouldBe true)
  }

  it should "assign sequential IDs starting from startId" in {
    val rng        = new Random(42)
    val immigrants = Immigration.spawnImmigrants(5, 100, rng)
    immigrants.map(_.id) shouldBe Vector(HhId(100), HhId(101), HhId(102), HhId(103), HhId(104))
  }

  it should "start all immigrants as Unemployed(0)" in {
    val rng        = new Random(42)
    val immigrants = Immigration.spawnImmigrants(10, 0, rng)
    immigrants.foreach(_.status shouldBe HhStatus.Unemployed(0))
  }

  it should "clamp skill within valid range" in {
    val rng        = new Random(42)
    val immigrants = Immigration.spawnImmigrants(100, 0, rng)
    immigrants.foreach { h =>
      h.skill should be >= Share(0.15)
      h.skill should be <= Share(0.95)
    }
  }

  it should "clamp MPC within valid range" in {
    val rng        = new Random(42)
    val immigrants = Immigration.spawnImmigrants(100, 0, rng)
    immigrants.foreach { h =>
      h.mpc should be >= Share(0.7)
      h.mpc should be <= Share(0.98)
    }
  }

  it should "set lastSectorIdx to a valid sector" in {
    val rng        = new Random(42)
    val immigrants = Immigration.spawnImmigrants(100, 0, rng)
    immigrants.foreach { h =>
      h.lastSectorIdx.toInt should be >= 0
      h.lastSectorIdx.toInt should be < 6
    }
  }

  it should "produce zero immigrants when count is 0" in {
    val rng        = new Random(42)
    val immigrants = Immigration.spawnImmigrants(0, 0, rng)
    immigrants shouldBe empty
  }

  // ---- removeReturnMigrants ----

  "Immigration.removeReturnMigrants" should "remove oldest immigrants first" in {
    val hhs    = Vector(
      Household.State(
        HhId(0),
        PLN(1000.0),
        PLN.Zero,
        PLN(1800.0),
        Share(0.5),
        Share(0.0),
        Share(0.85),
        HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(6000.0)),
        Array.empty[HhId],
        bankId = BankId(0),
        equityWealth = PLN.Zero,
        lastSectorIdx = SectorIdx(-1),
        isImmigrant = false,
        numDependentChildren = 0,
        consumerDebt = PLN.Zero,
        education = 2,
        taskRoutineness = Share(0.5),
        wageScar = Share.Zero,
      ),
      Household.State(
        HhId(1),
        PLN(1000.0),
        PLN.Zero,
        PLN(1800.0),
        Share(0.5),
        Share(0.0),
        Share(0.85),
        HhStatus.Employed(FirmId(1), SectorIdx(0), PLN(5000.0)),
        Array.empty[HhId],
        bankId = BankId(0),
        equityWealth = PLN.Zero,
        lastSectorIdx = SectorIdx(-1),
        isImmigrant = true,
        numDependentChildren = 0,
        consumerDebt = PLN.Zero,
        education = 2,
        taskRoutineness = Share(0.5),
        wageScar = Share.Zero,
      ),
      Household.State(
        HhId(2),
        PLN(1000.0),
        PLN.Zero,
        PLN(1800.0),
        Share(0.5),
        Share(0.0),
        Share(0.85),
        HhStatus.Employed(FirmId(2), SectorIdx(0), PLN(5000.0)),
        Array.empty[HhId],
        bankId = BankId(0),
        equityWealth = PLN.Zero,
        lastSectorIdx = SectorIdx(-1),
        isImmigrant = true,
        numDependentChildren = 0,
        consumerDebt = PLN.Zero,
        education = 2,
        taskRoutineness = Share(0.5),
        wageScar = Share.Zero,
      ),
      Household.State(
        HhId(3),
        PLN(1000.0),
        PLN.Zero,
        PLN(1800.0),
        Share(0.5),
        Share(0.0),
        Share(0.85),
        HhStatus.Employed(FirmId(3), SectorIdx(0), PLN(5000.0)),
        Array.empty[HhId],
        bankId = BankId(0),
        equityWealth = PLN.Zero,
        lastSectorIdx = SectorIdx(-1),
        isImmigrant = true,
        numDependentChildren = 0,
        consumerDebt = PLN.Zero,
        education = 2,
        taskRoutineness = Share(0.5),
        wageScar = Share.Zero,
      ),
    )
    val result = Immigration.removeReturnMigrants(hhs, 2)
    result.length shouldBe 2
    result.map(_.id) should contain(HhId(0))    // native stays
    result.map(_.id) should contain(HhId(3))    // newest immigrant stays
    result.map(_.id) should not contain HhId(1) // oldest immigrant removed
    result.map(_.id) should not contain HhId(2) // second oldest removed
  }

  it should "not remove natives" in {
    val hhs    = Vector(
      Household.State(
        HhId(0),
        PLN(1000.0),
        PLN.Zero,
        PLN(1800.0),
        Share(0.5),
        Share(0.0),
        Share(0.85),
        HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(6000.0)),
        Array.empty[HhId],
        bankId = BankId(0),
        equityWealth = PLN.Zero,
        lastSectorIdx = SectorIdx(-1),
        isImmigrant = false,
        numDependentChildren = 0,
        consumerDebt = PLN.Zero,
        education = 2,
        taskRoutineness = Share(0.5),
        wageScar = Share.Zero,
      ),
      Household.State(
        HhId(1),
        PLN(1000.0),
        PLN.Zero,
        PLN(1800.0),
        Share(0.5),
        Share(0.0),
        Share(0.85),
        HhStatus.Employed(FirmId(1), SectorIdx(0), PLN(6000.0)),
        Array.empty[HhId],
        bankId = BankId(0),
        equityWealth = PLN.Zero,
        lastSectorIdx = SectorIdx(-1),
        isImmigrant = false,
        numDependentChildren = 0,
        consumerDebt = PLN.Zero,
        education = 2,
        taskRoutineness = Share(0.5),
        wageScar = Share.Zero,
      ),
    )
    val result = Immigration.removeReturnMigrants(hhs, 5)
    result.length shouldBe 2 // no immigrants to remove
  }

  it should "return unchanged households when count is 0" in {
    val hhs = Vector(
      Household.State(
        HhId(0),
        PLN(1000.0),
        PLN.Zero,
        PLN(1800.0),
        Share(0.5),
        Share(0.0),
        Share(0.85),
        HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(6000.0)),
        Array.empty[HhId],
        bankId = BankId(0),
        equityWealth = PLN.Zero,
        lastSectorIdx = SectorIdx(-1),
        isImmigrant = true,
        numDependentChildren = 0,
        consumerDebt = PLN.Zero,
        education = 2,
        taskRoutineness = Share(0.5),
        wageScar = Share.Zero,
      ),
    )
    Immigration.removeReturnMigrants(hhs, 0) shouldBe hhs
  }

  // ---- step ----

  "Immigration.step" should "maintain non-negative immigrant stock" in {
    // Even with large outflow, stock should not go negative
    val prev   = Immigration.State(2, 0, 0, PLN.Zero)
    val result = Immigration.step(prev, Vector.empty, PLN(8000.0), Share(0.05))
    result.immigrantStock should be >= 0
  }

  // ---- Immigration.State.zero ----

  "Immigration.State.zero" should "have all fields at zero" in {
    Immigration.State.zero.immigrantStock shouldBe 0
    Immigration.State.zero.monthlyInflow shouldBe 0
    Immigration.State.zero.monthlyOutflow shouldBe 0
    Immigration.State.zero.remittanceOutflow shouldBe PLN.Zero
  }
