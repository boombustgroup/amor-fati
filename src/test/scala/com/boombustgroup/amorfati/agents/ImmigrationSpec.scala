package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.TestHouseholdState

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.types.*

import com.boombustgroup.amorfati.random.RandomStream

class ImmigrationSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams = SimParams.defaults

  // ---- computeInflow ----

  "Immigration.computeRemittances" should "return 0 for non-immigrant households" in {
    val hhs = Vector(
      TestHouseholdState(
        HhId(0),
        PLN(5000),
        PLN(0),
        PLN(1800),
        Share.decimal(5, 1),
        Share(0),
        Share.decimal(85, 2),
        HhStatus.Employed(FirmId(0), SectorIdx(1), PLN(6000)),
        Array.empty[HhId],
        bankId = BankId(0),
        equityWealth = PLN.Zero,
        lastSectorIdx = SectorIdx(-1),
        isImmigrant = false,
        numDependentChildren = 0,
        consumerDebt = PLN.Zero,
        education = 2,
        taskRoutineness = Share.decimal(5, 1),
        wageScar = Share.Zero,
      ),
    )
    Immigration.computeRemittances(hhs) shouldBe PLN.Zero
  }

  // ---- chooseSector ----

  "Immigration.chooseSector" should "return valid sector index (0-5)" in {
    val rng = RandomStream.seeded(42)
    for _ <- 0 until 100 do
      val sector = Immigration.chooseSector(rng)
      sector.toInt should be >= 0
      sector.toInt should be < 6
  }

  // ---- spawnImmigrants ----

  "Immigration.spawnImmigrants" should "create correct number of immigrants" in {
    val rng        = RandomStream.seeded(42)
    val immigrants = Immigration.spawnImmigrants(50, 1000, rng)
    immigrants.length shouldBe 50
  }

  it should "set isImmigrant=true on all spawned HH" in {
    val rng        = RandomStream.seeded(42)
    val immigrants = Immigration.spawnImmigrants(20, 500, rng)
    immigrants.foreach(_.isImmigrant shouldBe true)
  }

  it should "assign sequential IDs starting from startId" in {
    val rng        = RandomStream.seeded(42)
    val immigrants = Immigration.spawnImmigrants(5, 100, rng)
    immigrants.map(_.id) shouldBe Vector(HhId(100), HhId(101), HhId(102), HhId(103), HhId(104))
  }

  it should "start all immigrants as Unemployed(0)" in {
    val rng        = RandomStream.seeded(42)
    val immigrants = Immigration.spawnImmigrants(10, 0, rng)
    immigrants.foreach(_.status shouldBe HhStatus.Unemployed(0))
  }

  it should "assign endogenous contract types from sampled sectors" in {
    val rng        = RandomStream.seeded(42)
    val immigrants = Immigration.spawnImmigrants(200, 0, rng)
    val contracts  = immigrants.map(_.contractType).toSet

    contracts should contain(ContractType.Zlecenie)
    contracts should contain(ContractType.B2B)
  }

  it should "clamp skill within valid range" in {
    val rng        = RandomStream.seeded(42)
    val immigrants = Immigration.spawnImmigrants(100, 0, rng)
    immigrants.foreach { h =>
      h.skill should be >= Share.decimal(15, 2)
      h.skill should be <= Share.decimal(95, 2)
    }
  }

  it should "clamp MPC within valid range" in {
    val rng        = RandomStream.seeded(42)
    val immigrants = Immigration.spawnImmigrants(100, 0, rng)
    immigrants.foreach { h =>
      h.mpc should be >= Share.decimal(7, 1)
      h.mpc should be <= Share.decimal(98, 2)
    }
  }

  it should "set lastSectorIdx to a valid sector" in {
    val rng        = RandomStream.seeded(42)
    val immigrants = Immigration.spawnImmigrants(100, 0, rng)
    immigrants.foreach { h =>
      h.lastSectorIdx.toInt should be >= 0
      h.lastSectorIdx.toInt should be < 6
    }
  }

  it should "produce zero immigrants when count is 0" in {
    val rng        = RandomStream.seeded(42)
    val immigrants = Immigration.spawnImmigrants(0, 0, rng)
    immigrants shouldBe empty
  }

  // ---- removeReturnMigrants ----

  "Immigration.removeReturnMigrants" should "remove oldest immigrants first" in {
    val hhs    = Vector(
      TestHouseholdState(
        HhId(0),
        PLN(1000),
        PLN.Zero,
        PLN(1800),
        Share.decimal(5, 1),
        Share(0),
        Share.decimal(85, 2),
        HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(6000)),
        Array.empty[HhId],
        bankId = BankId(0),
        equityWealth = PLN.Zero,
        lastSectorIdx = SectorIdx(-1),
        isImmigrant = false,
        numDependentChildren = 0,
        consumerDebt = PLN.Zero,
        education = 2,
        taskRoutineness = Share.decimal(5, 1),
        wageScar = Share.Zero,
      ),
      TestHouseholdState(
        HhId(1),
        PLN(1000),
        PLN.Zero,
        PLN(1800),
        Share.decimal(5, 1),
        Share(0),
        Share.decimal(85, 2),
        HhStatus.Employed(FirmId(1), SectorIdx(0), PLN(5000)),
        Array.empty[HhId],
        bankId = BankId(0),
        equityWealth = PLN.Zero,
        lastSectorIdx = SectorIdx(-1),
        isImmigrant = true,
        numDependentChildren = 0,
        consumerDebt = PLN.Zero,
        education = 2,
        taskRoutineness = Share.decimal(5, 1),
        wageScar = Share.Zero,
      ),
      TestHouseholdState(
        HhId(2),
        PLN(1000),
        PLN.Zero,
        PLN(1800),
        Share.decimal(5, 1),
        Share(0),
        Share.decimal(85, 2),
        HhStatus.Employed(FirmId(2), SectorIdx(0), PLN(5000)),
        Array.empty[HhId],
        bankId = BankId(0),
        equityWealth = PLN.Zero,
        lastSectorIdx = SectorIdx(-1),
        isImmigrant = true,
        numDependentChildren = 0,
        consumerDebt = PLN.Zero,
        education = 2,
        taskRoutineness = Share.decimal(5, 1),
        wageScar = Share.Zero,
      ),
      TestHouseholdState(
        HhId(3),
        PLN(1000),
        PLN.Zero,
        PLN(1800),
        Share.decimal(5, 1),
        Share(0),
        Share.decimal(85, 2),
        HhStatus.Employed(FirmId(3), SectorIdx(0), PLN(5000)),
        Array.empty[HhId],
        bankId = BankId(0),
        equityWealth = PLN.Zero,
        lastSectorIdx = SectorIdx(-1),
        isImmigrant = true,
        numDependentChildren = 0,
        consumerDebt = PLN.Zero,
        education = 2,
        taskRoutineness = Share.decimal(5, 1),
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
      TestHouseholdState(
        HhId(0),
        PLN(1000),
        PLN.Zero,
        PLN(1800),
        Share.decimal(5, 1),
        Share(0),
        Share.decimal(85, 2),
        HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(6000)),
        Array.empty[HhId],
        bankId = BankId(0),
        equityWealth = PLN.Zero,
        lastSectorIdx = SectorIdx(-1),
        isImmigrant = false,
        numDependentChildren = 0,
        consumerDebt = PLN.Zero,
        education = 2,
        taskRoutineness = Share.decimal(5, 1),
        wageScar = Share.Zero,
      ),
      TestHouseholdState(
        HhId(1),
        PLN(1000),
        PLN.Zero,
        PLN(1800),
        Share.decimal(5, 1),
        Share(0),
        Share.decimal(85, 2),
        HhStatus.Employed(FirmId(1), SectorIdx(0), PLN(6000)),
        Array.empty[HhId],
        bankId = BankId(0),
        equityWealth = PLN.Zero,
        lastSectorIdx = SectorIdx(-1),
        isImmigrant = false,
        numDependentChildren = 0,
        consumerDebt = PLN.Zero,
        education = 2,
        taskRoutineness = Share.decimal(5, 1),
        wageScar = Share.Zero,
      ),
    )
    val result = Immigration.removeReturnMigrants(hhs, 5)
    result.length shouldBe 2 // no immigrants to remove
  }

  it should "return unchanged households when count is 0" in {
    val hhs = Vector(
      TestHouseholdState(
        HhId(0),
        PLN(1000),
        PLN.Zero,
        PLN(1800),
        Share.decimal(5, 1),
        Share(0),
        Share.decimal(85, 2),
        HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(6000)),
        Array.empty[HhId],
        bankId = BankId(0),
        equityWealth = PLN.Zero,
        lastSectorIdx = SectorIdx(-1),
        isImmigrant = true,
        numDependentChildren = 0,
        consumerDebt = PLN.Zero,
        education = 2,
        taskRoutineness = Share.decimal(5, 1),
        wageScar = Share.Zero,
      ),
    )
    Immigration.removeReturnMigrants(hhs, 0) shouldBe hhs
  }

  // ---- step ----

  "Immigration.step" should "maintain non-negative immigrant stock" in {
    // Even with large outflow, stock should not go negative
    val prev   = Immigration.State(2, 0, 0, PLN.Zero)
    val result = Immigration.step(prev, Vector.empty, PLN(8000), Share.decimal(5, 2))
    result.immigrantStock should be >= 0
  }

  // ---- Immigration.State.zero ----

  "Immigration.State.zero" should "have all fields at zero" in {
    Immigration.State.zero.immigrantStock shouldBe 0
    Immigration.State.zero.monthlyInflow shouldBe 0
    Immigration.State.zero.monthlyOutflow shouldBe 0
    Immigration.State.zero.remittanceOutflow shouldBe PLN.Zero
  }
