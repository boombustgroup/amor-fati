package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.TestHouseholdState

import com.boombustgroup.amorfati.TestFirmState

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.mechanisms.SectoralMobility
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import com.boombustgroup.amorfati.random.RandomStream

class SectoralMobilitySpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  private def adjustedSuccess(base: BigDecimal, friction: BigDecimal): BigDecimal =
    decimal(SectoralMobility.frictionAdjustedSuccess(shareBD(base), shareBD(friction)))

  // --- Default friction matrix ---

  "DefaultFrictionMatrix" should "be 6x6" in {
    SectoralMobility.DefaultFrictionMatrix.length shouldBe 6
    SectoralMobility.DefaultFrictionMatrix.foreach(_.length shouldBe 6)
  }

  it should "have zero diagonal" in {
    for i <- 0 until 6 do SectoralMobility.DefaultFrictionMatrix(i)(i) shouldBe Share.Zero
  }

  it should "be symmetric" in {
    val m = SectoralMobility.DefaultFrictionMatrix
    for i <- 0 until 6; j <- 0 until 6 do m(i)(j) shouldBe m(j)(i)
  }

  it should "have values in [0, 1]" in {
    for row <- SectoralMobility.DefaultFrictionMatrix; v <- row do
      v.bd should be >= BigDecimal(0)
      v.bd should be <= BigDecimal("1.0")
  }

  // --- sectorVacancies ---

  "sectorVacancies" should "return non-negative vacancies per sector" in {
    val firms = mkFirms(6)
    val hhs   =
      (0 until 5).map(i => mkHousehold(i, HhStatus.Employed(FirmId(i % 6), SectorIdx(i % 6), PLN(8000)))).toVector
    val vac   = SectoralMobility.sectorVacancies(hhs, firms)
    vac.length shouldBe 6
    vac.foreach(_ should be >= 0)
  }

  it should "show vacancies when firms need more workers than employed" in {
    val firms = Vector(mkFirm(0, 2, TechState.Traditional(10)))                               // needs 10
    val hhs   = Vector(mkHousehold(0, HhStatus.Employed(FirmId(0), SectorIdx(2), PLN(8000)))) // 1 employed
    val vac   = SectoralMobility.sectorVacancies(hhs, firms)
    vac(2) shouldBe 9 // 10 needed - 1 employed
  }

  // --- sectorWages ---

  "sectorWages" should "compute average wage per sector" in {
    val hhs   = Vector(
      mkHousehold(0, HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(10000))),
      mkHousehold(1, HhStatus.Employed(FirmId(1), SectorIdx(0), PLN(12000))),
      mkHousehold(2, HhStatus.Employed(FirmId(2), SectorIdx(2), PLN(8000))),
      mkHousehold(3, HhStatus.Unemployed(1)),
    )
    val wages = SectoralMobility.sectorWages(hhs)
    wages(0).bd shouldBe (BigDecimal("11000.0") +- BigDecimal("0.01")) // (10000+12000)/2
    wages(2).bd shouldBe BigDecimal("8000.0")
    wages(1) shouldBe PLN.Zero                                         // no employed in sector 1
  }

  // --- selectTargetSector ---

  "selectTargetSector" should "not select the source sector" in {
    val rng   = RandomStream.seeded(42)
    val wages = Vector(PLN(10000), PLN(12000), PLN(8000), PLN(15000), PLN(9000), PLN(7000))
    val vac   = Vector(5, 10, 3, 8, 2, 1)
    for _ <- 0 until 100 do
      val target =
        SectoralMobility.selectTargetSector(0, wages, vac, SectoralMobility.DefaultFrictionMatrix, Coefficient(2), rng)
      target should not be 0
  }

  it should "prefer high-wage high-vacancy low-friction sectors" in {
    val rng    = RandomStream.seeded(42)
    // Sector 1 (Mfg): high wage, high vacancies
    // Sector 5 (Agr): high friction from BPO (0.9)
    val wages  = Vector(PLN(0), PLN(20000), PLN(5000), PLN(5000), PLN(5000), PLN(5000))
    val vac    = Vector(0, 100, 1, 1, 1, 1)
    val counts = new Array[Int](6)
    for _ <- 0 until 1000 do
      val target =
        SectoralMobility.selectTargetSector(0, wages, vac, SectoralMobility.DefaultFrictionMatrix, Coefficient(2), rng)
      counts(target) += 1
    // Sector 1 should be heavily preferred
    counts(1) should be > 500
  }

  it should "handle all-zero wages gracefully" in {
    val rng    = RandomStream.seeded(42)
    val wages  = Vector.fill(6)(PLN.Zero)
    val vac    = Vector.fill(6)(0)
    val target =
      SectoralMobility.selectTargetSector(0, wages, vac, SectoralMobility.DefaultFrictionMatrix, Coefficient(2), rng)
    target should not be 0
    target should be >= 0
    target should be < 6
  }

  // --- frictionAdjustedParams ---

  "frictionAdjustedParams" should "increase duration and cost with higher friction" in {
    val rp0 = SectoralMobility.frictionAdjustedParams(Share(0), Multiplier(1), Share.decimal(5, 1))
    val rp9 = SectoralMobility.frictionAdjustedParams(Share.decimal(9, 1), Multiplier(1), Share.decimal(5, 1))
    rp9.duration should be > rp0.duration
    rp9.cost.bd should be > rp0.cost.bd
  }

  it should "return base values at zero friction" in {
    val rp = SectoralMobility.frictionAdjustedParams(Share(0), Multiplier(1), Share.decimal(5, 1))
    rp.duration shouldBe p.household.retrainingDuration
    rp.cost.bd shouldBe (p.household.retrainingCost.bd +- BigDecimal("0.01"))
  }

  // --- crossSectorWagePenalty ---

  "crossSectorWagePenalty" should "return 1.0 at zero friction" in {
    SectoralMobility.crossSectorWagePenalty(Share(0)).bd shouldBe BigDecimal("1.0")
  }

  it should "return 0.7 at friction 1.0" in {
    SectoralMobility.crossSectorWagePenalty(Share(1)).bd shouldBe (BigDecimal("0.7") +- BigDecimal("0.001"))
  }

  it should "decrease monotonically with friction" in {
    for f <- 1 to 10 do
      val low  = SectoralMobility.crossSectorWagePenalty(shareBD(f * BigDecimal("0.1") - BigDecimal("0.1"))).bd
      val high = SectoralMobility.crossSectorWagePenalty(shareBD(f * BigDecimal("0.1"))).bd
      high should be <= low
  }

  // --- frictionAdjustedSuccess ---

  "frictionAdjustedSuccess" should "reduce success probability with friction" in {
    val base = BigDecimal("0.6")
    adjustedSuccess(base, BigDecimal("0.0")).shouldBe(base)
    adjustedSuccess(base, BigDecimal("0.5")).shouldBe((base * BigDecimal("0.75")) +- BigDecimal("0.001"))
    adjustedSuccess(base, BigDecimal("1.0")).shouldBe((base * BigDecimal("0.5")) +- BigDecimal("0.001"))
  }

  // --- SectoralMobility.State ---

  "SectoralMobility.zero" should "have zero fields" in {
    val z = SectoralMobility.zero
    z.crossSectorHires shouldBe 0
    z.voluntaryQuits shouldBe 0
    z.sectorMobilityRate shouldBe Share.Zero
  }

  // --- helpers ---

  private def mkFirms(n: Int): Vector[Firm.State] =
    (0 until n).map { i =>
      mkFirm(i, i % 6, TechState.Traditional(10))
    }.toVector

  private def mkFirm(id: Int, sector: Int, tech: TechState): Firm.State =
    TestFirmState(
      FirmId(id),
      PLN(50000),
      PLN.Zero,
      tech,
      Share.decimal(5, 1),
      Multiplier.One,
      Share.decimal(5, 1),
      SectorIdx(sector),
      Vector.empty[FirmId],
      bankId = BankId(0),
      equityRaised = PLN.Zero,
      initialSize = 10,
      capitalStock = PLN.Zero,
      foreignOwned = false,
      inventory = PLN.Zero,
      greenCapital = PLN.Zero,
      accumulatedLoss = PLN.Zero,
    )

  private def mkHousehold(
      id: Int,
      status: HhStatus,
      skill: BigDecimal = BigDecimal("0.7"),
      healthPenalty: BigDecimal = BigDecimal("0.0"),
  ): Household.State =
    TestHouseholdState(
      HhId(id),
      PLN(20000),
      PLN.Zero,
      PLN(1800),
      shareBD(skill),
      shareBD(healthPenalty),
      Share.decimal(82, 2),
      status,
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
    )
