package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.agents.*
import sfc.config.{Config, SECTORS}
import sfc.types.*

import scala.util.Random

class SectoralMobilitySpec extends AnyFlatSpec with Matchers:

  // --- Default friction matrix ---

  "DefaultFrictionMatrix" should "be 6x6" in {
    SectoralMobility.DefaultFrictionMatrix.length shouldBe 6
    SectoralMobility.DefaultFrictionMatrix.foreach(_.length shouldBe 6)
  }

  it should "have zero diagonal" in {
    for i <- 0 until 6 do SectoralMobility.DefaultFrictionMatrix(i)(i) shouldBe 0.0
  }

  it should "be symmetric" in {
    val m = SectoralMobility.DefaultFrictionMatrix
    for i <- 0 until 6; j <- 0 until 6 do m(i)(j) shouldBe m(j)(i)
  }

  it should "have values in [0, 1]" in {
    for row <- SectoralMobility.DefaultFrictionMatrix; v <- row do
      v should be >= 0.0
      v should be <= 1.0
  }

  // --- sectorVacancies ---

  "sectorVacancies" should "return non-negative vacancies per sector" in {
    val firms = mkFirms(6)
    val hhs =
      (0 until 5).map(i => mkHousehold(i, HhStatus.Employed(FirmId(i % 6), SectorIdx(i % 6), PLN(8000.0)))).toVector
    val vac = SectoralMobility.sectorVacancies(hhs, firms)
    vac.length shouldBe 6
    vac.foreach(_ should be >= 0)
  }

  it should "show vacancies when firms need more workers than employed" in {
    val firms = Array(mkFirm(0, 2, TechState.Traditional(10))) // needs 10
    val hhs = Vector(mkHousehold(0, HhStatus.Employed(FirmId(0), SectorIdx(2), PLN(8000.0)))) // 1 employed
    val vac = SectoralMobility.sectorVacancies(hhs, firms)
    vac(2) shouldBe 9 // 10 needed - 1 employed
  }

  // --- sectorWages ---

  "sectorWages" should "compute average wage per sector" in {
    val hhs = Vector(
      mkHousehold(0, HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(10000.0))),
      mkHousehold(1, HhStatus.Employed(FirmId(1), SectorIdx(0), PLN(12000.0))),
      mkHousehold(2, HhStatus.Employed(FirmId(2), SectorIdx(2), PLN(8000.0))),
      mkHousehold(3, HhStatus.Unemployed(1)),
    )
    val wages = SectoralMobility.sectorWages(hhs)
    wages(0) shouldBe 11000.0 +- 0.01 // (10000+12000)/2
    wages(2) shouldBe 8000.0
    wages(1) shouldBe 0.0 // no employed in sector 1
  }

  // --- selectTargetSector ---

  "selectTargetSector" should "not select the source sector" in {
    val rng = new Random(42)
    val wages = Array(10000.0, 12000.0, 8000.0, 15000.0, 9000.0, 7000.0)
    val vac = Array(5, 10, 3, 8, 2, 1)
    for _ <- 0 until 100 do
      val target = SectoralMobility.selectTargetSector(0, wages, vac, SectoralMobility.DefaultFrictionMatrix, 2.0, rng)
      target should not be 0
  }

  it should "prefer high-wage high-vacancy low-friction sectors" in {
    val rng = new Random(42)
    // Sector 1 (Mfg): high wage, high vacancies
    // Sector 5 (Agr): high friction from BPO (0.9)
    val wages = Array(0.0, 20000.0, 5000.0, 5000.0, 5000.0, 5000.0)
    val vac = Array(0, 100, 1, 1, 1, 1)
    val counts = new Array[Int](6)
    for _ <- 0 until 1000 do
      val target = SectoralMobility.selectTargetSector(0, wages, vac, SectoralMobility.DefaultFrictionMatrix, 2.0, rng)
      counts(target) += 1
    // Sector 1 should be heavily preferred
    counts(1) should be > 500
  }

  it should "handle all-zero wages gracefully" in {
    val rng = new Random(42)
    val wages = Array.fill(6)(0.0)
    val vac = Array.fill(6)(0)
    val target = SectoralMobility.selectTargetSector(0, wages, vac, SectoralMobility.DefaultFrictionMatrix, 2.0, rng)
    target should not be 0
    target should be >= 0
    target should be < 6
  }

  // --- frictionAdjustedParams ---

  "frictionAdjustedParams" should "increase duration and cost with higher friction" in {
    val (dur0, cost0) = SectoralMobility.frictionAdjustedParams(0.0, 1.0, 0.5)
    val (dur9, cost9) = SectoralMobility.frictionAdjustedParams(0.9, 1.0, 0.5)
    dur9 should be > dur0
    cost9 should be > cost0
  }

  it should "return base values at zero friction" in {
    val (dur, cost) = SectoralMobility.frictionAdjustedParams(0.0, 1.0, 0.5)
    dur shouldBe Config.HhRetrainingDuration
    cost shouldBe Config.HhRetrainingCost +- 0.01
  }

  // --- crossSectorWagePenalty ---

  "crossSectorWagePenalty" should "return 1.0 at zero friction" in {
    SectoralMobility.crossSectorWagePenalty(0.0) shouldBe 1.0
  }

  it should "return 0.7 at friction 1.0" in {
    SectoralMobility.crossSectorWagePenalty(1.0) shouldBe 0.7 +- 0.001
  }

  it should "decrease monotonically with friction" in {
    for f <- 1 to 10 do
      val low = SectoralMobility.crossSectorWagePenalty(f * 0.1 - 0.1)
      val high = SectoralMobility.crossSectorWagePenalty(f * 0.1)
      high should be <= low
  }

  // --- frictionAdjustedSuccess ---

  "frictionAdjustedSuccess" should "reduce success probability with friction" in {
    val base = 0.6
    SectoralMobility.frictionAdjustedSuccess(base, 0.0) shouldBe base
    SectoralMobility.frictionAdjustedSuccess(base, 0.5) shouldBe (base * 0.75) +- 0.001
    SectoralMobility.frictionAdjustedSuccess(base, 1.0) shouldBe (base * 0.5) +- 0.001
  }

  // --- SectoralMobility.State ---

  "SectoralMobility.zero" should "have zero fields" in {
    val z = SectoralMobility.zero
    z.crossSectorHires shouldBe 0
    z.voluntaryQuits shouldBe 0
    z.sectorMobilityRate shouldBe 0.0
  }

  // --- helpers ---

  private def mkFirms(n: Int): Array[Firm.State] =
    (0 until n).map { i =>
      mkFirm(i, i % 6, TechState.Traditional(10))
    }.toArray

  private def mkFirm(id: Int, sector: Int, tech: TechState): Firm.State =
    Firm.State(
      FirmId(id),
      PLN(50000.0),
      PLN.Zero,
      tech,
      Ratio(0.5),
      1.0,
      Ratio(0.5),
      SectorIdx(sector),
      Array.empty[Int],
    )

  private def mkHousehold(
    id: Int,
    status: HhStatus,
    skill: Double = 0.7,
    healthPenalty: Double = 0.0,
  ): Household.State =
    Household.State(
      id,
      PLN(20000.0),
      PLN.Zero,
      PLN(1800.0),
      Ratio(skill),
      Ratio(healthPenalty),
      Ratio(0.82),
      status,
      Array.empty[Int],
    )
