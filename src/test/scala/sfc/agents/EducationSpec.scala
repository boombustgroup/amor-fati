package sfc.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.types.*

import scala.util.Random

class EducationSpec extends AnyFlatSpec with Matchers:

  // ---- Config helpers ----

  "Config.drawEducation" should "return values in [0, 3]" in {
    val rng = new Random(42)
    for _ <- 0 until 200 do
      val edu = sfc.config.Config.drawEducation(0, rng)
      edu should be >= 0
      edu should be <= 3
  }

  it should "return valid education for all sectors (0-5)" in {
    val rng = new Random(42)
    for s <- 0 until 6 do
      for _ <- 0 until 50 do
        val edu = sfc.config.Config.drawEducation(s, rng)
        edu should be >= 0
        edu should be <= 3
  }

  "Config.drawImmigrantEducation" should "return values in [0, 3]" in {
    val rng = new Random(42)
    for _ <- 0 until 200 do
      val edu = sfc.config.Config.drawImmigrantEducation(rng)
      edu should be >= 0
      edu should be <= 3
  }

  // ---- Wage premia ----

  "Config.eduWagePremium" should "be monotonically increasing" in {
    val premia = (0 to 3).map(sfc.config.Config.eduWagePremium)
    for i <- 0 until 3 do
      premia(i) should be < premia(i + 1)
  }

  it should "have Secondary = 1.0 (normalization)" in {
    sfc.config.Config.eduWagePremium(2) shouldBe 1.0
  }

  it should "clamp out-of-range education to valid bounds" in {
    sfc.config.Config.eduWagePremium(-1) shouldBe sfc.config.Config.eduWagePremium(0)
    sfc.config.Config.eduWagePremium(5) shouldBe sfc.config.Config.eduWagePremium(3)
  }

  // ---- Skill ranges ----

  "Config.eduSkillRange" should "have floors <= ceilings for all levels" in {
    for edu <- 0 to 3 do
      val (floor, ceil) = sfc.config.Config.eduSkillRange(edu)
      floor should be <= ceil
  }

  it should "have increasing floors" in {
    val floors = (0 to 3).map(e => sfc.config.Config.eduSkillRange(e)._1)
    for i <- 0 until 3 do
      floors(i) should be <= floors(i + 1)
  }

  it should "have increasing ceilings" in {
    val ceilings = (0 to 3).map(e => sfc.config.Config.eduSkillRange(e)._2)
    for i <- 0 until 3 do
      ceilings(i) should be <= ceilings(i + 1)
  }

  // ---- Retraining multipliers ----

  "Config.eduRetrainMultiplier" should "be monotonically increasing" in {
    val mults = (0 to 3).map(sfc.config.Config.eduRetrainMultiplier)
    for i <- 0 until 3 do
      mults(i) should be < mults(i + 1)
  }

  it should "have Secondary = 1.0 (normalization)" in {
    sfc.config.Config.eduRetrainMultiplier(2) shouldBe 1.0
  }

  // ---- Household field ----

  "Household.education" should "default to 2 (Secondary)" in {
    val hh = Household(0, 1000.0, 0.0, 1800.0, 0.5, 0.0, 0.85,
      HhStatus.Employed(FirmId(0), SectorIdx(0), 6000.0), Array.empty[Int])
    hh.education shouldBe 2
  }

  it should "be preserved through copy" in {
    val hh = Household(0, 1000.0, 0.0, 1800.0, 0.5, 0.0, 0.85,
      HhStatus.Employed(FirmId(0), SectorIdx(0), 6000.0), Array.empty[Int], education = 3)
    val copied = hh.copy(savings = 2000.0)
    copied.education shouldBe 3
  }

  // ---- Education-aware retention ----

  "LaborMarket.separations" should "retain tertiary workers before primary in automated firms" in {
    // Set up: one firm that just automated with skeleton crew = 2
    // Two workers: one primary (edu=0, skill=0.5), one tertiary (edu=3, skill=0.4)
    // Tertiary should be retained despite lower skill (skeleton crew = max(2, 10*0.02) = 2, but need 3 workers to test)
    val prevFirm = Firm(FirmId(0), 1000000.0, 0.0,
      TechState.Traditional(3), 0.5, 1.0, 0.5, SectorIdx(0), Array.empty[Int])
    val newFirm = Firm(FirmId(0), 1000000.0, 0.0,
      TechState.Automated(2), 0.5, 1.0, 0.5, SectorIdx(0), Array.empty[Int])

    val hhPrimary = Household(0, 5000.0, 0, 1800.0, 0.5, 0.0, 0.85,
      HhStatus.Employed(FirmId(0), SectorIdx(0), 6000.0), Array.empty[Int], education = 0)
    val hhTertiary = Household(1, 5000.0, 0, 1800.0, 0.4, 0.0, 0.85,
      HhStatus.Employed(FirmId(0), SectorIdx(0), 5000.0), Array.empty[Int], education = 3)
    val hhVocational = Household(2, 5000.0, 0, 1800.0, 0.6, 0.0, 0.85,
      HhStatus.Employed(FirmId(0), SectorIdx(0), 5500.0), Array.empty[Int], education = 1)

    val result = sfc.engine.LaborMarket.separations(
      Vector(hhPrimary, hhTertiary, hhVocational),
      Array(prevFirm),
      Array(newFirm)
    )

    // Tertiary (id=1) should be retained first, then vocational (id=2). Primary (id=0) fired.
    result(0).status shouldBe a[HhStatus.Unemployed]  // primary fired
    result(1).status shouldBe a[HhStatus.Employed]    // tertiary retained
    result(2).status shouldBe a[HhStatus.Employed]    // vocational retained
  }

  it should "use skill as tiebreaker within same education level" in {
    val prevFirm = Firm(FirmId(0), 1000000.0, 0.0,
      TechState.Traditional(3), 0.5, 1.0, 0.5, SectorIdx(0), Array.empty[Int])
    val newFirm = Firm(FirmId(0), 1000000.0, 0.0,
      TechState.Automated(2), 0.5, 1.0, 0.5, SectorIdx(0), Array.empty[Int])

    val hhLowSkill = Household(0, 5000.0, 0, 1800.0, 0.3, 0.0, 0.85,
      HhStatus.Employed(FirmId(0), SectorIdx(0), 6000.0), Array.empty[Int], education = 2)
    val hhHighSkill = Household(1, 5000.0, 0, 1800.0, 0.9, 0.0, 0.85,
      HhStatus.Employed(FirmId(0), SectorIdx(0), 7000.0), Array.empty[Int], education = 2)
    val hhMidSkill = Household(2, 5000.0, 0, 1800.0, 0.5, 0.0, 0.85,
      HhStatus.Employed(FirmId(0), SectorIdx(0), 6500.0), Array.empty[Int], education = 2)

    val result = sfc.engine.LaborMarket.separations(
      Vector(hhLowSkill, hhHighSkill, hhMidSkill),
      Array(prevFirm),
      Array(newFirm)
    )

    result(0).status shouldBe a[HhStatus.Unemployed]  // low skill fired
    result(1).status shouldBe a[HhStatus.Employed]    // high skill retained
    result(2).status shouldBe a[HhStatus.Employed]    // mid skill retained
  }

  // ---- Immigrant education ----

  "ImmigrationLogic.spawnImmigrants" should "assign education levels to immigrants" in {
    val rng = new Random(42)
    val immigrants = ImmigrationLogic.spawnImmigrants(100, 0, rng)
    immigrants.foreach { h =>
      h.education should be >= 0
      h.education should be <= 3
    }
  }

  it should "clamp immigrant skill within education-specific range" in {
    val rng = new Random(42)
    val immigrants = ImmigrationLogic.spawnImmigrants(200, 0, rng)
    immigrants.foreach { h =>
      val (floor, ceil) = sfc.config.Config.eduSkillRange(h.education)
      h.skill should be >= floor
      h.skill should be <= ceil
    }
  }

  // ---- HouseholdInit education ----

  "HouseholdInit.initialize" should "assign education to all households" in {
    val rng = new Random(42)
    val firms = Array(
      Firm(FirmId(0), 1000000.0, 0.0, TechState.Traditional(5), 0.5, 1.0, 0.5, SectorIdx(0), Array.empty[Int]),
      Firm(FirmId(1), 1000000.0, 0.0, TechState.Traditional(5), 0.5, 1.0, 0.5, SectorIdx(2), Array.empty[Int])
    )
    val socialNet = Array.fill(10)(Array.empty[Int])
    val hhs = HouseholdInit.initialize(10, 2, firms, socialNet, rng)
    hhs.foreach { h =>
      h.education should be >= 0
      h.education should be <= 3
    }
  }

  it should "clamp skill within education-specific range" in {
    val rng = new Random(42)
    val firms = Array(
      Firm(FirmId(0), 1000000.0, 0.0, TechState.Traditional(10), 0.5, 1.0, 0.5, SectorIdx(0), Array.empty[Int])
    )
    val socialNet = Array.fill(10)(Array.empty[Int])
    val hhs = HouseholdInit.initialize(10, 1, firms, socialNet, rng)
    hhs.foreach { h =>
      val (floor, ceil) = sfc.config.Config.eduSkillRange(h.education)
      h.skill should be >= floor
      h.skill should be <= ceil
    }
  }
