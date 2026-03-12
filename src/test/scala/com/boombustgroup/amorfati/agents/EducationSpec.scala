package com.boombustgroup.amorfati.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.engine.markets
import com.boombustgroup.amorfati.engine.markets.LaborMarket
import com.boombustgroup.amorfati.types.*

import scala.util.Random

class EducationSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  // ---- Config helpers ----

  "p.social.drawEducation" should "return values in [0, 3]" in {
    val rng = new Random(42)
    for _ <- 0 until 200 do
      val edu = p.social.drawEducation(0, rng)
      edu should be >= 0
      edu should be <= 3
  }

  it should "return valid education for all sectors (0-5)" in {
    val rng = new Random(42)
    for s <- 0 until 6 do
      for _ <- 0 until 50 do
        val edu = p.social.drawEducation(s, rng)
        edu should be >= 0
        edu should be <= 3
  }

  "p.social.drawImmigrantEducation" should "return values in [0, 3]" in {
    val rng = new Random(42)
    for _ <- 0 until 200 do
      val edu = p.social.drawImmigrantEducation(rng)
      edu should be >= 0
      edu should be <= 3
  }

  // ---- Wage premia ----

  "p.social.eduWagePremium" should "be monotonically increasing" in {
    val premia = (0 to 3).map(p.social.eduWagePremium)
    for i <- 0 until 3 do premia(i) should be < premia(i + 1)
  }

  it should "have Secondary = 1.0 (normalization)" in {
    p.social.eduWagePremium(2) shouldBe 1.0
  }

  it should "clamp out-of-range education to valid bounds" in {
    p.social.eduWagePremium(-1) shouldBe p.social.eduWagePremium(0)
    p.social.eduWagePremium(5) shouldBe p.social.eduWagePremium(3)
  }

  // ---- Skill ranges ----

  "p.social.eduSkillRange" should "have floors <= ceilings for all levels" in {
    for edu <- 0 to 3 do
      val (floor, ceil) = p.social.eduSkillRange(edu)
      floor should be <= ceil
  }

  it should "have increasing floors" in {
    val floors = (0 to 3).map(e => p.social.eduSkillRange(e)._1)
    for i <- 0 until 3 do floors(i) should be <= floors(i + 1)
  }

  it should "have increasing ceilings" in {
    val ceilings = (0 to 3).map(e => p.social.eduSkillRange(e)._2)
    for i <- 0 until 3 do ceilings(i) should be <= ceilings(i + 1)
  }

  // ---- Retraining multipliers ----

  "p.social.eduRetrainMultiplier" should "be monotonically increasing" in {
    val mults = (0 to 3).map(p.social.eduRetrainMultiplier)
    for i <- 0 until 3 do mults(i) should be < mults(i + 1)
  }

  it should "have Secondary = 1.0 (normalization)" in {
    p.social.eduRetrainMultiplier(2) shouldBe 1.0
  }

  // ---- Household field ----

  "Household.education" should "default to 2 (Secondary)" in {
    val hh = Household.State(
      HhId(0),
      PLN(1000.0),
      PLN.Zero,
      PLN(1800.0),
      Ratio(0.5),
      Ratio(0.0),
      Ratio(0.85),
      HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(6000.0)),
      Array.empty[HhId],
      bankId = BankId(0),
      equityWealth = PLN.Zero,
      lastSectorIdx = SectorIdx(-1),
      isImmigrant = false,
      numDependentChildren = 0,
      consumerDebt = PLN.Zero,
      education = 2,
    )
    hh.education shouldBe 2
  }

  it should "be preserved through copy" in {
    val hh     = Household.State(
      HhId(0),
      PLN(1000.0),
      PLN.Zero,
      PLN(1800.0),
      Ratio(0.5),
      Ratio(0.0),
      Ratio(0.85),
      HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(6000.0)),
      Array.empty[HhId],
      bankId = BankId(0),
      equityWealth = PLN.Zero,
      lastSectorIdx = SectorIdx(-1),
      isImmigrant = false,
      numDependentChildren = 0,
      consumerDebt = PLN.Zero,
      education = 3,
    )
    val copied = hh.copy(savings = PLN(2000.0))
    copied.education shouldBe 3
  }

  // ---- Education-aware retention ----

  "LaborMarket.separations" should "retain tertiary workers before primary in automated firms" in {
    // Set up: one firm that just automated with skeleton crew = 2
    // Two workers: one primary (edu=0, skill=0.5), one tertiary (edu=3, skill=0.4)
    // Tertiary should be retained despite lower skill (skeleton crew = max(2, 10*0.02) = 2, but need 3 workers to test)
    val prevFirm = Firm.State(
      FirmId(0),
      PLN(1000000.0),
      PLN.Zero,
      TechState.Traditional(3),
      Ratio(0.5),
      1.0,
      Ratio(0.5),
      SectorIdx(0),
      Vector.empty[FirmId],
      bankId = BankId(0),
      equityRaised = PLN.Zero,
      initialSize = 10,
      capitalStock = PLN.Zero,
      bondDebt = PLN.Zero,
      foreignOwned = false,
      inventory = PLN.Zero,
      greenCapital = PLN.Zero,
    )
    val newFirm  = Firm.State(
      FirmId(0),
      PLN(1000000.0),
      PLN.Zero,
      TechState.Automated(2),
      Ratio(0.5),
      1.0,
      Ratio(0.5),
      SectorIdx(0),
      Vector.empty[FirmId],
      bankId = BankId(0),
      equityRaised = PLN.Zero,
      initialSize = 10,
      capitalStock = PLN.Zero,
      bondDebt = PLN.Zero,
      foreignOwned = false,
      inventory = PLN.Zero,
      greenCapital = PLN.Zero,
    )

    val hhPrimary    = Household.State(
      HhId(0),
      PLN(5000.0),
      PLN.Zero,
      PLN(1800.0),
      Ratio(0.5),
      Ratio(0.0),
      Ratio(0.85),
      HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(6000.0)),
      Array.empty[HhId],
      bankId = BankId(0),
      equityWealth = PLN.Zero,
      lastSectorIdx = SectorIdx(-1),
      isImmigrant = false,
      numDependentChildren = 0,
      consumerDebt = PLN.Zero,
      education = 0,
    )
    val hhTertiary   = Household.State(
      HhId(1),
      PLN(5000.0),
      PLN.Zero,
      PLN(1800.0),
      Ratio(0.4),
      Ratio(0.0),
      Ratio(0.85),
      HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(5000.0)),
      Array.empty[HhId],
      bankId = BankId(0),
      equityWealth = PLN.Zero,
      lastSectorIdx = SectorIdx(-1),
      isImmigrant = false,
      numDependentChildren = 0,
      consumerDebt = PLN.Zero,
      education = 3,
    )
    val hhVocational = Household.State(
      HhId(2),
      PLN(5000.0),
      PLN.Zero,
      PLN(1800.0),
      Ratio(0.6),
      Ratio(0.0),
      Ratio(0.85),
      HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(5500.0)),
      Array.empty[HhId],
      bankId = BankId(0),
      equityWealth = PLN.Zero,
      lastSectorIdx = SectorIdx(-1),
      isImmigrant = false,
      numDependentChildren = 0,
      consumerDebt = PLN.Zero,
      education = 1,
    )

    val result = LaborMarket.separations(
      Vector(hhPrimary, hhTertiary, hhVocational),
      Vector(prevFirm),
      Vector(newFirm),
    )

    // Tertiary (id=1) should be retained first, then vocational (id=2). Primary (id=0) fired.
    result(0).status shouldBe a[HhStatus.Unemployed] // primary fired
    result(1).status shouldBe a[HhStatus.Employed]   // tertiary retained
    result(2).status shouldBe a[HhStatus.Employed]   // vocational retained
  }

  it should "use skill as tiebreaker within same education level" in {
    val prevFirm = Firm.State(
      FirmId(0),
      PLN(1000000.0),
      PLN.Zero,
      TechState.Traditional(3),
      Ratio(0.5),
      1.0,
      Ratio(0.5),
      SectorIdx(0),
      Vector.empty[FirmId],
      bankId = BankId(0),
      equityRaised = PLN.Zero,
      initialSize = 10,
      capitalStock = PLN.Zero,
      bondDebt = PLN.Zero,
      foreignOwned = false,
      inventory = PLN.Zero,
      greenCapital = PLN.Zero,
    )
    val newFirm  = Firm.State(
      FirmId(0),
      PLN(1000000.0),
      PLN.Zero,
      TechState.Automated(2),
      Ratio(0.5),
      1.0,
      Ratio(0.5),
      SectorIdx(0),
      Vector.empty[FirmId],
      bankId = BankId(0),
      equityRaised = PLN.Zero,
      initialSize = 10,
      capitalStock = PLN.Zero,
      bondDebt = PLN.Zero,
      foreignOwned = false,
      inventory = PLN.Zero,
      greenCapital = PLN.Zero,
    )

    val hhLowSkill  = Household.State(
      HhId(0),
      PLN(5000.0),
      PLN.Zero,
      PLN(1800.0),
      Ratio(0.3),
      Ratio(0.0),
      Ratio(0.85),
      HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(6000.0)),
      Array.empty[HhId],
      bankId = BankId(0),
      equityWealth = PLN.Zero,
      lastSectorIdx = SectorIdx(-1),
      isImmigrant = false,
      numDependentChildren = 0,
      consumerDebt = PLN.Zero,
      education = 2,
    )
    val hhHighSkill = Household.State(
      HhId(1),
      PLN(5000.0),
      PLN.Zero,
      PLN(1800.0),
      Ratio(0.9),
      Ratio(0.0),
      Ratio(0.85),
      HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(7000.0)),
      Array.empty[HhId],
      bankId = BankId(0),
      equityWealth = PLN.Zero,
      lastSectorIdx = SectorIdx(-1),
      isImmigrant = false,
      numDependentChildren = 0,
      consumerDebt = PLN.Zero,
      education = 2,
    )
    val hhMidSkill  = Household.State(
      HhId(2),
      PLN(5000.0),
      PLN.Zero,
      PLN(1800.0),
      Ratio(0.5),
      Ratio(0.0),
      Ratio(0.85),
      HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(6500.0)),
      Array.empty[HhId],
      bankId = BankId(0),
      equityWealth = PLN.Zero,
      lastSectorIdx = SectorIdx(-1),
      isImmigrant = false,
      numDependentChildren = 0,
      consumerDebt = PLN.Zero,
      education = 2,
    )

    val result = markets.LaborMarket.separations(
      Vector(hhLowSkill, hhHighSkill, hhMidSkill),
      Vector(prevFirm),
      Vector(newFirm),
    )

    result(0).status shouldBe a[HhStatus.Unemployed] // low skill fired
    result(1).status shouldBe a[HhStatus.Employed]   // high skill retained
    result(2).status shouldBe a[HhStatus.Employed]   // mid skill retained
  }

  // ---- Immigrant education ----

  "Immigration.spawnImmigrants" should "assign education levels to immigrants" in {
    val rng        = new Random(42)
    val immigrants = Immigration.spawnImmigrants(100, 0, rng)
    immigrants.foreach { h =>
      h.education should be >= 0
      h.education should be <= 3
    }
  }

  it should "clamp immigrant skill within education-specific range" in {
    val rng        = new Random(42)
    val immigrants = Immigration.spawnImmigrants(200, 0, rng)
    immigrants.foreach { h =>
      val (floor, ceil) = p.social.eduSkillRange(h.education)
      h.skill.toDouble should be >= floor
      h.skill.toDouble should be <= ceil
    }
  }

  // ---- HouseholdInit education ----

  "Household.Init.initialize" should "assign education to all households" in {
    val rng       = new Random(42)
    val mkF       = (id: Int, sec: Int, w: Int) =>
      Firm.State(
        FirmId(id),
        PLN(1000000.0),
        PLN.Zero,
        TechState.Traditional(w),
        Ratio(0.5),
        1.0,
        Ratio(0.5),
        SectorIdx(sec),
        Vector.empty[FirmId],
        bankId = BankId(0),
        equityRaised = PLN.Zero,
        initialSize = w,
        capitalStock = PLN.Zero,
        bondDebt = PLN.Zero,
        foreignOwned = false,
        inventory = PLN.Zero,
        greenCapital = PLN.Zero,
      )
    val firms     = Vector(mkF(0, 0, 5), mkF(1, 2, 5))
    val socialNet = Array.fill(10)(Array.empty[Int])
    val hhs       = Household.Init.initialize(10, firms, socialNet, rng)
    hhs.foreach { h =>
      h.education should be >= 0
      h.education should be <= 3
    }
  }

  it should "clamp skill within education-specific range" in {
    val rng       = new Random(42)
    val firms     = Vector(
      Firm.State(
        FirmId(0),
        PLN(1000000.0),
        PLN.Zero,
        TechState.Traditional(10),
        Ratio(0.5),
        1.0,
        Ratio(0.5),
        SectorIdx(0),
        Vector.empty[FirmId],
        bankId = BankId(0),
        equityRaised = PLN.Zero,
        initialSize = 10,
        capitalStock = PLN.Zero,
        bondDebt = PLN.Zero,
        foreignOwned = false,
        inventory = PLN.Zero,
        greenCapital = PLN.Zero,
      ),
    )
    val socialNet = Array.fill(10)(Array.empty[Int])
    val hhs       = Household.Init.initialize(10, firms, socialNet, rng)
    hhs.foreach { h =>
      val (floor, ceil) = p.social.eduSkillRange(h.education)
      h.skill.toDouble should be >= floor
      h.skill.toDouble should be <= ceil
    }
  }
