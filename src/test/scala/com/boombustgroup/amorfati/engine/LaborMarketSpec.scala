package com.boombustgroup.amorfati.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.engine.markets.LaborMarket
import com.boombustgroup.amorfati.types.*

import scala.util.Random

class LaborMarketSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams = SimParams.defaults
  private val td  = ComputationBoundary

  // --- separations ---

  "LaborMarket.separations" should "not change households when no firms changed" in {
    val firms  = mkFirms(5)
    val hhs    =
      (0 until 10).map(i => mkHousehold(i, HhStatus.Employed(FirmId(i % 5), SectorIdx(2), PLN(8000.0)))).toVector
    val result = LaborMarket.separations(hhs, firms, firms)
    result.foreach(_.status shouldBe a[HhStatus.Employed])
  }

  it should "make workers unemployed when firm goes bankrupt" in {
    val prevFirms = mkFirms(3)
    val newFirms  = prevFirms.updated(1, prevFirms(1).copy(tech = TechState.Bankrupt(BankruptReason.Other("test"))))

    val hhs    = Vector(
      mkHousehold(0, HhStatus.Employed(FirmId(0), SectorIdx(2), PLN(8000.0))),
      mkHousehold(1, HhStatus.Employed(FirmId(1), SectorIdx(2), PLN(8000.0))),
      mkHousehold(2, HhStatus.Employed(FirmId(2), SectorIdx(2), PLN(8000.0))),
    )
    val result = LaborMarket.separations(hhs, prevFirms, newFirms)
    result(0).status shouldBe a[HhStatus.Employed]
    result(1).status shouldBe HhStatus.Unemployed(0)
    result(2).status shouldBe a[HhStatus.Employed]
  }

  it should "make workers unemployed when firm automates" in {
    val prevFirms = mkFirms(2)
    val newFirms  = prevFirms.updated(0, prevFirms(0).copy(tech = TechState.Automated(1.5)))

    val hhs        = (0 until 5).map(i => mkHousehold(i, HhStatus.Employed(FirmId(0), SectorIdx(2), PLN(8000.0)))).toVector
    val result     = LaborMarket.separations(hhs, prevFirms, newFirms)
    // Automated firms keep skeletonCrew workers, rest become unemployed
    val skCrew     = Firm.skeletonCrew(newFirms(0))
    val employed   = result.count(_.status.isInstanceOf[HhStatus.Employed])
    val unemployed = result.count(_.status == HhStatus.Unemployed(0))
    employed shouldBe skCrew
    unemployed shouldBe (5 - skCrew)
  }

  it should "not affect already unemployed households" in {
    val prevFirms = mkFirms(2)
    val newFirms  = prevFirms.updated(0, prevFirms(0).copy(tech = TechState.Bankrupt(BankruptReason.Other("test"))))

    val hhs    = Vector(
      mkHousehold(0, HhStatus.Employed(FirmId(0), SectorIdx(2), PLN(8000.0))),
      mkHousehold(1, HhStatus.Unemployed(5)),
      mkHousehold(2, HhStatus.Employed(FirmId(1), SectorIdx(2), PLN(8000.0))),
    )
    val result = LaborMarket.separations(hhs, prevFirms, newFirms)
    result(1).status shouldBe HhStatus.Unemployed(5)
  }

  // --- jobSearch ---

  "LaborMarket.jobSearch" should "employ unemployed when vacancies exist" in {
    val rng    = new Random(42)
    val firms  = mkFirms(3)
    val hhs    = Vector(
      mkHousehold(0, HhStatus.Employed(FirmId(0), SectorIdx(2), PLN(8000.0))),
      mkHousehold(1, HhStatus.Unemployed(2), skill = 0.8),
      mkHousehold(2, HhStatus.Employed(FirmId(2), SectorIdx(2), PLN(8000.0))),
    )
    // Firm 1 has 10 workers (Traditional) but only hh(0) is assigned to firm 0, etc.
    // With 3 firms x 10 workers each = 30 needed, only 2 employed → 28 vacancies
    val result = LaborMarket.jobSearch(hhs, firms, PLN(8000.0), rng).households
    result(1).status shouldBe a[HhStatus.Employed]
  }

  it should "prefer higher-skilled workers" in {
    val rng    = new Random(42)
    val firms  = Vector(mkFirms(1)(0))
    // Only 1 vacancy: Traditional(10) needs 10, but we have 11 workers
    val hhs    = Vector(
      mkHousehold(0, HhStatus.Employed(FirmId(0), SectorIdx(2), PLN(8000.0))),
      mkHousehold(1, HhStatus.Employed(FirmId(0), SectorIdx(2), PLN(8000.0))),
      mkHousehold(2, HhStatus.Employed(FirmId(0), SectorIdx(2), PLN(8000.0))),
      mkHousehold(3, HhStatus.Employed(FirmId(0), SectorIdx(2), PLN(8000.0))),
      mkHousehold(4, HhStatus.Employed(FirmId(0), SectorIdx(2), PLN(8000.0))),
      mkHousehold(5, HhStatus.Employed(FirmId(0), SectorIdx(2), PLN(8000.0))),
      mkHousehold(6, HhStatus.Employed(FirmId(0), SectorIdx(2), PLN(8000.0))),
      mkHousehold(7, HhStatus.Employed(FirmId(0), SectorIdx(2), PLN(8000.0))),
      mkHousehold(8, HhStatus.Employed(FirmId(0), SectorIdx(2), PLN(8000.0))),
      mkHousehold(9, HhStatus.Unemployed(1), skill = 0.9),
      mkHousehold(10, HhStatus.Unemployed(1), skill = 0.3),
    )
    // Firm needs 10 workers, has 9 employed → 1 vacancy
    val result = LaborMarket.jobSearch(hhs, firms, PLN(8000.0), rng).households
    // Higher skilled (id=9, skill=0.9) should get the job
    result(9).status shouldBe a[HhStatus.Employed]
  }

  // --- updateWages ---

  "LaborMarket.updateWages" should "produce mean wage = marketWage for employed" in {
    // With normalization, mean employed wage = marketWage regardless of sector/skill
    val hhs    = Vector(
      mkHousehold(0, HhStatus.Employed(FirmId(0), SectorIdx(2), PLN(5000.0)), skill = 0.8),
      mkHousehold(1, HhStatus.Unemployed(3)),
    )
    val result = LaborMarket.updateWages(hhs, mkFirms(hhs.length), PLN(10000.0))
    result(0).status match
      case HhStatus.Employed(_, _, wage) =>
        // Single employed: normalized to marketWage
        td.toDouble(wage) shouldBe 10000.0 +- 1.0
      case other                         => fail(s"Expected Employed, got $other")
    result(1).status shouldBe HhStatus.Unemployed(3)
  }

  it should "reduce relative wage with health penalty" in {
    // Two employed: one with penalty, one without. Penalty one gets less.
    val hhs    = Vector(
      mkHousehold(0, HhStatus.Employed(FirmId(0), SectorIdx(2), PLN(5000.0)), skill = 0.8, healthPenalty = 0.0),
      mkHousehold(1, HhStatus.Employed(FirmId(1), SectorIdx(2), PLN(5000.0)), skill = 0.8, healthPenalty = 0.2),
    )
    val result = LaborMarket.updateWages(hhs, mkFirms(hhs.length), PLN(10000.0))
    val wage0  = result(0).status.asInstanceOf[HhStatus.Employed].wage
    val wage1  = result(1).status.asInstanceOf[HhStatus.Employed].wage
    // wage1 should be less than wage0 (health penalty reduces relative wage)
    td.toDouble(wage1) should be < td.toDouble(wage0)
    // Mean should be marketWage
    td.toDouble(wage0 + wage1) / 2.0 shouldBe 10000.0 +- 1.0
  }

  // --- immigrant wage discount ---

  // --- helpers ---

  private def mkFirms(n: Int): Vector[Firm.State] =
    (0 until n).map { i =>
      Firm.State(
        FirmId(i),
        PLN(50000.0),
        PLN.Zero,
        TechState.Traditional(10),
        Share(0.5),
        1.0,
        Share(0.5),
        SectorIdx(2),
        Vector.empty[FirmId],
        bankId = BankId(0),
        equityRaised = PLN.Zero,
        initialSize = 10,
        capitalStock = PLN.Zero,
        bondDebt = PLN.Zero,
        foreignOwned = false,
        inventory = PLN.Zero,
        greenCapital = PLN.Zero,
        accumulatedLoss = PLN.Zero,
      ) // sector 2 = Retail/Services
    }.toVector

  private def mkHousehold(
      id: Int,
      status: HhStatus,
      skill: Double = 0.7,
      healthPenalty: Double = 0.0,
  ): Household.State =
    Household.State(
      HhId(id),
      PLN(20000.0),
      PLN.Zero,
      PLN(1800.0),
      Share(skill),
      Share(healthPenalty),
      Share(0.82),
      status,
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
    )
