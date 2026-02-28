package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen
import sfc.agents.*

import scala.util.Random

class SectoralMobilityPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  // --- Friction matrix properties ---

  "DefaultFrictionMatrix" should "have all off-diagonal elements in (0, 1)" in {
    val m = SectoralMobility.DefaultFrictionMatrix
    for i <- 0 until 6; j <- 0 until 6 if i != j do
      m(i)(j) should be > 0.0
      m(i)(j) should be < 1.0
  }

  // --- crossSectorWagePenalty ---

  "crossSectorWagePenalty" should "be in [0.7, 1.0] for friction in [0, 1]" in {
    forAll(Gen.choose(0.0, 1.0)) { friction =>
      val p = SectoralMobility.crossSectorWagePenalty(friction)
      p should be >= 0.7
      p should be <= 1.0
    }
  }

  // --- frictionAdjustedSuccess ---

  "frictionAdjustedSuccess" should "be in [0, base] for friction in [0, 1]" in {
    forAll(Gen.choose(0.0, 1.0), Gen.choose(0.0, 1.0)) { (base, friction) =>
      val s = SectoralMobility.frictionAdjustedSuccess(base, friction)
      s should be >= 0.0
      s should be <= base + 1e-10
    }
  }

  // --- selectTargetSector ---

  "selectTargetSector" should "always return a valid sector != from" in {
    forAll(Gen.choose(0, 5)) { from =>
      val rng = new Random(42)
      val wages = Array.fill(6)(10000.0)
      val vac = Array.fill(6)(5)
      val target = SectoralMobility.selectTargetSector(
        from, wages, vac, SectoralMobility.DefaultFrictionMatrix, 2.0, rng)
      target should not be from
      target should be >= 0
      target should be < 6
    }
  }

  // --- sectorVacancies ---

  "sectorVacancies" should "return array of length 6" in {
    val firms = Array(Firm(0, 50000.0, 0.0, TechState.Traditional(10), 0.5, 1.0, 0.5, 2, Array.empty))
    val hhs = Vector.empty[Household]
    val vac = SectoralMobility.sectorVacancies(hhs, firms)
    vac.length shouldBe 6
  }

  // --- sectorWages ---

  "sectorWages" should "return non-negative values" in {
    val hhs = (0 until 10).map(i =>
      Household(i, 20000.0, 0.0, 1800.0, 0.7, 0.0, 0.82,
        HhStatus.Employed(i, i % 6, 8000.0 + i * 100), Array.empty)
    ).toVector
    val wages = SectoralMobility.sectorWages(hhs)
    wages.foreach(_ should be >= 0.0)
  }

  // --- frictionAdjustedParams ---

  "frictionAdjustedParams" should "increase monotonically with friction" in {
    forAll(Gen.choose(0.0, 0.99), Gen.choose(0.01, 2.0), Gen.choose(0.01, 2.0)) {
      (friction, durMult, costMult) =>
        val (dur1, cost1) = SectoralMobility.frictionAdjustedParams(friction, durMult, costMult)
        val (dur2, cost2) = SectoralMobility.frictionAdjustedParams(friction + 0.01, durMult, costMult)
        dur2 should be >= dur1
        cost2 should be >= cost1
    }
  }
