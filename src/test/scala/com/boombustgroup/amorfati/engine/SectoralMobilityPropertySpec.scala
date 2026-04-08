package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.mechanisms.SectoralMobility
import com.boombustgroup.amorfati.types.*
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.util.Random

class SectoralMobilityPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given SimParams = SimParams.defaults

  private def adjustedSuccess(base: Double, friction: Double): Double =
    ComputationBoundary.toDouble(SectoralMobility.frictionAdjustedSuccess(Share(base), Share(friction)))
  // --- Friction matrix properties ---

  "DefaultFrictionMatrix" should "have all off-diagonal elements in (0, 1)" in {
    val m = SectoralMobility.DefaultFrictionMatrix
    for i <- 0 until 6; j <- 0 until 6 if i != j do
      m(i)(j).bd should be > BigDecimal(0)
      m(i)(j).bd should be < BigDecimal("1.0")
  }

  // --- crossSectorWagePenalty ---

  "crossSectorWagePenalty" should "be in [0.7, 1.0] for friction in [0, 1]" in
    forAll(Gen.choose(0.0, 1.0)) { friction =>
      val p = SectoralMobility.crossSectorWagePenalty(Share(friction)).bd
      p should be >= BigDecimal("0.7")
      p should be <= BigDecimal("1.0")
    }

  // --- frictionAdjustedSuccess ---

  "frictionAdjustedSuccess" should "be in [0, base] for friction in [0, 1]" in
    forAll(Gen.choose(0.0, 1.0), Gen.choose(0.0, 1.0)) { (base, friction) =>
      val s = adjustedSuccess(base, friction)
      s should be >= 0.0
      s should be <= base + 1e-10
    }

  // --- selectTargetSector ---

  "selectTargetSector" should "always return a valid sector != from" in
    forAll(Gen.choose(0, 5)) { from =>
      val rng    = new Random(42)
      val wages  = Vector.fill(6)(PLN(10000.0))
      val vac    = Vector.fill(6)(5)
      val target =
        SectoralMobility.selectTargetSector(from, wages, vac, SectoralMobility.DefaultFrictionMatrix, Coefficient(2.0), rng)
      target should not be from
      target should be >= 0
      target should be < 6
    }

  // --- sectorVacancies ---

  "sectorVacancies" should "return array of length 6" in {
    val firms = Vector(
      Firm.State(
        FirmId(0),
        PLN(50000.0),
        PLN.Zero,
        TechState.Traditional(10),
        Share(0.5),
        Multiplier.One,
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
      ),
    )
    val hhs   = Vector.empty[Household.State]
    val vac   = SectoralMobility.sectorVacancies(hhs, firms)
    vac.length shouldBe 6
  }

  // --- sectorWages ---

  "sectorWages" should "return non-negative values" in {
    val hhs   = (0 until 10)
      .map(i =>
        Household.State(
          HhId(i),
          PLN(20000.0),
          PLN.Zero,
          PLN(1800.0),
          Share(0.7),
          Share(0.0),
          Share(0.82),
          HhStatus.Employed(FirmId(i), SectorIdx(i % 6), PLN(8000.0 + i * 100)),
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
      .toVector
    val wages = SectoralMobility.sectorWages(hhs)
    wages.foreach(w => w.bd should be >= BigDecimal(0))
  }

  // --- frictionAdjustedParams ---

  "frictionAdjustedParams" should "increase monotonically with friction" in
    forAll(Gen.choose(0.0, 0.99), Gen.choose(0.01, 2.0), Gen.choose(0.01, 2.0)) { (friction, durMult, costMult) =>
      val rp1 = SectoralMobility.frictionAdjustedParams(Share(friction), Multiplier(durMult), Share(costMult))
      val rp2 = SectoralMobility.frictionAdjustedParams(Share(friction + 0.01), Multiplier(durMult), Share(costMult))
      rp2.duration should be >= rp1.duration
      rp2.cost.bd should be >= rp1.cost.bd
    }
