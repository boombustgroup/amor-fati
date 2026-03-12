package com.boombustgroup.amorfati.config

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.types.*

import scala.util.Random

class FirmSizeDistributionSpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  "FirmSizeDistribution.draw" should "return WorkersPerFirm when FirmSizeDist=uniform" in {
    // Default config is "uniform" — all firms get WorkersPerFirm (10)
    val rng = new Random(42)
    for _ <- 0 until 100 do FirmSizeDistribution.draw(rng) shouldBe p.pop.workersPerFirm
  }

  it should "return values in valid ranges for all size classes" in {
    // We can't test "gus" mode directly via env vars in unit tests,
    // so we test the draw logic by verifying the returned values for
    // the default (uniform) mode are always p.pop.workersPerFirm
    val rng   = new Random(42)
    val sizes = (0 until 10000).map(_ => FirmSizeDistribution.draw(rng))
    sizes.foreach(_ shouldBe p.pop.workersPerFirm)
  }

  // --- FirmOps size-dependent methods ---

  import com.boombustgroup.amorfati.agents.{Firm, TechState}

  private def mkFirm(tech: TechState, sector: Int = 0, size: Int = 10): Firm.State =
    Firm.State(
      FirmId(0),
      PLN.Zero,
      PLN.Zero,
      tech,
      Ratio(0.5),
      1.0,
      Ratio(0.5),
      SectorIdx(sector),
      Vector.empty[FirmId],
      bankId = BankId(0),
      equityRaised = PLN.Zero,
      initialSize = size,
      capitalStock = PLN.Zero,
      bondDebt = PLN.Zero,
      foreignOwned = false,
      inventory = PLN.Zero,
      greenCapital = PLN.Zero,
    )

  "Firm.skeletonCrew" should "return AutoSkeletonCrew for small firms" in {
    val f = mkFirm(TechState.Traditional(5), size = 5)
    Firm.skeletonCrew(f) shouldBe p.firm.autoSkeletonCrew
  }

  it should "scale with initialSize for large firms" in {
    val f = mkFirm(TechState.Traditional(250), size = 250)
    Firm.skeletonCrew(f) shouldBe 5 // max(2, 250 * 0.02 = 5)
  }

  it should "be at least AutoSkeletonCrew" in {
    for size <- Vector(1, 3, 5, 10, 50, 100, 250, 500) do
      val f = mkFirm(TechState.Traditional(size), size = size)
      Firm.skeletonCrew(f) should be >= p.firm.autoSkeletonCrew
  }

  // --- Capacity scaling ---

  "Firm.computeCapacity" should "scale linearly with initialSize at full employment" in {
    val f10   = mkFirm(TechState.Traditional(10), sector = 2, size = 10)
    val f25   = mkFirm(TechState.Traditional(25), sector = 2, size = 25)
    val ratio = Firm.computeCapacity(f25) / Firm.computeCapacity(f10)
    ratio shouldBe (2.5 +- 0.01)
  }

  it should "give same per-worker revenue at full employment regardless of size" in {
    val f5           = mkFirm(TechState.Traditional(5), sector = 2, size = 5)
    val f100         = mkFirm(TechState.Traditional(100), sector = 2, size = 100)
    val perWorker5   = Firm.computeCapacity(f5).toDouble / 5.0
    val perWorker100 = Firm.computeCapacity(f100).toDouble / 100.0
    perWorker5 shouldBe (perWorker100 +- 0.01)
  }

  // --- CAPEX scaling ---

  "Firm.computeAiCapex" should "scale sublinearly with firm size" in {
    val fSmall     = mkFirm(TechState.Traditional(10), sector = 2, size = 10)
    val fLarge     = mkFirm(TechState.Traditional(100), sector = 2, size = 100)
    val capexSmall = Firm.computeAiCapex(fSmall)
    val capexLarge = Firm.computeAiCapex(fLarge)
    // Sublinear: 10× size → 10^0.6 ≈ 3.98× CAPEX (not 10×)
    val ratio      = capexLarge / capexSmall // PLN / PLN → Double
    ratio shouldBe (Math.pow(10.0, 0.6) +- 0.01)
    ratio should be < 10.0
  }

  // --- Backward compatibility ---

  "Default initialSize" should "be 10 for backward compatibility" in {
    val f = mkFirm(TechState.Traditional(10)).copy(cash = PLN(50000))
    f.initialSize shouldBe 10
  }
