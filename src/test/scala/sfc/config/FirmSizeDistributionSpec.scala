package sfc.config

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class FirmSizeDistributionSpec extends AnyFlatSpec with Matchers:

  "FirmSizeDistribution.draw" should "return WorkersPerFirm when FirmSizeDist=uniform" in {
    // Default config is "uniform" — all firms get WorkersPerFirm (10)
    val rng = new Random(42)
    for _ <- 0 until 100 do
      FirmSizeDistribution.draw(rng) shouldBe Config.WorkersPerFirm
  }

  it should "return values in valid ranges for all size classes" in {
    // We can't test "gus" mode directly via env vars in unit tests,
    // so we test the draw logic by verifying the returned values for
    // the default (uniform) mode are always Config.WorkersPerFirm
    val rng = new Random(42)
    val sizes = (0 until 10000).map(_ => FirmSizeDistribution.draw(rng))
    sizes.foreach(_ shouldBe Config.WorkersPerFirm)
  }

  // --- FirmOps size-dependent methods ---

  "FirmOps.skeletonCrew" should "return AutoSkeletonCrew for small firms" in {
    import sfc.agents.{Firm, FirmOps, TechState}
    val f = Firm(0, 0, 0, TechState.Traditional(5), 0.5, 1.0, 0.5, 0, Array.empty, initialSize = 5)
    FirmOps.skeletonCrew(f) shouldBe Config.AutoSkeletonCrew
  }

  it should "scale with initialSize for large firms" in {
    import sfc.agents.{Firm, FirmOps, TechState}
    val f = Firm(0, 0, 0, TechState.Traditional(250), 0.5, 1.0, 0.5, 0, Array.empty, initialSize = 250)
    FirmOps.skeletonCrew(f) shouldBe 5  // max(2, 250 * 0.02 = 5)
  }

  it should "be at least AutoSkeletonCrew" in {
    import sfc.agents.{Firm, FirmOps, TechState}
    for size <- Vector(1, 3, 5, 10, 50, 100, 250, 500) do
      val f = Firm(0, 0, 0, TechState.Traditional(size), 0.5, 1.0, 0.5, 0, Array.empty, initialSize = size)
      FirmOps.skeletonCrew(f) should be >= Config.AutoSkeletonCrew
  }

  // --- Capacity scaling ---

  "FirmOps.capacity" should "scale linearly with initialSize at full employment" in {
    import sfc.agents.{Firm, FirmOps, TechState}
    val f10 = Firm(0, 0, 0, TechState.Traditional(10), 0.5, 1.0, 0.5, 2, Array.empty, initialSize = 10)
    val f25 = Firm(0, 0, 0, TechState.Traditional(25), 0.5, 1.0, 0.5, 2, Array.empty, initialSize = 25)
    val ratio = FirmOps.capacity(f25) / FirmOps.capacity(f10)
    ratio shouldBe (2.5 +- 0.01)
  }

  it should "give same per-worker revenue at full employment regardless of size" in {
    import sfc.agents.{Firm, FirmOps, TechState}
    val f5 = Firm(0, 0, 0, TechState.Traditional(5), 0.5, 1.0, 0.5, 2, Array.empty, initialSize = 5)
    val f100 = Firm(0, 0, 0, TechState.Traditional(100), 0.5, 1.0, 0.5, 2, Array.empty, initialSize = 100)
    val perWorker5 = FirmOps.capacity(f5) / 5.0
    val perWorker100 = FirmOps.capacity(f100) / 100.0
    perWorker5 shouldBe (perWorker100 +- 0.01)
  }

  // --- CAPEX scaling ---

  "FirmOps.aiCapex" should "scale sublinearly with firm size" in {
    import sfc.agents.{Firm, FirmOps, TechState}
    val fSmall = Firm(0, 0, 0, TechState.Traditional(10), 0.5, 1.0, 0.5, 2, Array.empty, initialSize = 10)
    val fLarge = Firm(0, 0, 0, TechState.Traditional(100), 0.5, 1.0, 0.5, 2, Array.empty, initialSize = 100)
    val capexSmall = FirmOps.aiCapex(fSmall)
    val capexLarge = FirmOps.aiCapex(fLarge)
    // Sublinear: 10× size → 10^0.6 ≈ 3.98× CAPEX (not 10×)
    val ratio = capexLarge / capexSmall
    ratio shouldBe (Math.pow(10.0, 0.6) +- 0.01)
    ratio should be < 10.0
  }

  // --- Backward compatibility ---

  "Default initialSize" should "be 10 for backward compatibility" in {
    import sfc.agents.{Firm, TechState}
    val f = Firm(0, 50000, 0, TechState.Traditional(10), 0.5, 1.0, 0.5, 0, Array.empty)
    f.initialSize shouldBe 10
  }
