package sfc.config

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sfc.Generators.*
import sfc.types.*

class SimConfigPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  // --- Sector invariants ---

  "SectorDefs" should "have shares summing to approximately 1.0" in {
    val sum = SectorDefs.map(_.share.toDouble).sum
    sum shouldBe (1.0 +- 0.01)
  }

  it should "have all sigma > 0" in {
    for s <- SectorDefs do s.sigma should be > 0.0
  }

  it should "have all multipliers > 0" in {
    for s <- SectorDefs do
      s.wageMultiplier should be > 0.0
      s.revenueMultiplier should be > 0.0
      s.aiCapexMultiplier should be > 0.0
      s.hybridCapexMultiplier should be > 0.0
  }

  it should "have hybridRetainFrac in (0, 1]" in {
    for s <- SectorDefs do
      s.hybridRetainFrac.toDouble should be > 0.0
      s.hybridRetainFrac.toDouble should be <= 1.0
  }

  it should "have baseDigitalReadiness in [0, 1]" in {
    for s <- SectorDefs do
      s.baseDigitalReadiness.toDouble should be >= 0.0
      s.baseDigitalReadiness.toDouble should be <= 1.0
  }

  // --- IoMatrix invariants ---

  "Config.IoMatrix" should "have non-negative entries" in {
    for
      row <- Config.IoMatrix
      v <- row
    do v should be >= 0.0
  }

  it should "have column sums < 1.0" in {
    for j <- 0 until 6 do
      val colSum = Config.IoMatrix.map(_(j)).sum
      colSum should be < 1.0
  }

  "Config.IoColumnSums" should "match IoMatrix computation" in {
    for j <- 0 until 6 do
      val expected = Config.IoMatrix.map(_(j)).sum
      Config.IoColumnSums(j) shouldBe (expected +- 1e-10)
  }

  // --- RunConfig properties ---

  "RunConfig" should "have isNoBdp iff bdpAmount == 0" in {
    forAll(Gen.choose(0.0, 5000.0)) { (bdp: Double) =>
      val rc = RunConfig(bdp, 1, "test")
      rc.isNoBdp shouldBe (bdp == 0.0)
    }
  }

  // --- Generated IoMatrix properties ---

  "Generated IoMatrix" should "have non-negative entries and column sums < 1.0" in {
    forAll(genIoMatrix) { (m: Vector[Vector[Double]]) =>
      for
        i <- 0 until 6
        j <- 0 until 6
      do m(i)(j) should be >= 0.0

      for j <- 0 until 6 do m.map(_(j)).sum should be < 1.0
    }
  }
