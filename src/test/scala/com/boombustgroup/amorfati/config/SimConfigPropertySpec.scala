package com.boombustgroup.amorfati.config

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.boombustgroup.amorfati.Generators.*
import com.boombustgroup.amorfati.types.*

class SimConfigPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  // --- Sector invariants ---

  "p.sectorDefs" should "have shares summing to approximately 1.0" in {
    val sum = p.sectorDefs.map(_.share.toDouble).sum
    sum shouldBe (1.0 +- 0.01)
  }

  it should "have all sigma > 0" in {
    for s <- p.sectorDefs do s.sigma should be > 0.0
  }

  it should "have all multipliers > 0" in {
    for s <- p.sectorDefs do
      s.wageMultiplier should be > 0.0
      s.revenueMultiplier should be > 0.0
      s.aiCapexMultiplier should be > 0.0
      s.hybridCapexMultiplier should be > 0.0
  }

  it should "have hybridRetainFrac in (0, 1]" in {
    for s <- p.sectorDefs do
      s.hybridRetainFrac.toDouble should be > 0.0
      s.hybridRetainFrac.toDouble should be <= 1.0
  }

  it should "have baseDigitalReadiness in [0, 1]" in {
    for s <- p.sectorDefs do
      s.baseDigitalReadiness.toDouble should be >= 0.0
      s.baseDigitalReadiness.toDouble should be <= 1.0
  }

  // --- IoMatrix invariants ---

  "p.io.matrix" should "have non-negative entries" in {
    for
      row <- p.io.matrix
      v   <- row
    do v should be >= 0.0
  }

  it should "have column sums < 1.0" in {
    for j <- 0 until 6 do
      val colSum = p.io.matrix.map(_(j)).sum
      colSum should be < 1.0
  }

  "p.io.columnSums" should "match IoMatrix computation" in {
    for j <- 0 until 6 do
      val expected = p.io.matrix.map(_(j)).sum
      p.io.columnSums(j) shouldBe (expected +- 1e-10)
  }

  // --- Generated IoMatrix properties ---

  "Generated IoMatrix" should "have non-negative entries and column sums < 1.0" in
    forAll(genIoMatrix) { (m: Vector[Vector[Double]]) =>
      for
        i <- 0 until 6
        j <- 0 until 6
      do m(i)(j) should be >= 0.0

      for j <- 0 until 6 do m.map(_(j)).sum should be < 1.0
    }
