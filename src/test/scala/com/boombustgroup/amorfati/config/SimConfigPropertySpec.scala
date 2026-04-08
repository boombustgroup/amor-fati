package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.Generators.*
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class SimConfigPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  // --- Sector invariants ---

  "p.sectorDefs" should "have shares summing to approximately 1.0" in {
    val sum = p.sectorDefs.map(_.share).foldLeft(Share.Zero)(_ + _)
    (sum - Share.One).abs should be <= Share(0.01)
  }

  it should "have all sigma > 0" in {
    for s <- p.sectorDefs do s.sigma should be > Sigma.Zero
  }

  it should "have all multipliers > 0" in {
    for s <- p.sectorDefs do
      s.wageMultiplier should be > Multiplier.Zero
      s.revenueMultiplier should be > Multiplier.Zero
      s.aiCapexMultiplier should be > Multiplier.Zero
      s.hybridCapexMultiplier should be > Multiplier.Zero
  }

  it should "have hybridRetainFrac in (0, 1]" in {
    for s <- p.sectorDefs do
      s.hybridRetainFrac should be > Share.Zero
      s.hybridRetainFrac should be <= Share.One
  }

  it should "have baseDigitalReadiness in [0, 1]" in {
    for s <- p.sectorDefs do
      s.baseDigitalReadiness should be >= Share.Zero
      s.baseDigitalReadiness should be <= Share.One
  }

  // --- IoMatrix invariants ---

  "p.io.matrix" should "have non-negative entries" in {
    for
      row <- p.io.matrix
      v   <- row
    do v should be >= Share.Zero
  }

  it should "have column sums < 1.0" in {
    for j <- 0 until 6 do
      val colSum = p.io.matrix.map(_(j)).sum
      colSum should be < Share.One
  }

  "p.io.columnSums" should "match IoMatrix computation" in {
    for j <- 0 until 6 do
      val expected = p.io.matrix.map(_(j)).sum
      p.io.columnSums(j) shouldBe expected
  }

  // --- Generated IoMatrix properties ---

  "Generated IoMatrix" should "have non-negative entries and column sums < 1.0" in
    forAll(genIoMatrix): (m: Vector[Vector[Double]]) =>
      val typed = m.map(_.map(Share(_)))
      for
        i <- 0 until 6
        j <- 0 until 6
      do typed(i)(j) should be >= Share.Zero

      for j <- 0 until 6 do typed.map(_(j)).foldLeft(Share.Zero)(_ + _) should be < Share.One
      IoConfig(matrix = typed).columnSums shouldBe
        (0 until 6).map(j => typed.map(_(j)).foldLeft(Share.Zero)(_ + _)).toVector
