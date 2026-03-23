package com.boombustgroup.amorfati.engine.steps

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.boombustgroup.amorfati.types.*

class SigmaDynamicsPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams = SimParams.defaults
  private val td  = ComputationBoundary

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  private val genSigmaVector: Gen[Vector[Double]] =
    Gen.sequence[Vector[Double], Double](0.until(6).map(_ => Gen.choose(0.5, 50.0)))

  private val genAdoptionVector: Gen[Vector[Double]] =
    Gen.sequence[Vector[Double], Double](0.until(6).map(_ => Gen.choose(0.0, 1.0)))

  /** Generate (current, base, capMult) such that current(i) <= base(i) *
    * capMult for all i — the valid operating regime where the ratchet holds.
    */
  private val genBelowCapInputs: Gen[(Vector[Double], Vector[Double], Double)] =
    for
      base    <- Gen.sequence[Vector[Double], Double](0.until(6).map(_ => Gen.choose(1.0, 50.0)))
      capMult <- Gen.choose(1.5, 5.0)
      fracs   <- Gen.sequence[Vector[Double], Double](0.until(6).map(_ => Gen.choose(0.1, 0.95)))
    yield
      val current = base.zip(fracs).map((b, f) => b * capMult * f)
      (current, base, capMult)

  private def toSigmas(v: Vector[Double]): Vector[Sigma] = v.map(Sigma(_))

  // --- Ratchet property ---

  "PriceEquityStep.evolveSigmas" should "never decrease sigma when below cap (ratchet)" in
    forAll(genBelowCapInputs, genAdoptionVector, Gen.choose(0.001, 0.10)) {
      (inputs: (Vector[Double], Vector[Double], Double), adoption: Vector[Double], lambda: Double) =>
        val (current, base, capMult) = inputs
        val result                   = PriceEquityStep.evolveSigmas(toSigmas(current), base, adoption, lambda, capMult)
        for i <- current.indices do td.toDouble(result(i)) should be >= (current(i) - 0.0001)
    }

  // --- Cap property ---

  it should "cap sigma at base * capMult" in
    forAll(genBelowCapInputs, genAdoptionVector, Gen.choose(0.001, 0.10)) {
      (inputs: (Vector[Double], Vector[Double], Double), adoption: Vector[Double], lambda: Double) =>
        val (_, base, capMult) = inputs
        val current            = base.map(_ * capMult * 0.99)
        val result             = PriceEquityStep.evolveSigmas(toSigmas(current), base, adoption, lambda, capMult)
        for i <- result.indices do td.toDouble(result(i)) should be <= (base(i) * capMult + 1e-10)
    }

  // --- Lambda=0 is identity ---

  it should "be identity when lambda=0" in
    forAll(genSigmaVector, genSigmaVector, genAdoptionVector) { (current: Vector[Double], base: Vector[Double], adoption: Vector[Double]) =>
      val result = PriceEquityStep.evolveSigmas(toSigmas(current), base, adoption, 0.0, 3.0)
      result shouldBe toSigmas(current)
    }

  // --- Adoption=0 → identity when below cap ---

  it should "be identity for sectors with zero adoption when below cap" in
    forAll(genBelowCapInputs, Gen.choose(0.001, 0.10)) { (inputs: (Vector[Double], Vector[Double], Double), lambda: Double) =>
      val (current, base, capMult) = inputs
      val zeroAdoption             = Vector.fill(6)(0.0)
      val result                   = PriceEquityStep.evolveSigmas(toSigmas(current), base, zeroAdoption, lambda, capMult)
      for i <- current.indices do td.toDouble(result(i)) shouldBe (current(i) +- 1e-3)
    }

  // --- Positive lambda + positive adoption → increase ---

  it should "increase sigma when lambda > 0 and adoption > 0 and below cap" in
    forAll(Gen.choose(0.01, 0.10), Gen.choose(1.5, 5.0)) { (lambda: Double, capMult: Double) =>
      val base     = Vector(5.0, 10.0, 3.0, 2.0, 1.0, 4.0)
      val current  = base
      val adoption = Vector.fill(6)(0.5)
      val result   = PriceEquityStep.evolveSigmas(toSigmas(current), base, adoption, lambda, capMult)
      for i <- result.indices do td.toDouble(result(i)) should be > current(i)
    }

  // --- Length preservation ---

  it should "preserve vector length" in
    forAll(genSigmaVector, genSigmaVector, genAdoptionVector, Gen.choose(0.0, 0.10), Gen.choose(1.5, 5.0)) {
      (current: Vector[Double], base: Vector[Double], adoption: Vector[Double], lambda: Double, capMult: Double) =>
        val result = PriceEquityStep.evolveSigmas(toSigmas(current), base, adoption, lambda, capMult)
        result.length shouldBe current.length
    }

  // --- Sector independence ---

  it should "have sector independence (changing sector i doesn't affect sector j)" in
    forAll(genBelowCapInputs, genAdoptionVector, Gen.choose(0.001, 0.10), Gen.choose(0, 5)) {
      (inputs: (Vector[Double], Vector[Double], Double), adoption: Vector[Double], lambda: Double, targetSector: Int) =>
        val (current, base, capMult) = inputs
        val adoption2                = adoption.updated(targetSector, 0.0)
        val r1                       = PriceEquityStep.evolveSigmas(toSigmas(current), base, adoption, lambda, capMult)
        val r2                       = PriceEquityStep.evolveSigmas(toSigmas(current), base, adoption2, lambda, capMult)
        for i <- current.indices if i != targetSector do td.toDouble(r1(i)) shouldBe (td.toDouble(r2(i)) +- 1e-3)
    }

  // --- Monotonic in lambda ---

  it should "be monotonic in lambda (higher lambda -> higher or equal sigma)" in
    forAll(genBelowCapInputs, genAdoptionVector) { (inputs: (Vector[Double], Vector[Double], Double), adoption: Vector[Double]) =>
      val (current, base, capMult) = inputs
      val r1                       = PriceEquityStep.evolveSigmas(toSigmas(current), base, adoption, 0.01, capMult)
      val r2                       = PriceEquityStep.evolveSigmas(toSigmas(current), base, adoption, 0.10, capMult)
      for i <- current.indices do td.toDouble(r2(i)) should be >= (td.toDouble(r1(i)) - 1e-10)
    }
