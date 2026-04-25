package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.boombustgroup.amorfati.types.*

class SigmaDynamicsPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams = SimParams.defaults

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  private val genSigmaVector: Gen[Vector[BigDecimal]] =
    Gen.sequence[Vector[BigDecimal], BigDecimal](0.until(6).map(_ => genDecimal("0.5", "50.0")))

  private val genAdoptionVector: Gen[Vector[BigDecimal]] =
    Gen.sequence[Vector[BigDecimal], BigDecimal](0.until(6).map(_ => genDecimal("0.0", "1.0")))

  /** Generate (current, base, capMult) such that current(i) <= base(i) *
    * capMult for all i — the valid operating regime where the ratchet holds.
    */
  private val genBelowCapInputs: Gen[(Vector[BigDecimal], Vector[BigDecimal], BigDecimal)] =
    for
      base    <- Gen.sequence[Vector[BigDecimal], BigDecimal](0.until(6).map(_ => genDecimal("1.0", "50.0")))
      capMult <- genDecimal("1.5", "5.0")
      fracs   <- Gen.sequence[Vector[BigDecimal], BigDecimal](0.until(6).map(_ => genDecimal("0.1", "0.95")))
    yield
      val current = base.zip(fracs).map((b, f) => b * capMult * f)
      (current, base, capMult)

  private def toSigmas(v: Vector[BigDecimal]): Vector[Sigma] = v.map(Sigma(_))

  private def evolveSigmas(
      current: Vector[BigDecimal],
      base: Vector[BigDecimal],
      adoption: Vector[BigDecimal],
      lambda: BigDecimal,
      capMult: BigDecimal,
  ): Vector[Sigma] =
    PriceEquityEconomics.evolveSigmas(toSigmas(current), toSigmas(base), adoption.map(Share(_)), Coefficient(lambda), Multiplier(capMult))

  // --- Ratchet property ---

  "PriceEquityEconomics.evolveSigmas" should "never decrease sigma when below cap (ratchet)" in
    forAll(genBelowCapInputs, genAdoptionVector, genDecimal("0.001", "0.10")) {
      (inputs: (Vector[BigDecimal], Vector[BigDecimal], BigDecimal), adoption: Vector[BigDecimal], lambda: BigDecimal) =>
        val (current, base, capMult) = inputs
        val result                   = evolveSigmas(current, base, adoption, lambda, capMult)
        for i <- current.indices do decimal(result(i)) should be >= (current(i) - BigDecimal("0.0001"))
    }

  // --- Cap property ---

  it should "cap sigma at base * capMult" in
    forAll(genBelowCapInputs, genAdoptionVector, genDecimal("0.001", "0.10")) {
      (inputs: (Vector[BigDecimal], Vector[BigDecimal], BigDecimal), adoption: Vector[BigDecimal], lambda: BigDecimal) =>
        val (_, base, capMult) = inputs
        val current            = base.map(_ * capMult * BigDecimal("0.99"))
        val result             = evolveSigmas(current, base, adoption, lambda, capMult)
        for i <- result.indices do decimal(result(i)) should be <= (base(i) * capMult + BigDecimal("1e-10"))
    }

  // --- Lambda=0 is identity ---

  it should "be identity when lambda=0" in
    forAll(genSigmaVector, genSigmaVector, genAdoptionVector) { (current: Vector[BigDecimal], base: Vector[BigDecimal], adoption: Vector[BigDecimal]) =>
      val result = evolveSigmas(current, base, adoption, BigDecimal("0.0"), BigDecimal("3.0"))
      result shouldBe toSigmas(current)
    }

  // --- Adoption=0 -> identity when below cap ---

  it should "be identity for sectors with zero adoption when below cap" in
    forAll(genBelowCapInputs, genDecimal("0.001", "0.10")) { (inputs: (Vector[BigDecimal], Vector[BigDecimal], BigDecimal), lambda: BigDecimal) =>
      val (current, base, capMult) = inputs
      val zeroAdoption             = Vector.fill(6)(BigDecimal("0.0"))
      val result                   = evolveSigmas(current, base, zeroAdoption, lambda, capMult)
      for i <- current.indices do decimal(result(i)) shouldBe (current(i) +- BigDecimal("1e-3"))
    }

  // --- Positive lambda + positive adoption -> increase ---

  it should "increase sigma when lambda > 0 and adoption > 0 and below cap" in
    forAll(genDecimal("0.01", "0.10"), genDecimal("1.5", "5.0")) { (lambda: BigDecimal, capMult: BigDecimal) =>
      val base     = Vector(BigDecimal("5.0"), BigDecimal("10.0"), BigDecimal("3.0"), BigDecimal("2.0"), BigDecimal("1.0"), BigDecimal("4.0"))
      val current  = base
      val adoption = Vector.fill(6)(BigDecimal("0.5"))
      val result   = evolveSigmas(current, base, adoption, lambda, capMult)
      for i <- result.indices do decimal(result(i)) should be > current(i)
    }

  // --- Length preservation ---

  it should "preserve vector length" in
    forAll(genSigmaVector, genSigmaVector, genAdoptionVector, genDecimal("0.0", "0.10"), genDecimal("1.5", "5.0")) {
      (current: Vector[BigDecimal], base: Vector[BigDecimal], adoption: Vector[BigDecimal], lambda: BigDecimal, capMult: BigDecimal) =>
        val result = evolveSigmas(current, base, adoption, lambda, capMult)
        result.length shouldBe current.length
    }

  // --- Sector independence ---

  it should "have sector independence (changing sector i doesn't affect sector j)" in
    forAll(genBelowCapInputs, genAdoptionVector, genDecimal("0.001", "0.10"), Gen.choose(0, 5)) {
      (inputs: (Vector[BigDecimal], Vector[BigDecimal], BigDecimal), adoption: Vector[BigDecimal], lambda: BigDecimal, targetSector: Int) =>
        val (current, base, capMult) = inputs
        val adoption2                = adoption.updated(targetSector, BigDecimal("0.0"))
        val r1                       = evolveSigmas(current, base, adoption, lambda, capMult)
        val r2                       = evolveSigmas(current, base, adoption2, lambda, capMult)
        for i <- current.indices if i != targetSector do decimal(r1(i)) shouldBe (decimal(r2(i)) +- BigDecimal("1e-3"))
    }

  // --- Monotonic in lambda ---

  it should "be monotonic in lambda (higher lambda -> higher or equal sigma)" in
    forAll(genBelowCapInputs, genAdoptionVector) { (inputs: (Vector[BigDecimal], Vector[BigDecimal], BigDecimal), adoption: Vector[BigDecimal]) =>
      val (current, base, capMult) = inputs
      val r1                       = evolveSigmas(current, base, adoption, BigDecimal("0.01"), capMult)
      val r2                       = evolveSigmas(current, base, adoption, BigDecimal("0.10"), capMult)
      for i <- current.indices do decimal(r2(i)) should be >= (decimal(r1(i)) - BigDecimal("1e-10"))
    }
