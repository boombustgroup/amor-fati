package sfc.agents

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sfc.Generators.*
import sfc.types.*

class FirmPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  // --- capacity properties ---

  "Firm.capacity" should "be >= 0 for all tech states" in {
    forAll(genFirm) { (firm: Firm.State) =>
      Firm.capacity(firm) should be >= 0.0
    }
  }

  it should "be 0 iff Bankrupt" in {
    forAll(genFirm) { (firm: Firm.State) =>
      firm.tech match
        case _: TechState.Bankrupt => Firm.capacity(firm) shouldBe 0.0
        case _                     => Firm.capacity(firm) should be > 0.0
    }
  }

  // --- workers properties ---

  "Firm.workers" should "be >= 0 for all tech states" in {
    forAll(genFirm) { (firm: Firm.State) =>
      Firm.workers(firm) should be >= 0
    }
  }

  it should "be 0 iff Bankrupt" in {
    forAll(genFirm) { (firm: Firm.State) =>
      firm.tech match
        case _: TechState.Bankrupt => Firm.workers(firm) shouldBe 0
        case _                     => Firm.workers(firm) should be > 0
    }
  }

  // --- isAlive property ---

  "Firm.isAlive" should "be false iff Bankrupt" in {
    forAll(genFirm) { (firm: Firm.State) =>
      firm.tech match
        case _: TechState.Bankrupt => Firm.isAlive(firm) shouldBe false
        case _                     => Firm.isAlive(firm) shouldBe true
    }
  }

  // --- sigmaThreshold properties ---

  "Firm.sigmaThreshold" should "be in [0, 1]" in {
    forAll(genSigma) { (sigma: Double) =>
      val t = Firm.sigmaThreshold(sigma)
      t should be >= 0.0
      t should be <= 1.0
    }
  }

  it should "be monotonic in sigma" in {
    forAll(genSigma) { (sigma: Double) =>
      whenever(sigma > 0.1 && sigma < 99.0) {
        val t1 = Firm.sigmaThreshold(sigma)
        val t2 = Firm.sigmaThreshold(sigma * 2)
        t2 should be >= (t1 - 1e-10)
      }
    }
  }

  it should "be 1.0 for very large sigma" in {
    Firm.sigmaThreshold(1000.0) shouldBe 1.0
  }

  // --- aiCapex properties ---

  "Firm.aiCapex" should "be > 0 for alive firms" in {
    forAll(genAliveFirm) { (firm: Firm.State) =>
      Firm.aiCapex(firm) should be > 0.0
    }
  }

  "Firm.hybridCapex" should "be > 0 for alive firms" in {
    forAll(genAliveFirm) { (firm: Firm.State) =>
      Firm.hybridCapex(firm) should be > 0.0
    }
  }

  "Firm.aiCapex" should "scale with innovationCostFactor" in {
    forAll(genAliveFirm, Gen.choose(1.0, 3.0)) { (firm: Firm.State, factor: Double) =>
      val f1 = firm.copy(innovationCostFactor = 1.0)
      val f2 = firm.copy(innovationCostFactor = factor)
      Firm.aiCapex(f2) shouldBe (Firm.aiCapex(f1) * factor +- 0.01)
    }
  }

  // --- capacity(Traditional) scales with sqrt(workers/initialSize) ---

  "capacity for Traditional" should "scale with sqrt(workers/initialSize)" in {
    forAll(Gen.choose(0, 5), Gen.choose(0.5, 2.0), Gen.choose(0.02, 0.98)) {
      (sector: Int, innov: Double, digiR: Double) =>
        // Same initialSize=16, different worker counts: sqrt(4/16) vs sqrt(16/16)
        val f1 = Firm.State(
          FirmId(0),
          PLN.Zero,
          PLN.Zero,
          TechState.Traditional(4),
          Ratio(0.5),
          innov,
          Ratio(digiR),
          SectorIdx(sector),
          Array.empty[FirmId],
          initialSize = 16,
        )
        val f2 = Firm.State(
          FirmId(0),
          PLN.Zero,
          PLN.Zero,
          TechState.Traditional(16),
          Ratio(0.5),
          innov,
          Ratio(digiR),
          SectorIdx(sector),
          Array.empty[FirmId],
          initialSize = 16,
        )
        val ratio = Firm.capacity(f2) / Firm.capacity(f1)
        ratio shouldBe (2.0 +- 0.01)
    }
  }

  it should "scale linearly with initialSize at full employment" in {
    forAll(Gen.choose(0, 5), Gen.choose(0.5, 2.0), Gen.choose(0.02, 0.98)) {
      (sector: Int, innov: Double, digiR: Double) =>
        val f1 = Firm.State(
          FirmId(0),
          PLN.Zero,
          PLN.Zero,
          TechState.Traditional(10),
          Ratio(0.5),
          innov,
          Ratio(digiR),
          SectorIdx(sector),
          Array.empty[FirmId],
          initialSize = 10,
        )
        val f2 = Firm.State(
          FirmId(0),
          PLN.Zero,
          PLN.Zero,
          TechState.Traditional(25),
          Ratio(0.5),
          innov,
          Ratio(digiR),
          SectorIdx(sector),
          Array.empty[FirmId],
          initialSize = 25,
        )
        val ratio = Firm.capacity(f2) / Firm.capacity(f1)
        ratio shouldBe (2.5 +- 0.01)
    }
  }

  // --- localAutoRatio properties ---

  "Firm.localAutoRatio" should "be in [0, 1]" in {
    val firms = (0 until 10).map { i =>
      val tech = if i < 3 then TechState.Automated(1.0) else TechState.Traditional(10)
      Firm.State(
        FirmId(i),
        PLN(100000),
        PLN.Zero,
        tech,
        Ratio(0.5),
        1.0,
        Ratio(0.4),
        SectorIdx(0),
        (0 until 10).filter(_ != i).map(FirmId(_)).toArray,
      )
    }.toArray
    for f <- firms do
      val r = Firm.localAutoRatio(f, firms)
      r should be >= 0.0
      r should be <= 1.0
  }

  it should "be 0 when no neighbors" in {
    val firm = Firm.State(
      FirmId(0),
      PLN(100000),
      PLN.Zero,
      TechState.Traditional(10),
      Ratio(0.5),
      1.0,
      Ratio(0.4),
      SectorIdx(0),
      Array.empty[FirmId],
    )
    val firms = Array(firm)
    Firm.localAutoRatio(firm, firms) shouldBe 0.0
  }
