package sfc.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen
import sfc.testutil.Generators.*
import sfc.config.{Config, SECTORS}
import sfc.types.*

class FirmPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  // --- capacity properties ---

  "FirmOps.capacity" should "be >= 0 for all tech states" in {
    forAll(genFirm) { (firm: Firm) =>
      FirmOps.capacity(firm) should be >= 0.0
    }
  }

  it should "be 0 iff Bankrupt" in {
    forAll(genFirm) { (firm: Firm) =>
      firm.tech match
        case _: TechState.Bankrupt => FirmOps.capacity(firm) shouldBe 0.0
        case _ => FirmOps.capacity(firm) should be > 0.0
    }
  }

  // --- workers properties ---

  "FirmOps.workers" should "be >= 0 for all tech states" in {
    forAll(genFirm) { (firm: Firm) =>
      FirmOps.workers(firm) should be >= 0
    }
  }

  it should "be 0 iff Bankrupt" in {
    forAll(genFirm) { (firm: Firm) =>
      firm.tech match
        case _: TechState.Bankrupt => FirmOps.workers(firm) shouldBe 0
        case _ => FirmOps.workers(firm) should be > 0
    }
  }

  // --- isAlive property ---

  "FirmOps.isAlive" should "be false iff Bankrupt" in {
    forAll(genFirm) { (firm: Firm) =>
      firm.tech match
        case _: TechState.Bankrupt => FirmOps.isAlive(firm) shouldBe false
        case _ => FirmOps.isAlive(firm) shouldBe true
    }
  }

  // --- sigmaThreshold properties ---

  "FirmOps.sigmaThreshold" should "be in [0, 1]" in {
    forAll(genSigma) { (sigma: Double) =>
      val t = FirmOps.sigmaThreshold(sigma)
      t should be >= 0.0
      t should be <= 1.0
    }
  }

  it should "be monotonic in sigma" in {
    forAll(genSigma) { (sigma: Double) =>
      whenever(sigma > 0.1 && sigma < 99.0) {
        val t1 = FirmOps.sigmaThreshold(sigma)
        val t2 = FirmOps.sigmaThreshold(sigma * 2)
        t2 should be >= (t1 - 1e-10)
      }
    }
  }

  it should "be 1.0 for very large sigma" in {
    FirmOps.sigmaThreshold(1000.0) shouldBe 1.0
  }

  // --- aiCapex properties ---

  "FirmOps.aiCapex" should "be > 0 for alive firms" in {
    forAll(genAliveFirm) { (firm: Firm) =>
      FirmOps.aiCapex(firm) should be > 0.0
    }
  }

  "FirmOps.hybridCapex" should "be > 0 for alive firms" in {
    forAll(genAliveFirm) { (firm: Firm) =>
      FirmOps.hybridCapex(firm) should be > 0.0
    }
  }

  "FirmOps.aiCapex" should "scale with innovationCostFactor" in {
    forAll(genAliveFirm, Gen.choose(1.0, 3.0)) { (firm: Firm, factor: Double) =>
      val f1 = firm.copy(innovationCostFactor = 1.0)
      val f2 = firm.copy(innovationCostFactor = factor)
      FirmOps.aiCapex(f2) shouldBe (FirmOps.aiCapex(f1) * factor +- 0.01)
    }
  }

  // --- capacity(Traditional) scales with sqrt(workers/initialSize) ---

  "capacity for Traditional" should "scale with sqrt(workers/initialSize)" in {
    forAll(Gen.choose(0, 5), Gen.choose(0.5, 2.0), Gen.choose(0.02, 0.98)) {
      (sector: Int, innov: Double, digiR: Double) =>
        // Same initialSize=16, different worker counts: sqrt(4/16) vs sqrt(16/16)
        val f1 = Firm(FirmId(0), 0, 0, TechState.Traditional(4), 0.5, innov, digiR, SectorIdx(sector), Array.empty[Int],
          initialSize = 16)
        val f2 = Firm(FirmId(0), 0, 0, TechState.Traditional(16), 0.5, innov, digiR, SectorIdx(sector), Array.empty[Int],
          initialSize = 16)
        val ratio = FirmOps.capacity(f2) / FirmOps.capacity(f1)
        ratio shouldBe (2.0 +- 0.01)
    }
  }

  it should "scale linearly with initialSize at full employment" in {
    forAll(Gen.choose(0, 5), Gen.choose(0.5, 2.0), Gen.choose(0.02, 0.98)) {
      (sector: Int, innov: Double, digiR: Double) =>
        val f1 = Firm(FirmId(0), 0, 0, TechState.Traditional(10), 0.5, innov, digiR, SectorIdx(sector), Array.empty[Int],
          initialSize = 10)
        val f2 = Firm(FirmId(0), 0, 0, TechState.Traditional(25), 0.5, innov, digiR, SectorIdx(sector), Array.empty[Int],
          initialSize = 25)
        val ratio = FirmOps.capacity(f2) / FirmOps.capacity(f1)
        ratio shouldBe (2.5 +- 0.01)
    }
  }
