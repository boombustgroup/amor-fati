package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.TestFirmState

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.boombustgroup.amorfati.Generators.*
import com.boombustgroup.amorfati.types.*

class FirmPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams = SimParams.defaults

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  // --- capacity properties ---

  "Firm.computeCapacity" should "be >= 0 for all tech states" in
    forAll(genFirm) { (firm: Firm.State) =>
      Firm.computeCapacity(firm) should be >= PLN.Zero
    }

  it should "be 0 iff Bankrupt" in
    forAll(genFirm) { (firm: Firm.State) =>
      firm.tech match
        case _: TechState.Bankrupt => Firm.computeCapacity(firm) shouldBe PLN.Zero
        case _                     => Firm.computeCapacity(firm) should be > PLN.Zero
    }

  // --- workers properties ---

  "Firm.workerCount" should "be >= 0 for all tech states" in
    forAll(genFirm) { (firm: Firm.State) =>
      Firm.workerCount(firm) should be >= 0
    }

  it should "be 0 iff Bankrupt" in
    forAll(genFirm) { (firm: Firm.State) =>
      firm.tech match
        case _: TechState.Bankrupt => Firm.workerCount(firm) shouldBe 0
        case _                     => Firm.workerCount(firm) should be > 0
    }

  // --- isAlive property ---

  "Firm.isAlive" should "be false iff Bankrupt" in
    forAll(genFirm) { (firm: Firm.State) =>
      firm.tech match
        case _: TechState.Bankrupt => Firm.isAlive(firm) shouldBe false
        case _                     => Firm.isAlive(firm) shouldBe true
    }

  // --- sigmaThreshold properties ---

  "Firm.sigmaThreshold" should "be in [0, 1]" in
    forAll(genSigma) { (sigma: BigDecimal) =>
      val t = Firm.sigmaThreshold(Sigma(sigma))
      t.bd should be >= BigDecimal(0)
      t.bd should be <= BigDecimal("1.0")
    }

  it should "be monotonic in sigma" in
    forAll(genSigma) { (sigma: BigDecimal) =>
      whenever(sigma > BigDecimal("0.1") && sigma < BigDecimal("99.0")) {
        val t1 = Firm.sigmaThreshold(Sigma(sigma)).bd
        val t2 = Firm.sigmaThreshold(Sigma(sigma * 2)).bd
        t2 should be >= t1
      }
    }

  it should "be 1.0 for very large sigma" in {
    Firm.sigmaThreshold(Sigma("1000.0")).bd shouldBe BigDecimal("1.0")
  }

  // --- aiCapex properties ---

  "Firm.computeAiCapex" should "be > 0 for alive firms" in
    forAll(genAliveFirm) { (firm: Firm.State) =>
      Firm.computeAiCapex(firm) should be > PLN.Zero
    }

  "Firm.computeHybridCapex" should "be > 0 for alive firms" in
    forAll(genAliveFirm) { (firm: Firm.State) =>
      Firm.computeHybridCapex(firm) should be > PLN.Zero
    }

  "Firm.computeAiCapex" should "scale with innovationCostFactor" in
    forAll(genAliveFirm, genDecimal("1.0", "3.0")) { (firm: Firm.State, factor: BigDecimal) =>
      val scaledFactor = Multiplier(factor)
      val f1           = firm.copy(innovationCostFactor = Multiplier.One)
      val f2           = firm.copy(innovationCostFactor = scaledFactor)
      val expected     = Firm.computeAiCapex(f1).bd * scaledFactor.bd
      val tolerance    = (expected * BigDecimal("0.03")).max(BigDecimal("0.01"))
      Firm.computeAiCapex(f2).bd shouldBe (expected +- tolerance)
    }

  // --- capacity(Traditional) scales linearly with workers/initialSize ---

  "capacity for Traditional" should "scale linearly with workers/initialSize" in
    forAll(Gen.choose(0, 5), genDecimal("0.5", "2.0"), genDecimal("0.02", "0.98")) { (sector: Int, innov: BigDecimal, digiR: BigDecimal) =>
      // Same initialSize=16, different worker counts: (16/16) / (4/16) = 4.0
      val f1    = TestFirmState(
        FirmId(0),
        PLN.Zero,
        PLN.Zero,
        TechState.Traditional(4),
        Share("0.5"),
        Multiplier(innov),
        Share(digiR),
        SectorIdx(sector),
        Vector.empty[FirmId],
        bankId = BankId(0),
        equityRaised = PLN.Zero,
        initialSize = 16,
        capitalStock = PLN.Zero,
        foreignOwned = false,
        inventory = PLN.Zero,
        greenCapital = PLN.Zero,
        accumulatedLoss = PLN.Zero,
      )
      val f2    = TestFirmState(
        FirmId(0),
        PLN.Zero,
        PLN.Zero,
        TechState.Traditional(16),
        Share("0.5"),
        Multiplier(innov),
        Share(digiR),
        SectorIdx(sector),
        Vector.empty[FirmId],
        bankId = BankId(0),
        equityRaised = PLN.Zero,
        initialSize = 16,
        capitalStock = PLN.Zero,
        foreignOwned = false,
        inventory = PLN.Zero,
        greenCapital = PLN.Zero,
        accumulatedLoss = PLN.Zero,
      )
      val ratio = Firm.computeCapacity(f2) / Firm.computeCapacity(f1)
      decimal(ratio) shouldBe (BigDecimal("4.0") +- BigDecimal("0.01"))
    }

  it should "scale linearly with initialSize at full employment" in
    forAll(Gen.choose(0, 5), genDecimal("0.5", "2.0"), genDecimal("0.02", "0.98")) { (sector: Int, innov: BigDecimal, digiR: BigDecimal) =>
      val f1    = TestFirmState(
        FirmId(0),
        PLN.Zero,
        PLN.Zero,
        TechState.Traditional(10),
        Share("0.5"),
        Multiplier(innov),
        Share(digiR),
        SectorIdx(sector),
        Vector.empty[FirmId],
        bankId = BankId(0),
        equityRaised = PLN.Zero,
        initialSize = 10,
        capitalStock = PLN.Zero,
        foreignOwned = false,
        inventory = PLN.Zero,
        greenCapital = PLN.Zero,
        accumulatedLoss = PLN.Zero,
      )
      val f2    = TestFirmState(
        FirmId(0),
        PLN.Zero,
        PLN.Zero,
        TechState.Traditional(25),
        Share("0.5"),
        Multiplier(innov),
        Share(digiR),
        SectorIdx(sector),
        Vector.empty[FirmId],
        bankId = BankId(0),
        equityRaised = PLN.Zero,
        initialSize = 25,
        capitalStock = PLN.Zero,
        foreignOwned = false,
        inventory = PLN.Zero,
        greenCapital = PLN.Zero,
        accumulatedLoss = PLN.Zero,
      )
      val ratio = Firm.computeCapacity(f2) / Firm.computeCapacity(f1)
      decimal(ratio) shouldBe (BigDecimal("2.5") +- BigDecimal("0.01"))
    }

  // --- localAutoRatio properties ---

  "Firm.computeLocalAutoRatio" should "be in [0, 1]" in {
    val firms = (0 until 10).map { i =>
      val tech = if i < 3 then TechState.Automated(Multiplier.One) else TechState.Traditional(10)
      TestFirmState(
        FirmId(i),
        PLN(100000),
        PLN.Zero,
        tech,
        Share("0.5"),
        Multiplier.One,
        Share("0.4"),
        SectorIdx(0),
        (0 until 10).filter(_ != i).map(FirmId(_)).toVector,
        bankId = BankId(0),
        equityRaised = PLN.Zero,
        initialSize = 10,
        capitalStock = PLN.Zero,
        foreignOwned = false,
        inventory = PLN.Zero,
        greenCapital = PLN.Zero,
        accumulatedLoss = PLN.Zero,
      )
    }.toVector
    for f <- firms do
      val r = Firm.computeLocalAutoRatio(f, firms)
      r should be >= Share.Zero
      r should be <= Share.One
  }

  it should "be 0 when no neighbors" in {
    val firm  = TestFirmState(
      FirmId(0),
      PLN(100000),
      PLN.Zero,
      TechState.Traditional(10),
      Share("0.5"),
      Multiplier.One,
      Share("0.4"),
      SectorIdx(0),
      Vector.empty[FirmId],
      bankId = BankId(0),
      equityRaised = PLN.Zero,
      initialSize = 10,
      capitalStock = PLN.Zero,
      foreignOwned = false,
      inventory = PLN.Zero,
      greenCapital = PLN.Zero,
      accumulatedLoss = PLN.Zero,
    )
    val firms = Vector(firm)
    Firm.computeLocalAutoRatio(firm, firms) shouldBe Share.Zero
  }
