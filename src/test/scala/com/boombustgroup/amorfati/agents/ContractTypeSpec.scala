package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.random.RandomStream
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ContractTypeSpec extends AnyFlatSpec with Matchers:

  "zusEmployerRate" should "be highest for Permanent" in {
    decimal(ContractType.zusEmployerRate(ContractType.Permanent)) should be > decimal(ContractType.zusEmployerRate(ContractType.Zlecenie))
    decimal(ContractType.zusEmployerRate(ContractType.Zlecenie)) should be > decimal(ContractType.zusEmployerRate(ContractType.B2B))
  }

  it should "be zero for B2B" in {
    ContractType.zusEmployerRate(ContractType.B2B) shouldBe Share.Zero
  }

  "fpRate" should "be positive only for Permanent" in {
    decimal(ContractType.fpRate(ContractType.Permanent)) should be > BigDecimal("0.0")
    ContractType.fpRate(ContractType.Zlecenie) shouldBe Share.Zero
    ContractType.fpRate(ContractType.B2B) shouldBe Share.Zero
  }

  "firingPriority" should "fire B2B first, Permanent last" in {
    ContractType.firingPriority(ContractType.B2B) should be < ContractType.firingPriority(ContractType.Zlecenie)
    ContractType.firingPriority(ContractType.Zlecenie) should be < ContractType.firingPriority(ContractType.Permanent)
  }

  "aiVulnerability" should "be highest for B2B" in {
    ContractType.aiVulnerability(ContractType.B2B) should be > ContractType.aiVulnerability(ContractType.Permanent)
  }

  "sectorMix" should "sum to 1.0 for all sectors" in {
    for s <- 0 until 6 do
      val (p, z, b) = ContractType.sectorMix(s)
      decimal(p + z + b) shouldBe BigDecimal("1.0") +- BigDecimal("0.001")
  }

  it should "have high B2B in BPO sector" in {
    val (_, _, b2b) = ContractType.sectorMix(0) // BPO
    decimal(b2b) should be > BigDecimal("0.3")
  }

  it should "have high Permanent in Public sector" in {
    val (perm, _, _) = ContractType.sectorMix(4) // Public
    decimal(perm) should be > BigDecimal("0.8")
  }

  "sampleForSector" should "draw deterministically for a seeded stream" in {
    val firstRng  = RandomStream.seeded(293)
    val secondRng = RandomStream.seeded(293)
    val first     = (0 until 20).map(_ => ContractType.sampleForSector(SectorIdx(0), firstRng)).toVector
    val second    = (0 until 20).map(_ => ContractType.sampleForSector(SectorIdx(0), secondRng)).toVector
    first shouldBe second
  }

  it should "sample all contract types from a mixed sector" in {
    val rng     = RandomStream.seeded(293)
    val samples = (0 until 1000).map(_ => ContractType.sampleForSector(SectorIdx(0), rng)).toSet
    samples should contain(ContractType.Permanent)
    samples should contain(ContractType.Zlecenie)
    samples should contain(ContractType.B2B)
  }
