package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.fp.ComputationBoundary
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ContractTypeSpec extends AnyFlatSpec with Matchers:

  private val td = ComputationBoundary

  "zusEmployerRate" should "be highest for Permanent" in {
    td.toDouble(ContractType.zusEmployerRate(ContractType.Permanent)) should be > td.toDouble(ContractType.zusEmployerRate(ContractType.Zlecenie))
    td.toDouble(ContractType.zusEmployerRate(ContractType.Zlecenie)) should be > td.toDouble(ContractType.zusEmployerRate(ContractType.B2B))
  }

  it should "be zero for B2B" in {
    ContractType.zusEmployerRate(ContractType.B2B) shouldBe Share.Zero
  }

  "fpRate" should "be positive only for Permanent" in {
    td.toDouble(ContractType.fpRate(ContractType.Permanent)) should be > 0.0
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
      (p + z + b) shouldBe 1.0 +- 0.001
  }

  it should "have high B2B in BPO sector" in {
    val (_, _, b2b) = ContractType.sectorMix(0) // BPO
    b2b should be > 0.3
  }

  it should "have high Permanent in Public sector" in {
    val (perm, _, _) = ContractType.sectorMix(4) // Public
    perm should be > 0.8
  }
