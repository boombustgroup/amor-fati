package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.fp.ComputationBoundary
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EclStagingSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults
  private val td  = ComputationBoundary

  private val loans = PLN(10e9)
  private val init  = EclStaging.State(loans, PLN.Zero, PLN.Zero) // all S1

  "migrationRate" should "be zero when unemployment below NAIRU and GDP growing" in {
    val rate = EclStaging.migrationRate(Share(0.03), Coefficient(0.01))
    td.toDouble(rate) shouldBe 0.0 +- 0.001
  }

  it should "increase with unemployment above NAIRU" in {
    val low  = EclStaging.migrationRate(Share(0.06), Coefficient.Zero)
    val high = EclStaging.migrationRate(Share(0.10), Coefficient.Zero)
    td.toDouble(high) should be > td.toDouble(low)
  }

  it should "increase with GDP contraction" in {
    val growing     = EclStaging.migrationRate(Share(0.05), Coefficient(0.01))
    val contracting = EclStaging.migrationRate(Share(0.05), Coefficient(-0.02))
    td.toDouble(contracting) should be > td.toDouble(growing)
  }

  it should "not exceed maxMigration" in {
    val extreme = EclStaging.migrationRate(Share(0.50), Coefficient(-0.20))
    td.toDouble(extreme) should be <= td.toDouble(summon[SimParams].banking.eclMaxMigration)
  }

  "EclStaging.step" should "keep all loans in S1 when economy is stable" in {
    val result = EclStaging.step(init, loans, PLN.Zero, Share(0.04), Coefficient(0.01))
    td.toDouble(result.newStaging.stage1) shouldBe td.toDouble(loans) +- 1.0
    td.toDouble(result.newStaging.stage2) shouldBe 0.0 +- 1.0
  }

  it should "migrate S1->S2 when unemployment rises" in {
    val result = EclStaging.step(init, loans, PLN.Zero, Share(0.10), Coefficient.Zero)
    td.toDouble(result.newStaging.stage2) should be > 0.0
    td.toDouble(result.newStaging.stage1) should be < td.toDouble(loans)
  }

  it should "move defaults to S3" in {
    val nplNew = PLN(100e6)
    val s2Init = EclStaging.State(loans - nplNew, nplNew, PLN.Zero)
    val result = EclStaging.step(s2Init, loans, nplNew, Share(0.04), Coefficient(0.01))
    td.toDouble(result.newStaging.stage3) should be > 0.0
  }

  it should "increase provisions when loans migrate S1->S2" in {
    val result = EclStaging.step(init, loans, PLN.Zero, Share(0.10), Coefficient(-0.02))
    // S1->S2 migration -> higher provisions -> positive provisionChange
    td.toDouble(result.provisionChange) should be > 0.0
  }

  it should "preserve total loans across stages" in {
    val result = EclStaging.step(init, loans, PLN(50e6), Share(0.08), Coefficient(-0.01))
    val total  = result.newStaging.stage1 + result.newStaging.stage2 + result.newStaging.stage3
    td.toDouble(total) shouldBe td.toDouble(loans) +- 1.0
  }

  it should "cure some S3 loans back to S2" in {
    val s3Init = EclStaging.State(PLN.Zero, PLN.Zero, loans)
    val result = EclStaging.step(s3Init, loans, PLN.Zero, Share(0.04), Coefficient(0.01))
    td.toDouble(result.newStaging.stage2) should be > 0.0
    td.toDouble(result.newStaging.stage3) should be < td.toDouble(loans)
  }
