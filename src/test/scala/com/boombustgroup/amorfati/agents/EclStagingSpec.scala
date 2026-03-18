package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EclStagingSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  private val loans = PLN(10e9)
  private val init  = EclStaging.State(loans, PLN.Zero, PLN.Zero) // all S1

  "migrationRate" should "be zero when unemployment below NAIRU and GDP growing" in {
    val rate = EclStaging.migrationRate(Ratio(0.03), 0.01)
    rate.toDouble shouldBe 0.0 +- 0.001
  }

  it should "increase with unemployment above NAIRU" in {
    val low  = EclStaging.migrationRate(Ratio(0.06), 0.0)
    val high = EclStaging.migrationRate(Ratio(0.10), 0.0)
    high.toDouble should be > low.toDouble
  }

  it should "increase with GDP contraction" in {
    val growing     = EclStaging.migrationRate(Ratio(0.05), 0.01)
    val contracting = EclStaging.migrationRate(Ratio(0.05), -0.02)
    contracting.toDouble should be > growing.toDouble
  }

  it should "not exceed maxMigration" in {
    val extreme = EclStaging.migrationRate(Ratio(0.50), -0.20)
    extreme.toDouble should be <= summon[SimParams].banking.eclMaxMigration.toDouble
  }

  "EclStaging.step" should "keep all loans in S1 when economy is stable" in {
    val result = EclStaging.step(init, loans, PLN.Zero, Ratio(0.04), 0.01)
    result.newStaging.stage1.toDouble shouldBe loans.toDouble +- 1.0
    result.newStaging.stage2.toDouble shouldBe 0.0 +- 1.0
  }

  it should "migrate S1→S2 when unemployment rises" in {
    val result = EclStaging.step(init, loans, PLN.Zero, Ratio(0.10), 0.0)
    result.newStaging.stage2.toDouble should be > 0.0
    result.newStaging.stage1.toDouble should be < loans.toDouble
  }

  it should "move defaults to S3" in {
    val nplNew = PLN(100e6)
    val s2Init = EclStaging.State(loans - nplNew, nplNew, PLN.Zero)
    val result = EclStaging.step(s2Init, loans, nplNew, Ratio(0.04), 0.01)
    result.newStaging.stage3.toDouble should be > 0.0
  }

  it should "increase provisions when loans migrate S1→S2" in {
    val result = EclStaging.step(init, loans, PLN.Zero, Ratio(0.10), -0.02)
    // S1→S2 migration → higher provisions → positive provisionChange
    result.provisionChange.toDouble should be > 0.0
  }

  it should "preserve total loans across stages" in {
    val result = EclStaging.step(init, loans, PLN(50e6), Ratio(0.08), -0.01)
    val total  = result.newStaging.stage1 + result.newStaging.stage2 + result.newStaging.stage3
    total.toDouble shouldBe loans.toDouble +- 1.0
  }

  it should "cure some S3 loans back to S2" in {
    val s3Init = EclStaging.State(PLN.Zero, PLN.Zero, loans)
    val result = EclStaging.step(s3Init, loans, PLN.Zero, Ratio(0.04), 0.01)
    result.newStaging.stage2.toDouble should be > 0.0
    result.newStaging.stage3.toDouble should be < loans.toDouble
  }
