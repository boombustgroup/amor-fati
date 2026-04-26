package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EclStagingSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  private val loans = PLN(10000000000L)
  private val init  = EclStaging.State(loans, PLN.Zero, PLN.Zero) // all S1

  "migrationRate" should "be zero when unemployment below NAIRU and GDP growing" in {
    val rate = EclStaging.migrationRate(Share.decimal(3, 2), Coefficient.decimal(1, 2))
    decimal(rate) shouldBe BigDecimal("0.0") +- BigDecimal("0.001")
  }

  it should "increase with unemployment above NAIRU" in {
    val low  = EclStaging.migrationRate(Share.decimal(6, 2), Coefficient.Zero)
    val high = EclStaging.migrationRate(Share.decimal(10, 2), Coefficient.Zero)
    decimal(high) should be > decimal(low)
  }

  it should "increase with GDP contraction" in {
    val growing     = EclStaging.migrationRate(Share.decimal(5, 2), Coefficient.decimal(1, 2))
    val contracting = EclStaging.migrationRate(Share.decimal(5, 2), Coefficient.decimal(-2, 2))
    decimal(contracting) should be > decimal(growing)
  }

  it should "not exceed maxMigration" in {
    val extreme = EclStaging.migrationRate(Share.decimal(50, 2), Coefficient.decimal(-20, 2))
    decimal(extreme) should be <= decimal(summon[SimParams].banking.eclMaxMigration)
  }

  "EclStaging.step" should "keep all loans in S1 when economy is stable" in {
    val result = EclStaging.step(init, loans, PLN.Zero, Share.decimal(4, 2), Coefficient.decimal(1, 2))
    decimal(result.newStaging.stage1) shouldBe decimal(loans) +- BigDecimal("1.0")
    decimal(result.newStaging.stage2) shouldBe BigDecimal("0.0") +- BigDecimal("1.0")
  }

  it should "migrate S1->S2 when unemployment rises" in {
    val result = EclStaging.step(init, loans, PLN.Zero, Share.decimal(10, 2), Coefficient.Zero)
    decimal(result.newStaging.stage2) should be > BigDecimal("0.0")
    decimal(result.newStaging.stage1) should be < decimal(loans)
  }

  it should "move defaults to S3" in {
    val nplNew = PLN(100000000)
    val s2Init = EclStaging.State(loans - nplNew, nplNew, PLN.Zero)
    val result = EclStaging.step(s2Init, loans, nplNew, Share.decimal(4, 2), Coefficient.decimal(1, 2))
    decimal(result.newStaging.stage3) should be > BigDecimal("0.0")
  }

  it should "increase provisions when loans migrate S1->S2" in {
    val result = EclStaging.step(init, loans, PLN.Zero, Share.decimal(10, 2), Coefficient.decimal(-2, 2))
    // S1->S2 migration -> higher provisions -> positive provisionChange
    decimal(result.provisionChange) should be > BigDecimal("0.0")
  }

  it should "preserve total loans across stages" in {
    val result = EclStaging.step(init, loans, PLN(50000000), Share.decimal(8, 2), Coefficient.decimal(-1, 2))
    val total  = result.newStaging.stage1 + result.newStaging.stage2 + result.newStaging.stage3
    decimal(total) shouldBe decimal(loans) +- BigDecimal("1.0")
  }

  it should "cure some S3 loans back to S2" in {
    val s3Init = EclStaging.State(PLN.Zero, PLN.Zero, loans)
    val result = EclStaging.step(s3Init, loans, PLN.Zero, Share.decimal(4, 2), Coefficient.decimal(1, 2))
    decimal(result.newStaging.stage2) should be > BigDecimal("0.0")
    decimal(result.newStaging.stage3) should be < decimal(loans)
  }
