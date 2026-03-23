package com.boombustgroup.amorfati

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

class OpaqueTypesSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  private val td = ComputationBoundary

  // === BankId ===

  "BankId" should "wrap and unwrap Int" in {
    BankId(5).toInt shouldBe 5
  }

  it should "compare with Int via toInt" in {
    BankId(3).toInt shouldBe 3
    (BankId(3).toInt == 4) shouldBe false
  }

  it should "have NoBank sentinel" in {
    BankId.NoBank.toInt shouldBe -1
  }

  // === FirmId ===

  "FirmId" should "wrap and unwrap Int" in {
    FirmId(42).toInt shouldBe 42
  }

  // === SectorIdx ===

  "SectorIdx" should "wrap and unwrap Int" in {
    SectorIdx(3).toInt shouldBe 3
  }

  // === PLN arithmetic ===

  "PLN" should "add two amounts" in {
    val a = PLN(100.0)
    val b = PLN(50.0)
    (a + b) shouldBe PLN(150.0)
  }

  it should "subtract two amounts" in {
    (PLN(100.0) - PLN(30.0)) shouldBe PLN(70.0)
  }

  it should "multiply by scalar" in {
    (PLN(100.0) * Share(0.5)) shouldBe PLN(50.0)
  }

  it should "multiply by Rate" in {
    val amount = PLN(1200.0)
    val rate   = Rate(0.06)
    td.toDouble(amount * rate) shouldBe 72.0 +- 1e-10
  }

  it should "divide amount by amount yielding Double ratio" in {
    val ratio: Double = PLN(75.0) / PLN(100.0)
    ratio shouldBe 0.75
  }

  it should "divide by scalar" in {
    (PLN(120.0) / 12L) shouldBe PLN(10.0)
  }

  it should "negate" in {
    (-PLN(50.0)) shouldBe PLN(-50.0)
  }

  it should "compute abs" in {
    PLN(-30.0).abs shouldBe PLN(30.0)
  }

  it should "compute max and min" in {
    PLN(10.0).max(PLN(20.0)) shouldBe PLN(20.0)
    PLN(10.0).min(PLN(20.0)) shouldBe PLN(10.0)
  }

  it should "compare with > < >= <=" in {
    PLN(10.0) > PLN(5.0) shouldBe true
    PLN(5.0) < PLN(10.0) shouldBe true
    PLN(5.0) >= PLN(5.0) shouldBe true
    PLN(5.0) <= PLN(5.0) shouldBe true
  }

  it should "have Zero constant" in {
    PLN.Zero shouldBe PLN(0.0)
  }

  // === Rate arithmetic ===

  "Rate" should "add two rates" in {
    td.toDouble(Rate(0.05) + Rate(0.01)) shouldBe 0.06 +- 1e-15
  }

  it should "subtract rates" in {
    td.toDouble(Rate(0.05) - Rate(0.02)) shouldBe 0.03 +- 1e-15
  }

  it should "multiply by scalar" in {
    td.toDouble(Rate(0.06) * Multiplier(0.5)) shouldBe 0.03 +- 1e-3
  }

  it should "divide by scalar" in {
    Rate(0.06).monthly
    td.toDouble(Rate(0.06).monthly) shouldBe 0.005 +- 1e-3
  }

  it should "negate" in {
    td.toDouble(-Rate(0.05)) shouldBe -0.05 +- 1e-15
  }

  it should "compute abs, max, min" in {
    td.toDouble(Rate(-0.02).abs) shouldBe 0.02 +- 1e-15
    td.toDouble(Rate(0.03).max(Rate(0.05))) shouldBe 0.05 +- 1e-15
    td.toDouble(Rate(0.03).min(Rate(0.05))) shouldBe 0.03 +- 1e-15
  }

  it should "compare with > <" in {
    Rate(0.05) > Rate(0.03) shouldBe true
    Rate(0.03) < Rate(0.05) shouldBe true
  }

  // === Share arithmetic ===

  "Share" should "add two shares" in {
    td.toDouble(Share(0.3) + Share(0.2)) shouldBe 0.5 +- 1e-9
  }

  it should "subtract shares" in {
    td.toDouble(Share(0.8) - Share(0.3)) shouldBe 0.5 +- 1e-9
  }

  it should "multiply by scalar" in {
    td.toDouble(Share(0.5) * Share(2.0)) shouldBe 1.0 +- 1e-9
  }

  it should "multiply share by share" in {
    td.toDouble(Share(0.5) * Share(0.4)) shouldBe 0.2 +- 1e-9
  }

  it should "compare with > <" in {
    Share(0.8) > Share(0.2) shouldBe true
    Share(0.2) < Share(0.8) shouldBe true
  }

  it should "have Zero and One constants" in {
    Share.Zero shouldBe Share(0.0)
    td.toDouble(Share.One) shouldBe 1.0 +- 1e-9
  }
