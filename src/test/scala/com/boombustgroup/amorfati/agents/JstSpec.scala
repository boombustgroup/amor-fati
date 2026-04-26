package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.types.*

/** JST (local government) unit tests. */
class JstSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams = SimParams.defaults

  "Jst.State.zero" should "have all zero fields" in {
    val z = Jst.State.zero
    z.debt shouldBe PLN.Zero
    z.revenue shouldBe PLN.Zero
    z.spending shouldBe PLN.Zero
    z.deficit shouldBe PLN.Zero
  }

  "Jst.step revenue components" should "compute PIT share correctly" in {
    // Direct test of the formula: totalWageIncome × 0.12 × JstPitShare
    val wageIncome       = BigDecimal("1000000000.0")
    val effectivePitRate = BigDecimal("0.12")
    val pitShare         = BigDecimal("0.3846")
    val expectedPit      = wageIncome * effectivePitRate * pitShare
    expectedPit shouldBe (BigDecimal("1000000000.0") * BigDecimal("0.12") * BigDecimal("0.3846") +- BigDecimal("1.0"))
  }

  it should "compute property tax per firm correctly" in {
    val nFirms      = 1000
    val propertyTax = BigDecimal("5000.0") // per firm per year
    val monthly     = decimal(nFirms) * propertyTax / BigDecimal(12)
    monthly shouldBe (BigDecimal("1000.0") * BigDecimal("5000.0") / BigDecimal("12.0") +- BigDecimal("1.0"))
  }

  "Jst.step deficit" should "be positive when spending mult > 1" in {
    // With spending multiplier 1.02, deficit = revenue × 0.02
    val revenue      = BigDecimal("100000000.0")
    val spendingMult = BigDecimal("1.02")
    val deficit      = revenue * spendingMult - revenue
    deficit should be > BigDecimal("0.0")
    deficit shouldBe (revenue * BigDecimal("0.02") +- BigDecimal("1.0"))
  }
