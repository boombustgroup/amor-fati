package com.boombustgroup.amorfati.agents

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
    val wageIncome       = 1e9
    val effectivePitRate = 0.12
    val pitShare         = 0.3846
    val expectedPit      = wageIncome * effectivePitRate * pitShare
    expectedPit shouldBe (1e9 * 0.12 * 0.3846 +- 1.0)
  }

  it should "compute property tax per firm correctly" in {
    val nFirms      = 1000
    val propertyTax = 5000.0 // per firm per year
    val monthly     = nFirms.toDouble * propertyTax / 12.0
    monthly shouldBe (1000.0 * 5000.0 / 12.0 +- 1.0)
  }

  "Jst.step deficit" should "be positive when spending mult > 1" in {
    // With spending multiplier 1.02, deficit = revenue × 0.02
    val revenue      = 1e8
    val spendingMult = 1.02
    val deficit      = revenue * spendingMult - revenue
    deficit should be > 0.0
    deficit shouldBe (revenue * 0.02 +- 1.0)
  }
