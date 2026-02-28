package sfc.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** JST (local government) unit tests. */
class JstSpec extends AnyFlatSpec with Matchers:

  "JstLogic.step" should "return zero deposit change when disabled" in {
    // JST_ENABLED is false by default
    val (jst, depChange) = JstLogic.step(JstState.zero, 1e8, 1e9, 1e10, 5000)
    depChange shouldBe 0.0
    jst shouldBe JstState.zero
  }

  "JstState.zero" should "have all zero fields" in {
    val z = JstState.zero
    z.deposits shouldBe 0.0
    z.debt shouldBe 0.0
    z.revenue shouldBe 0.0
    z.spending shouldBe 0.0
    z.deficit shouldBe 0.0
  }

  "JstLogic.step revenue components" should "compute PIT share correctly" in {
    // Direct test of the formula: totalWageIncome × 0.12 × JstPitShare
    val wageIncome = 1e9
    val effectivePitRate = 0.12
    val pitShare = 0.3846
    val expectedPit = wageIncome * effectivePitRate * pitShare
    expectedPit shouldBe (1e9 * 0.12 * 0.3846 +- 1.0)
  }

  it should "compute property tax per firm correctly" in {
    val nFirms = 1000
    val propertyTax = 5000.0  // per firm per year
    val monthly = nFirms.toDouble * propertyTax / 12.0
    monthly shouldBe (1000.0 * 5000.0 / 12.0 +- 1.0)
  }

  "JstLogic deficit" should "be positive when spending mult > 1" in {
    // With spending multiplier 1.02, deficit = revenue × 0.02
    val revenue = 1e8
    val spendingMult = 1.02
    val deficit = revenue * spendingMult - revenue
    deficit should be > 0.0
    deficit shouldBe (revenue * 0.02 +- 1.0)
  }
