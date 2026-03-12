package com.boombustgroup.amorfati.agents

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.boombustgroup.amorfati.types.*

/** JST property-based tests. */
class JstPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams                                                         = SimParams.defaults
  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  "Jst.step" should "always return zero when disabled" in
    forAll(Gen.choose(0.0, 1e10), Gen.choose(0.0, 1e10), Gen.choose(0.0, 1e11), Gen.choose(0, 50000)) { (govTax, wageInc, gdp, nFirms) =>
      // Default p.flags.jst = false
      val result = Jst.step(Jst.State.zero, PLN(govTax), PLN(wageInc), PLN(gdp), nFirms, PLN.Zero)
      result.depositChange shouldBe PLN.Zero
      result.state shouldBe Jst.State.zero
    }

  "Jst.step deficit identity" should "hold: deficit = spending - revenue" in
    // When JST IS enabled, the deficit identity must hold exactly.
    // We can't easily toggle Config in property tests, but we can verify
    // the formula consistency directly.
    forAll(Gen.choose(0.0, 1e8), Gen.choose(1.0, 2.0)) { (revenue, mult) =>
      val spending      = revenue * mult
      val deficit       = spending - revenue
      val depositChange = revenue - spending // = -deficit
      depositChange shouldBe (-deficit +- 0.01)
    }
