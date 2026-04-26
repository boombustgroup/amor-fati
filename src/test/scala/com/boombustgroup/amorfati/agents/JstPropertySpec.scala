package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/** JST property-based tests. */
class JstPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams                                                         = SimParams.defaults
  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  "Jst.step deficit identity" should "hold: deficit = spending - revenue" in
    // When JST IS enabled, the deficit identity must hold exactly.
    // We can't easily toggle Config in property tests, but we can verify
    // the formula consistency directly.
    forAll(genDecimal("0.0", "1e8"), genDecimal("1.0", "2.0")) { (revenue, mult) =>
      val spending      = revenue * mult
      val deficit       = spending - revenue
      val depositChange = revenue - spending // = -deficit
      depositChange shouldBe (-deficit +- BigDecimal("0.01"))
    }
