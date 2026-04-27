package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.boombustgroup.amorfati.types.*

/** Property-based tests for public sector. */
class PublicSectorPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams                                                         = SimParams.defaults
  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  "PPK bond purchase" should "be non-negative" in
    forAll(genDecimal("0.0", "1e8")) { contributions =>
      val ppk = SocialSecurity.PpkState(plnBD(contributions))
      SocialSecurity.ppkBondPurchase(ppk) should be >= PLN.Zero
    }
