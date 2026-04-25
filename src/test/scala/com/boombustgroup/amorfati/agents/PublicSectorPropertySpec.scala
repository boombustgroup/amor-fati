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

  "FUS balance identity" should "hold: ΔfusBalance = contributions - pensions" in
    forAll(genDecimal("-1e10", "1e10"), genDecimal("0.0", "1e9"), genDecimal("0.0", "1e9")) { (prevBal, contributions, pensions) =>
      val expectedChange = contributions - pensions
      val newBal         = prevBal + expectedChange
      (newBal - prevBal) shouldBe (expectedChange +- BigDecimal("0.01"))
    }
