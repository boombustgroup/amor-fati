package com.boombustgroup.amorfati.agents

import org.scalacheck.Gen
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
    forAll(Gen.choose(0.0, 1e10), Gen.choose(0.0, 1e8)) { (holdings, contributions) =>
      val ppk = SocialSecurity.PpkState(PLN(holdings), PLN(contributions))
      SocialSecurity.ppkBondPurchase(ppk) should be >= PLN.Zero
    }

  "FUS balance identity" should "hold: ΔfusBalance = contributions - pensions" in
    forAll(Gen.choose(-1e10, 1e10), Gen.choose(0.0, 1e9), Gen.choose(0.0, 1e9)) { (prevBal, contributions, pensions) =>
      val expectedChange = contributions - pensions
      val newBal         = prevBal + expectedChange
      (newBal - prevBal) shouldBe (expectedChange +- 0.01)
    }
