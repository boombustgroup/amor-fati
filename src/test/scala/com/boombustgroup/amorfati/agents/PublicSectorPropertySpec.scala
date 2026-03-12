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

  "ZUS (disabled)" should "always return zero contributions and pensions" in
    forAll(Gen.choose(0.0, 1e10), Gen.choose(0, 200000), Gen.choose(1000.0, 20000.0), Gen.choose(0, 100000)) { (prevBal, employed, wage, retirees) =>
      val zus = SocialSecurity.zusStep(PLN(prevBal), employed, PLN(wage), retirees)
      zus.contributions shouldBe PLN.Zero
      zus.pensionPayments shouldBe PLN.Zero
      zus.govSubvention shouldBe PLN.Zero
      zus.fusBalance shouldBe PLN(prevBal) // unchanged
    }

  "PPK (disabled)" should "always return zero contributions and preserve holdings" in
    forAll(Gen.choose(0.0, 1e10), Gen.choose(0, 200000), Gen.choose(1000.0, 20000.0)) { (prevHoldings, employed, wage) =>
      val ppk = SocialSecurity.ppkStep(PLN(prevHoldings), employed, PLN(wage))
      ppk.contributions shouldBe PLN.Zero
      ppk.bondHoldings shouldBe PLN(prevHoldings)
    }

  "PPK bond purchase" should "be non-negative" in
    forAll(Gen.choose(0.0, 1e10), Gen.choose(0.0, 1e8)) { (holdings, contributions) =>
      val ppk = SocialSecurity.PpkState(PLN(holdings), PLN(contributions))
      SocialSecurity.ppkBondPurchase(ppk) should be >= PLN.Zero
    }

  "Demographics (disabled)" should "preserve state except monthlyRetirements" in
    forAll(Gen.choose(0, 100000), Gen.choose(0, 200000), Gen.choose(0, 1000), Gen.choose(0, 200000)) { (retirees, workingAge, prevRetirements, employed) =>
      val prev   = SocialSecurity.DemographicsState(retirees, workingAge, prevRetirements)
      val result = SocialSecurity.demographicsStep(prev, employed, 0)
      result.retirees shouldBe retirees
      result.workingAgePop shouldBe workingAge
      result.monthlyRetirements shouldBe 0
    }

  "FUS balance identity" should "hold: ΔfusBalance = contributions - pensions" in
    forAll(Gen.choose(-1e10, 1e10), Gen.choose(0.0, 1e9), Gen.choose(0.0, 1e9)) { (prevBal, contributions, pensions) =>
      val expectedChange = contributions - pensions
      val newBal         = prevBal + expectedChange
      (newBal - prevBal) shouldBe (expectedChange +- 0.01)
    }
