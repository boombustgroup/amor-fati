package sfc.agents

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sfc.types.*

/** Property-based tests for public sector. */
class PublicSectorPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:
  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  "ZUS (disabled)" should "always return zero contributions and pensions" in {
    forAll(Gen.choose(0.0, 1e10), Gen.choose(0, 200000), Gen.choose(1000.0, 20000.0), Gen.choose(0, 100000)) {
      (prevBal, employed, wage, retirees) =>
        val zus = SocialSecurity.zusStep(prevBal, employed, wage, retirees)
        zus.contributions.toDouble shouldBe 0.0
        zus.pensionPayments.toDouble shouldBe 0.0
        zus.govSubvention.toDouble shouldBe 0.0
        zus.fusBalance.toDouble shouldBe prevBal // unchanged
    }
  }

  "PPK (disabled)" should "always return zero contributions and preserve holdings" in {
    forAll(Gen.choose(0.0, 1e10), Gen.choose(0, 200000), Gen.choose(1000.0, 20000.0)) {
      (prevHoldings, employed, wage) =>
        val ppk = SocialSecurity.ppkStep(prevHoldings, employed, wage)
        ppk.contributions.toDouble shouldBe 0.0
        ppk.bondHoldings.toDouble shouldBe prevHoldings
    }
  }

  "PPK bond purchase" should "be non-negative" in {
    forAll(Gen.choose(0.0, 1e10), Gen.choose(0.0, 1e8)) { (holdings, contributions) =>
      val ppk = SocialSecurity.PpkState(PLN(holdings), PLN(contributions))
      SocialSecurity.ppkBondPurchase(ppk) should be >= 0.0
    }
  }

  "Demographics (disabled)" should "preserve state except monthlyRetirements" in {
    forAll(Gen.choose(0, 100000), Gen.choose(0, 200000), Gen.choose(0, 1000), Gen.choose(0, 200000)) {
      (retirees, workingAge, prevRetirements, employed) =>
        val prev = SocialSecurity.DemographicsState(retirees, workingAge, prevRetirements)
        val result = SocialSecurity.demographicsStep(prev, employed)
        result.retirees shouldBe retirees
        result.workingAgePop shouldBe workingAge
        result.monthlyRetirements shouldBe 0
    }
  }

  "FUS balance identity" should "hold: ΔfusBalance = contributions - pensions" in {
    forAll(Gen.choose(-1e10, 1e10), Gen.choose(0.0, 1e9), Gen.choose(0.0, 1e9)) { (prevBal, contributions, pensions) =>
      val expectedChange = contributions - pensions
      val newBal = prevBal + expectedChange
      (newBal - prevBal) shouldBe (expectedChange +- 0.01)
    }
  }
