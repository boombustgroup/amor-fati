package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen
import sfc.config.Config

class HousingMarketPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  private val genHousingState: Gen[HousingMarketState] = for
    hpi       <- Gen.choose(50.0, 200.0)
    value     <- Gen.choose(1e9, 1e13)
    mortgage  <- Gen.choose(0.0, 1e12)
    rate      <- Gen.choose(0.02, 0.15)
    wealth    <- Gen.choose(0.0, 1e13)
  yield HousingMarketState(hpi, value, mortgage, rate, wealth, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)

  // --- applyFlows properties ---

  "HousingMarket.applyFlows" should "never produce negative mortgage stock" in {
    forAll(genHousingState, Gen.choose(0.0, 1e10), Gen.choose(0.0, 1e10), Gen.choose(0.0, 1e8)) {
      (state, principal, defaultAmt, interest) =>
        val result = HousingMarket.applyFlows(state, principal, defaultAmt, interest)
        result.mortgageStock should be >= 0.0
    }
  }

  it should "always decrease or maintain mortgage stock" in {
    forAll(genHousingState, Gen.choose(0.0, 1e8), Gen.choose(0.0, 1e8), Gen.choose(0.0, 1e6)) {
      (state, principal, defaultAmt, interest) =>
        val result = HousingMarket.applyFlows(state, principal, defaultAmt, interest)
        result.mortgageStock should be <= state.mortgageStock
    }
  }

  it should "compute housing wealth as totalValue - mortgageStock" in {
    forAll(genHousingState, Gen.choose(0.0, 1e6), Gen.choose(0.0, 1e6), Gen.choose(0.0, 1e6)) {
      (state, principal, defaultAmt, interest) =>
        val result = HousingMarket.applyFlows(state, principal, defaultAmt, interest)
        result.hhHousingWealth shouldBe (result.totalValue - result.mortgageStock +- 1.0)
    }
  }

  it should "track repayment and default amounts correctly" in {
    forAll(genHousingState, Gen.choose(0.0, 1e6), Gen.choose(0.0, 1e6), Gen.choose(0.0, 1e6)) {
      (state, principal, defaultAmt, interest) =>
        val result = HousingMarket.applyFlows(state, principal, defaultAmt, interest)
        result.lastRepayment shouldBe principal
        result.lastDefault shouldBe defaultAmt
        result.mortgageInterestIncome shouldBe interest
    }
  }

  // --- processMortgageFlows properties ---

  "HousingMarket.processMortgageFlows" should "return zeros when RE disabled (default)" in {
    forAll(genHousingState, Gen.choose(0.02, 0.15), Gen.choose(0.0, 0.50)) {
      (state, rate, unempRate) =>
        val (interest, principal, defLoss) = HousingMarket.processMortgageFlows(state, rate, unempRate)
        // RE_ENABLED is false by default → all zeros
        interest shouldBe 0.0
        principal shouldBe 0.0
        defLoss shouldBe 0.0
    }
  }

  // --- Zero state invariant ---

  "HousingMarket.zero" should "have all fields equal to zero" in {
    val z = HousingMarket.zero
    z.priceIndex shouldBe 0.0
    z.totalValue shouldBe 0.0
    z.mortgageStock shouldBe 0.0
    z.avgMortgageRate shouldBe 0.0
    z.hhHousingWealth shouldBe 0.0
    z.lastOrigination shouldBe 0.0
    z.lastRepayment shouldBe 0.0
    z.lastDefault shouldBe 0.0
    z.lastWealthEffect shouldBe 0.0
    z.monthlyReturn shouldBe 0.0
    z.mortgageInterestIncome shouldBe 0.0
  }

  // --- Mortgage stock identity property ---

  "Mortgage stock identity" should "hold: Δstock = origination - principal - defaultAmt" in {
    forAll(genHousingState, Gen.choose(0.0, 1e6), Gen.choose(0.0, 1e6), Gen.choose(0.0, 1e6)) {
      (state, origination, principal, defaultAmt) =>
        whenever(state.mortgageStock > principal + defaultAmt) {
          val stateAfterOrig = state.copy(
            mortgageStock = state.mortgageStock + origination,
            lastOrigination = origination
          )
          val result = HousingMarket.applyFlows(stateAfterOrig, principal, defaultAmt, 0.0)
          val expectedStock = state.mortgageStock + origination - principal - defaultAmt
          result.mortgageStock shouldBe (expectedStock +- 1.0)
        }
    }
  }
