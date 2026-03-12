package com.boombustgroup.amorfati.engine

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.boombustgroup.amorfati.engine.markets.HousingMarket
import com.boombustgroup.amorfati.types.*

class HousingMarketPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams = SimParams.defaults

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  private val genHousingState: Gen[HousingMarket.State] = for
    hpi      <- Gen.choose(50.0, 200.0)
    value    <- Gen.choose(1e9, 1e13)
    mortgage <- Gen.choose(0.0, 1e12)
    rate     <- Gen.choose(0.02, 0.15)
    wealth   <- Gen.choose(0.0, 1e13)
  yield HousingMarket.State(
    hpi,
    PLN(value),
    PLN(mortgage),
    Rate(rate),
    PLN(wealth),
    PLN.Zero,
    PLN.Zero,
    PLN.Zero,
    PLN.Zero,
    Rate.Zero,
    PLN.Zero,
  )

  @annotation.nowarn("msg=unused private member") // defaults used by callers
  private def mkFlows(
      interest: Double = 0.0,
      principal: Double = 0.0,
      defaultAmount: Double = 0.0,
      defaultLoss: Double = 0.0,
  ): HousingMarket.MortgageFlows =
    HousingMarket.MortgageFlows(PLN(interest), PLN(principal), PLN(defaultAmount), PLN(defaultLoss))

  // --- applyFlows properties ---

  "HousingMarket.applyFlows" should "never produce negative mortgage stock" in
    forAll(genHousingState, Gen.choose(0.0, 1e10), Gen.choose(0.0, 1e10), Gen.choose(0.0, 1e8)) { (state, principal, defaultAmt, interest) =>
      val result = HousingMarket.applyFlows(state, mkFlows(interest, principal, defaultAmt))
      result.mortgageStock.toDouble should be >= 0.0
    }

  it should "always decrease or maintain mortgage stock" in
    forAll(genHousingState, Gen.choose(0.0, 1e8), Gen.choose(0.0, 1e8), Gen.choose(0.0, 1e6)) { (state, principal, defaultAmt, interest) =>
      val result = HousingMarket.applyFlows(state, mkFlows(interest, principal, defaultAmt))
      result.mortgageStock.toDouble should be <= state.mortgageStock.toDouble
    }

  it should "compute housing wealth as totalValue - mortgageStock" in
    forAll(genHousingState, Gen.choose(0.0, 1e6), Gen.choose(0.0, 1e6), Gen.choose(0.0, 1e6)) { (state, principal, defaultAmt, interest) =>
      val result = HousingMarket.applyFlows(state, mkFlows(interest, principal, defaultAmt))
      result.hhHousingWealth.toDouble shouldBe ((result.totalValue - result.mortgageStock).toDouble +- 1.0)
    }

  it should "track repayment and default amounts correctly" in
    forAll(genHousingState, Gen.choose(0.0, 1e6), Gen.choose(0.0, 1e6), Gen.choose(0.0, 1e6)) { (state, principal, defaultAmt, interest) =>
      val result = HousingMarket.applyFlows(state, mkFlows(interest, principal, defaultAmt))
      result.lastRepayment.toDouble shouldBe principal
      result.lastDefault.toDouble shouldBe defaultAmt
      result.mortgageInterestIncome.toDouble shouldBe interest
    }

  // --- processMortgageFlows properties ---

  "HousingMarket.processMortgageFlows" should "return zeros when RE disabled (default)" in
    forAll(genHousingState, Gen.choose(0.02, 0.15), Gen.choose(0.0, 0.50)) { (state, rate, unempRate) =>
      val flows = HousingMarket.processMortgageFlows(state, Rate(rate), Ratio(unempRate))
      // RE_ENABLED is false by default -> all zeros
      flows.interest shouldBe PLN.Zero
      flows.principal shouldBe PLN.Zero
      flows.defaultLoss shouldBe PLN.Zero
    }

  // --- Zero state invariant ---

  "HousingMarket.zero" should "have all fields equal to zero" in {
    val z = HousingMarket.zero
    z.priceIndex shouldBe 0.0
    z.totalValue shouldBe PLN.Zero
    z.mortgageStock shouldBe PLN.Zero
    z.avgMortgageRate.toDouble shouldBe 0.0
    z.hhHousingWealth shouldBe PLN.Zero
    z.lastOrigination shouldBe PLN.Zero
    z.lastRepayment shouldBe PLN.Zero
    z.lastDefault shouldBe PLN.Zero
    z.lastWealthEffect shouldBe PLN.Zero
    z.monthlyReturn.toDouble shouldBe 0.0
    z.mortgageInterestIncome shouldBe PLN.Zero
    z.regions shouldBe None
  }

  // --- Mortgage stock identity property ---

  "Mortgage stock identity" should "hold: Δstock = origination - principal - defaultAmt" in
    forAll(genHousingState, Gen.choose(0.0, 1e6), Gen.choose(0.0, 1e6), Gen.choose(0.0, 1e6)) { (state, origination, principal, defaultAmt) =>
      whenever(state.mortgageStock.toDouble > principal + defaultAmt) {
        val stateAfterOrig = state.copy(
          mortgageStock = PLN(state.mortgageStock.toDouble + origination),
          lastOrigination = PLN(origination),
        )
        val result         = HousingMarket.applyFlows(stateAfterOrig, mkFlows(principal = principal, defaultAmount = defaultAmt))
        val expectedStock  = state.mortgageStock.toDouble + origination - principal - defaultAmt
        result.mortgageStock.toDouble shouldBe (expectedStock +- 1.0)
      }
    }

  // --- Regional housing property tests ---

  private val valueShares    = Vector(0.25, 0.08, 0.07, 0.08, 0.04, 0.05, 0.43)
  private val mortgageShares = Vector(0.30, 0.10, 0.08, 0.09, 0.04, 0.06, 0.33)
  private val hpis           = Vector(230.0, 190.0, 170.0, 175.0, 110.0, 140.0, 100.0)

  private def makeRegionalState(aggValue: Double, aggMortgage: Double): HousingMarket.State =
    val regions = (0 until 7).map { r =>
      HousingMarket.RegionalState(
        priceIndex = hpis(r),
        totalValue = PLN(aggValue * valueShares(r)),
        mortgageStock = PLN(aggMortgage * mortgageShares(r)),
        lastOrigination = PLN.Zero,
        lastRepayment = PLN.Zero,
        lastDefault = PLN.Zero,
        monthlyReturn = Rate.Zero,
      )
    }.toVector
    HousingMarket.State(
      100.0,
      PLN(aggValue),
      PLN(aggMortgage),
      Rate(0.08),
      PLN(aggValue - aggMortgage),
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      Rate.Zero,
      PLN.Zero,
      Some(regions),
    )

  "applyFlows with regions" should "never produce negative regional mortgage stock" in
    forAll(
      Gen.choose(1e9, 1e12),
      Gen.choose(1e8, 5e11),
      Gen.choose(0.0, 1e8),
      Gen.choose(0.0, 1e8),
      Gen.choose(0.0, 1e6),
    ) { (value, mortgage, principal, defaultAmt, interest) =>
      whenever(mortgage < value) {
        val state  = makeRegionalState(value, mortgage)
        val result = HousingMarket.applyFlows(state, mkFlows(interest, principal, defaultAmt))
        result.regions.get.foreach { r =>
          r.mortgageStock.toDouble should be >= 0.0
        }
      }
    }

  it should "have regional stocks summing to aggregate" in
    forAll(
      Gen.choose(1e9, 1e12),
      Gen.choose(1e8, 5e11),
      Gen.choose(0.0, 1e6),
      Gen.choose(0.0, 1e6),
      Gen.choose(0.0, 1e6),
    ) { (value, mortgage, principal, defaultAmt, interest) =>
      whenever(mortgage < value && mortgage > principal + defaultAmt) {
        val state  = makeRegionalState(value, mortgage)
        val result = HousingMarket.applyFlows(state, mkFlows(interest, principal, defaultAmt))
        val regSum = result.regions.get.map(_.mortgageStock.toDouble).sum
        regSum shouldBe (result.mortgageStock.toDouble +- 1.0)
      }
    }

  it should "have regional repayments summing to aggregate" in
    forAll(Gen.choose(1e9, 1e12), Gen.choose(1e8, 5e11), Gen.choose(0.0, 1e6), Gen.choose(0.0, 1e6)) { (value, mortgage, principal, defaultAmt) =>
      whenever(mortgage < value && mortgage > 0) {
        val state         = makeRegionalState(value, mortgage)
        val result        = HousingMarket.applyFlows(state, mkFlows(principal = principal, defaultAmount = defaultAmt))
        val regRepaySum   = result.regions.get.map(_.lastRepayment.toDouble).sum
        regRepaySum shouldBe (principal +- 0.01)
        val regDefaultSum = result.regions.get.map(_.lastDefault.toDouble).sum
        regDefaultSum shouldBe (defaultAmt +- 0.01)
      }
    }

  it should "preserve None regions when state has no regions" in
    forAll(genHousingState, Gen.choose(0.0, 1e6), Gen.choose(0.0, 1e6), Gen.choose(0.0, 1e6)) { (state, principal, defaultAmt, interest) =>
      val result = HousingMarket.applyFlows(state, mkFlows(interest, principal, defaultAmt))
      result.regions shouldBe None
    }

  "Mortgage stock identity with regions" should "hold at aggregate level" in
    forAll(
      Gen.choose(1e9, 1e12),
      Gen.choose(1e8, 5e11),
      Gen.choose(0.0, 1e6),
      Gen.choose(0.0, 1e5),
      Gen.choose(0.0, 1e5),
    ) { (value, mortgage, origination, principal, defaultAmt) =>
      whenever(mortgage < value && mortgage > principal + defaultAmt) {
        val state          = makeRegionalState(value, mortgage)
        val updatedRegs    = state.regions.get.zipWithIndex.map { (r, i) =>
          val rOrig = origination * valueShares(i)
          r.copy(mortgageStock = PLN(r.mortgageStock.toDouble + rOrig), lastOrigination = PLN(rOrig))
        }
        val stateAfterOrig = state.copy(
          mortgageStock = PLN(mortgage + origination),
          lastOrigination = PLN(origination),
          regions = Some(updatedRegs),
        )
        val result         = HousingMarket.applyFlows(stateAfterOrig, mkFlows(principal = principal, defaultAmount = defaultAmt))
        val expectedStock  = mortgage + origination - principal - defaultAmt
        result.mortgageStock.toDouble shouldBe (expectedStock +- 1.0)
        val regSum         = result.regions.get.map(_.mortgageStock.toDouble).sum
        regSum shouldBe (result.mortgageStock.toDouble +- 1.0)
      }
    }
