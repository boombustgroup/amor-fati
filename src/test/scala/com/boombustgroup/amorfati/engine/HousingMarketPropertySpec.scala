package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.markets.HousingMarket
import com.boombustgroup.amorfati.types.*
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class HousingMarketPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given SimParams = SimParams.defaults

  private val onePln     = PLN.fromLong(1)
  private val hundredPln = PLN.fromLong(100)

  private def pln(units: Long): PLN                                              = PLN.fromLong(units)
  private def rateBps(bps: Long): Rate                                           = Rate.fromRaw(bps)
  private def priceIndex(points: Long): PriceIndex                               = PriceIndex.fromRaw(points * 10000L)
  private def shouldBeClosePln(actual: PLN, expected: PLN, tolerance: PLN): Unit = (actual - expected).abs should be <= tolerance

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  private val genHousingState: Gen[HousingMarket.State] = for
    hpiPoints     <- Gen.chooseNum(50L, 200L)
    valueUnits    <- Gen.chooseNum(1_000_000_000L, 10_000_000_000_000L)
    mortgageUnits <- Gen.chooseNum(0L, 1_000_000_000_000L)
    rateBpsValue  <- Gen.chooseNum(200L, 1500L)
    wealthUnits   <- Gen.chooseNum(0L, 10_000_000_000_000L)
  yield HousingMarket.State(
    priceIndex(hpiPoints),
    pln(valueUnits),
    pln(mortgageUnits),
    rateBps(rateBpsValue),
    pln(wealthUnits),
    PLN.Zero,
    PLN.Zero,
    PLN.Zero,
    PLN.Zero,
    Rate.Zero,
    PLN.Zero,
  )

  @annotation.nowarn("msg=unused private member") // defaults used by callers
  private def mkFlows(
      interest: PLN = PLN.Zero,
      principal: PLN = PLN.Zero,
      defaultAmount: PLN = PLN.Zero,
      defaultLoss: PLN = PLN.Zero,
  ): HousingMarket.MortgageFlows =
    HousingMarket.MortgageFlows(interest, principal, defaultAmount, defaultLoss)

  // --- applyFlows properties ---

  "HousingMarket.applyFlows" should "never produce negative mortgage stock" in
    forAll(genHousingState, Gen.chooseNum(0L, 10_000_000_000L), Gen.chooseNum(0L, 10_000_000_000L), Gen.chooseNum(0L, 100_000_000L)) {
      (state, principalUnits, defaultAmtUnits, interestUnits) =>
        val result = HousingMarket.applyFlows(state, mkFlows(pln(interestUnits), pln(principalUnits), pln(defaultAmtUnits)))
        result.mortgageStock should be >= PLN.Zero
    }

  it should "always decrease or maintain mortgage stock" in
    forAll(genHousingState, Gen.chooseNum(0L, 100_000_000L), Gen.chooseNum(0L, 100_000_000L), Gen.chooseNum(0L, 1_000_000L)) {
      (state, principalUnits, defaultAmtUnits, interestUnits) =>
        val result = HousingMarket.applyFlows(state, mkFlows(pln(interestUnits), pln(principalUnits), pln(defaultAmtUnits)))
        result.mortgageStock should be <= state.mortgageStock
    }

  it should "compute housing wealth as totalValue - mortgageStock" in
    forAll(genHousingState, Gen.chooseNum(0L, 1_000_000L), Gen.chooseNum(0L, 1_000_000L), Gen.chooseNum(0L, 1_000_000L)) {
      (state, principalUnits, defaultAmtUnits, interestUnits) =>
        val result = HousingMarket.applyFlows(state, mkFlows(pln(interestUnits), pln(principalUnits), pln(defaultAmtUnits)))
        shouldBeClosePln(result.hhHousingWealth, result.totalValue - result.mortgageStock, onePln)
    }

  it should "track repayment and default amounts correctly" in
    forAll(genHousingState, Gen.chooseNum(1L, 1_000_000L), Gen.chooseNum(1L, 1_000_000L), Gen.chooseNum(1L, 1_000_000L)) {
      (state, principalUnits, defaultAmtUnits, interestUnits) =>
        val result = HousingMarket.applyFlows(state, mkFlows(pln(interestUnits), pln(principalUnits), pln(defaultAmtUnits)))
        result.lastRepayment shouldBe pln(principalUnits)
        result.lastDefault shouldBe pln(defaultAmtUnits)
        result.mortgageInterestIncome shouldBe pln(interestUnits)
    }

  // --- processMortgageFlows properties ---

  // --- Zero state invariant ---

  "HousingMarket.zero" should "have all fields equal to zero" in {
    val z = HousingMarket.zero
    z.priceIndex shouldBe PriceIndex.Zero
    z.totalValue shouldBe PLN.Zero
    z.mortgageStock shouldBe PLN.Zero
    z.avgMortgageRate shouldBe Rate.Zero
    z.hhHousingWealth shouldBe PLN.Zero
    z.lastOrigination shouldBe PLN.Zero
    z.lastRepayment shouldBe PLN.Zero
    z.lastDefault shouldBe PLN.Zero
    z.lastWealthEffect shouldBe PLN.Zero
    z.monthlyReturn shouldBe Rate.Zero
    z.mortgageInterestIncome shouldBe PLN.Zero
    z.regions shouldBe None
  }

  // --- Mortgage stock identity property ---

  "Mortgage stock identity" should "hold: Δstock = origination - principal - defaultAmt" in
    forAll(genHousingState, Gen.chooseNum(0L, 1_000_000L), Gen.chooseNum(0L, 1_000_000L), Gen.chooseNum(0L, 1_000_000L)) {
      (state, originationUnits, principalUnits, defaultAmtUnits) =>
        val origination = pln(originationUnits)
        val principal   = pln(principalUnits)
        val defaultAmt  = pln(defaultAmtUnits)
        whenever(state.mortgageStock > principal + defaultAmt) {
          val stateAfterOrig = state.copy(
            mortgageStock = state.mortgageStock + origination,
            lastOrigination = origination,
          )
          val result         = HousingMarket.applyFlows(stateAfterOrig, mkFlows(principal = principal, defaultAmount = defaultAmt))
          result.mortgageStock shouldBe state.mortgageStock + origination - principal - defaultAmt
        }
    }

  // --- Regional housing property tests ---

  private val valueShares    = Vector(
    Share.fraction(25, 100),
    Share.fraction(8, 100),
    Share.fraction(7, 100),
    Share.fraction(8, 100),
    Share.fraction(4, 100),
    Share.fraction(5, 100),
    Share.fraction(43, 100),
  )
  private val mortgageShares = Vector(
    Share.fraction(30, 100),
    Share.fraction(10, 100),
    Share.fraction(8, 100),
    Share.fraction(9, 100),
    Share.fraction(4, 100),
    Share.fraction(6, 100),
    Share.fraction(33, 100),
  )
  private val hpis           = Vector(230L, 190L, 170L, 175L, 110L, 140L, 100L)

  private def makeRegionalState(aggValue: PLN, aggMortgage: PLN): HousingMarket.State =
    val regions = (0 until 7).map { r =>
      HousingMarket.RegionalState(
        priceIndex = priceIndex(hpis(r)),
        totalValue = valueShares(r) * aggValue,
        mortgageStock = mortgageShares(r) * aggMortgage,
        lastOrigination = PLN.Zero,
        lastRepayment = PLN.Zero,
        lastDefault = PLN.Zero,
        monthlyReturn = Rate.Zero,
      )
    }.toVector
    HousingMarket.State(
      priceIndex(100),
      aggValue,
      aggMortgage,
      rateBps(800),
      aggValue - aggMortgage,
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
      Gen.chooseNum(1_000_000_000L, 1_000_000_000_000L),
      Gen.chooseNum(100_000_000L, 500_000_000_000L),
      Gen.chooseNum(0L, 100_000_000L),
      Gen.chooseNum(0L, 100_000_000L),
      Gen.chooseNum(0L, 1_000_000L),
    ) { (valueUnits, mortgageUnits, principalUnits, defaultAmtUnits, interestUnits) =>
      val value    = pln(valueUnits)
      val mortgage = pln(mortgageUnits)
      whenever(mortgage < value) {
        val state  = makeRegionalState(value, mortgage)
        val result = HousingMarket.applyFlows(state, mkFlows(pln(interestUnits), pln(principalUnits), pln(defaultAmtUnits)))
        result.regions.get.foreach { r =>
          r.mortgageStock should be >= PLN.Zero
        }
      }
    }

  it should "have regional stocks summing to aggregate" in
    forAll(
      Gen.chooseNum(1_000_000_000L, 1_000_000_000_000L),
      Gen.chooseNum(100_000_000L, 500_000_000_000L),
      Gen.chooseNum(0L, 1_000_000L),
      Gen.chooseNum(0L, 1_000_000L),
      Gen.chooseNum(0L, 1_000_000L),
    ) { (valueUnits, mortgageUnits, principalUnits, defaultAmtUnits, interestUnits) =>
      val value    = pln(valueUnits)
      val mortgage = pln(mortgageUnits)
      whenever(mortgage < value && mortgage > pln(principalUnits) + pln(defaultAmtUnits)) {
        val state  = makeRegionalState(value, mortgage)
        val result = HousingMarket.applyFlows(state, mkFlows(pln(interestUnits), pln(principalUnits), pln(defaultAmtUnits)))
        shouldBeClosePln(result.regions.get.map(_.mortgageStock).sum, result.mortgageStock, hundredPln)
      }
    }

  it should "have regional repayments summing to aggregate" in
    forAll(
      Gen.chooseNum(1_000_000_000L, 1_000_000_000_000L),
      Gen.chooseNum(100_000_000L, 500_000_000_000L),
      Gen.chooseNum(0L, 1_000_000L),
      Gen.chooseNum(0L, 1_000_000L),
    ) { (valueUnits, mortgageUnits, principalUnits, defaultAmtUnits) =>
      val value      = pln(valueUnits)
      val mortgage   = pln(mortgageUnits)
      val principal  = pln(principalUnits)
      val defaultAmt = pln(defaultAmtUnits)
      whenever(mortgage < value && mortgage > PLN.Zero) {
        val state  = makeRegionalState(value, mortgage)
        val result = HousingMarket.applyFlows(state, mkFlows(principal = principal, defaultAmount = defaultAmt))
        result.regions.get.map(_.lastRepayment).sum shouldBe principal
        result.regions.get.map(_.lastDefault).sum shouldBe defaultAmt
      }
    }

  it should "preserve None regions when state has no regions" in
    forAll(genHousingState, Gen.chooseNum(0L, 1_000_000L), Gen.chooseNum(0L, 1_000_000L), Gen.chooseNum(0L, 1_000_000L)) {
      (state, principalUnits, defaultAmtUnits, interestUnits) =>
        val result = HousingMarket.applyFlows(state, mkFlows(pln(interestUnits), pln(principalUnits), pln(defaultAmtUnits)))
        result.regions shouldBe None
    }

  "Mortgage stock identity with regions" should "hold at aggregate level" in
    forAll(
      Gen.chooseNum(1_000_000_000L, 1_000_000_000_000L),
      Gen.chooseNum(100_000_000L, 500_000_000_000L),
      Gen.chooseNum(0L, 1_000_000L),
      Gen.chooseNum(0L, 100_000L),
      Gen.chooseNum(0L, 100_000L),
    ) { (valueUnits, mortgageUnits, originationUnits, principalUnits, defaultAmtUnits) =>
      val value       = pln(valueUnits)
      val mortgage    = pln(mortgageUnits)
      val origination = pln(originationUnits)
      val principal   = pln(principalUnits)
      val defaultAmt  = pln(defaultAmtUnits)
      whenever(mortgage < value && mortgage > principal + defaultAmt) {
        val state          = makeRegionalState(value, mortgage)
        val updatedRegs    = state.regions.get.zipWithIndex.map { (r, i) =>
          val rOrig = valueShares(i) * origination
          r.copy(mortgageStock = r.mortgageStock + rOrig, lastOrigination = rOrig)
        }
        val stateAfterOrig = state.copy(
          mortgageStock = mortgage + origination,
          lastOrigination = origination,
          regions = Some(updatedRegs),
        )
        val result         = HousingMarket.applyFlows(stateAfterOrig, mkFlows(principal = principal, defaultAmount = defaultAmt))
        result.mortgageStock shouldBe mortgage + origination - principal - defaultAmt
        shouldBeClosePln(result.regions.get.map(_.mortgageStock).sum, result.mortgageStock, hundredPln)
      }
    }
