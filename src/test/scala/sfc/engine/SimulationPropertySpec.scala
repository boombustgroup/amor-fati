package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen
import sfc.testutil.Generators.*
import sfc.config.{Config, MonetaryRegime, RunConfig}
import sfc.sfc.GovState

class SimulationPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  private val plnConfig = RunConfig(2000.0, 1, "test", MonetaryRegime.Pln)
  private val eurConfig = RunConfig(2000.0, 1, "test", MonetaryRegime.Eur)

  // Combined generator for gov inputs (avoids >6 forAll limit)
  private val genGovInputs: Gen[(GovState, Double, Double, Boolean, Double, Double, Double)] =
    for
      prev     <- genGovState
      cit      <- Gen.choose(0.0, 1e8)
      vat      <- Gen.choose(0.0, 1e8)
      active   <- Gen.oneOf(true, false)
      bdp      <- Gen.choose(0.0, 5000.0)
      price    <- genPrice
      unempBen <- Gen.choose(0.0, 1e7)
    yield (prev, cit, vat, active, bdp, price, unempBen)

  // Combined generator for inflation inputs (avoids >6 forAll limit)
  private val genInflInputs: Gen[(Double, Double, Double, Double, Double, Double, Double)] =
    for
      prevInfl   <- genInflation
      prevPrice  <- genPrice
      demandMult <- Gen.choose(0.5, 2.0)
      wageGrowth <- Gen.choose(-0.10, 0.10)
      exRateDev  <- Gen.choose(-0.20, 0.20)
      autoR      <- genFraction
      hybR       <- genFraction
    yield (prevInfl, prevPrice, demandMult, wageGrowth, exRateDev, autoR, hybR)

  // --- updateCbRate properties ---

  "updateCbRate" should "be in [RateFloor, RateCeiling] for PLN" in {
    forAll(genRate, genInflation, Gen.choose(-0.10, 0.10), Gen.choose(0, Config.TotalPopulation)) {
      (prevRate: Double, inflation: Double, exRateChg: Double, employed: Int) =>
        val r = Sectors.updateCbRate(prevRate, inflation, exRateChg, employed, plnConfig)
        r should be >= Config.RateFloor
        r should be <= Config.RateCeiling
    }
  }

  it should "be in [RateFloor, RateCeiling] for EUR" in {
    forAll(genRate, genInflation, Gen.choose(-0.10, 0.10), Gen.choose(0, Config.TotalPopulation)) {
      (prevRate: Double, inflation: Double, exRateChg: Double, employed: Int) =>
        val r = Sectors.updateCbRate(prevRate, inflation, exRateChg, employed, eurConfig)
        r should be >= Config.RateFloor
        r should be <= Config.RateCeiling
    }
  }

  it should "be monotonic in inflation for PLN (higher inflation -> higher rate)" in {
    forAll(genRate, Gen.choose(-0.10, 0.30), Gen.choose(0, Config.TotalPopulation)) {
      (prevRate: Double, baseInflation: Double, employed: Int) =>
        val lowInfl = baseInflation
        val highInfl = baseInflation + 0.10
        val rLow = Sectors.updateCbRate(prevRate, lowInfl, 0.0, employed, plnConfig)
        val rHigh = Sectors.updateCbRate(prevRate, highInfl, 0.0, employed, plnConfig)
        rHigh should be >= (rLow - 1e-10)
    }
  }

  it should "produce identical rates for EUR regardless of domestic inflation" in {
    forAll(genRate, genInflation, genInflation, Gen.choose(0, Config.TotalPopulation)) {
      (prevRate: Double, infl1: Double, infl2: Double, employed: Int) =>
        val r1 = Sectors.updateCbRate(prevRate, infl1, 0.0, employed, eurConfig)
        val r2 = Sectors.updateCbRate(prevRate, infl2, 0.0, employed, eurConfig)
        r1 shouldBe (r2 +- 1e-10)
    }
  }

  // --- updateInflation properties ---

  "updateInflation" should "keep price >= 0.30 floor" in {
    forAll(genInflInputs) { (inputs: (Double, Double, Double, Double, Double, Double, Double)) =>
      val (prevInfl, prevPrice, demandMult, wageGrowth, exRateDev, autoR, hybR) = inputs
      val (_, newPrice) = Sectors.updateInflation(prevInfl, prevPrice, demandMult,
        wageGrowth, exRateDev, autoR, hybR, plnConfig)
      newPrice should be >= 0.30
    }
  }

  it should "apply soft deflation floor (price >= 0.30)" in {
    val (_, price) = Sectors.updateInflation(
      -0.30, 1.0, 0.5, -0.10, 0.0, 0.80, 0.15, plnConfig)
    price should be >= 0.30
  }

  it should "produce lower inflation with more automation" in {
    forAll(genInflation, genPrice, Gen.choose(0.8, 1.2), Gen.choose(-0.02, 0.02)) {
      (prevInfl: Double, prevPrice: Double, demandMult: Double, wageGrowth: Double) =>
        val (infl1, _) = Sectors.updateInflation(prevInfl, prevPrice, demandMult,
          wageGrowth, 0.0, 0.05, 0.0, plnConfig)
        val (infl2, _) = Sectors.updateInflation(prevInfl, prevPrice, demandMult,
          wageGrowth, 0.0, 0.50, 0.0, plnConfig)
        infl2 should be <= (infl1 + 1e-10)
    }
  }

  // --- updateLaborMarket properties ---

  "updateLaborMarket" should "keep wage >= reservationWage" in {
    forAll(genWage, Gen.choose(4666.0, 10000.0), Gen.choose(0, Config.TotalPopulation)) {
      (prevWage: Double, resWage: Double, laborDemand: Int) =>
        val (newWage, _) = Sectors.updateLaborMarket(prevWage, resWage, laborDemand)
        newWage should be >= resWage
    }
  }

  it should "keep employed <= min(laborDemand, TotalPopulation)" in {
    forAll(genWage, Gen.choose(4666.0, 10000.0), Gen.choose(0, Config.TotalPopulation * 2)) {
      (prevWage: Double, resWage: Double, laborDemand: Int) =>
        val (_, employed) = Sectors.updateLaborMarket(prevWage, resWage, laborDemand)
        employed should be <= Math.min(laborDemand, Config.TotalPopulation)
    }
  }

  // --- updateGov properties ---

  "updateGov" should "have deficit = spending - revenue" in {
    forAll(genGovInputs) { (inputs: (GovState, Double, Double, Boolean, Double, Double, Double)) =>
      val (prev, cit, vat, active, bdp, price, unempBen) = inputs
      val gov = Sectors.updateGov(prev, cit, vat, active, bdp, price, unempBen)
      val totalRev = cit + vat
      val bdpSpend = if active then Config.TotalPopulation.toDouble * bdp else 0.0
      val totalSpend = bdpSpend + unempBen + Config.GovBaseSpending * price
      gov.deficit shouldBe (totalSpend - totalRev +- 1.0)
    }
  }

  it should "accumulate debt (newDebt = prev + deficit)" in {
    forAll(genGovInputs) { (inputs: (GovState, Double, Double, Boolean, Double, Double, Double)) =>
      val (prev, cit, vat, active, bdp, price, unempBen) = inputs
      val gov = Sectors.updateGov(prev, cit, vat, active, bdp, price, unempBen)
      gov.cumulativeDebt shouldBe (prev.cumulativeDebt + gov.deficit +- 1.0)
    }
  }

  it should "have zero BDP spending when not active" in {
    forAll(genGovState, Gen.choose(0.0, 1e8), Gen.choose(0.0, 1e8),
           Gen.choose(0.0, 5000.0), genPrice) {
      (prev: GovState, cit: Double, vat: Double, bdp: Double, price: Double) =>
        val gov = Sectors.updateGov(prev, cit, vat, false, bdp, price, 0.0)
        gov.bdpSpending shouldBe 0.0
    }
  }

  // --- updateForeign properties ---

  "updateForeign" should "keep fixed exchange rate for EUR" in {
    forAll(genForexState, Gen.choose(0.0, 1e8), Gen.choose(0.0, 1e7),
           genFraction, genRate, Gen.choose(1e6, 1e10)) {
      (prev: sfc.sfc.ForexState, importCons: Double, techImp: Double,
       autoR: Double, rate: Double, gdp: Double) =>
        val fx = Sectors.updateForeign(prev, importCons, techImp, autoR, rate, gdp, eurConfig)
        fx.exchangeRate shouldBe Config.BaseExRate
    }
  }

  it should "keep PLN exchange rate in [3.0, 8.0]" in {
    forAll(genForexState, Gen.choose(0.0, 1e8), Gen.choose(0.0, 1e7),
           genFraction, genRate, Gen.choose(1e6, 1e10)) {
      (prev: sfc.sfc.ForexState, importCons: Double, techImp: Double,
       autoR: Double, rate: Double, gdp: Double) =>
        val fx = Sectors.updateForeign(prev, importCons, techImp, autoR, rate, gdp, plnConfig)
        fx.exchangeRate should be >= 3.0
        fx.exchangeRate should be <= 8.0
    }
  }
