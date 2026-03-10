package sfc.engine

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sfc.Generators.*
import sfc.accounting.{ForexState, GovState}
import sfc.agents.Nbp
import sfc.McRunConfig
import sfc.config.SimParams
import sfc.engine.markets.{FiscalBudget, LaborMarket, OpenEconomy, PriceLevel}
import sfc.types.*

class SimulationPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  private val rc       = McRunConfig(1, "test")
  private val totalPop = p.pop.firmsCount * p.pop.workersPerFirm

  // Combined generator for gov inputs (avoids >6 forAll limit)
  private val genGovInputs: Gen[(GovState, Double, Double, Double, Double)] =
    for
      prev     <- genGovState
      cit      <- Gen.choose(0.0, 1e8)
      vat      <- Gen.choose(0.0, 1e8)
      price    <- genPrice
      unempBen <- Gen.choose(0.0, 1e7)
    yield (prev, cit, vat, price, unempBen)

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

  "updateCbRate" should "be in [RateFloor, RateCeiling] for PLN" in
    forAll(genRate, genInflation, Gen.choose(-0.10, 0.10), Gen.choose(0, totalPop)) { (prevRate: Double, inflation: Double, exRateChg: Double, employed: Int) =>
      val r = Nbp.updateRate(Rate(prevRate), Rate(inflation), exRateChg, employed, totalPop, rc)
      r.toDouble should be >= p.monetary.rateFloor.toDouble
      r.toDouble should be <= p.monetary.rateCeiling.toDouble
    }

  it should "be monotonic in inflation (higher inflation -> higher rate)" in
    forAll(genRate, Gen.choose(-0.10, 0.30), Gen.choose(0, totalPop)) { (prevRate: Double, baseInflation: Double, employed: Int) =>
      val lowInfl  = baseInflation
      val highInfl = baseInflation + 0.10
      val rLow     = Nbp.updateRate(Rate(prevRate), Rate(lowInfl), 0.0, employed, totalPop, rc)
      val rHigh    = Nbp.updateRate(Rate(prevRate), Rate(highInfl), 0.0, employed, totalPop, rc)
      rHigh.toDouble should be >= (rLow.toDouble - 1e-10)
    }

  // --- updateInflation properties ---

  "updateInflation" should "keep price >= 0.30 floor" in
    forAll(genInflInputs) { (inputs: (Double, Double, Double, Double, Double, Double, Double)) =>
      val (prevInfl, prevPrice, demandMult, wageGrowth, exRateDev, autoR, hybR) = inputs
      val (_, newPrice)                                                         =
        PriceLevel.update(prevInfl, prevPrice, demandMult, wageGrowth, exRateDev, autoR, hybR, rc)
      newPrice should be >= 0.30
    }

  it should "apply soft deflation floor (price >= 0.30)" in {
    val (_, price) = PriceLevel.update(-0.30, 1.0, 0.5, -0.10, 0.0, 0.80, 0.15, rc)
    price should be >= 0.30
  }

  it should "produce lower inflation with more automation" in
    forAll(genInflation, genPrice, Gen.choose(0.8, 1.2), Gen.choose(-0.02, 0.02)) {
      (prevInfl: Double, prevPrice: Double, demandMult: Double, wageGrowth: Double) =>
        val (infl1, _) = PriceLevel.update(prevInfl, prevPrice, demandMult, wageGrowth, 0.0, 0.05, 0.0, rc)
        val (infl2, _) = PriceLevel.update(prevInfl, prevPrice, demandMult, wageGrowth, 0.0, 0.50, 0.0, rc)
        infl2 should be <= (infl1 + 1e-10)
    }

  // --- updateLaborMarket properties ---

  "updateLaborMarket" should "keep wage >= reservationWage" in
    forAll(genWage, Gen.choose(4666.0, 10000.0), Gen.choose(0, totalPop)) { (prevWage: Double, resWage: Double, laborDemand: Int) =>
      val (newWage, _) = LaborMarket.updateLaborMarket(prevWage, resWage, laborDemand, totalPop)
      newWage should be >= resWage
    }

  it should "keep employed <= min(laborDemand, TotalPopulation)" in
    forAll(genWage, Gen.choose(4666.0, 10000.0), Gen.choose(0, totalPop * 2)) { (prevWage: Double, resWage: Double, laborDemand: Int) =>
      val (_, employed) = LaborMarket.updateLaborMarket(prevWage, resWage, laborDemand, totalPop)
      employed should be <= Math.min(laborDemand, totalPop)
    }

  // --- updateGov properties ---

  "updateGov" should "have deficit = spending - revenue" in
    forAll(genGovInputs) { (inputs: (GovState, Double, Double, Double, Double)) =>
      val (prev, cit, vat, price, unempBen) = inputs
      val gov                               = FiscalBudget.update(prev, cit, vat, price, unempBen)
      val totalRev                          = cit + vat
      val totalSpend                        = unempBen + p.fiscal.govBaseSpending.toDouble * price
      gov.deficit.toDouble shouldBe (totalSpend - totalRev +- 1.0)
    }

  it should "accumulate debt (newDebt = prev + deficit)" in
    forAll(genGovInputs) { (inputs: (GovState, Double, Double, Double, Double)) =>
      val (prev, cit, vat, price, unempBen) = inputs
      val gov                               = FiscalBudget.update(prev, cit, vat, price, unempBen)
      gov.cumulativeDebt.toDouble shouldBe (prev.cumulativeDebt.toDouble + gov.deficit.toDouble +- 1.0)
    }

  it should "include debtService in deficit calculation" in
    forAll(genGovInputs, Gen.choose(0.0, 1e7)) { (inputs: (GovState, Double, Double, Double, Double), debtSvc: Double) =>
      val (prev, cit, vat, price, unempBen) = inputs
      val gov                               = FiscalBudget.update(prev, cit, vat, price, unempBen, debtSvc)
      val totalRev                          = cit + vat
      val totalSpend                        = unempBen + p.fiscal.govBaseSpending.toDouble * price + debtSvc
      gov.deficit.toDouble shouldBe (totalSpend - totalRev +- 1.0)
    }

  it should "include nbpRemittance in revenue" in
    forAll(genGovInputs, Gen.choose(0.0, 1e7)) { (inputs: (GovState, Double, Double, Double, Double), nbpRemit: Double) =>
      val (prev, cit, vat, price, unempBen) = inputs
      val govNoRemit                        = FiscalBudget.update(prev, cit, vat, price, unempBen)
      val govWithRemit                      = FiscalBudget.update(prev, cit, vat, price, unempBen, 0.0, nbpRemit)
      // nbpRemittance reduces deficit
      govWithRemit.deficit.toDouble shouldBe (govNoRemit.deficit.toDouble - nbpRemit +- 1.0)
    }

  // --- updateForeign properties ---

  "updateForeign" should "keep exchange rate in [3.0, 8.0]" in
    forAll(genForexState, Gen.choose(0.0, 1e8), Gen.choose(0.0, 1e7), genFraction, genRate, Gen.choose(1e6, 1e10)) {
      (prev: ForexState, importCons: Double, techImp: Double, autoR: Double, rate: Double, gdp: Double) =>
        val fx = OpenEconomy.updateForeign(prev, importCons, techImp, autoR, rate, gdp, rc)
        fx.exchangeRate should be >= 3.0
        fx.exchangeRate should be <= 8.0
    }
