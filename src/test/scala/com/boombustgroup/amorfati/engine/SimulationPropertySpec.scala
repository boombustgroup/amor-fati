package com.boombustgroup.amorfati.engine

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.boombustgroup.amorfati.Generators.*
import com.boombustgroup.amorfati.agents.Nbp
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.markets.{FiscalBudget, LaborMarket, OpenEconomy, PriceLevel}
import com.boombustgroup.amorfati.types.*

class SimulationPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  private val totalPop = p.pop.firmsCount * p.pop.workersPerFirm

  // Combined generator for gov inputs (avoids >6 forAll limit)
  private val genGovInputs: Gen[(FiscalBudget.GovState, Double, Double, Double, Double)] =
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
      val r = Nbp.updateRate(Rate(prevRate), Rate(inflation), exRateChg, employed, totalPop)
      r.toDouble should be >= p.monetary.rateFloor.toDouble
      r.toDouble should be <= p.monetary.rateCeiling.toDouble
    }

  it should "be monotonic in inflation (higher inflation -> higher rate)" in
    forAll(genRate, Gen.choose(-0.10, 0.30), Gen.choose(0, totalPop)) { (prevRate: Double, baseInflation: Double, employed: Int) =>
      val lowInfl  = baseInflation
      val highInfl = baseInflation + 0.10
      val rLow     = Nbp.updateRate(Rate(prevRate), Rate(lowInfl), 0.0, employed, totalPop)
      val rHigh    = Nbp.updateRate(Rate(prevRate), Rate(highInfl), 0.0, employed, totalPop)
      rHigh.toDouble should be >= (rLow.toDouble - 1e-10)
    }

  // --- updateInflation properties ---

  "updateInflation" should "keep price >= 0.30 floor" in
    forAll(genInflInputs) { (inputs: (Double, Double, Double, Double, Double, Double, Double)) =>
      val (prevInfl, prevPrice, demandMult, wageGrowth, exRateDev, autoR, hybR) = inputs
      val r                                                                     = PriceLevel.update(Rate(prevInfl), prevPrice, demandMult, wageGrowth, exRateDev, autoR, hybR)
      r.priceLevel should be >= 0.30
    }

  it should "apply soft deflation floor (price >= 0.30)" in {
    val r = PriceLevel.update(Rate(-0.30), 1.0, 0.5, -0.10, 0.0, 0.80, 0.15)
    r.priceLevel should be >= 0.30
  }

  it should "produce lower inflation with more automation" in
    forAll(genInflation, genPrice, Gen.choose(0.8, 1.2), Gen.choose(-0.02, 0.02)) {
      (prevInfl: Double, prevPrice: Double, demandMult: Double, wageGrowth: Double) =>
        val r1 = PriceLevel.update(Rate(prevInfl), prevPrice, demandMult, wageGrowth, 0.0, 0.05, 0.0)
        val r2 = PriceLevel.update(Rate(prevInfl), prevPrice, demandMult, wageGrowth, 0.0, 0.50, 0.0)
        r2.inflation.toDouble should be <= (r1.inflation.toDouble + 1e-10)
    }

  // --- updateLaborMarket properties ---

  "updateLaborMarket" should "keep wage >= reservationWage" in
    forAll(genWage, Gen.choose(4666.0, 10000.0), Gen.choose(0, totalPop)) { (prevWage: Double, resWage: Double, laborDemand: Int) =>
      val r = LaborMarket.updateLaborMarket(PLN(prevWage), PLN(resWage), laborDemand, totalPop)
      r.wage.toDouble should be >= resWage
    }

  it should "keep employed <= min(laborDemand, TotalPopulation)" in
    forAll(genWage, Gen.choose(4666.0, 10000.0), Gen.choose(0, totalPop * 2)) { (prevWage: Double, resWage: Double, laborDemand: Int) =>
      val r = LaborMarket.updateLaborMarket(PLN(prevWage), PLN(resWage), laborDemand, totalPop)
      r.employed should be <= Math.min(laborDemand, totalPop)
    }

  // --- updateGov properties ---

  "updateGov" should "have deficit = spending - revenue" in
    forAll(genGovInputs) { (inputs: (FiscalBudget.GovState, Double, Double, Double, Double)) =>
      val (prev, cit, vat, price, unempBen) = inputs
      val gov                               = FiscalBudget.update(FiscalBudget.Input(prev, price, citPaid = PLN(cit), vat = PLN(vat), unempBenefitSpend = PLN(unempBen)))
      val totalRev                          = cit + vat
      val totalSpend                        = unempBen + p.fiscal.govBaseSpending.toDouble * price
      gov.deficit.toDouble shouldBe (totalSpend - totalRev +- 1.0)
    }

  it should "accumulate debt (newDebt = prev + deficit)" in
    forAll(genGovInputs) { (inputs: (FiscalBudget.GovState, Double, Double, Double, Double)) =>
      val (prev, cit, vat, price, unempBen) = inputs
      val gov                               = FiscalBudget.update(FiscalBudget.Input(prev, price, citPaid = PLN(cit), vat = PLN(vat), unempBenefitSpend = PLN(unempBen)))
      gov.cumulativeDebt.toDouble shouldBe (prev.cumulativeDebt.toDouble + gov.deficit.toDouble +- 1.0)
    }

  it should "include debtService in deficit calculation" in
    forAll(genGovInputs, Gen.choose(0.0, 1e7)) { (inputs: (FiscalBudget.GovState, Double, Double, Double, Double), debtSvc: Double) =>
      val (prev, cit, vat, price, unempBen) = inputs
      val gov                               =
        FiscalBudget.update(FiscalBudget.Input(prev, price, citPaid = PLN(cit), vat = PLN(vat), unempBenefitSpend = PLN(unempBen), debtService = PLN(debtSvc)))
      val totalRev                          = cit + vat
      val totalSpend                        = unempBen + p.fiscal.govBaseSpending.toDouble * price + debtSvc
      gov.deficit.toDouble shouldBe (totalSpend - totalRev +- 1.0)
    }

  it should "include nbpRemittance in revenue" in
    forAll(genGovInputs, Gen.choose(0.0, 1e7)) { (inputs: (FiscalBudget.GovState, Double, Double, Double, Double), nbpRemit: Double) =>
      val (prev, cit, vat, price, unempBen) = inputs
      val base                              = FiscalBudget.Input(prev, price, citPaid = PLN(cit), vat = PLN(vat), unempBenefitSpend = PLN(unempBen))
      val govNoRemit                        = FiscalBudget.update(base)
      val govWithRemit                      = FiscalBudget.update(base.copy(nbpRemittance = PLN(nbpRemit)))
      // nbpRemittance reduces deficit
      govWithRemit.deficit.toDouble shouldBe (govNoRemit.deficit.toDouble - nbpRemit +- 1.0)
    }

  // --- updateForeign properties ---

  "updateForeign" should "keep exchange rate in [3.0, 8.0]" in
    forAll(genForexState, Gen.choose(0.0, 1e8), Gen.choose(0.0, 1e7), genFraction, genRate, Gen.choose(1e6, 1e10)) {
      (prev: OpenEconomy.ForexState, importCons: Double, techImp: Double, autoR: Double, rate: Double, gdp: Double) =>
        val fx = OpenEconomy.updateForeign(prev, PLN(importCons), PLN(techImp), Ratio(autoR), Rate(rate), PLN(gdp))
        fx.exchangeRate should be >= 3.0
        fx.exchangeRate should be <= 8.0
    }
