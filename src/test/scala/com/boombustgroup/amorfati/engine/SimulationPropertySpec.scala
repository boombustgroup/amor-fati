package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.boombustgroup.amorfati.Generators.*
import com.boombustgroup.amorfati.agents.Nbp
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.markets.{FiscalBudget, LaborMarket, PriceLevel}
import com.boombustgroup.amorfati.types.*

class SimulationPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  private val totalPop = p.pop.firmsCount * p.pop.workersPerFirm

  // Combined generator for gov inputs (avoids >6 forAll limit)
  private val genGovInputs: Gen[(FiscalBudget.GovState, BigDecimal, BigDecimal, BigDecimal, BigDecimal)] =
    for
      prev     <- genGovState
      cit      <- genDecimal("0.0", "1e8")
      vat      <- genDecimal("0.0", "1e8")
      price    <- genPrice
      unempBen <- genDecimal("0.0", "1e7")
    yield (prev, cit, vat, price, unempBen)

  // Combined generator for inflation inputs (avoids >6 forAll limit)
  private val genInflInputs: Gen[(BigDecimal, BigDecimal, BigDecimal, BigDecimal, BigDecimal, BigDecimal, BigDecimal)] =
    for
      prevInfl   <- genInflation
      prevPrice  <- genPrice
      demandMult <- genDecimal("0.5", "2.0")
      wageGrowth <- genDecimal("-0.10", "0.10")
      exRateDev  <- genDecimal("-0.20", "0.20")
      autoR      <- genFraction
      hybR       <- genFraction
    yield (prevInfl, prevPrice, demandMult, wageGrowth, exRateDev, autoR, hybR)

  // --- updateCbRate properties ---

  "updateCbRate" should "be in [RateFloor, RateCeiling] for PLN" in
    forAll(genRate, genInflation, genDecimal("-0.10", "0.10"), Gen.choose(0, totalPop)) {
      (prevRate: BigDecimal, inflation: BigDecimal, exRateChg: BigDecimal, employed: Int) =>
        val r = Nbp.updateRate(Rate(prevRate), Rate(inflation), Coefficient(exRateChg), employed, totalPop)
        decimal(r) should be >= decimal(p.monetary.rateFloor)
        decimal(r) should be <= decimal(p.monetary.rateCeiling)
    }

  it should "be monotonic in inflation (higher inflation -> higher rate)" in
    forAll(genRate, genDecimal("-0.10", "0.30"), Gen.choose(0, totalPop)) { (prevRate: BigDecimal, baseInflation: BigDecimal, employed: Int) =>
      val lowInfl  = baseInflation
      val highInfl = baseInflation + BigDecimal("0.10")
      val rLow     = Nbp.updateRate(Rate(prevRate), Rate(lowInfl), Coefficient.Zero, employed, totalPop)
      val rHigh    = Nbp.updateRate(Rate(prevRate), Rate(highInfl), Coefficient.Zero, employed, totalPop)
      decimal(rHigh) should be >= (decimal(rLow) - BigDecimal("1e-10"))
    }

  // --- updateInflation properties ---

  "updateInflation" should "keep price >= 0.30 floor" in
    forAll(genInflInputs) { (inputs: (BigDecimal, BigDecimal, BigDecimal, BigDecimal, BigDecimal, BigDecimal, BigDecimal)) =>
      val (prevInfl, prevPrice, demandMult, wageGrowth, exRateDev, _, _) = inputs
      val r                                                              =
        PriceLevel.update(Rate(prevInfl), PriceIndex(prevPrice), Multiplier(demandMult), Coefficient(wageGrowth), ExchangeRateShock(exRateDev))
      decimal(r.priceLevel).should(be >= BigDecimal("0.30"))
    }

  it should "apply soft deflation floor (price >= 0.30)" in {
    val r = PriceLevel.update(Rate("-0.30"), PriceIndex.Base, Multiplier("0.5"), Coefficient("-0.10"), ExchangeRateShock.Zero)
    decimal(r.priceLevel).should(be >= BigDecimal("0.30"))
  }

  it should "produce higher inflation with more import pressure" in
    forAll(genInflation, genPrice, genDecimal("0.8", "1.2"), genDecimal("-0.02", "0.02"), genDecimal("0.0", "0.15"), genDecimal("0.16", "0.40")) {
      (prevInfl: BigDecimal, prevPrice: BigDecimal, demandMult: BigDecimal, wageGrowth: BigDecimal, exLow: BigDecimal, exHigh: BigDecimal) =>
        val r1 =
          PriceLevel.update(
            Rate(prevInfl),
            PriceIndex(prevPrice),
            Multiplier(demandMult),
            Coefficient(wageGrowth),
            ExchangeRateShock(exLow),
          )
        val r2 =
          PriceLevel.update(
            Rate(prevInfl),
            PriceIndex(prevPrice),
            Multiplier(demandMult),
            Coefficient(wageGrowth),
            ExchangeRateShock(exHigh),
          )
        decimal(r2.inflation).should(be >= (decimal(r1.inflation) - BigDecimal("1e-10")))
    }

  // --- updateLaborMarket properties ---

  "updateLaborMarket" should "keep wage >= reservationWage" in
    forAll(genWage, genDecimal("4666.0", "10000.0"), Gen.choose(0, totalPop)) { (prevWage: BigDecimal, resWage: BigDecimal, laborDemand: Int) =>
      val r = LaborMarket.updateLaborMarket(PLN(prevWage), PLN(resWage), laborDemand, totalPop)
      decimal(r.wage) should be >= (resWage - BigDecimal("0.0001"))
    }

  it should "keep employed <= min(laborDemand, TotalPopulation)" in
    forAll(genWage, genDecimal("4666.0", "10000.0"), Gen.choose(0, totalPop * 2)) { (prevWage: BigDecimal, resWage: BigDecimal, laborDemand: Int) =>
      val r = LaborMarket.updateLaborMarket(PLN(prevWage), PLN(resWage), laborDemand, totalPop)
      r.employed should be <= DecimalMath.min(laborDemand, totalPop)
    }

  // --- updateGov properties ---

  "updateGov" should "have deficit = spending - revenue" in
    forAll(genGovInputs) { (inputs: (FiscalBudget.GovState, BigDecimal, BigDecimal, BigDecimal, BigDecimal)) =>
      val (prev, cit, vat, price, unempBen) = inputs
      whenever(price >= BigDecimal("0.01")) {
        val gov        =
          FiscalBudget.update(
            FiscalBudget.Input(prev, PriceIndex(price), citPaid = PLN(cit), govDividendRevenue = PLN.Zero, vat = PLN(vat), unempBenefitSpend = PLN(unempBen)),
          )
        val totalRev   = cit + vat
        val totalSpend = unempBen + decimal(p.fiscal.govBaseSpending) * price
        val tol        = decimal(p.fiscal.govBaseSpending) * BigDecimal("0.0001") + BigDecimal("1.0")
        decimal(gov.deficit) shouldBe (totalSpend - totalRev +- tol)
      }
    }

  it should "accumulate debt (newDebt = prev + deficit)" in
    forAll(genGovInputs) { (inputs: (FiscalBudget.GovState, BigDecimal, BigDecimal, BigDecimal, BigDecimal)) =>
      val (prev, cit, vat, price, unempBen) = inputs
      val gov                               =
        FiscalBudget.update(
          FiscalBudget.Input(prev, PriceIndex(price), citPaid = PLN(cit), govDividendRevenue = PLN.Zero, vat = PLN(vat), unempBenefitSpend = PLN(unempBen)),
        )
      decimal(gov.cumulativeDebt) shouldBe (decimal(prev.cumulativeDebt) + decimal(gov.deficit) +- BigDecimal("1.0"))
    }

  it should "include debtService in deficit calculation" in
    forAll(genGovInputs, genDecimal("0.0", "1e7")) { (inputs: (FiscalBudget.GovState, BigDecimal, BigDecimal, BigDecimal, BigDecimal), debtSvc: BigDecimal) =>
      val (prev, cit, vat, price, unempBen) = inputs
      whenever(price >= BigDecimal("0.01")) {
        val gov        =
          FiscalBudget.update(
            FiscalBudget.Input(
              prev,
              PriceIndex(price),
              citPaid = PLN(cit),
              govDividendRevenue = PLN.Zero,
              vat = PLN(vat),
              unempBenefitSpend = PLN(unempBen),
              debtService = PLN(debtSvc),
            ),
          )
        val totalRev   = cit + vat
        val totalSpend = unempBen + decimal(p.fiscal.govBaseSpending) * price + debtSvc
        val tol        = decimal(p.fiscal.govBaseSpending) * BigDecimal("0.0001") + BigDecimal("1.0")
        decimal(gov.deficit) shouldBe (totalSpend - totalRev +- tol)
      }
    }

  it should "include nbpRemittance in revenue" in
    forAll(genGovInputs, genDecimal("0.0", "1e7")) { (inputs: (FiscalBudget.GovState, BigDecimal, BigDecimal, BigDecimal, BigDecimal), nbpRemit: BigDecimal) =>
      val (prev, cit, vat, price, unempBen) = inputs
      val base                              =
        FiscalBudget.Input(prev, PriceIndex(price), citPaid = PLN(cit), govDividendRevenue = PLN.Zero, vat = PLN(vat), unempBenefitSpend = PLN(unempBen))
      val govNoRemit                        = FiscalBudget.update(base)
      val govWithRemit                      = FiscalBudget.update(base.copy(nbpRemittance = PLN(nbpRemit)))
      // nbpRemittance reduces deficit
      decimal(govWithRemit.deficit) shouldBe (decimal(govNoRemit.deficit) - nbpRemit +- BigDecimal("1.0"))
    }
