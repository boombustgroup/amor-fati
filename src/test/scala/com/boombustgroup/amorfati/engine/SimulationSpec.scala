package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.agents.Nbp
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.markets.{FiscalBudget, LaborMarket, PriceLevel}
import com.boombustgroup.amorfati.types.*

class SimulationSpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]
  private val totalPop     = p.pop.firmsCount * p.pop.workersPerFirm

  // --- updateLaborMarket ---

  "LaborMarket.updateLaborMarket" should "increase wage when demand exceeds supply" in {
    val r1 = LaborMarket.updateLaborMarket(p.household.baseWage, p.household.baseReservationWage, totalPop, totalPop)
    val r2 = LaborMarket.updateLaborMarket(p.household.baseWage, p.household.baseReservationWage, totalPop * 2, totalPop)
    r2.wage.should(be > r1.wage)
  }

  it should "keep wage at or above reservation wage" in {
    // Very low demand → wage should still be >= reservation
    val r = LaborMarket.updateLaborMarket(p.household.baseWage, p.household.baseReservationWage, 0, totalPop)
    decimal(r.wage).should(be >= decimal(p.household.baseReservationWage))
  }

  // --- updateInflation ---

  "PriceLevel.update" should "produce higher inflation with higher demand" in {
    val r1 = PriceLevel.update(Rate.decimal(25, 3), PriceIndex.Base, Multiplier.One, Coefficient.Zero, ExchangeRateShock.Zero)
    val r2 = PriceLevel.update(Rate.decimal(25, 3), PriceIndex.Base, Multiplier.decimal(15, 1), Coefficient.Zero, ExchangeRateShock.Zero)
    r2.inflation.should(be > r1.inflation)
  }

  it should "produce higher inflation with FX import pressure" in {
    val r1 = PriceLevel.update(Rate.decimal(25, 3), PriceIndex.Base, Multiplier.One, Coefficient.Zero, ExchangeRateShock.Zero)
    val r2 = PriceLevel.update(Rate.decimal(25, 3), PriceIndex.Base, Multiplier.One, Coefficient.Zero, ExchangeRateShock.decimal(2, 1))
    r2.inflation.should(be > r1.inflation)
  }

  it should "ignore negative wage growth in aggregate cost-push" in {
    val flat = PriceLevel.update(Rate.decimal(25, 3), PriceIndex.Base, Multiplier.One, Coefficient.Zero, ExchangeRateShock.Zero)
    val down = PriceLevel.update(Rate.decimal(25, 3), PriceIndex.Base, Multiplier.One, Coefficient.decimal(-5, 2), ExchangeRateShock.Zero)

    down.inflation shouldBe flat.inflation
    down.priceLevel shouldBe flat.priceLevel
    down.costPush shouldBe Coefficient.Zero
  }

  it should "apply only partial downside pass-through under slack demand" in {
    val flat  = PriceLevel.update(Rate.decimal(25, 3), PriceIndex.Base, Multiplier.One, Coefficient.Zero, ExchangeRateShock.Zero)
    val slack = PriceLevel.update(Rate.decimal(25, 3), PriceIndex.Base, Multiplier.decimal(8, 1), Coefficient.Zero, ExchangeRateShock.Zero)

    slack.inflation should be < flat.inflation
    slack.priceLevel should be < flat.priceLevel
    slack.demandPull should be < Coefficient.Zero
    slack.demandPull shouldBe Coefficient.decimal(-75, 4)
  }

  it should "anchor aggregate inflation to expected inflation when fundamentals are flat" in {
    val low  = PriceLevel.update(Rate.decimal(15, 3), PriceIndex.Base, Multiplier.One, Coefficient.Zero, ExchangeRateShock.Zero)
    val high = PriceLevel.update(Rate.decimal(30, 3), PriceIndex.Base, Multiplier.One, Coefficient.Zero, ExchangeRateShock.Zero)

    high.inflation should be > low.inflation
  }

  it should "enforce price floor at 0.30" in {
    val r = PriceLevel.update(Rate.decimal(-50, 2), PriceIndex.decimal(31, 2), Multiplier.decimal(5, 1), Coefficient.decimal(-1, 1), ExchangeRateShock.Zero)
    decimal(r.priceLevel).should(be >= BigDecimal("0.30"))
  }

  it should "apply soft deflation floor at -1.5%/mo" in {
    val r = PriceLevel.update(Rate.decimal(-10, 2), PriceIndex.Base, Multiplier.decimal(5, 1), Coefficient.decimal(-5, 2), ExchangeRateShock.Zero)
    // The soft floor means deflation doesn't accelerate as fast
    // Raw monthly would be very negative; with floor, annualized should be bounded
    decimal(r.inflation).should(be > BigDecimal("-1.0")) // deflation shouldn't exceed 100% annualized
  }

  it should "expose demand, cost, and import channel diagnostics" in {
    val r = PriceLevel.update(Rate.decimal(25, 3), PriceIndex.Base, Multiplier.decimal(110, 2), Coefficient.decimal(3, 2), ExchangeRateShock.decimal(8, 2))

    r.rawMonthly shouldBe (r.demandPull + r.costPush + r.importPush)
    r.flooredMonthly shouldBe r.rawMonthly
    r.demandPull should be > Coefficient.Zero
    r.costPush should be > Coefficient.Zero
    r.importPush should be > Coefficient.Zero
  }

  // --- updateCbRate ---

  "Nbp.updateRate" should "increase rate when inflation rises (PLN)" in {
    val rate1 = Nbp.updateRate(Rate.decimal(575, 4), Rate.decimal(3, 2), Coefficient.Zero, totalPop * 95 / 100, totalPop)
    val rate2 = Nbp.updateRate(Rate.decimal(575, 4), Rate.decimal(10, 2), Coefficient.Zero, totalPop * 95 / 100, totalPop)
    decimal(rate2).should(be >= decimal(rate1))
  }

  it should "bound rate between floor and ceiling" in {
    val rateLow = Nbp.updateRate(Rate.decimal(5, 3), Rate.decimal(-50, 2), Coefficient.Zero, totalPop * 95 / 100, totalPop)
    decimal(rateLow).should(be >= decimal(p.monetary.rateFloor))

    val rateHigh = Nbp.updateRate(Rate.decimal(25, 2), Rate(1), Coefficient.decimal(5, 1), totalPop * 95 / 100, totalPop)
    decimal(rateHigh).should(be <= decimal(p.monetary.rateCeiling))
  }

  // --- updateGov ---

  "FiscalBudget.update" should "compute deficit as spending - revenue" in {
    val prev   = FiscalBudget.GovState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
    val result = FiscalBudget.update(
      FiscalBudget.Input(
        prev,
        priceLevel = PriceIndex.Base,
        citPaid = PLN(100000),
        govDividendRevenue = PLN.Zero,
        vat = PLN(200000),
      ),
    )
    decimal(result.deficit).shouldBe((decimal(p.fiscal.govBaseSpending) - 300000) +- BigDecimal("1.0"))
  }

  it should "accumulate debt" in {
    val prev   = FiscalBudget.GovState(PLN.Zero, PLN.Zero, PLN(1000000), PLN.Zero)
    val result = FiscalBudget.update(
      FiscalBudget.Input(
        prev,
        priceLevel = PriceIndex.Base,
        citPaid = PLN(100000),
        govDividendRevenue = PLN.Zero,
        vat = PLN(200000),
      ),
    )
    decimal(result.cumulativeDebt).shouldBe((1000000 + decimal(result.deficit)) +- BigDecimal("1.0"))
  }

  it should "track SOE dividends separately from tax revenue" in {
    val prev   = FiscalBudget.GovState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
    val result = FiscalBudget.update(
      FiscalBudget.Input(
        prev,
        priceLevel = PriceIndex.Base,
        citPaid = PLN(100000),
        govDividendRevenue = PLN(50000),
        vat = PLN(200000),
      ),
    )

    result.taxRevenue shouldBe PLN(300000)
    result.govDividendRevenue shouldBe PLN(50000)
    result.totalRevenue shouldBe PLN(350000)
  }
