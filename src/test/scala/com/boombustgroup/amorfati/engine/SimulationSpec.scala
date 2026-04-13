package com.boombustgroup.amorfati.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.agents.Nbp
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.markets.{FiscalBudget, LaborMarket, PriceLevel}
import com.boombustgroup.amorfati.types.*

class SimulationSpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]
  private val td           = ComputationBoundary
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
    td.toDouble(r.wage).should(be >= td.toDouble(p.household.baseReservationWage))
  }

  // --- updateInflation ---

  "PriceLevel.update" should "produce higher inflation with higher demand" in {
    val r1 = PriceLevel.update(Rate(0.02), PriceIndex.Base, Multiplier.One, Coefficient.Zero, ExchangeRateShock.Zero)
    val r2 = PriceLevel.update(Rate(0.02), PriceIndex.Base, Multiplier(1.5), Coefficient.Zero, ExchangeRateShock.Zero)
    r2.inflation.should(be > r1.inflation)
  }

  it should "produce higher inflation with FX import pressure" in {
    val r1 = PriceLevel.update(Rate(0.02), PriceIndex.Base, Multiplier.One, Coefficient.Zero, ExchangeRateShock.Zero)
    val r2 = PriceLevel.update(Rate(0.02), PriceIndex.Base, Multiplier.One, Coefficient.Zero, ExchangeRateShock(0.2))
    r2.inflation.should(be > r1.inflation)
  }

  it should "enforce price floor at 0.30" in {
    val r = PriceLevel.update(Rate(-0.50), PriceIndex(0.31), Multiplier(0.5), Coefficient(-0.1), ExchangeRateShock.Zero)
    td.toDouble(r.priceLevel).should(be >= 0.30)
  }

  it should "apply soft deflation floor at -1.5%/mo" in {
    val r = PriceLevel.update(Rate(-0.10), PriceIndex.Base, Multiplier(0.5), Coefficient(-0.05), ExchangeRateShock.Zero)
    // The soft floor means deflation doesn't accelerate as fast
    // Raw monthly would be very negative; with floor, annualized should be bounded
    td.toDouble(r.inflation).should(be > -1.0) // deflation shouldn't exceed 100% annualized
  }

  // --- updateCbRate ---

  "Nbp.updateRate" should "increase rate when inflation rises (PLN)" in {
    val rate1 = Nbp.updateRate(Rate(0.0575), Rate(0.03), Coefficient.Zero, totalPop * 95 / 100, totalPop)
    val rate2 = Nbp.updateRate(Rate(0.0575), Rate(0.10), Coefficient.Zero, totalPop * 95 / 100, totalPop)
    td.toDouble(rate2).should(be >= td.toDouble(rate1))
  }

  it should "bound rate between floor and ceiling" in {
    val rateLow = Nbp.updateRate(Rate(0.005), Rate(-0.50), Coefficient.Zero, totalPop * 95 / 100, totalPop)
    td.toDouble(rateLow).should(be >= td.toDouble(p.monetary.rateFloor))

    val rateHigh = Nbp.updateRate(Rate(0.25), Rate(1.0), Coefficient(0.5), totalPop * 95 / 100, totalPop)
    td.toDouble(rateHigh).should(be <= td.toDouble(p.monetary.rateCeiling))
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
    td.toDouble(result.deficit).shouldBe((td.toDouble(p.fiscal.govBaseSpending) - 300000) +- 1.0)
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
    td.toDouble(result.cumulativeDebt).shouldBe((1000000 + td.toDouble(result.deficit)) +- 1.0)
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
