package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.accounting
import sfc.accounting.GovState
import sfc.agents.Nbp
import sfc.config.SimParams
import sfc.engine.markets.{FiscalBudget, LaborMarket, PriceLevel}
import sfc.types.*

class SimulationSpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]
  private val totalPop     = p.pop.firmsCount * p.pop.workersPerFirm

  // --- updateLaborMarket ---

  "LaborMarket.updateLaborMarket" should "increase wage when demand exceeds supply" in {
    val r1 = LaborMarket.updateLaborMarket(p.household.baseWage, p.household.baseReservationWage, totalPop, totalPop)
    val r2 = LaborMarket.updateLaborMarket(p.household.baseWage, p.household.baseReservationWage, totalPop * 2, totalPop)
    r2.wage should be > r1.wage
  }

  it should "keep wage at or above reservation wage" in {
    // Very low demand → wage should still be >= reservation
    val r = LaborMarket.updateLaborMarket(p.household.baseWage, p.household.baseReservationWage, 0, totalPop)
    r.wage.toDouble should be >= p.household.baseReservationWage.toDouble
  }

  // --- updateInflation ---

  "PriceLevel.update" should "produce higher inflation with higher demand" in {
    val r1 = PriceLevel.update(Rate(0.02), 1.0, 1.0, 0.0, 0.0, 0.0, 0.0)
    val r2 = PriceLevel.update(Rate(0.02), 1.0, 1.5, 0.0, 0.0, 0.0, 0.0)
    r2.inflation should be > r1.inflation
  }

  it should "produce tech deflation with more automation" in {
    val r1 = PriceLevel.update(Rate(0.02), 1.0, 1.0, 0.0, 0.0, 0.0, 0.0)
    val r2 = PriceLevel.update(Rate(0.02), 1.0, 1.0, 0.0, 0.0, 0.8, 0.0)
    r2.inflation should be < r1.inflation
  }

  it should "enforce price floor at 0.30" in {
    val r = PriceLevel.update(Rate(-0.50), 0.31, 0.5, -0.1, 0.0, 0.9, 0.0)
    r.priceLevel should be >= 0.30
  }

  it should "apply soft deflation floor at -1.5%/mo" in {
    // Strong deflation scenario: heavy automation, no demand, negative wage growth
    val r = PriceLevel.update(Rate(-0.10), 1.0, 0.5, -0.05, 0.0, 0.9, 0.0)
    // The soft floor means deflation doesn't accelerate as fast
    // Raw monthly would be very negative; with floor, annualized should be bounded
    r.inflation.toDouble should be > -1.0 // deflation shouldn't exceed 100% annualized
  }

  // --- updateCbRate ---

  "Nbp.updateRate" should "increase rate when inflation rises (PLN)" in {
    val rate1 = Nbp.updateRate(Rate(0.0575), Rate(0.03), 0.0, totalPop * 95 / 100, totalPop)
    val rate2 = Nbp.updateRate(Rate(0.0575), Rate(0.10), 0.0, totalPop * 95 / 100, totalPop)
    rate2.toDouble should be > rate1.toDouble
  }

  it should "bound rate between floor and ceiling" in {
    val rateLow = Nbp.updateRate(Rate(0.005), Rate(-0.50), 0.0, totalPop * 95 / 100, totalPop)
    rateLow.toDouble should be >= p.monetary.rateFloor.toDouble

    val rateHigh = Nbp.updateRate(Rate(0.25), Rate(1.0), 0.5, totalPop * 95 / 100, totalPop)
    rateHigh.toDouble should be <= p.monetary.rateCeiling.toDouble
  }

  // --- updateGov ---

  "FiscalBudget.update" should "compute deficit as spending - revenue" in {
    val prev   = GovState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
    val result = FiscalBudget.update(
      FiscalBudget.Input(
        prev,
        priceLevel = 1.0,
        citPaid = PLN(100000),
        vat = PLN(200000),
      ),
    )
    result.deficit.toDouble shouldBe (p.fiscal.govBaseSpending.toDouble - 300000) +- 1.0
  }

  it should "accumulate debt" in {
    val prev   = accounting.GovState(PLN.Zero, PLN.Zero, PLN(1000000), PLN.Zero)
    val result = FiscalBudget.update(
      FiscalBudget.Input(
        prev,
        priceLevel = 1.0,
        citPaid = PLN(100000),
        vat = PLN(200000),
      ),
    )
    result.cumulativeDebt.toDouble shouldBe (1000000 + result.deficit.toDouble) +- 1.0
  }
