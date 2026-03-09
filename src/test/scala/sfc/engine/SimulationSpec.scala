package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.accounting
import sfc.accounting.GovState
import sfc.agents.Nbp
import sfc.config.{RunConfig, SimParams}
import sfc.engine.markets.{FiscalBudget, LaborMarket, PriceLevel}
import sfc.types.*

class SimulationSpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]
  private val totalPop     = p.pop.firmsCount * p.pop.workersPerFirm

  // --- updateLaborMarket ---

  "LaborMarket.updateLaborMarket" should "increase wage when demand exceeds supply" in {
    val (wage1, _) = LaborMarket.updateLaborMarket(
      p.household.baseWage.toDouble,
      p.household.baseReservationWage.toDouble,
      totalPop,
      totalPop,
    )
    val (wage2, _) =
      LaborMarket.updateLaborMarket(
        p.household.baseWage.toDouble,
        p.household.baseReservationWage.toDouble,
        totalPop * 2,
        totalPop,
      )
    wage2 should be > wage1
  }

  it should "keep wage at or above reservation wage" in {
    // Very low demand → wage should still be >= reservation
    val (wage, _) = LaborMarket.updateLaborMarket(
      p.household.baseWage.toDouble,
      p.household.baseReservationWage.toDouble,
      0,
      totalPop,
    )
    wage should be >= p.household.baseReservationWage.toDouble
  }

  // --- updateInflation ---

  "PriceLevel.update" should "produce higher inflation with higher demand" in {
    val rc         = RunConfig(1, "test")
    val (infl1, _) = PriceLevel.update(0.02, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, rc)
    val (infl2, _) = PriceLevel.update(0.02, 1.0, 1.5, 0.0, 0.0, 0.0, 0.0, rc)
    infl2 should be > infl1
  }

  it should "produce tech deflation with more automation" in {
    val rc         = RunConfig(1, "test")
    val (infl1, _) = PriceLevel.update(0.02, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, rc)
    val (infl2, _) = PriceLevel.update(0.02, 1.0, 1.0, 0.0, 0.0, 0.8, 0.0, rc)
    infl2 should be < infl1
  }

  it should "enforce price floor at 0.30" in {
    val rc         = RunConfig(1, "test")
    val (_, price) = PriceLevel.update(-0.50, 0.31, 0.5, -0.1, 0.0, 0.9, 0.0, rc)
    price should be >= 0.30
  }

  it should "apply soft deflation floor at -1.5%/mo" in {
    val rc            = RunConfig(1, "test")
    // Strong deflation scenario: heavy automation, no demand, negative wage growth
    val (inflHard, _) = PriceLevel.update(-0.10, 1.0, 0.5, -0.05, 0.0, 0.9, 0.0, rc)
    // The soft floor means deflation doesn't accelerate as fast
    // Raw monthly would be very negative; with floor, annualized should be bounded
    inflHard should be > -1.0 // deflation shouldn't exceed 100% annualized
  }

  // --- updateCbRate ---

  "Nbp.updateRate" should "increase rate when inflation rises (PLN)" in {
    val rc    = RunConfig(1, "test")
    val rate1 = Nbp.updateRate(0.0575, 0.03, 0.0, totalPop * 95 / 100, totalPop, rc)
    val rate2 = Nbp.updateRate(0.0575, 0.10, 0.0, totalPop * 95 / 100, totalPop, rc)
    rate2 should be > rate1
  }

  it should "bound rate between floor and ceiling" in {
    val rc      = RunConfig(1, "test")
    val rateLow = Nbp.updateRate(0.005, -0.50, 0.0, totalPop * 95 / 100, totalPop, rc)
    rateLow should be >= p.monetary.rateFloor.toDouble

    val rateHigh = Nbp.updateRate(0.25, 1.0, 0.5, totalPop * 95 / 100, totalPop, rc)
    rateHigh should be <= p.monetary.rateCeiling.toDouble
  }

  // --- updateGov ---

  "FiscalBudget.update" should "compute deficit as spending - revenue" in {
    val prev   = GovState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
    val result = FiscalBudget.update(
      prev,
      citPaid = 100000,
      vat = 200000,
      priceLevel = 1.0,
      unempBenefitSpend = 0,
    )
    result.deficit.toDouble shouldBe (p.fiscal.govBaseSpending.toDouble - 300000) +- 1.0
  }

  it should "accumulate debt" in {
    val prev   = accounting.GovState(PLN.Zero, PLN.Zero, PLN(1000000), PLN.Zero)
    val result =
      FiscalBudget.update(
        prev,
        100000,
        200000,
        priceLevel = 1.0,
        unempBenefitSpend = 0,
      )
    result.cumulativeDebt.toDouble shouldBe (1000000 + result.deficit.toDouble) +- 1.0
  }
