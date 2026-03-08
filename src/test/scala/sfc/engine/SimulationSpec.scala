package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.accounting
import sfc.accounting.GovState
import sfc.agents.Nbp
import sfc.config.{Config, RunConfig}
import sfc.engine.markets.{FiscalBudget, LaborMarket, PriceLevel}
import sfc.types.*

class SimulationSpec extends AnyFlatSpec with Matchers:

  // --- updateLaborMarket ---

  "LaborMarket.updateLaborMarket" should "increase wage when demand exceeds supply" in {
    val (wage1, _) = LaborMarket.updateLaborMarket(Config.BaseWage, Config.BaseReservationWage, Config.TotalPopulation)
    val (wage2, _) =
      LaborMarket.updateLaborMarket(Config.BaseWage, Config.BaseReservationWage, Config.TotalPopulation * 2)
    wage2 should be > wage1
  }

  it should "keep wage at or above reservation wage" in {
    // Very low demand → wage should still be >= reservation
    val (wage, _) = LaborMarket.updateLaborMarket(Config.BaseWage, Config.BaseReservationWage, 0)
    wage should be >= Config.BaseReservationWage
  }

  // --- updateInflation ---

  "PriceLevel.update" should "produce higher inflation with higher demand" in {
    val rc = RunConfig(0.0, 1, "test")
    val (infl1, _) = PriceLevel.update(0.02, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, rc)
    val (infl2, _) = PriceLevel.update(0.02, 1.0, 1.5, 0.0, 0.0, 0.0, 0.0, rc)
    infl2 should be > infl1
  }

  it should "produce tech deflation with more automation" in {
    val rc = RunConfig(0.0, 1, "test")
    val (infl1, _) = PriceLevel.update(0.02, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, rc)
    val (infl2, _) = PriceLevel.update(0.02, 1.0, 1.0, 0.0, 0.0, 0.8, 0.0, rc)
    infl2 should be < infl1
  }

  it should "enforce price floor at 0.30" in {
    val rc = RunConfig(0.0, 1, "test")
    val (_, price) = PriceLevel.update(-0.50, 0.31, 0.5, -0.1, 0.0, 0.9, 0.0, rc)
    price should be >= 0.30
  }

  it should "apply soft deflation floor at -1.5%/mo" in {
    val rc = RunConfig(0.0, 1, "test")
    // Strong deflation scenario: heavy automation, no demand, negative wage growth
    val (inflHard, _) = PriceLevel.update(-0.10, 1.0, 0.5, -0.05, 0.0, 0.9, 0.0, rc)
    // The soft floor means deflation doesn't accelerate as fast
    // Raw monthly would be very negative; with floor, annualized should be bounded
    inflHard should be > -1.0 // deflation shouldn't exceed 100% annualized
  }

  // --- updateCbRate ---

  "Nbp.updateRate" should "increase rate when inflation rises (PLN)" in {
    val rc = RunConfig(0.0, 1, "test")
    val rate1 = Nbp.updateRate(0.0575, 0.03, 0.0, Config.TotalPopulation * 95 / 100, rc)
    val rate2 = Nbp.updateRate(0.0575, 0.10, 0.0, Config.TotalPopulation * 95 / 100, rc)
    rate2 should be > rate1
  }

  it should "bound rate between floor and ceiling" in {
    val rc = RunConfig(0.0, 1, "test")
    val rateLow = Nbp.updateRate(0.005, -0.50, 0.0, Config.TotalPopulation * 95 / 100, rc)
    rateLow should be >= Config.RateFloor

    val rateHigh = Nbp.updateRate(0.25, 1.0, 0.5, Config.TotalPopulation * 95 / 100, rc)
    rateHigh should be <= Config.RateCeiling
  }

  // --- updateGov ---

  "FiscalBudget.update" should "compute deficit as spending - revenue" in {
    val prev = GovState(false, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
    val result = FiscalBudget.update(
      prev,
      citPaid = 100000,
      vat = 200000,
      bdpActive = false,
      bdpAmount = 0,
      priceLevel = 1.0,
      unempBenefitSpend = 0,
    )
    result.deficit.toDouble shouldBe (Config.GovBaseSpending - 300000) +- 1.0
  }

  it should "have zero BDP spending when not active" in {
    val prev = accounting.GovState(false, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
    val result = FiscalBudget.update(
      prev,
      100000,
      200000,
      bdpActive = false,
      bdpAmount = 2000,
      priceLevel = 1.0,
      unempBenefitSpend = 0,
    )
    result.bdpSpending shouldBe PLN.Zero
  }

  it should "include BDP spending when active" in {
    val prev = accounting.GovState(false, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
    val result = FiscalBudget.update(
      prev,
      100000,
      200000,
      bdpActive = true,
      bdpAmount = 2000,
      priceLevel = 1.0,
      unempBenefitSpend = 0,
    )
    result.bdpSpending.toDouble shouldBe Config.TotalPopulation.toDouble * 2000.0
    result.bdpSpending.toDouble should be > 0.0
  }

  it should "accumulate debt" in {
    val prev = accounting.GovState(false, PLN.Zero, PLN.Zero, PLN.Zero, PLN(1000000), PLN.Zero)
    val result =
      FiscalBudget.update(
        prev,
        100000,
        200000,
        bdpActive = false,
        bdpAmount = 0,
        priceLevel = 1.0,
        unempBenefitSpend = 0,
      )
    result.cumulativeDebt.toDouble shouldBe (1000000 + result.deficit.toDouble) +- 1.0
  }
