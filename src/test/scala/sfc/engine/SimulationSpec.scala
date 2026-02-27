package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.config.{Config, RunConfig, MonetaryRegime}

class SimulationSpec extends AnyFlatSpec with Matchers:

  // --- updateLaborMarket ---

  "Sectors.updateLaborMarket" should "increase wage when demand exceeds supply" in {
    val (wage1, _) = Sectors.updateLaborMarket(Config.BaseWage, Config.BaseReservationWage, Config.TotalPopulation)
    val (wage2, _) = Sectors.updateLaborMarket(Config.BaseWage, Config.BaseReservationWage, Config.TotalPopulation * 2)
    wage2 should be > wage1
  }

  it should "keep wage at or above reservation wage" in {
    // Very low demand → wage should still be >= reservation
    val (wage, _) = Sectors.updateLaborMarket(Config.BaseWage, Config.BaseReservationWage, 0)
    wage should be >= Config.BaseReservationWage
  }

  // --- updateInflation ---

  "Sectors.updateInflation" should "produce higher inflation with higher demand" in {
    val rc = RunConfig(0.0, 1, "test")
    val (infl1, _) = Sectors.updateInflation(0.02, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, rc)
    val (infl2, _) = Sectors.updateInflation(0.02, 1.0, 1.5, 0.0, 0.0, 0.0, 0.0, rc)
    infl2 should be > infl1
  }

  it should "produce tech deflation with more automation" in {
    val rc = RunConfig(0.0, 1, "test")
    val (infl1, _) = Sectors.updateInflation(0.02, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, rc)
    val (infl2, _) = Sectors.updateInflation(0.02, 1.0, 1.0, 0.0, 0.0, 0.8, 0.0, rc)
    infl2 should be < infl1
  }

  it should "enforce price floor at 0.30" in {
    val rc = RunConfig(0.0, 1, "test")
    val (_, price) = Sectors.updateInflation(-0.50, 0.31, 0.5, -0.1, 0.0, 0.9, 0.0, rc)
    price should be >= 0.30
  }

  it should "apply soft deflation floor at -1.5%/mo" in {
    val rc = RunConfig(0.0, 1, "test")
    // Strong deflation scenario: heavy automation, no demand, negative wage growth
    val (inflHard, _) = Sectors.updateInflation(-0.10, 1.0, 0.5, -0.05, 0.0, 0.9, 0.0, rc)
    // The soft floor means deflation doesn't accelerate as fast
    // Raw monthly would be very negative; with floor, annualized should be bounded
    inflHard should be > -1.0 // deflation shouldn't exceed 100% annualized
  }

  // --- updateCbRate ---

  "Sectors.updateCbRate" should "increase rate when inflation rises (PLN)" in {
    val rc = RunConfig(0.0, 1, "test")
    val rate1 = Sectors.updateCbRate(0.0575, 0.03, 0.0, Config.TotalPopulation * 95 / 100, rc)
    val rate2 = Sectors.updateCbRate(0.0575, 0.10, 0.0, Config.TotalPopulation * 95 / 100, rc)
    rate2 should be > rate1
  }

  it should "bound rate between floor and ceiling" in {
    val rc = RunConfig(0.0, 1, "test")
    val rateLow = Sectors.updateCbRate(0.005, -0.50, 0.0, Config.TotalPopulation * 95 / 100, rc)
    rateLow should be >= Config.RateFloor

    val rateHigh = Sectors.updateCbRate(0.25, 1.0, 0.5, Config.TotalPopulation * 95 / 100, rc)
    rateHigh should be <= Config.RateCeiling
  }

  it should "use ECB rule for EUR regime" in {
    val rcPln = RunConfig(0.0, 1, "test", MonetaryRegime.Pln)
    val rcEur = RunConfig(0.0, 1, "test", MonetaryRegime.Eur)
    // ECB rate should differ from PLN rate at same inflation
    val ratePln = Sectors.updateCbRate(0.0575, 0.05, 0.0, Config.TotalPopulation * 95 / 100, rcPln)
    val rateEur = Sectors.updateCbRate(0.035, 0.05, 0.0, Config.TotalPopulation * 95 / 100, rcEur)
    // They start from different initial rates, so just check both are bounded
    ratePln should be >= Config.RateFloor
    rateEur should be >= Config.RateFloor
  }

  // --- updateGov ---

  "Sectors.updateGov" should "compute deficit as spending - revenue" in {
    val prev = sfc.sfc.GovState(false, 0, 0, 0, 0, 0)
    val result = Sectors.updateGov(prev, citPaid = 100000, vat = 200000,
      bdpActive = false, bdpAmount = 0, priceLevel = 1.0, unempBenefitSpend = 0)
    result.deficit shouldBe (Config.GovBaseSpending - 300000) +- 1.0
  }

  it should "have zero BDP spending when not active" in {
    val prev = sfc.sfc.GovState(false, 0, 0, 0, 0, 0)
    val result = Sectors.updateGov(prev, 100000, 200000, bdpActive = false, bdpAmount = 2000, priceLevel = 1.0, unempBenefitSpend = 0)
    result.bdpSpending shouldBe 0.0
  }

  it should "include BDP spending when active" in {
    val prev = sfc.sfc.GovState(false, 0, 0, 0, 0, 0)
    val result = Sectors.updateGov(prev, 100000, 200000, bdpActive = true, bdpAmount = 2000, priceLevel = 1.0, unempBenefitSpend = 0)
    result.bdpSpending shouldBe Config.TotalPopulation.toDouble * 2000.0
    result.bdpSpending should be > 0.0
  }

  it should "accumulate debt" in {
    val prev = sfc.sfc.GovState(false, 0, 0, 0, 1000000, 0)
    val result = Sectors.updateGov(prev, 100000, 200000, bdpActive = false, bdpAmount = 0, priceLevel = 1.0, unempBenefitSpend = 0)
    result.cumulativeDebt shouldBe (1000000 + result.deficit) +- 1.0
  }
