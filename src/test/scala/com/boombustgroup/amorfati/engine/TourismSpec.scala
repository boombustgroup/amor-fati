package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import org.scalatest.flatspec.AnyFlatSpec
import com.boombustgroup.amorfati.Generators
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.markets.OpenEconomy
import com.boombustgroup.amorfati.types.*

class TourismSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]
  private val baseEr       = decimal(p.forex.baseExRate)

  // ==========================================================================
  // Config defaults (10 tests)
  // ==========================================================================

  "TourismInboundShare" should "default to 0.05" in {
    decimal(p.tourism.inboundShare) shouldBe BigDecimal("0.05")
  }

  "TourismOutboundShare" should "default to 0.03" in {
    decimal(p.tourism.outboundShare) shouldBe BigDecimal("0.03")
  }

  "TourismErElasticity" should "default to 0.6" in {
    decimal(p.tourism.erElasticity) shouldBe BigDecimal("0.6")
  }

  "TourismSeasonality" should "default to 0.40" in {
    decimal(p.tourism.seasonality) shouldBe BigDecimal("0.40")
  }

  "TourismPeakMonth" should "default to 7" in {
    p.tourism.peakMonth shouldBe 7
  }

  "TourismGrowthRate" should "default to 0.03" in {
    decimal(p.tourism.growthRate) shouldBe BigDecimal("0.03")
  }

  "TourismShockMonth" should "default to 0" in {
    p.tourism.shockMonth shouldBe 0
  }

  "TourismShockSize" should "default to 0.80" in {
    decimal(p.tourism.shockSize) shouldBe BigDecimal("0.80")
  }

  "TourismShockRecovery" should "default to 0.03" in {
    decimal(p.tourism.shockRecovery) shouldBe BigDecimal("0.03")
  }

  // ==========================================================================
  // Seasonal factor (3 tests)
  // ==========================================================================

  "Seasonal factor" should "peak in July (month 7) above 1.0" in {
    val monthInYear = 7
    val factor      = BigDecimal(1) + decimal(p.tourism.seasonality) * cosTurns(monthInYear - p.tourism.peakMonth, 12)
    factor shouldBe BigDecimal("1.0") + decimal(p.tourism.seasonality) // cos(0) = 1
    factor should be > BigDecimal("1.0")
  }

  it should "trough in January below 1.0" in {
    val monthInYear = 1
    val factor      = BigDecimal(1) + decimal(p.tourism.seasonality) * cosTurns(monthInYear - p.tourism.peakMonth, 12)
    factor should be < BigDecimal("1.0")
  }

  it should "average approximately 1.0 over 12 months" in {
    val factors = (1 to 12).map { m =>
      BigDecimal(1) + decimal(p.tourism.seasonality) * cosTurns(m - p.tourism.peakMonth, 12)
    }
    val avg     = factors.sum / BigDecimal(12)
    avg shouldBe BigDecimal("1.0") +- BigDecimal("0.0001")
  }

  // ==========================================================================
  // ER adjustment (3 tests)
  // ==========================================================================

  "ER adjustment" should "increase inbound tourism when PLN weakens" in {
    val weakerER     = baseEr * BigDecimal("1.2")
    val inboundErAdj = powDecimal(weakerER / baseEr, decimal(p.tourism.erElasticity))
    inboundErAdj should be > BigDecimal("1.0")
  }

  it should "decrease outbound tourism when PLN weakens" in {
    val weakerER      = baseEr * BigDecimal("1.2")
    val outboundErAdj = powDecimal(baseEr / weakerER, decimal(p.tourism.erElasticity))
    outboundErAdj should be < BigDecimal("1.0")
  }

  it should "apply partial pass-through (exponent = 0.6)" in {
    val weakerER     = baseEr * BigDecimal("1.2")
    val inboundErAdj = powDecimal(weakerER / baseEr, BigDecimal("0.6"))
    inboundErAdj should be > BigDecimal("1.0")
    inboundErAdj should be < BigDecimal("1.2") // partial, not full pass-through
  }

  // ==========================================================================
  // Trend adjustment (2 tests)
  // ==========================================================================

  "Trend adjustment" should "equal 1.0 at month 0" in {
    val trendAdj = powDecimal(BigDecimal(1) + decimal(p.tourism.growthRate) / BigDecimal(12), 0)
    trendAdj shouldBe BigDecimal("1.0")
  }

  it should "grow over 12 months" in {
    val trend12 = powDecimal(BigDecimal(1) + decimal(p.tourism.growthRate) / BigDecimal(12), 12)
    trend12 should be > BigDecimal("1.0")
    trend12 shouldBe (BigDecimal(1) + decimal(p.tourism.growthRate)) +- BigDecimal("0.001")
  }

  // ==========================================================================
  // COVID shock (3 tests)
  // ==========================================================================

  "COVID shock" should "have no disruption when shock month is 0" in {
    val disruption =
      if 0 > 0 && 10 >= 0 then decimal(p.tourism.shockSize) * powDecimal(BigDecimal(1) - decimal(p.tourism.shockRecovery), 10)
      else BigDecimal(0)
    disruption shouldBe BigDecimal("0.0")
  }

  it should "apply disruption at shock trigger month" in {
    val shockMonth  = 24
    val m           = 24
    val disruption  =
      if shockMonth > 0 && m >= shockMonth then decimal(p.tourism.shockSize) * powDecimal(BigDecimal(1) - decimal(p.tourism.shockRecovery), m - shockMonth)
      else BigDecimal(0)
    // At trigger month: disruption = 0.80 * (0.97)^0 = 0.80
    disruption shouldBe BigDecimal("0.80")
    val shockFactor = BigDecimal(1) - disruption
    shockFactor shouldBe BigDecimal("0.20") +- BigDecimal("1e-10")
  }

  it should "recover gradually after shock" in {
    val shockMonth  = 24
    val m           = 36 // 12 months after shock
    val disruption  =
      decimal(p.tourism.shockSize) * powDecimal(BigDecimal(1) - decimal(p.tourism.shockRecovery), m - shockMonth)
    disruption should be < decimal(p.tourism.shockSize)
    disruption should be > BigDecimal("0.0")
    val shockFactor = BigDecimal(1) - disruption
    shockFactor should be > BigDecimal("0.20") // Better than at trigger
    shockFactor should be < BigDecimal("1.0") // Not fully recovered
  }

  // ==========================================================================
  // Full formula (1 test)
  // ==========================================================================

  "Full formula" should "combine all components multiplicatively" in {
    val baseGdp     = BigDecimal("1000000000.0")
    val monthInYear = 7 // peak month
    val m           = 12
    val er          = baseEr * BigDecimal("1.1")

    val seasonalFactor = BigDecimal(1) + decimal(p.tourism.seasonality) * cosTurns(monthInYear - p.tourism.peakMonth, 12)
    val inboundErAdj   = powDecimal(er / baseEr, decimal(p.tourism.erElasticity))
    val outboundErAdj  = powDecimal(baseEr / er, decimal(p.tourism.erElasticity))
    val trendAdj       = powDecimal(BigDecimal(1) + decimal(p.tourism.growthRate) / BigDecimal(12), m)
    val shockFactor    = BigDecimal(1) // no shock

    val inbound  = baseGdp * decimal(p.tourism.inboundShare) *
      seasonalFactor * inboundErAdj * trendAdj * shockFactor
    val outbound = baseGdp * decimal(p.tourism.outboundShare) *
      seasonalFactor * outboundErAdj * trendAdj * shockFactor

    inbound should be > BigDecimal("0.0")
    outbound should be > BigDecimal("0.0")
    // Inbound > outbound because: higher share (5% vs 3%) AND weaker PLN boosts inbound
    inbound should be > outbound
    // Seasonal peak → factor = 1.4
    seasonalFactor shouldBe BigDecimal("1.4") +- BigDecimal("1e-10")
    // ER: weaker PLN → inbound ↑, outbound ↓
    inboundErAdj should be > BigDecimal("1.0")
    outboundErAdj should be < BigDecimal("1.0")
  }

  // ==========================================================================
  // OpenEconomy integration (2 tests)
  // ==========================================================================

  "OpenEconomy exports" should "include tourismExport" in {
    val prevBop   = OpenEconomy.BopState.zero
    val prevForex = OpenEconomy.ForexState(p.forex.baseExRate, PLN.Zero, p.openEcon.exportBase, PLN.Zero, PLN.Zero)

    val base          = OpenEconomy.StepInput(
      prevBop = prevBop,
      prevForex = prevForex,
      importCons = PLN.Zero,
      techImports = PLN.Zero,
      autoRatio = Share.Zero,
      domesticRate = Rate.decimal(5, 2),
      gdp = PLN(1000000000),
      priceLevel = PriceIndex.Base,
      sectorOutputs = Vector.fill(p.sectorDefs.length)(PLN(100000000)),
      month = ExecutionMonth(1),
      nbpFxReserves = prevBop.reserves,
    )
    val resultWith    = OpenEconomy.step(base.copy(tourismExport = PLN(1000)))
    val resultWithout = OpenEconomy.step(base.copy(tourismExport = PLN.Zero))

    resultWith.bop.exports.shouldBe(resultWithout.bop.exports + PLN(1000))
  }

  "OpenEconomy imports" should "include tourismImport" in {
    val prevBop   = OpenEconomy.BopState.zero
    val prevForex = OpenEconomy.ForexState(p.forex.baseExRate, PLN.Zero, p.openEcon.exportBase, PLN.Zero, PLN.Zero)

    val base          = OpenEconomy.StepInput(
      prevBop = prevBop,
      prevForex = prevForex,
      importCons = PLN.Zero,
      techImports = PLN.Zero,
      autoRatio = Share.Zero,
      domesticRate = Rate.decimal(5, 2),
      gdp = PLN(1000000000),
      priceLevel = PriceIndex.Base,
      sectorOutputs = Vector.fill(p.sectorDefs.length)(PLN(100000000)),
      month = ExecutionMonth(1),
      nbpFxReserves = prevBop.reserves,
    )
    val resultWith    = OpenEconomy.step(base.copy(tourismImport = PLN(500)))
    val resultWithout = OpenEconomy.step(base.copy(tourismImport = PLN.Zero))

    resultWith.bop.totalImports.shouldBe(resultWithout.bop.totalImports + PLN(500))
  }

  // ==========================================================================
  // World defaults (1 test)
  // ==========================================================================

  "World" should "default tourismExport and tourismImport to 0.0" in {
    val w = Generators.testWorld(
      priceLevel = PriceIndex.Base,
      currentSigmas = Vector.fill(p.sectorDefs.length)(Sigma.decimal(1, 1)),
      forex = OpenEconomy.ForexState(p.forex.baseExRate, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      marketWage = PLN(5000),
      reservationWage = PLN(4000),
    )
    w.flows.tourismExport.shouldBe(PLN.Zero)
    w.flows.tourismImport.shouldBe(PLN.Zero)
  }
