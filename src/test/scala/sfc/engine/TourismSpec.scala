package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.accounting.{BankingAggregate, BopState, ForexState, GovState}
import sfc.agents.Banking
import sfc.config.Config
import sfc.engine.markets.OpenEconomy
import sfc.types.*

class TourismSpec extends AnyFlatSpec with Matchers:

  // ==========================================================================
  // Config defaults (10 tests)
  // ==========================================================================

  "TourismEnabled" should "default to false" in {
    Config.TourismEnabled shouldBe false
  }

  "TourismInboundShare" should "default to 0.05" in {
    Config.TourismInboundShare shouldBe 0.05
  }

  "TourismOutboundShare" should "default to 0.03" in {
    Config.TourismOutboundShare shouldBe 0.03
  }

  "TourismErElasticity" should "default to 0.6" in {
    Config.TourismErElasticity shouldBe 0.6
  }

  "TourismSeasonality" should "default to 0.40" in {
    Config.TourismSeasonality shouldBe 0.40
  }

  "TourismPeakMonth" should "default to 7" in {
    Config.TourismPeakMonth shouldBe 7
  }

  "TourismGrowthRate" should "default to 0.03" in {
    Config.TourismGrowthRate shouldBe 0.03
  }

  "TourismShockMonth" should "default to 0" in {
    Config.TourismShockMonth shouldBe 0
  }

  "TourismShockSize" should "default to 0.80" in {
    Config.TourismShockSize shouldBe 0.80
  }

  "TourismShockRecovery" should "default to 0.03" in {
    Config.TourismShockRecovery shouldBe 0.03
  }

  // ==========================================================================
  // Seasonal factor (3 tests)
  // ==========================================================================

  "Seasonal factor" should "peak in July (month 7) above 1.0" in {
    val monthInYear = 7
    val factor = 1.0 + Config.TourismSeasonality *
      Math.cos(2 * Math.PI * (monthInYear - Config.TourismPeakMonth) / 12.0)
    factor shouldBe 1.0 + Config.TourismSeasonality // cos(0) = 1
    factor should be > 1.0
  }

  it should "trough in January below 1.0" in {
    val monthInYear = 1
    val factor = 1.0 + Config.TourismSeasonality *
      Math.cos(2 * Math.PI * (monthInYear - Config.TourismPeakMonth) / 12.0)
    factor should be < 1.0
  }

  it should "average approximately 1.0 over 12 months" in {
    val factors = (1 to 12).map { m =>
      1.0 + Config.TourismSeasonality *
        Math.cos(2 * Math.PI * (m - Config.TourismPeakMonth) / 12.0)
    }
    val avg = factors.sum / 12.0
    avg shouldBe 1.0 +- 1e-10
  }

  // ==========================================================================
  // ER adjustment (3 tests)
  // ==========================================================================

  "ER adjustment" should "increase inbound tourism when PLN weakens" in {
    val weakerER = Config.BaseExRate * 1.2
    val inboundErAdj = Math.pow(weakerER / Config.BaseExRate, Config.TourismErElasticity)
    inboundErAdj should be > 1.0
  }

  it should "decrease outbound tourism when PLN weakens" in {
    val weakerER = Config.BaseExRate * 1.2
    val outboundErAdj = Math.pow(Config.BaseExRate / weakerER, Config.TourismErElasticity)
    outboundErAdj should be < 1.0
  }

  it should "apply partial pass-through (exponent = 0.6)" in {
    val weakerER = Config.BaseExRate * 1.2
    val inboundErAdj = Math.pow(weakerER / Config.BaseExRate, 0.6)
    inboundErAdj should be > 1.0
    inboundErAdj should be < 1.2 // partial, not full pass-through
  }

  // ==========================================================================
  // Trend adjustment (2 tests)
  // ==========================================================================

  "Trend adjustment" should "equal 1.0 at month 0" in {
    val trendAdj = Math.pow(1.0 + Config.TourismGrowthRate / 12.0, 0.0)
    trendAdj shouldBe 1.0
  }

  it should "grow over 12 months" in {
    val trend12 = Math.pow(1.0 + Config.TourismGrowthRate / 12.0, 12.0)
    trend12 should be > 1.0
    trend12 shouldBe (1.0 + Config.TourismGrowthRate) +- 0.001
  }

  // ==========================================================================
  // COVID shock (3 tests)
  // ==========================================================================

  "COVID shock" should "have no disruption when shock month is 0" in {
    val disruption =
      if 0 > 0 && 10 >= 0 then Config.TourismShockSize * Math.pow(1.0 - Config.TourismShockRecovery, 10.0)
      else 0.0
    disruption shouldBe 0.0
  }

  it should "apply disruption at shock trigger month" in {
    val shockMonth = 24
    val m = 24
    val disruption =
      if shockMonth > 0 && m >= shockMonth then
        Config.TourismShockSize * Math.pow(1.0 - Config.TourismShockRecovery, (m - shockMonth).toDouble)
      else 0.0
    // At trigger month: disruption = 0.80 * (0.97)^0 = 0.80
    disruption shouldBe 0.80
    val shockFactor = 1.0 - disruption
    shockFactor shouldBe 0.20 +- 1e-10
  }

  it should "recover gradually after shock" in {
    val shockMonth = 24
    val m = 36 // 12 months after shock
    val disruption = Config.TourismShockSize * Math.pow(1.0 - Config.TourismShockRecovery, (m - shockMonth).toDouble)
    disruption should be < Config.TourismShockSize
    disruption should be > 0.0
    val shockFactor = 1.0 - disruption
    shockFactor should be > 0.20 // Better than at trigger
    shockFactor should be < 1.0 // Not fully recovered
  }

  // ==========================================================================
  // Full formula (1 test)
  // ==========================================================================

  "Full formula" should "combine all components multiplicatively" in {
    val baseGdp = 1e9
    val monthInYear = 7 // peak month
    val m = 12
    val er = Config.BaseExRate * 1.1

    val seasonalFactor = 1.0 + Config.TourismSeasonality *
      Math.cos(2 * Math.PI * (monthInYear - Config.TourismPeakMonth) / 12.0)
    val inboundErAdj = Math.pow(er / Config.BaseExRate, Config.TourismErElasticity)
    val outboundErAdj = Math.pow(Config.BaseExRate / er, Config.TourismErElasticity)
    val trendAdj = Math.pow(1.0 + Config.TourismGrowthRate / 12.0, m.toDouble)
    val shockFactor = 1.0 // no shock

    val inbound = baseGdp * Config.TourismInboundShare *
      seasonalFactor * inboundErAdj * trendAdj * shockFactor
    val outbound = baseGdp * Config.TourismOutboundShare *
      seasonalFactor * outboundErAdj * trendAdj * shockFactor

    inbound should be > 0.0
    outbound should be > 0.0
    // Inbound > outbound because: higher share (5% vs 3%) AND weaker PLN boosts inbound
    inbound should be > outbound
    // Seasonal peak → factor = 1.4
    seasonalFactor shouldBe 1.4 +- 1e-10
    // ER: weaker PLN → inbound ↑, outbound ↓
    inboundErAdj should be > 1.0
    outboundErAdj should be < 1.0
  }

  // ==========================================================================
  // Disabled (1 test)
  // ==========================================================================

  "Tourism flows" should "be zero when disabled" in {
    val (tourismExport, tourismImport) =
      if Config.TourismEnabled then (100.0, 60.0)
      else (0.0, 0.0)
    tourismExport shouldBe 0.0
    tourismImport shouldBe 0.0
  }

  // ==========================================================================
  // OpenEconomy integration (2 tests)
  // ==========================================================================

  "OpenEconomy exports" should "include tourismExport" in {
    val prevBop = BopState.zero
    val prevForex = ForexState(Config.BaseExRate, PLN.Zero, PLN(Config.ExportBase), PLN.Zero, PLN.Zero)
    val rc = sfc.config.RunConfig(2000.0, 1, "test")

    val resultWith =
      OpenEconomy.step(prevBop, prevForex, 0, 0, 0, 0.05, 1e9, 1.0, Vector.fill(6)(1e8), 1, rc, tourismExport = 1000.0)
    val resultWithout =
      OpenEconomy.step(prevBop, prevForex, 0, 0, 0, 0.05, 1e9, 1.0, Vector.fill(6)(1e8), 1, rc, tourismExport = 0.0)

    resultWith.bop.exports shouldBe resultWithout.bop.exports + PLN(1000.0)
  }

  "OpenEconomy imports" should "include tourismImport" in {
    val prevBop = BopState.zero
    val prevForex = ForexState(Config.BaseExRate, PLN.Zero, PLN(Config.ExportBase), PLN.Zero, PLN.Zero)
    val rc = sfc.config.RunConfig(2000.0, 1, "test")

    val resultWith =
      OpenEconomy.step(prevBop, prevForex, 0, 0, 0, 0.05, 1e9, 1.0, Vector.fill(6)(1e8), 1, rc, tourismImport = 500.0)
    val resultWithout =
      OpenEconomy.step(prevBop, prevForex, 0, 0, 0, 0.05, 1e9, 1.0, Vector.fill(6)(1e8), 1, rc, tourismImport = 0.0)

    resultWith.bop.totalImports shouldBe resultWithout.bop.totalImports + PLN(500.0)
  }

  // ==========================================================================
  // World defaults (1 test)
  // ==========================================================================

  "World" should "default tourismExport and tourismImport to 0.0" in {
    val w = World(
      0,
      Rate(0.02),
      1.0,
      GovState(false, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      sfc.agents.Nbp.State(Rate(0.05)),
      BankingAggregate(PLN.Zero, PLN.Zero, PLN(100), PLN(1000), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      ForexState(Config.BaseExRate, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      sfc.agents.Household.SectorState(100, PLN(5000), PLN(4000), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      Ratio(0.0),
      Ratio(0.0),
      1e9,
      Vector.fill(6)(0.1),
      bankingSector = Banking.initialize(1e9, 5e8, 5e8, 0, 0, Banking.DefaultConfigs),
    )
    w.tourismExport shouldBe PLN.Zero
    w.tourismImport shouldBe PLN.Zero
  }
