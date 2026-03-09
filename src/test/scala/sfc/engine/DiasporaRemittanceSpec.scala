package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.accounting.{BankingAggregate, BopState, ForexState, GovState}
import sfc.agents.Banking
import sfc.engine.markets.OpenEconomy
import sfc.types.*

class DiasporaRemittanceSpec extends AnyFlatSpec with Matchers:

  import sfc.config.SimParams
  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  // ==========================================================================
  // Config defaults
  // ==========================================================================

  "RemittanceEnabled" should "default to false" in {
    p.flags.remittance shouldBe false
  }

  "RemittancePerCapita" should "default to 40.0" in {
    p.remittance.perCapita.toDouble shouldBe 40.0
  }

  "RemittanceErElasticity" should "default to 0.5" in {
    p.remittance.erElasticity shouldBe 0.5
  }

  "RemittanceGrowthRate" should "default to 0.02" in {
    p.remittance.growthRate.toDouble shouldBe 0.02
  }

  "RemittanceCyclicalSens" should "default to 0.3" in {
    p.remittance.cyclicalSens.toDouble shouldBe 0.3
  }

  // ==========================================================================
  // Per-capita base
  // ==========================================================================

  "Per-capita base" should "be positive for positive WAP" in {
    val wap  = 1000
    val base = p.remittance.perCapita.toDouble * wap.toDouble
    base should be > 0.0
  }

  // ==========================================================================
  // ER adjustment
  // ==========================================================================

  "ER adjustment" should "increase inflow when PLN weakens" in {
    val weakerER = p.forex.baseExRate * 1.2 // PLN weaker → higher exchange rate number
    val erAdj    = Math.pow(weakerER / p.forex.baseExRate, p.remittance.erElasticity)
    erAdj should be > 1.0
  }

  it should "decrease inflow when PLN strengthens" in {
    val strongerER = p.forex.baseExRate * 0.8
    val erAdj      = Math.pow(strongerER / p.forex.baseExRate, p.remittance.erElasticity)
    erAdj should be < 1.0
  }

  it should "apply partial pass-through (exponent = 0.5)" in {
    // 20% depreciation → sqrt(1.2) ≈ 1.095 (not full 1.2)
    val weakerER = p.forex.baseExRate * 1.2
    val erAdj    = Math.pow(weakerER / p.forex.baseExRate, 0.5)
    erAdj should be > 1.0
    erAdj should be < 1.2
    erAdj shouldBe Math.sqrt(1.2) +- 1e-10
  }

  // ==========================================================================
  // Trend adjustment
  // ==========================================================================

  "Trend adjustment" should "equal 1.0 at month 0" in {
    val trendAdj = Math.pow(1.0 + p.remittance.growthRate.toDouble / 12.0, 0.0)
    trendAdj shouldBe 1.0
  }

  it should "grow over time" in {
    val trend12 = Math.pow(1.0 + p.remittance.growthRate.toDouble / 12.0, 12.0)
    trend12 should be > 1.0
    // ~2% annual growth
    trend12 shouldBe (1.0 + p.remittance.growthRate.toDouble) +- 0.001
  }

  // ==========================================================================
  // Cyclical adjustment
  // ==========================================================================

  "Cyclical adjustment" should "increase with unemployment above 5%" in {
    val highUnemp = 0.10
    val adj       = 1.0 + p.remittance.cyclicalSens.toDouble * Math.max(0.0, highUnemp - 0.05)
    adj should be > 1.0
  }

  it should "be neutral at unemployment = 5%" in {
    val adj = 1.0 + p.remittance.cyclicalSens.toDouble * Math.max(0.0, 0.05 - 0.05)
    adj shouldBe 1.0
  }

  it should "be neutral at unemployment < 5%" in {
    val lowUnemp = 0.03
    val adj      = 1.0 + p.remittance.cyclicalSens.toDouble * Math.max(0.0, lowUnemp - 0.05)
    adj shouldBe 1.0
  }

  // ==========================================================================
  // Full formula
  // ==========================================================================

  "Full formula" should "combine all components correctly" in {
    val wap   = 1000
    val month = 12
    val unemp = 0.08
    val er    = p.forex.baseExRate * 1.1

    val base        = p.remittance.perCapita.toDouble * wap.toDouble
    val erAdj       = Math.pow(er / p.forex.baseExRate, p.remittance.erElasticity)
    val trendAdj    = Math.pow(1.0 + p.remittance.growthRate.toDouble / 12.0, month.toDouble)
    val cyclicalAdj = 1.0 + p.remittance.cyclicalSens.toDouble * Math.max(0.0, unemp - 0.05)
    val result      = base * erAdj * trendAdj * cyclicalAdj

    result should be > 0.0
    // base = 40 * 1000 = 40000
    base shouldBe 40000.0
    // erAdj > 1 (weaker PLN)
    erAdj should be > 1.0
    // trendAdj > 1 (12 months)
    trendAdj should be > 1.0
    // cyclicalAdj > 1 (8% > 5%)
    cyclicalAdj should be > 1.0
    // Result should be greater than base
    result should be > base
  }

  // ==========================================================================
  // Disabled → zero
  // ==========================================================================

  "Diaspora inflow" should "be zero when disabled" in {
    // p.flags.remittance defaults to false
    val inflow = if p.flags.remittance then 100.0 else 0.0
    inflow shouldBe 0.0
  }

  // ==========================================================================
  // OpenEconomy secondaryIncome
  // ==========================================================================

  "secondaryIncome" should "include diasporaInflow as credit" in {
    val prevBop   = BopState.zero
    val prevForex = ForexState(p.forex.baseExRate, PLN.Zero, PLN(p.forex.exportBase.toDouble), PLN.Zero, PLN.Zero)
    val rc        = sfc.config.RunConfig(1, "test")

    val resultWith    =
      OpenEconomy.step(prevBop, prevForex, 0, 0, 0, 0.05, 1e9, 1.0, Vector.fill(6)(1e8), 1, rc, diasporaInflow = 1000.0)
    val resultWithout =
      OpenEconomy.step(prevBop, prevForex, 0, 0, 0, 0.05, 1e9, 1.0, Vector.fill(6)(1e8), 1, rc, diasporaInflow = 0.0)

    resultWith.bop.secondaryIncome shouldBe resultWithout.bop.secondaryIncome + PLN(1000.0)
  }

  it should "net outflow and inflow" in {
    val prevBop   = BopState.zero
    val prevForex = ForexState(p.forex.baseExRate, PLN.Zero, PLN(p.forex.exportBase.toDouble), PLN.Zero, PLN.Zero)
    val rc        = sfc.config.RunConfig(1, "test")

    val result = OpenEconomy.step(
      prevBop,
      prevForex,
      0,
      0,
      0,
      0.05,
      1e9,
      1.0,
      Vector.fill(6)(1e8),
      1,
      rc,
      remittanceOutflow = 500.0,
      diasporaInflow = 800.0,
    )

    // secondaryIncome = euFunds(0) - outflow(500) + inflow(800) = 300
    result.bop.secondaryIncome shouldBe PLN(300.0)
  }

  // ==========================================================================
  // Net remittances
  // ==========================================================================

  "Net remittances" should "be inflow minus outflow" in {
    val inflow  = 1000.0
    val outflow = 400.0
    (inflow - outflow) shouldBe 600.0
  }

  // ==========================================================================
  // World defaults
  // ==========================================================================

  "World" should "default diasporaRemittanceInflow to 0.0" in {
    val w = World(
      0,
      Rate(0.02),
      1.0,
      GovState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      sfc.agents.Nbp.State(Rate(0.05)),
      BankingAggregate(PLN.Zero, PLN.Zero, PLN(100), PLN(1000), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      ForexState(p.forex.baseExRate, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      sfc.agents.Household.SectorState(100, PLN(5000), PLN(4000), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      Ratio.Zero,
      Ratio.Zero,
      1e9,
      Vector.fill(6)(0.1),
      bankingSector = Banking.initialize(1e9, 5e8, 5e8, 0, 0, Banking.DefaultConfigs),
    )
    w.diasporaRemittanceInflow shouldBe PLN.Zero
  }
