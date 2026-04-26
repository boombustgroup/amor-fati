package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import org.scalatest.flatspec.AnyFlatSpec
import com.boombustgroup.amorfati.Generators
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.markets.OpenEconomy
import com.boombustgroup.amorfati.types.*

class DiasporaRemittanceSpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]
  private val baseEr       = decimal(p.forex.baseExRate)

  // ==========================================================================
  // Config defaults
  // ==========================================================================

  "RemittancePerCapita" should "default to 40.0" in {
    decimal(p.remittance.perCapita) shouldBe BigDecimal("40.0")
  }

  "RemittanceErElasticity" should "default to 0.5" in {
    decimal(p.remittance.erElasticity) shouldBe BigDecimal("0.5")
  }

  "RemittanceGrowthRate" should "default to 0.02" in {
    decimal(p.remittance.growthRate) shouldBe BigDecimal("0.02")
  }

  "RemittanceCyclicalSens" should "default to 0.3" in {
    decimal(p.remittance.cyclicalSens) shouldBe BigDecimal("0.3")
  }

  // ==========================================================================
  // Per-capita base
  // ==========================================================================

  "Per-capita base" should "be positive for positive WAP" in {
    val wap  = 1000
    val base = decimal(p.remittance.perCapita) * decimal(wap)
    base should be > BigDecimal("0.0")
  }

  // ==========================================================================
  // ER adjustment
  // ==========================================================================

  "ER adjustment" should "increase inflow when PLN weakens" in {
    val weakerER = baseEr * BigDecimal("1.2") // PLN weaker → higher exchange rate number
    val erAdj    = powDecimal(weakerER / baseEr, decimal(p.remittance.erElasticity))
    erAdj should be > BigDecimal("1.0")
  }

  it should "decrease inflow when PLN strengthens" in {
    val strongerER = baseEr * BigDecimal("0.8")
    val erAdj      = powDecimal(strongerER / baseEr, decimal(p.remittance.erElasticity))
    erAdj should be < BigDecimal("1.0")
  }

  it should "apply partial pass-through (exponent = 0.5)" in {
    // 20% depreciation → sqrt(1.2) ≈ 1.095 (not full 1.2)
    val weakerER = baseEr * BigDecimal("1.2")
    val erAdj    = sqrtDecimal(weakerER / baseEr)
    erAdj should be > BigDecimal("1.0")
    erAdj should be < BigDecimal("1.2")
    erAdj shouldBe sqrtDecimal(BigDecimal("1.2")) +- BigDecimal("1e-10")
  }

  // ==========================================================================
  // Trend adjustment
  // ==========================================================================

  "Trend adjustment" should "equal 1.0 at month 0" in {
    val trendAdj = powDecimal(BigDecimal(1) + decimal(p.remittance.growthRate) / BigDecimal(12), 0)
    trendAdj shouldBe BigDecimal("1.0")
  }

  it should "grow over time" in {
    val trend12 = powDecimal(BigDecimal(1) + decimal(p.remittance.growthRate) / BigDecimal(12), 12)
    trend12 should be > BigDecimal("1.0")
    // ~2% annual growth
    trend12 shouldBe (BigDecimal(1) + decimal(p.remittance.growthRate)) +- BigDecimal("0.001")
  }

  // ==========================================================================
  // Cyclical adjustment
  // ==========================================================================

  "Cyclical adjustment" should "increase with unemployment above 5%" in {
    val highUnemp = BigDecimal("0.10")
    val adj       = BigDecimal(1) + decimal(p.remittance.cyclicalSens) * (highUnemp - BigDecimal("0.05")).max(BigDecimal(0))
    adj should be > BigDecimal("1.0")
  }

  it should "be neutral at unemployment = 5%" in {
    val adj = BigDecimal(1) + decimal(p.remittance.cyclicalSens) * (BigDecimal("0.05") - BigDecimal("0.05")).max(BigDecimal(0))
    adj shouldBe BigDecimal("1.0")
  }

  it should "be neutral at unemployment < 5%" in {
    val lowUnemp = BigDecimal("0.03")
    val adj      = BigDecimal(1) + decimal(p.remittance.cyclicalSens) * (lowUnemp - BigDecimal("0.05")).max(BigDecimal(0))
    adj shouldBe BigDecimal("1.0")
  }

  // ==========================================================================
  // Full formula
  // ==========================================================================

  "Full formula" should "combine all components correctly" in {
    val wap   = 1000
    val month = 12
    val unemp = BigDecimal("0.08")
    val er    = baseEr * BigDecimal("1.1")

    val base        = decimal(p.remittance.perCapita) * decimal(wap)
    val erAdj       = powDecimal(er / baseEr, decimal(p.remittance.erElasticity))
    val trendAdj    = powDecimal(BigDecimal(1) + decimal(p.remittance.growthRate) / BigDecimal(12), month)
    val cyclicalAdj = BigDecimal(1) + decimal(p.remittance.cyclicalSens) * (unemp - BigDecimal("0.05")).max(BigDecimal(0))
    val result      = base * erAdj * trendAdj * cyclicalAdj

    result should be > BigDecimal("0.0")
    // base = 40 * 1000 = 40000
    base shouldBe BigDecimal("40000.0")
    // erAdj > 1 (weaker PLN)
    erAdj should be > BigDecimal("1.0")
    // trendAdj > 1 (12 months)
    trendAdj should be > BigDecimal("1.0")
    // cyclicalAdj > 1 (8% > 5%)
    cyclicalAdj should be > BigDecimal("1.0")
    // Result should be greater than base
    result should be > base
  }

  // ==========================================================================
  // OpenEconomy secondaryIncome
  // ==========================================================================

  "secondaryIncome" should "include diasporaInflow as credit" in {
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
      sectorOutputs = Vector.fill(6)(PLN(100000000)),
      month = ExecutionMonth(1),
      nbpFxReserves = prevBop.reserves,
    )
    val resultWith    = OpenEconomy.step(base.copy(diasporaInflow = PLN(1000)))
    val resultWithout = OpenEconomy.step(base.copy(diasporaInflow = PLN.Zero))

    resultWith.bop.secondaryIncome.shouldBe(resultWithout.bop.secondaryIncome + PLN(1000))
  }

  it should "net outflow and inflow" in {
    val prevBop   = OpenEconomy.BopState.zero
    val prevForex = OpenEconomy.ForexState(p.forex.baseExRate, PLN.Zero, p.openEcon.exportBase, PLN.Zero, PLN.Zero)

    val base   = OpenEconomy.StepInput(
      prevBop = prevBop,
      prevForex = prevForex,
      importCons = PLN.Zero,
      techImports = PLN.Zero,
      autoRatio = Share.Zero,
      domesticRate = Rate.decimal(5, 2),
      gdp = PLN(1000000000),
      priceLevel = PriceIndex.Base,
      sectorOutputs = Vector.fill(6)(PLN(100000000)),
      month = ExecutionMonth(1),
      nbpFxReserves = prevBop.reserves,
    )
    val result = OpenEconomy.step(base.copy(remittanceOutflow = PLN(500), diasporaInflow = PLN(800)))

    // secondaryIncome = euFunds(0) - outflow(500) + inflow(800) = 300
    result.bop.secondaryIncome.shouldBe(PLN(300))
  }

  // ==========================================================================
  // Net remittances
  // ==========================================================================

  "Net remittances" should "be inflow minus outflow" in {
    val inflow  = BigDecimal("1000.0")
    val outflow = BigDecimal("400.0")
    (inflow - outflow).shouldBe(BigDecimal("600.0"))
  }

  // ==========================================================================
  // World defaults
  // ==========================================================================

  "World" should "default diasporaRemittanceInflow to 0.0" in {
    val w = Generators.testWorld(
      currentSigmas = Vector.fill(6)(Sigma.decimal(1, 1)),
      forex = OpenEconomy.ForexState(p.forex.baseExRate, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      marketWage = PLN(5000),
      reservationWage = PLN(4000),
    )
    w.flows.diasporaRemittanceInflow.shouldBe(PLN.Zero)
  }
