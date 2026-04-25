package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.markets.{CapitalFlows, OpenEconomy}
import com.boombustgroup.amorfati.types.*

class OpenEconomySpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  private val baseForex         = OpenEconomy.ForexState(p.forex.baseExRate, PLN.Zero, p.openEcon.exportBase, PLN.Zero, PLN.Zero)
  private val baseSectorOutputs =
    Vector(BigDecimal("30000.0"), BigDecimal("160000.0"), BigDecimal("450000.0"), BigDecimal("60000.0"), BigDecimal("220000.0"), BigDecimal("80000.0")).map(
      plnBD(_),
    )
  private val gdp               = PLN(1000000000)

  private def baseInput(
      prevBop: OpenEconomy.BopState = OpenEconomy.BopState.zero,
      prevForex: OpenEconomy.ForexState = baseForex,
      autoRatio: BigDecimal = BigDecimal("0.0"),
      month: Int = 30,
  ) = OpenEconomy.StepInput(
    prevBop = prevBop,
    prevForex = prevForex,
    importCons = PLN(10000000),
    techImports = PLN(5000000),
    autoRatio = shareBD(autoRatio),
    domesticRate = p.monetary.initialRate,
    gdp = gdp,
    priceLevel = PriceIndex.Base,
    sectorOutputs = baseSectorOutputs,
    month = ExecutionMonth(month),
    nbpFxReserves = prevBop.reserves,
  )

  // ---- Export tests ----

  "OpenEconomy.step" should "produce positive exports" in {
    val r = OpenEconomy.step(baseInput())
    decimal(r.bop.exports) should be > BigDecimal("0.0")
  }

  it should "use zero elapsed foreign GDP growth in the first execution month" in {
    val r = OpenEconomy.step(baseInput(month = 1))

    decimal(r.bop.exports) shouldBe (decimal(p.openEcon.exportBase) +- BigDecimal("0.01"))
  }

  it should "increase exports with higher automation (ULC effect)" in {
    val r0 = OpenEconomy.step(baseInput(autoRatio = BigDecimal("0.0")))
    val r1 = OpenEconomy.step(baseInput(autoRatio = BigDecimal("0.5")))
    decimal(r1.bop.exports) should be > decimal(r0.bop.exports)
  }

  it should "increase exports with weaker PLN (higher ER)" in {
    val weakPln = baseForex.copy(exchangeRate = ExchangeRate.decimal(55, 1))
    val r0      = OpenEconomy.step(baseInput(prevForex = baseForex))
    val r1      = OpenEconomy.step(baseInput(prevForex = weakPln))
    decimal(r1.bop.exports) should be > decimal(r0.bop.exports)
  }

  // ---- Import tests ----

  it should "produce positive total imports" in {
    val r = OpenEconomy.step(baseInput())
    decimal(r.bop.totalImports) should be > BigDecimal("0.0")
  }

  it should "produce per-sector imported intermediates" in {
    val r = OpenEconomy.step(baseInput())
    r.importedIntermediates.length shouldBe 6
    r.importedIntermediates.forall(_ >= PLN.Zero) shouldBe true
  }

  it should "have manufacturing highest import content" in {
    val r = OpenEconomy.step(baseInput())
    r.importedIntermediates(1) should be > r.importedIntermediates(4)
  }

  // ---- BoP identity ----

  it should "satisfy BoP identity: CA + KA + deltaReserves = 0" in {
    val r      = OpenEconomy.step(baseInput())
    val bopSum = decimal(r.bop.currentAccount) + decimal(r.bop.capitalAccount) +
      decimal(r.bop.reserves - OpenEconomy.BopState.zero.reserves)
    DecimalMath.abs(bopSum) should be < BigDecimal("1.0")
  }

  it should "decompose financial-account channels without changing the net portfolio effect" in {
    val prevBop = OpenEconomy.BopState.zero.copy(carryTradeStock = PLN(20000000))
    val input   = baseInput(prevBop = prevBop, month = 30).copy(
      domesticRate = Rate.decimal(8, 2),
      bondYield = Rate.decimal(9, 2),
      prevBidToCover = Multiplier.decimal(50, 2),
    )

    val r                            = OpenEconomy.step(input)
    val capitalFlight                = CapitalFlows.compute(
      month = input.month,
      yieldSpread = input.bondYield - p.forex.foreignRate,
      bidToCover = input.prevBidToCover,
      prevCarry = CapitalFlows.CarryState(prevBop.carryTradeStock),
      monthlyGdp = input.gdp,
    )
    val expectedCapitalFlightOutflow =
      (-capitalFlight.riskOffOutflow).max(PLN.Zero) + (-capitalFlight.auctionSignal).max(PLN.Zero)
    val legacyAdjustedPortfolio      = r.bop.portfolioFlows + capitalFlight.totalAdjustment
    val decomposedPortfolio          = r.bop.portfolioFlows + r.bop.carryTradeFlow - r.bop.capitalFlightOutflow

    r.bop.carryTradeFlow shouldBe capitalFlight.carryTradeFlow
    r.bop.carryTradeFlow should be > PLN.Zero
    r.bop.capitalFlightOutflow shouldBe expectedCapitalFlightOutflow
    r.bop.capitalFlightOutflow should be > PLN.Zero
    decomposedPortfolio shouldBe legacyAdjustedPortfolio
    r.bop.capitalAccount shouldBe r.bop.fdi + decomposedPortfolio
  }

  // ---- Current account ----

  it should "include EU transfers in current account" in {
    val r = OpenEconomy.step(baseInput().copy(euFundsMonthly = p.openEcon.euTransfers))
    decimal(r.bop.secondaryIncome) shouldBe decimal(p.openEcon.euTransfers) +- BigDecimal("0.01")
  }

  it should "compute trade balance as exports - imports" in {
    val r          = OpenEconomy.step(baseInput())
    val expectedTb = decimal(r.bop.exports - r.bop.totalImports)
    decimal(r.bop.tradeBalance) shouldBe expectedTb +- BigDecimal("0.01")
  }

  // ---- NFA tracking ----

  it should "update NFA from current account" in {
    val r = OpenEconomy.step(baseInput())
    decimal(r.bop.nfa) should not be BigDecimal("0.0")
  }

  it should "accumulate NFA across steps" in {
    val r1 = OpenEconomy.step(baseInput(month = 30))
    val r2 = OpenEconomy.step(baseInput(prevBop = r1.bop, prevForex = r1.forex, month = 31))
    decimal(r2.bop.nfa) should not be decimal(r1.bop.nfa)
  }

  // ---- Exchange rate bounds ----

  it should "keep exchange rate within floor and ceiling" in {
    val r = OpenEconomy.step(baseInput())
    r.forex.exchangeRate should be >= p.openEcon.erFloor
    r.forex.exchangeRate should be <= p.openEcon.erCeiling
  }

  // ---- PPP drift ----

  it should "depreciate PLN when domestic inflation exceeds foreign" in {
    val highInfl = baseInput().copy(inflation = Rate.decimal(10, 2)) // 10% domestic vs 2% foreign
    val lowInfl  = baseInput().copy(inflation = Rate.decimal(2, 2))  // equal to foreign
    val rHigh    = OpenEconomy.step(highInfl)
    val rLow     = OpenEconomy.step(lowInfl)
    // Higher domestic inflation → weaker PLN (higher ER)
    rHigh.forex.exchangeRate should be > rLow.forex.exchangeRate
  }

  it should "appreciate PLN when domestic inflation below foreign" in {
    val lowInfl = baseInput().copy(inflation = Rate(0))            // 0% domestic vs 2% foreign
    val eqInfl  = baseInput().copy(inflation = Rate.decimal(2, 2)) // equal
    val rLow    = OpenEconomy.step(lowInfl)
    val rEq     = OpenEconomy.step(eqInfl)
    // Lower domestic inflation → stronger PLN (lower ER)
    rLow.forex.exchangeRate should be < rEq.forex.exchangeRate
  }

  it should "have no PPP drift when inflation equals foreign" in {
    val eqInfl   = baseInput().copy(inflation = p.gvc.foreignInflation)
    val zeroInfl = baseInput().copy(inflation = Rate.Zero)
    val rEq      = OpenEconomy.step(eqInfl)
    val rZero    = OpenEconomy.step(zeroInfl)
    // With equal inflation, PPP drift is zero — ER difference comes only from other channels
    // Zero inflation case has negative PPP drift (domestic < foreign) → stronger PLN
    rEq.forex.exchangeRate should be > rZero.forex.exchangeRate
  }
