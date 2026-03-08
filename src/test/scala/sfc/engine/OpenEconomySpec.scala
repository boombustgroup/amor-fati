package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.accounting.{BopState, ForexState}
import sfc.config.{Config, RunConfig}
import sfc.types.*

class OpenEconomySpec extends AnyFlatSpec with Matchers:

  private val rc = RunConfig(2000.0, 1, "test")

  private val baseForex = ForexState(Config.BaseExRate, PLN.Zero, PLN(Config.ExportBase), PLN.Zero, PLN.Zero)
  private val baseSectorOutputs = Vector(30000.0, 160000.0, 450000.0, 60000.0, 220000.0, 80000.0)
  private val gdp = 1e9

  private def runStep(
    rc: RunConfig = rc,
    prevBop: BopState = BopState.zero,
    prevForex: ForexState = baseForex,
    autoRatio: Double = 0.0,
    month: Int = 30,
  ): OpenEconomy.Result =
    OpenEconomy.step(
      prevBop,
      prevForex,
      1e7,
      5e6,
      autoRatio,
      Config.NbpInitialRate,
      gdp,
      1.0,
      baseSectorOutputs,
      month,
      rc,
    )

  // ---- Export tests ----

  "OpenEconomy.step" should "produce positive exports" in {
    val r = runStep()
    r.bop.exports.toDouble should be > 0.0
  }

  it should "increase exports with higher automation (ULC effect)" in {
    val r0 = runStep(autoRatio = 0.0)
    val r1 = runStep(autoRatio = 0.5)
    r1.bop.exports.toDouble should be > r0.bop.exports.toDouble
  }

  it should "increase exports with weaker PLN (higher ER)" in {
    val weakPln = baseForex.copy(exchangeRate = 5.5)
    val r0 = runStep(prevForex = baseForex)
    val r1 = runStep(prevForex = weakPln)
    r1.bop.exports.toDouble should be > r0.bop.exports.toDouble
  }

  // ---- Import tests ----

  it should "produce positive total imports" in {
    val r = runStep()
    r.bop.totalImports.toDouble should be > 0.0
  }

  it should "produce per-sector imported intermediates" in {
    val r = runStep()
    r.importedIntermediates.length shouldBe 6
    r.importedIntermediates.forall(_ >= 0.0) shouldBe true
  }

  it should "have manufacturing highest import content" in {
    val r = runStep()
    // Manufacturing has import content 0.50, highest among sectors
    r.importedIntermediates(1) should be > r.importedIntermediates(4) // Mfg > Public
  }

  // ---- BoP identity ----

  it should "satisfy BoP identity: CA + KA + deltaReserves = 0" in {
    val r = runStep()
    val bopSum = r.bop.currentAccount.toDouble + r.bop.capitalAccount.toDouble +
      (r.bop.reserves - BopState.zero.reserves).toDouble
    Math.abs(bopSum) should be < 1.0
  }

  // ---- Current account ----

  it should "include EU transfers in current account" in {
    val r = OpenEconomy.step(
      BopState.zero,
      baseForex,
      1e7,
      5e6,
      0.0,
      Config.NbpInitialRate,
      gdp,
      1.0,
      baseSectorOutputs,
      30,
      rc,
      euFundsMonthly = Config.OeEuTransfers,
    )
    r.bop.secondaryIncome.toDouble shouldBe Config.OeEuTransfers +- 0.01
  }

  it should "compute trade balance as exports - imports" in {
    val r = runStep()
    val expectedTb = (r.bop.exports - r.bop.totalImports).toDouble
    r.bop.tradeBalance.toDouble shouldBe expectedTb +- 0.01
  }

  // ---- NFA tracking ----

  it should "update NFA from current account" in {
    val r = runStep()
    // NFA should change from zero by approximately currentAccount + valuation
    r.bop.nfa.toDouble should not be 0.0
  }

  it should "accumulate NFA across steps" in {
    val r1 = runStep(month = 30)
    val r2 = runStep(prevBop = r1.bop, prevForex = r1.forex, month = 31)
    // NFA should differ from first step's NFA
    r2.bop.nfa.toDouble should not be r1.bop.nfa.toDouble
  }

  // ---- Exchange rate bounds ----

  it should "keep exchange rate within floor and ceiling" in {
    val r = runStep()
    r.forex.exchangeRate should be >= Config.OeErFloor
    r.forex.exchangeRate should be <= Config.OeErCeiling
  }
