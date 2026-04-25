package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.boombustgroup.amorfati.Generators.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.markets.OpenEconomy
import com.boombustgroup.amorfati.types.*

class OpenEconomyPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  private val defaultSectorOutputs = Vector.fill(6)(PLN(100000000))

  @annotation.nowarn("msg=unused private member") // default used by callers
  private def makeForex(er: BigDecimal = decimal(p.forex.baseExRate)): OpenEconomy.ForexState =
    OpenEconomy.ForexState(exchangeRateBD(er), PLN(100000000), PLN(100000000), PLN.Zero, PLN(10000000))

  private def makeBop(nfa: BigDecimal = BigDecimal("0.0"), fAssets: BigDecimal = BigDecimal("1e9")): OpenEconomy.BopState =
    OpenEconomy.BopState(
      plnBD(nfa),
      plnBD(fAssets),
      PLN(500000000),
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN(100000000),
      PLN(100000000),
      PLN(100000000),
      PLN.Zero,
    )

  private def baseInput(
      prevBop: OpenEconomy.BopState = makeBop(),
      er: BigDecimal = decimal(p.forex.baseExRate),
      importCons: BigDecimal = BigDecimal("1e7"),
      techImp: BigDecimal = BigDecimal("1e6"),
      autoR: BigDecimal = BigDecimal("0.1"),
      rate: BigDecimal = BigDecimal("0.05"),
      gdp: BigDecimal = BigDecimal("1e9"),
      price: BigDecimal = BigDecimal("1.0"),
      month: Int = 30,
  ) = OpenEconomy.StepInput(
    prevBop = prevBop,
    prevForex = makeForex(er),
    importCons = plnBD(importCons),
    techImports = plnBD(techImp),
    autoRatio = shareBD(autoR),
    domesticRate = rateBD(rate),
    gdp = plnBD(gdp),
    priceLevel = priceIndexBD(price),
    sectorOutputs = defaultSectorOutputs,
    month = ExecutionMonth(month),
    nbpFxReserves = prevBop.reserves,
  )

  // Combined generator for OE step inputs (avoids >6 forAll limit)
  private val genOeInputs: Gen[(BigDecimal, BigDecimal, BigDecimal, BigDecimal, BigDecimal, BigDecimal, BigDecimal, Int)] =
    for
      er      <- genExchangeRate
      importC <- genDecimal("0.0", "1e8")
      techImp <- genDecimal("0.0", "1e7")
      autoR   <- genFraction
      rate    <- genRate
      gdp     <- genDecimal("1e6", "1e10")
      price   <- genPrice
      month   <- Gen.choose(1, 120)
    yield (er, importC, techImp, autoR, rate, gdp, price, month)

  // --- Exchange rate bounds ---

  "OpenEconomy.step" should "keep ER in [OeErFloor, OeErCeiling] for PLN" in
    forAll(genOeInputs) { case (er, importCons, techImp, autoR, rate, gdp, price, month) =>
      val r =
        OpenEconomy.step(baseInput(er = er, importCons = importCons, techImp = techImp, autoR = autoR, rate = rate, gdp = gdp, price = price, month = month))
      r.forex.exchangeRate should be >= p.openEcon.erFloor
      r.forex.exchangeRate should be <= p.openEcon.erCeiling
    }

  // --- Exports >= 0 ---

  it should "have exports >= 0" in
    forAll(genExchangeRate, genFraction, Gen.choose(1, 120)) { (er: BigDecimal, autoR: BigDecimal, month: Int) =>
      val r = OpenEconomy.step(baseInput(er = er, autoR = autoR, month = month))
      decimal(r.bop.exports) should be >= BigDecimal("0.0")
    }

  // --- Total imports >= 0 ---

  it should "have total imports >= 0" in
    forAll(genDecimal("0.0", "1e8"), genDecimal("0.0", "1e7")) { (importCons: BigDecimal, techImp: BigDecimal) =>
      val r = OpenEconomy.step(baseInput(importCons = importCons, techImp = techImp))
      decimal(r.bop.totalImports) should be >= BigDecimal("0.0")
    }

  // --- Trade balance identity ---

  it should "have tradeBalance = exports - totalImports" in
    forAll(genDecimal("0.0", "1e8"), genDecimal("0.0", "1e7"), genFraction) { (importCons: BigDecimal, techImp: BigDecimal, autoR: BigDecimal) =>
      val r = OpenEconomy.step(baseInput(importCons = importCons, techImp = techImp, autoR = autoR))
      decimal(r.bop.tradeBalance) shouldBe (decimal(r.bop.exports - r.bop.totalImports) +- BigDecimal("1.0"))
    }

  // --- CA identity ---

  it should "have CA = tradeBalance + primaryIncome + secondaryIncome" in
    forAll(genDecimal("0.0", "1e8"), genFraction) { (importCons: BigDecimal, autoR: BigDecimal) =>
      val r = OpenEconomy.step(baseInput(importCons = importCons, autoR = autoR))
      decimal(r.bop.currentAccount) shouldBe
        (decimal(r.bop.tradeBalance + r.bop.primaryIncome + r.bop.secondaryIncome) +- BigDecimal("1.0"))
    }

  // --- BoP identity: CA + KA + ΔReserves ≈ 0 ---

  it should "satisfy BoP identity: CA + KA + deltaReserves approx 0" in
    forAll(genFraction, genRate) { (autoR: BigDecimal, rate: BigDecimal) =>
      val prevBop       = makeBop()
      val r             = OpenEconomy.step(baseInput(prevBop = prevBop, autoR = autoR, rate = rate))
      val deltaReserves = decimal(r.bop.reserves - prevBop.reserves)
      val bopSum        = decimal(r.bop.currentAccount) + decimal(r.bop.capitalAccount) + deltaReserves
      DecimalMath.abs(bopSum) should be < BigDecimal("1.0")
    }

  // --- Imported intermediates per-sector >= 0 and length = 6 ---

  it should "have per-sector imported intermediates >= 0 and length = 6" in
    forAll(genExchangeRate, genFraction) { (er: BigDecimal, autoR: BigDecimal) =>
      val r = OpenEconomy.step(baseInput(er = er, autoR = autoR))
      r.importedIntermediates.length shouldBe 6
      for v <- r.importedIntermediates do decimal(v) should be >= BigDecimal("0.0")
    }

  // --- Higher autoRatio → higher exports (ULC effect) ---

  it should "produce higher exports with higher autoRatio" in {
    val r1 = OpenEconomy.step(baseInput(autoR = BigDecimal("0.05")))
    val r2 = OpenEconomy.step(baseInput(autoR = BigDecimal("0.50")))
    decimal(r2.bop.exports) should be > decimal(r1.bop.exports)
  }

  // --- ΔNFA = CA + valuationEffect ---

  it should "have deltaNFA = CA + valuationEffect" in
    forAll(genFraction, genRate, genDecimal("-1e9", "1e9")) { (autoR: BigDecimal, rate: BigDecimal, prevNfa: BigDecimal) =>
      val prevBop  = makeBop(nfa = prevNfa)
      val r        = OpenEconomy.step(baseInput(prevBop = prevBop, autoR = autoR, rate = rate))
      val deltaNfa = decimal(r.bop.nfa) - prevNfa
      deltaNfa shouldBe (decimal(r.bop.currentAccount) + decimal(r.valuationEffect) +- BigDecimal("1.0"))
    }

  // --- FDI >= 0 ---

  it should "have FDI >= 0" in
    forAll(genFraction, genRate) { (autoR: BigDecimal, rate: BigDecimal) =>
      val r = OpenEconomy.step(baseInput(autoR = autoR, rate = rate))
      decimal(r.bop.fdi) should be >= BigDecimal("0.0")
    }
