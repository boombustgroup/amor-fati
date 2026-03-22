package com.boombustgroup.amorfati.engine

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.boombustgroup.amorfati.Generators.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.markets.OpenEconomy
import com.boombustgroup.amorfati.types.*

class OpenEconomyPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  private val defaultSectorOutputs = Vector.fill(6)(PLN(1e8))

  @annotation.nowarn("msg=unused private member") // default used by callers
  private def makeForex(er: Double = p.forex.baseExRate): OpenEconomy.ForexState =
    OpenEconomy.ForexState(er, PLN(1e8), PLN(1e8), PLN.Zero, PLN(1e7))

  private def makeBop(nfa: Double = 0.0, fAssets: Double = 1e9): OpenEconomy.BopState =
    OpenEconomy.BopState(
      PLN(nfa),
      PLN(fAssets),
      PLN(5e8),
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN(1e8),
      PLN(1e8),
      PLN(1e8),
      PLN.Zero,
    )

  private def baseInput(
      prevBop: OpenEconomy.BopState = makeBop(),
      er: Double = p.forex.baseExRate,
      importCons: Double = 1e7,
      techImp: Double = 1e6,
      autoR: Double = 0.1,
      rate: Double = 0.05,
      gdp: Double = 1e9,
      price: Double = 1.0,
      month: Int = 30,
  ) = OpenEconomy.StepInput(
    prevBop = prevBop,
    prevForex = makeForex(er),
    importCons = PLN(importCons),
    techImports = PLN(techImp),
    autoRatio = Share(autoR),
    domesticRate = Rate(rate),
    gdp = PLN(gdp),
    priceLevel = price,
    sectorOutputs = defaultSectorOutputs,
    month = month,
  )

  // Combined generator for OE step inputs (avoids >6 forAll limit)
  private val genOeInputs: Gen[(Double, Double, Double, Double, Double, Double, Double, Int)] =
    for
      er      <- genExchangeRate
      importC <- Gen.choose(0.0, 1e8)
      techImp <- Gen.choose(0.0, 1e7)
      autoR   <- genFraction
      rate    <- genRate
      gdp     <- Gen.choose(1e6, 1e10)
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
    forAll(genExchangeRate, genFraction, Gen.choose(1, 120)) { (er: Double, autoR: Double, month: Int) =>
      val r = OpenEconomy.step(baseInput(er = er, autoR = autoR, month = month))
      r.bop.exports.toDouble should be >= 0.0
    }

  // --- Total imports >= 0 ---

  it should "have total imports >= 0" in
    forAll(Gen.choose(0.0, 1e8), Gen.choose(0.0, 1e7)) { (importCons: Double, techImp: Double) =>
      val r = OpenEconomy.step(baseInput(importCons = importCons, techImp = techImp))
      r.bop.totalImports.toDouble should be >= 0.0
    }

  // --- Trade balance identity ---

  it should "have tradeBalance = exports - totalImports" in
    forAll(Gen.choose(0.0, 1e8), Gen.choose(0.0, 1e7), genFraction) { (importCons: Double, techImp: Double, autoR: Double) =>
      val r = OpenEconomy.step(baseInput(importCons = importCons, techImp = techImp, autoR = autoR))
      r.bop.tradeBalance.toDouble shouldBe ((r.bop.exports - r.bop.totalImports).toDouble +- 1.0)
    }

  // --- CA identity ---

  it should "have CA = tradeBalance + primaryIncome + secondaryIncome" in
    forAll(Gen.choose(0.0, 1e8), genFraction) { (importCons: Double, autoR: Double) =>
      val r = OpenEconomy.step(baseInput(importCons = importCons, autoR = autoR))
      r.bop.currentAccount.toDouble shouldBe
        ((r.bop.tradeBalance + r.bop.primaryIncome + r.bop.secondaryIncome).toDouble +- 1.0)
    }

  // --- BoP identity: CA + KA + ΔReserves ≈ 0 ---

  it should "satisfy BoP identity: CA + KA + deltaReserves approx 0" in
    forAll(genFraction, genRate) { (autoR: Double, rate: Double) =>
      val prevBop       = makeBop()
      val r             = OpenEconomy.step(baseInput(prevBop = prevBop, autoR = autoR, rate = rate))
      val deltaReserves = (r.bop.reserves - prevBop.reserves).toDouble
      val bopSum        = r.bop.currentAccount.toDouble + r.bop.capitalAccount.toDouble + deltaReserves
      Math.abs(bopSum) should be < 1.0
    }

  // --- Imported intermediates per-sector >= 0 and length = 6 ---

  it should "have per-sector imported intermediates >= 0 and length = 6" in
    forAll(genExchangeRate, genFraction) { (er: Double, autoR: Double) =>
      val r = OpenEconomy.step(baseInput(er = er, autoR = autoR))
      r.importedIntermediates.length shouldBe 6
      for v <- r.importedIntermediates do v.toDouble should be >= 0.0
    }

  // --- Higher autoRatio → higher exports (ULC effect) ---

  it should "produce higher exports with higher autoRatio" in {
    val r1 = OpenEconomy.step(baseInput(autoR = 0.05))
    val r2 = OpenEconomy.step(baseInput(autoR = 0.50))
    r2.bop.exports.toDouble should be > r1.bop.exports.toDouble
  }

  // --- ΔNFA = CA + valuationEffect ---

  it should "have deltaNFA = CA + valuationEffect" in
    forAll(genFraction, genRate, Gen.choose(-1e9, 1e9)) { (autoR: Double, rate: Double, prevNfa: Double) =>
      val prevBop  = makeBop(nfa = prevNfa)
      val r        = OpenEconomy.step(baseInput(prevBop = prevBop, autoR = autoR, rate = rate))
      val deltaNfa = r.bop.nfa.toDouble - prevNfa
      deltaNfa shouldBe (r.bop.currentAccount.toDouble + r.valuationEffect.toDouble +- 1.0)
    }

  // --- FDI >= 0 ---

  it should "have FDI >= 0" in
    forAll(genFraction, genRate) { (autoR: Double, rate: Double) =>
      val r = OpenEconomy.step(baseInput(autoR = autoR, rate = rate))
      r.bop.fdi.toDouble should be >= 0.0
    }
