package sfc.engine

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sfc.Generators.*
import sfc.accounting.{BopState, ForexState}
import sfc.config.{Config, MonetaryRegime, RunConfig}
import sfc.types.*

class OpenEconomyPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  private val plnConfig = RunConfig(2000.0, 1, "test", MonetaryRegime.Pln)
  private val eurConfig = RunConfig(2000.0, 1, "test", MonetaryRegime.Eur)

  private val defaultSectorOutputs = Vector.fill(6)(1e8)

  private def makeForex(er: Double = Config.BaseExRate): ForexState =
    ForexState(er, PLN(1e8), PLN(1e8), PLN.Zero, PLN(1e7))

  private def makeBop(nfa: Double = 0.0, fAssets: Double = 1e9): BopState =
    BopState(
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

  // Combined generator for OE step inputs (avoids >6 forAll limit)
  private val genOeInputs: Gen[(Double, Double, Double, Double, Double, Double, Double, Int)] =
    for
      er <- genExchangeRate
      importC <- Gen.choose(0.0, 1e8)
      techImp <- Gen.choose(0.0, 1e7)
      autoR <- genFraction
      rate <- genRate
      gdp <- Gen.choose(1e6, 1e10)
      price <- genPrice
      month <- Gen.choose(1, 120)
    yield (er, importC, techImp, autoR, rate, gdp, price, month)

  // --- Exchange rate bounds ---

  "OpenEconomy.step" should "keep ER in [OeErFloor, OeErCeiling] for PLN" in {
    forAll(genOeInputs) { (inputs: (Double, Double, Double, Double, Double, Double, Double, Int)) =>
      val (er, importCons, techImp, autoR, rate, gdp, price, month) = inputs
      val r = OpenEconomy.step(
        makeBop(),
        makeForex(er),
        importCons,
        techImp,
        autoR,
        rate,
        gdp,
        price,
        defaultSectorOutputs,
        month,
        plnConfig,
      )
      r.forex.exchangeRate should be >= Config.OeErFloor
      r.forex.exchangeRate should be <= Config.OeErCeiling
    }
  }

  it should "keep ER = BaseExRate for EUR" in {
    forAll(Gen.choose(0.0, 1e8), genFraction, genRate, Gen.choose(1e6, 1e10)) {
      (importCons: Double, autoR: Double, rate: Double, gdp: Double) =>
        val r = OpenEconomy.step(
          makeBop(),
          makeForex(),
          importCons,
          1e6,
          autoR,
          rate,
          gdp,
          1.0,
          defaultSectorOutputs,
          30,
          eurConfig,
        )
        r.forex.exchangeRate shouldBe Config.BaseExRate
    }
  }

  // --- Exports >= 0 ---

  it should "have exports >= 0" in {
    forAll(genExchangeRate, genFraction, Gen.choose(1, 120)) { (er: Double, autoR: Double, month: Int) =>
      val r = OpenEconomy.step(
        makeBop(),
        makeForex(er),
        1e7,
        1e6,
        autoR,
        0.05,
        1e9,
        1.0,
        defaultSectorOutputs,
        month,
        plnConfig,
      )
      r.bop.exports.toDouble should be >= 0.0
    }
  }

  // --- Total imports >= 0 ---

  it should "have total imports >= 0" in {
    forAll(Gen.choose(0.0, 1e8), Gen.choose(0.0, 1e7)) { (importCons: Double, techImp: Double) =>
      val r = OpenEconomy.step(
        makeBop(),
        makeForex(),
        importCons,
        techImp,
        0.1,
        0.05,
        1e9,
        1.0,
        defaultSectorOutputs,
        30,
        plnConfig,
      )
      r.bop.totalImports.toDouble should be >= 0.0
    }
  }

  // --- Trade balance identity ---

  it should "have tradeBalance = exports - totalImports" in {
    forAll(Gen.choose(0.0, 1e8), Gen.choose(0.0, 1e7), genFraction) {
      (importCons: Double, techImp: Double, autoR: Double) =>
        val r = OpenEconomy.step(
          makeBop(),
          makeForex(),
          importCons,
          techImp,
          autoR,
          0.05,
          1e9,
          1.0,
          defaultSectorOutputs,
          30,
          plnConfig,
        )
        r.bop.tradeBalance.toDouble shouldBe ((r.bop.exports - r.bop.totalImports).toDouble +- 1.0)
    }
  }

  // --- CA identity ---

  it should "have CA = tradeBalance + primaryIncome + secondaryIncome" in {
    forAll(Gen.choose(0.0, 1e8), genFraction) { (importCons: Double, autoR: Double) =>
      val r = OpenEconomy.step(
        makeBop(),
        makeForex(),
        importCons,
        1e6,
        autoR,
        0.05,
        1e9,
        1.0,
        defaultSectorOutputs,
        30,
        plnConfig,
      )
      r.bop.currentAccount.toDouble shouldBe
        ((r.bop.tradeBalance + r.bop.primaryIncome + r.bop.secondaryIncome).toDouble +- 1.0)
    }
  }

  // --- BoP identity: CA + KA + ΔReserves ≈ 0 ---

  it should "satisfy BoP identity: CA + KA + deltaReserves approx 0" in {
    forAll(genFraction, genRate) { (autoR: Double, rate: Double) =>
      val prevBop = makeBop()
      val r =
        OpenEconomy.step(prevBop, makeForex(), 1e7, 1e6, autoR, rate, 1e9, 1.0, defaultSectorOutputs, 30, plnConfig)
      val deltaReserves = (r.bop.reserves - prevBop.reserves).toDouble
      val bopSum = r.bop.currentAccount.toDouble + r.bop.capitalAccount.toDouble + deltaReserves
      Math.abs(bopSum) should be < 1.0
    }
  }

  // --- EUR: zero portfolio flows ---

  it should "have zero portfolio flows for EUR" in {
    forAll(genFraction, genRate) { (autoR: Double, rate: Double) =>
      val r =
        OpenEconomy.step(makeBop(), makeForex(), 1e7, 1e6, autoR, rate, 1e9, 1.0, defaultSectorOutputs, 30, eurConfig)
      r.bop.portfolioFlows.toDouble shouldBe 0.0
    }
  }

  // --- Imported intermediates per-sector >= 0 and length = 6 ---

  it should "have per-sector imported intermediates >= 0 and length = 6" in {
    forAll(genExchangeRate, genFraction) { (er: Double, autoR: Double) =>
      val r =
        OpenEconomy.step(makeBop(), makeForex(er), 1e7, 1e6, autoR, 0.05, 1e9, 1.0, defaultSectorOutputs, 30, plnConfig)
      r.importedIntermediates.length shouldBe 6
      for v <- r.importedIntermediates do v should be >= 0.0
    }
  }

  // --- Higher autoRatio → higher exports (ULC effect) ---

  it should "produce higher exports with higher autoRatio" in {
    val r1 =
      OpenEconomy.step(makeBop(), makeForex(), 1e7, 1e6, 0.05, 0.05, 1e9, 1.0, defaultSectorOutputs, 30, plnConfig)
    val r2 =
      OpenEconomy.step(makeBop(), makeForex(), 1e7, 1e6, 0.50, 0.05, 1e9, 1.0, defaultSectorOutputs, 30, plnConfig)
    r2.bop.exports.toDouble should be > r1.bop.exports.toDouble
  }

  // --- ΔNFA = CA + valuationEffect ---

  it should "have deltaNFA = CA + valuationEffect" in {
    forAll(genFraction, genRate, Gen.choose(-1e9, 1e9)) { (autoR: Double, rate: Double, prevNfa: Double) =>
      val prevBop = makeBop(nfa = prevNfa)
      val r =
        OpenEconomy.step(prevBop, makeForex(), 1e7, 1e6, autoR, rate, 1e9, 1.0, defaultSectorOutputs, 30, plnConfig)
      val deltaNfa = r.bop.nfa.toDouble - prevNfa
      deltaNfa shouldBe (r.bop.currentAccount.toDouble + r.valuationEffect +- 1.0)
    }
  }

  // --- EUR: zero valuation effect ---

  it should "have zero valuation effect for EUR" in {
    forAll(genFraction) { (autoR: Double) =>
      val r =
        OpenEconomy.step(makeBop(), makeForex(), 1e7, 1e6, autoR, 0.05, 1e9, 1.0, defaultSectorOutputs, 30, eurConfig)
      r.valuationEffect shouldBe 0.0
    }
  }

  // --- FDI >= 0 ---

  it should "have FDI >= 0" in {
    forAll(genFraction, genRate) { (autoR: Double, rate: Double) =>
      val r =
        OpenEconomy.step(makeBop(), makeForex(), 1e7, 1e6, autoR, rate, 1e9, 1.0, defaultSectorOutputs, 30, plnConfig)
      r.bop.fdi.toDouble should be >= 0.0
    }
  }
