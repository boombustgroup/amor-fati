package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen
import sfc.testutil.Generators.*
import sfc.config.{Config, MonetaryRegime, RunConfig}
import sfc.types.*

class ExternalSectorPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  private val plnConfig = RunConfig(2000.0, 1, "test", MonetaryRegime.Pln)
  private val eurConfig = RunConfig(2000.0, 1, "test", MonetaryRegime.Eur)
  private val defaultSectorOutputs = Vector.fill(6)(1e8)

  private def runStep(er: Double = Config.BaseExRate, price: Double = 1.0,
                      autoR: Double = 0.0, month: Int = 30,
                      rc: RunConfig = plnConfig): GvcTrade.State =
    GvcTrade.step(GvcTrade.initial, defaultSectorOutputs, price, er, autoR, month, rc)

  // --- Exports always non-negative ---

  "GvcTrade.step" should "always have non-negative total exports" in {
    forAll(genExchangeRate, genPrice, genFraction, Gen.choose(1, 120)) {
      (er: Double, price: Double, autoR: Double, month: Int) =>
        val r = runStep(er, price, autoR, month)
        r.totalExports.toDouble should be >= 0.0
    }
  }

  // --- Imports always non-negative ---

  it should "always have non-negative total intermediate imports" in {
    forAll(genExchangeRate, genPrice) { (er: Double, price: Double) =>
      val r = runStep(er, price)
      r.totalIntermImports.toDouble should be >= 0.0
    }
  }

  // --- Foreign price always >= 1.0 ---

  it should "always have foreign price index >= 1.0" in {
    forAll(Gen.choose(1, 120)) { (month: Int) =>
      val r = runStep(month = month)
      r.foreignPriceIndex should be >= 1.0
    }
  }

  // --- Disruption in [0, 1] ---

  it should "keep disruption index in [0, 1]" in {
    forAll(genExchangeRate, genFraction, Gen.choose(1, 120)) {
      (er: Double, autoR: Double, month: Int) =>
        val r = runStep(er, autoR = autoR, month = month)
        r.disruptionIndex.toDouble should be >= 0.0
        r.disruptionIndex.toDouble should be <= 1.0
    }
  }

  // --- Sector vectors have length 6 ---

  it should "always have sector vectors of length 6" in {
    forAll(genExchangeRate, genPrice) { (er: Double, price: Double) =>
      val r = runStep(er, price)
      r.sectorExports.length shouldBe 6
      r.sectorImports.length shouldBe 6
    }
  }

  // --- Trade concentration in (0, 1] ---

  it should "have trade concentration HHI in (0, 1]" in {
    forAll(genExchangeRate) { (er: Double) =>
      val r = runStep(er)
      r.tradeConcentration.toDouble should be > 0.0
      r.tradeConcentration.toDouble should be <= 1.0
    }
  }

  // --- EUR regime: exports still positive ---

  it should "produce positive exports under EUR regime" in {
    forAll(genFraction, Gen.choose(1, 120)) { (autoR: Double, month: Int) =>
      val r = runStep(Config.BaseExRate, 1.0, autoR, month, eurConfig)
      r.totalExports.toDouble should be > 0.0
    }
  }

  // --- Import cost index >= 1 ---

  it should "have import cost index >= 1.0" in {
    forAll(Gen.choose(1, 120)) { (month: Int) =>
      val r = runStep(month = month)
      r.importCostIndex should be >= 1.0
    }
  }
