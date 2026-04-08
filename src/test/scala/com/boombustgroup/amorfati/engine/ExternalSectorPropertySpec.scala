package com.boombustgroup.amorfati.engine

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.boombustgroup.amorfati.Generators.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.markets.GvcTrade
import com.boombustgroup.amorfati.types.*

class ExternalSectorPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]
  private val td           = ComputationBoundary

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  private val defaultSectorOutputs = Vector.fill(6)(1e8)

  private def runStep(
      er: Double = p.forex.baseExRate,
      price: Double = 1.0,
      autoR: Double = 0.0,
      month: Int = 30,
  ): GvcTrade.State =
    GvcTrade.step(GvcTrade.StepInput(GvcTrade.initial, defaultSectorOutputs, PriceIndex(price), er, autoR, month, rng = new scala.util.Random(42)))

  // --- Exports always non-negative ---

  "GvcTrade.step" should "always have non-negative total exports" in
    forAll(genExchangeRate, genPrice, genFraction, Gen.choose(1, 120)) { (er: Double, price: Double, autoR: Double, month: Int) =>
      val r = runStep(er, price, autoR, month)
      td.toDouble(r.totalExports) should be >= 0.0
    }

  // --- Imports always non-negative ---

  it should "always have non-negative total intermediate imports" in
    forAll(genExchangeRate, genPrice) { (er: Double, price: Double) =>
      val r = runStep(er, price)
      td.toDouble(r.totalIntermImports) should be >= 0.0
    }

  // --- Foreign price always >= 1.0 ---

  it should "always have foreign price index >= 1.0" in
    forAll(Gen.choose(1, 120)) { (month: Int) =>
      val r = runStep(month = month)
      td.toDouble(r.foreignPriceIndex) should be >= 1.0
    }

  // --- Disruption in [0, 1] ---

  it should "keep disruption index in [0, 1]" in
    forAll(genExchangeRate, genFraction, Gen.choose(1, 120)) { (er: Double, autoR: Double, month: Int) =>
      val r = runStep(er, autoR = autoR, month = month)
      td.toDouble(r.disruptionIndex) should be >= 0.0
      td.toDouble(r.disruptionIndex) should be <= 1.0
    }

  // --- Sector vectors have length 6 ---

  it should "always have sector vectors of length 6" in
    forAll(genExchangeRate, genPrice) { (er: Double, price: Double) =>
      val r = runStep(er, price)
      r.sectorExports.length shouldBe 6
      r.sectorImports.length shouldBe 6
    }

  // --- Trade concentration in (0, 1] ---

  it should "have trade concentration HHI in (0, 1]" in
    forAll(genExchangeRate) { (er: Double) =>
      val r = runStep(er)
      td.toDouble(r.tradeConcentration) should be > 0.0
      td.toDouble(r.tradeConcentration) should be <= 1.0
    }

  // --- Import cost index >= 1 ---

  it should "have import cost index >= 1.0" in
    forAll(Gen.choose(1, 120)) { (month: Int) =>
      val r = runStep(month = month)
      td.toDouble(r.importCostIndex) should be >= 1.0
    }
