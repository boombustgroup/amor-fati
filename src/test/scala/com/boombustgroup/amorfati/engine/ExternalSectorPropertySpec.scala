package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.boombustgroup.amorfati.Generators.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.markets.GvcTrade
import com.boombustgroup.amorfati.random.RandomStream
import com.boombustgroup.amorfati.types.*

class ExternalSectorPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  private val defaultSectorOutputs = Vector.fill(p.sectorDefs.length)(PLN(100000000))

  private def runStep(
      er: BigDecimal = decimal(p.forex.baseExRate),
      price: BigDecimal = BigDecimal("1.0"),
      autoR: BigDecimal = BigDecimal("0.0"),
      month: Int = 30,
  ): GvcTrade.State =
    GvcTrade.step(
      GvcTrade.StepInput(
        GvcTrade.initial,
        defaultSectorOutputs,
        priceIndexBD(price),
        exchangeRateBD(er),
        shareBD(autoR),
        ExecutionMonth(month),
        rng = RandomStream.seeded(42),
      ),
    )

  // --- Exports always non-negative ---

  "GvcTrade.step" should "always have non-negative total exports" in
    forAll(genExchangeRate, genPrice, genFraction, Gen.choose(1, 120)) { (er: BigDecimal, price: BigDecimal, autoR: BigDecimal, month: Int) =>
      val r = runStep(er, price, autoR, month)
      decimal(r.totalExports).should(be >= BigDecimal("0.0"))
    }

  // --- Imports always non-negative ---

  it should "always have non-negative total intermediate imports" in
    forAll(genExchangeRate, genPrice) { (er: BigDecimal, price: BigDecimal) =>
      val r = runStep(er, price)
      decimal(r.totalIntermImports).should(be >= BigDecimal("0.0"))
    }

  // --- Foreign price always >= 1.0 ---

  it should "always have foreign price index >= 1.0" in
    forAll(Gen.choose(1, 120)) { (month: Int) =>
      val r = runStep(month = month)
      decimal(r.foreignPriceIndex).should(be >= BigDecimal("1.0"))
    }

  // --- Disruption in [0, 1] ---

  it should "keep disruption index in [0, 1]" in
    forAll(genExchangeRate, genFraction, Gen.choose(1, 120)) { (er: BigDecimal, autoR: BigDecimal, month: Int) =>
      val r = runStep(er, autoR = autoR, month = month)
      decimal(r.disruptionIndex).should(be >= BigDecimal("0.0"))
      decimal(r.disruptionIndex).should(be <= BigDecimal("1.0"))
    }

  // --- Sector vectors have expected length ---

  it should "always have sector vectors matching sector count" in
    forAll(genExchangeRate, genPrice) { (er: BigDecimal, price: BigDecimal) =>
      val r = runStep(er, price)
      r.sectorExports.length shouldBe p.sectorDefs.length
      r.sectorImports.length shouldBe p.sectorDefs.length
    }

  // --- Trade concentration in (0, 1] ---

  it should "have trade concentration HHI in (0, 1]" in
    forAll(genExchangeRate) { (er: BigDecimal) =>
      val r = runStep(er)
      decimal(r.tradeConcentration).should(be > BigDecimal("0.0"))
      decimal(r.tradeConcentration).should(be <= BigDecimal("1.0"))
    }

  // --- Import cost index >= 1 ---

  it should "have import cost index >= 1.0" in
    forAll(Gen.choose(1, 120)) { (month: Int) =>
      val r = runStep(month = month)
      decimal(r.importCostIndex).should(be >= BigDecimal("1.0"))
    }
