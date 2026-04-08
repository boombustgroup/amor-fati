package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.fp.FixedPointBase.ScaleD
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.boombustgroup.amorfati.types.*

class FxInterventionPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams                          = SimParams.defaults
  private val p: SimParams                 = summon[SimParams]
  private val td                           = ComputationBoundary
  private val baseEr                       = td.toDouble(p.forex.baseExRate)
  private def plnValue(x: PLN): Double     = x.toLong.toDouble / ScaleD
  private def shareValue(x: Share): Double = x.toLong.toDouble / ScaleD

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  private val genER       = Gen.choose(2.5, 10.0)
  private val genReserves = Gen.choose(0.0, 1e11)
  private val genGdp      = Gen.choose(1e6, 1e12)

  // Helper: call with enabled=true
  private def fxEnabled(er: Double, reserves: Double, gdp: Double) =
    Nbp.fxIntervention(ExchangeRate(er), PLN(reserves), PLN(gdp), enabled = true)

  "Nbp.fxIntervention (enabled)" should "never produce negative reserves" in
    forAll(genER, genReserves, genGdp) { (er, reserves, gdp) =>
      val result = fxEnabled(er, reserves, gdp)
      result.newReserves.should(be >= PLN.Zero)
    }

  it should "bound eurTraded by reserves" in
    forAll(genER, genReserves, genGdp) { (er, reserves, gdp) =>
      val result = fxEnabled(er, reserves, gdp)
      // When selling EUR (eurTraded < 0), magnitude <= reserves
      // When buying EUR (eurTraded > 0), magnitude <= reserves * maxMonthly
      result.eurTraded.abs.should(be <= PLN(reserves + 1e-6))
    }

  it should "have erShock opposing deviation when outside band" in
    forAll(genER, genReserves, genGdp) { (er, reserves, gdp) =>
      val result = fxEnabled(er, reserves, gdp)
      val erDev  = ExchangeRate(er).deviationFrom(p.forex.baseExRate)
      if erDev.abs.toScalar > p.monetary.fxBand.toScalar && result.erShock != ExchangeRateShock.Zero then result.erShock.sign shouldBe (-erDev.sign)
    }

  "Nbp.fxIntervention (enabled)" should "return zero effect when ER within band" in {
    // Generate ER strictly inside band (0.5% margin avoids FP boundary issues)
    val genERInBand = Gen.choose(
      baseEr * (1.0 - shareValue(p.monetary.fxBand) + 0.005),
      baseEr * (1.0 + shareValue(p.monetary.fxBand) - 0.005),
    )
    forAll(genERInBand, genReserves, genGdp) { (er, reserves, gdp) =>
      val result = fxEnabled(er, reserves, gdp)
      result.erShock.shouldBe(ExchangeRateShock.Zero)
      result.eurTraded.shouldBe(PLN.Zero)
    }
  }

  it should "conserve reserves: |newReserves - oldReserves| = |eurTraded|" in
    forAll(genER, genReserves, genGdp) { (er, reserves, gdp) =>
      val result = fxEnabled(er, reserves, gdp)
      // newReserves = max(0, reserves + eurTraded)
      // When reserves + eurTraded >= 0: |newReserves - reserves| = |eurTraded|
      // Tolerance 1.0 for large magnitudes (~1e10), consistent with SFC check
      if result.newReserves > PLN.Zero then Math.abs(plnValue(result.newReserves) - reserves).shouldBe(plnValue(result.eurTraded.abs) +- 1.0)
    }
