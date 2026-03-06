package sfc.agents

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sfc.config.Config

class FxInterventionPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  private val genER = Gen.choose(2.5, 10.0)
  private val genReserves = Gen.choose(0.0, 1e11)
  private val genGdp = Gen.choose(1e6, 1e12)

  // Helper: call with enabled=true
  private def fxEnabled(er: Double, reserves: Double, gdp: Double) =
    Nbp.fxIntervention(er, reserves, gdp, enabled = true)

  "Nbp.fxIntervention (enabled)" should "never produce negative reserves" in {
    forAll(genER, genReserves, genGdp) { (er, reserves, gdp) =>
      val result = fxEnabled(er, reserves, gdp)
      result.newReserves should be >= 0.0
    }
  }

  it should "bound eurTraded by reserves" in {
    forAll(genER, genReserves, genGdp) { (er, reserves, gdp) =>
      val result = fxEnabled(er, reserves, gdp)
      // When selling EUR (eurTraded < 0), magnitude <= reserves
      // When buying EUR (eurTraded > 0), magnitude <= reserves * maxMonthly
      Math.abs(result.eurTraded) should be <= (reserves + 1e-6)
    }
  }

  it should "have erEffect opposing deviation when outside band" in {
    forAll(genER, genReserves, genGdp) { (er, reserves, gdp) =>
      val result = fxEnabled(er, reserves, gdp)
      val erDev = (er - Config.BaseExRate) / Config.BaseExRate
      if Math.abs(erDev) > Config.NbpFxBand && reserves > 0 && gdp > 0 then
        // erEffect should oppose the deviation
        if erDev > 0 then result.erEffect should be <= 0.0
        else result.erEffect should be >= 0.0
    }
  }

  "Nbp.fxIntervention (disabled)" should "return zero effect" in {
    // Config.NbpFxIntervention defaults to false
    forAll(genER, genReserves, genGdp) { (er, reserves, gdp) =>
      val result = Nbp.fxIntervention(er, reserves, gdp)
      result.erEffect shouldBe 0.0
      result.eurTraded shouldBe 0.0
      result.newReserves shouldBe reserves
    }
  }

  "Nbp.fxIntervention (enabled)" should "return zero effect when ER within band" in {
    // Generate ER strictly inside band (0.5% margin avoids FP boundary issues)
    val genERInBand = Gen.choose(
      Config.BaseExRate * (1.0 - Config.NbpFxBand + 0.005),
      Config.BaseExRate * (1.0 + Config.NbpFxBand - 0.005),
    )
    forAll(genERInBand, genReserves, genGdp) { (er, reserves, gdp) =>
      val result = fxEnabled(er, reserves, gdp)
      result.erEffect shouldBe 0.0
      result.eurTraded shouldBe 0.0
    }
  }

  it should "conserve reserves: |newReserves - oldReserves| = |eurTraded|" in {
    forAll(genER, genReserves, genGdp) { (er, reserves, gdp) =>
      val result = fxEnabled(er, reserves, gdp)
      // newReserves = max(0, reserves + eurTraded)
      // When reserves + eurTraded >= 0: |newReserves - reserves| = |eurTraded|
      // Tolerance 1.0 for large magnitudes (~1e10), consistent with SFC check
      if result.newReserves > 0 then
        Math.abs(result.newReserves - reserves) shouldBe (Math.abs(result.eurTraded) +- 1.0)
    }
  }
