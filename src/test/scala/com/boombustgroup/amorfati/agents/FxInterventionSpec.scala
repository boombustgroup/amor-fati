package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.fp.FixedPointBase.ScaleD
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.types.*

class FxInterventionSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams                                      = SimParams.defaults
  private val p: SimParams                             = summon[SimParams]
  private val td                                       = ComputationBoundary
  private val baseEr                                   = td.toDouble(p.forex.baseExRate)
  private def plnValue(x: PLN): Double                 = x.toLong.toDouble / ScaleD
  private def shockValue(x: ExchangeRateShock): Double = x.toLong.toDouble / ScaleD

  // Helper: call fxIntervention with enabled=true for tests that need active intervention
  private def fxEnabled(er: Double, reserves: Double, gdp: Double) =
    Nbp.fxIntervention(ExchangeRate(er), PLN(reserves), PLN(gdp), enabled = true)

  // --- fxIntervention ---

  "Nbp.fxIntervention" should "return zero effect when ER within band" in {
    // ER deviation = 5% < default band of 10%
    val er     = baseEr * 1.05
    val result = fxEnabled(er, 1e10, 1e9)
    result.erShock.shouldBe(ExchangeRateShock.Zero)
    result.eurTraded.shouldBe(PLN.Zero)
  }

  it should "return zero effect when ER just inside band boundary" in {
    // Use 9.9% deviation (just inside default 10% band) to avoid FP edge case
    val er     = baseEr * 1.099
    val result = fxEnabled(er, 1e10, 1e9)
    result.erShock.shouldBe(ExchangeRateShock.Zero)
    result.eurTraded.shouldBe(PLN.Zero)
  }

  it should "intervene when PLN depreciates beyond band (sell EUR)" in {
    // PLN depreciates: ER > baseER * 1.10 -> NBP sells EUR to strengthen PLN
    // erDev > 0 -> direction = -1 -> sells EUR (eurTraded < 0)
    val er       = baseEr * 1.20 // 20% depreciation
    val reserves = 1e10
    val result   = fxEnabled(er, reserves, 1e9)
    result.eurTraded.should(be < PLN.Zero) // sold EUR
    shockValue(result.erShock).should(be < 0.0) // dampens upward ER deviation
    result.newReserves.should(be < PLN(reserves))
  }

  it should "intervene when PLN appreciates beyond band (buy EUR)" in {
    // PLN appreciates: ER < baseER * 0.90 -> NBP buys EUR to weaken PLN
    // erDev < 0 -> direction = +1 -> buys EUR (eurTraded > 0)
    val er       = baseEr * 0.80 // 20% appreciation
    val reserves = 1e10
    val result   = fxEnabled(er, reserves, 1e9)
    result.eurTraded.should(be > PLN.Zero) // bought EUR
    shockValue(result.erShock).should(be > 0.0) // dampens downward ER deviation
    result.newReserves.should(be > PLN(reserves))
  }

  it should "not sell more EUR than available reserves" in {
    val er       = baseEr * 1.50 // massive depreciation
    val reserves = 100.0         // tiny reserves
    val result   = fxEnabled(er, reserves, 1e9)
    result.newReserves.should(be >= PLN.Zero)
    result.eurTraded.abs.should(be <= PLN(reserves))
  }

  it should "produce erShock opposing the deviation direction" in {
    // Depreciation -> negative erShock (pushes ER down)
    val erHigh     = baseEr * 1.30
    val resultHigh = fxEnabled(erHigh, 1e10, 1e9)
    shockValue(resultHigh.erShock).should(be < 0.0)

    // Appreciation -> positive erShock (pushes ER up)
    val erLow     = baseEr * 0.70
    val resultLow = fxEnabled(erLow, 1e10, 1e9)
    shockValue(resultLow.erShock).should(be > 0.0)
  }

  it should "produce reserves consistent with eurTraded" in {
    val er       = baseEr * 1.25
    val reserves = 1e10
    val result   = fxEnabled(er, reserves, 1e9)
    // newReserves = max(0, reserves + eurTraded)
    plnValue(result.newReserves).shouldBe(Math.max(0.0, reserves + plnValue(result.eurTraded)) +- 1e-6)
  }

  it should "produce zero erShock when gdp is zero (no div-by-zero)" in {
    val er     = baseEr * 1.25
    val result = fxEnabled(er, 1e10, 0.0)
    result.erShock.shouldBe(ExchangeRateShock.Zero)
    // But intervention still occurs (reserves change)
    result.eurTraded.should(be < PLN.Zero)
  }

  it should "produce no intervention when ER equals baseER (Eurozone scenario)" in {
    // In EUR regime, ER = baseER -> erDev = 0 -> within any band
    val result = fxEnabled(baseEr, 1e10, 1e9)
    result.erShock.shouldBe(ExchangeRateShock.Zero)
    result.eurTraded.shouldBe(PLN.Zero)
  }

  // --- FxInterventionResult ---

  "FxInterventionResult" should "be constructable with all fields" in {
    val r = Nbp.FxInterventionResult(ExchangeRateShock(0.01), PLN(-5e8), PLN(9.5e9), PLN(-5e8 * 4.33))
    shockValue(r.erShock).should(be(0.01))
    r.eurTraded.should(be(PLN(-5e8)))
    r.newReserves.should(be(PLN(9.5e9)))
    r.plnInjection.should(be(PLN(-5e8 * 4.33)))
  }

  // --- NbpState monthly operation fields ---

  "Nbp.State" should "carry monthly FX operation state without FX reserve ownership" in {
    val nbp = Nbp.State(Rate(0.0575), false, PLN.Zero, PLN.Zero)
    nbp.lastFxTraded.shouldBe(PLN.Zero)
  }

  it should "accept explicit last traded amount" in {
    val nbp = Nbp.State(Rate(0.05), false, PLN.Zero, PLN(-1e8))
    nbp.lastFxTraded.shouldBe(PLN(-1e8))
  }
