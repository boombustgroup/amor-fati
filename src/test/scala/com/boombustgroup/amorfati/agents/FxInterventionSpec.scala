package com.boombustgroup.amorfati.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.fp.ComputationBoundary
import com.boombustgroup.amorfati.types.*

class FxInterventionSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]
  private val td           = ComputationBoundary

  // Helper: call fxIntervention with enabled=true for tests that need active intervention
  private def fxEnabled(er: Double, reserves: Double, gdp: Double) =
    Nbp.fxIntervention(er, reserves, gdp, enabled = true)

  // --- fxIntervention ---

  "Nbp.fxIntervention" should "return zero effect when ER within band" in {
    // ER deviation = 5% < default band of 10%
    val er     = p.forex.baseExRate * 1.05
    val result = fxEnabled(er, 1e10, 1e9)
    result.erEffect shouldBe 0.0
    result.eurTraded shouldBe PLN.Zero
  }

  it should "return zero effect when ER just inside band boundary" in {
    // Use 9.9% deviation (just inside default 10% band) to avoid FP edge case
    val er     = p.forex.baseExRate * 1.099
    val result = fxEnabled(er, 1e10, 1e9)
    result.erEffect shouldBe 0.0
    result.eurTraded shouldBe PLN.Zero
  }

  it should "intervene when PLN depreciates beyond band (sell EUR)" in {
    // PLN depreciates: ER > baseER * 1.10 -> NBP sells EUR to strengthen PLN
    // erDev > 0 -> direction = -1 -> sells EUR (eurTraded < 0)
    val er       = p.forex.baseExRate * 1.20 // 20% depreciation
    val reserves = 1e10
    val result   = fxEnabled(er, reserves, 1e9)
    result.eurTraded should be < PLN.Zero // sold EUR
    result.erEffect should be < 0.0 // dampens upward ER deviation
    result.newReserves should be < PLN(reserves)
  }

  it should "intervene when PLN appreciates beyond band (buy EUR)" in {
    // PLN appreciates: ER < baseER * 0.90 -> NBP buys EUR to weaken PLN
    // erDev < 0 -> direction = +1 -> buys EUR (eurTraded > 0)
    val er       = p.forex.baseExRate * 0.80 // 20% appreciation
    val reserves = 1e10
    val result   = fxEnabled(er, reserves, 1e9)
    result.eurTraded should be > PLN.Zero // bought EUR
    result.erEffect should be > 0.0 // dampens downward ER deviation
    result.newReserves should be > PLN(reserves)
  }

  it should "not sell more EUR than available reserves" in {
    val er       = p.forex.baseExRate * 1.50 // massive depreciation
    val reserves = 100.0                     // tiny reserves
    val result   = fxEnabled(er, reserves, 1e9)
    result.newReserves should be >= PLN.Zero
    result.eurTraded.abs should be <= PLN(reserves)
  }

  it should "produce erEffect opposing the deviation direction" in {
    // Depreciation -> negative erEffect (pushes ER down)
    val erHigh     = p.forex.baseExRate * 1.30
    val resultHigh = fxEnabled(erHigh, 1e10, 1e9)
    resultHigh.erEffect should be < 0.0

    // Appreciation -> positive erEffect (pushes ER up)
    val erLow     = p.forex.baseExRate * 0.70
    val resultLow = fxEnabled(erLow, 1e10, 1e9)
    resultLow.erEffect should be > 0.0
  }

  it should "produce reserves consistent with eurTraded" in {
    val er       = p.forex.baseExRate * 1.25
    val reserves = 1e10
    val result   = fxEnabled(er, reserves, 1e9)
    // newReserves = max(0, reserves + eurTraded)
    td.toDouble(result.newReserves) shouldBe Math.max(0.0, reserves + td.toDouble(result.eurTraded)) +- 1e-6
  }

  it should "produce zero erEffect when gdp is zero (no div-by-zero)" in {
    val er     = p.forex.baseExRate * 1.25
    val result = fxEnabled(er, 1e10, 0.0)
    result.erEffect shouldBe 0.0
    // But intervention still occurs (reserves change)
    result.eurTraded should be < PLN.Zero
  }

  it should "produce no intervention when ER equals baseER (Eurozone scenario)" in {
    // In EUR regime, ER = baseER -> erDev = 0 -> within any band
    val result = fxEnabled(p.forex.baseExRate, 1e10, 1e9)
    result.erEffect shouldBe 0.0
    result.eurTraded shouldBe PLN.Zero
  }

  // --- FxInterventionResult ---

  "FxInterventionResult" should "be constructable with all fields" in {
    val r = Nbp.FxInterventionResult(0.01, PLN(-5e8), PLN(9.5e9), PLN(-5e8 * 4.33))
    r.erEffect should be(0.01)
    r.eurTraded should be(PLN(-5e8))
    r.newReserves should be(PLN(9.5e9))
    r.plnInjection should be(PLN(-5e8 * 4.33))
  }

  // --- NbpState FX fields ---

  "Nbp.State" should "have backward-compatible constructor with FX defaults" in {
    val nbp = Nbp.State(Rate(0.0575), PLN.Zero, false, PLN.Zero, PLN.Zero, PLN.Zero)
    nbp.fxReserves shouldBe PLN.Zero
    nbp.lastFxTraded shouldBe PLN.Zero
  }

  it should "accept explicit FX field values" in {
    val nbp = Nbp.State(Rate(0.05), PLN.Zero, false, PLN.Zero, PLN(5e9), PLN(-1e8))
    nbp.fxReserves shouldBe PLN(5e9)
    nbp.lastFxTraded shouldBe PLN(-1e8)
  }
