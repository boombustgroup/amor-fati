package com.boombustgroup.amorfati.engine.markets

import com.boombustgroup.amorfati.agents.Nbp
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class FiscalRulesSpec extends AnyWordSpec with Matchers:

  given SimParams = SimParams.defaults

  // monthlyGdp = 250e9 → annualGdp = 3000e9
  private val baseInput = FiscalRules.Input(
    rawGovPurchases = PLN(100e9),
    prevGovSpend = PLN(95e9),
    cumulativeDebt = PLN(300e9),
    monthlyGdp = PLN(250e9), // 10% debt/GDP — well below any threshold
    prevRevenue = PLN(80e9),
    prevDeficit = PLN(5e9),
    inflation = Rate(0.025),
    outputGap = Ratio(0.0),
  )

  "FiscalRules.constrain" should {

    "apply no constraint at 30% debt/GDP" in {
      val in     = baseInput.copy(cumulativeDebt = PLN(900e9))
      val result = FiscalRules.constrain(in)
      // SRW blending still applies (always-on), but no consolidation cuts
      result.status.bindingRule should be <= 1
      result.constrainedGovPurchases should be > PLN.Zero
    }

    "cap spending via SRW ceiling" in {
      // Large raw spending, small prev spending — SRW should pull it down
      val in     = baseInput.copy(
        rawGovPurchases = PLN(200e9),
        prevGovSpend = PLN(90e9),
      )
      val result = FiscalRules.constrain(in)
      // SRW blends raw toward ceiling — result should be less than raw
      result.constrainedGovPurchases.toDouble should be < 200e9
    }

    "apply SGP when deficit/GDP exceeds 3%" in {
      val in     = baseInput.copy(
        prevDeficit = PLN(120e9), // 120e9 / 250e9 = 48% >> 3%
      )
      val result = FiscalRules.constrain(in)
      result.status.deficitToGdp.toDouble should be > 0.03
      // SGP caps at revenue + monthlyGdp × sgpDeficitLimit
      val sgpCap = 80e9 + 250e9 * 0.03
      result.constrainedGovPurchases.toDouble should be <= sgpCap * 1.01 // within tolerance
    }

    "apply Art. 86 (55%) consolidation" in {
      val in     = baseInput.copy(cumulativeDebt = PLN(1700e9)) // 56.7% debt/GDP
      val result = FiscalRules.constrain(in)
      result.status.debtToGdp.toDouble should be > 0.55
      result.status.bindingRule should be >= 3
      result.status.spendingCutRatio.toDouble should be > 0.0
    }

    "force budget balance at Art. 216 (60%)" in {
      val in     = baseInput.copy(cumulativeDebt = PLN(1900e9)) // 63.3% debt/GDP
      val result = FiscalRules.constrain(in)
      result.status.debtToGdp.toDouble should be > 0.60
      result.status.bindingRule shouldBe 4
      // Hard ceiling: cannot exceed revenue
      result.constrainedGovPurchases.toDouble should be <= in.prevRevenue.toDouble
    }

    "most restrictive rule wins when multiple bind" in {
      // Both SGP and Art. 86 binding
      val in     = baseInput.copy(
        cumulativeDebt = PLN(1700e9), // 56.7% — Art. 86 binding
        prevDeficit = PLN(120e9),     // huge deficit — SGP binding
      )
      val result = FiscalRules.constrain(in)
      // Art. 86 is more severe than SGP in the cascade
      result.status.bindingRule should be >= 2
    }

    "guarantee constrained ≤ raw always" in {
      val scenarios = Seq(
        baseInput,                                    // low debt
        baseInput.copy(cumulativeDebt = PLN(1700e9)), // 55%+
        baseInput.copy(cumulativeDebt = PLN(1900e9)), // 60%+
        baseInput.copy(prevDeficit = PLN(120e9)),     // high deficit
      )
      for in <- scenarios do
        val result = FiscalRules.constrain(in)
        result.constrainedGovPurchases.toDouble should be <= in.rawGovPurchases.toDouble
    }
  }

  "Nbp.piecewiseFiscalRisk (via bondYield)" should {

    "have zero fiscal risk at 35% debt/GDP" in {
      val y        = Nbp.bondYield(Rate(0.05), Ratio(0.35), Ratio.Zero, PLN.Zero, Rate.Zero)
      val expected = 0.05 + 0.005 // ref + termPremium
      y.toDouble shouldBe expected +- 0.001
    }

    "have base-only risk at 45% debt/GDP" in {
      val y        = Nbp.bondYield(Rate(0.05), Ratio(0.45), Ratio.Zero, PLN.Zero, Rate.Zero)
      val baseRisk = 2.0 * 0.05
      val expected = 0.05 + 0.005 + baseRisk
      y.toDouble shouldBe expected +- 0.001
    }

    "be monotonically non-decreasing with debt/GDP" in {
      val debtLevels = Seq(0.30, 0.35, 0.40, 0.42, 0.45, 0.50, 0.55, 0.56, 0.60, 0.62, 0.70, 0.90)
      val yields     = debtLevels.map(d => Nbp.bondYield(Rate(0.05), Ratio(d), Ratio.Zero, PLN.Zero, Rate.Zero).toDouble)
      for (y1, y2) <- yields.zip(yields.tail) do y2 should be >= y1
    }

    "increase above 40% threshold" in {
      val yBelow = Nbp.bondYield(Rate(0.05), Ratio(0.39), Ratio.Zero, PLN.Zero, Rate.Zero)
      val yAbove = Nbp.bondYield(Rate(0.05), Ratio(0.42), Ratio.Zero, PLN.Zero, Rate.Zero)
      yAbove.toDouble should be > yBelow.toDouble
    }

    "never exceed FiscalRiskCap (10%)" in {
      val y = Nbp.bondYield(Rate(0.05), Ratio(0.90), Ratio.Zero, PLN.Zero, Rate.Zero)
      y.toDouble should be <= 0.05 + 0.005 + 0.10 + 0.001
    }
  }
