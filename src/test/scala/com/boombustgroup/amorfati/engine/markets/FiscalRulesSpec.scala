package com.boombustgroup.amorfati.engine.markets

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.agents.Nbp
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class FiscalRulesSpec extends AnyWordSpec with Matchers:

  given SimParams = SimParams.defaults

  // monthlyGdp = 250e9 → annualGdp = 3000e9
  private val baseInput = FiscalRules.Input(
    rawGovPurchases = PLN("100e9"),
    prevGovSpend = PLN("95e9"),
    cumulativeDebt = PLN("300e9"),
    monthlyGdp = PLN("250e9"), // 10% debt/GDP — well below any threshold
    prevRevenue = PLN("80e9"),
    prevDeficit = PLN("5e9"),
    inflation = Rate("0.025"),
    outputGap = Coefficient("0.0"),
  )

  "FiscalRules.constrain" should {

    "apply no constraint at 30% debt/GDP" in {
      val in     = baseInput.copy(cumulativeDebt = PLN("900e9"))
      val result = FiscalRules.constrain(in)
      // SRW blending still applies (always-on), but no consolidation cuts
      result.status.bindingRule should be <= 1
      result.constrainedGovPurchases should be > PLN.Zero
    }

    "cap spending via SRW ceiling" in {
      // Large raw spending, small prev spending — SRW should pull it down
      val in     = baseInput.copy(
        rawGovPurchases = PLN("200e9"),
        prevGovSpend = PLN("90e9"),
      )
      val result = FiscalRules.constrain(in)
      // SRW blends raw toward ceiling — result should be less than raw
      decimal(result.constrainedGovPurchases) should be < BigDecimal("200e9")
    }

    "apply SGP when deficit/GDP exceeds 3%" in {
      val in     = baseInput.copy(
        prevDeficit = PLN("120e9"), // 120e9 / 250e9 = 48% >> 3%
      )
      val result = FiscalRules.constrain(in)
      decimal(result.status.deficitToGdp) should be > BigDecimal("0.03")
      // SGP caps at revenue + monthlyGdp × sgpDeficitLimit
      val sgpCap = BigDecimal("80e9") + BigDecimal("250e9") * BigDecimal("0.03")
      decimal(result.constrainedGovPurchases) should be <= sgpCap * BigDecimal("1.01") // within tolerance
    }

    "apply Art. 86 (55%) consolidation" in {
      val in     = baseInput.copy(cumulativeDebt = PLN("1700e9")) // 56.7% debt/GDP
      val result = FiscalRules.constrain(in)
      decimal(result.status.debtToGdp) should be > BigDecimal("0.55")
      result.status.bindingRule should be >= 3
      decimal(result.status.spendingCutRatio) should be > BigDecimal("0.0")
    }

    "force budget balance at Art. 216 (60%)" in {
      val in     = baseInput.copy(cumulativeDebt = PLN("1900e9")) // 63.3% debt/GDP
      val result = FiscalRules.constrain(in)
      decimal(result.status.debtToGdp) should be > BigDecimal("0.60")
      result.status.bindingRule shouldBe 4
      // Hard ceiling: cannot exceed revenue
      decimal(result.constrainedGovPurchases) should be <= decimal(in.prevRevenue)
    }

    "most restrictive rule wins when multiple bind" in {
      // Both SGP and Art. 86 binding
      val in     = baseInput.copy(
        cumulativeDebt = PLN("1700e9"), // 56.7% — Art. 86 binding
        prevDeficit = PLN("120e9"),     // huge deficit — SGP binding
      )
      val result = FiscalRules.constrain(in)
      // Art. 86 is more severe than SGP in the cascade
      result.status.bindingRule should be >= 2
    }

    "guarantee constrained ≤ raw always" in {
      val scenarios = Seq(
        baseInput,                                      // low debt
        baseInput.copy(cumulativeDebt = PLN("1700e9")), // 55%+
        baseInput.copy(cumulativeDebt = PLN("1900e9")), // 60%+
        baseInput.copy(prevDeficit = PLN("120e9")),     // high deficit
      )
      for in <- scenarios do
        val result = FiscalRules.constrain(in)
        decimal(result.constrainedGovPurchases) should be <= decimal(in.rawGovPurchases)
    }
  }

  "Nbp.piecewiseFiscalRisk (via bondYield)" should {

    "have zero fiscal risk at 35% debt/GDP" in {
      val y        = Nbp.bondYield(Rate("0.05"), Share("0.35"), Share.Zero, PLN.Zero, Rate.Zero)
      val expected = BigDecimal("0.05") + BigDecimal("0.005") // ref + termPremium
      decimal(y) shouldBe expected +- BigDecimal("0.001")
    }

    "have base-only risk at 45% debt/GDP" in {
      val y        = Nbp.bondYield(Rate("0.05"), Share("0.45"), Share.Zero, PLN.Zero, Rate.Zero)
      val baseRisk = BigDecimal("2.0") * BigDecimal("0.05")
      val expected = BigDecimal("0.05") + BigDecimal("0.005") + baseRisk
      decimal(y) shouldBe expected +- BigDecimal("0.001")
    }

    "be monotonically non-decreasing with debt/GDP" in {
      val debtLevels = Seq(
        BigDecimal("0.30"),
        BigDecimal("0.35"),
        BigDecimal("0.40"),
        BigDecimal("0.42"),
        BigDecimal("0.45"),
        BigDecimal("0.50"),
        BigDecimal("0.55"),
        BigDecimal("0.56"),
        BigDecimal("0.60"),
        BigDecimal("0.62"),
        BigDecimal("0.70"),
        BigDecimal("0.90"),
      )
      val yields     = debtLevels.map(d => decimal(Nbp.bondYield(Rate("0.05"), Share(d), Share.Zero, PLN.Zero, Rate.Zero)))
      for (y1, y2) <- yields.zip(yields.tail) do y2 should be >= y1
    }

    "increase above 40% threshold" in {
      val yBelow = Nbp.bondYield(Rate("0.05"), Share("0.39"), Share.Zero, PLN.Zero, Rate.Zero)
      val yAbove = Nbp.bondYield(Rate("0.05"), Share("0.42"), Share.Zero, PLN.Zero, Rate.Zero)
      decimal(yAbove) should be > decimal(yBelow)
    }

    "never exceed FiscalRiskCap (10%)" in {
      val y = Nbp.bondYield(Rate("0.05"), Share("0.90"), Share.Zero, PLN.Zero, Rate.Zero)
      decimal(y) should be <= BigDecimal("0.05") + BigDecimal("0.005") + BigDecimal("0.10") + BigDecimal("0.001")
    }
  }
