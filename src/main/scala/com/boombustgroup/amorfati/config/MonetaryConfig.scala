package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.types.*

/** NBP (National Bank of Poland) monetary policy: Taylor rule, standing
  * facilities, QE, and FX intervention.
  *
  * Implements the NBP's interest rate corridor (deposit facility, lombard
  * rate), Taylor-rule rate setting with inertia, quantitative easing via
  * government bond purchases, and FX intervention with reserve management.
  * Standing facilities calibrated to NBP 2024 corridor structure (Uchwala RPP
  * nr 7/2003).
  *
  * Stock values (`qePace`, `fxReserves`) are in raw PLN — scaled by `gdpRatio`
  * in `SimParams.defaults`.
  *
  * @param initialRate
  *   NBP reference rate at simulation start (NBP 2024: 5.75%)
  * @param targetInfl
  *   NBP inflation target (NBP: 2.5% +/- 1pp)
  * @param neutralRate
  *   long-run neutral real interest rate (estimated)
  * @param taylorAlpha
  *   Taylor rule coefficient on inflation gap
  * @param taylorBeta
  *   Taylor rule coefficient on output gap
  * @param taylorInertia
  *   interest rate smoothing parameter (weight on previous rate)
  * @param rateFloor
  *   lower bound on policy rate (effective lower bound)
  * @param rateCeiling
  *   upper bound on policy rate (15%, ~2× post-EU-accession max of 6.75%;
  *   pre-2004 rates reached 24% but model baseline is modern PL economy)
  * @param maxRateChange
  *   maximum monthly rate change in pp (0 = unconstrained)
  * @param nairu
  *   non-accelerating inflation rate of unemployment (estimated: 5%)
  * @param taylorDelta
  *   Taylor rule coefficient on unemployment gap
  * @param reserveRateMult
  *   reserve remuneration as fraction of policy rate (NBP 2024)
  * @param depositFacilitySpread
  *   spread below policy rate for deposit facility (NBP 2024: 1pp)
  * @param lombardSpread
  *   spread above policy rate for lombard facility (NBP 2024: 1pp)
  * @param qePace
  *   monthly QE purchase pace in raw PLN (scaled by gdpRatio)
  * @param qeMaxGdpShare
  *   maximum QE holdings as fraction of GDP
  * @param fxBand
  *   intervention band width around base exchange rate
  * @param fxReserves
  *   initial FX reserves in raw PLN (NBP 2024: ~185 mld PLN, scaled by
  *   gdpRatio)
  * @param fxMaxMonthly
  *   maximum monthly intervention as fraction of reserves
  * @param fxStrength
  *   effectiveness of FX intervention on exchange rate
  */
case class MonetaryConfig(
    initialRate: Rate = Rate.decimal(575, 4),
    targetInfl: Rate = Rate.decimal(25, 3),
    neutralRate: Rate = Rate.decimal(4, 2),
    taylorAlpha: Coefficient = Coefficient.decimal(15, 1),
    taylorBeta: Coefficient = Coefficient.decimal(8, 1),
    taylorInertia: Share = Share.decimal(70, 2),
    rateFloor: Rate = Rate.decimal(1, 3),
    rateCeiling: Rate = Rate.decimal(15, 2),
    maxRateChange: Rate = Rate(0),
    nairu: Share = Share.decimal(5, 2),
    taylorDelta: Coefficient = Coefficient.decimal(5, 1),
    reserveRateMult: Share = Share.decimal(5, 1),
    depositFacilitySpread: Rate = Rate.decimal(1, 2),
    lombardSpread: Rate = Rate.decimal(1, 2),
    // QE (raw — scaled by gdpRatio in SimParams.defaults)
    qePace: PLN = PLN(5000000000L),
    qeMaxGdpShare: Share = Share.decimal(30, 2),
    // FX intervention (raw — scaled by gdpRatio in SimParams.defaults)
    fxBand: Share = Share.decimal(10, 2),
    fxReserves: PLN = PLN(185000000000L),
    fxMaxMonthly: Share = Share.decimal(3, 2),
    fxStrength: Coefficient = Coefficient.decimal(5, 1),
):
  require(rateFloor < rateCeiling, s"rateFloor ($rateFloor) must be < rateCeiling ($rateCeiling)")
  require(rateFloor >= Rate.Zero, s"rateFloor must be non-negative: $rateFloor")
  require(
    qeMaxGdpShare > Share.Zero && qeMaxGdpShare <= Share.One,
    s"qeMaxGdpShare must be in (0,1]: $qeMaxGdpShare",
  )
