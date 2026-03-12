package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.types.*

/** Residential real estate market: prices, mortgages, wealth effects, and
  * regional disaggregation.
  *
  * Models the Polish housing market with income/rate-driven price dynamics,
  * mortgage origination subject to LTV limits (KNF Recommendation S), mortgage
  * default with unemployment sensitivity, housing wealth effects on
  * consumption, and 7-region disaggregation (Warsaw, Krakow, Wroclaw, Gdansk,
  * Poznan, Lodz, rest-of-Poland). Calibrated to NBP residential price survey
  * 2024, KNF Recommendation S, and GUS wage surveys 2024.
  *
  * Stock values (`initValue`, `initMortgage`) are in raw PLN — scaled by
  * `gdpRatio` in `SimParams.defaults`.
  *
  * @param initHpi
  *   initial house price index (base = 100)
  * @param initValue
  *   initial aggregate housing stock value in raw PLN (~3.0 bln PLN, scaled by
  *   gdpRatio)
  * @param initMortgage
  *   initial aggregate mortgage stock in raw PLN (NBP 2024: ~485 mld PLN,
  *   scaled by gdpRatio)
  * @param priceIncomeElast
  *   elasticity of house prices to income growth
  * @param priceRateElast
  *   elasticity of house prices to interest rate changes (negative = rate hikes
  *   depress prices)
  * @param priceReversion
  *   monthly mean-reversion speed of HPI toward fundamental value
  * @param mortgageSpread
  *   mortgage rate spread over policy rate (NBP 2024: ~2.5pp)
  * @param mortgageMaturity
  *   average mortgage maturity in months (25 years = 300, KNF Recommendation S)
  * @param ltvMax
  *   maximum loan-to-value ratio (KNF Recommendation S: 80%)
  * @param originationRate
  *   monthly mortgage origination as fraction of housing stock
  * @param defaultBase
  *   base monthly mortgage default rate
  * @param defaultUnempSens
  *   sensitivity of mortgage default to unemployment rate
  * @param mortgageRecovery
  *   recovery rate on defaulted mortgages (collateral value)
  * @param wealthMpc
  *   marginal propensity to consume from housing wealth (Case, Quigley &
  *   Shiller 2005)
  * @param rentalYield
  *   annual rental yield as fraction of property value (Otodom/NBP: ~4.5%)
  * @param regionalHpi
  *   initial HPI by region (7 regions: Warsaw, Krakow, Wroclaw, Gdansk, Poznan,
  *   Lodz, rest)
  * @param regionalValueShares
  *   share of total housing value by region (NBP/GUS 2024)
  * @param regionalMortgageShares
  *   share of total mortgage stock by region (NBP 2024)
  * @param regionalGammas
  *   region-specific mean-reversion speeds
  * @param regionalIncomeMult
  *   regional income multiplier vs. national average (GUS wage surveys 2024)
  */
case class HousingConfig(
    initHpi: Double = 100.0,
    initValue: PLN = PLN(3.0e12),   // raw — scaled by gdpRatio
    initMortgage: PLN = PLN(485e9), // raw — scaled by gdpRatio
    priceIncomeElast: Double = 1.2,
    priceRateElast: Double = -0.8,
    priceReversion: Ratio = Ratio(0.05),
    mortgageSpread: Rate = Rate(0.025),
    mortgageMaturity: Int = 300,
    ltvMax: Ratio = Ratio(0.80),
    originationRate: Ratio = Ratio(0.003),
    defaultBase: Ratio = Ratio(0.001),
    defaultUnempSens: Double = 0.05,
    mortgageRecovery: Ratio = Ratio(0.70),
    wealthMpc: Ratio = Ratio(0.05),
    rentalYield: Rate = Rate(0.045),
    // Regional housing (7 regions: Warsaw, Krakow, Wroclaw, Gdansk, Poznan, Lodz, rest)
    regionalHpi: Vector[Double] = Vector(230.0, 190.0, 170.0, 175.0, 110.0, 140.0, 100.0),
    regionalValueShares: Vector[Ratio] = Vector(Ratio(0.25), Ratio(0.08), Ratio(0.07), Ratio(0.08), Ratio(0.04), Ratio(0.05), Ratio(0.43)),
    regionalMortgageShares: Vector[Ratio] = Vector(Ratio(0.30), Ratio(0.10), Ratio(0.08), Ratio(0.09), Ratio(0.04), Ratio(0.06), Ratio(0.33)),
    regionalGammas: Vector[Ratio] = Vector(Ratio(0.03), Ratio(0.04), Ratio(0.04), Ratio(0.04), Ratio(0.06), Ratio(0.05), Ratio(0.06)),
    regionalIncomeMult: Vector[Double] = Vector(1.35, 1.15, 1.10, 1.12, 0.95, 1.05, 0.82),
):
  require(ltvMax > Ratio.Zero && ltvMax <= Ratio.One, s"ltvMax must be in (0,1]: $ltvMax")
  require(mortgageMaturity > 0, s"mortgageMaturity must be positive: $mortgageMaturity")
  require(initValue >= PLN.Zero, s"initValue must be non-negative: $initValue")
