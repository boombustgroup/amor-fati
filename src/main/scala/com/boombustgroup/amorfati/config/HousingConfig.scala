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
    priceIncomeElast: Coefficient = Coefficient(1.2),
    priceRateElast: Coefficient = Coefficient(-0.8),
    priceReversion: Coefficient = Coefficient(0.05),
    mortgageSpread: Rate = Rate(0.025),
    mortgageMaturity: Int = 300,
    ltvMax: Share = Share(0.80),
    originationRate: Share = Share(0.003),
    defaultBase: Share = Share(0.001),
    defaultUnempSens: Coefficient = Coefficient(0.05),
    mortgageRecovery: Share = Share(0.70),
    wealthMpc: Share = Share(0.05),
    rentalYield: Rate = Rate(0.045),
    // Regional housing (7 regions: Warsaw, Krakow, Wroclaw, Gdansk, Poznan, Lodz, rest)
    regionalHpi: Vector[Double] = Vector(230.0, 190.0, 170.0, 175.0, 110.0, 140.0, 100.0),
    regionalValueShares: Vector[Share] = Vector(Share(0.25), Share(0.08), Share(0.07), Share(0.08), Share(0.04), Share(0.05), Share(0.43)),
    regionalMortgageShares: Vector[Share] = Vector(Share(0.30), Share(0.10), Share(0.08), Share(0.09), Share(0.04), Share(0.06), Share(0.33)),
    regionalGammas: Vector[Coefficient] = Vector(Coefficient(0.03), Coefficient(0.04), Coefficient(0.04), Coefficient(0.04), Coefficient(0.06), Coefficient(0.05), Coefficient(0.06)),
    regionalIncomeMult: Vector[Multiplier] = Vector(Multiplier(1.35), Multiplier(1.15), Multiplier(1.10), Multiplier(1.12), Multiplier(0.95), Multiplier(1.05), Multiplier(0.82)),
):
  require(ltvMax > Share.Zero && ltvMax <= Share.One, s"ltvMax must be in (0,1]: $ltvMax")
  require(mortgageMaturity > 0, s"mortgageMaturity must be positive: $mortgageMaturity")
  require(initValue >= PLN.Zero, s"initValue must be non-negative: $initValue")
