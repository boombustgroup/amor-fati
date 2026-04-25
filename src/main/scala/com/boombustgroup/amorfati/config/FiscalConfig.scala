package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.types.*

/** Government fiscal policy: taxation, spending, transfers, bond market, and
  * local government (JST).
  *
  * Covers the full fiscal architecture of the Polish state:
  * CIT/VAT/PIT/excise/customs revenue, government consumption and investment,
  * EU fund absorption, minimum wage indexation, unemployment benefits,
  * government bond market, JST (local government) fiscal sharing, PIT brackets,
  * and 800+ social transfers.
  *
  * Stock values (`govBaseSpending`, `initGovDebt`) are in raw PLN — scaled by
  * `gdpRatio` in `SimParams.defaults`.
  *
  * @param citRate
  *   corporate income tax rate (MF 2024: 19%)
  * @param vatRates
  *   per-sector effective VAT rates (6 sectors, MF 2024: 23%/19%/12%/6%/10%/7%)
  * @param exciseRates
  *   per-sector effective excise rates (6 sectors, MF 2024: ~80 mld PLN total)
  * @param customsDutyRate
  *   average customs duty rate on non-EU imports (EU CET/Eurostat TARIC: ~4%)
  * @param customsNonEuShare
  *   share of imports subject to customs duties (non-EU origin)
  * @param govBaseSpending
  *   monthly government consumption in raw PLN (scaled by gdpRatio, MF 2024)
  * @param govFiscalRecyclingRate
  *   share of tax revenue recycled into spending (automatic stabilizer)
  * @param govAutoStabMult
  *   multiplier for automatic stabilizer response to output gap
  * @param govInvestShare
  *   share of government spending allocated to capital investment (MF 2024)
  * @param govCapitalMultiplier
  *   fiscal multiplier for government capital spending (Ilzetzki, Mendoza &
  *   Vegh 2013)
  * @param govCurrentMultiplier
  *   fiscal multiplier for government current spending
  * @param govDepreciationRate
  *   annual depreciation rate of public capital stock (GUS F-01)
  * @param govInitCapital
  *   initial public capital stock (PLN, 0 = built from flow)
  * @param euFundsTotalEur
  *   total EU funds allocation 2021-2027 in EUR (EC 2021: 76 mld EUR)
  * @param euFundsPeriodMonths
  *   programming period length in months (7 years = 84)
  * @param euFundsStartMonth
  *   simulation month when EU fund absorption begins
  * @param euFundsAlpha
  *   Beta distribution alpha for absorption curve shape (MFiPR 2024)
  * @param euFundsBeta
  *   Beta distribution beta for absorption curve shape
  * @param euCofinanceRate
  *   national co-financing rate (typically 15%)
  * @param euCapitalShare
  *   share of EU funds directed to capital investment (vs. current)
  * @param minWageAdjustMonths
  *   months between minimum wage adjustments (typically 12)
  * @param minWageInflationIndex
  *   whether to index minimum wage to inflation
  * @param minWageTargetRatio
  *   target ratio of minimum wage to average wage (Art. 5 Ustawa o min.
  *   wynagrodzeniu: 50%)
  * @param minWageConvergenceSpeed
  *   annual convergence speed toward target ratio
  * @param fofConsWeights
  *   Flow-of-Funds: household consumption weights by sector (6 sectors, GUS
  *   2024)
  * @param fofGovWeights
  *   Flow-of-Funds: government spending weights by sector (6 sectors, MF 2024)
  * @param fofExportShares
  *   Flow-of-Funds: export demand shares by sector (6 sectors, GUS/NBP 2024)
  * @param fofInvestWeights
  *   Flow-of-Funds: investment demand weights by sector (6 sectors)
  * @param govBenefitM1to3
  *   monthly unemployment benefit for months 1-3 (PLN, GUS 2024: 1,500)
  * @param govBenefitM4to6
  *   monthly unemployment benefit for months 4-6 (PLN, GUS 2024: 1,200)
  * @param govBenefitDuration
  *   maximum benefit duration in months
  * @param govBenefitCoverage
  *   fraction of unemployed receiving benefits (GUS 2024: ~15%)
  * @param govFiscalRiskBeta
  *   sensitivity of bond spread to debt/GDP ratio
  * @param govTermPremium
  *   term premium on government bonds over policy rate (NBP 2024)
  * @param govAvgMaturityMonths
  *   average maturity of government bond portfolio in months (MF 2024: ~4.5
  *   years = 54 months). Controls yield pass-through speed: each month
  *   1/avgMaturity of the portfolio matures and is refinanced at current yield.
  *   The weighted average coupon converges to market yield gradually, not
  *   instantly. This prevents unrealistic debt service spikes after yield
  *   shocks — matching the actual MF flat redemption profile.
  * @param baseForeignShare
  *   baseline foreign holding share of SPW (NBP 2024: ~35%)
  * @param maxForeignShare
  *   ceiling on foreign share (structural limit, ~55%)
  * @param foreignYieldSensitivity
  *   elasticity of foreign demand to yield spread vs Bund
  * @param foreignErSensitivity
  *   elasticity of foreign demand to PLN depreciation (risk-off)
  * @param bundYield
  *   German 10Y Bund yield benchmark (ECB 2024: ~2.5%)
  * @param bankBondAbsorptionShare
  *   fraction of bank deposits available for bond absorption (KNF 2024: banks
  *   hold ~25-30% of assets in gov bonds, 0% RWA under Basel III)
  * @param initGovDebt
  *   initial government debt in raw PLN (scaled by gdpRatio, MF 2024: ~1.6 bln
  *   PLN)
  * @param jstPitShare
  *   JST (local government) share of PIT revenue (Art. 4 Ustawa o dochodach
  *   JST: 38.46%)
  * @param jstCitShare
  *   JST share of CIT revenue (Art. 4: 6.71%)
  * @param jstPropertyTax
  *   annual property tax per household (PLN, MF 2024)
  * @param jstSubventionShare
  *   education subvention as share of central budget (MF 2024)
  * @param jstDotacjeShare
  *   earmarked grants (dotacje celowe) as share of central budget
  * @param jstSpendingMult
  *   JST spending multiplier (slightly above 1 due to own revenue)
  * @param pitRate1
  *   PIT first bracket rate (Ustawa o PIT 2024: 12%)
  * @param pitRate2
  *   PIT second bracket rate (Ustawa o PIT 2024: 32%)
  * @param pitBracket1Annual
  *   annual income threshold for second PIT bracket (PLN)
  * @param pitTaxCreditAnnual
  *   annual tax credit / free amount (PLN, kwota wolna)
  * @param pitEffectiveRate
  *   effective average PIT rate applied in simplified monthly calculation
  * @param social800
  *   monthly 800+ benefit per child (PLN, Dz.U. 2023)
  * @param social800ChildrenPerHh
  *   average number of eligible children per household (GUS 2024)
  */
case class FiscalConfig(
    // Tax rates
    citRate: Rate = Rate("0.19"),
    citCarryforwardMaxShare: Share = Share("0.50"),             // max 50% of profit offset per year (Art. 7 ustawy o CIT)
    citCarryforwardDecay: Rate = Rate(1.0 / 60),                // monthly decay ≈ 5-year expiry horizon
    vatRates: Vector[Rate] = Vector(Rate("0.23"), Rate("0.19"), Rate("0.12"), Rate("0.06"), Rate("0.10"), Rate("0.07")),
    exciseRates: Vector[Rate] = Vector(Rate("0.01"), Rate("0.04"), Rate("0.03"), Rate("0.005"), Rate("0.002"), Rate("0.02")),
    customsDutyRate: Rate = Rate("0.04"),
    customsNonEuShare: Share = Share("0.30"),
    // Government spending (raw — scaled by gdpRatio in SimParams.defaults)
    govBaseSpending: PLN = PLN("58.3e9"),
    govFiscalRecyclingRate: Share = Share("0.85"),
    govAutoStabMult: Coefficient = Coefficient("3.0"),
    // Government investment
    govInvestShare: Share = Share("0.20"),
    govCapitalMultiplier: Multiplier = Multiplier("1.5"),
    govCurrentMultiplier: Multiplier = Multiplier("0.8"),
    govDepreciationRate: Rate = Rate("0.06"),
    govInitCapital: PLN = PLN("0.0"),
    // EU Funds
    euFundsTotalEur: Multiplier = Multiplier("76e9"),
    euFundsPeriodMonths: Int = 84,
    euFundsStartMonth: Int = 1,
    euFundsAlpha: Scalar = Scalar("2.0"),
    euFundsBeta: Scalar = Scalar("5.0"),
    euCofinanceRate: Share = Share("0.15"),
    euCapitalShare: Share = Share("0.60"),
    // Minimum wage
    minWageAdjustMonths: Int = 12,
    minWageInflationIndex: Boolean = true,
    minWageTargetRatio: Share = Share("0.50"),
    minWageConvergenceSpeed: Share = Share("0.33"),
    // Flow-of-Funds weights (6 sectors)
    fofConsWeights: Vector[Share] = Vector(Share("0.02"), Share("0.22"), Share("0.53"), Share("0.06"), Share("0.07"), Share("0.10")),
    fofGovWeights: Vector[Share] = Vector(Share("0.04"), Share("0.12"), Share("0.08"), Share("0.16"), Share("0.50"), Share("0.10")),
    fofExportShares: Vector[Share] = Vector(Share("0.07"), Share("0.52"), Share("0.12"), Share("0.02"), Share("0.03"), Share("0.24")),
    fofInvestWeights: Vector[Share] = Vector(Share("0.10"), Share("0.40"), Share("0.15"), Share("0.05"), Share("0.20"), Share("0.10")),
    // Unemployment benefits
    govBenefitM1to3: PLN = PLN("1500.0"),
    govBenefitM4to6: PLN = PLN("1200.0"),
    govBenefitDuration: Int = 6,
    govBenefitCoverage: Share = Share("0.15"),
    // Bond market
    govFiscalRiskBeta: Coefficient = Coefficient("2.0"),
    govTermPremium: Rate = Rate("0.005"),
    govAvgMaturityMonths: Int = 54,
    // Bond auction — foreign demand (NBP SPW holder structure 2024)
    baseForeignShare: Share = Share("0.35"),
    maxForeignShare: Share = Share("0.55"),
    foreignYieldSensitivity: Coefficient = Coefficient("8.0"),
    foreignErSensitivity: Coefficient = Coefficient("4.0"),
    bundYield: Rate = Rate("0.025"),
    bankBondAbsorptionShare: Share = Share("0.30"),
    // Fiscal rules (Art. 216 Konstytucja RP, SRW Art. 112aa uFP, SGP)
    fiscalRuleDebtCeiling: Share = Share("0.60"),               // Art. 216: constitutional 60% debt/GDP ceiling
    fiscalRuleCautionThreshold: Share = Share("0.55"),          // Art. 86 uFP: cautionary 55% debt/GDP threshold
    srwRealGrowthCap: Rate = Rate("0.015"),                     // SRW: max real growth allowance (CPI + 1.5pp)
    srwCorrectionSpeed: Share = Share("0.33"),                  // SRW: annual convergence speed toward ceiling
    srwOutputGapSensitivity: Coefficient = Coefficient("0.50"), // SRW: correction term sensitivity to output gap
    fiscalConsolidationSpeed55: Share = Share("0.10"),          // annual spending cut rate at 55% threshold
    fiscalConsolidationSpeed60: Share = Share("0.25"),          // annual spending cut rate at 60% threshold
    sgpDeficitLimit: Share = Share("0.03"),                     // SGP: 3% deficit/GDP Maastricht limit
    fiscalRiskBeta55: Coefficient = Coefficient("3.5"),         // bond yield sensitivity above 55% debt/GDP
    fiscalRiskBeta60: Coefficient = Coefficient("6.0"),         // bond yield sensitivity above 60% debt/GDP
    // Government debt (raw — scaled by gdpRatio in SimParams.defaults)
    initGovDebt: PLN = PLN("1600e9"),
    // JST (local government, Art. 4 Ustawa o dochodach JST)
    jstPitShare: Share = Share("0.3846"),
    jstCitShare: Share = Share("0.0671"),
    jstPropertyTax: PLN = PLN("5000.0"),
    jstSubventionShare: Share = Share("0.03"),
    jstDotacjeShare: Share = Share("0.01"),
    jstSpendingMult: Multiplier = Multiplier("1.02"),
    // PIT (Ustawa o PIT 2024)
    pitRate1: Rate = Rate("0.12"),
    pitRate2: Rate = Rate("0.32"),
    pitBracket1Annual: PLN = PLN("120000.0"),
    pitTaxCreditAnnual: PLN = PLN("3600.0"),
    pitEffectiveRate: Rate = Rate("0.09"),
    // Social 800+ (Dz.U. 2023)
    social800: PLN = PLN("800.0"),
    social800ChildrenPerHh: Scalar = Scalar("0.35"),
):
  require(citRate >= Rate.Zero && citRate <= Rate("1.0"), s"citRate must be in [0,1]: $citRate")
  require(govBaseSpending >= PLN.Zero, s"govBaseSpending must be non-negative: $govBaseSpending")
  require(initGovDebt >= PLN.Zero, s"initGovDebt must be non-negative: $initGovDebt")
  require(vatRates.length == 6, s"vatRates must have 6 sectors: ${vatRates.length}")
  require(exciseRates.length == 6, s"exciseRates must have 6 sectors: ${exciseRates.length}")
  require(fofConsWeights.length == 6, s"fofConsWeights must have 6 sectors: ${fofConsWeights.length}")
  require(fofGovWeights.length == 6, s"fofGovWeights must have 6 sectors: ${fofGovWeights.length}")
  require(fofExportShares.length == 6, s"fofExportShares must have 6 sectors: ${fofExportShares.length}")
  require(fofInvestWeights.length == 6, s"fofInvestWeights must have 6 sectors: ${fofInvestWeights.length}")
