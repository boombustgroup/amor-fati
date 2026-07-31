# Institutional Sector Equations

This document consolidates the implemented institutional-sector model into a
publication-facing equation section. It covers central government, social
funds, local government, NBP monetary operations, external-sector/BOP dynamics,
insurance, NBFI/TFI, and quasi-fiscal BGK/PFR-style vehicles.

It describes current executable behavior. It does not introduce new fiscal,
monetary, external, or non-bank financial rules and does not replace the
implementation anchors listed below.

## Source Anchors

| Source | Role |
| --- | --- |
| [Model specification](model-specification.md#reviewer-reading-path) | Canonical reviewer path and model overview. |
| [Model notation and state vector](model-notation-and-state-vector.md) | Public, NBP, external, social-fund, insurance, NBFI, and quasi-fiscal state notation. |
| [Monthly transition function](monthly-transition-function.md) | $X_{t} \to X_{\tau}$ timing, same-month economics, flow emission, SFC validation, and next-pre boundary. |
| [Banking and financial-sector equations](banking-and-financial-sector-equations.md) | Commercial-bank, financial-stability, and bank-facing financial-sector interface equations. |
| [Fiscal, monetary, bond-market, and external rules](behavioral-equations-and-decision-rules.md#fiscal-monetary-bond-market-and-external-rules) and [insurance, NBFI, quasi-fiscal, and JST rules](behavioral-equations-and-decision-rules.md#insurance-nbfi-quasi-fiscal-and-jst-rules) | Implementation-oriented institutional-sector rule catalogs. |
| [SFC matrix evidence](sfc-matrix-evidence.md) | Accounting matrix evidence, generated BSM/TFM artifacts, mechanism semantics, and stock-flow reconciliation. |
| [Calibration register](calibration-register.md) and [data bridge](data-bridge-national-financial-accounts.md) | Parameter provenance, official source bridge, empirical transformation rules, and visible calibration gaps. |
| [`engine/markets/FiscalBudget.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/markets/FiscalBudget.scala) and [`engine/markets/FiscalRules.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/markets/FiscalRules.scala) | Central-government budget, debt metric, public capital, spending constraints, and fiscal-rule diagnostics. |
| [`agents/SocialSecurity.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/SocialSecurity.scala), [`agents/EarmarkedFunds.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/EarmarkedFunds.scala), and [`agents/Jst.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Jst.scala) | ZUS/FUS, NFZ, PPK, FP/PFRON/FGSP, demographics, and local-government fiscal aggregates. |
| [`agents/Nbp.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Nbp.scala), [`engine/economics/OpenEconEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/OpenEconEconomics.scala), and [`engine/markets/OpenEconomy.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/markets/OpenEconomy.scala) | NBP policy rule, sovereign yield, QE request, FX intervention, BOP, exchange-rate, NFA, and external-trade dynamics. |
| [`agents/Insurance.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Insurance.scala), [`agents/Nbfi.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Nbfi.scala), and [`agents/QuasiFiscal.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/QuasiFiscal.scala) | Insurance reserves and flows, TFI/NBFI assets and credit, and quasi-fiscal bonds/lending. |
| [`engine/flows/*`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/flows) | Runtime flow emitters for government, social funds, NBP/banking plumbing, external sector, insurance, NBFI, JST, and quasi-fiscal mechanisms. |
| [`montecarlo/timeseries/McTimeseriesSchema.scala`](../modules/montecarlo/src/main/scala/com/boombustgroup/amorfati/montecarlo/timeseries/McTimeseriesSchema.scala) | Public output columns for fiscal, monetary, external, insurance, NBFI, JST, and quasi-fiscal surfaces. |

## Institutional State And Ownership Boundary

The institutional model uses both behavioral/state memory and ledger-owned
financial stocks. The main world-state families are:

$$
\begin{aligned}
W^{gov}_{t} & \text{central-government fiscal state} \\
W^{nbp}_{t} & \text{NBP policy state, QE regime memory, FX-operation memory} \\
W^{bop}_{t} & \text{balance-of-payments and NFA macro memory} \\
W^{fx}_{t} & \text{exchange rate, exports, imports, trade-balance memory} \\
W^{soc}_{t} & \text{ZUS, NFZ, PPK, JST, demographics, FP/PFRON/FGSP state} \\
W^{fin}_{t} & \text{insurance, NBFI/TFI, quasi-fiscal, bond/equity market memory} \\
W^{ext}_{t} & \text{GVC, immigration, remittance, and tourism state}
\end{aligned}
$$

Ledger-owned institutional balances are stored in:

$$
\begin{aligned}
L^{G}_{t} & \text{government bond issuer stock} \\
L^{ROW}_{t} & \text{foreign government-bond, foreign-asset, and equity holdings} \\
L^{\mathrm{NBP}}_{t} & \text{NBP government-bond holdings, foreign assets, reserve liability} \\
L^{INS}_{t} & \text{insurance reserves, bond holdings, equity holdings} \\
L^{FUNDS}_{t} & \text{ZUS/NFZ/PPK/FP/PFRON/FGSP/JST cash, TFI/NBFI, quasi-fiscal balances}
\end{aligned}
$$

The ownership boundary matters:

- `GovState.cumulativeDebt` is the domestic fiscal-rule debt metric. The
  holder-resolved tradable government-bond stock is ledger-owned.
- `Jst.State.debt` is a local-government cumulative fiscal metric. JST deposits
  are ledger-owned; JST debt is not yet holder-resolved as a tradable
  instrument.
- `OpenEconomy.BopState` is BOP/IIP macro memory. NBP FX reserve ownership is
  ledger-owned in $L^{NBP}$.
- `Nbp.qeCumulative` tracks QE-regime purchases, not total NBP government-bond
  holdings.
- Insurance and TFI corporate-bond holdings are settled by the corporate-bond
  ledger path. Insurance/NBFI receive opening views for income and return
  closing non-corporate-bond balances.
- Quasi-fiscal bonds and subsidized loan stocks are ledger-owned fund balances;
  ESA 2010 debt is a diagnostic aggregation over central-government and
  quasi-fiscal stocks.

## Central Government Budget

Central-government budget revenue uses the fiscal net NBP remittance:

$$
\begin{aligned}
NbpBondIncome_{\tau} &=  \\
&\quad NbpGovBondHoldings_{t} \cdot GovBondMarketYield_{\tau} / 12 \\
 \\
NbpFiscalRemittance_{\tau} &=  \\
&\quad NbpBondIncome_{\tau} \\
&\quad - ReserveInterest_{\tau} \\
&\quad - StandingFacilityNet_{\tau} \\
 \\
TaxRevenue_{\tau} &=  \\
&\quad CIT_{\tau} + VAT_{\tau} + NbpFiscalRemittance_{\tau} \\
&\quad + Excise_{\tau} + Customs_{\tau} \\
 \\
TotalRevenue_{\tau} &= TaxRevenue_{\tau} + GovDividendRevenue_{\tau}
\end{aligned}
$$

Government purchases are either supplied by the fiscal-demand stage or fall
back to price-indexed base spending:

$$
\begin{aligned}
GovPurchasesRaw_{\tau} &=
\begin{cases}
GovPurchasesActual_{\tau}, & \text{if supplied and positive}, \\
GovBaseSpending \cdot PriceLevel_{\tau}, & \text{otherwise},
\end{cases} \\
 \\
GovCurrentSpend_{\tau} &=  \\
&\quad GovPurchasesRaw_{\tau} \cdot (1 - govInvestShare) \\
 \\
GovCapitalSpend_{\tau} &=  \\
&\quad GovPurchasesRaw_{\tau} \cdot govInvestShare
\end{aligned}
$$

Central-government spending is:

$$
\begin{aligned}
TotalSpend_{\tau} &=  \\
&\quad UnemploymentBenefits_{\tau} \\
&\quad + SocialTransfers_{\tau} \\
&\quad + GovCurrentSpend_{\tau} \\
&\quad + GovCapitalSpend_{\tau} \\
&\quad + DebtService_{\tau} \\
&\quad + ZUSSubvention_{\tau} \\
&\quad + NFZSubvention_{\tau} \\
&\quad + EarmarkedFundSubvention_{\tau} \\
&\quad + EuCofinancing_{\tau}
\end{aligned}
$$

The monthly deficit and domestic fiscal-rule debt metric are:

$$
\begin{aligned}
Deficit_{\tau} &= TotalSpend_{\tau} - TotalRevenue_{\tau} \\
GovDebtMetric_{\tau} &= GovDebtMetric_{t} + Deficit_{\tau}
\end{aligned}
$$

Public capital is a stock updated by depreciation, domestic public investment,
and EU project capital:

$$
\begin{aligned}
PublicCapital_{\tau} &=  \\
&\quad PublicCapital_{t} \cdot (1 - govDepreciationRate / 12) \\
&\quad + GovCapitalSpend_{\tau} \\
&\quad + EuProjectCapital_{\tau}
\end{aligned}
$$

These are accounting identities and stock recursions conditional on the
current-month fiscal inputs. They do not by themselves determine the realism of
the fiscal path.

## Government Purchases And Fiscal Rules

Before the budget update, unconstrained government purchases are computed from
price-indexed base spending and an unemployment stabilizer:

$$
\begin{aligned}
UnempGap_{\tau} &= \max(UnemploymentRate_{\tau} - NAIRU, 0) \\
 \\
RawGovPurchases_{\tau} &=  \\
&\quad GovBaseSpending \cdot \max(PriceLevel_{\tau}, 1) \\
&\quad + GovBaseSpending \cdot UnempGap_{\tau} \cdot govAutoStabMult
\end{aligned}
$$

Fiscal rules constrain the discretionary purchase envelope. The debt and
deficit diagnostics are:

$$
\begin{aligned}
AnnualGDP_{\tau} &= 12 \cdot GDP_{\tau} \\
DebtToGdp_{\tau} &= GovDebtMetric_{t} / AnnualGDP_{\tau} \\
DeficitToGdp_{\tau} &= PrevDeficit_{t} / GDP_{\tau}
\end{aligned}
$$

The stabilizing expenditure rule (SRW) ceiling is:

$$
\begin{aligned}
SrwCeiling_{\tau} &=  \\
&\quad PrevGovSpend_{t} \cdot \\
&\quad (1 + MonthlyInflation_{\tau} + MonthlyRealCap \\
&\quad + \max(OutputGap_{\tau} \cdot srwOutputGapSensitivity, 0) / 12)
\end{aligned}
$$

If the SRW ceiling is below raw purchases, spending blends toward the ceiling
at the configured monthly correction speed:

$$
\begin{aligned}
AfterSrw_{\tau} &=  \\
&\quad RawGovPurchases_{\tau} \cdot (1 - srwCorrectionSpeed/12) \\
&\quad + SrwCeiling_{\tau} \cdot (srwCorrectionSpeed/12)
\end{aligned}
$$

The SGP branch applies only when the annualized deficit ratio exceeds the
configured 3 percent limit. It estimates lagged non-purchase spending and
gradually moves discretionary spending toward the revenue-plus-allowable
deficit path:

$$
\begin{aligned}
PrevTotalSpend_{t} &= PrevRevenue_{t} + PrevDeficit_{t} \\
PrevNonPurchaseSpend_{t} &= \max(PrevTotalSpend_{t} - PrevGovSpend_{t}, 0) \\
AllowableDeficit_{\tau} &= GDP_{\tau} \cdot sgpDeficitLimit \\
MaxDiscretionarySpend_{\tau} &=  \\
&\quad \max(PrevRevenue_{t} + AllowableDeficit_{\tau} - PrevNonPurchaseSpend_{t}, 0)
\end{aligned}
$$

Debt-threshold branches then apply consolidation paths above the 55 percent
and 60 percent debt/GDP thresholds. The binding rule code is an observability
surface:

$$
\begin{aligned}
0 &= \text{no rule} \\
1 &= SRW \\
2 &= SGP \\
3 &= \text{55 percent caution threshold} \\
4 &= \text{60 percent constitutional ceiling}
\end{aligned}
$$

These rules are policy heuristics and fiscal-stabilization constraints. The
budget identity above is the accounting object that receives their realized
spending output.

## Social Funds And Local Government

The payroll base for social funds is computed from employed household
contracts:

$$
\begin{aligned}
Payroll_{\tau} &=  \\
&\quad (Employed_{\tau}, GrossWages_{\tau}, \\
&\quad ZUSContrib_{\tau}, NFZContrib_{\tau}, PPKContrib_{\tau}, \\
&\quad FPContrib_{\tau}, FGSPContrib_{\tau})
\end{aligned}
$$

ZUS/FUS flows are:

$$
\begin{aligned}
ZUSContrib_{\tau} &= PayrollZUS_{\tau} \\
PensionPayments_{\tau} &= Retirees_{\tau} \cdot zusBasePension \\
ZUSSubvention_{\tau} &=  \\
&\quad \max(PensionPayments_{\tau} - ZUSContrib_{\tau}, 0)
\end{aligned}
$$

NFZ flows are:

$$
\begin{aligned}
NFZContrib_{\tau} &= PayrollNFZ_{\tau} \\
 \\
NFZSpending_{\tau} &=  \\
&\quad WorkingAgePopulation_{\tau} \cdot nfzPerCapitaCost \\
&\quad + Retirees_{\tau} \cdot nfzPerCapitaCost \cdot nfzAgingElasticity \\
 \\
NFZSubvention_{\tau} &=  \\
&\quad \max(NFZSpending_{\tau} - NFZContrib_{\tau}, 0)
\end{aligned}
$$

PPK contributions are payroll contributions. PPK government-bond purchases are
a configured allocation of those contributions:

$$
\begin{aligned}
PPKContrib_{\tau} &= PayrollPPK_{\tau} \\
PPKBondPurchase_{\tau} &= PPKContrib_{\tau} \cdot ppkBondAlloc
\end{aligned}
$$

Earmarked funds use dedicated contribution and spending identities:

$$
\begin{aligned}
FPContrib_{\tau} &= PayrollFP_{\tau} \\
FPSpending_{\tau} &=  \\
&\quad UnemploymentBenefits_{\tau} + Employed_{\tau} \cdot fpAlmpSpendPerWorker \\
FPSubvention_{\tau} &= \max(FPSpending_{\tau} - FPContrib_{\tau}, 0) \\
 \\
PFRONContrib_{\tau} &= pfronMonthlyRevenue \\
PFRONSpending_{\tau} &= pfronMonthlySpending \\
PFRONSubvention_{\tau} &= \max(PFRONSpending_{\tau} - PFRONContrib_{\tau}, 0) \\
 \\
FGSPContrib_{\tau} &= PayrollFGSP_{\tau} \\
FGSPSpending_{\tau} &=  \\
&\quad BankruptFirms_{\tau} \cdot AvgWorkersPerBankruptFirm_{\tau} \cdot fgspPayoutPerWorker \\
FGSPSubvention_{\tau} &= \max(FGSPSpending_{\tau} - FGSPContrib_{\tau}, 0)
\end{aligned}
$$

Local-government own revenue is:

$$
\begin{aligned}
JstOwnRevenue_{\tau} &=  \\
&\quad PitRevenue_{\tau} \cdot jstPitShare \\
&\quad + CentralCitRevenue_{\tau} \cdot jstCitShare \\
&\quad + Firms_{\tau} \cdot jstPropertyTax / 12 \\
 \\
JstGovTransfers_{\tau} &=  \\
&\quad GDP_{\tau} \cdot jstSubventionShare / 12 \\
&\quad + GDP_{\tau} \cdot jstDotacjeShare / 12 \\
 \\
JstTotalRevenue_{\tau} &=  \\
&\quad JstOwnRevenue_{\tau} + JstGovTransfers_{\tau}
\end{aligned}
$$

If explicit PIT revenue is unavailable, the JST PIT share falls back to a wage
income proxy. JST spending, deficit, deposit change, and debt metric are:

$$
\begin{aligned}
JstSpending_{\tau} &= JstTotalRevenue_{\tau} \cdot jstSpendingMultiplier \\
JstDeficit_{\tau} &= JstSpending_{\tau} - JstTotalRevenue_{\tau} \\
JstDepositChange_{\tau} &= JstTotalRevenue_{\tau} - JstSpending_{\tau} \\
JstDebtMetric_{\tau} &= JstDebtMetric_{t} + JstDeficit_{\tau}
\end{aligned}
$$

In runtime flow evidence, `JstRevenue` denotes the own-revenue mechanism and
`JstGovSubvention` denotes central-budget transfers. The public `JstRevenue`
TSV column reports $JstTotalRevenue_{\tau}$ from `Jst.State.revenue`.

Social-fund cash balances and JST deposits are ledger-owned. ZUS, NFZ,
earmarked-fund, PPK, and JST state objects are monthly flow and fiscal metric
surfaces.

## NBP Policy, Sovereign Yield, QE, And FX

The NBP reference rate follows a smoothed Taylor-type rule. Policy inflation
combines realized and expected inflation:

$$
\begin{aligned}
PolicyInflation_{\tau} &=  \\
&\quad ExpectedInflationWeight \cdot ExpectedInflation_{t} \\
&\quad + (1 - ExpectedInflationWeight) \cdot Inflation_{\tau}
\end{aligned}
$$

The raw policy-rate target is:

$$
\begin{aligned}
TaylorTarget_{\tau} &=  \\
&\quad NeutralRate \\
&\quad + taylorAlpha \cdot (PolicyInflation_{\tau} - TargetInflation) \\
&\quad - taylorDelta \cdot OutputGap_{\tau} \\
&\quad + taylorBeta \cdot ExchangeRateChange_{\tau}
\end{aligned}
$$

The implemented rate then applies inertia, a maximum monthly move, and the
configured floor/ceiling:

$$
\begin{aligned}
SmoothedRate_{\tau} &=  \\
&\quad ReferenceRate_{t} \cdot taylorInertia \\
&\quad + TaylorTarget_{\tau} \cdot (1 - taylorInertia) \\
 \\
ReferenceRate_{\tau} &=  \\
&\quad \mathrm{clamp}(SmoothedRate_{\tau}, rateFloor, rateCeiling, maxMonthlyChange)
\end{aligned}
$$

The sovereign yield is:

$$
\begin{aligned}
ForeignDemandDiscount_{\tau} &=
\begin{cases}
\mathrm{ForeignDemandDiscount}, & \text{if } NFA_{t} > 0, \\
0, & \text{otherwise},
\end{cases} \\
 \\
GovBondMarketYield_{\tau} &=  \\
&\quad \max(ReferenceRate_{\tau} + TermPremium, BundYield + TermPremium) \\
&\quad + \mathrm{FiscalRisk}(DebtToGdp_{\tau}) \\
&\quad - \mathrm{QeCompression}(NbpQeCumulative_{t} / AnnualGDP_{\tau}) \\
&\quad - ForeignDemandDiscount_{\tau} \\
&\quad + CredibilityPremium_{\tau}
\end{aligned}
$$

The weighted coupon on government debt is a rolling weighted-average-maturity
rule:

$$
\begin{aligned}
FreshShare_{\tau} &=  \\
&\quad \min(1 / govAvgMaturityMonths \\
&\quad + \max(Deficit_{t}, 0) / GovBondOutstanding_{t}, \\
&\quad 1) \\
 \\
GovDebtWeightedCoupon_{\tau} &=  \\
&\quad GovDebtWeightedCoupon_{t} \cdot (1 - FreshShare_{\tau}) \\
&\quad + GovBondMarketYield_{\tau} \cdot FreshShare_{\tau}
\end{aligned}
$$

Government debt service is computed from ledger-owned government-bond
outstanding and the updated weighted coupon, with a protective cap relative to
same-month GDP:

$$
\begin{aligned}
DebtService_{\tau} &=  \\
&\quad \min(GovBondOutstanding_{t} \cdot GovDebtWeightedCoupon_{\tau} / 12, \\
&\quad GDP_{\tau} \cdot maxDebtServiceGdpShare)
\end{aligned}
$$

QE is a policy request, not a direct stock mutation. QE activates when the
reference rate is near the lower bound and realized or expected inflation is
materially below target. The requested purchase is bounded by the GDP-share
ceiling, bank-held bond supply, and QE pace. The GDP-share ceiling uses the
same annualized GDP basis as the bond-market debt/GDP and QE-compression
ratios:

$$
\begin{aligned}
QeRequest_{\tau} &=  \\
&\quad \min(\max(qeMaxGdpShare \cdot AnnualGDP_{\tau} - NbpGovBondHoldings_{t}, 0), \\
&\quad BankGovBondHoldings_{t}, \\
&\quad QePace_{\tau})
\end{aligned}
$$

Actual QE settlement occurs in the government-bond waterfall. This keeps the
asset transfer exact: bonds sold by banks equal bonds acquired by NBP.

FX intervention is an NBP reserve operation. If the exchange rate leaves the
configured band around the base exchange rate, NBP buys or sells EUR:

$$
\begin{aligned}
ExchangeRateDeviation_{t} &=  \\
&\quad ExchangeRate_{t} / BaseExchangeRate - 1 \\
 \\
FxTrade_{\tau} &=
\begin{cases}
0, & \mathrm{abs}(ExchangeRateDeviation_{t}) \le fxBand, \\
\text{signed bounded reserve trade}, & \text{otherwise}.
\end{cases}
\end{aligned}
$$

EUR purchases increase NBP foreign assets and inject PLN reserves into banks.
EUR sales reduce foreign assets and drain bank reserves. The same operation
also contributes an exchange-rate shock term to the external-sector update.

## External Sector And BOP

Exports are either supplied by the GVC module or computed from an opening
export base, foreign GDP growth, real exchange-rate competitiveness, and an
automation-driven unit-labor-cost effect:

$$
\begin{aligned}
Exports_{\tau} &=
\begin{cases}
GvcExports_{\tau}, & \text{if supplied}, \\
exportBase \cdot RealExchangeRateEffect_{\tau}
\cdot ForeignGdpGrowthFactor_{\tau}
\cdot AutomationUlcEffect_{\tau}, & \text{otherwise}.
\end{cases}
\end{aligned}
$$

Imports include household consumption imports, technology and investment
imports, imported intermediates, and tourism imports:

$$
\begin{aligned}
Imports_{\tau} &=  \\
&\quad HouseholdImportCons_{\tau} \\
&\quad + TechnologyAndInvestmentImports_{\tau} \\
&\quad + ImportedIntermediates_{\tau} \\
&\quad + TourismImport_{\tau} \\
 \\
TradeBalance_{\tau} &=  \\
&\quad Exports_{\tau} + TourismExport_{\tau} - Imports_{\tau}
\end{aligned}
$$

When GVC intermediate imports are not supplied, sectoral intermediate imports
are:

$$
\begin{aligned}
ImportedIntermediates_{s,\tau} &=  \\
&\quad RealSectorOutput_{s,\tau} \\
&\quad \cdot importContent_{s} \\
&\quad \cdot NominalExchangeRateEffect_{\tau}
\end{aligned}
$$

The current account before firm/equity owner adjustments is:

$$
\begin{aligned}
PrimaryIncome_{\tau} &= NFA_{t} \cdot nfaReturnRate / 12 \\
SecondaryIncome_{\tau} &= EUFunds_{\tau} - RemittanceOutflow_{\tau} + DiasporaInflow_{\tau} \\
CurrentAccount0_{\tau} &=  \\
&\quad TradeBalance_{\tau} + PrimaryIncome_{\tau} + SecondaryIncome_{\tau}
\end{aligned}
$$

The exported BOP then subtracts foreign dividend outflows and FDI-related
profit shifting/repatriation:

$$
\begin{aligned}
CurrentAccount_{\tau} &=  \\
&\quad CurrentAccount0_{\tau} \\
&\quad - ForeignDividendOutflow_{\tau} \\
&\quad - FdiProfitShifting_{\tau} \\
&\quad - FdiRepatriation_{\tau}
\end{aligned}
$$

Profit shifting is also booked into imports/trade balance as an imported
service. The document and output columns therefore treat `FdiGrossOutflow` as
diagnostic composition, not the direct current-account subtraction term.

The capital account combines FDI, portfolio flows, carry-trade flows, and
capital-flight outflows:

$$
\begin{aligned}
FDI_{\tau} &=  \\
&\quad fdiBase \\
&\quad \cdot AutomationBoost_{\tau} \\
&\quad \cdot NegativeNfaDampening_{\tau} \\
 \\
PortfolioFlows_{\tau} &=  \\
&\quad GDP_{\tau} \\
&\quad \cdot (RateDifferential_{\tau} + NfaRiskPremium_{\tau}) \\
&\quad \cdot portfolioSensitivity \\
 \\
CapitalAccount_{\tau} &=  \\
&\quad FDI_{\tau} + PortfolioFlows_{\tau} \\
&\quad + CarryTradeFlow_{\tau} \\
&\quad - CapitalFlightOutflow_{\tau}
\end{aligned}
$$

The exchange-rate update responds to the BOP ratio, negative-NFA risk, FX
intervention shock, and PPP drift:

$$
\begin{aligned}
ExchangeRateShock_{\tau} &=  \\
&\quad exRateAdjSpeed \cdot (-(CurrentAccount0_{\tau} + CapitalAccount_{\tau}) / GDP_{\tau} \\
&\quad + NfaRisk_{\tau}) \\
&\quad + FxInterventionShock_{\tau} \\
&\quad + PppDrift_{\tau} \\
 \\
ExchangeRate_{\tau} &=  \\
&\quad \mathrm{clamp}(ExchangeRate_{t} \cdot (1 + ExchangeRateShock_{\tau}), \\
&\quad erFloor, \\
&\quad erCeiling)
\end{aligned}
$$

Open-economy NFA is first updated by the pre-adjustment current account plus a
partial exchange-rate valuation effect on foreign assets:

$$
\begin{aligned}
ValuationEffect_{\tau} &=  \\
&\quad ForeignAssets_{t} \cdot ExchangeRateChange_{\tau} \cdot valuationPassThrough \\
 \\
NFA0_{\tau} &= NFA_{t} + CurrentAccount0_{\tau} + ValuationEffect_{\tau}
\end{aligned}
$$

The final exported BOP position then applies foreign-dividend and FDI-owner
outflows:

$$
\begin{aligned}
NFA_{\tau} &=  \\
&\quad NFA0_{\tau} \\
&\quad - ForeignDividendOutflow_{\tau} \\
&\quad - FdiProfitShifting_{\tau} \\
&\quad - FdiRepatriation_{\tau}
\end{aligned}
$$

`BopState` is macro external-position memory. It is not the holder-resolved
ledger. Trade, FDI, portfolio, carry-trade, EU funds, diaspora remittance,
tourism, and capital-flight flows are emitted through runtime mechanisms and
validated through the SFC/ledger layer where supported.

## Insurance

Insurance premiums are proportional to the employed wage bill:

$$
\begin{aligned}
LifePremium_{\tau} &=  \\
&\quad Employed_{\tau} \cdot Wage_{\tau} \cdot lifePremiumRate \\
 \\
NonLifePremium_{\tau} &=  \\
&\quad Employed_{\tau} \cdot Wage_{\tau} \cdot nonLifePremiumRate
\end{aligned}
$$

Claims are:

$$
\begin{aligned}
LifeClaims_{\tau} &=  \\
&\quad LifePremium_{\tau} \cdot lifeLossRatio \\
 \\
NonLifeClaims_{\tau} &=  \\
&\quad NonLifePremium_{\tau} \cdot nonLifeLossRatio \\
&\quad \cdot (1 + \max(UnemploymentRate_{\tau} - nonLifeUnempThreshold, 0) \\
&\quad \cdot nonLifeUnempSensitivity)
\end{aligned}
$$

Investment income is:

$$
\begin{aligned}
InsuranceInvestmentIncome_{\tau} &=  \\
&\quad InsGovBondHoldings_{t} \cdot GovBondMarketYield_{\tau} / 12 \\
&\quad + InsCorpBondHoldings_{t} \cdot CorpBondYield_{\tau} / 12 \\
&\quad + InsEquityHoldings_{t} \cdot EquityReturn_{\tau} \\
&\quad - InsCorpBondDefaultLoss_{\tau}
\end{aligned}
$$

Reserve updates split investment income by the opening life/non-life reserve
share:

$$
\begin{aligned}
LifeReserves_{\tau} &=  \\
&\quad LifeReserves_{t} \\
&\quad + LifePremium_{\tau} \\
&\quad - LifeClaims_{\tau} \\
&\quad + InsuranceInvestmentIncome_{\tau} \cdot LifeReserveShare_{t} \\
 \\
NonLifeReserves_{\tau} &=  \\
&\quad NonLifeReserves_{t} \\
&\quad + NonLifePremium_{\tau} \\
&\quad - NonLifeClaims_{\tau} \\
&\quad + InsuranceInvestmentIncome_{\tau} \cdot (1 - LifeReserveShare_{t})
\end{aligned}
$$

Government-bond and equity holdings rebalance gradually toward configured
target shares of total insurance assets. Corporate-bond holdings are not
updated by `Insurance.step`; they are ledger-owned and settled by the
corporate-bond market.

## NBFI And TFI

TFI net inflow is a wage-bill flow modulated by the excess return of a
government-bond, corporate-bond, and equity fund portfolio relative to bank
deposits:

$$
\begin{aligned}
BaseTfiInflow_{\tau} &=  \\
&\quad Employed_{\tau} \cdot Wage_{\tau} \cdot tfiInflowRate \\
 \\
FundReturn_{\tau} &=  \\
&\quad GovBondMarketYield_{\tau} \cdot tfiGovBondShare \\
&\quad + EquityReturnAnnualized_{\tau} \cdot tfiEquityShare \\
&\quad + GovBondMarketYield_{\tau} \cdot tfiCorpBondShare \\
 \\
TfiNetInflow_{\tau} &=  \\
&\quad BaseTfiInflow_{\tau} \\
&\quad \cdot (1 + \mathrm{clamp}(FundReturn_{\tau} - DepositRate_{\tau}, cap) \\
&\quad \cdot excessReturnSensitivity)
\end{aligned}
$$

The TFI inflow signal currently proxies the corporate-bond expected-return leg
with $GovBondMarketYield_{\tau}$, matching `Nbfi.tfiInflow`. Realized TFI investment
income below uses the ledger-owned corporate-bond stock and $CorpBondYield_{\tau}$.

TFI AUM and allocations are:

$$
\begin{aligned}
TfiInvestmentIncome_{\tau} &=  \\
&\quad TfiGovBondHoldings_{t} \cdot GovBondMarketYield_{\tau} / 12 \\
&\quad + TfiCorpBondHoldings_{t} \cdot CorpBondYield_{\tau} / 12 \\
&\quad + TfiEquityHoldings_{t} \cdot EquityReturn_{\tau} \\
&\quad - TfiCorpBondDefaultLoss_{\tau} \\
 \\
TfiAum_{\tau} &=  \\
&\quad \max(TfiAum_{t} + TfiNetInflow_{\tau} + TfiInvestmentIncome_{\tau}, 0)
\end{aligned}
$$

TFI government-bond and equity holdings rebalance toward target shares of AUM.
Corporate-bond holdings are ledger-owned by the corporate-bond market. TFI
inflow is exposed as a banking deposit-drain channel:

$$
\begin{aligned}
NbfiDepositDrain_{\tau} &= -TfiNetInflow_{\tau}
\end{aligned}
$$

NBFI credit uses bank stress as a counter-cyclical origination signal:

$$
\begin{aligned}
BankTightness_{\tau} &=  \\
&\quad \mathrm{clamp}((BankNplRatio_{\tau} - 0.03) / 0.03, 0, 1) \\
 \\
NbfiOrigination_{\tau} &=  \\
&\quad NbfiLoanStock_{t} \cdot creditBaseRate \\
&\quad \cdot (1 + countercyclical \cdot BankTightness_{\tau}) \\
 \\
NbfiRepayment_{\tau} &=  \\
&\quad NbfiLoanStock_{t} / creditMaturity \\
 \\
NbfiDefault_{\tau} &=  \\
&\quad NbfiLoanStock_{t} \cdot defaultBase \\
&\quad \cdot (1 + defaultUnempSensitivity \\
&\quad \cdot \max(UnemploymentRate_{\tau} - 0.05, 0)) \\
 \\
NbfiLoanStock_{\tau} &=  \\
&\quad \max(NbfiLoanStock_{t} \\
&\quad + NbfiOrigination_{\tau} \\
&\quad - NbfiRepayment_{\tau} \\
&\quad - NbfiDefault_{\tau}, \\
&\quad 0)
\end{aligned}
$$

`NbfiNetStockFlow` is
$\mathrm{NbfiOrigination} - \mathrm{NbfiRepayment} - \mathrm{NbfiDefault}$.
`NbfiDepositDrainToAum` diagnoses fund-flow pressure relative to AUM; it is not
a direct term in the NBFI loan-stock identity.

## Quasi-Fiscal BGK/PFR-Style Vehicles

Quasi-fiscal vehicles are consolidated BGK/PFR-style public financial vehicles.
They issue quasi-fiscal bonds and make subsidized loans outside the central
government debt metric, while still contributing to ESA 2010 debt diagnostics.

Monthly issuance is a share of government capital programs:

$$
\begin{aligned}
QfIssuance_{\tau} &=  \\
&\quad \max((GovCapitalSpend_{\tau} + EuProjectCapital_{\tau}) \\
&\quad \cdot qfIssuanceShare, \\
&\quad 0)
\end{aligned}
$$

Bond amortization and absorption are:

$$
\begin{aligned}
QfBondAmortization_{\tau} &=  \\
&\quad QfBondsOutstanding_{t} / qfAvgMaturityMonths \\
 \\
QfNbpAbsorption_{\tau} &=
\begin{cases}
QfIssuance_{\tau} \cdot qfNbpAbsorptionShare, & \text{if NBP QE is active}, \\
0, & \text{otherwise},
\end{cases} \\
 \\
QfBankBondIssuance_{\tau} &=  \\
&\quad QfIssuance_{\tau} - QfNbpAbsorption_{\tau}
\end{aligned}
$$

Amortization is split across bank and NBP holders according to opening holder
shares. The stock update and holder-clearing identity are:

$$
\begin{aligned}
QfBondsOutstanding_{\tau} &=  \\
&\quad \max(QfBondsOutstanding_{t} \\
&\quad + QfIssuance_{\tau} \\
&\quad - QfBondAmortization_{\tau}, \\
&\quad 0) \\
 \\
\mathrm{QfBondClearing}_{\tau} &\Longleftrightarrow
QfBondsOutstanding_{\tau} = QfBankHoldings_{\tau} + QfNbpHoldings_{\tau}
\end{aligned}
$$

Subsidized lending is:

$$
\begin{aligned}
QfLending_{\tau} &=  \\
&\quad QfIssuance_{\tau} \cdot qfLendingShare \\
 \\
QfLoanRepayment_{\tau} &=  \\
&\quad QfLoanPortfolio_{t} / qfLoanMaturityMonths \\
 \\
QfLoanPortfolio_{\tau} &=  \\
&\quad \max(QfLoanPortfolio_{t} \\
&\quad + QfLending_{\tau} \\
&\quad - QfLoanRepayment_{\tau}, \\
&\quad 0)
\end{aligned}
$$

Quasi-fiscal lending has two runtime evidence legs: the quasi-fiscal loan
portfolio movement and the matching bank-deposit creation/destruction side of
the credit channel.

ESA 2010 debt is:

$$
\begin{aligned}
Esa2010Debt_{\tau} &=  \\
&\quad GovDebtMetric_{\tau} + QfBondsOutstanding_{\tau}
\end{aligned}
$$

This is a diagnostic general-government debt bridge, not a separate ledger
instrument.

## Accounting And SFC Mapping

Institutional behavior enters the runtime ledger through typed flow mechanisms:

| Surface | Main flow/SFC interpretation |
| --- | --- |
| taxes | firm/household/banking/equity revenue to Treasury, plus VAT/excise/customs direct Treasury revenue |
| government purchases and transfers | Treasury cash to firms/households/infrastructure |
| debt service | Treasury cash to holder-resolved government-bond owners |
| government bonds and QE | holder-resolved government-bond stock transfers among banks, foreign sector, NBP, insurance, PPK, and TFI |
| ZUS/NFZ/earmarked funds | contributions, benefits/spending, and government subventions through fund cash rows |
| JST | local-government revenue, spending, subvention, deposits, and debt metric |
| NBP reserve/standing-facility/FX plumbing | reserve liability and settlement flows between NBP and banks |
| trade/tourism/remittances/EU funds | foreign-sector cash transfers against domestic firms, households, or government |
| FDI/portfolio/carry/capital flight | foreign capital-account cash transfers with signed direction where applicable |
| insurance | premiums, claims, investment income/loss, reserves, and portfolio holdings |
| NBFI/TFI | fund-unit/AUM flows, non-bank loan origination/repayment/default, and deposit-drain diagnostics |
| quasi-fiscal vehicles | quasi-fiscal bond issuance/amortization/absorption and subsidized loan/deposit legs |

The SFC semantic projection separately validates key public and non-bank
identities, including:

$$
\begin{aligned}
JstDepositsDelta &=  \\
&\quad JstOwnRevenue + JstGovSubvention - JstSpending \\
 \\
QfBondStockDelta &=  \\
&\quad QuasiFiscalBankBondIssuance \\
&\quad + QuasiFiscalNbpAbsorption \\
&\quad - QuasiFiscalBankBondAmortization \\
&\quad - QuasiFiscalNbpBondAmortization \\
 \\
QfLoanPortfolioDelta &=  \\
&\quad QfLending - QfRepayment \\
 \\
NbfiLoanStockDelta &=  \\
&\quad NbfiOrigination - NbfiRepayment - NbfiDefault
\end{aligned}
$$

`QuasiFiscalBankBondIssuance` and `QuasiFiscalBankBondAmortization` denote the
commercial-bank holder legs emitted as `FlowMechanism.QuasiFiscalBondIssuance`
and `FlowMechanism.QuasiFiscalBondAmortization`. The SFC projection aggregates
those bank legs with the separately addressable NBP absorption/amortization
legs when it computes total quasi-fiscal issuance and amortization.

Public and institutional outputs should be interpreted together with the
generated SFC artifacts, especially
[flow-mechanism-semantics.md](sfc-matrix-artifacts/flow-mechanism-semantics.md)
and
[stock-flow-reconciliation.md](sfc-matrix-artifacts/stock-flow-reconciliation.md).

## Output And Diagnostic Surfaces

Representative output columns include:

| Surface | Columns |
| --- | --- |
| Fiscal stance | `GovCurrentSpend`, `GovCapitalSpendDomestic`, `DebtService`, `GovPrimaryDeficitToGdp`, `DebtToGdp`, `DeficitToGdp`, `FiscalRuleBinding`, `GovSpendingCutRatio`, `PublicCapitalStock` |
| Government bonds and NBP | `RefRate`, `GovBondMarketYield`, `GovDebtWeightedCoupon`, `BondsOutstanding`, `ForeignBondHoldings`, `NbpBondHoldings`, `QeActive`, `NbpRemittance`, `NbpBondIncome`, `FxReserves`, `FxInterventionAmt` |
| Social funds and JST | `ZusContributions`, `ZusPensionPayments`, `ZusGovSubvention`, `NfzContributions`, `NfzSpending`, `NfzBalance`, `NfzGovSubvention`, `PpkContributions`, `PpkBondHoldings`, `FpBalance`, `FpContributions`, `PfronBalance`, `FgspBalance`, `FgspSpending`, `JstRevenue`, `JstSpending`, `JstDebt`, `JstDeposits`, `JstDeficit` |
| External sector | `NFA`, `CurrentAccount`, `CurrentAccountToGdp`, `CurrentAccountPrimaryIncome`, `CurrentAccountSecondaryIncome`, `CurrentAccountClosureResidual`, `CapitalAccount`, `CapitalAccountToGdp`, `TradeBalance_OE`, `Exports_OE`, `TotalImports_OE`, `FDI`, `RemittanceOutflow`, `DiasporaRemittanceInflow`, `NetRemittances`, `TourismExport`, `TourismImport`, `NetTourismBalance`, `TourismSeasonalFactor` |
| Insurance | `InsLifeReserves`, `InsNonLifeReserves`, `InsGovBondHoldings`, `InsLifePremium`, `InsNonLifePremium`, `InsLifeClaims`, `InsNonLifeClaims` |
| NBFI/TFI | `NbfiTfiAum`, `NbfiTfiGovBondHoldings`, `NbfiLoanStock`, `NbfiOrigination`, `NbfiRepayment`, `NbfiDefaults`, `NbfiNetStockFlow`, `NbfiOriginationToStock`, `NbfiRepaymentToStock`, `NbfiDefaultsToStock`, `NbfiBankTightness`, `NbfiDepositDrain`, `NbfiDepositDrainToAum`, `NbfiLoansToGdp` |
| Quasi-fiscal | `QfBondsOutstanding`, `QfNbpHoldings`, `QfLoanPortfolio`, `QfIssuance`, `Esa2010DebtToGdp` |

`NbpRemittance` reports the net fiscal transfer to the budget:
$\mathrm{NbpBondIncome} - \mathrm{ReserveInterest} - \mathrm{StandingFacilityNet}$.
`NbpBondIncome`
reports the gross NBP government-bond coupon income before reserve and
standing-facility costs.

## Validation Coverage

| Layer | Coverage |
| --- | --- |
| Unit/config tests | Fiscal, monetary, social-fund, NBP, open-economy, insurance, NBFI, quasi-fiscal, calibration, and parameter-scaling specs. |
| Flow tests | `GovBudgetFlows`, `ZusFlows`, `NfzFlows`, `PpkFlows`, `EarmarkedFlows`, `JstFlows`, `OpenEconFlows`, `InsuranceFlows`, `NbfiFlows`, `QuasiFiscalFlows`, `GovBondFlows`, and composite flow conservation specs. |
| Integration/nightly | Monte Carlo TSV integration, nightly diagnostics, scenario runs, sensitivity/robustness profiles, generated SFC matrix artifacts, and generated-output guard. |
| Accounting | Runtime ledger execution, SFC semantic projection, generated BSM/TFM artifacts, stock-flow reconciliation, and institutional row-to-flow identity checks. |

## Current Limitations

- Central government, NBP, JST, social funds, insurance, NBFI, quasi-fiscal
  vehicles, and rest-of-world are institutional aggregate agents, not
  heterogeneous micro-institution populations.
- `GovState.cumulativeDebt`, `Jst.State.debt`, `BopState`, and
  `Nbp.qeCumulative` are fiscal or macro memory surfaces. They should not be
  read as independent holder-resolved ledger instruments.
- The fiscal-rule implementation is a monthly stabilizing path. It is not a
  full legal budget-process model and does not represent political approval,
  intra-year budget amendments, or cash/accrual reconciliation in detail.
- The NBP rule is a Taylor-type policy heuristic with a QE lower-bound branch
  and FX-intervention band. It is not an estimated reaction function.
- External-sector behavior combines structural trade, GVC, remittance,
  tourism, FDI, portfolio, carry-trade, and capital-flight channels. Several
  coefficients remain calibration or scenario targets rather than final
  econometric estimates.
- Insurance and NBFI are aggregate balance-sheet agents. Policyholder,
  beneficiary, fund-investor, leasing-contract, and individual-insurance
  contract heterogeneity is not yet represented.
- Quasi-fiscal vehicles are represented as one consolidated BGK/PFR-style
  sector. Instrument-level program detail, guarantee accounting, and ESA
  reclassification uncertainty remain outside the current executable model.
- Some institutional stocks are diagnostic-only or unsupported as transferable
  ledger-owned instruments. Publication use should cite the ownership boundary
  above and the generated SFC artifacts rather than inferring unsupported
  counterparties.
