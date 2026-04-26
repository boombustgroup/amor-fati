# Empirical Validation Report Skeleton

This report defines how Amor Fati outputs should be compared with empirical
macro, meso, and micro stylized facts. It is deliberately a skeleton: the
model already writes numeric simulation evidence, but the external data bridge
and publication-grade source tables are still tracked separately in #437.

The goal is to keep failed, missing, or weak validation targets visible. A row
with `MISSING_OUTPUT`, `MISSING_DATA_BRIDGE`, or `NOT_RUN` is part of the report
surface, not an omission.

## Validation Boundary

Accounting validation and empirical validation answer different questions:

| Surface | Question | Artifact |
| --- | --- | --- |
| Runtime ledger checks | Did emitted monetary flows conserve value through the ledger? | Engine runtime checks and SFC identity validation |
| Paper-facing accounting structure | Are Balance Sheet Matrix and Transactions Flow Matrix rows documented and traceable to runtime mechanisms? | `docs/sfc-matrix-evidence.md` |
| Empirical validation | Do simulated paths reproduce empirical Polish/EU macro, meso, and micro stylized facts? | This report |
| Revaluation and other-change accounting | Do stocks reconcile with transactions plus independent revaluations or other changes? | Follow-up #436 |

Empirical validation must not be used to relax accounting constraints. A model
run can fit an empirical target and still be invalid if the ledger or SFC
checks fail.

## Status Taxonomy

| Status | Meaning |
| --- | --- |
| `READY_FOR_BASELINE` | Output columns exist and the empirical target family is identified. |
| `PARTIAL_OUTPUT` | Output exists but requires a derived metric, normalization, or additional aggregation. |
| `MISSING_DATA_BRIDGE` | Model output exists, but external source extraction/transformation is not documented yet. |
| `MISSING_OUTPUT` | Target is known, but the Monte Carlo output schema does not yet expose enough model state. |
| `ACCOUNTING_ONLY` | This belongs to accounting validation, not empirical fit. |
| `NOT_RUN` | The baseline validation run has not yet been summarized in this report. |

## Reproducible Workflow

Run a small deterministic baseline Monte Carlo batch:

```bash
sbt "run 3 validation-baseline --duration 120 --run-id validation-baseline"
```

Expected output files:

```text
mc/validation-baseline_validation-baseline_120m_seed001.csv
mc/validation-baseline_validation-baseline_120m_seed002.csv
mc/validation-baseline_validation-baseline_120m_seed003.csv
mc/validation-baseline_validation-baseline_120m_hh.csv
mc/validation-baseline_validation-baseline_120m_banks.csv
```

Use the per-seed CSV files for monthly macro, meso, financial, and mechanism
paths. Use the `_hh.csv` and `_banks.csv` files for terminal household and bank
cross-section summaries.

Manual update procedure until #437 adds a formal data bridge:

1. Run the command above from the repository root.
2. Record commit hash, run id, duration, seed count, and parameter branch.
3. For each target row below, compute the model statistic from the mapped CSV
   column or terminal summary field.
4. Fill empirical source, vintage, target value, model value, tolerance, and
   status in the report snapshot.
5. Keep rows with missing model output or missing empirical data in the table.

## Output Mapping

The primary numeric surface is
`src/main/scala/com/boombustgroup/amorfati/montecarlo/McTimeseriesSchema.scala`.
Terminal cross-sections come from
`src/main/scala/com/boombustgroup/amorfati/montecarlo/McTerminalSummarySchema.scala`.

| Validation target | Empirical comparator | Model output mapping | Suggested statistic | Current status |
| --- | --- | --- | --- | --- |
| GDP growth | GUS national accounts real GDP growth | Engine state `world.cachedMonthlyGdpProxy`; no direct CSV column yet. `GovDebt` and `DebtToGdp` can imply annualized GDP only when both are valid. | Annual or quarterly growth of real/price-adjusted GDP proxy | `MISSING_OUTPUT` |
| Inflation | GUS CPI / HICP, NBP inflation target | `Inflation`, `PriceLevel`, `ExpectedInflation`, `InflationForecastError` | Mean, volatility, target deviation, persistence | `READY_FOR_BASELINE` |
| Unemployment | GUS BAEL / registered unemployment | `Unemployment`, `Unemp_Central`, `Unemp_South`, `Unemp_East`, `Unemp_Northwest`, `Unemp_Southwest`, `Unemp_North` | Mean level, volatility, regional dispersion | `READY_FOR_BASELINE` |
| Wages | GUS average wage, sector wage indices | `MarketWage`, `MinWageLevel`; terminal household income distribution not directly emitted | Mean wage level and growth; minimum/market wage ratio | `PARTIAL_OUTPUT` |
| Credit/GDP | NBP credit aggregates to GDP | `CreditToGdpGap`, `ConsumerLoans`, `MortgageToGdp`, `MortgageStock`, `NbfiLoanStock`; terminal `_banks.csv` field `Loans` | Credit/GDP level, gap, household/firm split | `PARTIAL_OUTPUT` |
| Public debt/GDP | MF public debt, ESA2010 general-government debt | `DebtToGdp`, `Esa2010DebtToGdp`, `GovDebt`, `QfBondsOutstanding`, `BondsOutstanding` | Terminal debt/GDP and path against thresholds | `READY_FOR_BASELINE` |
| Current account | NBP balance of payments | `CurrentAccount`, `TradeBalance_OE`, `Exports_OE`, `TotalImports_OE`, `NetRemittances`, `NetTourismBalance`, `FDI` | Annualized current-account/GDP and component signs | `PARTIAL_OUTPUT` |
| Firm-size distribution | GUS/REGON firm-size distribution | Initialization config `PopulationConfig`; no monthly or terminal firm-size histogram CSV yet | Init distribution and survival-weighted terminal distribution | `MISSING_OUTPUT` |
| Bankruptcies | GUS / Ministry of Justice corporate insolvencies, consumer bankruptcy statistics | `FirmDeaths`, `FirmBirths`, `NetEntry`, `BankFailures`; terminal `_hh.csv` fields `HH_Bankrupt`, `BankruptcyRate`, `MeanMonthsToRuin` | Firm exit rate, household bankruptcy rate, bank failures | `PARTIAL_OUTPUT` |
| Bank capital and liquidity | KNF banking-sector CAR, LCR, NSFR, NPL | `MinBankCAR`, `MinBankLCR`, `MinBankNSFR`, `NPL`, `MaxBankNPL`; terminal `_banks.csv` fields `CAR`, `NPL`, `Capital`, `Deposits`, `Loans` | Minimum and distributional stress indicators | `READY_FOR_BASELINE` |
| Inequality | GUS household surveys, EU-SILC, OECD income/wealth indicators | Terminal `_hh.csv` fields `Gini_Individual`, `Gini_Wealth`, `ConsumptionP10`, `ConsumptionP50`, `ConsumptionP90`, `PovertyRate_50pct`, `PovertyRate_30pct` | Terminal Gini, poverty rates, consumption percentile ratios | `MISSING_DATA_BRIDGE` |
| Sectoral output | GUS national accounts by sector, supply-use tables | Sector columns currently expose `BPO_Auto`, `Manuf_Auto`, `Retail_Auto`, `Health_Auto`, `Public_Auto`, `Agri_Auto`, sector sigmas, and `IoFlows`; direct sector output is not emitted | Sector output shares and growth | `MISSING_OUTPUT` |
| External prices and FX | NBP exchange rate, ECB/Eurostat external prices | `ExRate`, `ForeignPriceIndex`, `GvcImportCostIndex`, `CommodityPriceIndex`, `FxReserves`, `FxInterventionAmt` | FX level/volatility, reserve path, import-cost shocks | `READY_FOR_BASELINE` |
| Housing and mortgages | NBP housing prices, mortgage stock, KNF mortgage risk | `HousingPriceIndex`, `WawHpi`, `KrkHpi`, `WroHpi`, `GdnHpi`, `LdzHpi`, `PozHpi`, `RestHpi`, `MortgageToGdp`, `MortgageDefault` | HPI path, regional dispersion, mortgage/GDP, defaults | `READY_FOR_BASELINE` |
| Fiscal stance | MF budget execution, Eurostat deficit/GDP | `DeficitToGdp`, `GovCurrentSpend`, `GovCapitalSpendDomestic`, `DebtService`, `FiscalRuleBinding`, `GovSpendingCutRatio` | Deficit/GDP, expenditure mix, fiscal-rule episodes | `READY_FOR_BASELINE` |
| Monetary and financial market conditions | NBP reference rate, WIBOR, bond yields, GPW | `RefRate`, `WIBOR_1M`, `WIBOR_3M`, `WIBOR_6M`, `BondYield`, `GpwIndex`, `GpwMarketCap`, `CorpBondYield`, `CorpBondSpread` | Policy-rate path, spread behavior, market stress | `READY_FOR_BASELINE` |

## Baseline Report Snapshot

This table is the publication-facing slot that should be filled after a
baseline run. Placeholder values remain explicit until the empirical data
bridge and baseline analysis have been completed.

| Target | Empirical source and vintage | Empirical value | Model run | Model value | Tolerance / criterion | Status | Notes |
| --- | --- | --- | --- | --- | --- | --- | --- |
| GDP growth | TBD via #437 | TBD | `validation-baseline` | TBD | TBD | `NOT_RUN` | Direct GDP output column still needed. |
| Inflation | TBD via #437 | TBD | `validation-baseline` | TBD | TBD | `NOT_RUN` | Use `Inflation` and `PriceLevel`. |
| Unemployment | TBD via #437 | TBD | `validation-baseline` | TBD | TBD | `NOT_RUN` | Include regional dispersion. |
| Wages | TBD via #437 | TBD | `validation-baseline` | TBD | TBD | `NOT_RUN` | Current output is aggregate market wage. |
| Credit/GDP | TBD via #437 | TBD | `validation-baseline` | TBD | TBD | `NOT_RUN` | Firm-loan split depends partly on terminal bank summary. |
| Public debt/GDP | TBD via #437 | TBD | `validation-baseline` | TBD | TBD | `NOT_RUN` | Compare `DebtToGdp` and `Esa2010DebtToGdp`. |
| Current account | TBD via #437 | TBD | `validation-baseline` | TBD | TBD | `NOT_RUN` | Needs GDP denominator and annualization convention. |
| Firm-size distribution | TBD via #437 | TBD | `validation-baseline` | TBD | TBD | `NOT_RUN` | Terminal firm-size histogram not emitted yet. |
| Bankruptcies | TBD via #437 | TBD | `validation-baseline` | TBD | TBD | `NOT_RUN` | Use firm deaths and household bankruptcy separately. |
| Bank capital/liquidity | TBD via #437 | TBD | `validation-baseline` | TBD | TBD | `NOT_RUN` | Use minima and terminal bank distribution. |
| Inequality | TBD via #437 | TBD | `validation-baseline` | TBD | TBD | `NOT_RUN` | Terminal household summary has first-pass measures. |
| Sectoral output | TBD via #437 | TBD | `validation-baseline` | TBD | TBD | `NOT_RUN` | Direct sector output columns still needed. |

## Target-Specific Notes

GDP growth should be reported only after the model exposes a direct GDP or
monthly GDP proxy column. The engine already computes `world.cachedMonthlyGdpProxy`,
but relying on `GovDebt / DebtToGdp` as a CSV reconstruction is too fragile for
publication use.

Inflation validation should use both the period inflation column and the price
level path. The report should state whether the statistic is monthly,
annualized, or year-over-year.

Credit/GDP should distinguish bank firm loans, consumer loans, mortgage credit,
and NBFI credit. Current outputs cover several pieces but do not yet provide a
single total-credit-to-GDP series.

Firm-size distribution and sectoral output are currently the most visible
meso-level output gaps. The model has sector metadata, firm state, and
population initialization, but the Monte Carlo CSV does not yet emit a terminal
firm-size histogram or sector output shares.

Inequality validation is terminal-only for now. The household summary already
emits Gini, poverty, and consumption percentile fields, but external source
mapping and frequency conventions belong in #437.

## Follow-Up Links

- #437: external data bridge, source vintages, licenses, transformations, and
  empirical target values.
- `docs/sensitivity-robustness-workflow.md`: stochastic uncertainty, parameter
  sensitivity, confidence envelopes, and robustness metrics around this
  baseline report.
- `docs/scenario-registry.md`: named scenarios whose outputs should reuse the
  same validation target table.
- #436: stock-flow reconciliation and revaluation artifact; keep this separate
  from empirical fit.
- #438: milestone tracker for the full research-readiness spine.
