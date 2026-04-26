# Data Bridge To National And Financial Accounts

This bridge defines how empirical Polish, EU, and financial-account datasets
should map into Amor Fati initialization stocks, target flows, calibrated
parameters, scenario inputs, and validation targets.

It is a documentation contract, not a vendored data extract. Future extraction
scripts should use this file as the reference for source selection,
transformation rules, vintages, and model ownership. The calibration register
documents current parameter provenance; the empirical validation report
documents model-output comparisons. This bridge connects both to external data.

## Boundary And Data Roles

| Role | Meaning | Examples | Output contract |
| --- | --- | --- | --- |
| Initialization data | External data used to build the opening model state. | Initial deposits, loans, government debt, FX reserves, firm-size distribution. | Feed `SimParams.defaults`, `WorldInit.initialize`, and ledger-owned opening stocks. |
| Calibration target | External statistic used to set or constrain a parameter. | Wage level, MPC distribution, spreads, entry rates, recovery rates. | Referenced from `docs/calibration-register.md`. |
| Scenario input | External path or policy assumption used to define a named experiment. | Energy prices, policy-rate paths, EU funds absorption, fiscal shocks. | Referenced from `docs/scenario-registry.md`. |
| Validation target | External statistic used after a run to assess fit. | GDP growth, inflation, unemployment, debt/GDP, current account, CAR/LCR/NSFR. | Referenced from `docs/empirical-validation-report.md`. |

Raw source data should remain outside committed source code unless there is an
explicit reason to vendor a small static fixture. Derived extracts should record
source URL, dataset/table code, vintage, extraction date, license/reuse note,
transformation code or command, and target model field.

## Source Priority

1. Official Polish source for Polish institutional data: Statistics Poland
   (GUS), Narodowy Bank Polski (NBP), Ministry of Finance (MF), Polish
   Financial Supervision Authority (KNF), ZUS, and other public agencies.
2. Eurostat and ECB sources for harmonized EU definitions, ESA 2010 financial
   accounts, and cross-country comparators.
3. International public sources only when the Polish or EU source is missing.
4. Proprietary or commercial data only as documented optional inputs, never as
   an untraceable baseline dependency.

## Source Register

| Provider | Bridge use | Access and cadence | Reuse note | Primary links |
| --- | --- | --- | --- | --- |
| Statistics Poland / GUS | National accounts, CPI, labor market, firms, households, regional data. | Web releases, BDL/SWAID databases, REST API. Cadence depends on dataset: monthly, quarterly, annual. | Public statistics; cite source and dataset/table. Verify dataset-specific notices before publication. | [BDL API](https://api.stat.gov.pl/Home/BdlApi?lang=en), [National accounts](https://stat.gov.pl/en/topics/national-accounts/), [Labour market](https://stat.gov.pl/en/topics/labour-market/), [CPI](https://stat.gov.pl/en/topics/prices-trade/price-indices/price-indices-of-consumer-goods-and-services/), [Living conditions](https://stat.gov.pl/en/topics/living-conditions/living-conditions/) |
| Eurostat | Harmonized EU national accounts, government finance, ESA 2010 financial accounts, HICP, EU-SILC comparators. | Data Browser and REST API. Cadence follows each dataset's release schedule. | Eurostat statistical data and metadata are reusable with source acknowledgement unless a dataset notice says otherwise. | [API guide](https://ec.europa.eu/eurostat/web/user-guides/data-browser/api-data-access/api-introduction), [reuse policy](https://ec.europa.eu/eurostat/help/copyright-notice), [quarterly GDP `namq_10_gdp`](https://ec.europa.eu/eurostat/cache/metadata/en/namq_10_gdp_esms.htm), [quarterly financial flows and stocks `nasq_10_f`](https://ec.europa.eu/eurostat/cache/metadata/en/nasq_10_f_esms.htm), [annual financial flows and stocks `nasa_10_f`](https://ec.europa.eu/eurostat/cache/metadata/en/nasa_10_f_esms.htm), [government finance](https://ec.europa.eu/eurostat/web/government-finance-statistics/information-data) |
| Narodowy Bank Polski / NBP | Monetary and financial statistics, NBP balance sheet, MFI balance sheet, interest rates, FX, balance of payments, IIP, household wealth survey. | NBP statistics pages, downloadable time series, NBP Web API for exchange rates and gold prices. Daily, monthly, quarterly, and annual cadences. | Official NBP publications and statistics; cite source, table, vintage, and access date. | [Statistics](https://nbp.pl/en/statistic-and-financial-reporting/), [MFI balance sheet](https://nbp.pl/en/statistic-and-financial-reporting/monetary-and-financial-statistics/consolidated-balance-sheet-of-mfis/), [MIR interest rates](https://nbp.pl/en/statistic-and-financial-reporting/monetary-and-financial-statistics/mir-statistics/), [balance of payments](https://nbp.pl/en/statistic-and-financial-reporting/balance-of-payments-statistics/balance-of-payments/), [NBP API](https://api.nbp.pl/en.html), [household wealth survey](https://nbp.pl/publikacje/integracja-europejska/publikacje-ebc/badanie-zasobnosci-gospodarstw-domowych/) |
| KNF | Banking-sector supervisory aggregates: capital, liquidity, NPLs, mortgages, sector structure. | Monthly and annual banking-sector data files. | Official supervisory data; cite file month and publication date. | [Monthly banking data](https://www.knf.gov.pl/en/REPORTS_AND_ANALYSIS/Banking/Monthly_data) |
| Ministry of Finance / MF | State budget execution, public debt, State Treasury debt, fiscal time series, SPW holder structure. | Monthly state-budget execution, monthly State Treasury debt, quarterly public-debt reports, fiscal time-series workbooks. | Official public-finance data; cite report and vintage. | [State budget](https://www.gov.pl/web/finance/state-budget), [revenue/expenditure execution](https://www.gov.pl/web/finance/revenue-expenditure-execution), [public debt](https://www.gov.pl/web/finance/public-debt), [public-finance time series](https://www.gov.pl/web/finanse/szeregi-czasowe--finanse-publiczne) |
| ZUS | Social-security contributions, pensions, FUS benefits, insured persons, beneficiaries. | ZUS statistical portal and periodic publications. Monthly, quarterly, annual, and projection cadences. | Official ZUS statistics; cite publication/table and access date. | [ZUS statistics](https://www.zus.pl/baza-wiedzy/statystyka), [ZUS statistical portal](https://psz.zus.pl/), [FUS information](https://www.zus.pl/baza-wiedzy/statystyka/wazniejsze-informacje-z-zakresu-ubezpieczen-spolecznych-fundusz-ubezpieczen-spolecznych) |
| ECB Data Portal | EU monetary, market-rate, banking, non-bank financial, and euro-area comparator series. | Data portal and SDMX REST API. Dataset cadence depends on dataflow; API supports production and historical-version retrieval where available. | Official ECB statistics; cite dataflow, series key, vintage, and access date. | [Data portal](https://data.ecb.europa.eu/), [API overview](https://data.ecb.europa.eu/help/api), [API data queries](https://data.ecb.europa.eu/help/api/data) |

## Transformation Rules

| Source shape | Runtime rule | Notes |
| --- | --- | --- |
| Annual flow | Divide by 12 for a flat monthly baseline unless the source provides a seasonal path or the model scenario intentionally uses a different absorption profile. | Use for government spending, transfers, broad national-account flows, and EU funds when no monthly path exists. |
| Quarterly flow | Distribute across the three months of the quarter. For validation, compare quarterly aggregates when monthly interpolation would create false precision. | Use for GDP, financial transactions, and quarterly fiscal or external accounts. |
| Monthly flow | Use directly after unit conversion and sign normalization. | Preferred for budget execution, CPI, wages, banking flows, and balance-of-payments monthly estimates. |
| Daily or market rate | Use month-end for stock/rate state variables and monthly average for flow pricing unless the consuming model rule states otherwise. | Applies to FX, policy rates, WIBOR-like rates, bond yields, equity prices, and commodity prices. |
| Stock or balance sheet | Use the observation date as the opening or month-end state. Do not divide by months. | Applies to loans, deposits, debt, FX reserves, MFI balance sheets, and ESA financial balance sheets. |
| Price index | Rebase to the model's initial `PriceLevel` or `PriceIndex.Base` convention before comparing growth. | State whether validation uses month-over-month, annualized, or year-over-year inflation. |
| Nominal PLN macro stock or flow | Keep raw PLN in the bridge, then apply the model's `gdpRatio` only when the target config field is a macro stock or monthly macro flow. | Agent-level values such as wages, rents, and per-worker revenue are not scaled by `gdpRatio`. |
| EUR/USD or foreign-currency value | Convert to PLN using NBP exchange rates: monthly average for flows, month-end rate for stocks. | Keep the original currency and FX conversion series in the extraction manifest. |
| ESA 2010 sector/instrument | Map through the sector and instrument crosswalk below. | Any many-to-one mapping must be tagged `BRIDGE_ASSUMPTION` until it has a paper note. |
| Survey distribution | Fit explicit distribution parameters and record survey weights, filters, and winsorization/trimming. | Applies to MPC, savings, wealth, debt, income, poverty, and consumption-percentile targets. |

## Source-To-Model Mapping

| Domain | Source dataset family | Model use | Runtime mapping | Transformation | Current gap and priority |
| --- | --- | --- | --- | --- | --- |
| National accounts | GUS national accounts; Eurostat `namq_10_gdp`, `namq_10_a10`, supply-use and input-output tables. | Initialization scale, GDP denominator, sector shares, validation targets. | `pop.realGdp`, `gdpRatio`, `sectorDefs.share`, GDP growth validation, sectoral output validation. | Quarterly GDP to monthly runtime denominator; annual sector shares to sector initialization; real/nominal split documented per target. | Direct GDP CSV output and direct sector-output columns are still missing. Priority: high. |
| Financial accounts | Eurostat `nasq_10_f` / `nasa_10_f`; NBP financial-account and MFI balance-sheet statistics. | Opening Balance Sheet Matrix stocks, financial stock validation, stock-flow reconciliation comparators. | Deposits, loans, bonds, equity, fund units, insurance reserves, government/NBP holdings, household assets/liabilities. | ESA sectors and AF instruments mapped to runtime sectors and `AssetType`; stocks use date-specific balance sheets, transactions use monthly/quarterly flow allocation. | Holder-resolved equity, mortgage counterpart splits, and independent revaluation/other-change channels need explicit extraction. Priority: high. |
| Fiscal accounts | MF state budget execution, public debt, State Treasury debt and holder structure; Eurostat government finance statistics. | Fiscal spending, taxes, debt, maturity, local-government and social-fund targets. | `FiscalConfig`, `GovDebt`, `DebtToGdp`, `Esa2010DebtToGdp`, `GovCurrentSpend`, `GovCapitalSpendDomestic`, SPW holdings. | Monthly cash execution mapped to runtime flows; ESA deficit/debt used for validation; stock debt uses month/quarter-end values. | Effective VAT/excise by model sector and cash-vs-accrual bridge are incomplete. Priority: high. |
| Banking and credit | NBP MFI balance sheet, MIR interest rates, KNF monthly banking data. | Bank opening stocks, credit calibration, prudential validation, spread calibration. | `banking.initDeposits`, `initLoans`, `initCapital`, `initGovBonds`, `baseSpread`, `MinBankCAR`, `MinBankLCR`, `MinBankNSFR`, `NPL`. | Monthly stocks to opening/month-end values; regulatory ratios direct; MIR rates converted to annual/monthly conventions used by the model. | Maturity distributions, recovery rates, and bank-level heterogeneity need better empirical backing. Priority: high. |
| Labor market and wages | GUS LFS/BAEL, registered unemployment, employment, wage and salary series; Eurostat LFS comparators. | Wage calibration, unemployment validation, labor mobility, sector composition. | `household.baseWage`, `labor.frictionMatrix`, `labor.unionDensity`, unemployment columns, regional unemployment columns, `MarketWage`. | Monthly wage series direct; quarterly LFS held or compared quarterly; sector wage indices mapped to six model sectors. | Cross-sector transition/friction matrix and terminal wage distribution output need stronger support. Priority: high. |
| Firms and production | GUS enterprise demography, REGON/BDL, F-01/non-financial enterprise results, supply-use tables. | Firm-size distribution, entry/exit, revenue/costs, capital ratios, sector productivity. | `pop.firmSizeDist`, `pop.firmSize*Share`, `firm.entryRate`, `firm.baseRevenue`, `capital.klRatios`, `capital.depRates`, `FirmDeaths`, `FirmBirths`. | Annual/quarterly firm results mapped to six sectors; per-worker and per-firm normalizers recorded explicitly. | Terminal firm-size histogram and direct sector output are missing from Monte Carlo outputs. Priority: high. |
| Households and inequality | GUS household budget survey, EU-SILC, NBP household wealth survey. | MPC, savings, debt, wealth, consumption and poverty validation. | `household.mpc*`, `household.savings*`, `household.debt*`, terminal household Gini/poverty/percentile fields. | Survey microdata/distributions fitted to model parameters with documented weights and filters; annual survey targets compared to terminal or annualized summaries. | Microdata extraction, wealth distribution fitting, and disposable-income mapping are not yet implemented. Priority: high. |
| Prices and inflation | GUS CPI, HICP; Eurostat HICP; NBP inflation target and expectations/forecast material. | Inflation calibration and validation, price-level normalization, expectations checks. | `PriceLevel`, `Inflation`, `ExpectedInflation`, `InflationForecastError`, `monetary.targetInfl`. | CPI/HICP indexes rebased to model start; report monthly, annualized, or year-over-year statistic explicitly. | Sector CPI deflators and expectation-survey mapping remain partial. Priority: medium. |
| External sector | NBP balance of payments and IIP, NBP exchange rates, Eurostat/ECB BoP and IIP comparators. | Exports, imports, FDI, portfolio flows, reserves, FX, current account. | `openEcon.exportBase`, `openEcon.euTransfers`, `openEcon.fdiBase`, `CurrentAccount`, `TradeBalance_OE`, `ExRate`, `FxReserves`, `FDI`. | Monthly BoP direct where available; quarterly/annual BoP allocated or compared at native frequency; FX average for flows and month-end for stocks. | IIP holder mapping and sector-specific export/import bridges remain incomplete. Priority: medium. |
| Housing and mortgages | NBP housing prices, KNF mortgage data, GUS living-condition and housing tables. | Housing price validation, mortgage stock/defaults, household balance-sheet stress. | `HousingPriceIndex`, regional HPI columns, `MortgageStock`, `MortgageToGdp`, `MortgageDefault`. | Monthly/quarterly HPI rebased to model start; mortgage stocks are month-end; defaults use native cadence where available. | Regional mapping and borrower-distribution calibration need more detail. Priority: medium. |
| Social insurance and public services | ZUS statistics, ZUS portal, FUS reports, MF public-finance time series, NFZ/MF budget lines. | Contributions, pensions, benefits, health and social-fund flows. | `SocialConfig`, ZUS/NFZ contribution rates, pensions, social benefits, FUS/NFZ flow validation. | Legal rates direct; flow amounts monthly or annual/12 depending source; beneficiaries and insured persons used as level targets. | Benefit eligibility distributions and public-service sector mapping are partial. Priority: medium. |
| Financial markets and non-bank finance | NBP/ECB rates, KNF capital-market and pension-fund statistics, GPW or official market data where available. | Corporate bonds, funds, insurance, pension funds, equity market, stress channels. | `BondYield`, `CorpBondYield`, `CorpBondSpread`, `GpwIndex`, `GpwMarketCap`, fund/insurance holdings and flows. | Market prices month-end or monthly average depending state/flow semantics; fund and insurance stocks mapped to ESA S.128/S.129. | Official GPW data and non-bank holder splits need a stable open-source path. Priority: medium. |
| Scenario-specific shocks | EU ETS/energy data, tourism statistics, EU funds absorption, policy announcements, official projections. | Named scenarios and sensitivity inputs. | `climate.*`, `tourism.*`, `fiscal.eu*`, `scenarioRun` deltas. | Baseline calibration stays empirical; scenario magnitudes must state whether they are historical analogues, official projections, or policy counterfactuals. | Scenario provenance notes should be added per scenario. Priority: medium. |

## Sector Crosswalk

| Runtime sector | Primary ESA / institutional mapping | Bridge note |
| --- | --- | --- |
| Households | S.14 households, optionally S.15 NPISH when the source aggregates them. | Mark S.14+S.15 combinations when Eurostat or GUS publishes them together. |
| Firms | S.11 non-financial corporations plus household market producers when needed. | Household entrepreneurs are a bridge assumption unless separately measured. |
| Banks | S.122 deposit-taking corporations except the central bank. | For broad MFI data, remove or separately identify NBP where possible. |
| NBP | S.121 central bank. | Central-bank balance sheet and reserves come primarily from NBP sources. |
| Government | S.13 general government and subsectors S.1311/S.1313/S.1314 when needed. | MF cash data and ESA accrual data must not be mixed without a stated bridge. |
| Insurance | S.128 insurance corporations. | Often reported with pension funds or other financial corporations; document splits. |
| Funds | S.124 non-MMF investment funds, S.125 other financial intermediaries, S.129 pension funds as needed. | The current runtime sector aggregates several non-bank financial roles. |
| Rest of world | S.2 rest of the world. | Used for trade, BoP/IIP, FDI, remittances, foreign bond/equity holdings. |

## Production-Sector Crosswalk

| Runtime production sector | Candidate NACE Rev. 2 mapping | Bridge note |
| --- | --- | --- |
| BPO/SSC | J, M, N, selected K support services. | Requires explicit service-export and business-services bridge. |
| Manufacturing | C. | Direct high-level mapping. |
| Retail/Services | G, H, I, L, selected M/N/R/S. | Broad residual services bucket; document included sections per extraction. |
| Healthcare | Q. | Public/private split should be recorded when used for fiscal or employment targets. |
| Public | O and P, plus public components of other sections when data allow. | Public administration and education are direct; public health belongs to healthcare unless stated otherwise. |
| Agriculture | A. | Direct high-level mapping. |

## Financial-Instrument Crosswalk

| Runtime instrument / asset type | ESA 2010 financial instrument | Bridge note |
| --- | --- | --- |
| Cash | AF.21 currency. | NBP/MFI currency in circulation can be used for aggregate validation. |
| Deposits | AF.22 transferable deposits and AF.29 other deposits. | Split demand/term deposits when source allows; otherwise aggregate. |
| Loans | AF.4 loans. | Consumer, mortgage, firm, interbank, and public-sector loans should be tagged separately. |
| Reserves | AF.22 deposits with the central bank or central-bank reserve balances. | Treat as bank asset and NBP liability. |
| Government bonds | AF.3 debt securities issued by S.13 or State Treasury instruments. | Holder structure comes from MF/NBP where available. |
| Corporate bonds | AF.3 debt securities issued by firms. | Needs issuer/holder split for SFC matrices. |
| Equity | AF.5 equity and investment fund shares/units as applicable. | Listed equity market and unlisted equity need separate valuation notes. |
| Fund units | AF.52 investment fund shares/units. | Map to funds sector liabilities and holder assets. |
| Insurance reserves | AF.6 insurance, pension, and standardized guarantee schemes. | Split insurance and pension claims where the source allows. |
| Net worth / capital | Balance-sheet residual, not an ESA financial instrument. | Used as accounting equity in BSM and runtime financial stocks. |

## Vintage And Extraction Manifest

Every future empirical extract should carry at least:

| Field | Example |
| --- | --- |
| `source_provider` | `GUS`, `Eurostat`, `NBP`, `KNF`, `MF`, `ZUS`, `ECB` |
| `source_url` | Source page or API endpoint. |
| `dataset_code` | Eurostat code, BDL variable id, NBP table name, KNF file name, MF report name. |
| `vintage` | Published period, release date, and revision timestamp when available. |
| `accessed_at` | Date of extraction. |
| `license_or_reuse_note` | Official reuse/citation note or dataset-specific restriction. |
| `frequency` | Daily, monthly, quarterly, annual, survey wave. |
| `unit` | PLN, EUR, index, percent, persons, firms, ratio. |
| `transformation` | Frequency conversion, rebasing, FX conversion, sector crosswalk, scaling. |
| `model_target` | Config field, output column, scenario id, or validation row. |
| `status` | `READY`, `PARTIAL`, `MISSING_OUTPUT`, `MISSING_SOURCE_DETAIL`, `BRIDGE_ASSUMPTION`. |

## Priority Gaps

| Priority | Gap | Why it matters | Expected owner surface |
| --- | --- | --- | --- |
| High | Direct GDP and sector-output Monte Carlo columns. | GDP and sectoral fit cannot be publication-grade while reconstructed indirectly. | `McTimeseriesSchema`, empirical validation report. |
| High | Holder-resolved financial-account stocks and revaluations. | SFC stock-flow reconciliation needs independent stock, transaction, and other-change channels. | Matrix evidence, future empirical extract. |
| High | Household microdata/distribution extraction. | MPC, savings, debt, inequality, and wealth calibration currently rely on weak provenance. | Calibration register, terminal household validation. |
| High | Firm terminal size distribution and sector output. | ABM meso validation should show firm demographics and production structure, not only macro aggregates. | Monte Carlo terminal summary. |
| High | Credit maturity, recovery, and default histories. | Banking stress paths depend on loss timing, recovery, and maturity structure. | Banking config, KNF/NBP bridge rows. |
| Medium | Effective tax rates by model sector. | VAT/excise/cash-accrual fiscal mapping affects fiscal multipliers and price channels. | Fiscal config and validation report. |
| Medium | Source vintage lockfile or extraction manifest. | Reproducibility requires knowing which empirical vintage produced a calibration or validation table. | Future data-extract workflow. |
| Medium | Scenario provenance per registered scenario. | Policy and shock experiments need a clear empirical or counterfactual basis. | Scenario registry. |

## Operating Checklist

Before a parameter, validation target, or scenario input is claimed as
empirical:

1. Identify the provider, source URL, dataset/table code, and vintage.
2. Record the unit, frequency, sector/instrument classification, and release
   cadence.
3. Apply the frequency, stock/flow, price, FX, and `gdpRatio` rules above.
4. Record whether the value initializes state, calibrates a parameter, defines
   a scenario, or validates output.
5. Link the target field to `docs/calibration-register.md`,
   `docs/scenario-registry.md`, or `docs/empirical-validation-report.md`.
6. Keep unresolved assumptions visible with `BRIDGE_ASSUMPTION`,
   `MISSING_SOURCE_DETAIL`, `MISSING_OUTPUT`, or `PARTIAL`.
