# Amor Fati Model Specification

This is the canonical publication-facing entry point for the current Amor Fati
model. It consolidates the model's purpose, scope, state vector, monthly timing,
equation families, SFC/accounting contract, stochasticity, calibration surface,
validation evidence, and known limitation boundaries.

This document describes the implemented executable model. It is not a normative
target model, and it does not replace the detailed source documents listed
below.

## Source Map

This table lists the detailed sources used by the specification. It is not a
second reading order; the canonical first-pass path is in
[Reviewer Reading Path](#reviewer-reading-path).

| Source | Role in the model specification |
| --- | --- |
| [Model notation and state vector](model-notation-and-state-vector.md) | Canonical symbols, state vector, time indexing, quantity classes, stochastic notation, and implementation anchors. |
| [Monthly transition function](monthly-transition-function.md) | Formal $X_{t} \to X_{\tau}$ month-step contract, including randomness, same-month economics, closed-month state, flow emission, runtime ledger execution, SFC validation, and next-pre boundary. |
| [Stochastic processes and replay](stochastic-processes-and-replay.md) | Publication-facing randomness contract: initialization streams, month streams, stochastic decision surfaces, Monte Carlo seed policy, deterministic replay, validation, and limitations. |
| [Household equations](household-equations.md) | Publication-facing household-sector state, income, PIT, transfers, consumption, mortgage service, consumer credit, liquidity shortfall, distress, retraining, remittances, evidence, and limitations. |
| [Firm equations](firm-equations.md) | Publication-facing firm-sector state, production, P&L, labor, pricing, inventory, investment, technology, financing, default/NPL, entry/exit, evidence, and limitations. |
| [Banking and financial-sector equations](banking-and-financial-sector-equations.md) | Publication-facing banking and financial-stability section: bank state, rates, approval gates, ratios, ECL, interbank, bond waterfall, capital, failure/resolution, financial-sector interfaces, outputs, validation, and limitations. |
| [Institutional sector equations](institutional-sector-equations.md) | Publication-facing central-government, social-fund, JST, NBP, external-sector, insurance, NBFI/TFI, quasi-fiscal, SFC, output, validation, and limitation surface. |
| [ODD / ODD+D model documentation](odd-model-documentation.md) | ODD/ODD+D description of purpose, entities, scales, scheduling, initialization, inputs, submodels, observation surfaces, and decisions. |
| [Behavioral equations and decision rules](behavioral-equations-and-decision-rules.md) | Implemented equations and algorithmic decision rules by model family. |
| [SFC matrix evidence](sfc-matrix-evidence.md) | Balance Sheet Matrix, Transactions Flow Matrix, stock-flow reconciliation evidence, sign conventions, and generated matrix artifacts. |
| [Engine invariants and economic semantics](engine-invariants-and-semantics.md) | Hard invariants, normal-path expectations, stress semantics, known limitations, enforcement points, and coverage. |
| [Calibration register](calibration-register.md) | Parameter names, units, owners, empirical targets, transformations, provenance status, and searchable gaps. |
| [Data bridge to national and financial accounts](data-bridge-national-financial-accounts.md) | Official data sources and empirical bridges used for initialization, calibration, scenarios, and validation. |
| [Empirical validation report](empirical-validation-report.md) | Empirical-validation workflow and current snapshot artifacts. |
| [Operational appendix index](operations.md#operational-appendix-index) | Entry point for CI, integration tests, generated outputs, nightly diagnostics, stress profiles, profiling, scenarios, robustness, and observability appendices. |

## Model Identity

Amor Fati is a stock-flow consistent agent-based macroeconomic model of the
Polish economy. It simulates heterogeneous households, heterogeneous firms,
multi-row banking-sector balance sheets, public-sector institutions, financial
markets, non-bank financial institutions, insurance, and the rest of world.

The model is designed for executable counterfactual analysis under strict
accounting discipline:

- behavioral rules are implemented as explicit agent, market, and institutional
  mechanisms;
- every supported monetary flow is routed through the runtime ledger and SFC
  validation surface;
- generated diagnostics and matrix artifacts expose whether model behavior is
  normal-path, stress, exploratory, benchmark, or performance evidence;
- calibration and empirical validation are documented as evidence surfaces, not
  as hidden assumptions.

The strongest model contract is accounting correctness. Macro paths can be
revised, calibrated, or rejected. Silent monetary drift is a model error.

## Scope And Scale

| Dimension | Current implementation |
| --- | --- |
| Economy | Poland with explicit rest-of-world sector |
| Time | Monthly discrete steps |
| Households | Individual heterogeneous household agents |
| Firms | Individual heterogeneous firm agents |
| Banks | Ten banking-sector rows: named bank archetypes plus residual Other banks |
| Production sectors | BPO/SSC, Manufacturing, Retail/Services, Healthcare, Public, Agriculture |
| Regions | Six NUTS-1 macroregions for regional labor and housing mechanics |
| Public sector | Central government, local government, ZUS, NFZ, PPK, FP, PFRON, FGSP, quasi-fiscal vehicles |
| Financial sector | Banks, NBP, insurers, investment funds/NBFI, corporate bonds, government bonds, listed equity |
| External sector | Trade, tourism, remittances, FDI, portfolio flows, FX reserves, NFA/current-account channels |
| Money domain | PLN-denominated fixed-point values unless explicitly marked otherwise |

The model is not a GIS model. Region is a market-segmentation and demographic
attribute, not a continuous spatial coordinate.

## Canonical State Vector

The complete month-boundary state is:

$$
\begin{aligned}
X_{t} &= (m_{t}, W_{t}, F_{t}, H_{t}, B_{t}, A^{H}_{t}, L_{t})
\end{aligned}
$$

| Symbol | Runtime field | Meaning |
| --- | --- | --- |
| $m_{t}$ | `FlowSimulation.SimState.completedMonth` | completed month index |
| $W_{t}$ | `FlowSimulation.SimState.world` | macro, market, mechanism, signal, and diagnostic world state |
| $F_{t}$ | `FlowSimulation.SimState.firms` | firm behavioral state vector |
| $H_{t}$ | `FlowSimulation.SimState.households` | household behavioral state vector |
| $B_{t}$ | `FlowSimulation.SimState.banks` | bank operational state vector |
| $A^{H}_{t}$ | `FlowSimulation.SimState.householdAggregates` | household aggregate diagnostics and market aggregates |
| $L_{t}$ | `FlowSimulation.SimState.ledgerFinancialState` | ledger-owned financial balances |

The state vector intentionally separates:

- behavioral agent state: household, firm, and bank decision-relevant state;
- macro and market state: prices, policy, external conditions, expectations,
  market memory, demand signals, regional wages, mechanism state, and flow
  diagnostics;
- ledger-owned financial state: supported financial balances in
  `LedgerFinancialState`, projected into runtime execution and SFC validation.

Detailed notation, stock/flow/rate/share conventions, stochastic notation, and
state-to-code mapping live in
[model-notation-and-state-vector.md](model-notation-and-state-vector.md).

## Monthly Transition Function

One model month is the transition:

$$
\begin{aligned}
\Phi_{\tau} : (X_{t}, RND_{\tau}, \theta) \to (X_{\tau}, E_{\tau}) \\
\tau &= t + 1
\end{aligned}
$$

where:

| Symbol | Meaning |
| --- | --- |
| $X_{t}$ | month-boundary state after completed month `t` |
| $RND_{\tau}$ | explicit month randomness contract |
| $\theta$ | model parameter vector, including scenario-adjusted parameters |
| $\Phi_{\tau}$ | executable one-month transition implemented by `FlowSimulation.step` |
| $X_{\tau}$ | next month-boundary state |
| $E_{\tau}$ | trace, emitted flows, runtime ledger evidence, SFC validation, diagnostics, and deltas |

The execution order is:

$$
\begin{aligned}
\text{pre boundary}
{} \to \text{same-month economics}
{} \to \text{same-month boundary views} \\
{} \to \text{semantic closed month and seed extraction}
{} \to \text{flow emission} \\
{} \to \text{runtime ledger execution}
{} \to \text{next-pre materialization}
{} \to \text{SFC validation gate}
\end{aligned}
$$

Same-month economics calculates decisions, prices, rates, quantities, and
closing projections. The flow layer translates those quantities into typed
monetary mechanisms, executes them through the ledger topology, and validates
semantic stock-flow identities before the step result is accepted. Closed-month
and next-pre logic materialize the next boundary state and next-period decision
signals. The formal transition contract lives in
[monthly-transition-function.md](monthly-transition-function.md).

## Entity And Institution Families

| Family | Implemented role | Detailed source |
| --- | --- | --- |
| Households | Labor supply, income, consumption, savings, rent, mortgages, consumer credit, remittances, retraining, distress, bankruptcy, social-neighbor effects | [ODD](odd-model-documentation.md), [behavioral equations](behavioral-equations-and-decision-rules.md#household-rules) |
| Firms | Production, capacity, hiring/firing, inventory, investment, technology adoption, credit demand, bond/equity financing, default, entry/exit | [ODD](odd-model-documentation.md), [behavioral equations](behavioral-equations-and-decision-rules.md#firm-rules) |
| Banks | Lending, deposits, interest margins, CAR/NPL/LCR/NSFR, ECL staging, interbank, bond portfolio, failures, resolution, bail-in, BFG and Polish bank levies | [banking and financial-sector equations](banking-and-financial-sector-equations.md), [behavioral equations](behavioral-equations-and-decision-rules.md#banking-rules), [engine invariants and semantics](engine-invariants-and-semantics.md) |
| Central government | Taxes, spending, transfers, fiscal-rule constraints, bond issuance, public debt and deficit metrics | [institutional sector equations](institutional-sector-equations.md), [behavioral equations](behavioral-equations-and-decision-rules.md#government-budget-and-debt) |
| NBP | Reference rate, monetary policy, reserves, standing facilities, QE, FX operations, monetary aggregates | [institutional sector equations](institutional-sector-equations.md), [behavioral equations](behavioral-equations-and-decision-rules.md#nbp-policy-bond-yield-qe-fx) |
| External sector | Exports, imports, current account, capital account, FDI, remittances, tourism, foreign holdings, NFA | [institutional sector equations](institutional-sector-equations.md), [external-sector calibration](external-sector-baseline-calibration.md), [behavioral equations](behavioral-equations-and-decision-rules.md#external-sector) |
| Insurance | Premiums, claims, reserves, investment income, reserve assets/liabilities | [institutional sector equations](institutional-sector-equations.md), [behavioral equations](behavioral-equations-and-decision-rules.md#insurance-nbfi-quasi-fiscal-and-jst-rules) |
| NBFI/funds | TFI/NBFI assets, fund units, non-bank credit renewal, deposit drain, PPK, quasi-fiscal lending and bonds | [institutional sector equations](institutional-sector-equations.md), [private-credit calibration](private-credit-renewal-calibration.md), [behavioral equations](behavioral-equations-and-decision-rules.md#insurance-nbfi-quasi-fiscal-and-jst-rules) |

## Equation And Rule Families

This specification treats detailed equations as source-linked rule families.
The canonical detailed rule source remains
[behavioral-equations-and-decision-rules.md](behavioral-equations-and-decision-rules.md).

| Rule family | Model role | Current source of truth |
| --- | --- | --- |
| Household income, tax, transfers, consumption, saving, credit, distress | Maps employment and financial state into disposable income, consumption, debt service, defaults, liquidity stress, and household aggregates | [household equations](household-equations.md), [`Household.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Household.scala), [`HouseholdIncomeEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/HouseholdIncomeEconomics.scala), [`HouseholdFinancialEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/HouseholdFinancialEconomics.scala), household sections in behavioral equations |
| Labor, wages, demographics, social funds | Determines market wage, employment, immigration, retirements, ZUS/NFZ/PPK and earmarked fund flows | [`LaborEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/LaborEconomics.scala), [`LaborMarket.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/markets/LaborMarket.scala), [`SocialSecurity.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/SocialSecurity.scala), [`EarmarkedFunds.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/EarmarkedFunds.scala) |
| Demand, GDP, prices, equity, macroprudential | Allocates demand, computes GDP proxy, inflation, price index, equity market updates, and credit-gap policy state | [`DemandEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/DemandEconomics.scala), [`PriceEquityEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/PriceEquityEconomics.scala), [`GdpAccounting.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/GdpAccounting.scala), macroprudential mechanisms |
| Firm production, investment, technology, financing, default, entry | Computes production/capacity, pricing, labor adjustment, investment, financing mix, credit rejection, default/NPL, births/deaths | [firm equations](firm-equations.md), `agents/firm/*`, [`FirmEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/FirmEconomics.scala), `engine/economics/firm/*` |
| Banking and monetary plumbing | Updates bank P&L, capital, provisioning, credit approval, rates, interbank, bond waterfall, failures/resolution, monetary aggregates | [banking and financial-sector equations](banking-and-financial-sector-equations.md), `agents/banking/*`, [`BankingEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/BankingEconomics.scala), `engine/economics/banking/*` |
| Housing and mortgages | Updates housing prices, mortgage stock, origination, repayment, default, and mortgage-to-GDP outputs | [`HousingMarket.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/markets/HousingMarket.scala), banking housing stage, mortgage flow modules |
| Fiscal, NBP, bonds, external sector | Computes public budget, public debt, rates, QE, bond yields, BoP/forex, GVC, trade, and current-account closure | [institutional sector equations](institutional-sector-equations.md), fiscal, NBP, open-economy, bond-market, and external-sector modules |
| Insurance, NBFI, quasi-fiscal, JST | Computes premiums, claims, reserves, NBFI credit, fund AUM, PPK holdings, quasi-fiscal issuance/lending, and local-government flows | [institutional sector equations](institutional-sector-equations.md), insurance, NBFI, quasi-fiscal, PPK, JST modules |
| Scenario, robustness, diagnostics | Defines executable counterfactuals, sensitivity envelopes, health summaries, and profiling evidence | [operational appendix index](operations.md#operational-appendix-index), which routes to the detailed scenario, robustness, diagnostics, and profiling appendices |

When writing publication equations, use the notation in
[model-notation-and-state-vector.md](model-notation-and-state-vector.md) and
link back to these rule-family sources rather than duplicating implementation
prose.

## SFC And Accounting Contract

Amor Fati's accounting contract has three layers:

1. Runtime ledger execution: supported monetary flows are emitted as typed
   mechanisms and executed through the verified ledger topology.
2. Ledger-owned stock projection: supported financial balances are materialized
   in `LedgerFinancialState` and projected into agent/economics execution DTOs.
3. Semantic SFC validation: exact SFC identities validate the economic stock-flow
   interpretation of the month.

The project maintains generated SFC evidence:

| Artifact | Purpose |
| --- | --- |
| [Symbolic BSM](sfc-matrix-artifacts/symbolic-bsm.md) | Paper-facing stock matrix by instrument and sector |
| [Symbolic TFM](sfc-matrix-artifacts/symbolic-tfm.md) | Paper-facing monthly transaction matrix |
| [Matrix mapping](sfc-matrix-artifacts/matrix-mapping.md) | Symbolic row to runtime asset/mechanism/coverage mapping |
| [Flow-channel semantics](sfc-matrix-artifacts/flow-mechanism-semantics.md) | Economic meaning of runtime flow mechanisms |
| [Stock-flow reconciliation](sfc-matrix-artifacts/stock-flow-reconciliation.md) | Executed-run evidence for stock deltas, levels, revaluation, defaults, write-offs, and other changes |

The hand-maintained [model equations to SFC map](model-equations-to-sfc-map.md)
connects equation families and state variables to those generated rows,
identities, and runtime evidence. It is reviewed with the SFC artifacts, but is
not generated by the SFC export tasks.

Known unsupported, diagnostic-only, or non-holder-resolved stock families must
remain explicit. In particular, bank capital is a persisted bank regulatory and
accounting buffer validated by SFC, not holder-resolved bank equity. The
unretained share of bank gross income currently has no modeled holder-side
receiver and is therefore an explicit SFC concession rather than a hidden
dividend flow.

## Stochasticity And Replay

The model is deterministic conditional on:

$$
\begin{aligned}
(X_{t}, RND_{\tau}, \theta)
\end{aligned}
$$

$RND_{\tau}$ is an explicit `MonthRandomness.Contract`, not ambient global
randomness. It derives named streams for household income, firm economics,
household financial economics, open-economy economics, banking economics, FDI
M&A, firm entry, startup staffing, and regional migration.

Monte Carlo output is distributional across seeds. Within one seed and one
month boundary, replay requires the same state, same parameter vector, and same
randomness contract.

The publication-facing seed policy, initialization streams, monthly stream map,
stochastic decision surfaces, diagnostic seed semantics, validation coverage,
and limitations live in
[stochastic-processes-and-replay.md](stochastic-processes-and-replay.md).

## Calibration And Empirical Evidence

Calibration is currently documented through:

- [calibration-register.md](calibration-register.md): parameter-level register,
  units, owners, empirical targets, transformations, and provenance status;
- [data-bridge-national-financial-accounts.md](data-bridge-national-financial-accounts.md):
  source mapping from official national and financial accounts into model
  initialization, calibration, scenario, and validation surfaces;
- [empirical-validation-report.md](empirical-validation-report.md): current
  empirical-validation workflow and generated validation snapshot;
- targeted calibration notes for external sector, household credit stress, and
  private credit renewal.

Use these artifacts after the model-equation and SFC review when the question
becomes "which sources, transformations, snapshots, and visible gaps support
the current parameterization?" They are evidence surfaces, not a second
equation narrative. Calibration governance is intentionally treated as a
separate design problem; this model specification references current
calibration artifacts but does not declare a parameter source-of-truth policy.

## Validation And Diagnostics

Validation is layered:

| Layer | Role |
| --- | --- |
| Unit/property tests | Local mechanism, algebraic, schema, parser, and invariant checks |
| Integration tests | Short end-to-end engine health and deterministic TSV checks |
| Generated-output guard | Ensures committed generated docs/resources match source generators |
| Diagnostics profiles | Long validation and research diagnostics from assembled jar under Nix |
| Nightly health summary | Compact thresholded verdict over existing diagnostics artifacts |
| Performance telemetry | Step runtime, throughput, memory, GC, and soft regression-budget evidence |
| Hot-path profiling | JFR-backed runtime and allocation evidence |

Failure semantics are not uniform. Accounting, ledger conservation, malformed
outputs, missing required artifacts, impossible stock states, and exact SFC
breaks are hard failures. Calibration metrics, stress outcomes, exploratory
diagnostics, and performance budgets start as warning/report evidence unless a
written threshold rationale promotes them.

The operational appendix index in
[operations.md](operations.md#operational-appendix-index) routes to the detailed
validation, diagnostics, profiling, scenario, and generated-output contracts.

## Observation Surfaces

The primary numeric model output is the Monte Carlo time-series schema, backed
by TSV outputs and diagnostics artifacts. Representative surfaces include:

- macro variables: GDP proxy, inflation, unemployment, wages, public debt,
  fiscal balance, current account, prices, monetary aggregates;
- private-sector variables: household consumption, deposits, credit, distress,
  firm production, investment, technology adoption, credit demand, defaults,
  entry/exit;
- financial-stability variables: bank capital, NPLs, CAR, LCR, NSFR, interbank,
  BFG levy, Polish bank levy, bail-in, failures, resolution, bank
  reconciliation residuals;
- SFC/accounting artifacts: BSM, TFM, stock-flow reconciliation, flow-mechanism
  semantics, SFC identity diagnostics;
- validation artifacts: health summaries, empirical validation snapshots,
  scenario outputs, robustness envelopes, loan-origination diagnostics,
  HH-bank lead-lag diagnostics, profiling artifacts.

Output-column details belong in the detailed diagnostics and schema documents,
not in this overview.

## Implemented Model, Limitations, And Future Extensions

This specification distinguishes current implementation from future research
extensions:

| Category | Current status |
| --- | --- |
| Implemented model | Monthly SFC-ABM with heterogeneous households/firms, multi-row banks, public sector, NBP, external sector, insurance, NBFI/funds, quasi-fiscal vehicles, executable scenarios, diagnostics, SFC evidence, and validation/profile workflows |
| Known limitations | Bank capital is not holder-resolved equity, and unretained bank gross income has no modeled holder-side receiver; some symbolic matrix rows intentionally expose unsupported or diagnostic-only coverage; calibration governance is not yet settled; several empirical bridges remain incomplete; performance budgets are soft warnings |
| Future research extensions | Long-horizon cycle/regime validation, richer calibration governance, deeper holder-resolved ownership where supported by data, refined empirical validation, and publication-grade sector equation consolidation |

The implemented model should be read as an executable scientific object:
behavioral mechanisms can be revised, calibration can be improved, and
extensions can be added, but the accounting and validation surfaces must remain
auditable.

## Reviewer Reading Path

For a first academic review, use this path and treat every other document as a
supporting source rather than a competing entry point:

1. Model spine: read this document first.
2. ABM/ODD and executable model contract: read
   [odd-model-documentation.md](odd-model-documentation.md),
   [model-notation-and-state-vector.md](model-notation-and-state-vector.md),
   [monthly-transition-function.md](monthly-transition-function.md), and
   [stochastic-processes-and-replay.md](stochastic-processes-and-replay.md).
   The ODD document is the companion source for ABM entities, scheduling,
   submodels, observation surfaces, and decision structure.
3. Sector and behavior detail: read the sector equation documents for the
   mechanism families relevant to the review:
   [household](household-equations.md), [firm](firm-equations.md),
   [banking and financial sector](banking-and-financial-sector-equations.md),
   and [institutional sector](institutional-sector-equations.md). Use
   [behavioral-equations-and-decision-rules.md](behavioral-equations-and-decision-rules.md)
   as a companion reference when implementation-level rule detail is needed.
4. SFC evidence boundary: read
   [model-equations-to-sfc-map.md](model-equations-to-sfc-map.md) and
   [sfc-matrix-evidence.md](sfc-matrix-evidence.md) as hand-maintained entry
   points, then inspect generated `docs/sfc-matrix-artifacts/*` only for the
   specific matrix rows or reconciliation evidence under review.
5. Calibration evidence: after the equation and SFC review, use
   [calibration-register.md](calibration-register.md) and
   [data-bridge-national-financial-accounts.md](data-bridge-national-financial-accounts.md)
   to inspect current parameter evidence, external-source bridges, and visible
   provenance gaps.
6. Validation evidence: read
   [empirical-validation-report.md](empirical-validation-report.md) and
   [engine-invariants-and-semantics.md](engine-invariants-and-semantics.md).
7. Operational appendices: use
   [operations.md](operations.md#operational-appendix-index) as the entry point
   only when reproducing runs, changing CI/diagnostics, or navigating local
   run artifacts.
