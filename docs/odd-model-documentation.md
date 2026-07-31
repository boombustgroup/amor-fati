# ODD and ODD+D Model Documentation

This document is the ODD/ODD+D source document for Amor Fati as a stock-flow
consistent agent-based macroeconomic model of the Polish economy. It follows
the ODD protocol structure and adds ODD+D notes for human and institutional
decision making.

For the canonical publication-facing model entry point, see
[`docs/model-specification.md`](model-specification.md). This document remains
the ODD/ODD+D source for entities, scheduling, submodels, and decisions.

The document describes the current implementation, not an idealized target
model. Open gaps are listed explicitly at the end.

## Protocol References

The structure follows:

- Grimm et al. (2020), "The ODD protocol for describing agent-based and other
  simulation models: A second update to improve clarity, replication, and
  structural realism", JASSS 23(2), https://www.jasss.org/23/2/7.html
- Grimm et al. (2006), "A standard protocol for describing individual-based
  and agent-based models", https://www.usgs.gov/publications/a-standard-protocol-describing-individual-based-and-agent-based-models
- Mueller et al. (2013), "Describing human decisions in agent-based models -
  ODD+D, an extension of the ODD protocol",
  https://doi.org/10.1016/j.envsoft.2013.06.003

ODD is used here as a stable model-description format. ODD+D is used as an
overlay for decision rules, information, constraints, heterogeneity, and
adaptation in households, firms, banks, and policy institutions.

## 1. Purpose And Patterns

### Purpose

Amor Fati is designed to support executable counterfactual analysis of a
monetary production economy under strict stock-flow consistency. The immediate
domain is the Polish economy with heterogeneous households, heterogeneous firms,
ten banking-sector rows, public-sector institutions, financial markets,
non-bank financial institutions, insurance, and a rest-of-world sector. The
banking rows are not individual commercial-bank case studies: they consist of
named bank archetypes plus a residual Other banks row for the remaining banking
sector.

The model is intended to answer questions of the following kind:

- How do fiscal, monetary, banking, external-sector, and technology shocks
  propagate through heterogeneous agents and balance sheets?
- Which channels produce macro outcomes such as unemployment, inflation,
  bank stress, credit contraction, public debt pressure, current-account
  movement, and firm entry or exit?
- Which outputs are behavioral model results and which are accounting
  invariants enforced by the runtime ledger?

The central design constraint is stronger than calibration plausibility:
financial flows must conserve value exactly and the semantic SFC identities
must close each month. Behavioral rules may be revised; accounting drift is a
model error.

### Target Patterns

The current model is expected to generate and expose:

- macro time series: GDP proxy, inflation, unemployment, wages, public debt,
  fiscal balance, credit, deposits, bank capital, and monetary aggregates;
- meso patterns: sectoral output, hiring pressure, production capacity,
  firm-size and technology composition, regional labor variation, and external
  trade/GVC stress;
- micro patterns: household employment status, consumption, savings,
  financial-distress state, retraining, bankruptcy, education, region, and
  social-neighbor effects;
- financial-stability patterns: CAR, NPLs, LCR/NSFR inputs, interbank stress,
  bank failure and resolution, BFG levy, Polish bank levy and bail-in, government-bond portfolio
  effects, and credit rationing;
- accounting artifacts: symbolic Balance Sheet Matrix (BSM), Transactions
  Flow Matrix (TFM), stock-flow reconciliation evidence, and runtime mapping
  generated from an executed simulation step.

Empirical validation of these patterns is structured in
`docs/empirical-validation-report.md`.

## 2. Entities, State Variables, And Scales

### Entity Classes

| Entity | Representation | Main state | Code anchor |
| --- | --- | --- | --- |
| Households | Individual agents | employment/activity status, wage, skill, health penalty, MPC, rent, social neighbors, bank assignment, region, education, contract type, household financial stocks projected from the ledger | [`agents/Household.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Household.scala) |
| Firms | Individual agents | sector, technology regime, workers, productivity/capacity, capital stock, inventory, cash/debt/equity projections, digital readiness, network position, bankruptcy state | [`agents/Firm.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Firm.scala) |
| Banks | Ten banking-sector rows: named bank archetypes plus residual Other banks | operational status, capital diagnostics, NPLs, risk buckets, regulatory ratios, per-row rates, resolution state; financial stocks projected from ledger rows | [`agents/Banking.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Banking.scala) |
| Government | Aggregate institution | spending, tax revenue, debt, fiscal rules, bond issuance, benefits, transfers, capital investment | [`engine/markets/FiscalBudget.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/markets/FiscalBudget.scala), [`engine/flows/GovBudgetFlows.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/flows/GovBudgetFlows.scala) |
| NBP | Central bank institution | reference rate, QE policy, FX operations, reserve/standing-facility plumbing, government-bond and foreign-asset stocks | [`agents/Nbp.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Nbp.scala), [`engine/flows/BankingFlows.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/flows/BankingFlows.scala) |
| Social funds | Aggregate public-fund agents | ZUS/FUS, NFZ, PPK, FP, PFRON, FGSP balances and monthly contribution/spending flows | [`agents/SocialSecurity.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/SocialSecurity.scala), [`agents/EarmarkedFunds.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/EarmarkedFunds.scala) |
| Local government | Aggregate JST sector | local tax revenue, central-government subvention, spending, unsupported local debt metric, cash | [`agents/Jst.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Jst.scala), [`engine/flows/JstFlows.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/flows/JstFlows.scala) |
| Insurance | Aggregate life and non-life sector | premiums, claims, investment income, reserves, bond/equity holdings | [`agents/Insurance.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Insurance.scala), [`engine/flows/InsuranceFlows.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/flows/InsuranceFlows.scala) |
| NBFI and funds | Aggregate TFI/NBFI/BGK/PFR layer | fund units, AUM, deposit drain, NBFI credit, quasi-fiscal bonds and lending | [`agents/Nbfi.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Nbfi.scala), [`agents/QuasiFiscal.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/QuasiFiscal.scala) |
| Rest of world | Aggregate external sector | trade, tourism, FDI, portfolio flows, carry trade, remittances, primary income, EU funds, foreign holdings | [`engine/markets/OpenEconomy.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/markets/OpenEconomy.scala), [`engine/flows/OpenEconFlows.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/flows/OpenEconFlows.scala) |

The ledger-facing sector ontology is `EntitySector`: Households, Firms, Banks,
Government, NBP, Insurance, Funds, and Foreign. In paper matrices, Foreign is
shown as ROW, meaning Rest of World.

### State Variables

Amor Fati separates state into three broad surfaces:

- behavioral agent state: household, firm, and bank decision-relevant state;
- macro and market state: prices, policy, external conditions, expectations,
  market memory, demand signals, regional wages, and mechanism state;
- ledger-owned financial state: supported financial balances in
  `LedgerFinancialState`, projected into runtime execution and validation.

The canonical mathematical notation for these surfaces is defined in
[`docs/model-notation-and-state-vector.md`](model-notation-and-state-vector.md).
In that notation, the complete month-boundary state is
$X_{t} = (m_{t}, W_{t}, F_{t}, H_{t}, B_{t}, A^{H}_{t}, L_{t})$, directly mapped to
`FlowSimulation.SimState`.

The top-level monthly boundary is `FlowSimulation.SimState`, containing:

- completed month index;
- `World` macro/runtime state;
- firm, household, and bank vectors;
- household aggregate diagnostics;
- ledger financial state.

`World` contains nested state for social systems, household market state,
financial markets, external sector, real economy, mechanisms, monetary plumbing,
pipeline signals, flows, and regional wages.

### Scale

The default scale is:

- economy: Poland, with explicit rest-of-world sector;
- time: monthly discrete time steps;
- firms: 10,000 default firm agents;
- households: default total population equals $\mathrm{firmsCount} \cdot \mathrm{workersPerFirm}$,
  currently 100,000 household agents under default parameters;
- banks: ten banking-sector rows: PKO BP, Pekao, mBank, ING BSK,
  Santander, BPS/Coop, BNP Paribas, Millennium, Alior, and residual Other
  banks;
- production sectors: BPO/SSC, Manufacturing, Retail/Services, Healthcare,
  Public, Agriculture;
- regions: six NUTS-1 macroregions used for regional labor and housing
  mechanics;
- money: PLN-denominated fixed-point domain values, with agent-level flows
  scaled to Polish macro magnitudes through `gdpRatio`.

The model is not a geospatial ABM in the GIS sense. Region is an agent
attribute and market segmentation device, not a continuous spatial coordinate.

## 3. Process Overview And Scheduling

### Initialization

Initialization is deterministic given an `InitRandomness.Contract`. `WorldInit`
constructs:

- initial `World` macro state;
- firms, households, and banks;
- household aggregates;
- initial `LedgerFinancialState`.

Sub-factories create specific slices: firm network and sector structure, bank
rows, household demographic/financial heterogeneity, immigrants, demographics,
GVC state, insurance, NBFI, expectations, and housing.

### Monthly Step

Each model month is executed by:

```scala
FlowSimulation.step(state: SimState, randomness: MonthRandomness.Contract)
```

The high-level order is fixed:

1. Read the start-of-month state and explicit randomness contract.
2. Compute same-month economics in an ordered deterministic pipeline.
3. Narrow same-month outputs into flow-plan, signal, closing, semantic, and
   evidence views.
4. Build the semantic closed-month state and extract next-month decision
   signals.
5. Translate calculated quantities into typed monetary-flow mechanisms.
6. Emit and execute batched flows through the runtime ledger topology.
7. Project supported runtime deltas back into ledger-owned financial state.
8. Run exact SFC validation for the $X_{t} \to X_{\tau}$ transition.
9. Return `StepOutput` with `nextState`, trace, flows, validation, and deltas.

The mathematical transition contract is documented in
[`docs/monthly-transition-function.md`](monthly-transition-function.md).

### Economics Pipeline

The monthly calculus boundary is:

| Module | Domain |
| --- | --- |
| `FiscalConstraintEconomics` | minimum wage indexation, reservation wage, lending base rate |
| `LaborEconomics` | labor market, wages, employment, demographics, immigration, payroll base |
| `HouseholdIncomeEconomics` | household income, consumption, saving, portfolios, separations, mobility |
| `DemandEconomics` | demand allocation across consumption, government purchases, investment, exports |
| `FirmEconomics` | production, I-O intermediate market, capex, financing, labor, NPL detection |
| `HouseholdFinancialEconomics` | mortgages, deposit interest, remittances, tourism, consumer credit |
| `PriceEquityEconomics` | inflation, equity market, GDP proxy, macroprudential, EU funds |
| `OpenEconEconomics` | BoP, forex, GVC, Taylor rule, bond yields, insurance, NBFI |
| `BankingEconomics` | bank P&L, provisioning, CAR, resolution, interbank, BFG and Polish bank levies, M1/M2/M3 |
| `closedmonth/MonthClosing` | aggregation, informal economy, lifecycle transitions, SFC status, next state |

### Flow Emission And Accounting Boundary

Economics stages compute quantities. Monetary execution is done through
runtime flows. Each emitted flow has a `FlowMechanism` and an `AssetType`.
Flows are grouped into `BatchedFlow` values and executed through the ledger.

The SFC matrix evidence workflow generates symbolic BSM, TFM, stock-flow
reconciliation, and runtime-mapping artifacts from an executed deterministic
step. See `docs/sfc-matrix-evidence.md`.

### Timing Contract

The model distinguishes:

- `pre` decision inputs persisted at the beginning of a month;
- same-month operational signals computed inside a month;
- closed-month realized outcomes;
- extracted next-month `DecisionSignals`.

This contract is implemented through `MonthSemantics`, `OperationalSignals`,
`SignalExtraction`, and `MonthTrace`. It prevents same-month results from being
silently read as if they were beginning-of-month information.

### Randomness And Reproducibility

Initialization and monthly execution use explicit root seeds split into named
random streams. Fixing the initialization seed and month-level randomness
schedule fixes replay. Monte Carlo runs use the same engine path, varying seeds
and writing per-seed time series.

## 4. Design Concepts

### Basic Principles

The model combines:

- stock-flow consistent macro accounting;
- heterogeneous-agent ABM microstructure;
- institutional detail for Poland;
- ledger-first monetary execution;
- bounded, rule-based decisions rather than representative-agent optimization.

The strongest invariant is accounting conservation. Behavioral rules are
research hypotheses encoded as executable mechanisms.

### Emergence

The model treats macro variables as emergent from agent and institutional
interactions: GDP proxy, inflation, unemployment, credit, defaults, public debt,
bank stress, trade balance, monetary aggregates, firm entry/exit, sectoral
composition, automation ratios, and financial-market indicators.

### Adaptation

Households adapt consumption, saving pressure, sectoral mobility, retraining,
credit usage, and distress state to income, employment, networks, prices, and
bank rates. Firms adapt hiring, firing, pricing, investment, technology choice,
financing, production, and survival to demand, costs, credit, expectations, and
sector conditions. Banks adapt lending rates, approval conditions, liquidity,
interbank behavior, provisioning, and resolution behavior to capital, NPLs,
rates, liquidity and regulation. Fiscal, monetary, and macroprudential rules
respond to debt, inflation, output, unemployment, and credit conditions.

### Objectives

The model uses local procedural objectives:

- households attempt to maintain consumption, housing payments, employment,
  financial buffers, and solvency under bounded resources;
- firms attempt to produce profitably, meet demand, manage labor and capital,
  finance investment, adopt technology when favorable, and avoid insolvency;
- banks attempt to lend profitably while satisfying capital and liquidity
  constraints and absorbing losses;
- government follows fiscal-rule and spending/tax logic;
- NBP follows monetary and liquidity-plumbing rules;
- funds, insurance, and external-sector modules follow institutional flow and
  balance-sheet rules.

No global social planner optimizes the economy.

### Learning And Prediction

The model contains adaptive expectations and lagged signal propagation rather
than full rational expectations. The `DecisionSignals` surface carries lagged
inflation, expected inflation, unemployment, hiring slack, startup absorption,
and sector demand signals. Expectations, pricing, investment, mobility, and
entry decisions use these signals through local rules.

### Sensing

Agents and institutions sense local or aggregate state depending on their role:

- households sense wage, employment status, bank rates, financial buffers,
  social-neighbor distress, region, and sector mobility opportunities;
- firms sense sector demand, capacity, labor cost, capital cost, technology
  readiness, cash/debt constraints, neighbors, and market prices;
- banks sense loan books, deposits, reserves, NPLs, CAR, regulatory buffers,
  bond yields, failed-bank state, and interbank conditions;
- public institutions sense tax bases, unemployment, debt, spending pressure,
  social-insurance balances, inflation, and external-sector conditions.

### Interaction

Interactions are mediated by:

- labor markets and firm-worker matching;
- goods demand and sectoral production;
- household social networks and distress contagion;
- firm networks and technology demonstration effects;
- bank assignment, rates, lending, defaults, interbank, and resolution;
- fiscal transfers, taxes, benefits, and government purchases;
- bond, equity, mortgage, insurance, NBFI, and quasi-fiscal markets;
- external trade, capital flows, remittances, tourism, and GVC shocks;
- the runtime ledger, which records monetary consequences.

### Stochasticity

Stochasticity enters through explicit random streams. Examples include
initial heterogeneity, firm network topology, household traits, sectoral
assignment, mobility, retraining, technology adoption/failure, market draws,
and Monte Carlo seeds. Randomness is not hidden in global mutable state.

### Collectives

Some entities are true individual agents: households and firms. The banking
sector is represented by ten bank rows/archetypes, including one residual Other
banks row, which behave as bank decision units but should not be read as ten
individual commercial banks. Other entities are collective or institutional
sectors represented at aggregate resolution: government, NBP, social funds,
insurance, NBFI/funds, quasi-fiscal bodies, and rest of world.
This is intentional: the model allocates agent detail where it is expected to
affect macro dynamics or distributional outcomes.

### Observation

Observation surfaces include:

- `MonthTrace` for boundary, randomness, validation, and timing diagnostics;
- Monte Carlo per-seed TSV time series;
- household, bank, and firm terminal summary TSVs;
- optional selected-month household micro snapshot and shortfall-cohort TSVs;
- optional selected-month firm micro snapshot TSVs;
- optional selected-firm decision trace TSVs for adoption, financing,
  implementation, and labor/digital-investment gate audits;
- SFC validation results;
- ledger-derived symbolic BSM, TFM, and stock-flow reconciliation artifacts;
- symbolic-row to runtime-ledger mapping.

## ODD+D Decision Notes

The following table summarizes the current human and institutional decision
surfaces. Detailed mathematical rules are documented in
`docs/behavioral-equations-and-decision-rules.md`.

| Decision unit | Decisions | Information used | Constraints | Heterogeneity and adaptation |
| --- | --- | --- | --- | --- |
| Households | consume, save/draw buffers, pay rent/debt, use consumer credit, enter or recover from financial distress, retrain, change sector, enter personal-insolvency filing/workout | wage, employment status, bank rates, debt service, deposits, financial-distress state, social-neighbor distress, region, education, sector signals | income, deposits, debt, credit terms, distress persistence, unemployment duration, retraining cost and success probability | skill, health, MPC, education, region, contract type, immigrant status, social network |
| Firms | produce, hire/fire, price/markup, invest, adopt hybrid/AI technology, borrow, issue equity/bonds, enter or exit | demand pressure, capacity, wages, costs, credit rates, cash, debt, sector, network effects, digital readiness | cash, working capital, labor availability, bank lending, profitability thresholds, technology failure risk | sector, size, technology regime, readiness, productivity, bank relation, network position |
| Banks | price loans/deposits, approve credit, manage liquidity, absorb losses, provision, resolve failures, participate in interbank and bond markets | deposits, loans, reserves, capital, NPLs, bond yields, regulatory buffers, failed-bank state | CAR, LCR/NSFR inputs, reserve requirement, BFG/bail-in rules, failed-bank exclusion | ten bank rows: named archetypes plus residual Other banks, with row-specific buffers, spreads, capital, and balance sheets |
| Government | tax, spend, transfer, invest, issue debt, service debt, respond to fiscal rules | fiscal balance, debt, unemployment, tax bases, bond demand, social fund balances | fiscal-rule severity, debt brake, spending cuts, financing needs | aggregate institution with policy parameters |
| NBP | set rate channel inputs, provide reserve interest and standing facilities, QE/FX operations | inflation, output, forex, reserves, bond market, interbank state | policy corridor, reserves, QE pace, FX reserve stock | aggregate institution |
| Funds and insurance | collect contributions/premiums, pay claims/benefits, allocate holdings, issue/absorb quasi-fiscal instruments | payroll, unemployment, bankruptcies, AUM, reserves, bond/equity returns | statutory contribution/spending rules, portfolio shares, reserve/liability constraints | aggregate institution by fund family |
| Rest of world | trade, tourism, FDI, portfolio, carry trade, primary income, remittances, foreign bond/equity participation | exchange rate, yields, demand, GVC state, risk-off conditions | external demand, capital-flow rules, foreign-asset positions | aggregate foreign sector |

Decision rules are currently procedural and bounded. The project does not claim
that every behavioral rule is empirically final. Parameter provenance and
calibration status are documented in `docs/calibration-register.md`.

## 5. Initialization

The initialized state is produced by `WorldInit.initialize`, using
`InitRandomness.Contract.fromSeed(seed)`. The initializer:

- builds firm and household populations from `PopulationConfig`, `FirmConfig`,
  `HouseholdConfig`, sector definitions, firm-size distribution, networks, and
  region/education/skill draws;
- builds ten banking-sector rows and their initial financial-stock DTOs:
  named archetypes plus residual Other banks;
- initializes government, NBP, social funds, insurance, NBFI, quasi-fiscal,
  housing, GVC, expectations, immigration, and demographics state;
- assembles initial `LedgerFinancialState` as the source of truth for
  supported financial stocks;
- derives initial household aggregates.

Default parameters are hardcoded in `SimParams.defaults`; stock values are
scaled by `gdpRatio` to map agent-level monthly flows to real Polish macro
magnitudes. The current initialization does not yet read a formal external data
bundle, but the source-to-model bridge for future empirical bundles is
documented in `docs/data-bridge-national-financial-accounts.md`.

## 6. Input Data

Current inputs are:

- source-code defaults in `SimParams.defaults` and sub-config classes;
- explicit initialization seed;
- explicit monthly randomness schedule;
- Monte Carlo configuration (`nSeeds`, duration, run id, output prefix);
- named scenario changes from `docs/scenario-registry.md`, or direct config
  changes in code for exploratory work.

The model contains many Poland-specific calibration comments and values, but a
structured calibration register is documented in
`docs/calibration-register.md`. The reproducible scenario registry is
documented in `docs/scenario-registry.md`. External source selection,
unit/frequency conversion, and national/financial-account crosswalks are
documented in `docs/data-bridge-national-financial-accounts.md`.

## 7. Submodels

The model is organized into submodels by package:

- `agents`: autonomous household, firm, bank, public-sector, insurance, NBFI,
  quasi-fiscal, migration, and social-security state/rules;
- `init`: factories for deterministic initial state construction;
- `engine/economics`: fixed monthly computation pipeline;
- `engine/flows`: monetary-flow emission and ledger execution;
- `engine/ledger`: financial-stock ownership contracts and runtime projection;
- `engine/markets`: market clearing, prices, yields, equity, housing,
  corporate bonds, open economy, capital flows, GVC, I-O, and regional markets;
- `engine/mechanisms`: policy, expectations, macroprudential, mobility, tax,
  yield-curve, informal-economy, and entry mechanisms;
- `accounting`: exact SFC identities and ledger-derived matrix evidence;
- `montecarlo`: multi-seed execution and output schemas.

The paper-facing behavioral equation catalog is documented in
`docs/behavioral-equations-and-decision-rules.md`. This ODD document keeps the
protocol-level model description, while that rule book carries equations,
algorithmic rules, implementation references, output columns, and explicit
simplifications.

The empirical validation report is documented in
`docs/empirical-validation-report.md`. It describes the manifest and snapshot
workflow for macro, meso, micro, financial, and external validation targets and
keeps missing data or output coverage visible.

The sensitivity and robustness workflow is documented in
`docs/sensitivity-robustness-workflow.md`. It runs small Monte Carlo seed bands
and one-at-a-time parameter sweeps, producing seed envelopes and sensitivity
summaries under `target/`.

The reproducible scenario registry is documented in
`docs/scenario-registry.md`. It defines named policy, banking, external,
energy, tourism, and quasi-fiscal patches applied to a selected baseline, with
exact parameter deltas and an executable `scenarioRun` command path.

## Accounting And Validation Boundary

Amor Fati has two related but distinct accounting surfaces:

- runtime ledger execution: every emitted monetary flow is applied through the
  ledger topology and must conserve value exactly;
- semantic SFC validation: 15 accounting identities are checked over the
  economic stocks and flows exposed by the model.

The matrix evidence workflow adds paper-facing symbolic artifacts:

- Balance Sheet Matrix (BSM);
- Transactions Flow Matrix (TFM);
- Stock-Flow Reconciliation and Revaluation Evidence;
- symbolic-row to runtime-ledger mapping.

Numeric evidence remains in the simulation and Monte Carlo outputs. Symbolic
matrix artifacts document the accounting structure used to interpret those
outputs.

## Open Gaps

This document links the research-readiness spine docs. Remaining gaps are
tracked inside the calibration register, data bridge, empirical validation
report, and matrix evidence artifact as table-level statuses such as
`UNKNOWN_SOURCE`, `MISSING_OUTPUT`, `MISSING_DATA_BRIDGE`,
`BRIDGE_ASSUMPTION`, and `PARTIAL`.
