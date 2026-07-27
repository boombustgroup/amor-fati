# Model Constants Inventory

This document is the initial inventory of production values that are currently
embedded in the Amor Fati runtime. It is an architecture and migration
inventory, not a claim that every literal has already been scientifically
classified.

## Classification

| Category | Meaning | Target owner |
| --- | --- | --- |
| `empirical_input` | Value representing an observed economy or institution at a stated vintage. | Versioned baseline in `amor-fati-economies` |
| `initialization_policy` | Rule for constructing opening state from baseline inputs. | Typed model initialization policy in `amor-fati` |
| `model_parameter` | Behavioural or structural parameter used during a run. | Versioned model configuration / scenario |
| `structural_invariant` | Domain or algorithm invariant that must hold for every economy. | Core model code and contract tests |
| `runtime_default` | Compatibility default selected when no explicit baseline is supplied. | Remove or isolate as legacy |
| `legacy_compatibility` | Value retained only to reproduce the former `SimParams` path. | Legacy adapter, scheduled for removal |
| `test_or_diagnostic` | Value used only by tests, probes, or diagnostics. | Test or diagnostic module |

## Initial Production Inventory

The rows below identify value families and their current code owners. A family
must be split into individual typed fields before it is migrated; grouping here
prevents the same source from being counted as independent calibration data in
multiple places.

| ID | Current location | Value family | Category | Current source | Target owner | Migration action | Status |
| --- | --- | --- | --- | --- | --- | --- | --- |
| `simparams.defaults` | `modules/model/.../config/SimParams.scala` | Complete default parameter aggregate | `legacy_compatibility` | In-memory Scala defaults | Baseline manifest plus typed model configuration | Freeze digest, then replace callers with explicit `BaselineRef` | `open` |
| `population.scale` | `SimParams.scala`, `PopulationConfig.scala` | Firms, workers, households, and represented population scale | `empirical_input` / `model_parameter` | Current Poland-oriented defaults | `amor-fati-economies` population bundle plus representation policy | Separate observed counts, weights, and simulation scale | `open` |
| `banking.opening-stocks` | `BankingConfig.scala`, `WorldInit.scala` | Opening deposits, loans, reserves, bonds, and capital | `empirical_input` | Defaults and opening-bank bridge rows | PL baseline institutional component | Move values to typed opening financial state; retain allocation policy in core | `open` |
| `opening.bank-profiles` | `OpeningBankBalanceProfileBridge.scala`, `OpeningBankProfileTargets.scala` | Per-bank opening balance profiles and residual routing | `empirical_input` / `initialization_policy` | Embedded bridge rows and runtime target maps | Baseline institutional bundle plus initialization policy | Version rows and make residual policy explicit | `open` |
| `banking.default-configs` | `agents/banking/BankDefaultConfigs.scala` | Default bank identities, products, and risk settings | `model_parameter` | Scala constants | Model configuration | Confirm which fields are universal versus Poland-specific | `open` |
| `fiscal.opening-state` | `FiscalConfig.scala`, `WorldInit.scala` | Government capital, debt, spending, and opening balances | `empirical_input` / `initialization_policy` | `gdpRatio`-scaled defaults | Baseline fiscal component plus typed initialization policy | Remove implicit GDP scaling from source data | `open` |
| `monetary.opening-state` | `MonetaryConfig.scala`, `ExpectationsInit.scala`, `WorldInit.scala` | Target inflation, policy rate, FX, QE, and expectations | `empirical_input` / `model_parameter` | Defaults and opening policy | Baseline assumptions component plus scenario policy | Distinguish observed starting values from policy rules | `open` |
| `firm.population` | `FirmConfig.scala`, `FirmInit.scala`, `FirmCalibration.scala` | Firm count, workers per firm, size distribution, and startup state | `empirical_input` / `initialization_policy` | `SimParams.defaults` and calibration helpers | Enterprise bundle and population compiler | Replace aggregate firm count with enterprise/workplace artifact | `open` |
| `firm.sector-bridges` | `ProductionSectorGvaShareBridge.scala`, `ProductionSectorLaborSourceBridge.scala` | Sector shares, GVA, labour, and PKD mapping | `empirical_input` | Embedded bridge tables | `amor-fati-economies` enterprise controls | Version by classification and baseline | `open` |
| `household.opening-stocks` | `Household.scala`, `HouseholdConfig.scala`, `WorldInit.scala` | Deposits, consumer loans, mortgages, MPC distributions | `empirical_input` / `initialization_policy` | Defaults and calibration routines | Population and institutional baseline components | Separate target stocks from allocation algorithms | `open` |
| `housing.opening-state` | `HousingConfig.scala`, `HousingInit.scala` | Housing stock, prices, mortgage opening state | `empirical_input` / `initialization_policy` | Defaults and derived ratios | Baseline housing component plus policy | Record vintage and unit for every stock | `open` |
| `social.demographics` | `SocialConfig.scala`, `DemographicsInit.scala` | Initial retirees, benefits, education and sector shares | `empirical_input` | Defaults and derived formulas | Population/social baseline components | Replace formulas tied to synthetic population scale | `open` |
| `immigration` | `ImmigrationConfig.scala`, `ImmigrantInit.scala`, `agents/Immigration.scala` | Foreign wage, monthly rate, cohort attributes, and migrant allocation | `model_parameter` / `initialization_policy` | Defaults and random allocation policy | Baseline migration assumptions plus policy | Declare source vintage and avoid hidden Poland assumptions | `open` |
| `regional-labour` | `LaborConfig.scala`, `RegionalMigration.scala`, `Region.scala` | Regional wages, labour transitions, friction, and mobility | `model_parameter` / `empirical_input` | Defaults and runtime transitions | Population controls and model policy | Keep source-specific BAEL margins separate from transition rules | `open` |
| `firm-entry` | `FirmConfig.scala`, `engine/mechanisms/FirmEntry.scala` | Entry caps, sector shares, startup staffing, and entry shocks | `model_parameter` / `initialization_policy` | Config defaults and formulas | Model policy, optionally baseline sector targets | Remove hardcoded target shares where source-backed | `open` |
| `informal-economy` | `InformalConfig.scala`, `engine/mechanisms/InformalEconomy.scala` | Unemployment threshold and cyclical sensitivity | `model_parameter` | Scala defaults | Model configuration / scenario | Document calibration evidence and uncertainty | `open` |
| `nbfi-opening-state` | `NbfiConfig.scala`, `NbfiInit.scala` | TFI/NBFI AUM, asset shares, loans, and deposits | `empirical_input` / `initialization_policy` | Defaults and opening rules | Institutional baseline component | Separate stocks from portfolio allocation policy | `open` |
| `insurance-opening-state` | `InsuranceConfig.scala`, `InsuranceInit.scala` | Insurance reserves, securities, cash, and portfolio shares | `empirical_input` / `initialization_policy` | Defaults and opening rules | Institutional baseline component | Add ownership and reconciliation evidence | `open` |
| `public-institution-bridges` | `Nbp.scala`, `Jst.scala`, `StateOwned.scala`, `QuasiFiscal.scala` | Opening institutional balances and fiscal/monetary bridges | `empirical_input` / `initialization_policy` | Embedded constants and defaults | Institutional baseline plus policy | Inventory each balance-sheet owner and counter-entry | `open` |
| `sector-structure` | `SimParams.scala`, `World.scala`, `PipelineState.scala` | Sector ordering, definitions, and array dimensions | `structural_invariant` | Core classification definitions | Core ontology / classification contract | Make versioned classifications explicit | `open` |
| `flow-and-ledger-guards` | `engine/flows`, `ledger`, `accounting` | Tolerances, zero states, rounding and conservation guards | `structural_invariant` | Core implementation | Core contracts and tests | Do not move into economic baseline | `open` |
| `initial-zero-states` | `WorldStateSegments.scala`, `FlowState.scala`, mechanism state objects | Empty stocks, counters, and initial mechanism states | `structural_invariant` | `*.zero`, `*.initialState` | Core state constructors | Verify no economic calibration is hidden in zero constructors | `open` |
| `scenario-defaults` | `ScenarioRegistry.scala`, `RobustnessScenarios.scala`, CLI diagnostics | Baseline patches and scenario defaults | `test_or_diagnostic` / `model_parameter` | `SimParams.defaults` and scenario code | Research scenario registry | Require explicit baseline and versioned scenario IDs | `open` |
| `baseline.manifest` | `amor-fati-economies/artifacts/PL/PL-2025-Q4-v1/population-controls/manifest.tsv`, `PopulationControlBundleLoader.Manifest` | Baseline identity, population scope, component digest, classifications, and provenance gate | `empirical_input` / `structural_invariant` | Versioned economy artifact and core loader contract | Baseline manifest plus Research API result manifest | Pin digest, schema, source vintage, and selected baseline before runtime initialization | `open` |
| `population-to-workplace-bridge` | `amor-fati-economies` population/employment controls; target `PopulationRepresentation` and workplace compiler | Typed person-to-workplace relation and represented workplace quantity | `initialization_policy` / `empirical_input` | Synthetic bridge constrained by employment controls | P0 population/workplace compilation contract | Define one Workplace-to-Enterprise link, units, residuals, and reconciliation evidence | `open` |
| `eu-funds-envelope` | `engine/mechanisms/EuFunds.scala` | Poland-scale reference firm count and fund envelope scaling | `empirical_input` / `model_parameter` | Embedded `ReferenceEconomy` and configured totals | Baseline exogenous-assumptions component | Move the reference scale to the selected baseline; keep draw-down timing as policy | `open` |
| `flow-account-indices` | `engine/flows/ZusFlows.scala`, `BankingFlows.scala`, and related flow modules | Numeric account identifiers used by ledger topology | `structural_invariant` | Core constants | Ledger contract | Keep in core, but replace duplicated literals with typed account identifiers | `open` |
| `flow-fallback-rates` | `engine/flows/InsuranceFlows.scala`, `JstFlows.scala`, and related emitters | Fallback shares/rates used when an aggregate is zero or unavailable | `model_parameter` / `structural_invariant` | Embedded fixed-point constants | Model policy with explicit fallback evidence | Inventory each fallback and test that it cannot silently become a baseline input | `open` |
| `distribution-kernels` | `util/Distributions.scala`, `agents/Immigration.scala` | Gaussian/Beta floors, caps, and noise scales | `model_parameter` | Algorithm defaults and agent constants | Versioned model configuration | Separate sampler invariants from calibrated distribution parameters | `open` |
| `market-safety-bounds` | `engine/markets/*`, `agents/*` | Clamp bounds, minimum prices, and non-negative guards | `structural_invariant` / `model_parameter` | Core fixed-point guards | Core contracts or model configuration | Classify each bound; do not move domain safety checks into economy bundles | `open` |
| `monthly-calendar` | `engine/SimulationMonth.scala`, `MonthDriver.scala` | Month zero/first-month and timing constants | `structural_invariant` | Core calendar semantics | Core execution contract | Keep in core and cover with timing tests | `open` |
| `state-zero-constructors` | `engine/WorldStateSegments.scala`, mechanism state objects | Empty arrays, counters, and state dimensions | `structural_invariant` | `zero` constructors and sector dimensions | Core state contract | Verify dimensions come from typed classifications, not Poland defaults | `open` |
| `hardcoded-institution-ids` | `agents/Nbp.scala`, `Jst.scala`, `StateOwned.scala`, flow modules | Numeric or symbolic IDs for public institutions and accounts | `structural_invariant` / `empirical_input` | Core constants and opening bridges | Typed institution catalog plus baseline identities | Distinguish universal roles from country-specific institutions | `open` |
| `diagnostic-probe-baselines` | `modules/cli/.../diagnostics/*` | Probe seeds, horizon defaults, and baseline assumptions | `test_or_diagnostic` | CLI defaults and legacy catalog | Diagnostic configuration | Require explicit baseline selection; never use as runtime calibration | `open` |
| `fixed-point-math-constants` | `modules/model/.../fp/FixedPointMath.scala` | Pi, logarithm constants, exponent caps, and raw fixed-point conversion constants | `structural_invariant` | Numerical algorithm | Core numerical contract | Keep in core; test precision and overflow bounds separately from economic calibration | `open` |
| `bank-default-profiles` | `agents/banking/BankDefaultConfigs.scala` | Bank IDs, affinity weights, lending spreads, and default profile rows | `empirical_input` / `model_parameter` | Embedded profile table | Baseline institutional component plus bank-behaviour configuration | Identify each institution and replace Poland-specific rows with versioned data | `open` |
| `fixed-sector-cardinality` | `engine/markets/GvcTrade.scala`, config and array constructors | Literal sector counts and sector-index assumptions | `structural_invariant` / `empirical_input` | Core arrays and classification defaults | Versioned classification contract | Replace literals with selected classification cardinality; reject mismatched bundles | `open` |
| `ledger-storage-defaults` | `modules/ledger/.../MutableWorldState.scala`, `ValidatedBatchPlan.scala` | Dynamic store creation and sector-size snapshots | `structural_invariant` | Ledger topology | Ledger contract | Keep data-oriented storage rules in ledger; ensure sizes come from runtime topology | `open` |
| `zero-weight-allocation-fallbacks` | `engine/economics/banking/*`, `BankBondPortfolio.scala`, flow allocators | Unit-weight fallback arrays when all weights are zero | `initialization_policy` / `structural_invariant` | Algorithm fallback | Core allocation policy | Document fallback semantics and expose them in evidence when triggered | `open` |
| `institution-account-defaults` | `engine/flows/*Flows.scala`, `Nbp.scala`, `Jst.scala` | Account index defaults and fallback institution IDs | `structural_invariant` / `empirical_input` | Flow topology constants | Typed institution and account catalog | Replace raw integers with typed IDs; baseline supplies institution membership | `open` |
| `informal-economy-rates` | `config/InformalConfig.scala` | CIT/VAT/PIT/excise evasion shares and sector rates | `model_parameter` | Scala defaults | Model configuration / scenario | Add provenance and uncertainty; do not treat as observed population control | `open` |
| `monte-carlo-sampling` | `modules/montecarlo/.../core`, `runner`, `snapshots` | Seed ranges, horizon defaults, schedules, snapshot cadence, and metric sentinels | `model_parameter` / `structural_invariant` | Runner and schema defaults | Research-run configuration | Require explicit values in `ExperimentSpec`; keep schema sentinels in core | `open` |
| `research-bundle-contract` | `modules/model/.../research` | API, result-schema, validation-status, and evidence-policy defaults | `structural_invariant` | Research API contract | Research API versioned contract | Version explicitly; never infer baseline or evidence status | `open` |
| `tsv-parser-limits` | `modules/tsv/...` | Row numbering, field handling, and parser error conventions | `structural_invariant` | Parser implementation | Data contract | Keep in core and test physical-line preservation | `open` |
| `network-construction` | `modules/model/.../networks` | Adjacency sizes, empty-network behavior, and connection defaults | `initialization_policy` / `structural_invariant` | Network constructors | Typed population/institution topology policy | Separate topology policy from baseline relationship data | `open` |
| `accounting-matrix-defaults` | `accounting`, `accounting/matrix` | Zero rows, tolerance defaults, and evidence fallback labels | `structural_invariant` | Accounting implementation | SFC evidence contract | Keep as contract values; expose tolerance versions in result manifests | `open` |
| `closed-month-defaults` | `engine/closedmonth`, `MonthClosing.scala` | Closing-stage empty states, residual handling, and diagnostic defaults | `initialization_policy` / `structural_invariant` | Runtime closing code | Core month-boundary policy | Verify no empirical stock is created by a closing fallback | `open` |
| `classification-crosswalks` | `config/ProductionSectorCrosswalk.scala`, sector bridge exports | PKD/sector IDs, ordering, and crosswalk fallbacks | `empirical_input` / `structural_invariant` | Embedded classification definitions | Versioned economy classification component | Pin classification version and reject mixed crosswalks | `open` |
| `remittance-and-tourism` | `config/RemittanceConfig.scala`, `TourismConfig.scala`, `OpenEconConfig.scala` | Remittance, visitor, and external-sector shares/rates | `model_parameter` / `empirical_input` | Scala defaults and Poland-oriented assumptions | Baseline exogenous assumptions or scenario | Add source vintage and distinguish residents from visitors | `open` |
| `pricing-and-contract-rules` | `engine/markets/CalvoPricing.scala`, `agents/ContractType.scala` | Price reset probability, contract durations, and wage/price floors | `model_parameter` | Model defaults | Versioned model configuration | Record calibration status and uncertainty interval | `open` |
| `external-sector-bridges` | `engine/markets/OpenEconomy.scala`, `GvcTrade.scala`, `FdiConfig.scala` | Trade, FDI, FX and foreign-demand envelopes | `empirical_input` / `model_parameter` | Defaults and bridge arrays | Baseline external-sector component | Separate observed flows from behavioural response rules | `open` |
| `banking-risk-thresholds` | `agents/banking/*`, `engine/economics/banking/*` | CAR/LCR/NPL thresholds, spreads, and failure counters | `model_parameter` / `structural_invariant` | Embedded fixed-point constants and config | Banking model configuration | Mark regulatory values by jurisdiction and vintage | `open` |
| `io-path-and-artifact-defaults` | `config/IoConfig.scala`, `modules/tsv/TsvFile.scala`, CLI output helpers | Default input/output paths, filenames, and artifact naming | `runtime_default` / `structural_invariant` | Process and filesystem conventions | Research runtime configuration | Require explicit output root and immutable artifact names | `open` |
| `randomness-contract` | `init/InitRandomness.scala`, `random/*`, Monte Carlo runners | Stream partitioning, seed derivation, and random-source ordering | `structural_invariant` | Core deterministic execution contract | Research API/run manifest | Version stream layout and include seed contract in manifests | `open` |
| `scenario-identity-registry` | `config/ScenarioRegistry.scala`, scenario config files | Scenario IDs, labels, baseline compatibility and patch defaults | `model_parameter` / `structural_invariant` | Scala registry | Versioned Research API scenario catalog | Require explicit baseline compatibility and immutable scenario IDs | `open` |
| `calibration-export-contracts` | `modules/cli/...Calibration*Export.scala`, bridge exporters | Export columns, evidence labels, tolerances, and diagnostic defaults | `test_or_diagnostic` / `structural_invariant` | CLI renderers | Result-bundle/evidence schema | Keep diagnostic values out of runtime baseline and version output schemas | `open` |
| `monte-carlo-output-layout` | `modules/montecarlo/.../McOutputFiles.scala`, TSV schemas | Snapshot filenames, schedule defaults, sentinel values, and output paths | `structural_invariant` / `runtime_default` | Monte Carlo IO code | Research result-bundle contract | Include schema version and selected baseline in every output | `open` |
| `feature-flags-and-gates` | CLI, diagnostics, engine and config modules | Boolean switches controlling optional mechanisms, evidence, and heavy runs | `runtime_default` / `model_parameter` | Compile-time and CLI defaults | Explicit experiment/runtime configuration | Require flag provenance and include active flags in result manifests | `open` |
| `policy-enums-and-modes` | Config enums and engine policy objects | Representation, allocation, failure, ownership, and validation modes | `model_parameter` / `structural_invariant` | Scala enum defaults | Versioned model policy and baseline compatibility | Record selected mode in baseline/run metadata | `open` |
| `execution-resource-limits` | Ledger, Monte Carlo, CLI and engine packages | Batch sizes, capacities, cache bounds, timeouts, and parallelism | `structural_invariant` / `runtime_default` | Implementation and runner defaults | Runtime execution profile | Keep separate from economic calibration; make resource profile explicit | `open` |
| `serialization-schema-contracts` | TSV, Monte Carlo schemas, Research API and manifests | Field order, schema versions, sentinels, and parser/renderer defaults | `structural_invariant` | Versioned artifact schemas | Research/result-bundle contract | Pin schema versions and reject incompatible artifacts | `open` |
| `error-taxonomy-and-gates` | Loader, engine failure, accounting and validation packages | Required checks, error categories, and rejection precedence | `structural_invariant` | `require`, `Either`, and typed errors | Core validation contract | Treat precedence and messages as tested API behaviour | `open` |

## Existing Evidence

`CalibrationProvenance.scala` is the current detailed register for many
parameter-level claims. Its accepted status tokens are `EMPIRICAL`,
`EMPIRICAL_TRANSFORMED`, `ASSUMED`, `TUNED_NEEDS_VALIDATION`, `POLICY_SCENARIO`,
`PLACEHOLDER`, and `UNKNOWN_SOURCE`. Future machine-readable inventory rows
must use one of these exact tokens and be checked by the corresponding parser
and tests.

The inventory is not complete until every production symbol reachable from
`WorldInit`, `SimParams`, configuration objects, and opening-state factories is
either linked to a row above or explicitly classified as a structural
invariant, test fixture, diagnostic-only value, or legacy compatibility value.

The ten-pass scan also found that not every suspicious literal is economic
calibration. Account indices, month markers, array dimensions, lower bounds,
and ledger identifiers are candidates for the structural-invariant category;
fallback rates, distribution noise, and Poland-scale envelopes remain model or
baseline candidates and require separate review.

The additional five-pass scan found two classes that must not be silently
discarded: numerical constants in fixed-point math (which are core algorithmic
invariants), and embedded institution/profile tables (which can be empirical
inputs even when represented as Scala collections). Both are now explicit rows
above.

## Package Coverage

The package-level review applies the same five passes to every production Scala
package under `modules/*/src/main/scala`: defaults/opening state, typed domain
literals, cardinality and indices, provenance/calibration markers, and
fallbacks/thresholds. This includes the model subpackages (`config`, `init`,
`agents`, `engine`, `markets`, `mechanisms`, `ledger`, `fp`, `research`,
`montecarlo`, `random`, `networks`, and `util`) as well as the standalone
`ledger`, `tsv`, `cli`, and `montecarlo` modules. Package-local constants that
are purely implementation details remain classified as structural invariants;
economic-looking values must be promoted to an explicit row before migration.

The repeated package audit produced 49 package-directory records. The five
passes yielded 354, 538, 206, 595, and 1,046 textual matches respectively.
These are candidate counts, not counts of economic parameters: one symbol can
match several filters, and most matches are structural or diagnostic. The
inventory therefore records families and requires field-level closure before a
value can be moved into a baseline.

The latest five-pass package review used date/vintage identifiers, collection
and registry literals, unit/classification strings, filesystem/environment
defaults, and seed/determinism rules. It found additional concerns in IO path
defaults, random stream partitioning, scenario identity, calibration export
schemas, and Monte Carlo output layout; these are recorded above rather than
being mistaken for empirical calibration.

The latest five-pass review added the non-numeric contract layer: feature flags,
policy enums, execution-resource limits, serialization schemas, and error
taxonomy/rejection gates. These values can materially change a run even when no
economic number changes, so they must be versioned or explicitly classified as
core invariants.

## Audit Metadata

- Scan date: 2026-07-27.
- Source commit: `a75cf293`.
- Audit tool: `rg` package scans executed from the repository root.
- Query definition: five passes for each production package covering dates and
  vintages; collection and registry literals; units and classifications;
  filesystem and environment defaults; and seeds and determinism.
- Package manifest: the 49 package-directory records under
  `modules/*/src/main/scala` enumerated by `find`.
- The inventory itself is the checked-in audit record; reruns must update its
  metadata and counts in the same commit.

The source commit and query definition make the counts reproducible; a future
rerun must update this metadata and replace stale counts rather than silently
appending a new claim.

## Completion Rule

A row is closed only when every production symbol in the audited package set,
and every symbol reachable from `WorldInit`, `SimParams`, configuration objects,
and opening-state factories, is either linked to an inventory row or listed as
an authoritative exclusion. Each closed row must have:

1. a typed owner;
2. a unit and scope;
3. a source or an explicit model-policy rationale;
4. a version/effective period where applicable;
5. a validation or reconciliation check; and
6. a migration decision: baseline artifact, model policy, invariant, test-only,
   or deletion.

## Release Triage

`target_release` is the release by which a row must have an explicit owner and
contract. It does not mean that every row becomes empirical data in that
release.

| Priority | Meaning | Default target |
| --- | --- | --- |
| `P0` | Required to construct and validate the first published Polish population/baseline slice. | `PL-2025-Q4-v1` (historical milestone; implementation target) |
| `P1` | Required for reproducible researcher execution and result interpretation. | `research-api-v0` |
| `P2` | Important economic coverage, but not required for the first end-to-end population run. | `PL-2025-Q4-v2` (historical milestone; deferred target) |
| `P3` | Core invariant, runtime concern, diagnostic-only value, or explicit removal work. | `core-invariant-v1`, `runtime-profile-v1`, or `legacy-removal` |

### P0 — `PL-2025-Q4-v1`

These are the only families that currently block the first baseline-backed
population run:

- `population.scale`, `firm.population`;
- `classification-crosswalks`, `fixed-sector-cardinality`;
- `opening.bank-profiles`, `banking.opening-stocks`;
- `household.opening-stocks`, `housing.opening-state`;
- `social.demographics`, `regional-labour`;
- `firm.sector-bridges` and the population-to-workplace bridge;
- `baseline.manifest` and all source/digest/provenance fields needed to verify
  the bundle.

### P1 — `research-api-v0`

These are required before calling the system a reproducible research tool:

- `randomness-contract`;
- `scenario-identity-registry`;
- `research-bundle-contract`, `serialization-schema-contracts`;
- `feature-flags-and-gates`;
- `monte-carlo-sampling`, `monte-carlo-output-layout`;
- `calibration-export-contracts` and `error-taxonomy-and-gates`.

### P2 — `PL-2025-Q4-v2`

Defer these until the first baseline-backed run exists:

- `remittance-and-tourism`;
- `external-sector-bridges`;
- `nbfi-opening-state`, `insurance-opening-state`;
- `informal-economy-rates`;
- `pricing-and-contract-rules`;
- `bank-default-profiles`, `banking-risk-thresholds`;
- `eu-funds-envelope` and other secondary institutional bridges.

### P3 — explicit non-baseline work

- `fixed-point-math-constants`, `flow-account-indices`, `ledger-storage-defaults`,
  `monthly-calendar`, `state-zero-constructors`: `core-invariant-v1`;
- `execution-resource-limits`, `io-path-and-artifact-defaults`:
  `runtime-profile-v1`;
- `simparams.defaults`, `legacy` scenario defaults: `legacy-removal`;
- diagnostic-only exporters and probes: `research-api-v0` schema compatibility,
  but never baseline calibration.

All rows not named in P0 or P1 are now considered deferred rather than active
blockers. They remain in the inventory so they cannot be forgotten, but they do
not expand the first implementation slice.
