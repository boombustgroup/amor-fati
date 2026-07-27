# Documentation Index

This is the index for Amor Fati documentation. It keeps the research-facing
reading path, operational appendices, generated evidence, and complete document
inventory in one place.

This document does not define model behavior. The documentation hygiene check
requires every committed artifact under `docs/` to appear in the complete
inventory below, so new documentation cannot silently become orphaned.

## Start Here

| Need | Start here |
| --- | --- |
| Model scope, intended use, and limitations | [Model card](model-card.md) |
| SFC + ABM background for new readers | [SFC + ABM reading map](background/reading-map.md) |
| First scientific review | [Model specification reviewer reading path](model-specification.md#reviewer-reading-path) |
| ABM structure and scheduling | [ODD / ODD+D model documentation](odd-model-documentation.md) |
| Sector equations and decision rules | [Model specification source map](model-specification.md#source-map) |
| SFC matrix and ledger-derived evidence | [SFC matrix evidence](sfc-matrix-evidence.md) and [model equations to SFC map](model-equations-to-sfc-map.md) |
| Calibration and empirical validation | [Calibration register](calibration-register.md), [data bridge](data-bridge-national-financial-accounts.md), and [empirical validation report](empirical-validation-report.md) |
| Proposed population, baseline, and representation scale | [Population and representation RFC](rfc/0001-population-and-representation.md) |
| Implemented population-control component format | [Population-control bundle format](population-control-bundle.md) |
| Implemented enterprise-control component format | [Enterprise-control bundle format](enterprise-control-bundle.md) |
| Proposed public Research API and notebook runtime | [Research API and notebook runtime RFC](rfc/0002-research-api-and-notebook-runtime.md) |
| Proposed model ontology and state architecture | [Model ontology and state architecture RFC](rfc/0003-model-ontology-and-state-architecture.md) |
| Proposed JVM, JIT, GC, and worker-runtime policy | [JVM runtime, JIT, and garbage collection policy RFC](rfc/0004-jvm-runtime-jit-and-garbage-collection-policy.md) |
| Code architecture and extension paths | [Architecture documentation](architecture/index.md) |
| Commands, CI, diagnostics, scenarios, and local outputs | [Operations appendix index](operations.md#operational-appendix-index) |

## Operational Docs

Operational, diagnostic, calibration, profiling, and scenario documents remain
appendices unless the model specification explicitly promotes them into the
first-pass reviewer path. Use [operations.md](operations.md#operational-appendix-index)
as the operational entry point.

## Owner Classes

| Owner class | Meaning |
| --- | --- |
| Canonical reviewer spine | Paper-facing model contract or primary reviewer reference. These files define how the implemented model should be read. |
| Generated evidence | Committed output produced by a task or exporter. Do not edit by hand; regenerate through the owning command. |
| Hand-maintained companion to generated evidence | Human-written bridge, index, or workflow note that explains generated evidence and must stay aligned with it. |
| Calibration or empirical evidence | Parameter, source, calibration, or empirical-validation evidence. These files do not define parameter governance policy; deeper governance belongs to the [Calibration Governance milestone](https://github.com/amor-fati-systems/amor-fati/milestone/27). |
| Architecture | Code-facing architecture contract, package-boundary map, runtime-loop explanation, state ownership boundary, or extension guide. |
| Operational appendix | Commands, CI, validation ownership, scenarios, robustness, and runbook material. Useful, but not the first scientific reading path. |
| Diagnostics or profiling appendix | Specific diagnostic/profiling methodology, exporter interpretation, or investigation evidence. |
| ADR or decision record | Durable architectural or semantic decision record. Preserve history unless superseded explicitly. |
| Background / reading map | Orientation material for readers who need conceptual routing before the model specification, without defining model behavior. |
| Design proposal / RFC | Proposed model or architecture contract under review. It is not canonical behavior or an accepted decision until promoted explicitly. |

## Complete Inventory

| Artifact | Owner class | Purpose |
| --- | --- | --- |
| [docs/background/reading-map.md](background/reading-map.md) | Background / reading map | Orientation map for SFC, ABM, SFC-ABM literature, and Amor Fati's ledger-first modeling stance. |
| [docs/data-bridge-national-financial-accounts.md](data-bridge-national-financial-accounts.md) | Calibration or empirical evidence | Empirical source bridge from Polish, EU, and financial-account data to initialization, calibration, scenarios, and validation. |
| [docs/empirical-validation-report.md](empirical-validation-report.md) | Calibration or empirical evidence | Interpretation contract for empirical-validation artifacts, status taxonomy, snapshot bundle, and source-manifest expectations. |
| [docs/empirical-validation-source-manifest.tsv](empirical-validation-source-manifest.tsv) | Calibration or empirical evidence | Editable source manifest used by empirical-validation export. |
| [docs/enterprise-control-bundle.md](enterprise-control-bundle.md) | Architecture | Data-only TSV format, integrity rules, source residual semantics, and reconciliation boundary for the enterprise-control baseline component. |
| [docs/baselines/pl-2025-q4-v1-enterprise-controls.md](baselines/pl-2025-q4-v1-enterprise-controls.md) | Calibration or empirical evidence | Acquisition record, measurement boundaries, source hierarchy, and publication gates for the planned first empirical enterprise-control component. |
| [docs/baselines/pl-2025-q4-v1-population-controls.md](baselines/pl-2025-q4-v1-population-controls.md) | Calibration or empirical evidence | Acquisition record, source hierarchy, transformations, and publication gates for the planned first empirical population-control component. |
| [docs/external-sector-baseline-calibration.md](external-sector-baseline-calibration.md) | Calibration or empirical evidence | External-sector baseline calibration note and evidence from the related diagnostic ticket. |
| [docs/household-credit-stress-calibration.md](household-credit-stress-calibration.md) | Calibration or empirical evidence | Household credit stress calibration note and diagnostic evidence. |
| [docs/private-credit-renewal-calibration.md](private-credit-renewal-calibration.md) | Calibration or empirical evidence | Private-credit renewal calibration note and baseline diagnostic evidence. |
| [docs/banking-and-financial-sector-equations.md](banking-and-financial-sector-equations.md) | Canonical reviewer spine | Paper-facing banking and financial-sector equations, timing, SFC mapping, diagnostics, and limitations. |
| [docs/behavioral-equations-and-decision-rules.md](behavioral-equations-and-decision-rules.md) | Canonical reviewer spine | Detailed rule catalog for household, firm, bank, fiscal, monetary, external, and non-bank behavior. |
| [docs/engine-invariants-and-semantics.md](engine-invariants-and-semantics.md) | Canonical reviewer spine | Reviewer-facing index of hard invariants, normal-path expectations, limitations, enforcement points, and coverage. |
| [docs/firm-equations.md](firm-equations.md) | Canonical reviewer spine | Paper-facing firm-sector equations, implementation anchors, SFC mapping, outputs, validation, and limitations. |
| [docs/household-equations.md](household-equations.md) | Canonical reviewer spine | Paper-facing household-sector equations, implementation anchors, SFC mapping, outputs, validation, and limitations. |
| [docs/institutional-sector-equations.md](institutional-sector-equations.md) | Canonical reviewer spine | Paper-facing public, monetary, external, insurance, NBFI, TFI, quasi-fiscal, and JST equations. |
| [docs/model-card.md](model-card.md) | Canonical reviewer spine | Intended use, uses outside scope, evidence status, known limitations, reproducibility, and responsible interpretation. |
| [docs/model-notation-and-state-vector.md](model-notation-and-state-vector.md) | Canonical reviewer spine | Canonical notation, state-vector definitions, symbols, ownership boundaries, and implementation anchors. |
| [docs/population-control-bundle.md](population-control-bundle.md) | Architecture | Data-only TSV format, integrity rules, source metadata, and reconciliation boundary for the population-control baseline component. |
| [docs/rfc/README.md](rfc/README.md) | Design proposal / RFC | Index and lifecycle policy for active, implemented, and superseded RFCs. |
| [docs/model-specification.md](model-specification.md) | Canonical reviewer spine | Canonical publication-facing model specification, reviewer reading path, and first scientific entry point. |
| [docs/monthly-transition-function.md](monthly-transition-function.md) | Canonical reviewer spine | Formal monthly transition contract from pre-boundary state through same-month economics, ledger execution, SFC validation, and next-pre materialization. |
| [docs/odd-model-documentation.md](odd-model-documentation.md) | Canonical reviewer spine | ODD/ODD+D model documentation for ABM structure, scheduling, entities, observations, and decision-making. |
| [docs/rfc/0001-population-and-representation.md](rfc/0001-population-and-representation.md) | Design proposal / RFC | Semantically resolved population specialization with pending implementation work for reference-economy compilation, representation scale, population storage, firms, migration, tourism, and opening relationships. |
| [docs/rfc/0002-research-api-and-notebook-runtime.md](rfc/0002-research-api-and-notebook-runtime.md) | Design proposal / RFC | Draft public Research API, experiment lifecycle, result-query, reproducibility, committed-notebook, and managed Almond/Jupyter runtime contract. |
| [docs/rfc/0002-research-api-contract.md](rfc/0002-research-api-contract.md) | Design proposal / RFC | Working Research API contract for baseline discovery and loading, scenario composition, historical research modes, validation-data separation, compatibility, and migration from `SimParams`. |
| [docs/rfc/0003-model-ontology-and-state-architecture.md](rfc/0003-model-ontology-and-state-architecture.md) | Design proposal / RFC | Draft model-wide ontology and state architecture for units, relationships, instruments, assets, representation resolution, state lifetimes, and data-oriented storage. |
| [docs/rfc/0003-model-ontology-matrix.md](rfc/0003-model-ontology-matrix.md) | Design proposal / RFC | Current-to-target ontology audit, ownership map, first-resolution recommendations, and decision register for RFC-0003. |
| [docs/rfc/0004-jvm-runtime-jit-and-garbage-collection-policy.md](rfc/0004-jvm-runtime-jit-and-garbage-collection-policy.md) | Design proposal / RFC | Draft supported JDK, worker-process, JIT, GC-profile, heap, runtime-provenance, and qualification-evidence policy. |
| [docs/stochastic-processes-and-replay.md](stochastic-processes-and-replay.md) | Canonical reviewer spine | Publication-facing randomness, seed, stream, replay, validation, and stochastic-limitation contract. |
| [README.md](../README.md) | Canonical reviewer spine | Repository front door, status overview, model identity, and top-level documentation entry. |
| [docs/architecture/extension-points.md](architecture/extension-points.md) | Architecture | Code-facing recipes for adding flow mechanisms, same-month economics stages, agents/sectors, scenarios, diagnostics, output columns, and tests. |
| [docs/architecture/index.md](architecture/index.md) | Architecture | Entry point for code-facing architecture documentation, source anchors, and maintenance rules. |
| [docs/architecture/overview.md](architecture/overview.md) | Architecture | Cross-package architecture map, layering rules, runtime core, and evidence layers. |
| [docs/architecture/runtime-loop.md](architecture/runtime-loop.md) | Architecture | Code-level one-month runtime path from `SimState` and explicit randomness through ledger execution, SFC validation, and `StepOutput`. |
| [docs/architecture/state-and-ledger-boundary.md](architecture/state-and-ledger-boundary.md) | Architecture | Ownership contract for behavioral state, macro/runtime state, ledger-owned financial stocks, runtime survivability, and supported delta materialization. |
| [docs/adr/0001-ledger-first-runtime.md](adr/0001-ledger-first-runtime.md) | ADR or decision record | Decision record for executing runtime monetary flows through the ledger-first path. |
| [docs/adr/0002-explicit-month-boundary.md](adr/0002-explicit-month-boundary.md) | ADR or decision record | Decision record for the explicit `FlowSimulation.SimState` month boundary and caller-owned randomness schedule. |
| [docs/adr/0003-separate-verified-ledger-repository.md](adr/0003-separate-verified-ledger-repository.md) | ADR or decision record | Decision record for keeping the verified accounting kernel in the separate `amor-fati-ledger` repository, checked out under `modules/ledger`. |
| [docs/adr/0004-ledger-owned-financial-state.md](adr/0004-ledger-owned-financial-state.md) | ADR or decision record | Decision record for keeping supported ledger-backed financial stocks in `LedgerFinancialState`. |
| [docs/adr/0005-fixed-point-domain-numerics.md](adr/0005-fixed-point-domain-numerics.md) | ADR or decision record | Decision record for using Long-backed fixed-point opaque types for domain numerics instead of untyped floating-point values. |
| [docs/adr/0006-data-oriented-high-cardinality-state.md](adr/0006-data-oriented-high-cardinality-state.md) | ADR or decision record | Decision record for columnar high-cardinality runtime state with typed control and observation boundaries. |
| [docs/adr/0007-controlled-model-core-replacement.md](adr/0007-controlled-model-core-replacement.md) | ADR or decision record | Decision record for replacing the stateful model core on the target ontology while preserving verified accounting and scientific infrastructure. |
| [docs/adr/0008-explicit-reference-population-and-representation-scale.md](adr/0008-explicit-reference-population-and-representation-scale.md) | ADR or decision record | Proposed decision separating a versioned reference population from explicit run-level representation scale and weights. |
| [docs/adr/0009-research-api-as-supported-scientific-interface.md](adr/0009-research-api-as-supported-scientific-interface.md) | ADR or decision record | Proposed decision establishing the versioned Research API as the supported programmatic scientific interface. |
| [docs/adr/0010-managed-almond-jupyter-runtime-and-committed-notebooks.md](adr/0010-managed-almond-jupyter-runtime-and-committed-notebooks.md) | ADR or decision record | Proposed decision for a managed Almond/Jupyter environment and committed executable research notebooks. |
| [docs/adr/0011-first-target-model-ontology-and-resolution-boundaries.md](adr/0011-first-target-model-ontology-and-resolution-boundaries.md) | ADR or decision record | Accepted first-target ontology and representation boundaries for units, relationships, contracts, assets, institutions, and resolution promotion. |
| [docs/adr/0012-synthetic-workplace-for-first-target-employment.md](adr/0012-synthetic-workplace-for-first-target-employment.md) | ADR or decision record | Accepted decision defining the synthetic workplace as the first-target labor and production locus, with baseline location provenance separate from ontology. |
| [docs/adr/README.md](adr/README.md) | ADR or decision record | Index and format note for architecture decision records. |
| [docs/bank-balance-sheet-benchmark.md](bank-balance-sheet-benchmark.md) | Diagnostics or profiling appendix | Bank balance-sheet benchmark diagnostic and bank-capital source interpretation. |
| [docs/bank-failure-ablations.md](bank-failure-ablations.md) | Diagnostics or profiling appendix | Bank-failure ablation diagnostic methodology and interpretation. |
| [docs/hh-bank-lead-lag-diagnostics.md](hh-bank-lead-lag-diagnostics.md) | Diagnostics or profiling appendix | Household-to-bank lead-lag diagnostic methodology and interpretation. |
| [docs/hot-path-profiling.md](hot-path-profiling.md) | Diagnostics or profiling appendix | Profiling workflow for hot execution paths and performance evidence. |
| [docs/loan-origination-quality-diagnostics.md](loan-origination-quality-diagnostics.md) | Diagnostics or profiling appendix | Loan-origination quality diagnostic methodology and interpretation. |
| [docs/calibration-register.md](calibration-register.md) | Generated evidence | Generated parameter register from `calibrationRegister`; records parameters, units, owners, provenance, and gaps. |
| [docs/empirical-source-extracts/io-technical-coefficients.tsv](empirical-source-extracts/io-technical-coefficients.tsv) | Generated evidence | Generated GUS source comparison for six-sector I-O technical coefficients. |
| [docs/empirical-source-extracts/opening-bank-balance-profile.tsv](empirical-source-extracts/opening-bank-balance-profile.tsv) | Generated evidence | Generated opening banking-sector profile bridge for named banks and the residual `Other banks` row. |
| [docs/empirical-source-extracts/production-sector-gva-shares.tsv](empirical-source-extracts/production-sector-gva-shares.tsv) | Generated evidence | Generated GUS-primary and Eurostat-allocation source extract for six-sector production GVA shares. |
| [docs/empirical-source-extracts/production-sector-labor-bridges.tsv](empirical-source-extracts/production-sector-labor-bridges.tsv) | Generated evidence | Generated GUS/Eurostat source extract for sector firm-population, employment, and wage-ratio bridges. |
| [docs/empirical-validation/baseline-validation-snapshot.tsv](empirical-validation/baseline-validation-snapshot.tsv) | Generated evidence | Generated empirical-validation baseline snapshot. |
| [docs/empirical-validation/model-run-manifest.tsv](empirical-validation/model-run-manifest.tsv) | Generated evidence | Generated model-run manifest for the empirical-validation snapshot. |
| [docs/empirical-validation/source-manifest.tsv](empirical-validation/source-manifest.tsv) | Generated evidence | Generated copy of the empirical-validation source manifest captured with the snapshot. |
| [docs/sfc-matrix-artifacts/flow-mechanism-semantics.md](sfc-matrix-artifacts/flow-mechanism-semantics.md) | Generated evidence | Generated audit table for every runtime `FlowMechanism`, including topology, asset class, SFC impact, survivability, and coverage. |
| [docs/sfc-matrix-artifacts/matrix-mapping.md](sfc-matrix-artifacts/matrix-mapping.md) | Generated evidence | Generated mapping from symbolic BSM/TFM rows to runtime assets, mechanisms, and coverage notes. |
| [docs/sfc-matrix-artifacts/stock-flow-reconciliation.md](sfc-matrix-artifacts/stock-flow-reconciliation.md) | Generated evidence | Generated executed-run evidence for exact SFC identities and stock-flow reconciliation. |
| [docs/sfc-matrix-artifacts/symbolic-bsm.md](sfc-matrix-artifacts/symbolic-bsm.md) | Generated evidence | Generated symbolic Balance Sheet Matrix snapshot. |
| [docs/sfc-matrix-artifacts/symbolic-tfm.md](sfc-matrix-artifacts/symbolic-tfm.md) | Generated evidence | Generated symbolic Transactions Flow Matrix snapshot. |
| [docs/empirical-validation/README.md](empirical-validation/README.md) | Hand-maintained companion to generated evidence | Source contract for the generated empirical-validation snapshot TSVs. |
| [docs/model-equations-to-sfc-map.md](model-equations-to-sfc-map.md) | Hand-maintained companion to generated evidence | Human-maintained bridge from equation families to generated SFC rows, identities, mechanisms, and known accounting limits. |
| [docs/sfc-matrix-evidence.md](sfc-matrix-evidence.md) | Hand-maintained companion to generated evidence | Workflow and source-contract document for ledger-derived SFC matrix artifacts and generated SFC evidence. |
| [docs/nightly-baseline-comparison.md](nightly-baseline-comparison.md) | Operational appendix | Nightly diagnostic baseline comparison policy and report semantics. |
| [docs/nightly-diagnostics.md](nightly-diagnostics.md) | Operational appendix | Diagnostic profile taxonomy, nightly/extended execution semantics, and manifest policy. |
| [docs/operations.md](operations.md) | Operational appendix | Command-oriented guide for local setup, model runs, tests, diagnostics, scenario runs, output locations, and guards. |
| [docs/performance-regression-budgets.md](performance-regression-budgets.md) | Operational appendix | Performance regression budget policy and promotion criteria for profiling/diagnostic telemetry. |
| [docs/README.md](README.md) | Operational appendix | Documentation index and ownership inventory. |
| [docs/scenario-registry.md](scenario-registry.md) | Operational appendix | Named policy and shock scenario registry with reproducible scenario-run metadata. |
| [docs/sensitivity-robustness-workflow.md](sensitivity-robustness-workflow.md) | Operational appendix | Seed uncertainty and one-at-a-time robustness workflow for scenario and publication prep. |
| [docs/validation-matrix.md](validation-matrix.md) | Operational appendix | Ownership map for CI, integration tests, generated-output validation, nightly diagnostics, stress, and profiling. |
