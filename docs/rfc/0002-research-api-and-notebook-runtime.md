# RFC-0002: Research API and Notebook Runtime

Status: Draft for decision
Scope: Public research workflows, experiment lifecycle, result queries,
reproducibility manifests, committed notebooks, and the managed Almond/Jupyter
runtime
Performance: Resource controls are in scope; engine optimization is not

## Purpose

Amor Fati is a complete research system, not a library that researchers are
expected to assemble into their own applications. Its current executable
surfaces are nevertheless implementation-oriented: initialization, monthly
execution, Monte Carlo orchestration, scenario exports, and diagnostics are
available through internal Scala entry points or dedicated CLI commands. A
researcher cannot yet open an interactive environment, construct a documented
experiment, inspect typed results, modify a scenario, and rerun it through a
stable public contract.

This RFC defines that contract before the target data-oriented core is built.
Committed Scala notebooks become the first external-style consumers of Amor
Fati and expose missing abstractions while those abstractions can still be
changed. Almond supplies the Scala kernel for Jupyter, but Almond is an adapter
technology rather than the public API itself.

The design deliberately preserves Amor Fati's system distribution model. It
does not require publishing the model as a Maven artifact or asking researchers
to reconstruct a compatible classpath with notebook-local dependency
directives. Amor Fati owns and launches a tested notebook environment containing
the engine, Research API, baseline assets, Scala, JVM, Almond, and Jupyter
components required by a release.

## Decision Order

This RFC follows the semantic ontology gate in the target-core design:

1. [Population and representation RFC](0001-population-and-representation.md)
   defines the statistical population units, weights, and opening population
   relationships needed by the first target slice.
2. The semantic gate of the
   [model ontology and state architecture RFC](0003-model-ontology-and-state-architecture.md)
   fixes model-wide concepts, representation modes, authoritative owners, and
   state lifetimes.
3. This RFC defines what a researcher can configure, execute, observe, compare,
   and reproduce using those accepted semantics.
4. The physical gate of the model ontology and state architecture RFC uses the
   resulting access patterns to complete indexes, views, and data-oriented
   storage design.

The documents have different scopes rather than a class-inheritance hierarchy.
The model-wide RFC may constrain which observations can be authoritative, but
it must not redesign accepted population meaning or force researchers to
configure storage details.

The Research API contract should be resolved before the data-oriented state
layout is finalized. A pilot implementation may run against the current engine
to validate ergonomics. The target core then replaces that engine behind the
same conceptual boundary; compatibility with current internal classes is not a
goal.

Two proposed decision records extract the independently durable parts of this
RFC:

- [ADR-0009](../adr/0009-research-api-as-supported-scientific-interface.md)
  makes the Research API the supported scientific programmatic boundary; and
- [ADR-0010](../adr/0010-managed-almond-jupyter-runtime-and-committed-notebooks.md)
  selects the managed Almond/Jupyter runtime and committed notebook policy.

Both remain Proposed until the relevant open decisions and acceptance criteria
in this RFC are resolved.

The [Research API contract companion](0002-research-api-contract.md) audits the
current configuration surface and maintains the detailed contract and decision
register. Its first slice covers baseline discovery and loading, scenario
composition, historical research modes, validation-data separation, and the
temporary `SimParams` adapter boundary.

## Goals

1. Make a first scientifically meaningful Amor Fati experiment executable and
   inspectable from a committed notebook.
2. Give researchers typed domain operations rather than access to engine
   internals or primitive storage.
3. Make baseline, scale, scenario, seed, requested evidence, and software
   provenance explicit in every run.
4. Use the same application-level contract for notebooks, CLI workflows, and
   future service adapters.
5. Support interactive exploration without weakening deterministic batch runs,
   accounting validation, or the explicit month boundary.
6. Make canonical notebooks executable contract tests for API ergonomics and
   release compatibility.
7. Keep the engine and Research API independent of Almond and Jupyter.

## Non-Goals

This RFC does not:

- turn Amor Fati into a general-purpose Scala package;
- expose mutable data-oriented columns as a public microdata API;
- define the final visualization library or publication style;
- make a notebook the authoritative storage format for run evidence;
- provide a hosted multi-tenant notebook service;
- treat notebook execution as a security sandbox;
- replace CLI and unattended batch execution with an interactive kernel; or
- settle the population, contract, or state semantics owned by the preceding
  and following RFCs.

## Current Amor Fati Surface

Useful execution capabilities already exist, but they are not one coherent
research interface:

- `WorldInit.initialize` constructs the current world;
- `FlowSimulation.step` exposes the current explicit one-month transition;
- `McRunner` owns production Monte Carlo orchestration;
- `ScenarioRunExport` executes registry scenarios and writes tabular artifacts;
- diagnostic exporters create specialized evidence bundles; and
- generated manifests already capture parts of configuration, calibration, and
  version provenance.

These are implementation anchors and evidence sources. They do not become the
public Research API by being renamed or imported into a notebook. In
particular, `FlowSimulation.SimState`, current agent `case class` collections,
runtime ledger indices, and future primitive columns remain internal.

## Proposed Decisions

1. Amor Fati introduces a versioned **Research API** as the sole supported
   programmatic boundary for scientific experiment construction, execution,
   observation, and comparison.
2. The Research API is independent of Almond, Jupyter, terminal rendering, and
   any particular charting library.
3. CLI experiment commands and the notebook adapter converge on the same
   application services. They may format results differently but must not
   implement separate simulation semantics.
4. Amor Fati supplies a managed Almond-based kernel with a release-pinned and
   tested Scala, JVM, Almond, Jupyter, model, baseline-schema, and adapter
   combination. Each selected baseline is verified separately against that
   compatibility boundary. A declared labour margin may use a source-specific
   statistical universe; it is not implicitly treated as a partition of the
   full person population unless the manifest declares matching universes.
5. Researchers launch the managed system environment. Canonical notebooks do
   not resolve Amor Fati through `$ivy`, publish-local conventions, or an
   independently assembled classpath.
6. Experiment configuration is immutable, typed, validated, and complete before
   execution. Interactive convenience builds new specifications; it does not
   silently mutate a running experiment's provenance.
7. Researchers select an immutable baseline through a catalog. Baselines,
   scenarios, representation, execution, evidence, and validation datasets are
   separate manifested inputs; `SimParams` remains internal.
8. A run owns its mutable execution lifecycle. Results, snapshots, manifests,
   and researcher-facing views are immutable observations of accepted month
   boundaries.
9. Public queries use economic domain names, stable selectors, and controlled
   tabular or typed views. They never expose raw high-cardinality storage,
   allocator slots, buffer swaps, or ledger implementation indices.
10. Every execution that reaches a controlled terminal state emits a
   machine-readable run manifest and validation status. A notebook file or
   visible cell output is not sufficient provenance.
11. Canonical notebooks are committed, editable examples. Their stored outputs
    are cleared by default, and representative notebooks execute headlessly in
    CI against the release environment.
12. Notebook interruption is cooperative cancellation at defined execution
    boundaries. A cancelled or failed month cannot publish a partially closed
    simulation state.
13. The kernel is trusted local code execution. Hosted or multi-user deployment
    requires a separate isolation and tenancy design.

## Architecture Boundary

The target dependency direction is:

```text
                              +----------------------+
                              | CLI and batch tools  |
                              +----------+-----------+
                                         |
+------------------+          +----------v-----------+
| Almond/Jupyter   +----------> Research API         |
| adapter          |          | experiment services |
+------------------+          +----------+-----------+
                                         |
                              +----------v-----------+
                              | Simulation core      |
                              | and population core  |
                              +----------+-----------+
                                         |
                              +----------v-----------+
                              | Verified ledger      |
                              +----------------------+
```

The core does not import Almond or Jupyter APIs. Rich display, progress cells,
and kernel-specific cancellation belong to the adapter. A future HTTP or
enterprise adapter can use the Research API without depending on notebook code.

The exact sbt project split remains an implementation decision, but the logical
owners are distinct:

| Boundary | Responsibility |
| --- | --- |
| Simulation core | Baseline compilation, model state, monthly transition, accounting, deterministic execution. |
| Research API | Experiment specifications, validation, lifecycle, queries, result bundles, comparison, provenance. |
| Notebook adapter | Almond predef, rich display, progress integration, notebook-friendly rendering and launch diagnostics. |
| CLI adapter | Commands, unattended execution, filesystem destinations, terminal progress and exit status. |

## Research API Contract

### Experiment specification

An experiment specification contains at least:

- an immutable baseline reference resolved through the baseline catalog to an
  exact identity and content digest;
- representation scale and any declared `1:1` exceptions;
- scenario identity plus typed, validated patches, driver paths, or timed
  interventions;
- master seed and deterministic stream policy;
- start boundary derived from the resolved baseline or checkpoint, plus the
  requested horizon;
- requested aggregate, snapshot, event, trace, and diagnostic evidence;
- separately identified validation datasets, when requested;
- execution profile and resource limits; and
- Research API version.

Population size is not inferred from unrelated firm or worker parameters.
Baseline and representation semantics come directly from the accepted
population RFC. `SimParams`, baseline file locations, and parsed bundle DTOs
do not enter public signatures.

Conceptually, notebook code should read like:

```scala
val experiment = AmorFati.experiment(
  baseline = Baseline("PL-2025-Q4-v1"),
  representation = Representation.oneTo(1000),
  seed = Seed(42)
)

val result = experiment
  .withScenario(Scenario.monetaryTightening(/* typed changes */))
  .run(months = 60)

result.series(Measure.RealGdp)
result.series(Measure.UnemploymentRate)
result.snapshot(Month(12)).households.sample(1000)
result.manifest
```

Names in this example are illustrative rather than accepted Scala signatures.
The final API should optimize for discoverability, compiler assistance, and
scientific meaning rather than minimizing character count.

### Commands and lifecycle

The public lifecycle distinguishes specification from execution:

```text
ExperimentSpec
      |
      v validate and compile
PreparedExperiment
      |
      v start
SimulationHandle ---- step / run / cancel / checkpoint
      |
      v close successfully
RunResult + RunManifest + ValidationReport
```

The supported lifecycle should include:

- validate and prepare without running;
- execute one or many explicit months;
- run to a horizon;
- inspect progress and the latest accepted boundary;
- request cooperative cancellation;
- create a checkpoint at an accepted boundary;
- fork a new experiment from a supported checkpoint;
- run a deterministic seed ensemble; and
- close resources explicitly or through a scoped API.

Notebook cell order cannot redefine these semantics. Re-executing a cell may
create a new handle, but it cannot retroactively change the manifest of an
existing run.

### Results and queries

The initial observation surface should provide:

- aggregate time series with units and definitions;
- validation, SFC, ledger, and reconciliation status;
- population and representation summaries;
- controlled snapshots at requested month boundaries;
- filtered or sampled immutable micro views where enabled;
- scenario and seed-ensemble comparisons;
- warnings, omissions, and aggregate-only representation declarations; and
- export into canonical result-bundle formats.

Queries declare their evidence cost. A request for a GDP series must not force
retention of all person columns for every month. Micro snapshots, event traces,
and decision traces are opt-in and carry explicit cadence, selection, and size
limits.

Typed row views may use Scala `case class` values. This does not conflict with
data-oriented persistent storage because views are bounded observations rather
than the authoritative high-cardinality state.

### Mutation policy

Researchers modify experiments through validated domain operations:

- baseline selection;
- scenario composition;
- typed parameter patches;
- evidence requests;
- representation choices; and
- checkpoint forks.

The API does not support `state.copy(...)`, mutation of raw arrays, direct
balance replacement, or bypassing the monthly ledger and closing boundary.
Experimental interventions that alter state during a run require an explicit,
manifested intervention command with defined timing and validation rules.

## Managed Almond Runtime

### System-owned kernel

Amor Fati provides a dedicated kernel identity, provisionally displayed as
`Amor Fati (Scala)`. A system command such as `./amor-fati notebook` installs or
selects the release-compatible kernel and launches the supported Jupyter
environment.

The kernel definition owns:

- the Java command and JVM options;
- the compiled Amor Fati classpath;
- the Scala and Almond versions;
- a small predef importing the Research API and notebook renderers;
- baseline and result-directory discovery;
- startup validation and a visible environment summary; and
- any resource defaults required for safe local execution.

The exact command name and packaging mechanism remain open until the repository
distribution path is selected. The invariant is that a researcher launches an
Amor Fati environment rather than manually constructing a generic Scala kernel.
JDK selection, kernel/worker process ownership, JIT, GC, heap, and runtime
qualification belong to
[RFC-0004](0004-jvm-runtime-jit-and-garbage-collection-policy.md).

### Compatibility matrix

Almond's ability to launch Scala kernels does not by itself prove compatibility
with Amor Fati's compiler, TASTy, JVM, and dependency combination. Every Amor
Fati release that advertises notebook support records and tests one exact
combination. Upgrading Scala or Almond requires executing the canonical notebook
suite before promotion.

The exact JVM distribution and runtime profile are qualified through
[RFC-0004](0004-jvm-runtime-jit-and-garbage-collection-policy.md). The notebook
matrix must distinguish the Almond kernel JVM from any separately launched
simulation-worker JVM.

The notebook startup cell or predef exposes the resolved environment versions
and fails clearly if the kernel does not match the checked-out system release.
Silent fallback to another Scala version is not permitted.

### Kernel state

A Jupyter kernel is stateful, but scientific runs remain explicitly identified:

- each started run receives a unique run ID;
- active handles are listed and can be closed;
- restarting the kernel invalidates in-memory handles but not completed result
  bundles or explicit checkpoints;
- hidden cell history is not treated as provenance; and
- canonical notebooks execute correctly from a fresh kernel in top-to-bottom
  order.

## Committed Notebook Policy

The initial canonical notebook set should remain small and workflow-oriented:

```text
notebooks/
|-- 01-baseline-overview.ipynb
|-- 02-first-scenario.ipynb
|-- 03-scenario-comparison.ipynb
|-- 04-seed-ensemble.ipynb
`-- 05-microdata-inspection.ipynb
```

The exact filenames may change during implementation. Each canonical notebook:

1. states its required Amor Fati and Research API compatibility;
2. starts from a fresh managed kernel;
3. contains no machine-specific paths or credentials;
4. uses a bounded configuration suitable for CI;
5. demonstrates one coherent research workflow rather than serving as general
   product documentation;
6. persists evidence through the result-bundle API rather than notebook output;
7. has outputs cleared before commit unless a small output is explicitly
   accepted as documentation evidence; and
8. is executed headlessly in CI at an appropriate cadence.

Large run outputs, checkpoints, generated plots, and data extracts are not
committed beside notebook source by default. CI should reject accidental large
cell outputs and execution against an unsupported kernel.

## Reproducibility and Evidence

The Research API defines a run-manifest contract inside the result-bundle
contract. The Research API, result bundle, and run manifest are separately
versioned so that artifact or manifest evolution does not silently change the
programmatic API. Every run that reaches a controlled terminal state --
successful completion, cooperative cancellation, or a captured failure -- emits
a manifest containing at least:

- Amor Fati release and Git commit, including dirty-worktree status;
- Research API, result-bundle schema, and run-manifest schema versions;
- baseline ID, source vintage, and content digest;
- population compiler and representation manifest;
- complete validated experiment specification;
- seed and random-stream policy;
- Scala, JVM, Almond, and notebook-adapter versions;
- notebook digest when execution originated from a notebook;
- requested and retained evidence policy;
- start, completion, cancellation, or captured-failure status;
- validation and reconciliation results; and
- hashes of canonical output artifacts.

Abrupt JVM or host termination, process kill, and out-of-memory failure are
outside that terminal-manifest guarantee. After such an interruption, the
durable run representation is the last atomically committed checkpoint at an
accepted month boundary, if one exists. Atomically completed artifacts written
after that checkpoint may instead be exposed only through a result bundle
explicitly marked `INCOMPLETE`; that bundle must identify the last accepted
boundary and must not claim successful completion or complete validation.

The existing `EmpiricalValidationExport.ModelRunManifestTsvSchema` and
`docs/empirical-validation/model-run-manifest.tsv` are a partial legacy export,
not the complete Research API manifest. Their current seven-column record is a
useful empirical-validation snapshot locator, but it lacks the baseline,
representation, contract-version, evidence-policy, validation, reconciliation,
and canonical-artifact-hash fields required above. It may remain as a
compatibility artifact until the versioned result-bundle manifest supersedes it.

Notebook source is contextual evidence, not the run record. A modified notebook
can be reproduced only when its executed specification and environment are
captured independently of mutable kernel history.

## Rich Display

The notebook adapter may provide rich representations for:

- experiment specifications and validation errors;
- population and baseline manifests;
- progress and cancellation status;
- time-series and scenario-comparison tables;
- validation and reconciliation reports; and
- bounded microdata views.

Rich rendering consumes public result values. Core result classes must retain a
useful text representation and machine-readable export without Jupyter. A
charting failure cannot invalidate or change a simulation result.

## Resources, Cancellation, and Failure

Interactive execution requires explicit operational behavior:

- representation scale, horizon, seed count, snapshot cadence, and trace volume
  are validated against the selected execution profile;
- progress is observable without retaining unbounded event history;
- cancellation is checked at defined safe points;
- an interrupted month does not publish closing state or a successful result;
- abrupt out-of-memory or kernel termination does not promise a terminal
  manifest; completed artifact writes remain atomic and recovery exposes only
  the last committed checkpoint or an explicitly incomplete bundle;
- temporary work directories are run-scoped and recoverable; and
- parallel seed execution preserves the deterministic seed schedule.

The first release may support only one active local run per kernel if concurrent
handles would complicate memory ownership or output routing. This is preferable
to implicit contention.

## Security Boundary

A notebook executes arbitrary Scala, Java, native-library, and operating-system
code with the permissions of its process. The managed kernel is not a sandbox.
Canonical notebooks must not embed credentials, and the launcher must not
inject unrelated secrets into the environment.

Remote, shared, or enterprise notebook hosting requires a separate design for
authentication, process and filesystem isolation, network policy, quotas,
secret handling, audit, and tenancy. Those requirements must not leak into the
local Research API prematurely.

## API and Notebook Versioning

The Research API is versioned with Amor Fati releases even though it is not
published as a standalone package. A result manifest records the API version,
and every canonical notebook declares the supported version or release range.

Before the first supported Research API release, signatures may change freely
with the committed notebooks in the same change. After promotion:

- incompatible semantic or signature changes require an explicit API-version
  transition;
- result schemas evolve through documented, machine-readable versions;
- deprecations state a removal release or migration path; and
- canonical notebooks for a release remain reproducible from that release's
  managed environment.

Internal core and DOD layouts are not covered by this compatibility policy.

## Implementation Sequence

### Phase 0: accept the semantic ontology

Phase 0 is complete.
[ADR-0008](../adr/0008-explicit-reference-population-and-representation-scale.md)
accepts the population and representation semantics required for baseline
selection, scale, weights, population summaries, and opening relationships.
[ADR-0011](../adr/0011-first-target-model-ontology-and-resolution-boundaries.md)
accepts the model-wide semantic gate for units, relationships, contracts,
instruments, authoritative owners, representation modes, and state lifetimes.
Research API names must preserve those decisions rather than expose current
implementation-shaped substitutes.

### Phase 1: define the research contract

1. Specify the initial baseline catalog and bundle, experiment, lifecycle,
   manifest, result, query, and error types without Almond dependencies.
2. Map existing `SimParams`, initialization, scenario, Monte Carlo, manifest,
   and exporter capabilities to that boundary, preserving the distinction
   between baseline inputs, scenario drivers, and validation outcomes.
3. Implement a narrow pre-release Research API facade over the current engine,
   using an accurately identified legacy baseline provider until
   `PL-2025-Q4-v1` is compiled and validated.
   Current internal engine types do not enter its public signatures.
4. Keep the contract explicitly pre-release so notebook evidence can correct it
   before the target state design is fixed.

### Phase 2: deliver the pilot notebook environment

1. Pin a development compatibility matrix using the provisional control runtime
   qualified by RFC-0004, and provide the minimal system-owned Almond launcher,
   kernelspec, and predef.
2. Commit baseline-overview and scenario-comparison notebooks against the
   pre-release Research API.
3. Execute both notebooks from a fresh kernel in CI with bounded workloads and
   clean stored outputs.
4. Record every awkward internal escape, expensive query, missing selector, and
   ambiguous population concept exposed by those workflows.

The pilot is a real usable notebook surface, not a mock-up. It is not yet the
supported long-term API, and fidelity to current internal engine classes is not
a compatibility requirement for the target core.

### Phase 3: complete the physical target state design

Use the accepted population semantics, accepted model ontology, Research API
decisions, and observed query patterns to complete RFC-0003's physical
state-design gate. Define indexes, projections, snapshot costs, stable views,
and evidence retention before fixing DOD table layouts.

### Phase 4: implement the target core behind the boundary

Implement the native population compiler and thin end-to-end target-core month.
Move Research API services to the target core without a public dependency on
old agent classes or new primitive storage. API changes remain possible while
the Research API is explicitly pre-release.

Harden the pilot environment while the target core grows:

1. expand and release-pin the kernel compatibility matrix;
2. complete the system-owned launcher, predef, renderers, cancellation, and
   resource cleanup;
3. expand the canonical notebook set with clean outputs;
4. retain fresh-kernel, top-to-bottom CI execution and add output-size checks;
   and
5. document local operational recovery without making notebooks the only
   supported execution mode.

### Phase 5: promote the supported interface

Promote a Research API version only after canonical notebooks, CLI runs,
deterministic replay, manifests, resource cleanup, cancellation, and target-core
validation pass together. Future enterprise adapters build on this promoted
boundary rather than on notebook internals.

## Acceptance Criteria

The first supported notebook interface is complete when:

1. a researcher can launch the managed environment through one Amor Fati
   command without publishing or locally installing Amor Fati as a package;
2. a fresh kernel can execute the baseline and scenario notebooks from top to
   bottom;
3. the same experiment specification produces equivalent scientific results
   through notebook and unattended CLI execution;
4. every run that reaches a controlled terminal state produces a complete
   manifest and explicit validation status;
5. canonical results can be queried without importing engine-internal state;
6. representation scale and weighted counts are visible and unambiguous;
7. requested micro evidence is bounded and declared before execution;
8. cancellation cannot publish a partially closed month;
9. kernel restart and stale-handle failures are clear and recoverable; and
10. the notebook suite passes against the pinned Scala, JVM, Almond, and Amor
    Fati combination in CI.

## Open Decisions

The following decisions remain before this RFC can become an ADR:

1. The exact first-version Research API type and package names.
2. Whether a pilot Research API over the current engine is shipped or remains a
   test-only ergonomics harness until the target core is available.
3. The physical baseline-bundle serialization, canonical hashing, and asset
   distribution or verified-cache policy.
4. The exact result-table interchange type and its unit/metadata representation.
5. The checkpoint serialization format and compatibility policy.
6. Which micro queries and snapshot cadences are supported in the first release.
7. Whether one or multiple concurrent run handles are allowed per kernel.
8. The managed distribution mechanism and final notebook launch command.
9. The tested Scala, JVM, Almond, and Jupyter compatibility matrix.
10. Whether canonical `.ipynb` files remain the sole notebook source or gain a
   paired text representation for reviewable diffs.
11. The CI cadence for lightweight notebooks and heavier scientific examples.
12. The first visualization and tabular renderers included in the adapter.
13. The Research API compatibility and deprecation policy after its first
    supported release.

## Implementation Anchors

- [`WorldInit.scala`](../../modules/model/src/main/scala/com/boombustgroup/amorfati/init/WorldInit.scala)
  for current world initialization;
- [`FlowSimulation.scala`](../../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/flows/FlowSimulation.scala)
  for the current explicit month transition;
- [`McRunner.scala`](../../modules/montecarlo/src/main/scala/com/boombustgroup/amorfati/montecarlo/runner/McRunner.scala)
  for current seed-ensemble orchestration; and
- [`ScenarioRunExport.scala`](../../modules/cli/src/main/scala/com/boombustgroup/amorfati/diagnostics/ScenarioRunExport.scala)
  for current registry-scenario execution and artifact export.

Almond capability references:

- [Almond installation options](https://almond.sh/docs/install-options) for
  kernel identity, display name, command, arguments, and predef configuration;
- [Almond advanced installation](https://almond.sh/docs/next/install-advanced)
  for launcher and Scala-kernel installation behavior; and
- [Almond Jupyter API](https://almond.sh/docs/api-jupyter) for adapter-owned rich
  display and notebook integration.
