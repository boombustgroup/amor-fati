# Operations

This document is the command-oriented operating guide for Amor Fati. It is not
model documentation; it records how to run, test, inspect, and compare the
current codebase from a local checkout.

## Requirements

Use the same toolchain shape as CI:

- JDK 21 as the supported baseline, Temurin in CI
- sbt 1.11.6, pinned in `project/build.properties`
- Scala 3.8.2, pinned in `build.sbt`

Newer local JDKs may work, but diagnose toolchain failures against JDK 21
first.

No Docker, Nix, or binary release artifact is required for the current local
workflow.

The runtime depends on `amor-fati-ledger`, checked out as the Git submodule at
`modules/ledger`. A checkout without that submodule is incomplete.

## First Run

Clone with submodules:

```bash
git clone --recurse-submodules https://github.com/boombustgroup/amor-fati.git
cd amor-fati
```

If the repository was cloned without submodules:

```bash
git submodule update --init --recursive
```

Then validate the checkout from the repository root:

```bash
sbt test
```

The first run may spend most of its time downloading the sbt launcher,
plugins, Scala compiler artifacts, and library dependencies through Coursier.

## Independent Clone Or Fork Workflow

Use a clone or fork when the work is a private counterfactual branch, a larger
mechanism change, or an experiment that should remain comparable with the
project baseline. This workflow does not assume contribution back to the
canonical repository.

Clone the repository directly:

```bash
git clone --recurse-submodules git@github.com:boombustgroup/amor-fati.git
cd amor-fati
```

Or clone your own fork and keep the canonical repository as `upstream` so you
can refresh the baseline when needed:

```bash
git clone --recurse-submodules git@github.com:<user>/amor-fati.git
cd amor-fati
git remote add upstream git@github.com:boombustgroup/amor-fati.git
git fetch upstream
```

Keep `main` as the baseline branch and do research work on named local
branches. For a direct clone, refresh from `origin` before creating an
experiment branch:

```bash
git checkout main
git pull --ff-only origin main
git submodule update --init --recursive
git checkout -b experiment/<short-name>
```

For a fork, refresh from `upstream` instead:

```bash
git checkout main
git fetch upstream
git merge --ff-only upstream/main
git submodule update --init --recursive
git checkout -b experiment/<short-name>
```

Before changing behavior, record a baseline command and keep the seed, duration,
and scenario selection fixed for the counterfactual run. For example:

```bash
sbt "runMain com.boombustgroup.amorfati.Main 1 baseline --duration 12 --run-id baseline-smoke"
sbt "runMain com.boombustgroup.amorfati.Main 1 counterfactual --duration 12 --run-id <short-name>-smoke"
```

For named scenarios, compare the same scenario set across branches:

```bash
sbt "scenarioRun --scenarios baseline,monetary-tightening --seeds 1 --months 12 --run-id baseline-review --out target/scenarios"
sbt "scenarioRun --scenarios baseline,monetary-tightening --seeds 1 --months 12 --run-id <short-name>-review --out target/scenarios"
```

Commit code, tests, and intentionally refreshed docs. Do not commit generated
local outputs from `mc/` or `target/`.

Before treating an experiment as locally valid:

```bash
sbt scalafmtCheckAll
sbt test
```

Also run the relevant heavy, integration, diagnostic, scenario, or robustness
commands from this document when the change touches shared month execution,
flows, Monte Carlo output, or research-facing scenario behavior. Sharing a
branch or opening a PR is optional and outside the assumed operating path.

## Run The Model

The main runtime entrypoint is:

```bash
sbt "runMain com.boombustgroup.amorfati.Main <nSeeds> <prefix> [--duration <months>] [--run-id <id>]"
```

Example smoke run:

```bash
sbt "runMain com.boombustgroup.amorfati.Main 1 local-smoke --duration 12 --run-id smoke"
```

The main runner writes generated CSV files under `mc/`:

```text
mc/<prefix>_<run-id>_<months>m_seed001.csv
mc/<prefix>_<run-id>_<months>m_hh.csv
mc/<prefix>_<run-id>_<months>m_banks.csv
mc/<prefix>_<run-id>_<months>m_firms.csv
```

Per-seed time-series CSV files emit macro PLN aggregates in Poland scale, ready
for empirical analysis. The internal `gdpRatio` scaling factor is not emitted
as a CSV column; it remains a model-computation boundary. Agent-level prices,
wages, indexes, rates, shares, and counts remain in their native units.

`mc/` is ignored by git. Keep committed research-facing artifacts under `docs/`
only when the command explicitly targets a committed documentation path.

## Tests

Default local tests:

```bash
sbt test
```

`sbt test` skips suites tagged with `com.boombustgroup.amorfati.tags.Heavy`.
This keeps normal local and coverage runs cheap.

Run one spec:

```bash
sbt "testOnly com.boombustgroup.amorfati.engine.FirmEntrySpec"
```

Run all heavy root tests:

```bash
sbt -DamorFati.includeHeavyTests=true "root / Test / testOnly * -- -n com.boombustgroup.amorfati.tags.Heavy"
```

Run all root tests, including heavy tests:

```bash
sbt -DamorFati.includeHeavyTests=true "root / Test / test"
```

Run integration tests:

```bash
sbt "integrationTests / Test / test"
```

Run the CI-style non-heavy coverage pass:

```bash
sbt coverage "root / Test / test" coverageReport
```

Check formatting:

```bash
sbt scalafmtCheckAll
```

Apply formatting:

```bash
sbt scalafmtAll
```

## Diagnostics

Use diagnostics when a model change needs a narrow view into one mechanism or
one reporting surface.

Bankruptcy probe:

```bash
sbt "runMain com.boombustgroup.amorfati.diagnostics.runBankruptcyProbe 1 12"
```

Inflation probe:

```bash
sbt "runMain com.boombustgroup.amorfati.diagnostics.runInflationProbe 1 12"
```

Labor demand probe:

```bash
sbt "runMain com.boombustgroup.amorfati.diagnostics.runLaborDemandProbe 1 2"
```

Generate scratch SFC matrix artifacts:

```bash
sbt "sfcMatrices --seed 1 --months 12 --out target/sfc-matrices"
```

Refresh the committed SFC matrix Markdown snapshots only when the matrix
artifacts themselves intentionally change:

```bash
sbt "sfcMatrices --seed 1 --months 12 --out docs/sfc-matrix-artifacts --format md"
```

## Scenario And Robustness Runs

Named scenarios come from
`src/main/scala/com/boombustgroup/amorfati/config/ScenarioRegistry.scala` and
are documented in [scenario-registry.md](scenario-registry.md).

Run a small scenario smoke check:

```bash
sbt "scenarioRun --scenarios baseline,monetary-tightening,fiscal-expansion --seeds 1 --months 12 --run-id scenario-smoke --out target/scenarios"
```

Run every registered scenario with a small seed envelope:

```bash
sbt "scenarioRun --scenarios all --seeds 2 --months 24 --run-id local-review --out target/scenarios"
```

Robustness scenario sets come from
`src/main/scala/com/boombustgroup/amorfati/config/RobustnessScenarios.scala` and
are documented in
[sensitivity-robustness-workflow.md](sensitivity-robustness-workflow.md).

Fast robustness smoke check:

```bash
sbt "robustnessReport --scenario-set smoke --seeds 1 --months 6 --out target/robustness-smoke"
```

Local robustness review:

```bash
sbt "robustnessReport --scenario-set core --seeds 2 --months 24 --out target/robustness"
```

## Seeds And Comparisons

For counterfactual comparisons, keep these fixed unless the seed policy itself
is part of the experiment:

- seed count or seed range
- duration in months
- scenario id or changed parameter set
- run id and output folder naming convention

The main Monte Carlo runner uses seeds `1..nSeeds`. Diagnostic entrypoints
accept explicit seed arguments. The monthly runtime randomness is derived
deterministically from the seed and month boundary, so equal seeds and equal
parameters are the basis for reproducible comparisons.

## Output Locations

Generated local outputs normally belong in ignored paths:

| Path | Producer | Commit? |
| --- | --- | --- |
| `mc/` | Main Monte Carlo runner | No |
| `target/sfc-matrices/` | Scratch SFC matrix exports | No |
| `target/scenarios/` | Scenario registry runs | No |
| `target/robustness*` | Robustness reports | No |
| `docs/sfc-matrix-artifacts/` | Intentional committed matrix snapshots | Yes, only when refreshed intentionally |

## Working Loop

A practical local loop for model changes:

1. Make a focused mechanism, parameter, or accounting change.
2. Execute the most specific affected spec with `sbt "testOnly ..."`.
3. Then run `sbt test`.
4. Perform a narrow diagnostic if the change touches bankruptcy, labor demand,
   inflation, matrices, scenarios, or robustness behavior.
5. Execute heavy and integration tests when the change touches month execution,
   flow simulation, Monte Carlo output, or shared ledger behavior.
6. Compare generated CSVs or reports under `mc/` or `target/` before committing.

## Troubleshooting

If compilation fails with classfile or toolchain errors, check that the active
JDK is 21.

If the first test run is slow, verify whether sbt/Coursier is still downloading
dependencies. Subsequent runs should be much faster.

If `sbt test` is fast but CI-like validation is needed, remember that heavy
tests and integration tests are separate commands.

If a simulation fails with an SFC violation, treat it as an accounting or flow
execution bug first. Behavioral instability is allowed; broken monetary
plumbing is not.

If generated results look stale, check the `--run-id`, `--out`, duration, seed
count, and scenario arguments before comparing files.
