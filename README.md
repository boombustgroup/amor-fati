<h1 align="center">AMOR FATI</h1>
<p align="center">
  <em>Stock-Flow Consistent Agent-Based Macroeconomic Engine</em>
</p>



<p align="center">
  <a href="https://github.com/boombustgroup/amor-fati/actions"><img src="https://github.com/boombustgroup/amor-fati/actions/workflows/ci.yml/badge.svg" alt="CI"></a>
  <a href="https://codecov.io/gh/boombustgroup/amor-fati"><img src="https://codecov.io/gh/boombustgroup/amor-fati/graph/badge.svg" alt="Coverage"></a>
  <img src="https://img.shields.io/badge/exact_SFC_identities-15-orange.svg" alt="15 exact SFC identities">
  <a href="LICENSE"><img src="https://img.shields.io/badge/license-Apache_2.0-blue.svg" alt="Apache 2.0"></a>
</p>

> <p align="center">
>   <em>"I want to learn more and more to see as beautiful what is necessary in things;<br>then I shall be one of those who makes things beautiful.<br>Amor fati: let that be my love henceforth!"</em><br>
>   — Friedrich Nietzsche, <em>The Gay Science</em> §276
> </p>

---

A ledger-first SFC-ABM of the Polish economy where **every monetary flow is accounted for**. Firms produce, households consume, banks lend, the central bank sets policy, the government taxes and spends, the external sector trades and moves capital — and the books must balance. Always.

Amor Fati is a **stock-flow consistent** (SFC) **agent-based model** (ABM) that simulates the Polish economy at the level of individual households, heterogeneous firms, and a realistic multi-bank financial system. The engine enforces 15 exact accounting identities each month — if a single zloty goes missing, the simulation fails.

The key design principle is simple:

> **Macro stories can be wrong. The ledger cannot.**

This engine is built on top of the separately verified [amor-fati-ledger](https://github.com/boombustgroup/amor-fati-ledger) flow interpreter, vendored here under `modules/ledger`. The practical consequence is that the strongest invariant in the entire project is not "inflation should look smooth" or "GDP should converge nicely after 10 years". It is this:

> **The books must balance to the end of the universe.**

That is the hard floor under every experiment in the model. Behavioral rules, policy heuristics, and long-horizon dynamics can be revised, recalibrated, or replaced. The accounting layer cannot silently drift.

## Table of Contents

- [Why](#why)
- [What Is Technically Distinctive](#what-is-technically-distinctive)
- [Core Invariants](#core-invariants)
- [State Ontology](#state-ontology)
- [Verified Ledger](#verified-ledger)
- [Model Documentation](#model-documentation)
- [Ledger-Derived Matrix Artifacts](#ledger-derived-matrix-artifacts)
- [Tech Stack](#tech-stack)
- [License](#license)

## Why

Standard macro models (DSGE) assume representative agents and rational expectations. Reality has neither. Amor Fati models the economy from the bottom up: thousands of heterogeneous agents making bounded decisions, interacting through markets, generating emergent macro dynamics.

**Counterfactual analysis through code.** Want to test a policy hypothesis? Fork the repo, modify the mechanism, run the simulation, compare. The model is the experiment.

## What Is Technically Distinctive

Amor Fati is not just an ABM with accounting checks bolted on afterwards. The project combines several layers that are rarely pushed into one executable research engine:

- heterodox macroeconomics and SFC discipline
- heterogeneous-agent ABM microstructure
- a runtime ledger execution layer backed by formal verification work
- a data-oriented mutable execution substrate for performance
- explicit state ontology separating behavioral state, macro state, and financial state

The result is a model where macro dynamics, institutional behavior, and monetary plumbing are all first-class parts of the implementation rather than separate narratives.

## Core Invariants

The project is built around a few non-negotiable invariants:

- executed financial flows must conserve value exactly at runtime
- SFC validation must preserve 15 exact semantic accounting identities
- financial execution must not depend on ad hoc mutable bookkeeping outside the ledger path
- behavioral rules may change, but accounting consistency must remain hard-constrained

This is the core philosophy of the engine: theories may evolve, but broken plumbing is not an acceptable macro result.

## State Ontology

Amor Fati uses a hybrid runtime ontology rather than a single undifferentiated world object:

- behavioral populations
  - households, firms, and banks carry heterogeneous agent state and decision logic
- macro and market state
  - prices, policy, external conditions, and inter-step signals live in the macro runtime layer
- ledger-owned financial state
  - supported financial balances are kept in an accounting-controlled stock layer and projected into runtime execution and validation

This split is intentional. It keeps rich agent behavior where object-level modeling is useful, while moving accounting-critical execution into a stricter ledger substrate.

## Verified Ledger

Most macro models treat accounting consistency as a secondary validation step. Amor Fati does not.

The simulation pipeline is anchored to the verified [amor-fati-ledger](https://github.com/boombustgroup/amor-fati-ledger) layer that enforces the project’s stock-flow constraints at runtime. In other words:

- macro behavior is experimental
- agent rules are revisable
- accounting identities are non-negotiable

This is why Amor Fati is useful even when the long-run path is still being calibrated. If a branch generates a bad macro regime, that may be a modeling problem. If the ledger breaks, the simulation itself is wrong.

That distinction matters. A nonlinear ABM can explore unstable, surprising, even pathological futures. But it should never "lose money" in the plumbing.

## Model Documentation

Amor Fati now includes a research-readiness documentation spine for review,
replication, calibration, validation, and publication work:

| Artifact | Purpose |
| --- | --- |
| [ODD / ODD+D model documentation](docs/odd-model-documentation.md) | Paper-facing model description: purpose, entities, state variables, scales, scheduling, initialization, inputs, submodels, observation surfaces, and decision-making notes. |
| [Behavioral equations and decision rules](docs/behavioral-equations-and-decision-rules.md) | Household, firm, bank, fiscal, monetary, external, insurance, NBFI, quasi-fiscal, and JST rules linked to implementation modules and numeric output columns. |
| [Calibration register](docs/calibration-register.md) | Key parameter values, units, implementation owners, empirical targets, transformations, provenance status, and searchable gaps. |
| [Data bridge to national and financial accounts](docs/data-bridge-national-financial-accounts.md) | Official Polish, EU, and financial-account sources mapped to initialization stocks, calibrated parameters, scenario inputs, validation targets, transformations, and prioritized empirical gaps. |
| [Empirical validation report skeleton](docs/empirical-validation-report.md) | Macro, meso, micro, financial, and external validation targets mapped to Monte Carlo output columns, with missing data/output gaps kept visible. |
| [Sensitivity and robustness workflow](docs/sensitivity-robustness-workflow.md) | Seed envelopes and one-at-a-time parameter-sensitivity artifacts generated from the Monte Carlo runner. |
| [Reproducible scenario registry](docs/scenario-registry.md) | Named policy and shock scenarios with exact parameter deltas from baseline, expected channels, seed/run metadata, and the `scenarioRun` execution path. |

## Ledger-Derived Matrix Artifacts

The project includes committed Markdown snapshots of the paper-facing SFC
matrix artifacts:

| Artifact | Purpose |
| --- | --- |
| [Balance Sheet Matrix (BSM)](docs/sfc-matrix-artifacts/symbolic-bsm.md) | Symbolic stock matrix by instrument and sector, using SFC asset/liability signs and explicit row sums. |
| [Transactions Flow Matrix (TFM)](docs/sfc-matrix-artifacts/symbolic-tfm.md) | Symbolic monthly flow matrix by sector, including income, taxes, transfers, interest, trade, credit, bonds, and deposit changes. |
| [Stock-Flow Reconciliation and Revaluation Evidence](docs/sfc-matrix-artifacts/stock-flow-reconciliation.md) | Executed-run evidence comparing observed stock deltas or level identities with independent transaction, revaluation, default, write-off, and other-change channels. |
| [Symbolic-row to runtime mapping](docs/sfc-matrix-artifacts/matrix-mapping.md) | Traceability table linking each symbolic matrix row to runtime assets, mechanisms, ids, and coverage notes. |

These snapshots are generated from an executed deterministic simulation step.
To regenerate a scratch copy under `target/`:

```bash
sbt "sfcMatrices --seed 1 --months 12 --out target/sfc-matrices"
```

To refresh the committed Markdown snapshots:

```bash
sbt "sfcMatrices --seed 1 --months 12 --out docs/sfc-matrix-artifacts --format md"
```

The workflow, sign conventions, coverage gaps, exact reconciliation rows, and
review checklist are documented in
[docs/sfc-matrix-evidence.md](docs/sfc-matrix-evidence.md).

## Tech Stack

![Scala](https://img.shields.io/badge/Scala_3-DC322F?logo=scala&logoColor=white)
![Stainless](https://img.shields.io/badge/Stainless-Formal%20Verification-4B5563)
![Z3](https://img.shields.io/badge/Z3-SMT%20Solver-1F6FEB)
![sbt](https://img.shields.io/badge/sbt-1.11.6-blue)

## License

[Apache 2.0](LICENSE) — Copyright 2026 [BoomBustGroup](https://www.boombustgroup.com/)
