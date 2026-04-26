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
  - supported financial balances live in `LedgerFinancialState` and are projected into the ledger runtime for execution and validation

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

The paper-facing ODD / ODD+D model description is documented in [docs/odd-model-documentation.md](docs/odd-model-documentation.md). It covers purpose, entities, state variables, scales, scheduling, design concepts, initialization, inputs, submodels, observation surfaces, decision-making notes, and open research-readiness gaps.

The detailed behavioral equations and institutional decision rules are documented in [docs/behavioral-equations-and-decision-rules.md](docs/behavioral-equations-and-decision-rules.md). It links household, firm, bank, fiscal, monetary, external, insurance, NBFI, quasi-fiscal, and JST rules to implementation modules and numeric output columns.

The calibration register is documented in [docs/calibration-register.md](docs/calibration-register.md). It records key parameter values, units, implementation owners, empirical targets, transformations, provenance status, and explicit searchable gaps.

The data bridge to national and financial accounts is documented in [docs/data-bridge-national-financial-accounts.md](docs/data-bridge-national-financial-accounts.md). It maps official Polish, EU, and financial-account sources to initialization stocks, calibrated parameters, scenario inputs, validation targets, transformation rules, and prioritized empirical gaps.

The empirical validation report skeleton is documented in [docs/empirical-validation-report.md](docs/empirical-validation-report.md). It maps macro, meso, micro, financial, and external validation targets to Monte Carlo output columns and keeps missing data/output gaps visible.

The sensitivity and robustness workflow is documented in [docs/sensitivity-robustness-workflow.md](docs/sensitivity-robustness-workflow.md). It generates seed envelopes and one-at-a-time parameter-sensitivity artifacts from the Monte Carlo runner.

The reproducible scenario registry is documented in [docs/scenario-registry.md](docs/scenario-registry.md). It defines named policy and shock scenarios, exact parameter deltas from baseline, expected channels, seed/run metadata, and the `scenarioRun` execution path.

## Ledger-Derived Matrix Artifacts

The project can regenerate paper-facing symbolic SFC matrices from an executed deterministic simulation step:

```bash
sbt "sfcMatrices --seed 1 --months 12 --out target/sfc-matrices"
```

This writes symbolic Balance Sheet Matrix (BSM), Transactions Flow Matrix (TFM), and stock-flow reconciliation/revaluation artifacts in LaTeX and Markdown, plus a symbolic-row to runtime-ledger mapping under `target/`. The workflow, sign conventions, coverage gaps, exact reconciliation rows, and review checklist are documented in [docs/sfc-matrix-evidence.md](docs/sfc-matrix-evidence.md).

## Tech Stack

![Scala](https://img.shields.io/badge/Scala_3-DC322F?logo=scala&logoColor=white)
![Stainless](https://img.shields.io/badge/Stainless-Formal%20Verification-4B5563)
![Z3](https://img.shields.io/badge/Z3-SMT%20Solver-1F6FEB)
![sbt](https://img.shields.io/badge/sbt-1.11.6-blue)

## License

[Apache 2.0](LICENSE) — Copyright 2026 [BoomBustGroup](https://www.boombustgroup.com/)
