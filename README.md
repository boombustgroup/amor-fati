<h1 align="center">AMOR FATI</h1>
<p align="center">
  <em>Stock-Flow Consistent Agent-Based Macroeconomic Engine</em>
</p>



<p align="center">
  <a href="https://github.com/boombustgroup/amor-fati/actions"><img src="https://github.com/boombustgroup/amor-fati/actions/workflows/ci.yml/badge.svg" alt="CI"></a>
  <a href="https://codecov.io/gh/boombustgroup/amor-fati"><img src="https://codecov.io/gh/boombustgroup/amor-fati/graph/badge.svg" alt="Coverage"></a>
  <img src="https://img.shields.io/badge/scala-3.8.2-red.svg" alt="Scala 3.8.2">
  <img src="https://img.shields.io/badge/SFC_identities-14-orange.svg" alt="14 SFC identities">
  <a href="LICENSE"><img src="https://img.shields.io/badge/license-Apache_2.0-blue.svg" alt="Apache 2.0"></a>
</p>

> <p align="center">
>   <em>"I want to learn more and more to see as beautiful what is necessary in things;<br>then I shall be one of those who makes things beautiful.<br>Amor fati: let that be my love henceforth!"</em><br>
>   — Friedrich Nietzsche, <em>The Gay Science</em> §276
> </p>

---

A closed-economy simulation where **every monetary flow is accounted for**. Firms produce, households consume, banks lend, the central bank sets policy, the government taxes and spends — and the books must balance. Always.

Amor Fati is a **stock-flow consistent** (SFC) **agent-based model** (ABM) that simulates the Polish economy at the level of individual households, heterogeneous firms, and a realistic multi-bank financial system. The engine enforces 14 accounting identities each month — if a single zloty goes missing, the simulation fails.

The key design principle is simple:

> **Macro stories can be wrong. The ledger cannot.**

This engine is built on top of the separately verified [amor-fati-ledger](https://github.com/boombustgroup/amor-fati-ledger-poc) flow interpreter. The practical consequence is that the strongest invariant in the entire project is not "inflation should look smooth" or "GDP should converge nicely after 10 years". It is this:

> **The books must balance to the end of the universe.**

That is the hard floor under every experiment in the model. Behavioral rules, policy heuristics, and long-horizon dynamics can be revised, recalibrated, or replaced. The accounting layer cannot silently drift.

## Why

Standard macro models (DSGE) assume representative agents and rational expectations. Reality has neither. Amor Fati models the economy from the bottom up: thousands of heterogeneous agents making bounded decisions, interacting through markets, generating emergent macro dynamics.

**Counterfactual analysis through code.** Want to test a policy hypothesis? Fork the repo, modify the mechanism, run the simulation, compare. The model is the experiment.

## Verified Ledger

Most macro models treat accounting consistency as a secondary validation step. Amor Fati does not.

The simulation pipeline is anchored to a verified ledger layer that enforces the project’s stock-flow constraints at runtime. In other words:

- macro behavior is experimental
- agent rules are revisable
- accounting identities are non-negotiable

This is why Amor Fati is useful even when the long-run path is still being calibrated. If a branch generates a bad macro regime, that may be a modeling problem. If the ledger breaks, the simulation itself is wrong.

That distinction matters. A nonlinear ABM can explore unstable, surprising, even pathological futures. But it should never "lose money" in the plumbing.

## Tech Stack

![Scala](https://img.shields.io/badge/Scala_3-DC322F?logo=scala&logoColor=white)
![Stainless](https://img.shields.io/badge/Stainless-Formal%20Verification-4B5563)
![Z3](https://img.shields.io/badge/Z3-SMT%20Solver-1F6FEB)
![sbt](https://img.shields.io/badge/sbt-1.11.6-blue)

## License

[Apache 2.0](LICENSE) — Copyright 2026 [BoomBustGroup](https://www.boombustgroup.com/)
