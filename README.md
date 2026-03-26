<p align="center">
  <img src="assets/logo.png" width="400" alt="Amor Fati — Rota Fortunae">
</p>

<p align="center">
  <h1>AMOR FATI</h1>
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

## Why

Standard macro models (DSGE) assume representative agents and rational expectations. Reality has neither. Amor Fati models the economy from the bottom up: thousands of heterogeneous agents making bounded decisions, interacting through markets, generating emergent macro dynamics.

**Counterfactual analysis through code.** Want to test a policy hypothesis? Fork the repo, modify the mechanism, run the simulation, compare. The model is the experiment.

## License

[Apache 2.0](LICENSE) — Copyright 2026 [BoomBustGroup](https://www.boombustgroup.com/)
