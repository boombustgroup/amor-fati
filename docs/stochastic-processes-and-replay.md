# Stochastic Processes And Replay

This document consolidates Amor Fati's implemented randomness contract into a
publication-facing section. It explains initialization seeds, monthly random
streams, stochastic decision surfaces, Monte Carlo seed policy, deterministic
replay, validation coverage, and current limitations.

It describes current executable behavior. It does not introduce new stochastic
assumptions, new probability distributions, or new seed scheduling rules.

## Source Anchors

| Source | Role |
| --- | --- |
| [Model specification](model-specification.md#reviewer-reading-path) | Canonical reviewer path and model overview. |
| [Model notation and state vector](model-notation-and-state-vector.md#stochastic-variables) | Stochastic notation, $RND_{\tau}$, and stream-key notation. |
| [Monthly transition function](monthly-transition-function.md#randomness-contract) | Formal one-month replay contract. |
| [`random/RandomStream.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/random/RandomStream.scala) | Project-wide RNG facade. |
| [`random/SeedDerivation.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/random/SeedDerivation.scala) | Stable seed splitting from one explicit root seed. |
| [`init/InitRandomness.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/init/InitRandomness.scala) | Initialization randomness contract. |
| [`engine/MonthRandomness.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/MonthRandomness.scala) | Month-step randomness contract. |
| [`montecarlo/runner/McRunner.scala`](../modules/montecarlo/src/main/scala/com/boombustgroup/amorfati/montecarlo/runner/McRunner.scala) | Production Monte Carlo initialization, month seed schedule, and per-seed streaming path. |
| [`montecarlo/diagnostics/McDiagnosticRunner.scala`](../modules/montecarlo/src/main/scala/com/boombustgroup/amorfati/montecarlo/diagnostics/McDiagnosticRunner.scala) | Shared scenario/diagnostic runner that reuses the production seed-month path. |
| [`engine/MonthTrace.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/MonthTrace.scala) | Month audit trace that records the explicit `MonthRandomness.Contract`. |
| [`MonthRandomnessSpec`](../modules/model/src/test/scala/com/boombustgroup/amorfati/engine/MonthRandomnessSpec.scala) and [`InitRandomnessSpec`](../modules/model/src/test/scala/com/boombustgroup/amorfati/init/InitRandomnessSpec.scala) | Stable stream derivation tests. |

## Core Contract

The model has no ambient production randomness. Domain code depends on the
opaque `RandomStream` facade, and the normal engine path constructs those
streams from explicit contracts.

The one-month transition is deterministic conditional on:

$$
\begin{aligned}
(X_{t}, RND_{\tau}, \theta)
\end{aligned}
$$

where:

| Symbol | Meaning |
| --- | --- |
| $X_{t}$ | completed month boundary state |
| $RND_{\tau}$ | explicit `MonthRandomness.Contract` for execution month $\tau$ |
| $\theta$ | complete `SimParams` vector, including scenario-adjusted parameters |

Initialization has a separate deterministic contract:

$$
\begin{aligned}
X_{0} &= \mathrm{init}(\theta, INIT_{s}) \\
INIT_{s} &= \mathrm{InitRandomness.Contract.fromSeed}(s)
\end{aligned}
$$

The replay rule is:

$$
\begin{aligned}
\theta, RND_{\tau}, X_{t}\ \mathrm{fixed} &\Longrightarrow X_{\tau}\ \mathrm{fixed}
\end{aligned}
$$

Different seeds are not alternative deterministic branches of one run. They are
Monte Carlo draws used to estimate distributional behavior of the same
parameterized model.

## Seed Derivation

`SeedDerivation.derive(rootSeed, keySalt)` maps one root seed and one stream key
to a stable 64-bit stream seed. `InitRandomness` and `MonthRandomness` use enum
ordinals as key salts and expose the derived seeds as named `StreamSeed`
values.

Each `StreamSeed` creates a fresh `RandomStream` with:

```text
RandomStream.seeded(streamSeed)
```

This gives two important guarantees:

| Guarantee | Meaning |
| --- | --- |
| Stable split | Rebuilding the same contract from the same root seed gives the same named stream seeds. |
| Stream isolation | Household, firm, banking, external, and lifecycle streams do not share one mutable RNG state. |

`RandomStream.fresh()` exists as a facade method, but it is not part of the
normal model execution path. Production model replay should be described in
terms of explicit contracts, not fresh process-local randomness.

## Initialization Randomness

Initialization randomness belongs to `InitRandomness.Contract`.

For production Monte Carlo seed `s`:

$$
\begin{aligned}
INIT_{s} &= \mathrm{InitRandomness.Contract.fromSeed}(s)
\end{aligned}
$$

The initialization streams are:

| Stream key | Runtime field | Main stochastic role |
| --- | --- | --- |
| `FirmNetwork` | `firms.network` | Initial firm network edges. |
| `FirmSectorAssignments` | `firms.sectorAssignments` | Initial sector slot assignment and shuffle. |
| `FirmSkeleton` | `firms.skeleton` | Firm cash, initial technology, risk profile, innovation-cost factor, and related firm attributes. |
| `FirmRegions` | `firms.regions` | Initial firm region assignment. |
| `FirmCapitalAndBank` | `firms.capitalAndBank` | Initial firm capital and bank assignment. |
| `FirmForeignOwnership` | `firms.foreignOwnership` | Initial foreign-owned firm flags. |
| `FirmStateOwnership` | `firms.stateOwnership` | Initial state-owned firm flags. |
| `HouseholdNetwork` | `households.network` | Initial social-neighbor network. |
| `HouseholdAttributes` | `households.attributes` | Initial savings, debt, rent, MPC, education, skill, children, equity, task routineness, contracts, and household regions. |
| `HouseholdInitialUnemployment` | `households.initialUnemployment` | Initial employed/unemployed assignment. |
| `InitialImmigrantStock` | `immigration.initialStock` | Initial immigrant-household attributes and opening balances. |

Initialization uses fixed-point sampling helpers such as categorical CDF
sampling, uniform fixed-point draws, Poisson draws, Gaussian perturbations, beta
style bounded shares, and lognormal PLN draws. These helpers are implementation
tools, not additional model-level seed sources.

## Monthly Randomness

Monthly randomness belongs to `MonthRandomness.Contract`.

For the production Monte Carlo path, seed `s` and opening state with completed
month `t` use:

$$
\begin{aligned}
RND_{t+1,s} &= \mathrm{MonthRandomness.Contract.fromSeed}(s \cdot 10000 + t)
\end{aligned}
$$

The first executed month therefore uses the root seed $s \cdot 10000$ because
the opening initialized state has $\mathrm{completedMonth} = 0$.

The monthly contract splits randomness into same-month stage streams and
closed-month lifecycle streams:

| Stream key | Runtime field | Phase | Main stochastic role |
| --- | --- | --- | --- |
| `HouseholdIncomeEconomics` | `stages.householdIncomeEconomics` | same month | Household income-stage micro flows, consumer-credit bank-supply approvals delegated from the household path, labor transitions executed inside household step logic, and household-row stochastic choices. |
| `FirmEconomics` | `stages.firmEconomics` | same month | Firm credit approval, operating adjustment, technology adoption lotteries, Calvo repricing, labor matching, and firm-stage stochastic choices. |
| `HouseholdFinancialEconomics` | `stages.householdFinancialEconomics` | same month | Household financial-stage randomness; currently retained as an explicit stage surface even where the present computation is mostly deterministic. |
| `OpenEconEconomics` | `stages.openEconEconomics` | same month | Open-economy and commodity/GVC stochastic surfaces. |
| `BankingEconomics` | `stages.bankingEconomics` | same month | Banking-stage deposit mobility and other bank-stage stochastic choices. Firm and household credit approvals call bank-side approval rules, but their RNG owners are the firm and household stage streams that invoke them. |
| `FdiMa` | `closing.fdiMa` | closed month | FDI M&A ownership transitions for eligible domestic firms. |
| `FirmEntry` | `closing.firmEntry` | closed month | New firm entry, sector choice, AI-native startups, startup attributes, foreign ownership, and entrant networks. |
| `StartupStaffing` | `closing.startupStaffing` | closed month | Startup hiring through labor-market job search. |
| `RegionalMigration` | `closing.regionalMigration` | closed month | Wage-gap-driven household regional relocation. |

The month trace records the complete `MonthRandomness.Contract`, so replay can
inspect the root seed and all derived stream seeds attached to that step.

## Stochastic Decision Surfaces

The major runtime stochastic surfaces are:

| Surface | Examples | Stream family |
| --- | --- | --- |
| Household initialization | Savings/debt distributions, rent, MPC, children, education, skill, equity ownership, task routineness, contracts, region, social network, initial unemployment | `InitRandomness.households` |
| Firm initialization | Sector slots, network, cash, initial hybrid technology, risk/innovation parameters, capital/bank assignment, foreign/state ownership | `InitRandomness.firms` |
| Immigration initialization | Initial immigrant sector, education, skill, savings, MPC, rent, children, contract, region | `InitRandomness.immigration` |
| Household monthly behavior | Voluntary search, retraining attempt/success, sector mobility, consumer-credit access eligibility, consumer-credit bank approval, shortfall underwriting | `HouseholdIncomeEconomics`, `HouseholdFinancialEconomics`; bank approval rules are invoked from this household path |
| Firm monthly behavior | Firm-credit approval, stochastic labor adjustment magnitude, digital investment/technology adoption, AI/hybrid upgrade outcomes, Calvo repricing, labor matching | `FirmEconomics`; bank approval rules are invoked from this firm path |
| Banking monthly behavior | Health-driven deposit mobility after bank stress/failure events and any stochastic choices executed by the banking stage itself | `BankingEconomics` |
| Open economy and commodities | GVC and commodity stochastic surfaces when enabled by the open-economy stage | `OpenEconEconomics` |
| Closed-month lifecycle | FDI M&A, firm entry, startup staffing, regional migration | `MonthRandomness.closing` |

Several institutional rules are deterministic conditional on state and
parameters: fiscal-rule constraints, the NBP Taylor-type rate rule, QE
activation logic, FX-intervention band logic, insurance reserve updates, NBFI
stock renewal, quasi-fiscal issuance, SFC projection, and ledger execution.
They can still produce different paths across seeds because their inputs are
affected by stochastic household, firm, bank, and external channels.

## Randomness Ownership By Layer

| Layer | Classification |
| --- | --- |
| Household | Initialization heterogeneity, labor and household financial choices, consumer-credit bank-supply approvals, and shortfall underwriting draws are household-facing stochastic mechanisms. |
| Firm | Initialization heterogeneity, credit approval, operating adjustment, pricing, technology adoption, labor matching, entry, and startup staffing are firm-facing stochastic mechanisms. |
| Banking | Bank-side approval rules can be stochastic, but their RNG owner is the invoking firm or household stage. The `BankingEconomics` stream is currently concentrated in health-driven deposit mobility and stochastic choices executed by the banking stage itself; interbank settlement, resolution accounting, and regulatory metrics are deterministic conditional on state. |
| External | Open-economy, commodity, and GVC stochastic surfaces belong to the explicit open-economy month stream when enabled by configuration. |
| Lifecycle | FDI M&A, firm entry, startup staffing, and regional migration execute after same-month economics through closed-month streams; monthly immigrant inflow attributes are drawn inside the firm-market same-month stage. |
| Diagnostics | Production diagnostics that use `McDiagnosticRunner` inherit the Monte Carlo seed contract; standalone probes must publish their explicit seed schedule in command metadata or source references. |
| Monte Carlo | Seed ranges are distributional sampling over deterministic seed paths, not changes to behavioral equations or `SimParams`. |

## Monte Carlo Seed Policy

`McRunner` is the production Monte Carlo owner.

For seed `s`:

$$
\begin{aligned}
\mathrm{WorldInit.initialize}(\mathrm{InitRandomness.Contract.fromSeed}(s)) \\
\mathrm{MonthRandomness.Contract.fromSeed}(s \cdot 10000 + completedMonth)
\end{aligned}
$$

`McRunner.runZIO` runs seeds in parallel, but each seed is an independent
streaming simulation. Parallel execution order does not change the seed
contract for any seed/month pair.

`McDiagnosticRunner` is the shared diagnostic runner for scenario and seed
sweeps. Diagnostics that use `McDiagnosticRunner` and `McRunner.seedMonths`
inherit the production initialization and monthly seed policy.

Some small standalone probes and matrix exporters construct their own explicit
`MonthRandomness.Contract` schedule. They remain replayable because the schedule
is explicit, but their seed-root formula should be read from that tool's source
or command metadata rather than inferred from `McRunner`.

## Distributional Interpretation

One seed is one deterministic realization:

$$
\begin{aligned}
s\ \mathrm{fixed} \to X_{0}\ \mathrm{fixed} \to X_{1}\ \mathrm{fixed} \to \ldots \to X_{T}\ \mathrm{fixed}
\end{aligned}
$$

A Monte Carlo run with `N` seeds is a distributional sample:

$$
\begin{aligned}
\{\,\mathrm{path}_{s} \mid s \in \mathrm{seedRange}\,\}
\end{aligned}
$$

Across-seed means, minima, maxima, envelopes, quantiles, and scenario
comparisons are therefore evidence about stochastic uncertainty under fixed
parameters and fixed scenario deltas. They are not new behavioral equations and
they do not alter `SimParams`.

Scenario runs keep the same seed semantics but change $\theta$ through
registered parameter deltas. Sensitivity and robustness reports separate
stochastic uncertainty across seeds from parameter perturbation across
scenarios.

## Replay Checklist

To reproduce a production Monte Carlo seed path, preserve:

| Required input | Source |
| --- | --- |
| Code revision | Commit or branch used for the run. |
| Parameter vector | `SimParams`, scenario deltas, and config overrides. |
| Seed range | `seedStart`, `seeds`, or explicit seed list. |
| Month horizon | `runDurationMonths` or diagnostic `months`. |
| Output options | Snapshot schedules, decision-trace selection, scenario selection, and diagnostic flags. |

For one month replay, preserve:

```text
stateIn
MonthRandomness.Contract
SimParams
trace options used by the caller
```

The engine-level replay boundary is narrower than a full CLI replay: it starts
from an already-materialized `FlowSimulation.SimState`. CLI-level replay also
requires initialization, scenario, output, and diagnostic configuration.

## Validation Coverage

| Layer | Coverage |
| --- | --- |
| Unit tests | `InitRandomnessSpec` and `MonthRandomnessSpec` validate stable named stream derivation, distinct stream seeds, and changed-root behavior. |
| Engine tests | Flow simulation, month lineage, SFC, and integration specs validate deterministic step behavior under explicit randomness contracts. |
| Monte Carlo tests | Monte Carlo TSV integration validates deterministic per-seed and summary outputs on the production runner path. |
| Diagnostics | Scenario, robustness, lead-lag, loan-origination, nightly, and profiling workflows record seed/month settings and use fixed seed ranges. |
| Generated artifacts | Generated SFC and documentation outputs are guarded so committed evidence remains reproducible from source. |

## Current Limitations

- The project documents executable stochastic behavior, but it does not yet
  claim econometric identification for every stochastic coefficient.
- Some standalone probes use explicit seed schedules that differ from the
  production Monte Carlo formula. They are replayable, but they should not be
  treated as the canonical production seed policy.
- `RandomStream` wraps `scala.util.Random`; the scientific contract is the
  explicit seed and named-stream boundary, not a claim about a cryptographic or
  platform-independent RNG.
- Distributional output across seeds is model uncertainty under the configured
  stochastic mechanisms. It is not empirical posterior uncertainty unless a
  separate calibration/governance document says so.
