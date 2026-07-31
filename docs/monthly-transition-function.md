# Monthly Transition Function

This document formalizes one Amor Fati model month as a transition from the
completed month boundary $X_{t}$ to the next completed month boundary $X_{\tau}$,
where $\tau = t + 1$.

It describes the implemented engine contract. It does not introduce new model
behavior and does not replace the detailed economics, flow, SFC, calibration,
or validation documents.

## Source Anchors

| Source | Role |
| --- | --- |
| [Model specification](model-specification.md#reviewer-reading-path) | Canonical reviewer path and publication-facing model overview. |
| [Model notation and state vector](model-notation-and-state-vector.md) | Canonical symbols, state vector, time indices, stocks, flows, rates, shares, and stochastic notation. |
| [Stochastic processes and replay](stochastic-processes-and-replay.md) | Detailed seed policy, initialization and month stream maps, stochastic decision surfaces, Monte Carlo replay, and validation coverage. |
| [Behavioral equations and decision rules](behavioral-equations-and-decision-rules.md) | Detailed rule families used by the same-month economics stages. |
| [SFC matrix evidence](sfc-matrix-evidence.md) | Accounting matrix evidence, sign conventions, generated artifacts, and reconciliation surface. |
| [Engine invariants and semantics](engine-invariants-and-semantics.md) | Hard invariants, failure semantics, known limitations, and validation routing. |
| [`FlowSimulation.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/flows/FlowSimulation.scala) | Public one-month `step` boundary. |
| [`MonthCalculusRunner.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/flows/MonthCalculusRunner.scala) | Ordered same-month economics workflow. |
| [`MonthSemantics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/MonthSemantics.scala) | Zero-cost phase tags for `Pre`, `SameMonth`, `Post`, and `NextPre`. |
| [`MonthRandomness.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/MonthRandomness.scala) | Explicit per-month randomness contract. |
| [`MonthFlowEmitter.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/flows/MonthFlowEmitter.scala) | Translation from same-month quantities into runtime ledger batches. |
| [`RuntimeFlowExecutor.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/flows/RuntimeFlowExecutor.scala) | Runtime ledger batch execution. |
| [`SfcSemanticProjection.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/flows/SfcSemanticProjection.scala) | Projection from executed batches and same-month semantic evidence into SFC validation flows. |
| [`closedmonth/MonthClosing.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/closedmonth/MonthClosing.scala) | Realized closed-month semantic state. |
| [`NextStateAdvancer.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/flows/NextStateAdvancer.scala) | Closed-month to next-pre state materialization. |

## Boundary State

The month-boundary state is the canonical state vector from
[model-notation-and-state-vector.md](model-notation-and-state-vector.md):

$$
\begin{aligned}
X_{t} &= (m_{t}, W_{t}, F_{t}, H_{t}, B_{t}, A^{H}_{t}, L_{t})
\end{aligned}
$$

where:

| Symbol | Meaning |
| --- | --- |
| $m_{t}$ | completed month index |
| $W_{t}$ | macro, market, mechanism, signal, and diagnostic world state |
| $F_{t}$ | firm behavioral state vector |
| $H_{t}$ | household behavioral state vector |
| $B_{t}$ | bank operational state vector |
| $A^{H}_{t}$ | household aggregates |
| $L_{t}$ | ledger-owned financial state |

The public one-month engine contract is:

$$
\begin{aligned}
\Phi_{\tau} : (X_{t}, RND_{\tau}, \theta) \to (X_{\tau}, E_{\tau}) \\
\tau &= t + 1
\end{aligned}
$$

where:

| Symbol | Meaning |
| --- | --- |
| $\theta$ | model parameter vector, including scenario-adjusted parameters |
| $RND_{\tau}$ | explicit month randomness contract |
| $X_{\tau}$ | next completed month boundary state |
| $E_{\tau}$ | evidence emitted by the transition: flow plan, batches, ledger execution, semantic SFC flows, validation result, traces, and diagnostics |

The transition is deterministic conditional on $(X_{t}, RND_{\tau}, \theta)$.

## Randomness Contract

$RND_{\tau}$ is an explicit `MonthRandomness.Contract`, not ambient global
randomness. It is derived from one `rootSeed` and split into independent named
streams:

| Stream group | Streams |
| --- | --- |
| Same-month economics | household income economics, firm economics, household financial economics, open-economy economics, banking economics |
| Closed-month mechanisms | FDI M&A, firm entry, startup staffing, regional migration |

Fixing $X_{t}$, $\theta$, and $RND_{\tau}.\mathrm{rootSeed}$ fixes all named streams and
therefore fixes the one-month replay.

The full production seed policy and stochastic surface map are documented in
[stochastic-processes-and-replay.md](stochastic-processes-and-replay.md).

## Phase Timeline

The engine uses four coarse temporal phases:

| Phase | Meaning | Runtime anchor |
| --- | --- | --- |
| `Pre` | Start-of-month boundary, including persisted decision signals from $X_{t}$ | `FlowSimulation.StepInput`, `MonthSemantics.SeedIn` |
| `SameMonth` | Ordered economics, operational signals, flow plan, semantic projection payload, and step evidence for month $\tau$ | `MonthCalculusRunner`, `MonthSemantics.FlowPlan`, `MonthSemantics.SignalView` |
| `Post` | Realized semantic closed-month state before it becomes the next public boundary | `MonthClosing.close`, `MonthSemantics.ClosedMonth` |
| `NextPre` | Extracted decision-signal seed and exposed $X_{\tau}$ boundary | `SignalExtraction`, `NextStateAdvancer` |

The publication-level dependency order is:

$$
\begin{aligned}
\text{pre boundary} \\
{} \to \text{same-month economics} \\
{} \to \text{same-month boundary views} \\
{} \to \text{semantic closed month and seed extraction} \\
{} \to \text{flow emission} \\
{} \to \text{runtime ledger execution} \\
{} \to \text{next-pre materialization} \\
{} \to \text{SFC validation gate and trace evidence}
\end{aligned}
$$

Implementation note: `MonthClosing.close` is pure and uses only same-month
closing inputs plus closing randomness. The public boundary $X_{\tau}$ is not
exposed until runtime ledger deltas are materialized by `NextStateAdvancer` and
SFC validation has been evaluated for the transition.

## Formal Stages

### 1. Pre Boundary

The engine constructs the execution month and opening snapshot:

$$
\begin{aligned}
\tau &= \mathrm{next}(m_{t}) \\
S_{t} &= \mathrm{seedIn}(W_{t}) \\
\Omega_{t} &= \mathrm{topology}(X_{t}) \\
M_{t} &= \mathrm{snapshot}(X_{t})
\end{aligned}
$$

where:

| Symbol | Meaning |
| --- | --- |
| $S_{t}$ | persisted start-of-month decision surface |
| $\Omega_{t}$ | runtime ledger topology derived from the opening state |
| $M_{t}$ | audit snapshot of the opening month boundary |

The lineage invariant is:

$$
\begin{aligned}
\tau &= \mathrm{next}(m_{t})
\end{aligned}
$$

The next state must complete exactly this execution month; skipping or
rewriting the month index is an engine invariant violation.

### 2. Same-Month Economics

Same-month economics is the deterministic ordered workflow:

$$
\begin{aligned}
C_{\tau} &= \Gamma_{\tau}(X_{t}, S_{t}, RND_{\tau}^{stage}, \theta)
\end{aligned}
$$

$\Gamma_{\tau}$ runs the same-month stages in this order:

$$
\begin{aligned}
\text{fiscal constraint} \\
{} \to \text{labor pre-clearing} \\
{} \to \text{opening payroll} \\
{} \to \text{household income} \\
{} \to \text{demand} \\
{} \to \text{firm processing} \\
{} \to \text{post-firm labor context} \\
{} \to \text{social funds} \\
{} \to \text{labor reconciliation} \\
{} \to \text{household financial flows} \\
{} \to \text{price/equity} \\
{} \to \text{open economy} \\
{} \to \text{banking} \\
{} \to \text{month execution assembly}
\end{aligned}
$$

The same-month output is then narrowed into boundary views:

| View | Meaning | Runtime type |
| --- | --- | --- |
| $K_{\tau}$ | flow translation plan | `MonthlyCalculus` / `MonthSemantics.FlowPlan` |
| $V_{\tau}$ | same-month labor and demand signal view | `MonthSemantics.SignalView` |
| $Z_{\tau}$ | closed-month input | `MonthSemantics.ClosingInput` |
| $U_{\tau}$ | narrow semantic projection payload for SFC | `MonthSemantics.SemanticProjection` |
| $D_{\tau}$ | diagnostics and export evidence | `MonthSemantics.StepEvidence` |

Same-month economics does not execute runtime ledger batches. It computes
quantities, rates, decisions, closing projections, and the narrow evidence
needed by later boundaries.

### 3. Closed-Month Semantic State

The semantic closed month is computed from the same-month closing input and
closing randomness:

$$
\begin{aligned}
C_{\tau}^{close} &= \mathrm{close}(Z_{\tau}, RND_{\tau}^{close}, \theta)
\end{aligned}
$$

This stage applies closed-month mechanisms and diagnostics, including:

- informal-economy adjustment;
- flow-of-funds diagnostic residual;
- population lifecycle transitions;
- firm entry and startup staffing;
- FDI M&A ownership transition;
- regional migration;
- final world, agent, bank, household-aggregate, and semantic ledger closing
  projection.

The closed-month world still carries the old `SeedIn`. The next decision signal
surface is extracted separately.

### 4. Signal Extraction

The next pre-month decision surface is derived from realized month outcomes:

$$
\begin{aligned}
S_{\tau} &= \mathrm{extractSeed}(V_{\tau}, C_{\tau}^{close})
\end{aligned}
$$

$S_{\tau}$ becomes the persisted decision-signal surface for the next public
boundary. This keeps same-month operational signals distinct from lagged
signals used by the following month.

### 5. Flow Emission

The flow plan is translated into runtime ledger batches:

$$
\begin{aligned}
B_{\tau} &= \mathrm{emit}(K_{\tau}, \Omega_{t}, \theta)
\end{aligned}
$$

$B_{\tau}$ is a vector of typed runtime batches. Each batch has a mechanism,
source sector, destination sector, asset class, amount, and topology-specific
legs.

Flow emission is a translation boundary. It does not decide new economics and
does not mutate the month state.

### 6. Runtime Ledger Execution

Runtime batches are executed through the ledger interpreter:

$$
\begin{aligned}
R_{\tau} &= \mathrm{execute}(B_{\tau}, \Omega_{t}) \\
R_{\tau} &= (\Delta L_{\tau}^{run}, netDelta_{\tau})
\end{aligned}
$$

where:

| Symbol | Meaning |
| --- | --- |
| $\Delta L_{\tau}^{run}$ | executed runtime delta ledger |
| $netDelta_{\tau}$ | conservation check over the runtime delta space |

The runtime ledger must conserve value. Execution failure is a hard engine
failure. The executed delta ledger is not by itself the final model state; it
is materialized into the supported persisted ledger slice at the next boundary.

### 7. Next-Pre Materialization

The final next boundary is:

$$
\begin{aligned}
X_{\tau} &= \mathrm{advance}(X_{t}, \tau, C_{\tau}^{close}, S_{t}, S_{\tau}, R_{\tau})
\end{aligned}
$$

`NextStateAdvancer` performs two jobs:

1. It applies $S_{\tau}$ to the next boundary world.
2. It materializes supported runtime ledger deltas from $R_{\tau}$ into
   `LedgerFinancialState`.

The exposed state has:

$$
\begin{aligned}
m_{\tau} &= \tau \\
W_{\tau}^{\mathrm{seedIn}} &= S_{\tau}
\end{aligned}
$$

and must satisfy the month-lineage and seed-boundary invariants checked by
`NextStateAdvancer`.

### 8. SFC Semantic Validation

The semantic SFC validation surface is built from:

$$
\begin{aligned}
Q_{\tau} &= \mathrm{project}(U_{\tau}, B_{\tau}, X_{\tau}^{\mathrm{world}}.\mathrm{plumbing.fofResidual}, \theta)
\end{aligned}
$$

The validation checks:

$$
\begin{aligned}
\mathrm{validate}(X_{t}, X_{\tau}, Q_{\tau}, B_{\tau}, R_{\tau}) &= pass
\end{aligned}
$$

SFC validation compares the previous runtime state, final next runtime state,
semantic monthly flows, emitted batches, and runtime execution deltas. A failed
exact SFC identity is a hard model error.

## Evidence Surface

The transition evidence is:

$$
\begin{aligned}
E_{\tau} &=  \\
&\quad (K_{\tau}, V_{\tau}, Z_{\tau}, U_{\tau}, D_{\tau}, \\
&\quad B_{\tau}, R_{\tau}, Q_{\tau}, SFC_{\tau}, Trace_{\tau})
\end{aligned}
$$

where:

| Evidence | Meaning |
| --- | --- |
| $K_{\tau}$ | flow plan used for batch emission |
| $V_{\tau}$ | same-month signal view |
| $Z_{\tau}$ | closing input |
| $U_{\tau}$ | semantic projection input |
| $D_{\tau}$ | diagnostic/export evidence |
| $B_{\tau}$ | emitted runtime batches |
| $R_{\tau}$ | runtime ledger execution result |
| $Q_{\tau}$ | projected SFC semantic flows |
| $SFC_{\tau}$ | exact SFC validation result |
| $Trace_{\tau}$ | month trace with boundary snapshots, seed transition, timing, executed flows, and validations |

This evidence is what diagnostics, Monte Carlo exports, generated SFC
artifacts, and validation workflows consume.

## Invariants

Every valid month transition must satisfy:

1. Month lineage: $\tau = next(m_{t})$ and $m_{\tau} = \tau$.
2. Deterministic replay: fixed $(X_{t}, RND_{\tau}, \theta)$ fixes $X_{\tau}$ and
   $E_{\tau}$.
3. Phase separation: same-month economics computes quantities and projections;
   runtime ledger execution books emitted batches; next-pre materialization
   exposes the final boundary.
4. Seed separation: `SeedIn` is consumed during month $\tau$; `SeedOut` is
   applied only at $X_{\tau}$.
5. Runtime ledger conservation: emitted batches execute without losing or
   creating PLN in the runtime delta ledger.
6. Exact SFC validation: semantic stock-flow identities close for
   $(X_{t}, X_{\tau}, Q_{\tau}, B_{\tau}, R_{\tau})$.
7. Traceability: $Trace_{\tau}$ records boundary, seed, timing, executed-flow, and
   validation evidence for review.

## Reading Guidance

For publication writing, use this document for the transition structure and
[model-notation-and-state-vector.md](model-notation-and-state-vector.md) for
symbols. Use
[behavioral-equations-and-decision-rules.md](behavioral-equations-and-decision-rules.md)
for family-level equations and
[sfc-matrix-evidence.md](sfc-matrix-evidence.md) for accounting evidence.
