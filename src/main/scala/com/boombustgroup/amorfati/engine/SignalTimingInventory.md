# Signal Timing Inventory

Date: 2026-04-09
Status: Accepted inventory for issue #308
Related: #309, #310, #311, #312, #313

## Purpose

This note is the authoritative inventory of the remaining timing-sensitive
signal consumers and producers across the monthly engine.

It answers four questions:

- which blocks currently consume persisted `DecisionSignals` / `seedIn`
- which same-month operational surfaces are produced inside month `t`
- which remaining direct `pipeline.*` uses are still architecturally
  transitional
- which boundaries should own persisted signals, same-month operational
  signals, extraction, and tracing

The document is descriptive, not prescriptive runtime code. It exists so that
follow-up issues can refactor against one shared timing map instead of
re-discovering local semantics.

## Current Month Timeline

The current engine shape is still stage-driven, but the timing semantics are
already visible:

1. `FlowSimulation.computeAll(...)` computes `s2Pre`, `s3`, `s4`, `s5`,
   post-firm `s2`, then `s6`-`s9`.
2. `WorldAssemblyEconomics.runStep(...)` assembles the month-`t` world, runs
   `FirmEntry`, runs same-month startup staffing, then hands realized outcomes
   to `SignalExtraction.compute(...)`.
3. `WorldAssemblyEconomics.buildPipelineState(...)` persists extracted
   `seedOut` plus a few observability fields on `PipelineState`.
4. `FlowSimulation.step(...)` emits `MonthTrace` from the month boundary.

Execution order and information ownership are still partially different. The
main transitional hotspot is that `FirmEconomics.prepareLending(...)` copies
same-month `s4` and `s2.operationalHiringSlack` into `w.pipeline`, so parts of
`Firm.process(...)` read same-month signals through `w.seedIn` and direct
`w.pipeline.*` access.

## Target Ownership Boundaries

The inventory assumes the following target owners:

- `DecisionSignals` / `seedIn`:
  persisted `pre` inputs only; valid owner is the month boundary, not an
  intra-month transport layer.
- same-month operational signals:
  explicit surfaces for `s2Pre`, `s4`, post-firm `s2`, and startup-staffing
  artifacts; these should feed intra-month consumers directly.
- extraction:
  one explicit `post -> pre` boundary that derives next-month
  `DecisionSignals` from realized month-`t` outcomes.
- tracing:
  a read-only emitted artifact that observes `seedIn`, selected same-month
  outputs, and `seedOut`, but does not transport operational data.

In practice this means:

- `World.seedIn` should stay the narrow persisted decision surface.
- the same-month bridge currently hidden inside `pipeline.copy(...)` belongs in
  a future `OperationalSignals`-style surface.
- `SignalExtraction.compute(...)` should remain the canonical extraction owner.
- `MonthTrace` should observe boundaries, not substitute for them.

## Current `seedIn` Consumers

Every current timing-sensitive `seedIn` read in `src/main` is classified below.

| Location | Current read | Class | Why it has this class | Target owner |
|----------|--------------|-------|------------------------|--------------|
| `DemandEconomics.compute(...)` | `seedIn.sectorHiringSignal` | `pre` | Lagged hiring signal is used as a persistence input when smoothing current `sectorPressure` into same-month `s4.sectorHiringSignal`. | `DecisionSignals` feeding same-month demand formation |
| `WorldAssemblyEconomics.runStep(...)` | full `seedIn` passed to `FirmEntry.LaggedEntrySignals.fromDecisionSignals(...)` | `pre` | `FirmEntry` is a macro decision block with start-of-month information semantics. | `DecisionSignals` |
| `FirmEconomics.compute(...)` | `seedIn.sectorDemandPressure`, `seedIn.sectorHiringSignal` | `transitional` | The standalone compute path reconstructs a same-month-shaped `DemandEconomics.Output` from lagged fields, so the temporal meaning is mixed back into an operational envelope. | explicit same-month demand surface |
| `BankingEconomics.compute(...)` | `seedIn.sectorDemandPressure`, `seedIn.sectorHiringSignal` | `transitional` | Same pattern as `FirmEconomics.compute(...)`: lagged signals are repackaged into a same-month `s4` object for downstream use. | explicit same-month demand surface |
| `Firm.desiredWorkers(...)` | `seedIn.sectorDemandMult`, `seedIn.sectorHiringSignal` | `transitional` | Incumbent firm planning runs inside `s5`, but the copied world from `prepareLending(...)` makes those reads observe same-month `s4` through the persisted `seedIn` accessor. | explicit firm-planning operational surface |
| `Firm.process(...)` execution path (`decideAutomated`, `decideHybrid`, `decideTraditional`, `applyInventory`) | `seedIn.sectorDemandMult` | `transitional` | PnL, inventory, and technology-choice logic use current-month demand multipliers, but they currently arrive by smuggling `s4` through `w.seedIn`. | explicit firm-planning operational surface |

### Classification Summary

- clean `pre` consumers: `DemandEconomics.compute(...)`,
  `WorldAssemblyEconomics.runStep(...)` for `FirmEntry`
- transitional consumers hidden behind `seedIn`:
  `FirmEconomics.compute(...)`, `BankingEconomics.compute(...)`, and the
  incumbent-firm execution path in `agents/Firm.scala`
- no current timing-sensitive `seedIn` read should remain in the
  `same-month` class once the boundary is made explicit; that class should move
  to dedicated operational surfaces instead of staying on `seedIn`

Boundary-observer note:

- `FlowSimulation.step(...)` reads `w.seedIn` for the trace start boundary and
  reads `assembled.world.seedIn` as the extracted `seedOut`. Those are audit
  reads, not timing-sensitive operational consumers.

## Remaining Direct `pipeline.*` Reads

Only one remaining direct `pipeline.*` read is timing-sensitive engine logic.

| Location | Direct field | Class | Why it has this class | Target owner |
|----------|--------------|-------|------------------------|--------------|
| `Firm.desiredWorkers(...)` | `w.pipeline.operationalHiringSlack` | `transitional` | This is a same-month labor-compression input consumed inside incumbent firm planning. It bypasses `seedIn`, but it is still transported through `PipelineState` rather than an explicit same-month surface. | explicit firm-planning operational surface |

The other direct `pipeline.*` reads in `src/main` are observability-only:

- `diagnostics/BankruptcyProbe.scala` reads `newWorld.pipeline.sectorDemandMult`
  for debug output.
- `montecarlo/SimOutput.scala` reads `fiscalRuleSeverity` and
  `govSpendingCutRatio` for exported columns.

Those reads do not currently participate in the timing contract of the engine
step itself.

## Same-Month Producers And Boundary Writers

The following blocks define the current same-month or boundary surfaces.

| Producer / boundary | Artifact | Class | Current consumers | Target owner |
|---------------------|----------|-------|-------------------|--------------|
| `LaborEconomics.compute(...)` | `s2Pre` | `same-month` | `DemandEconomics.compute(...)`, `FirmEconomics.runStep(...)` | early operational labor surface |
| `DemandEconomics.compute(...)` | `s4` (`sectorMults`, `sectorDemandPressure`, `sectorHiringSignal`, fiscal-rule status) | `same-month` | `FirmEconomics.runStep(...)`, `PriceEquityEconomics`, `OpenEconEconomics`, `BankingEconomics.runStep(...)`, extraction | same-month demand surface |
| `FirmEconomics.prepareLending(...)` | copied `World` with `pipeline.copy(...)` carrying `s4` plus `s2.operationalHiringSlack` | `transitional` | `Firm.process(...)` only | remove bridge; replace with explicit operational surface |
| `LaborEconomics.reconcilePostFirmStep(...)` | post-firm `s2` | `same-month` | `s6`, `s7`, `s8`, `s9`, extraction of `laggedHiringSlack`, startup staffing context | post-firm operational / realization surface |
| `WorldAssemblyEconomics.applyStartupStaffing(...)` | `startupAbsorptionRate`, startup HH aggregates, startup hires | `same-month` | `SignalExtraction.compute(...)`, final world assembly | startup-staffing operational surface |
| `SignalExtraction.compute(...)` | `seedOut: DecisionSignals` plus typed provenance | `post` boundary | `buildPipelineState(...)`, next month `w.seedIn`, `MonthTrace` | extraction boundary |
| `WorldAssemblyEconomics.buildPipelineState(...)` | persisted `PipelineState` for month `t+1` plus observability fields | `post` boundary | next month `World.seedIn`, diagnostics, MC output | persistence boundary |
| `FlowSimulation.step(...)` | `MonthTrace` | tracing boundary | tests, debugging, audit | tracing boundary |

## Key Inventory Findings

1. The dominant transitional mechanism is not `FirmEntry`; it is the
   incumbent-firm same-month bridge inside
   `FirmEconomics.prepareLending(...).pipeline.copy(...)`.
2. `seedIn` is already a good persisted owner for macro decision inputs, but it
   is still being used as an intra-month transport for incumbent firm logic.
3. `SignalExtraction.compute(...)` is the architectural choke point for
   `post -> pre` conversion and should remain singular.
4. There is only one remaining direct `pipeline.*` read in timing-sensitive
   engine code, but there are multiple transitional `seedIn` reads caused by
   the same hidden same-month bridge.
5. Diagnostics and MC output still read `PipelineState`, but those reads are
   observability surfaces, not step-timing surfaces.

## Downstream Refactor Map

- `#309` should introduce the explicit same-month operational surface that
  replaces the `pipeline.copy(...)` bridge used by incumbent firm processing.
- `#310` should isolate `SignalExtraction.compute(...)` and persistence as the formal
  `post -> pre` boundary of the monthly step.
- `#311` should make `MonthTrace` follow these boundaries instead of tracking
  transitional transport details ad hoc.
- `#312` should lock the resulting contract with regression coverage beyond the
  original `FirmEntry` path.
- `#313` remains the top-level owner of the eventual unfold-shaped monthly
  `step(state) -> (trace, next)` boundary.
