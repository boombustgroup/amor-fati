# Signal Timing Memo

Date: 2026-03-30
Branch: `fix/unemployment-root-cause`
Reference commit: `d9d38f4`

## Purpose

This memo captures the current month-`t` signal timing in the simulation pipeline.
The goal is to make lags explicit before further changes to labor, entry, startup,
or fiscal dynamics.

## Current Month Timeline

### 1. Pre-step state

At the start of month `t`, the model reads world state carried from the end of `t-1`.
This includes:

- `marketWage`
- `inflation`
- `expectedInflation`
- `sectorHiringSignal`
- `startupAbsorptionRate`

These are perception signals inherited from the previous step.

### 2. Early-month perception block

`FlowSimulation.computeAll(...)` first computes:

- `FiscalConstraintEconomics.compute(w)`
- `LaborEconomics.compute(w, firms, households, s1)`

This produces `s2Pre`, which acts as the first labor-market view for month `t`:

- wage
- employment
- labor demand
- aggregate hiring slack

This is the main perception signal used by downstream blocks.

### 3. Household income and demand in month `t`

The model then computes:

- household income / consumption from `s2Pre`
- demand formation from `s2Pre`

`DemandEconomics` also produces:

- `sectorDemandMult`
- `sectorDemandPressure`
- `sectorHiringSignal`

`sectorHiringSignal` is smoothed against the previous month's signal, so it is a hybrid:

- partly inherited from `t-1`
- partly updated using current-month demand conditions

### 4. Incumbent firm decisions in month `t`

`FirmEconomics.runStep(...)` then processes incumbent firm behavior:

- desired workers
- feasible workers
- upsize / downsize
- upgrades
- bankruptcies

Incumbent hiring currently uses mostly perception-style inputs:

- `sectorDemandMult`
- `sectorHiringSignal`
- `aggregateHiringSlack`

This is relatively coherent: firms react to signals available before final month-end outcomes are known.

### 5. Mid-step labor refresh after firm decisions

After `FirmEconomics`, the pipeline partially refreshes `s2`:

- `postLaborDemand`
- `post-firm employed`
- `post-firm aggregateHiringSlack`

So from this point onward, part of the model is no longer using pure perception signals.
It is using a mid-step outcome signal.

### 6. Prices, open economy, banking

The following blocks then consume the refreshed `s2`:

- household financial block
- price / equity block
- open economy block
- banking block

This means later month-`t` macro state already sees some same-month labor and firm outcomes.

### 7. World assembly and post-firm unemployment

`WorldAssemblyEconomics.runStep(...)` assembles a new world `newW` and computes:

- `postFirmEmployed`
- `unemploymentRate`

At this point, these are outcome signals for month `t`, not inherited perception signals.

### 8. Expansionary firm entry

`FirmEntry.process(...)` is called late in the step.
It currently uses a mixed timing set:

- `unemploymentRate`: post-firm month-`t`
- `inflation`: assembled month-`t`
- `expectedInflation`: assembled month-`t`
- `aggregateHiringSlack`: refreshed post-firm month-`t`
- `startupAbsorptionRate`: previous world's stored value, effectively `t-1`

This is the least timing-coherent block in the current architecture.

### 9. Same-month startup staffing

After entry, the model applies startup staffing in the same month:

- entrants attempt to hire
- `startupAbsorptionRate` is computed

This is operationally sensible: once firms are born, they can attempt to staff within month `t`.

### 10. Final outcome state

The model then writes month-`t` outcomes back into the world:

- `firmBirths`
- `netFirmBirths`
- `startupAbsorptionRate`

These become inherited perception signals for month `t+1`.

## Signal Classification

### Pre / perception signals

- previous `marketWage`
- previous `inflation`
- previous `expectedInflation`
- previous `sectorHiringSignal`
- previous `startupAbsorptionRate`

### Same-month operational signals

- incumbent hiring / downsizing
- startup staffing
- job matching

### Post / outcome signals

- post-firm employment
- assembled inflation
- month-`t` startup absorption
- firm births / deaths

## Review

### What looks coherent

1. Incumbent hiring is more coherent than before.
It mostly uses perception-style or smoothly updated signals rather than pure end-of-step outcomes.

2. Startup staffing as a same-month operational block is defensible.
If a firm is created in month `t`, it is reasonable that it can try to hire in month `t`.

### What looks incoherent

1. `expansionary net entry` is the weakest timing block.
It mixes:

- same-month unemployment
- same-month inflation
- same-month hiring slack
- previous-month startup absorption

This is neither a clean `t-1` decision rule nor a clean same-month operational rule.

2. `aggregateHiringSlack` currently sits in an ambiguous place.
It starts as a perception-style labor signal but is then refreshed after firm processing.
That makes it acceptable for diagnostics, but less clean as a macro decision input.

3. `startupAbsorptionRate` is operationally same-month, but currently acts as a lagged macro input.
This is not necessarily wrong, but it should be explicit rather than accidental.

## Preferred Timing Rule

For this architecture, the most defensible rule is:

- macro decision blocks should use signals available at the start of month `t`
- same-month operational blocks can use same-month vacancies, firms, and households
- realized outcomes from month `t` should mainly feed decisions in month `t+1`

Under that rule:

- `FirmEntry` should use clean perception inputs from `t-1` / start of `t`
- `startup staffing` should remain same-month
- `startupAbsorptionRate` from month `t` should influence entry in `t+1`, not entry in `t`

## Main Takeaway

At this point, the biggest timing risk in the architecture is not incumbent hiring.
It is the mixed-timing logic inside `expansionary net entry`.

That block should be treated as the primary place where lag semantics need to be made explicit before more tuning is attempted.
