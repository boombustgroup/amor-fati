# Monte Carlo

The montecarlo package owns the Monte Carlo simulation loop, output
schema, summary statistics, and CSV I/O. It is the only consumer of
`McRunConfig` — the engine pipeline has no dependency on this package.

## Files

| File | Object | Role                                                                                                       |
|------|--------|------------------------------------------------------------------------------------------------------------|
| `McRunner.scala` | `McRunner` | MC loop: runs N seeds, collects results, writes CSV, prints summary                                        |
| `McRunConfig.scala` | `McRunConfig` | Runtime config from CLI args: `nSeeds`, `outputPrefix`, `runDurationMonths`, `runId`                       |
| `McTimeseriesSchema.scala` | `McTimeseriesSchema` | 239-column timeseries schema with explicit sector/region contracts — typed `Col` definitions, `compute` function |
| `McTimeseriesCsv.scala` | `McTimeseriesCsv` | Streaming per-seed timeseries CSV sink using the shared timeseries schema                                    |
| `McTerminalSummarySchema.scala` | `McTerminalSummarySchema` | Terminal summary schema definitions and seed-level row extraction                                             |
| `McTerminalSummaryCsv.scala` | `McTerminalSummaryCsv` | Terminal summary CSV writers using the shared terminal summary schemas                                        |
| `McTypes.scala` | `RunResult`, `TimeSeries`, `DescriptiveStats`, `McResults` | Zero-cost typed wrappers for simulation output and summary statistics                                      |

## Data flow

```
Main ──→ McRunner.run(rc)
           │
           ├── for seed ← 1..N:
           │     WorldInit.initialize(InitRandomness.Contract.fromSeed(seed))
           │     MonthDriver.unfoldSteps(...).take(runDurationMonths)
           │     McTimeseriesSchema.compute  → Array[Double]
           │
           ├── McResults.summarize  → DescriptiveStats per column
           └── CSV writers (terminal, timeseries, hh, banks)
```

`McRunner.runSingle` is the only bridge between this package and the
engine — it calls `WorldInit.initialize`, drives the shared
`MonthDriver.unfoldSteps` iterator, then maps each monthly state
through `McTimeseriesSchema.compute`.

`runDurationMonths` is a Monte Carlo/runtime concern. It controls how
many monthly snapshots the runner materializes, but it is not part of
`SimParams` and should not leak into economic decision rules.
