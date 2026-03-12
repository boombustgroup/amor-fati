# Monte Carlo

The montecarlo package owns the Monte Carlo simulation loop, output
schema, summary statistics, and CSV I/O. It is the only consumer of
`McRunConfig` — the engine pipeline has no dependency on this package.

## Files

| File | Object | Role |
|------|--------|------|
| `McRunner.scala` | `McRunner` | MC loop: runs N seeds, collects results, writes CSV, prints summary |
| `McRunConfig.scala` | `McRunConfig` | Runtime config from CLI args: `nSeeds`, `outputPrefix` |
| `SimOutput.scala` | `SimOutput` | 197-column output schema — typed `Col` definitions, `compute` function |
| `McTypes.scala` | `RunResult`, `TimeSeries`, `DescriptiveStats`, `McResults` | Zero-cost typed wrappers for simulation output and summary statistics |

## Data flow

```
Main ──→ McRunner.run(rc)
           │
           ├── for seed ← 1..N:
           │     WorldInit.initialize(seed)
           │     Simulation.step  (no McRunConfig)
           │     SimOutput.compute  → Array[Double]
           │
           ├── McResults.summarize  → DescriptiveStats per column
           └── CSV writers (terminal, timeseries, hh, banks)
```

`McRunner.runSingle` is the only bridge between this package and the
engine — it calls `WorldInit.initialize` and `Simulation.step`, then
maps the resulting state through `SimOutput.compute`.
