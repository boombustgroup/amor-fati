# SFC-ABM Core

Stock-Flow Consistent Agent-Based Model engine for macroeconomic simulation.

## Overview

A type-safe, bilaterally consistent simulation engine where every monetary flow has a source and a sink. Built for studying economic phase transitions, non-ergodicity, and emergent dynamics in heterogeneous agent networks.

The model simulates **10 000 firms** across **6 sectors** (GUS 2024 calibration) interacting on a **Watts-Strogatz small-world network** over 120 months, with full Monte Carlo support.

## Architecture

```
src/main/scala/sfc/
├── Main.scala                    # @main entry point + Monte Carlo loop
├── config/
│   └── SimConfig.scala           # Config, SectorDef, SECTORS, RunConfig
├── agents/
│   ├── Firm.scala                # TechState, Firm, FirmOps, FirmLogic
│   ├── Household.scala           # HhState
│   └── CentralBank.scala         # NbpState
├── engine/
│   ├── Simulation.scala          # Sectors (labor, inflation, NBP, forex, gov) + Simulation.step()
│   └── World.scala               # World state
├── networks/
│   └── WattsStrogatz.scala       # Small-world network generator + local auto ratio
└── sfc/
    └── BalanceSheet.scala        # GovState, BankState, ForexState
```

### Key components

| Module | Description |
|--------|-------------|
| **Config** | 40+ parameters calibrated to GUS/NBP/ECB 2024 data |
| **Agents** | Firm technology adoption (Traditional → Hybrid → Automated) with network-aware mimetic pressure, uncertainty discount, and sector-specific CES elasticity (σ) |
| **Engine** | Monthly step: labor market clearing, Phillips curve inflation, Taylor rule, forex (IRP), fiscal balance |
| **Networks** | Watts-Strogatz graph (k=6, p=0.10) for demonstration effect diffusion |
| **SFC** | Balance sheet consistency: every flow has a source and a sink |

### Sectors (GUS 2024)

| Sector | Share | σ (CES) | Digital readiness |
|--------|------:|--------:|------------------:|
| BPO/SSC | 3% | 50.0 | 0.50 |
| Manufacturing | 16% | 10.0 | 0.45 |
| Retail/Services | 45% | 5.0 | 0.40 |
| Healthcare | 6% | 2.0 | 0.25 |
| Public | 22% | 1.0 | 0.08 |
| Agriculture | 8% | 3.0 | 0.12 |

## Usage

### Prerequisites

- JDK 17+
- [sbt](https://www.scala-sbt.org/)

### Run simulation

```bash
# BDP = 2000 PLN, 100 Monte Carlo seeds, output prefix "baseline"
sbt "run 2000 100 baseline"

# No BDP scenario
sbt "run 0 100 nobdp"

# High BDP scenario
sbt "run 3000 100 bdp3000"
```

Output CSVs are written to `mc/`:
- `mc/<prefix>_terminal.csv` — per-seed terminal values (month 120)
- `mc/<prefix>_timeseries.csv` — aggregated time-series (mean, std, p05, p95)

### Run via Ammonite (original script)

```bash
BDP=2000 SEEDS=100 PREFIX=baseline amm simulation_mc.sc
```

## Tech Stack

- **Scala 3.5** / sbt
- Zero external dependencies (stdlib only)

## Related

- [`paper-01-acceleration-paradox`](https://github.com/complexity-econ/paper-01-acceleration-paradox) — paper, analysis scripts, and figures using this engine

## Citation

See [CITATION.cff](CITATION.cff)

## License

MIT
