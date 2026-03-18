# Simulation Engine

The engine package orchestrates the monthly simulation loop. It owns the
`World` state, the 10-step pipeline, and delegates domain logic to two
subpackages: **markets** (clearing mechanisms) and **mechanisms** (policy /
regulatory rules).

```
engine/
├── Simulation.scala        # 10-step pipeline orchestrator
├── World.scala             # Immutable global state container
├── StepSeeds.scala         # Per-component RNG isolation
├── markets/                # Market clearing & price formation
├── mechanisms/             # Policy rules & regulatory instruments
└── steps/                  # Pipeline stages (typed Input/Output)
```

## Core files

| File | Responsibility |
|------|----------------|
| `Simulation.scala` | Executes the 10-stage monthly pipeline: transforms `(World, firms, households)` → `(World', firms', households')`. Pure function, no side effects. |
| `World.scala` | Case class holding all macro state: month, inflation, price level, government, NBP, banking sector, equity, housing, BOP, expectations, etc. |
| `StepSeeds.scala` | Per-component RNG isolation. Each pipeline step receives its own deterministic `Random` derived from `(masterSeed, month, componentId)`, so adding stochastic draws in one step never shifts the sequence of another. |

## markets/

Stateless (or thin-state) market-clearing modules. Each computes
equilibrium prices, quantities, or flows given current state.

| File | Domain |
|------|--------|
| `LaborMarket.scala` | Wage Phillips curve, worker separations, job search with sectoral priority |
| `PriceLevel.scala` | Inflation: demand-pull + cost-push + import pass-through − tech deflation, soft floor |
| `OpenEconomy.scala` | BoP, floating exchange rate, trade balance, capital account, NFA |
| `FiscalBudget.scala` | Government budget: revenue (CIT/VAT/excise/customs), spending, deficit, bond issuance |
| `EquityMarket.scala` | GPW: WIG index, market cap, dividend yield, foreign ownership, issuance |
| `HousingMarket.scala` | House price index (aggregate + 7 regions), mortgage origination/default/amortization |
| `CorporateBondMarket.scala` | Catalyst: corporate bond issuance, coupon, default, demand-side absorption |
| `BondAuction.scala` | SPW primary market: foreign demand f(yield spread vs Bund, ER), absorption constraint |
| `CapitalFlows.scala` | Capital flight: risk-off shock, carry trade (accumulation/unwind), auction confidence signal |
| `GvcTrade.scala` | GVC deep external sector: foreign firm partners, sector-level trade, disruption shocks |
| `IntermediateMarket.scala` | I-O intermediate goods: inter-sector purchases via input-output matrix |

## mechanisms/

Policy instruments and regulatory rules that modify agent behavior but
don't clear markets themselves.

| File | Domain |
|------|--------|
| `EuFunds.scala` | EU structural funds: Beta-curve absorption timing, co-financing, capital investment |
| `Expectations.scala` | Inflation expectations: adaptive-anchoring hybrid, central bank credibility |
| `FirmEntry.scala` | Endogenous firm entry: profit-weighted sector choice, regulatory barriers, AI-native startups |
| `Macroprudential.scala` | CCyB (countercyclical capital buffer), credit-to-GDP gap, O-SII buffers |
| `SectoralMobility.scala` | Cross-sector labor transitions: friction matrix, voluntary quits, wage penalties |
| `TaxRevenue.scala` | Fiscal revenue: VAT, excise, customs, informal-economy evasion adjustments |
| `YieldCurve.scala` | Interbank term structure: WIRON overnight → WIBOR 1M/3M/6M with term premia |

## steps/

The 10-stage pipeline, executed in fixed order each month. Each step is a
pure function `Input → Output` with typed case classes — no hidden state.

| # | Step | What it does |
|---|------|-------------|
| 1 | `FiscalConstraintStep` | Minimum wage path, lending base rate |
| 2 | `LaborDemographicsStep` | Wage clearing, employment, demographics (aging/migration), ZUS/NFZ/PPK |
| 3 | `HouseholdIncomeStep` | Disposable income, consumption, PIT, wealth effects |
| 4 | `DemandStep` | Per-sector demand multipliers, government purchases, capacity constraints |
| 5 | `FirmProcessingStep` | Production, I-O, tech adoption, credit origination, NPL, equity issuance |
| 6 | `HouseholdFinancialStep` | Consumer credit, mortgages, remittances, diaspora, tourism |
| 7 | `PriceEquityStep` | Inflation, GPW equity, sigma dynamics, network rewiring, GDP, EU funds |
| 8 | `OpenEconomyStep` | BoP/forex, GVC, Taylor rule, bond yields, QE, insurance, NBFI |
| 9 | `BankUpdateStep` | Bank P&L, provisioning, CAR, interbank, BFG levy, bail-in |
| 10 | `WorldAssemblyStep` | Final state assembly, household reassignment, 14-identity SFC check |

Steps reference each other via explicit `s1: Step1.Output, s2: Step2.Output, ...`
fields in their `Input` case classes. This makes data flow visible and
prevents hidden coupling.

## How to extend

**Adding a new market** (e.g., derivatives, crypto):
1. Create `markets/NewMarket.scala` with a `step(...)` or `update(...)` function.
2. Add state to `World.scala` if the market carries state across months.
3. Wire the call into the appropriate pipeline step (usually 5–8).
4. If flows affect bank capital, deposits, or government — update `SfcCheck`.

**Adding a new mechanism** (e.g., carbon tax, capital controls):
1. Create `mechanisms/NewMechanism.scala` — pure function, no World dependency.
2. Call it from the relevant step. Mechanisms are typically stateless or
   carry minimal state on `World`.

**Adding a new pipeline step:**
1. This should be rare — the 10-step ordering is economically determined.
2. If needed, create `steps/NewStep.scala` with `Input`/`Output` case classes.
3. Add the call in `Simulation.scala` at the correct position.
4. Thread `Output` into downstream steps via their `Input` fields.

**SFC rule:** Any flow that modifies bank capital, deposits, government
debt, NFA, bond holdings, or interbank positions **must** be reflected in
`Sfc.MonthlyFlows` / `Sfc.Snapshot`. The 14-identity check runs
every month and will fail at runtime if the accounting is broken.
