# Simulation Engine

The engine package orchestrates the monthly simulation loop. It owns the
`World` state and delegates domain logic to four subpackages: **economics**
(9-stage computation pipeline), **flows** (SFC-verified monetary flow
emission via ledger), **markets** (clearing mechanisms), and **mechanisms**
(policy / regulatory rules).

```
engine/
├── World.scala             # Immutable global state container (7 nested types)
├── economics/              # 9-stage computation pipeline (calculus, no flows)
├── flows/                  # SFC flow emission via verified ledger
├── markets/                # Market clearing & price formation
└── mechanisms/             # Policy rules & regulatory instruments
```

## Core files

| File | Responsibility |
|------|----------------|
| `World.scala` | Case class holding all macro state, decomposed into 7 nested types: `SocialState`, `FinancialMarketsState`, `ExternalState`, `RealState`, `MechanismsState`, `MonetaryPlumbingState`, `FlowState`. |

## economics/

The 9-stage computation pipeline, executed in fixed order each month. Each
module is a pure function producing calculus (quantities, rates, decisions)
without emitting monetary flows. `FlowSimulation` wires them together.

| File | Stage | Domain |
|------|-------|--------|
| `FiscalConstraintEconomics.scala` | s1 | Minimum wage indexation, reservation wage, lending base rate |
| `LaborEconomics.scala` | s2 | Phillips curve + expectations + union rigidity wages, employment, demographics, immigration |
| `HouseholdIncomeEconomics.scala` | s3 | Individual HH income, consumption, saving, portfolio; labor separations, wage updates, bank-specific rates, equity returns, sectoral mobility |
| `DemandEconomics.scala` | s4 | Sector demand allocation: HH consumption, government purchases, investment, exports; capacity constraints and spillover |
| `FirmEconomics.scala` | s5 | Production, I-O intermediate market, CAPEX, financing splits (equity/bonds/loans), labor matching, NPL detection |
| `HouseholdFinancialEconomics.scala` | s6 | Mortgage debt service, deposit interest, diaspora remittances, tourism, consumer credit aggregation |
| `PriceEquityEconomics.scala` | s7 | Inflation, GPW equity, sigma dynamics, network rewiring, GDP, macroprudential, EU funds |
| `OpenEconEconomics.scala` | s8 | BoP/forex, GVC trade, Taylor rule, bond yields, interbank, corporate bonds, insurance, NBFI |
| `BankingEconomics.scala` | s9 | Bank P&L, provisioning, CAR, multi-bank resolution, bail-in, interbank, BFG levy, monetary aggregates (M1/M2/M3) |
| `WorldAssemblyEconomics.scala` | final | Aggregation, informal economy, observables; assembles final World state + updated agents, 14-identity SFC check |

## flows/

SFC-verified monetary flow emission. Every PLN flow is recorded via the
`amor-fati-ledger` (formally verified with Stainless/Z3) and checked
against 14 accounting identities each month.

| File | Responsibility |
|------|----------------|
| `FlowSimulation.scala` | Sole pipeline entry point. `step()` runs the 9-stage `computeAll()`, then `emitAllFlows()` to record all monetary flows. Produces `StepResult(world, firms, households)`. |
| `FlowMechanism.scala` | Enum of ~80 named flow mechanisms (e.g. `FirmWages`, `HhConsumption`, `BankBfgLevy`). Each flow in the system maps to exactly one mechanism. |
| `StateAdapter.scala` | Bridges computation outputs to flow inputs: extracts ZUS, NFZ, PPK, earmarked, HH, insurance, and firm flow parameters from `FullComputation`. |
| `ZusFlows.scala` | ZUS/FUS pensions: contributions (HH → FUS), pensions (FUS → HH), gov subvention covering deficit |
| `NfzFlows.scala` | NFZ (National Health Fund): 9% składka zdrowotna, healthcare spending, gov subvention |
| `PpkFlows.scala` | PPK (Pracownicze Plany Kapitałowe): employee + employer contributions, bond purchases |
| `EarmarkedFlows.scala` | Earmarked funds (FP, PFRON, FGSP): contributions, spending, gov subvention covering deficit |
| `HouseholdFlows.scala` | HH aggregate flows: consumption, rent, PIT, debt service, deposits, remittances |
| `FirmFlows.scala` | Firm aggregate flows: wages, CIT, loans, investment, I-O, NPL, FDI |
| `GovBudgetFlows.scala` | Government budget: tax revenue, purchases, benefits, transfers, debt service, capital investment |
| `BankingFlows.scala` | Bank P&L flows: gov bond income, reserve/standing facility/interbank interest, BFG levy, unrealized losses, bail-in, NBP remittance |
| `EquityFlows.scala` | GPW: dividends (domestic net of Belka tax, foreign), equity issuance |
| `CorpBondFlows.scala` | Catalyst: coupon, default loss, issuance, amortization |
| `MortgageFlows.scala` | Housing: origination, principal repayment, interest, default |
| `InsuranceFlows.scala` | Insurance: life + non-life premiums, claims, investment income |
| `JstFlows.scala` | JST (local government): PIT/CIT shares, property tax, subventions, spending |
| `OpenEconFlows.scala` | BoP: trade, FDI, portfolio, primary income (NFA), secondary income (EU funds, diaspora), tourism, capital flight |

## markets/

Stateless (or thin-state) market-clearing modules. Each computes
equilibrium prices, quantities, or flows given current state.

| File | Domain |
|------|--------|
| `LaborMarket.scala` | Wage Phillips curve, worker separations, job search with sectoral priority |
| `PriceLevel.scala` | Inflation: demand-pull + cost-push + import pass-through, soft floor |
| `CalvoPricing.scala` | Calvo staggered pricing: per-firm markup lottery (θ=15%), endogenous markup, sticky prices |
| `OpenEconomy.scala` | BoP, floating exchange rate, trade balance, capital account, NFA |
| `FiscalBudget.scala` | Government budget: revenue (CIT/VAT/excise/customs), spending, deficit, bond issuance |
| `FiscalRules.scala` | Polish fiscal rules: SRW (stabilizing expenditure rule), SGP 3% deficit, Art. 216 debt brake, consolidation 55% |
| `EquityMarket.scala` | GPW: WIG index, market cap, dividend yield, foreign ownership, issuance |
| `HousingMarket.scala` | House price index (aggregate + 7 regions), mortgage origination/default/amortization |
| `CorporateBondMarket.scala` | Catalyst: corporate bond issuance, coupon, default, demand-side absorption |
| `BondAuction.scala` | SPW primary market: foreign demand f(yield spread vs Bund, ER), absorption constraint |
| `CapitalFlows.scala` | Capital flight: risk-off shock, carry trade (accumulation/unwind), auction confidence signal |
| `GvcTrade.scala` | GVC deep external sector: foreign firm partners, sector-level trade, disruption shocks |
| `IntermediateMarket.scala` | I-O intermediate goods: inter-sector purchases via input-output matrix |
| `RegionalClearing.scala` | Regional labor markets: 6 independent Phillips curves (NUTS-1), population-weighted national wage (Kahan sum) |

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

## How to extend

**Adding a new market** (e.g., derivatives, crypto):
1. Create `markets/NewMarket.scala` with a `step(...)` or `update(...)` function.
2. Add state to `World.scala` if the market carries state across months.
3. Wire the call into the appropriate economics stage (usually s5–s8).
4. If flows affect bank capital, deposits, or government — add a `FlowMechanism` entry and corresponding `*Flows.scala`.

**Adding a new mechanism** (e.g., carbon tax, capital controls):
1. Create `mechanisms/NewMechanism.scala` — pure function, no World dependency.
2. Call it from the relevant economics stage. Mechanisms are typically stateless or
   carry minimal state on `World`.

**Adding a new flow:**
1. Add a case to the `FlowMechanism` enum in `FlowMechanism.scala`.
2. Create or extend the appropriate `*Flows.scala` to emit the flow.
3. Wire the emission call in `FlowSimulation.emitAllFlows()`.
4. Update `Sfc.MonthlyFlows` / `Sfc.Snapshot` so the 14-identity check covers the new flow.

**SFC rule:** Any flow that modifies bank capital, deposits, government
debt, NFA, bond holdings, or interbank positions **must** be reflected in
`Sfc.MonthlyFlows` / `Sfc.Snapshot`. The 14-identity check runs
every month and will fail at runtime if the accounting is broken.
