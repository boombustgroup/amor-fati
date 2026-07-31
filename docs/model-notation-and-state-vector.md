# Model Notation And State Vector

This document defines the canonical notation layer for publication-facing Amor
Fati model descriptions. It is a map from mathematical symbols to the current
executable engine, not a new model and not a replacement for detailed equation,
ODD, SFC, calibration, or validation documents.

Use this document when writing monthly-transition, sector-equation,
stochasticity, SFC-mapping, calibration, or paper-facing overview material.

## Source Documents

The canonical reviewer path lives in
[model-specification.md#reviewer-reading-path](model-specification.md#reviewer-reading-path).
The table below is a local source map for notation work, not a separate
top-level reading order.

| Source | Role |
| --- | --- |
| [Monthly transition function](monthly-transition-function.md) | Formal $X_{t} \to X_{\tau}$ transition, randomness contract, phase timeline, emitted evidence, and month-step invariants. |
| [Stochastic processes and replay](stochastic-processes-and-replay.md) | Publication-facing seed contracts, stream maps, Monte Carlo seed policy, deterministic replay, and stochastic limitations. |
| [Banking and financial-sector equations](banking-and-financial-sector-equations.md) | Publication-facing bank, financial-stability, and financial-sector interface equations. |
| [Model equations to SFC map](model-equations-to-sfc-map.md) | Bridge from state variables and equation families to generated SFC rows, identities, runtime evidence, and accounting limitations. |
| [ODD / ODD+D model documentation](odd-model-documentation.md) | Purpose, entities, state variables, scales, scheduling, initialization, and decision-making context. |
| [Behavioral equations and decision rules](behavioral-equations-and-decision-rules.md) | Implemented household, firm, bank, fiscal, monetary, external, insurance, NBFI, quasi-fiscal, and JST rule surfaces. |
| [SFC matrix evidence](sfc-matrix-evidence.md) | Balance Sheet Matrix, Transactions Flow Matrix, stock-flow reconciliation evidence, sign conventions, and generated matrix artifacts. |
| [Engine invariants and economic semantics](engine-invariants-and-semantics.md) | Hard invariants, normal-path expectations, known limitations, enforcement points, and coverage. |
| [Validation matrix](validation-matrix.md) | CI, integration, generated-output, nightly, stress, profiling, and validation ownership boundaries. |
| [Calibration register](calibration-register.md) | Parameter names, units, implementation owners, empirical targets, and provenance status. |

Implementation anchors:

- [`engine/flows/FlowSimulation.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/flows/FlowSimulation.scala)
- [`engine/World.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/World.scala)
- [`engine/WorldStateSegments.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/WorldStateSegments.scala)
- [`engine/ledger/LedgerFinancialState.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/ledger/LedgerFinancialState.scala)
- [`agents/Household.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Household.scala)
- [`agents/Firm.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Firm.scala)
- [`agents/Banking.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Banking.scala)
- [`types.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/types.scala)
- [`engine/MonthRandomness.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/MonthRandomness.scala)

## Notation Rules

The notation follows the runtime ownership model:

- boundary stocks carry a time subscript, e.g. $X_{t}$, $D^{H}_{h,t}$;
- monthly flows use the execution-month subscript, e.g. $c^{H}_{h,\tau}$;
- $\tau = t + 1$ denotes the month executed from boundary state $X_{t}$;
- rates are annual unless explicitly marked as monthly;
- shares are dimensionless and bounded in `[0, 1]` unless the source rule
  states otherwise;
- all monetary quantities are PLN unless explicitly marked as foreign-currency
  or index values;
- ledger-owned financial stocks live in `LedgerFinancialState`;
- behavioral and diagnostic stocks that are not ledger-owned must be named as
  such rather than silently treated as transferable owner-resolved assets.

## Time And Transition Boundary

Let $X_{t}$ denote the complete simulation boundary after month `t` has been
completed. In code this is `FlowSimulation.SimState` with
$\mathrm{completedMonth} = t$.

One execution month is a transition:

$$
\begin{aligned}
\Phi_{\tau} : (X_{t}, RND_{\tau}, \theta) \to (X_{\tau}, E_{\tau}) \\
\tau &= t + 1
\end{aligned}
$$

where:

| Symbol | Meaning | Implementation anchor |
| --- | --- | --- |
| $X_{t}$ | start boundary after completed month `t` | `FlowSimulation.SimState` |
| $RND_{\tau}$ | explicit randomness contract for month $\tau$ | `MonthRandomness.Contract` |
| $\theta$ | model parameters and scenario-adjusted configuration | `SimParams` and scenario registry |
| $\Phi_{\tau}$ | executable one-month transition | `FlowSimulation.step` |
| $E_{\tau}$ | transition evidence: trace, emitted flows, SFC validation, deltas, diagnostics | `FlowSimulation.StepOutput`, `MonthTrace` |
| $X_{\tau}$ | next boundary after month $\tau$ closes | `StepOutput.nextState` |

The temporal order is:

$$
\begin{aligned}
\text{pre boundary}
{} \to \text{same-month economics}
{} \to \text{same-month boundary views} \\
{} \to \text{semantic closed month and seed extraction}
{} \to \text{flow emission} \\
{} \to \text{runtime ledger execution}
{} \to \text{next-pre materialization}
{} \to \text{SFC validation gate}
\end{aligned}
$$

This order is formalized in [monthly-transition-function.md](monthly-transition-function.md)
and described operationally in
`modules/model/src/main/scala/com/boombustgroup/amorfati/engine/README.md`.

## Full State Vector

The canonical boundary state is:

$$
\begin{aligned}
X_{t} &= (m_{t}, W_{t}, F_{t}, H_{t}, B_{t}, A^{H}_{t}, L_{t})
\end{aligned}
$$

| Symbol | Runtime field | Meaning |
| --- | --- | --- |
| $m_{t}$ | `SimState.completedMonth` | completed month index |
| $W_{t}$ | `SimState.world` | macro, market, mechanism, signal, and diagnostic world state |
| $F_{t}$ | `SimState.firms` | vector of firm behavioral states |
| $H_{t}$ | `SimState.households` | vector of household behavioral states |
| $B_{t}$ | `SimState.banks` | vector of bank operational states |
| $A^{H}_{t}$ | `SimState.householdAggregates` | household aggregate diagnostics and market aggregates |
| $L_{t}$ | `SimState.ledgerFinancialState` | ledger-owned financial balances |

$X_{t}$ is not a single undifferentiated object. It is a typed boundary between
three state surfaces:

| Surface | Symbol family | Runtime source | Publication meaning |
| --- | --- | --- | --- |
| Behavioral agent state | $H_{t}$, $F_{t}$, $B_{t}$ | household, firm, and bank state vectors | heterogeneous decision state and operational status |
| Macro and market state | $W_{t}$, $A^{H}_{t}$ | `World`, `Household.Aggregates` | prices, policies, market memory, signals, diagnostics, and aggregates |
| Ledger-owned financial state | $L_{t}$ | `LedgerFinancialState` | supported financial owner/issuer balances used by runtime execution and SFC validation |

## Entity And Index Sets

| Symbol | Meaning | Runtime anchor |
| --- | --- | --- |
| $h \in H_{t}$ | household agent | `Household.State.id: HhId` |
| $f \in F_{t}$ | firm agent | `Firm.State.id: FirmId` |
| $b \in B$ | bank row | `Banking.BankState.id: BankId` |
| $s \in S$ | production sector | `SectorIdx`, `SimParams.sectorDefs` |
| $r \in R$ | NUTS-1 macroregion | `Region` |
| $q \in Q$ | ledger-facing entity sector | `EntitySector` |
| $k \in K_{R}$ | named randomness stream | `MonthRandomness.StreamKey` |

The bank set `B` is a fixed vector of banking-sector rows: named bank
archetypes plus the residual Other banks row. A bank row is not a full legal
entity model of a single commercial bank.

## Behavioral Agent State

### Households

For household `h`, write behavioral state as:

$$
\begin{aligned}
a^{H}_{h,t}
\end{aligned}
$$

This is the non-ledger household state in `Household.State`: employment or
activity status, rent, skill, health penalty, MPC, social-neighbor ids, bank
routing id, last sector, immigrant flag, dependent children, education, task
routineness, wage scar, contract type, region, and financial-distress state.

Ledger-backed household balances are not part of $a^{H}_{h,t}$. They are:

$$
\begin{aligned}
l^{H}_{h,t} \in L_{t}^{\mathrm{households}}
\end{aligned}
$$

with the main stock families:

| Symbol | Runtime field | Type | Meaning |
| --- | --- | --- | --- |
| $D^{H}_{h,t}$ | `HouseholdBalances.demandDeposit` | stock, PLN | household demand-deposit asset |
| $M^{H}_{h,t}$ | `HouseholdBalances.mortgageLoan` | stock, PLN | household mortgage principal liability |
| $CL^{H}_{h,t}$ | `HouseholdBalances.consumerLoan` | stock, PLN | household consumer-loan principal liability |
| $E^{H}_{h,t}$ | `HouseholdBalances.equity` | stock, PLN | listed-equity asset |
| $IR^{life}_{h,t}$ | `HouseholdBalances.lifeReserveAsset` | stock, PLN | life-insurance reserve asset |
| $IR^{nonlife}_{h,t}$ | `HouseholdBalances.nonLifeReserveAsset` | stock, PLN | non-life insurance reserve asset |

### Firms

For firm `f`, write behavioral state as:

$$
\begin{aligned}
a^{F}_{f,t}
\end{aligned}
$$

This is the non-ledger firm state exposed through `Firm.State`: sector,
technology regime, worker/capacity logic, productivity, capital/inventory
structure, digital readiness, operational status, hiring signal state, and
decision-relevant attributes.

Ledger-backed firm balances are:

$$
\begin{aligned}
l^{F}_{f,t} \in L_{t}^{\mathrm{firms}}
\end{aligned}
$$

with the main stock families:

| Symbol | Runtime field | Type | Meaning |
| --- | --- | --- | --- |
| $Cash^{F}_{f,t}$ | `FirmBalances.cash` | stock, PLN | firm liquidity |
| $L^{F}_{f,t}$ | `FirmBalances.firmLoan` | stock, PLN | outstanding bank-loan principal |
| $CB^{F}_{f,t}$ | `FirmBalances.corpBond` | stock, PLN | corporate bonds issued by the firm |
| $Eq^{F}_{f,t}$ | `FirmBalances.equity` | stock, PLN | listed equity issued by the firm |

### Banks

For bank row `b`, write operational state as:

$$
\begin{aligned}
a^{B}_{b,t}
\end{aligned}
$$

This is `Banking.BankState`: id, regulatory capital, NPL stock diagnostics,
HTM book yield, active/failed status, corporate-loan maturity buckets,
consumer NPL stock, and IFRS 9 ECL staging.

Bank financial ownership balances are:

$$
\begin{aligned}
l^{B}_{b,t} \in L_{t}^{\mathrm{banks}}
\end{aligned}
$$

with the main stock families:

| Symbol | Runtime field | Type | Meaning |
| --- | --- | --- | --- |
| $D^{B}_{b,t}$ | `BankBalances.totalDeposits` | stock, PLN | total customer deposit liability |
| $D^{B,demand}_{b,t}$ | `BankBalances.demandDeposit` | stock, PLN | demand-deposit liability |
| $D^{B,term}_{b,t}$ | `BankBalances.termDeposit` | stock, PLN | term-deposit liability |
| $L^{B,F}_{b,t}$ | `BankBalances.firmLoan` | stock, PLN | firm-loan asset |
| $L^{B,H}_{b,t}$ | `BankBalances.consumerLoan` | stock, PLN | consumer-loan asset |
| $M^{B}_{b,t}$ | `BankBalances.mortgageLoan` | stock, PLN | mortgage-loan asset mirror used for BSM evidence |
| $GB^{AFS}_{b,t}$ | `BankBalances.govBondAfs` | stock, PLN | available-for-sale government-bond asset |
| $GB^{HTM}_{b,t}$ | `BankBalances.govBondHtm` | stock, PLN | held-to-maturity government-bond asset |
| $Res^{B}_{b,t}$ | `BankBalances.reserve` | stock, PLN | reserve asset at NBP |
| $IB_{b,t}$ | `BankBalances.interbankLoan` | stock, PLN | net interbank position |
| $CB^{B}_{b,t}$ | `BankBalances.corpBond` | stock, PLN | corporate-bond asset |
| $K^{B}_{b,t}$ | `BankState.capital` | diagnostic stock, PLN | regulatory/accounting bank-capital buffer, not holder-resolved ledger equity |

$K^{B}_{b,t}$ is intentionally outside `LedgerFinancialState.BankBalances`.
It is persisted bank regulatory/accounting state validated by SFC, not
transferable holder-resolved bank equity.

## Macro And Market State

Write the world state as:

$$
\begin{aligned}
W_{t} &= (P_{t}, \pi_{t}, \sigma_{t}, W^{gov}_{t}, W^{nbp}_{t}, W^{bank}_{t}, \\
&\quad W^{fx}_{t}, W^{bop}_{t}, W^{hhm}_{t}, W^{soc}_{t}, W^{fin}_{t}, \\
&\quad W^{ext}_{t}, W^{real}_{t}, W^{mech}_{t}, W^{plumb}_{t}, \\
&\quad W^{pipe}_{t}, W^{flow}_{t}, W^{regwage}_{t})
\end{aligned}
$$

| Symbol | Runtime field | Meaning |
| --- | --- | --- |
| $\pi_{t}$ | `World.inflation` | CPI YoY inflation rate |
| $P_{t}$ | `World.priceLevel` | cumulative CPI price index |
| $\sigma_{t}$ | `World.currentSigmas` | per-sector production sigma values |
| $W^{gov}_{t}$ | `World.gov` | central-government budget and debt state |
| $W^{nbp}_{t}$ | `World.nbp` | NBP policy, QE, FX-operation state |
| $W^{bank}_{t}$ | `World.bankingSector` | banking macro state, bank configs, interbank conditions, rate term structure |
| $W^{fx}_{t}$ | `World.forex` | exchange rate, exports, imports, trade balance state |
| $W^{bop}_{t}$ | `World.bop` | balance-of-payments state: NFA, current account, capital account, FDI |
| $W^{hhm}_{t}$ | `World.householdMarket` | household market wage and reservation wage |
| $W^{soc}_{t}$ | `World.social` | JST, ZUS, NFZ, PPK, demographics, earmarked funds |
| $W^{fin}_{t}$ | `World.financialMarkets` | equity, corporate-bond, insurance, NBFI, quasi-fiscal market memory |
| $W^{ext}_{t}$ | `World.external` | GVC, immigration, tourism state |
| $W^{real}_{t}$ | `World.real` | housing, sectoral mobility, investment, energy, productivity, automation/hybrid shares |
| $W^{mech}_{t}$ | `World.mechanisms` | macroprudential, expectations, BFG fund, informal-economy adjustment |
| $W^{plumb}_{t}$ | `World.plumbing` | reserve interest, standing facility, interbank interest, deposit facility, flow-of-funds residual |
| $W^{pipe}_{t}$ | `World.pipeline` | persisted next-month decision-signal surface |
| $W^{flow}_{t}$ | `World.flows` | single-step derived flow diagnostics used by SFC identities and outputs |
| $W^{regwage}_{t}$ | `World.regionalWages` | regional wage cache |

`World` does not own supported financial balances. It can store market memory,
diagnostic flows, unsupported transition fields, and signal surfaces; supported
financial ownership belongs in $L_{t}$.

## Ledger-Owned Financial State

Write the ledger-owned financial state as:

$$
\begin{aligned}
L_{t} &= (L^{H}_{t}, L^{F}_{t}, L^{B}_{t}, L^{G}_{t}, L^{ROW}_{t}, \\
&\quad L^{\mathrm{NBP}}_{t}, L^{INS}_{t}, L^{FUNDS}_{t})
\end{aligned}
$$

| Symbol | Runtime field | Meaning |
| --- | --- | --- |
| $L^{H}_{t}$ | `LedgerFinancialState.households` | household financial balances |
| $L^{F}_{t}$ | `LedgerFinancialState.firms` | firm financial balances |
| $L^{B}_{t}$ | `LedgerFinancialState.banks` | bank financial balances |
| $L^{G}_{t}$ | `LedgerFinancialState.government` | central-government issued bond stock |
| $L^{ROW}_{t}$ | `LedgerFinancialState.foreign` | foreign-sector government-bond, foreign-asset, and equity holdings |
| $L^{NBP}_{t}$ | `LedgerFinancialState.nbp` | NBP government-bond holdings, foreign assets, and reserve liability |
| $L^{INS}_{t}$ | `LedgerFinancialState.insurance` | insurance reserves, bond holdings, equity holdings |
| $L^{FUNDS}_{t}$ | `LedgerFinancialState.funds` | ZUS/NFZ/PPK/FP/PFRON/FGSP/JST cash, fund holdings, NBFI, quasi-fiscal balances |

Ledger rows are the canonical source for supported stock ownership. Agent and
economics modules may receive projections such as `Household.FinancialStocks`,
`Firm.FinancialStocks`, or `Banking.BankFinancialStocks`, but those projections
are execution DTOs, not independent persisted owners.

## Quantity Classes

| Class | Canonical notation | Runtime type | Interpretation |
| --- | --- | --- | --- |
| Monetary stock | uppercase with `t`, e.g. $D^{H}_{h,t}$ | `PLN` | boundary level owned, owed, or diagnostically persisted |
| Monetary flow | lowercase or named flow with $\tau$, e.g. $\mathrm{tax}_{h,\tau}$ | `PLN` | amount accumulated during one execution month |
| Rate | $r_{\tau}$ or named rate | `Rate` | annual by default; use `.monthly` for monthly application |
| Share | $s_{\tau}$, $q_{\tau}$, $\omega_{\tau}$ | `Share` | bounded ratio, usually `[0, 1]` |
| Multiplier | $\mu_{\tau}$ | `Multiplier` | gross multiplier or index-like factor |
| Price index | $P_{t}$ or named index | `PriceIndex`, `Multiplier` | cumulative price/index level |
| Elasticity or production curvature | $\sigma_{s,t}$ | `Sigma` | sector or production-function parameter |
| Count or index | $N_{t}$, `h`, `f`, `b`, `s` | `Int`, typed ids | population count or identifier |
| Stochastic draw | $\epsilon^{k}_{\tau}$ | `RandomStream` draw from `MonthRandomness.StreamKey` | seeded random variation from named stream `k` |

The Scala fixed-point type layer prevents accidental cross-type arithmetic:
`PLN`, `Rate`, `Share`, `Multiplier`, `Coefficient`, `PriceIndex`, `Sigma`,
`ExchangeRate`, and ids are distinct runtime domains.

## Major Flow Families

Monthly flow notation should follow the economic channel, not the runtime file
name. Detailed equations live in `behavioral-equations-and-decision-rules.md`;
the table below only fixes symbol families for publication work.

| Symbol family | Meaning | Main anchors |
| --- | --- | --- |
| $Y_{\tau}$ | output, GDP proxy, or revenue, with scope specified by subscript | `DemandEconomics`, `FirmEconomics`, `GdpAccounting`, `McTimeseriesSchema` |
| $C^{H}_{\tau}$ | household consumption | `HouseholdIncomeEconomics`, `HouseholdFlows` |
| $I^{F}_{\tau}$ | firm investment and capex | `FirmEconomics`, `FirmFlows` |
| $G_{\tau}$ | government purchases and spending | `FiscalBudget`, `GovBudgetFlows` |
| $X_{\tau}$, $IM_{\tau}$ | exports and imports | `OpenEconEconomics`, `OpenEconFlows` |
| $\mathrm{Wage}_{\tau}$ | wage payments and market wage outcomes | `LaborEconomics`, `LaborMarket`, `HouseholdIncomeEconomics` |
| $T_{\tau}$ | taxes by type: PIT, CIT, VAT, excise, levies | tax flow emitters, `Sfc` identities |
| $TR_{\tau}$ | transfers and benefits | household/public-sector flow emitters |
| $\mathrm{Int}_{\tau}$ | interest by instrument and counterparty | banking, household, NBP, bond, and external flow emitters |
| $\mathrm{Orig}_{\tau}$ | new credit origination | household, firm, mortgage, NBFI credit modules |
| $\mathrm{Prin}_{\tau}$ | scheduled principal repayment | credit and mortgage flow modules |
| $\mathrm{Def}_{\tau}$ | default or write-off branch | household, firm, mortgage, NBFI, bond, and bank loss modules |
| $\mathrm{Iss}_{\tau}$ | bond or equity issuance | corporate-bond, government-bond, quasi-fiscal, and equity modules |
| $\mathrm{Val}_{\tau}$ | revaluation or valuation effect | equity, bond, FX, and SFC semantic projection paths |

When a symbol is ambiguous, add sector and instrument superscripts. For example,
$\mathrm{Def}^{H,cons}_{\tau}$ is household consumer-loan default, while
$\mathrm{Def}^{F,loan}_{\tau}$ is firm bank-loan default.

## Rates, Shares, And Policy Variables

Use rates for interest, inflation, growth, and default-rate surfaces:

| Symbol | Meaning | Typical runtime source |
| --- | --- | --- |
| $r^{NBP}_{\tau}$ | NBP reference rate | `Nbp.State`, `OpenEconEconomics` |
| $r^{loan}_{b,\tau}$ | bank lending rate | `Banking`, `BankingStepRunner` |
| $r^{dep}_{b,\tau}$ | bank deposit rate | `Banking`, household income/financial economics |
| $\pi_{\tau}$ | inflation | `World.inflation`, `PriceEquityEconomics` |
| $u_{\tau}$ | unemployment rate | `World.unemploymentRate`, household aggregates |
| $\mathrm{CAR}_{b,\tau}$ | capital adequacy ratio | banking regulatory metrics |
| $\mathrm{NPL}_{b,\tau}$ | non-performing-loan ratio or stock, with unit specified | banking metrics and diagnostics |
| $\mathrm{LCR}_{b,\tau}$ | liquidity coverage ratio | banking regulatory metrics |
| $\mathrm{NSFR}_{b,\tau}$ | net stable funding ratio | banking regulatory metrics |
| $\mathrm{ccyb}_{\tau}$ | countercyclical capital buffer | macroprudential mechanism |
| $\theta$ | parameter vector | `SimParams` |

Use shares for bounded composition variables such as MPC, import propensity,
foreign ownership, automation ratio, hybrid ratio, tax-shadow share, and
sectoral weights.

## Stochastic Variables

The model is deterministic conditional on:

$$
\begin{aligned}
(X_{t}, RND_{\tau}, \theta)
\end{aligned}
$$

Random variables should be written as:

$$
\begin{aligned}
\epsilon^{k}_{\tau}
\end{aligned}
$$

where `k` names a stream from `MonthRandomness.StreamKey`.

| Stream key | Publication role | Runtime field |
| --- | --- | --- |
| `HouseholdIncomeEconomics` | household income, consumption, labor-transition randomness used in the household-income stage | `stages.householdIncomeEconomics` |
| `FirmEconomics` | firm decision, labor adjustment, pricing, credit and production-stage randomness | `stages.firmEconomics` |
| `HouseholdFinancialEconomics` | household financial-stage randomness | `stages.householdFinancialEconomics` |
| `OpenEconEconomics` | external, commodity, market, and open-economy stage randomness | `stages.openEconEconomics` |
| `BankingEconomics` | banking-stage deposit-mobility randomness and stochastic choices executed inside the banking stage; firm/household approval gates use their invoking stage streams | `stages.bankingEconomics` |
| `FdiMa` | closed-month FDI M&A ownership transition | `closing.fdiMa` |
| `FirmEntry` | closed-month firm-entry transition | `closing.firmEntry` |
| `StartupStaffing` | closed-month startup staffing transition | `closing.startupStaffing` |
| `RegionalMigration` | closed-month regional migration transition | `closing.regionalMigration` |

Initialization randomness is separate and belongs to `InitRandomness.Contract`.
Monthly stochasticity must not be described as ambient global randomness; it is
an explicit seed contract.

## Notation-To-Code Map

| Publication object | Code anchor | Notes |
| --- | --- | --- |
| Full boundary state $X_{t}$ | `FlowSimulation.SimState` | single input to one monthly step |
| Month transition $\Phi_{\tau}$ | `FlowSimulation.step`, `MonthDriver` | returns `StepOutput` and `nextState` |
| Macro/market state $W_{t}$ | `World`, `WorldStateSegments` | excludes supported financial ownership |
| Household behavioral state $a^{H}_{h,t}$ | `Household.State` | projected ledger balances carried separately |
| Firm behavioral state $a^{F}_{f,t}$ | `Firm.State` | `Firm.FinancialStocks` is a projection DTO |
| Bank operational state $a^{B}_{b,t}$ | `Banking.BankState` | includes regulatory capital diagnostic stock |
| Ledger financial state $L_{t}$ | `LedgerFinancialState` | source of truth for supported financial ownership |
| Household aggregate state $A^{H}_{t}$ | `Household.Aggregates` | aggregate diagnostics and market aggregates |
| Runtime monetary flow | `FlowMechanism`, `MonthFlowEmitter`, `RuntimeFlowExecutor` | executed through verified ledger topology |
| SFC semantic validation | `Sfc`, `SfcSemanticProjection` | exact SFC identities and semantic stock-flow terms |
| Randomness contract $RND_{\tau}$ | `MonthRandomness.Contract` | named deterministic seed streams |
| Parameter vector $\theta$ | `SimParams`, scenario deltas | calibrated defaults plus explicit scenario mutations |

## Usage Rules For Future Specification Tickets

1. Use $X_{t}$ for the full boundary state and $\Phi_{\tau}$ for the one-month
   transition.
2. Use $L_{t}$ only for ledger-owned financial balances.
3. Do not put bank regulatory capital into holder-resolved ledger equity.
4. Distinguish stocks from flows by time indexing and wording.
5. State whether a monetary variable is a boundary stock, monthly flow,
   diagnostic stock, valuation effect, or known unsupported gap.
6. Link detailed equations to the behavioral-equations document instead of
   duplicating long rule prose.
7. Link accounting claims to SFC matrix evidence and engine invariants.
8. Use $\epsilon^{k}_{\tau}$ only when the source stream `k` is identified.

## Known Scope Boundaries

- This document does not define new equations.
- This document does not settle calibration governance; that is tracked by
  the [Calibration Governance milestone](https://github.com/amor-fati-systems/amor-fati/milestone/27).
- This document does not replace generated SFC matrix artifacts.
- This document intentionally mirrors the current implementation. Future model
  changes should update this notation only when the runtime state boundary or
  symbol families change.
