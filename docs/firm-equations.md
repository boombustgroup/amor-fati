# Firm Equations

This document consolidates the implemented firm-sector model into a
publication-facing equation section. It uses the canonical notation in
[model-notation-and-state-vector.md](model-notation-and-state-vector.md) and
the monthly timing contract in
[monthly-transition-function.md](monthly-transition-function.md).

It describes current executable behavior. It does not introduce new model
behavior and does not replace the implementation anchors listed below.

## Source Anchors

| Source | Role |
| --- | --- |
| [Model specification](model-specification.md#reviewer-reading-path) | Canonical reviewer path and model overview. |
| [Model notation and state vector](model-notation-and-state-vector.md#firms) | Firm state, ledger-owned firm balances, and major symbol families. |
| [Monthly transition function](monthly-transition-function.md) | $X_{t} \to X_{\tau}$ timing, randomness, flow emission, SFC validation, and next-pre boundary. |
| [Behavioral equations and decision rules](behavioral-equations-and-decision-rules.md#firm-rules) | Detailed implementation-oriented rule catalog and output-column map. |
| [SFC matrix evidence](sfc-matrix-evidence.md) | Accounting matrix evidence and generated runtime mapping artifacts. |
| [Engine invariants and semantics](engine-invariants-and-semantics.md) | Validation ownership and hard-fail semantics. |
| [`agents/Firm.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Firm.scala) and [`agents/firm/*`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/firm) | Per-firm behavioral state, decisions, production, P&L, labor, technology, post-processing, and traces. |
| [`engine/economics/FirmEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/FirmEconomics.scala) and [`engine/economics/firm/*`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/firm) | Same-month firm-sector runner, lending surface, financing split, market stages, output assembly, and downstream monthly calculus boundary. |
| [`engine/mechanisms/FirmEntry.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/mechanisms/FirmEntry.scala) | Endogenous replacement and net firm entry. |
| [`montecarlo/timeseries/McTimeseriesSchema.scala`](../modules/montecarlo/src/main/scala/com/boombustgroup/amorfati/montecarlo/timeseries/McTimeseriesSchema.scala) | Public numeric output columns for firm behavior and diagnostics. |
| [`diagnostics/LoanOriginationQualityExport.scala`](../modules/cli/src/main/scala/com/boombustgroup/amorfati/diagnostics/LoanOriginationQualityExport.scala), [`diagnostics/ScenarioRunExport.scala`](../modules/cli/src/main/scala/com/boombustgroup/amorfati/diagnostics/ScenarioRunExport.scala), and [`diagnostics/SensitivityRobustnessExport.scala`](../modules/cli/src/main/scala/com/boombustgroup/amorfati/diagnostics/SensitivityRobustnessExport.scala) | Borrower-level underwriting outcomes, scenario diagnostics, and robustness surfaces that expose firm-credit, investment, entry, and default behavior. |

## State

For firm `f` in sector $s = s(f)$, the behavioral state is:

$$
\begin{aligned}
a^{F}_{f,t} &=  \\
&\quad (tech_{f,t}, risk_{f}, innovCost_{f}, DR_{f,t}, s_{f}, N_{f}, \\
&\quad bank_{f}, K_{f,t}, GK_{f,t}, Inv_{f,t}, markup_{f,t}, \\
&\quad foreign_{f}, stateOwned_{f}, region_{f}, startup_{f}, signal_{f}, \ldots)
\end{aligned}
$$

Ledger-owned firm balances are:

$$
\begin{aligned}
l^{F}_{f,t} &= (Cash^{F}_{f,t}, L^{F}_{f,t}, CB^{F}_{f,t}, Eq^{F}_{f,t})
\end{aligned}
$$

where $Cash^{F}$ is firm liquidity, $L^{F}$ is bank-loan principal, $CB^{F}$
is issued corporate-bond principal, and $Eq^{F}$ is listed-equity financing
recorded in the firm ledger slice.

The technology regime is:

$$
\begin{aligned}
tech_{f} \in \{\mathrm{Traditional}(n), \mathrm{Hybrid}(n, A), \mathrm{Automated}(A), \mathrm{Bankrupt}(reason)\}
\end{aligned}
$$

with effective worker count:

$$
\begin{aligned}
n_{f} &=
\begin{cases}
n, & \text{if } tech_{f} = \mathrm{Traditional}(n), \\
n, & \text{if } tech_{f} = \mathrm{Hybrid}(n, A), \\
\max(autoSkeletonCrew, \mathrm{floor}(phi_{skel} N_{f})), & \text{if } tech_{f} = \mathrm{Automated}(A), \\
0, & \text{if } tech_{f} = \mathrm{Bankrupt}.
\end{cases}
\end{aligned}
$$

Bankrupt firms are inactive until recycled through the entry mechanism.

## Capacity And Production

Let $N_{f}$ be initial firm size and `N_bar` the calibration workers-per-firm
scale. The size scale is:

$$
\begin{aligned}
size_{f} &= N_{f} / N_{bar}
\end{aligned}
$$

The labor/technology efficiency term is:

$$
\begin{aligned}
\ell_{f} &=
\begin{cases}
n_{f} / N_{f}, & \text{if traditional}, \\
omega_{H}^{L} n_{f} / N_{f} + omega_{H}^{A} A_{f}, & \text{if hybrid}, \\
A_{f}, & \text{if automated}, \\
0, & \text{if bankrupt}.
\end{cases}
\end{aligned}
$$

The structural capital target is based on planning workers:

$$
\begin{aligned}
K^{*}_{f,t} &= n^{plan}_{f,t} kappa_{s}
\end{aligned}
$$

where $n^{plan}$ is the current worker count, startup target, or initial size
floor used by the implementation for capital planning.

If physical capital and labor/technology efficiency are positive, firm capacity
is:

$$
\begin{aligned}
C_{f,\tau} &=  \\
&\quad baseRevenue \cdot size_{f} \cdot revMult_{s} \cdot \\
&\quad \mathrm{CES}_{\alpha,\sigma_{s}}(k_{f,t}, \ell_{f,t}) \\
 \\
k_{f,t} &= \mathrm{clamp}(K_{f,t} / K^{*}_{f,t}, 0.1, 2.0)
\end{aligned}
$$

Otherwise:

$$
\begin{aligned}
C_{f,\tau} &= baseRevenue \cdot size_{f} \cdot revMult_{s} \cdot \ell_{f,t}
\end{aligned}
$$

Runtime productivity scales the realized capacity surface:

$$
\begin{aligned}
C^{eff}_{f,\tau} &= prodIndex_{t} \cdot C_{f,\tau}
\end{aligned}
$$

The CES aggregator is Cobb-Douglas at the unit-elasticity boundary:

$$
\begin{aligned}
\mathrm{CES}_{\alpha,\sigma}(k, \ell) &=
\begin{cases}
k^{\alpha} \ell^{1-\alpha}, & \text{if } \sigma \approx 1, \\
\left[\alpha k^{\rho} + (1-\alpha) \ell^{\rho}\right]^{1/\rho}, & \text{otherwise},
\end{cases} \\
\rho &= (\sigma - 1) / \sigma
\end{aligned}
$$

## Revenue And Profit

Let $P_{t}$ be the domestic price level and $D_{s,\tau}$ the sector demand
multiplier used by same-month economics. Firm revenue is:

$$
\begin{aligned}
Y^{F}_{f,\tau} &= P_{t} C^{eff}_{f,\tau} D_{s,\tau}
\end{aligned}
$$

Labor cost is:

$$
\begin{aligned}
WCost_{f,\tau} &= n_{f} wage_{\tau} wageMult_{s}
\end{aligned}
$$

where $wageMult_{s}$ includes the sector wage multiplier and union wage premium.

Monthly interest cost uses bank and corporate-bond debt:

$$
\begin{aligned}
IntCost_{f,\tau} &= (L^{F}_{f,t} + CB^{F}_{f,t}) r^{L}_{\mathrm{bank}(f),\tau} / 12
\end{aligned}
$$

The pre-profit-shifting operating cost stack is:

$$
\begin{aligned}
Cost^{prePS}_{f,\tau} &=  \\
&\quad WCost_{f,\tau} \\
&\quad + OtherCost_{f,\tau} \\
&\quad + DepK_{f,\tau} \\
&\quad + AIOpex_{f,\tau} \\
&\quad + IntCost_{f,\tau} \\
&\quad + InvCarry_{f,\tau} \\
&\quad + EnergyETS_{f,\tau}
\end{aligned}
$$

where:

- `OtherCost` scales with firm size, price level, startup multiplier, and the
  share of costs not already made explicit through capital, energy, or
  inventory channels;
- $DepK = K_{f,t} \delta^K_s / 12$;
- `AIOpex` applies to automated and hybrid firms with domestic/import price
  splits and sublinear size scaling;
- $InvCarry = Inv_{f,t} r^{invCarry} / 12$;
- `EnergyETS` uses sector energy-cost shares, commodity prices, ETS carbon
  surcharge, and a green-capital discount.

Gross profit before FDI profit shifting is:

$$
\begin{aligned}
Pi^{gross}_{f,\tau} &= Y^{F}_{f,\tau} - Cost^{prePS}_{f,\tau}
\end{aligned}
$$

Foreign-owned firms shift a share of positive gross profit:

$$
\begin{aligned}
PS_{f,\tau} &=
\begin{cases}
profitShiftRate \cdot \max(Pi^{gross}_{f,\tau}, 0), & \text{if } foreign_{f}, \\
0, & \text{otherwise}.
\end{cases}
\end{aligned}
$$

Profit before CIT is:

$$
\begin{aligned}
Pi^{preTax}_{f,\tau} &= Pi^{gross}_{f,\tau} - PS_{f,\tau}
\end{aligned}
$$

CIT uses loss carryforward:

$$
\begin{aligned}
Pi^{+}_{f,\tau} &= \max(Pi^{preTax}_{f,\tau}, 0) \\
 \\
Taxable_{f,\tau} &=  \\
&\quad \max(Pi^{+}_{f,\tau} - \min(loss_{f,t}, carryMax \cdot Pi^{+}_{f,\tau}), 0) \\
 \\
CIT_{f,\tau} &= citRate \cdot Taxable_{f,\tau}
\end{aligned}
$$

Losses increase when $Pi^{preTax} < 0$; remaining accumulated losses decay
after offsets.

Post-tax profit is:

$$
\begin{aligned}
Pi^{net}_{f,\tau} &= Pi^{preTax}_{f,\tau} - CIT_{f,\tau}
\end{aligned}
$$

These P&L equations are deterministic conditional on the opening state,
same-month macro inputs, lending rate, and parameters.

## Labor Demand And Workforce Adjustment

Traditional and hybrid firms compute desired workers from a one-period
marginal-revenue comparison. For a hypothetical worker count `n`:

$$
\begin{aligned}
MR_{f}(n) &=  \\
&\quad [C^{eff}_{f}(n) - C^{eff}_{f}(n - 1)] \cdot \\
&\quad P_{t} \cdot \\
&\quad D^{hire}_{s,\tau}
\end{aligned}
$$

The demand signal used for hiring blends realized sector demand with positive
sector hiring pressure:

$$
\begin{aligned}
D^{hire}_{s,\tau} &=  \\
&\quad D_{s,\tau} + \max(HireSignal_{s,\tau} - D_{s,\tau}, 0) \cdot blend
\end{aligned}
$$

Desired workers are the largest `n` such that:

$$
\begin{aligned}
MR_{f}(n) > wage_{\tau} wageMult_{s}
\end{aligned}
$$

The raw target is then compressed by aggregate operational hiring slack:

$$
\begin{aligned}
n^{target}_{f,\tau} &=  \\
&\quad n_{f,t} + slack_{\tau} \cdot \max(n^{raw}_{f,\tau} - n_{f,t}, 0)
\end{aligned}
$$

Hiring is limited by persistence, monthly headroom, liquidity, startup runway,
and profitability. Firing is smoothed by an adjustment share and softened for
state-owned firms:

$$
\begin{aligned}
\Delta n_{f,\tau} &=  \\
\mathrm{stochastic}_{round}(\lambda_{hire} \cdot gap) & \text{if } gap > threshold_{hire} \\
-\mathrm{stochastic}_{round}(\lambda_{fire} \cdot |gap|) & \text{if } gap < -threshold_{fire} \\
0 & \text{otherwise}
\end{aligned}
$$

The residual stochastic rounding draw is part of the firm-economics random
stream and is recorded in firm decision traces.

If negative cash cannot be cured through working-capital grace or downsizing,
the firm enters bankruptcy.

## Pricing, Intermediate Goods, And Inventory

After per-firm decisions and financing are fixed, the firm market stage applies
three deterministic or stochastic market surfaces:

1. intermediate-market payments and cash adjustments using the input-output
   matrix;
2. Calvo markup updates using sector demand pressure, wage growth, and
   energy-cost pressure;
3. labor-market matching, immigration/remigration, wage updates, and firm
   staffing synchronization.

The markup update is a Calvo lottery. With probability $\theta$, firm `f` resets
its markup; otherwise it keeps the previous markup:

$$
\begin{aligned}
\mu_{f,\tau} &=
\begin{cases}
\mu^{*}_{f,\tau}, & U^{price}_{f,\tau} < \theta, \\
\mu_{f,t}, & \text{otherwise}.
\end{cases}
\end{aligned}
$$

The reset markup is:

$$
\begin{aligned}
\mu^{*}_{f,\tau} &=  \\
&\quad \mathrm{clamp}( \\
&\quad baseMarkup \cdot \\
&\quad \left[1 \\
&\quad + \min(\max(DPressure_{s,\tau} - 1, 0) \cdot demandSensitivity, demandCap) \\
&\quad + \min(\max(WageGrowth_{\tau}, 0) \cdot costPassthrough, costCap) \\
&\quad + \min(\max(EnergyPressure_{s,\tau}, 0), energyCap)\right], \\
&\quad minMarkup, \\
&\quad maxMarkup)
\end{aligned}
$$

where `EnergyPressure` depends on the commodity-price index, sector energy-cost
share, and the state-owned energy pass-through modifier. The aggregate markup
inflation contribution is the annualized capacity-weighted average markup
change:

$$
\begin{aligned}
\pi^{markup}_{\tau} &=  \\
&\quad \mathrm{annualize}( \\
&\quad \sum_{f} C^{eff}_{f,\tau}(\mu_{f,\tau} - \mu_{f,t}) \\
&\quad / \sum_{f} C^{eff}_{f,\tau})
\end{aligned}
$$

Inventory is adjusted after the primary decision:

$$
\begin{aligned}
Inv^{postSpoil}_{f,\tau} &= Inv_{f,t} (1 - spoil_{s} / 12) \\
TargetInv_{f,\tau} &= RealizedRevenue_{f,\tau} inventoryTarget_{s} \\
RawDeltaInv_{f,\tau} &=  \\
&\quad (TargetInv_{f,\tau} - Inv^{postSpoil}_{f,\tau}) inventoryAdjustSpeed
\end{aligned}
$$

Positive inventory replenishment is capped by a revenue-based budget; negative
inventory movement cannot draw below zero. If cash is negative, the firm can
liquidate inventory at a discount to raise cash.

## Physical And Green Investment

Physical capital investment occurs after the selected firm decision:

$$
\begin{aligned}
K^{postDep}_{f,\tau} &= K_{f,t} (1 - delta^{K}_{s} / 12) \\
K^{target}_{f,\tau} &=  \\
&\quad n^{plan}_{f,\tau} kappa_{s} \cdot \\
&\quad [1 + demandExpansionSensitivity \cdot pressure^+_{s,\tau} \cdot persistence_{f,t}]
\end{aligned}
$$

Desired gross investment is:

$$
\begin{aligned}
I^{des}_{f,\tau} &=  \\
&\quad DepK_{f,\tau} \\
&\quad + \max(K^{target}_{f,\tau} - K^{postDep}_{f,\tau}, 0) \cdot adjustSpeed \cdot investMult_{f}
\end{aligned}
$$

Pre-credit cash capacity is:

$$
\begin{aligned}
I^{cash,pre}_{f,\tau} &= \min(I^{des}_{f,\tau}, \max(Cash^{F}_{f,t}, 0))
\end{aligned}
$$

Desired investment generates a target bank-credit request before the firm falls
back to internal cash. A separate shortfall leg still lets cash-constrained
firms request credit for unfunded investment:

$$
\begin{aligned}
I^{targetDebt}_{f,\tau} &=  \\
&\quad I^{des}_{f,\tau} \cdot investmentDebtTargetShare \\
 \\
I^{shortfallDebt}_{f,\tau} &=  \\
&\quad \max(I^{des}_{f,\tau} - \max(Cash^{F}_{f,t}, 0), 0) \cdot investmentCreditShare \\
 \\
Demand^{invCredit}_{f,\tau} &=  \\
&\quad \min(\max(I^{targetDebt}_{f,\tau}, I^{shortfallDebt}_{f,\tau}), I^{des}_{f,\tau}) \\
 \\
I^{cash}_{f,\tau} &=  \\
&\quad \min(I^{des}_{f,\tau} - Credit^{approved}_{f,\tau}, \max(Cash^{F}_{f,t}, 0))
\end{aligned}
$$

Approved investment credit increases both cash and firm-loan principal before
being spent on capital. Rejected credit is exposed by reason in the
`FirmCredit_*` diagnostics. If target-debt credit is rejected but the firm has
enough cash, the real investment can still proceed as self-financed CAPEX.

Green investment follows the same target-gap idea using sector green
capital-labor ratios, green depreciation, a green adjustment speed, and a
separate cash budget share.

## Technology Adoption

Traditional firms evaluate full-AI and hybrid candidates; hybrid firms can
evaluate full-AI upgrades. Candidate capex is:

$$
\begin{aligned}
Capex^{AI}_{f,\tau} &=  \\
&\quad aiCapex \cdot aiCapexMult_{s} \cdot \\
&\quad (1 - digiCapexDiscount \cdot DR_{f,t}) \cdot \\
&\quad innovCost_{f} \cdot \\
&\quad size_{f}^{capexExp} \\
 \\
Capex^{Hybrid}_{f,\tau} &=  \\
&\quad hybridCapex \cdot hybridCapexMult_{s} \cdot \\
&\quad (1 - digiCapexDiscount \cdot DR_{f,t}) \cdot \\
&\quad innovCost_{f} \cdot \\
&\quad size_{f}^{capexExp}
\end{aligned}
$$

Each candidate defines:

$$
\begin{aligned}
Loan^{tech}_{f,\tau} &= loanShare_{path} \cdot Capex_{path} \\
Down_{path} &= downShare_{path} \cdot Capex_{path}
\end{aligned}
$$

A candidate is feasible when all of the following hold:

- current cost base exceeds the path cost threshold adjusted by $\sigma_{s}$;
- $Cash^{F}_{f,t} \ge Down_{path}$;
- $DR_{f,t} \ge readinessMin_{path}$;
- the relationship bank approves $Loan_{path}$.

For traditional-firm upgrades, the path cost threshold is divided by the
sectoral substitution threshold $sigmaThreshold(\sigma_{s})$, matching the runtime
use of current sector technology substitutability in upgrade feasibility. Hybrid
to full-AI upgrades use the full-AI margin threshold without that additional
sector-sigma divisor.

Adoption probabilities combine:

- firm risk profile;
- digital readiness;
- local neighbor adoption;
- aggregate automation and hybrid ratios;
- loss-making desperation;
- strategic early-adoption pressure;
- a calendar-time adoption-willingness ramp;
- technology-path feasibility.

For traditional firms:

$$
\begin{aligned}
p^{AI}_{f,\tau} &= \min(1, willingness \cdot rawFullAi) \\
p^{H}_{f,\tau} &= \min(1 - p^{AI}_{f,\tau}, willingness \cdot rawHybrid)
\end{aligned}
$$

A single adoption draw selects full-AI, hybrid, or no upgrade. Successful
upgrades draw technology efficiency. Failed implementations impose partial
capex, debt, and down-payment costs; catastrophic failures bankrupt the firm.

Digital-readiness investment is a separate fallback behavior when no technology
upgrade is selected and the firm has enough cash. It raises `DR` by a bounded
increment and records probability/roll evidence in decision traces.

Technology adoption is behavioral and stochastic. The resulting capex, loans,
imports, defaults, and state transitions become deterministic accounting terms
once the decision has been selected.

## Financing And Credit Approval

Firm credit requests are evaluated against the relationship bank's lending
surface. The decision carries approval probability, roll, and rejection reason
when available. Rejection reasons are exposed as:

```text
failed-bank, CAR gate, management capital-buffer throttle, LCR gate, NSFR gate,
portfolio-preference, unclassified
```

Approved technology and investment loan demand enters the firm financing split.
For a total new-financing amount $NewLoan_{f}$:

$$
\begin{aligned}
Equity_{f} &=
\begin{cases}
equityFrac \cdot NewLoan_{f}, & workers_{f} \ge equityMinSize, \\
0, & \text{otherwise},
\end{cases} \\
 \\
AfterEquity_{f} &= NewLoan_{f} - Equity_{f} \\
 \\
CorpBond_{f} &=
\begin{cases}
bondFrac \cdot AfterEquity_{f}, & workers_{f} \ge corpBondMinSize, \\
0, & \text{otherwise},
\end{cases} \\
 \\
BankLoan_{f} &= AfterEquity_{f} - CorpBond_{f}
\end{aligned}
$$

The technology portion is prorated across equity, corporate bonds, and bank
loans so the diagnostics can distinguish `TechBankLoan` from total financing.

Corporate-bond issuance is then constrained by market absorption. Unsold bond
issuance reverts to bank loans, preserving the global financing amount while
keeping channel diagnostics explicit.

## Default, NPL, And Exit

Per-firm decisions can produce bankruptcy directly when:

- an automated firm falls into an AI debt trap;
- technology implementation fails catastrophically;
- operating cash remains negative after grace and downsizing;
- startup runway is exhausted.

Newly dead firms are those alive at the opening stage and bankrupt after firm
market stages. Bank NPL creation and loss are:

$$
\begin{aligned}
NPL^{new}_{\tau} &= \sum_{\substack{f:\ \text{alive at } t \\
\text{bankrupt after firm stages}}} L^{F}_{f,\tau} \\
NPLLoss_{\tau} &= NPL^{new}_{\tau} \cdot (1 - loanRecovery)
\end{aligned}
$$

Corporate-bond defaults are computed from defaulted issuer debt and cleared
from the holder-resolved corporate-bond ownership layer.

Bank and bond losses are accounting consequences of the selected firm states.
The stochastic part has already occurred in technology, labor-adjustment,
credit, and entry decisions.

## Entry And Replacement

Firm entry runs in the closed-month lifecycle boundary. It has two channels:

1. replacement entry: recycle a bounded share of bankrupt firm slots;
2. net entry: append new firms when demand, inflation, hiring slack, and
   startup absorption support expansion.

Sector entry weights multiply the sector target share, the correction for
over/under-representation, demand pressure and utilization, the profitability
signal, and the sector entry-barrier multiplier.

The profitability signal is based on sector-average cash relative to the
living-firm average and is clamped to a bounded range.

New entrants draw size, startup workforce target, region, bank relationship,
risk profile, innovation cost, digital readiness, neighbors, foreign ownership,
capital, inventory, and green capital. AI-native entrants become possible when
aggregate technology adoption exceeds the entry threshold and the AI-entry draw
succeeds.

## FDI, Informality, And Ownership-Sensitive Flows

Foreign-owned firms have two channels:

$$
\begin{aligned}
ProfitShift_{f,\tau} &= profitShiftRate \cdot \max(Pi^{gross}_{f,\tau}, 0) \\
Repatriation_{f,\tau} &= \min(repatriationRate \cdot \max(Pi^{postTax}_{f,\tau}, 0), Cash^{available}_{f,\tau})
\end{aligned}
$$

Informal-economy CIT evasion is:

$$
\begin{aligned}
CITEvasion_{f,\tau} &=  \\
&\quad CIT_{f,\tau} \cdot \\
&\quad \mathrm{clamp}(shadowShare_{s} + cyclicalInformalAdjustment_{t}, 0, 1) \cdot \\
&\quad citEvasionRate
\end{aligned}
$$

These flows affect cash, tax payments, and external-sector/public-sector
evidence, but they do not create separate firm behavioral types.

## Accounting And SFC Mapping

Firm behavior is translated into ledger and SFC evidence through the monthly
flow layer:

| Firm surface | Main flow/SFC interpretation |
| --- | --- |
| wages and household income effects | household income and labor-market channels |
| CIT and CIT evasion | public-sector revenue and informal-economy tax adjustment |
| firm bank loans and principal | firm-credit stock movement and bank balance-sheet effects |
| firm NPL/default | bank loan-loss and firm-credit SFC diagnostics |
| corporate-bond issuance, coupon, amortization, default | holder-resolved corporate-bond runtime contracts |
| equity issuance and dividends | listed-equity and dividend/tax channels |
| investment and capex | firm cash, investment diagnostics, import split for technology capex |
| FDI profit shifting and repatriation | external income and foreign-sector flow channels |
| inventory and green capital | firm-state diagnostics and macro output surfaces |

The exact accounting contract is validated by runtime ledger conservation and
SFC semantic identities. Unsupported or diagnostic-only stock interpretations
must stay explicit in SFC matrix artifacts.

## Output And Diagnostic Surfaces

Representative output columns include:

| Surface | Columns |
| --- | --- |
| Production and investment | `GrossInvestment`, `PrivateGrossInvestmentToGdp`, `AggCapitalStock`, `AggInventoryStock`, `InventoryChange`, `InventoryToGdp`, `AggEnergyCost`, `GreenInvestment` |
| Technology adoption | `TotalAdoption`, `AutoRatio`, `HybridRatio`, `Automation_TechCapex`, `Automation_TechImports`, `Automation_TechLoans`, `Automation_UpgradeFailures`, `Automation_AiDebtTrap`, `Automation_NewFullAi`, `Automation_NewHybrid` |
| Firm credit | `FirmCredit_NewLoans`, `FirmCredit_PrincipalRepaid`, `FirmCredit_GrossDefault`, `FirmCredit_NplRecovery`, `FirmCredit_NplLoss`, `FirmCredit_NetStockFlow`, `FirmCredit_CreditDemand`, `FirmCredit_CreditApproved`, `FirmCredit_BankRejected`, `FirmCredit_ApprovalRate` |
| Credit rejection reasons | `FirmCredit_RejectedFailedBank`, `FirmCredit_RejectedCarGate`, `FirmCredit_RejectedCapitalBuffer`, `FirmCredit_RejectedLcrGate`, `FirmCredit_RejectedNsfrGate`, `FirmCredit_RejectedPortfolioPreference`, `FirmCredit_RejectedUnclassified`, `FirmCredit_RejectedPortfolioPreferenceToDemand`, `FirmCredit_RejectedPortfolioPreferenceToBankRejected` |
| Investment and technology credit | `FirmCredit_InvestmentDemand`, `FirmCredit_InvestmentApproved`, `FirmCredit_InvestmentBankRejected`, `FirmCredit_CashFinancedInvestment`, `FirmCredit_TechDemand`, `FirmCredit_TechApproved`, `FirmCredit_TechBankRejected`, selected/candidate tech-credit columns |
| Bonds and equity | `EquityIssuanceTotal`, `CorpBondOutstanding`, `CorpBondYield`, `CorpBondIssuance`, `CorpBondSpread`, `CorpBondAbsorptionRate`, holder corporate-bond columns |
| Entry and exit | `FirmBirths`, `FirmDeaths`, `NetEntry`, `LivingFirmCount`, `NetFirmBirths` |

In the credit rejection surface, `FirmCredit_RejectedPortfolioPreference` is the
absolute `portfolioPreferenceRejections` flow. The derived columns are
proportions: `FirmCredit_RejectedPortfolioPreferenceToDemand` is
`flowToFlowRatio(portfolioPreferenceRejections, firmCreditDemand)`, and
`FirmCredit_RejectedPortfolioPreferenceToBankRejected` is
`flowToFlowRatio(portfolioPreferenceRejections, firmCreditRejected)`. Both are
ratio/percentage-of-flow diagnostics, not PLN amounts.

Per-firm decision traces expose opening/closing technology, decision type,
cash/debt before and after, P&L, capex, credit decisions, bank approval
probabilities and rolls, portfolio-choice wedge components, technology
feasibility, adoption roll, implementation roll, efficiency draw,
investment-credit audit, digital-readiness investment, and labor-adjustment
residual rolls.

Loan-origination quality diagnostics link firm credit observations to later
bankruptcy outcomes over an outcome window. Scenario and sensitivity diagnostics
then aggregate firm deaths, credit, investment, and macro feedback for
publication-facing validation runs.

## Validation Coverage

| Layer | Coverage |
| --- | --- |
| Unit/property tests | `FirmSpec`, `FirmPropertySpec`, `SmoothLaborAdjustmentSpec`, `CesProductionSpec`, firm-size distribution, physical-capital, firm-entry, FDI composition, informal-economy, firm economics, and firm flow tests. |
| Schema/diagnostic tests | Firm decision trace selection/schema, firm snapshot schema/schedule, Monte Carlo schema, and loan-origination quality diagnostics. |
| Integration/nightly | Monte Carlo TSV integration, nightly diagnostics, loan-origination quality, sensitivity/robustness, scenario runs, SFC matrix evidence, and generated-output guard. |
| Accounting | Runtime ledger execution, firm-credit stock-flow, corporate-bond ownership, equity issuance, tax, external, and SFC semantic projection tests. |

## Current Limitations

- Firm equations are executable and auditable, but several empirical parameters
  still rely on calibration-register provenance rather than final publication
  estimates.
- Inventory, green capital, and informal-economy behavior are stylized model
  mechanisms with explicit diagnostics; they should be interpreted as current
  implemented channels, not settled empirical microfoundations.
- Corporate-bond and equity financing are channel splits and holder-aware
  accounting surfaces, not a full firm-level securities market with endogenous
  investor pricing at issuer resolution.
- Technology adoption is intentionally behavioral and stochastic. The model
  exposes feasibility, probabilities, and rolls, but the adoption law remains a
  calibration target for future empirical work.
- Entry and AI-native startup rules are current executable mechanisms. Their
  sector weights and AI-native threshold should be revisited when richer Polish
  firm-entry microdata are incorporated.
