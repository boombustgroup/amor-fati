# Household Equations

This document consolidates the implemented household-sector model into a
publication-facing equation section. It uses the canonical notation in
[model-notation-and-state-vector.md](model-notation-and-state-vector.md) and
the monthly timing contract in
[monthly-transition-function.md](monthly-transition-function.md).

It describes current executable behavior. It does not introduce new household
rules and does not replace the implementation anchors listed below.

## Source Anchors

| Source | Role |
| --- | --- |
| [Model specification](model-specification.md#reviewer-reading-path) | Canonical reviewer path and model overview. |
| [Model notation and state vector](model-notation-and-state-vector.md#households) | Household behavioral state, ledger-owned household balances, and major symbol families. |
| [Monthly transition function](monthly-transition-function.md) | $X_{t} \to X_{\tau}$ timing, randomness, flow emission, SFC validation, and next-pre boundary. |
| [Behavioral equations and decision rules](behavioral-equations-and-decision-rules.md#household-rules) | Detailed implementation-oriented household rule catalog and output-column map. |
| [SFC matrix evidence](sfc-matrix-evidence.md) | Accounting matrix evidence and generated runtime mapping artifacts. |
| [Engine invariants and semantics](engine-invariants-and-semantics.md) | Validation ownership and hard-fail semantics. |
| [`agents/Household.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Household.scala) and [`agents/household/*`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/household) | Household state, monthly budget pipeline, credit underwriting, liquidity waterfall, distress machine, labor transitions, and aggregate computation. |
| [`engine/economics/HouseholdIncomeEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/HouseholdIncomeEconomics.scala) and [`engine/economics/HouseholdFinancialEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/HouseholdFinancialEconomics.scala) | Same-month household income, consumption/import split, bank-rate surface, product-aware bank-credit supply, remittances, tourism services, and household financial aggregates. |
| [`engine/flows/HouseholdFlows.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/flows/HouseholdFlows.scala) and [`engine/flows/MortgageFlows.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/flows/MortgageFlows.scala) | Runtime household cash/credit/mortgage flow emission and SFC settlement shells. |
| [`montecarlo/timeseries/McTimeseriesSchema.scala`](../modules/montecarlo/src/main/scala/com/boombustgroup/amorfati/montecarlo/timeseries/McTimeseriesSchema.scala), [`montecarlo/snapshots/McHouseholdSnapshotSchema.scala`](../modules/montecarlo/src/main/scala/com/boombustgroup/amorfati/montecarlo/snapshots/McHouseholdSnapshotSchema.scala), and [`diagnostics/LoanOriginationQualityExport.scala`](../modules/cli/src/main/scala/com/boombustgroup/amorfati/diagnostics/LoanOriginationQualityExport.scala) | Public household time-series columns, optional household micro snapshots, and borrower-level credit-outcome diagnostics. |
| [`diagnostics/HhBankLeadLagDiagnosticsExport.scala`](../modules/cli/src/main/scala/com/boombustgroup/amorfati/diagnostics/HhBankLeadLagDiagnosticsExport.scala) and [`diagnostics/HouseholdCreditStressCalibrationExport.scala`](../modules/cli/src/main/scala/com/boombustgroup/amorfati/diagnostics/HouseholdCreditStressCalibrationExport.scala) | Household-bank lead/lag evidence and household credit-stress calibration surfaces. |

## State

For household `h`, the behavioral state is:

$$
\begin{aligned}
a^{H}_{h,t} &=  \\
&\quad (status_{h,t}, rent_{h}, skill_{h,t}, healthPenalty_{h,t}, mpc_{h,t}, \\
&\quad neighbors_{h}, bank_{h}, lastSector_{h}, immigrant_{h}, children_{h}, \\
&\quad education_{h}, routineness_{h}, wageScar_{h,t}, contract_{h}, region_{h}, \\
&\quad distressMonths_{h,t}, distressState_{h,t})
\end{aligned}
$$

Ledger-owned household balances are:

$$
\begin{aligned}
l^{H}_{h,t} &=  \\
&\quad (D^{H}_{h,t}, M^{H}_{h,t}, CL^{H}_{h,t}, E^{H}_{h,t}, \\
&\quad RemMtgMonths_{h,t})
\end{aligned}
$$

where $D^{H}$ is the household demand-deposit asset, $M^{H}$ is mortgage
principal, $CL^{H}$ is unsecured consumer-loan principal, $E^{H}$ is listed-equity
wealth, and `RemMtgMonths` is the remaining contractual mortgage maturity.

The activity status is:

$$
\begin{aligned}
status_{h} \in \{ \\
&\quad \mathrm{Employed}(firm, sector, wage), \\
&\quad \mathrm{Unemployed}(months), \\
&\quad \mathrm{Retraining}(monthsLeft, targetSector, cost), \\
\mathrm{Bankrupt} \\
\}
\end{aligned}
$$

The financial-distress lifecycle is separate from activity status:

$$
\begin{aligned}
distressState_{h} \in \{ \\
\mathrm{Current}, \mathrm{LiquidityStress}, \mathrm{Arrears}, \\
\mathrm{Restructuring}, \mathrm{Defaulted}, \mathrm{Bankruptcy} \\
\}
\end{aligned}
$$

The legacy activity status `Bankrupt` is an absorbing inactive labor/activity
state. The financial-distress `Bankruptcy` state is personal insolvency and
write-off evidence; it does not by itself remove the household from the labor
force.

## Monthly Household Boundary

The household batch step follows a fixed boundary:

$$
\begin{aligned}
\text{opening rows} \\
{} \to \text{validate aligned household and financial-stock rows} \\
{} \to \text{compute each household's monthly flows} \\
{} \to \text{aggregate exact public totals and per-bank flows} \\
{} \to \text{validate monthly diagnostics against aggregate totals} \\
{} \to \text{close updated household states, financial stocks, and monthly rows}
\end{aligned}
$$

Opening demand deposits must be non-negative. Any current-month deficit is
routed through explicit liquidity-shortfall financing and charge-off
diagnostics rather than persisted as a negative deposit.

## Income, PIT, And Transfers

The monthly labor-income base is:

$$
\begin{aligned}
BaseInc_{h,\tau} &=
\begin{cases}
wage_{h}, & \text{if Employed}, \\
\mathrm{UnempBenefit}(monthsUnemployed_{h}), & \text{if Unemployed}, \\
0, & \text{if Retraining or Bankrupt}.
\end{cases}
\end{aligned}
$$

Unemployed households increment their unemployment spell after benefit
resolution. Retraining and bankrupt activity states do not receive base income
inside the household step.

Bank-specific deposit interest is:

$$
\begin{aligned}
DepInt_{h,\tau} &=
\begin{cases}
D^{H}_{h,t} r^{D}_{\mathrm{bank}(h),\tau} / 12, & \text{if bank deposit rates are available}, \\
0, & \text{otherwise}.
\end{cases}
\end{aligned}
$$

Gross monthly income is:

$$
\begin{aligned}
GrossInc_{h,\tau} &= BaseInc_{h,\tau} + \max(DepInt_{h,\tau}, 0)
\end{aligned}
$$

PIT is annualized after employee ZUS:

$$
\begin{aligned}
PitBase_{h,\tau} &= GrossInc_{h,\tau} (1 - zusEmployeeRate) \\
AnnualPitBase_{h,\tau} &= 12 PitBase_{h,\tau} \\
PIT_{h,\tau} &=  \\
&\quad \max(\mathrm{ProgressiveTax}(AnnualPitBase_{h,\tau}) - annualTaxCredit, 0) / 12
\end{aligned}
$$

Dependent-child transfers are:

$$
\begin{aligned}
Transfer_{h,\tau} &= children_{h} \cdot social800 \\
Inc_{h,\tau} &= GrossInc_{h,\tau} - PIT_{h,\tau} + Transfer_{h,\tau}
\end{aligned}
$$

Aggregate pension income, when supplied by the monthly calculus, is added at
the household aggregate layer and consumed with the configured household MPC.

These income, PIT, and transfer equations are deterministic conditional on the
opening household state, bank-rate surface, and parameters.

## Mortgage Service

Household-side mortgage service uses the active contractual maturity:

$$
\begin{aligned}
RemMtgMonths^{*}_{h,t} &=
\begin{cases}
0, & M^{H}_{h,t} \le 0, \\
RemMtgMonths_{h,t}, & M^{H}_{h,t} > 0 \land RemMtgMonths_{h,t} > 0, \\
mortgageMaturity, & M^{H}_{h,t} > 0 \land RemMtgMonths_{h,t} = 0.
\end{cases}
\end{aligned}
$$

Scheduled principal and interest are:

$$
\begin{aligned}
MtgPrin_{h,\tau} &=  \\
M^{H}_{h,t} / RemMtgMonths^{*}_{h,t} & \text{if } RemMtgMonths^{*}_{h,t} > 0 \\
0 & \text{otherwise} \\
 \\
r^{M}_{h,\tau} &=  \\
r^{L}_{\mathrm{bank}(h),\tau} & \text{if bank lending rates are available} \\
r^{\mathrm{NBP}}_{\tau} + mortgageSpread & \text{otherwise} \\
 \\
MtgInt_{h,\tau} &= M^{H}_{h,t} \max(r^{M}_{h,\tau}, 0) / 12 \\
MtgService_{h,\tau} &= MtgPrin_{h,\tau} + MtgInt_{h,\tau}
\end{aligned}
$$

The closing household mortgage stock after scheduled service is:

$$
\begin{aligned}
M^{H}_{h,\tau} &= \max(M^{H}_{h,t} - MtgPrin_{h,\tau}, 0)
\end{aligned}
$$

If mortgage debt remains, contractual maturity decreases by one month but not
below one. If the mortgage is fully repaid, remaining months become zero.

Mortgage principal and interest have separate accounting semantics: principal
reduces the mortgage stock through the mortgage principal settlement shell,
while interest is a household-to-bank cash flow. Aggregate mortgage
origination/default is handled by the housing and banking stages, not by a
household micro origination rule in this step.

## Consumer Credit

Consumer-loan service uses the household's bank lending rate plus the consumer
credit spread:

$$
\begin{aligned}
r^{CC}_{h,\tau} &=  \\
r^{L}_{\mathrm{bank}(h),\tau} + ccSpread & \text{if bank lending rates are available} \\
r^{\mathrm{NBP}}_{\tau} + ccSpread & \text{otherwise} \\
 \\
CCPrin_{h,\tau} &= CL^{H}_{h,t} ccAmortRate \\
CCInt_{h,\tau} &= CL^{H}_{h,t} r^{CC}_{h,\tau} / 12 \\
CCService_{h,\tau} &= CCPrin_{h,\tau} + CCInt_{h,\tau}
\end{aligned}
$$

Normal underwritten consumer credit is available only to employed households.
The household-side eligibility rule is:

$$
\begin{aligned}
Stressed_{h} &=  \\
&\quad DisposablePreCredit_{h,\tau} < wage_{h} \cdot DisposableWageThreshold \\
 \\
Eligible_{h} &=  \\
&\quad Employed_{h} \land Stressed_{h} \\
&\quad \land \neg \mathrm{BlockedByDistress}(distressState_{h,t}) \\
&\quad \land U^{ccElig}_{h,\tau} < ccEligRate
\end{aligned}
$$

The distress states `Arrears`, `Restructuring`, `Defaulted`, and `Bankruptcy`
block new underwritten consumer-credit approval before the bank gate.
`Current` and `LiquidityStress` do not block it.

The DTI-based principal capacity is:

$$
\begin{aligned}
ExistingDti_{h} &=  \\
&\quad (MtgService_{h,\tau} + CCService_{h,\tau}) / Inc_{h,\tau} \\
 \\
PaymentHeadroom_{h} &=  \\
&\quad Inc_{h,\tau} \cdot \max(ccMaxDti - ExistingDti_{h}, 0) \\
 \\
PaymentFactor_{h} &= ccAmortRate + r^{CC}_{h,\tau} / 12 \\
 \\
Capacity^{CC}_{h,\tau} &=  \\
&\quad \min(PaymentHeadroom_{h} / PaymentFactor_{h}, ccMaxLoan)
\end{aligned}
$$

The desired underwritten principal is:

$$
\begin{aligned}
StressGap_{h} &=  \\
&\quad \max(wage_{h} \cdot DisposableWageThreshold - DisposablePreCredit_{h,\tau}, 0) \\
 \\
EssentialGap_{h} &=  \\
&\quad \max(basicConsumptionFloor + CCService_{h,\tau} - DisposablePreCredit_{h,\tau}, 0) \\
 \\
Demand^{CC}_{h,\tau} &=  \\
&\quad \min(\max(StressGap_{h}, EssentialGap_{h}), Capacity^{CC}_{h,\tau})
\end{aligned}
$$

Small requests below the configured minimum consumer-loan size are set to zero.
Positive eligible demand is then passed through the product-aware bank-side
supply gate with `ConsumerLoan` as the requested product. That gate uses the same
audited bank credit-approval surface used for firm credit: failed-bank,
projected CAR with the consumer-loan RWA bucket incremented, LCR, NSFR, and
bank-side approval audit after those hard gates. Household diagnostics distinguish
total demand, rejected demand, the subset rejected by bank supply, and the
bank-side approval audit fields for household snapshot rows.

Ordinary closing consumer debt before residual liquidity settlement is:

$$
\begin{aligned}
CL^{ordinary}_{h,\tau} &=  \\
&\quad \max(CL^{H}_{h,t} + ApprovedCC_{h,\tau} - CCPrin_{h,\tau}, 0)
\end{aligned}
$$

## Consumption And Savings

The first non-discretionary budget layer is:

$$
\begin{aligned}
Obligations_{h,\tau} &=  \\
&\quad rent_{h} + MtgService_{h,\tau} + Remit_{h,\tau} \\
 \\
DisposablePreCredit_{h,\tau} &=  \\
&\quad \max(Inc_{h,\tau} - Obligations_{h,\tau}, 0) \\
 \\
FullObligations_{h,\tau} &=  \\
&\quad Obligations_{h,\tau} + CCService_{h,\tau} \\
 \\
Disposable_{h,\tau} &=  \\
&\quad \max(Inc_{h,\tau} - FullObligations_{h,\tau}, 0)
\end{aligned}
$$

Savings-buffer drawdown depends on activity status:

$$
\begin{aligned}
TargetSavings_{h} &= \max(Inc_{h,\tau}, baseReservationWage) \cdot bufferTargetMonths \\
ProtectedBuffer_{h} &= TargetSavings_{h} \cdot bufferProtectedShare \\
 \\
Drawdown_{h} &=
\begin{cases}
\max(D^{H}_{h,t} - ProtectedBuffer_{h}, 0) \cdot bufferStressDrawdownRate, & \text{if Unemployed or Retraining}, \\
\max(D^{H}_{h,t} - TargetSavings_{h}, 0) \cdot bufferExcessDrawdownRate, & \text{if Employed or Bankrupt}.
\end{cases}
\end{aligned}
$$

The pre-waterfall consumption budget is:

$$
\begin{aligned}
ConsBudget_{h,\tau} &=  \\
&\quad Disposable_{h,\tau} + ApprovedCC_{h,\tau} + Drawdown_{h} \\
 \\
DesiredCons^{raw}_{h,\tau} &= mpc_{h,t} ConsBudget_{h,\tau}
\end{aligned}
$$

Social-neighbor distress and the household financial-distress state then apply
precautionary compression. Positive listed-equity revaluation and the aggregate
housing wealth effect add to desired consumption:

$$
\begin{aligned}
E^{H}_{h,\tau} &= \max(E^{H}_{h,t} (1 + r^{E}_{\tau}), 0) \\
EquityBoost_{h} &= \max(E^{H}_{h,\tau} - E^{H}_{h,t}, 0) \cdot equityWealthEffectMpc \\
HousingBoost_{h} &= HousingWealthEffect_{\tau} / population_{\tau} \\
DesiredCons_{h,\tau} &= DistressAdjustedCons_{h} + EquityBoost_{h} + HousingBoost_{h}
\end{aligned}
$$

The consumption waterfall pays basic consumption first, then obligations, then
affordable discretionary consumption:

$$
\begin{aligned}
BasicNeed_{h} &= \min(DesiredCons_{h,\tau}, basicConsumptionFloor) \\
AvailableCash_{h} &= \max(D^{H}_{h,t} + Inc_{h,\tau} + ApprovedCC_{h,\tau}, 0) \\
 \\
PaidBasic_{h} &= \min(BasicNeed_{h}, AvailableCash_{h}) \\
UnmetBasic_{h} &= BasicNeed_{h} - PaidBasic_{h} \\
 \\
AvailableAfterBills_{h} &=  \\
&\quad \max(AvailableCash_{h} - PaidBasic_{h} - \max(FullObligations_{h,\tau}, 0), 0) \\
 \\
PaidDiscretionary_{h} &=  \\
&\quad \min(\max(DesiredCons_{h,\tau} - BasicNeed_{h}, 0), AvailableAfterBills_{h}) \\
 \\
Cons_{h,\tau} &= PaidBasic_{h} + PaidDiscretionary_{h}
\end{aligned}
$$

Unmet basic consumption and discretionary compression are diagnostics, not
credit origination. They are recorded before any residual liquidity bridge is
settled.

The raw liquid-balance signal is:

$$
\begin{aligned}
RawD_{h,\tau} &=  \\
&\quad D^{H}_{h,t} + Inc_{h,\tau} - FullObligations_{h,\tau} \\
&\quad + ApprovedCC_{h,\tau} - Cons_{h,\tau}
\end{aligned}
$$

Retraining cost, if paid in the current month, is subtracted before final
liquidity settlement.

## Liquidity Shortfall Settlement

Before booking a bridge, any remaining negative raw liquid balance is offered
to the same underwritten consumer-credit path and product-aware bank supply up
to unused DTI capacity:

$$
\begin{aligned}
ResidualShortfall_{h} &= \max(-RawD_{h,\tau}, 0) \\
UnusedCapacity_{h} &= \max(Capacity^{CC}_{h,\tau} - ApprovedCC_{h,\tau}, 0) \\
ResidualDemand_{h} &= \min(ResidualShortfall_{h}, UnusedCapacity_{h})
\end{aligned}
$$

Approved residual demand increases `ApprovedCC` and the raw liquid balance. If
the final raw balance remains negative, it is settled as explicit same-month
liquidity-shortfall financing:

$$
\begin{aligned}
Shortfall_{h} &= \max(-RawD^{final}_{h,\tau}, 0) \\
D^{H}_{h,\tau} &= \max(RawD^{final}_{h,\tau}, 0)
\end{aligned}
$$

The shortfall is attributed in cash-priority order to:

```text
consumption shortfall
rent arrears
mortgage arrears
consumer-debt arrears
temporary overdraft
```

These components must sum exactly to
`HouseholdLiquidity_ShortfallFinancing`. The same amount is also
`LiquidityBridgeChargeOff`: it prevents negative demand deposits and is charged
off in the same month rather than becoming ordinary consumer debt.

The public consumer-credit decomposition is:

$$
\begin{aligned}
ConsumerOrigination &=  \\
&\quad ConsumerApprovedOrigination \\
 \\
ConsumerDefault &=  \\
&\quad ConsumerLoanDefault \\
 \\
ConsumerLoanDefault &=  \\
&\quad OrdinaryConsumerLoanDefault + ConsumerInsolvencyDefault \\
 \\
ConsumerCredit_{NetStockFlow} &=  \\
&\quad ConsumerOrigination - ConsumerPrincipal - ConsumerDefault \\
 \\
ConsumerCredit_{UnderwrittenNetFlow} &=  \\
&\quad ConsumerApprovedOrigination - ConsumerPrincipal - ConsumerLoanDefault \\
 \\
ConsumerCredit_{BridgeNetFlow} &=  \\
&\quad HouseholdLiquidity_{ShortfallFinancing} - LiquidityBridgeChargeOff
\end{aligned}
$$

This split is central to the accounting semantics: underwritten consumer credit
and residual liquidity settlement are both SFC-routed consumer-credit mechanisms,
but they are diagnostically separate. `LiquidityBridgeChargeOff` is never part
of ordinary `ConsumerLoanDefault`; personal-insolvency default is exposed as the
`ConsumerInsolvencyDefault` subset of `ConsumerLoanDefault`.

## MPC Adaptation

After survival resolution, the marginal propensity to consume adapts through a
buffer-stock rule:

$$
\begin{aligned}
TargetSavings_{h} &= Inc_{h,\tau} \cdot bufferTargetMonths \\
BufferRatio_{h} &= D^{H}_{h,t} / TargetSavings_{h} \\
Deviation_{h} &= BufferRatio_{h} - 1 \\
 \\
BufferAdj_{h} &=  \\
&\quad \mathrm{clamp}(1 - bufferSensitivity \cdot Deviation_{h}, 0, 1) \\
 \\
UnemployedAdj_{h} &=  \\
1 + mpcUnemployedBoost & \text{if Unemployed} \\
1 & \text{otherwise} \\
 \\
mpc_{h,\tau} &=  \\
&\quad \mathrm{clamp}(mpc_{h,t} \cdot BufferAdj_{h} \cdot UnemployedAdj_{h}, MpcFloor, MpcCeiling)
\end{aligned}
$$

If current income is non-positive, MPC is left unchanged.

## Financial Distress, Default, And Personal Insolvency

The monthly financial-distress trigger is:

$$
\begin{aligned}
EssentialOutflows_{h} &=  \\
&\quad \max(rent_{h} + MtgService_{h,\tau} + CCService_{h,\tau}, rent_{h}) \\
 \\
DistressFloor_{h} &= EssentialOutflows_{h} \cdot bankruptcyThreshold \\
 \\
Triggered_{h} &=  \\
&\quad RawD_{h,\tau} < 0 \\
&\quad \lor UnmetBasic_{h} > 0 \\
&\quad \lor RawD_{h,\tau} < DistressFloor_{h}
\end{aligned}
$$

Consecutive distress months evolve as:

$$
\begin{aligned}
distressMonths_{h,\tau} &=
\begin{cases}
distressMonths_{h,t} + 1, & Triggered_{h}, \\
0, & \text{otherwise}.
\end{cases}
\end{aligned}
$$

The distress state transition is threshold-driven by the updated consecutive
distress spell:

$$
\begin{aligned}
distressState_{h,\tau} &=
\begin{cases}
\mathrm{Restructuring}, & distressMonths_{h,\tau} = 0 \text{ and } distressState_{h,t} \in \{\mathrm{Arrears}, \mathrm{Defaulted}, \mathrm{Bankruptcy}\}, \\
\mathrm{Current}, & distressMonths_{h,\tau} = 0 \text{ and } distressState_{h,t} \in \{\mathrm{Current}, \mathrm{LiquidityStress}, \mathrm{Restructuring}\}, \\
\mathrm{Bankruptcy}, & \text{legacy activity status is Bankrupt or personal insolvency fires}, \\
\mathrm{Defaulted}, & distressMonths_{h,\tau} \ge bankruptcyDistressMonths, \\
\mathrm{Arrears}, & distressMonths_{h,\tau} \ge 2, \\
\mathrm{LiquidityStress}, & distressMonths_{h,\tau} = 1.
\end{cases}
\end{aligned}
$$

Personal insolvency is a stochastic filing hazard, not a deterministic counter
cliff. The hazard is zero before `personalInsolvencyMinDistressMonths`, then
rises with distress duration toward `personalInsolvencyDistressMonths` and with
arrears/debt-service burden:

$$
\begin{aligned}
durationRamp_{h} &=  \\
&\quad \mathrm{clamp}((distressMonths_{h,\tau} - personalInsolvencyMinDistressMonths) \\
&\quad / \max(1, personalInsolvencyDistressMonths - personalInsolvencyMinDistressMonths), 0, 1) \\
 \\
burden_{h} &=  \\
&\quad \mathrm{clamp}((LiquidityShortfall_{h} + ConsumerDebtService_{h}) / Inc_{h,\tau}, 0, 1) \\
 \\
piInsolvency_{h} &=  \\
&\quad \min(personalInsolvencyMaxHazard, \\
&\quad personalInsolvencyBaseHazard \\
&\quad + (personalInsolvencyMaxHazard - personalInsolvencyBaseHazard) \\
&\quad \cdot durationRamp_{h} \\
&\quad + personalInsolvencyBurdenHazardWeight \cdot burden_{h})
\end{aligned}
$$

Distressed workout and filing defaults are bounded by current consumer-debt
arrears, multiples of consumer-debt service, and a share of outstanding
principal:

$$
\begin{aligned}
DefaultCap_{h} &=  \\
&\quad \min(CL^{H}_{h,\tau-pre}, \\
&\quad \max(ConsumerDebtArrears_{h}, ConsumerDebtService_{h} \cdot debtServiceMonthsCap), \\
&\quad CL^{H}_{h,\tau-pre} \cdot outstandingShareCap) \\
 \\
CL^{H}_{h,\tau} &= CL^{H}_{h,\tau-pre} - DefaultCap_{h} \\
E^{H}_{h,\tau} &= 0 \\
distressState_{h,\tau} &= Bankruptcy
\end{aligned}
$$

The mortgage stock is not written off by this household personal-insolvency
branch. Mortgage default is an aggregate housing/banking flow, while household
personal insolvency is an unsecured consumer-credit workout/default and equity
write-off state.

Long unemployment creates labor-market scarring:

$$
\begin{aligned}
skill_{h,\tau} &=
\begin{cases}
skill_{h,t} (1 - skillDecayRate), & \text{if } monthsUnemployed_{h} \ge scarringOnset, \\
skill_{h,t}, & \text{otherwise},
\end{cases} \\
 \\
healthPenalty_{h,\tau} &=
\begin{cases}
\min(healthPenalty_{h,t} + scarringRate, scarringCap), & \text{if } monthsUnemployed_{h} \ge scarringOnset, \\
healthPenalty_{h,t}, & \text{otherwise},
\end{cases} \\
 \\
wageScar_{h,\tau} &=
\begin{cases}
\min(wageScar_{h,t} + wageScarRate, wageScarCap), & \text{if } monthsUnemployed_{h} \ge scarringOnset, \\
wageScar_{h,t}, & \text{otherwise}.
\end{cases}
\end{aligned}
$$

Re-employment decays the wage scar toward zero.

## Labor Mobility And Retraining

Employed households can attempt voluntary cross-sector search. The target
sector is selected from sector wage and vacancy signals, adjusted by the labor
friction matrix. A transition into retraining occurs only when:

$$
\begin{aligned}
RetrainingGate_{h,\tau} &\Longleftrightarrow
U^{search}_{h,\tau} < voluntarySearchProb \\
&\quad \land vacancies_{target,\tau} > 0 \\
&\quad \land targetWage_{target,\tau} \cdot \mathrm{crossSectorWagePenalty}(friction)
> wage_{h} \cdot (1 + voluntaryWageThreshold) \\
&\quad \land friction \le adjacentFrictionMax \\
&\quad \land D^{H}_{h,t} > \mathrm{retrainingCost}(friction)
\end{aligned}
$$

Unemployed households can enter retraining after the unemployment threshold if
retraining is enabled, they have enough deposits to pay the relevant cost, and
the retraining draw succeeds. Distressed social neighbors increase the
retraining probability.

Retraining completion succeeds with:

$$
\begin{aligned}
p^{retrain}_{h,\tau} &=  \\
&\quad retrainingBaseSuccess \\
&\quad \cdot skill_{h,\tau} \\
&\quad \cdot (1 - healthPenalty_{h,\tau}) \\
&\quad \cdot educationMultiplier_{education_{h}} \\
&\quad \cdot (1 - friction \cdot frictionSuccessDiscount)
\end{aligned}
$$

A successful completion resets the unemployment spell to zero; a failed
completion returns the household to unemployment with a configured
post-failure unemployment spell. Labor-market matching is handled by the labor
market stage, not by the household budget rule itself.

## Remittances, Imports, And External Links

Immigrant households send remittances out of post-tax-and-transfer monthly
income, before rent, debt service, consumption, and liquidity settlement are
applied:

$$
\begin{aligned}
Remit_{h,\tau} &=
\begin{cases}
Inc_{h,\tau} \cdot remitRate, & immigrant_{h}, \\
0, & \text{otherwise}.
\end{cases}
\end{aligned}
$$

The household import share is adjusted by the exchange rate:

$$
\begin{aligned}
ImportAdj_{\tau} &=  \\
&\quad \mathrm{clamp}(importPropensity \cdot (baseExRate / ExRate_{\tau})^{ImportErElasticity}, 0, 1)
\end{aligned}
$$

Household aggregate consumption is split as:

$$
\begin{aligned}
GoodsCons_{\tau} &= \sum_{h} Cons_{h,\tau} \\
TotalCons_{\tau} &= GoodsCons_{\tau} + \sum_{h} rent_{h} \\
ImportCons_{\tau} &= GoodsCons_{\tau} \cdot ImportAdj_{\tau} \\
DomesticCons_{\tau} &= TotalCons_{\tau} - ImportCons_{\tau}
\end{aligned}
$$

Diaspora remittance inflow and tourism services are computed at the household
financial-economics aggregate boundary. They are external-sector calibration
bridges, not household micro decisions.

## Accounting And SFC Mapping

Household behavior is translated into ledger and SFC evidence through the
monthly flow layer:

| Household surface | Main flow/SFC interpretation |
| --- | --- |
| goods consumption | household cash to firms |
| rent | household cash to landlord/housing settlement |
| PIT | household cash to government |
| deposit interest | bank cash to households |
| remittances | household cash to foreign transfer settlement |
| consumer-credit origination | bank-issued consumer-loan asset/liability stock movement |
| liquidity-shortfall financing | same-month consumer-loan bridge/write-off mechanism |
| consumer principal | consumer-loan stock repayment |
| consumer interest | household cash to banks |
| consumer default | consumer-loan stock write-off/default evidence |
| mortgage origination/repayment/default | mortgage principal settlement shell |
| mortgage interest | household cash to banks |
| equity revaluation and write-off | listed-equity stock evidence and household bankruptcy diagnostics |

The exact accounting contract is validated by runtime ledger conservation and
SFC semantic identities. Household monthly rows are also checked against
aggregate totals for consumer default, consumer-loan default, bridge
charge-off, liquidity shortfall, and each shortfall component.

## Output And Diagnostic Surfaces

Representative output columns include:

| Surface | Columns |
| --- | --- |
| Labor and income | `Unemployment`, `UnemployedShare`, `RetrainingShare`, `MarketWage`, `MeanEmployedWage`, contract-type shares |
| Consumption and distribution | aggregate consumption, domestic/import consumption, Gini/poverty/savings aggregates in household summaries |
| Consumer credit | `ConsumerLoans`, `ConsumerOrigination`, `ConsumerApprovedOrigination`, `ConsumerCreditDemand`, `ConsumerRejectedOrigination`, `ConsumerBankRejectedOrigination`, `ConsumerCredit_RejectedPortfolioPreference`, household snapshot bank-audit columns (`ConsumerBankApprovalProduct`, `ConsumerBankRejectionReason`, `ConsumerBankApprovalProbability`, `ConsumerBankApprovalRoll`, `ConsumerBankProjectedCAR`, `ConsumerBankMinCAR`, `ConsumerBankManagementCAR`, `ConsumerBankCapitalThrottle`, `ConsumerBankLCR`, `ConsumerBankNSFR`, `ConsumerBankPortfolio*`), `ConsumerDebtService`, `ConsumerPrincipal`, `ConsumerDefault`, `ConsumerLoanDefault`, `ConsumerInsolvencyDefault`, `LiquidityBridgeChargeOff`, `ConsumerCredit_NetStockFlow`, `ConsumerCredit_UnderwrittenNetFlow`, `ConsumerCredit_BridgeNetFlow`, `ConsumerCredit_NplStock`, approval/rejection ratios |
| Mortgages and housing wealth | `MortgageStock`, `AvgMortgageRate`, `MortgageOrigination`, `MortgageRepayment`, `MortgageDefault`, `MortgageNetStockFlow`, `MortgageToGdp`, mortgage flow-to-stock ratios, `MortgageInterestIncome`, `HhHousingWealth`, `HousingWealthEffect` |
| Distress and bankruptcy | `HouseholdBankruptcies`, `HouseholdBankruptcyRate`, `HouseholdDistress_Current`, `HouseholdDistress_LiquidityStress`, `HouseholdDistress_Arrears`, `HouseholdDistress_Restructuring`, `HouseholdDistress_Defaulted`, `HouseholdDistress_Bankruptcy`, corresponding shares, `HouseholdDistress_ActiveShare` |
| Liquidity | `HouseholdLiquidity_NetDemandDeposit`, positive deposits, implicit overdraft diagnostics, deposit percentiles, `HouseholdLiquidity_ShortfallFinancing`, unmet basic consumption, discretionary compression, consumption/rent/mortgage/consumer-debt arrears, temporary overdraft |
| External household links | `RemittanceOutflow`, `DiasporaRemittanceInflow`, `NetRemittances`, tourism import/export columns |
| Regional and micro surfaces | regional unemployment columns, optional household snapshot columns for status, wage, financial distress, deposits, mortgage/consumer debt, DSR inputs, arrears/defaults, and liquidity shortfall rows |

Borrower-level loan-origination diagnostics link household underwriting
observations to later arrears, default, bankruptcy, and write-off outcomes over
an outcome window. HH-bank lead/lag diagnostics expose timing relationships
between household credit stress and bank balance-sheet stress.

## Validation Coverage

| Layer | Coverage |
| --- | --- |
| Unit/property tests | `HouseholdSpec`, `HouseholdPropertySpec`, `BufferStockMpcSpec`, `PitSpec`, `SocialTransferSpec`, `ConsumerCreditSpec`, deposit mobility, household income/financial economics, household flow tests, labor-market and regional tests. |
| Schema/diagnostic tests | Household snapshot schema/schedule, household shortfall cohorts, Monte Carlo schema, loan-origination quality diagnostics, HH-bank lead/lag diagnostics, and household credit-stress calibration. |
| Integration/nightly | Monte Carlo TSV integration, nightly diagnostics, loan-origination quality, scenario runs, sensitivity/robustness, SFC matrix evidence, and generated-output guard. |
| Accounting | Runtime ledger execution, household-flow exactness, mortgage-flow exactness, consumer-credit stock-flow diagnostics, SFC semantic projection, and household monthly row-to-aggregate reconciliation. |

## Current Limitations

- Household equations are executable and auditable, but several credit-stress,
  consumption, mobility, and distress parameters remain calibration targets
  rather than final publication estimates.
- Mortgage origination and default are aggregate housing/banking mechanisms.
  The household step models scheduled household-side mortgage service and
  burden, not micro-level mortgage application, collateral liquidation, or
  foreclosure resolution.
- Personal insolvency writes off listed equity and records a bounded unsecured
  consumer-loan default, but it does not remove the household from the labor
  force or automatically erase all remaining unsecured principal. This is
  intentional in the current implementation and should be read as a
  financial-distress state, not demographic exit.
- Liquidity-shortfall financing is an accounting settlement mechanism for
  non-negative deposits. It is not discretionary consumption credit and should
  be interpreted together with the shortfall-component diagnostics.
- Retraining completion currently returns households to unemployment for later
  labor-market matching. The model does not yet implement a full individual
  training-to-employment placement mechanism.
- Household snapshots and loan-origination diagnostics are optional evidence
  surfaces. The standard Monte Carlo time series remains aggregate/meso-level,
  not a full household panel export by default.
