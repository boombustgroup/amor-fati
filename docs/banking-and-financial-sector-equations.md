# Banking And Financial-Sector Equations

This document consolidates Amor Fati's implemented banking and financial-sector
rules into a publication-facing model section. It describes the executable
SFC-ABM contract. It does not introduce new bank behavior, new calibration
targets, or new failure semantics.

The section complements [institutional-sector-equations.md](institutional-sector-equations.md):
that document owns central government, NBP, external sector, insurance, NBFI,
quasi-fiscal, JST, and social-fund details. This document makes the commercial
banking system and its financial-sector interfaces readable as one coherent
financial-stability block.

## Source Anchors

| Source | Role |
| --- | --- |
| [Model specification](model-specification.md#reviewer-reading-path) | Canonical reviewer path and model overview. |
| [Model notation and state vector](model-notation-and-state-vector.md#banks) | Bank state, ledger stock, rate, ratio, and flow notation. |
| [Monthly transition function](monthly-transition-function.md) | Month-step timing and deterministic transition contract. |
| [Behavioral rule catalog](behavioral-equations-and-decision-rules.md#banking-rules) | Implementation-oriented banking rules and output-column map. |
| [Institutional sector equations](institutional-sector-equations.md) | NBP, insurance, NBFI/TFI, quasi-fiscal, public-sector, and external-sector equations. |
| [Bank balance-sheet benchmark](bank-balance-sheet-benchmark.md) | Opening bank balance-sheet guardrails and calibration-warning bands. |
| [Engine invariants and semantics](engine-invariants-and-semantics.md) | Failure semantics, hard invariants, warning surfaces, and known limitations. |
| [`agents/Banking.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Banking.scala) and [`agents/banking/*`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/banking) | Bank domain facade, credit approval, regulatory metrics, bond portfolio, capital waterfall, and resolution helpers. |
| [`engine/economics/BankingEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/BankingEconomics.scala) and [`engine/economics/banking/*`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/banking) | Monthly banking-stage runner, bond waterfall, interbank settlement, failure pipeline, reconciliation, and ledger close. |
| [`engine/diagnostics/banking/BankDiagnostics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/diagnostics/banking/BankDiagnostics.scala) | Bank capital, failure, resolution, reconciliation, and ECL diagnostics. |
| [`engine/flows/BankingFlows.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/flows/BankingFlows.scala), [`InsuranceFlows.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/flows/InsuranceFlows.scala), [`NbfiFlows.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/flows/NbfiFlows.scala), [`QuasiFiscalFlows.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/flows/QuasiFiscalFlows.scala), [`CorpBondFlows.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/flows/CorpBondFlows.scala), [`GovBondFlows.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/flows/GovBondFlows.scala) | Runtime flow emission and SFC evidence for financial-sector mechanisms. |
| [`montecarlo/timeseries/McTimeseriesSchema.scala`](../modules/montecarlo/src/main/scala/com/boombustgroup/amorfati/montecarlo/timeseries/McTimeseriesSchema.scala) | Public output columns for bank, bond, insurance, NBFI, and quasi-fiscal diagnostics. |

## Scope And Timing

Commercial banks are ten banking-sector rows: named bank archetypes plus the
residual Other banks row. A row is a banking-sector balance-sheet archetype, not
a full legal-entity model of one commercial bank.

The banking stage runs near the end of the same-month economics sequence, after
households, firms, household financial aggregation, price/equity, and
open-economy/NBP conditions are known. It then:

1. projects opening ledger-owned bank balances;
2. computes public-finance and housing-bank flows;
3. settles NBP reserve, standing-facility, FX, and interbank channels;
4. settles government-bond and bank corporate-bond holdings;
5. detects failures, applies bail-in, resolves failed rows, and reconciles
   aggregate exactness;
6. emits closing bank state, financial stocks, monetary aggregates, diagnostics,
   and ledger/SFC flow evidence.

Firm and household credit approvals call the product-aware bank-side approval
rules, but the RNG owner is the stage that invokes them: firm approvals are
owned by the firm stage and household consumer-credit approvals by the household
path. The
`BankingEconomics` randomness stream is for stochastic choices executed inside
the banking stage itself, especially health-driven deposit mobility.

## State

For bank row `b`, the operational bank state is:

$$
\begin{aligned}
a^{B}_{b,t}
\end{aligned}
$$

It contains regulatory/accounting capital, failure status, HTM book yield,
firm-loan NPL diagnostics, consumer NPL diagnostics, loan-maturity buckets, and
IFRS 9 ECL staging.

Ledger-owned financial balances are:

$$
\begin{aligned}
l^{B}_{b,t} \in L_{t}^{\mathrm{banks}}
\end{aligned}
$$

with principal bank stock families:

| Symbol | Runtime field | Meaning |
| --- | --- | --- |
| $D^{B}_{b,t}$ | `BankBalances.totalDeposits` | customer deposit liability |
| $D^{B,demand}_{b,t}$ | `BankBalances.demandDeposit` | demand-deposit liability |
| $D^{B,term}_{b,t}$ | `BankBalances.termDeposit` | term-deposit liability |
| $L^{B,F}_{b,t}$ | `BankBalances.firmLoan` | firm-loan asset |
| $L^{B,H}_{b,t}$ | `BankBalances.consumerLoan` | consumer-loan asset |
| $M^{B}_{b,t}$ | `BankBalances.mortgageLoan` | mortgage-loan asset mirror used by SFC evidence |
| $GB^{AFS}_{b,t}$ | `BankBalances.govBondAfs` | available-for-sale government-bond asset |
| $GB^{HTM}_{b,t}$ | `BankBalances.govBondHtm` | held-to-maturity government-bond asset |
| $Res^{B}_{b,t}$ | `BankBalances.reserve` | reserve asset at NBP |
| $IB_{b,t}$ | `BankBalances.interbankLoan` | net interbank position |
| $CB^{B}_{b,t}$ | `BankBalances.corpBond` | corporate-bond asset |

Bank capital is written:

$$
\begin{aligned}
K^{B}_{b,t} &= \mathrm{BankState.capital}
\end{aligned}
$$

$K^{B}$ is persisted operational state and an SFC-validated diagnostic stock. It
is not a holder-resolved ledger equity instrument and does not live in
`LedgerFinancialState.BankBalances`. The bank-capital ownership contract is
kept in this section, [engine invariants and semantics](engine-invariants-and-semantics.md),
and the [bank balance-sheet benchmark](bank-balance-sheet-benchmark.md).

## Lending Rates And Approval

The household deposit rate is:

$$
\begin{aligned}
r^{dep}_{b,\tau} &=  \\
&\quad \max(r^{\mathrm{NBP}}_{\tau} - householdDepositSpread, 0)
\end{aligned}
$$

Bank-credit lending rates first form a product-aware pre-portfolio private-credit rate:

$$
\begin{aligned}
r^{preLoan}_{b,\tau}(q) &=  \\
&\quad r^{base}_{\tau} \\
&\quad + baseSpread \\
&\quad + lendingSpread_{b} \\
&\quad + \min(NPLRatio_{b}(q) \cdot nplSpreadFactor, NplSpreadCap) \\
&\quad + capitalPenalty_{b}
\end{aligned}
$$

where:

$$
\begin{aligned}
effectiveMinCar_{b}(ccyb_{\tau}) &=  \\
&\quad minCar \\
&\quad + ccyb_{\tau} \\
&\quad + OSIIBuffer_{b} \\
&\quad + P2RAddon_{b} \\
 \\
managementCAR_{b}(ccyb_{\tau}) &=  \\
&\quad effectiveMinCar_{b}(ccyb_{\tau}) + creditManagementCarBuffer \\
 \\
capitalPenalty_{b} &=  \\
&\quad \max(managementCAR_{b}(ccyb_{\tau}) - CAR_{b}, 0) \\
&\quad \cdot creditCarShortfallPenaltyScale
\end{aligned}
$$

The government-bond channel is a portfolio-choice wedge, not a loanable-funds
capacity subtraction. For product bucket `q`, the bank compares the
risk-adjusted private-credit return with the sovereign return:

$$
\begin{aligned}
expectedLossCost_{b}(q) &=  \\
&\quad NPLRatio_{b}(q) \cdot LGD_{q} \\
 \\
capitalCost_{b}(q) &=  \\
&\quad portfolioCapitalHurdleRate \\
&\quad \cdot (effectiveMinCar_{b}(ccyb_{\tau}) + creditManagementCarBuffer) \\
&\quad \cdot riskWeight_{q} \\
 \\
polishBankLevyCost_{b}(q, A) &=  \\
&\quad \ell^{bank}_{b,\tau}(q, A) \\
 \\
r^{loan,RA}_{b,\tau}(q, A) &=  \\
&\quad r^{preLoan}_{b,\tau}(q) \\
&\quad - expectedLossCost_{b}(q) \\
&\quad - capitalCost_{b}(q) \\
&\quad - polishBankLevyCost_{b}(q, A) \\
 \\
r^{bond,RA}_{b,\tau} &=  \\
&\quad govBondMarketYield_{\tau} \\
 \\
wedge_{b}(q, A) &=  \\
&\quad r^{loan,RA}_{b,\tau}(q, A) - r^{bond,RA}_{b,\tau} \\
 \\
portfolioPricePremium_{b}(q, A) &=  \\
&\quad \max(-wedge_{b}(q, A), 0) \cdot portfolioWedgePriceShare \\
 \\
r^{loan}_{b,\tau}(q) &=  \\
&\quad r^{preLoan}_{b,\tau}(q) + portfolioPricePremium_{b}(q, 0)
\end{aligned}
$$

Here $\ell^{bank}_{b,\tau}(q, A)$ is the annualized marginal
Polish-bank-levy rate on the requested private-credit asset.

The portfolio premium is amount-dependent because $wedge_{b}(q, A)$ includes the
marginal Polish bank-levy perimeter. Runtime pricing evaluates the premium at
$A = 0$: `BankCreditApproval.lendingRate` calls
`BankPortfolioChoice.compute(... amount = PLN.Zero ...)`. Runtime approval uses
the requested exposure: `BankCreditApproval.creditApproval` calls
`BankPortfolioChoice.compute(... amount = amount ...)`, so levy and quantity
throttling are evaluated on the actual credit request.

The capital stack used by `Macroprudential.effectiveMinCar` is auditable from
configuration and calibration provenance:

| Component | Runtime symbol | 2026-04-30 baseline status |
| --- | --- | --- |
| Basel/CRR base minimum | `minCar` | 8% minimum capital adequacy ratio. |
| Countercyclical buffer | $ccyb_{\tau}$ | Opens at $initialCcyb = 1\%$; later months follow the endogenous CCyB build/release rule capped by `ccybMax`. |
| O-SII buffer | $OSIIBuffer_{b}$ | KNF O-SII buffer mapped to the named bank archetype; `Alior` is zero because it is not on the KNF O-SII list, `BPS/Coop` maps cooperative-bank O-SII rows, and `Other banks` carries Handlowy/SGB/residual exposure. |
| Pillar 2 requirement | $P2RAddon_{b}$ | Bank-archetype bridge prior; the public row-level source is still marked as incomplete in the calibration register. |

Failed banks receive a fixed penalty spread and cannot approve new credit.

For a proposed new credit amount $A$ in product bucket
$q \in \{\mathrm{firm}, \mathrm{consumer}, \mathrm{mortgage}\}$, approval uses hard regulatory gates and a
stochastic draw. The requested product determines which exposure bucket is
incremented before projected RWA is computed:

$$
\begin{aligned}
FirmLoans'_{b}(q, A) &=
\begin{cases}
L^{B,F}_{b,t} + A, & q = \mathrm{firm}, \\
L^{B,F}_{b,t}, & \text{otherwise},
\end{cases} \\
 \\
ConsumerLoans'_{b}(q, A) &=
\begin{cases}
L^{B,H}_{b,t} + A, & q = \mathrm{consumer}, \\
L^{B,H}_{b,t}, & \text{otherwise},
\end{cases} \\
 \\
MortgageLoans'_{b}(q, A) &=
\begin{cases}
M^{B}_{b,t} + A, & q = \mathrm{mortgage}, \\
M^{B}_{b,t}, & \text{otherwise},
\end{cases} \\
 \\
ProjectedRWA_{b}(q, A) &=  \\
&\quad RWA_{b}( \\
firmLoans &= FirmLoans'_{b}(q, A), \\
consumerLoans &= ConsumerLoans'_{b}(q, A), \\
mortgageLoans &= MortgageLoans'_{b}(q, A), \\
corpBondHoldings &= CB^{B}_{b,t}, \\
interbankAssets &= \max(IB_{b,t}, 0), \\
govBondHoldings &= GB^{AFS}_{b,t} + GB^{HTM}_{b,t}, \\
reserves &= Res^{B}_{b,t}, \\
capitalBackstop &= \max(K^{B}_{b,t}, 0) \\
&\quad ) \\
 \\
ProjectedCAR_{b}(q, A) &= K^{B}_{b,t} / ProjectedRWA_{b}(q, A) \\
 \\
carOk &= ProjectedCAR_{b}(q, A) \ge effectiveMinCar_{b}(ccyb_{\tau}) \\
lcrOk &= LCR_{b} \ge lcrMin \\
nsfrOk &= NSFR_{b} \ge nsfrMin
\end{aligned}
$$

Firm credit requests use $q = firm$; household consumer-credit requests use
$q = consumer$ after household-side affordability and distress filters have
passed. Mortgage approval is represented in the same product vocabulary even
where current origination is handled by housing-stage aggregate flows.

If all hard gates pass, risk-weighted origination is still throttled by the
distance between projected CAR and the bank's internal management target:

$$
\begin{aligned}
managementCAR_{b} &= effectiveMinCar_{b}(ccyb_{\tau}) + creditManagementCarBuffer \\
capitalThrottle_{b}(q, A) &=  \\
&\quad \mathrm{clamp}((ProjectedCAR_{b}(q, A) - effectiveMinCar_{b}(ccyb_{\tau})) / \\
&\quad creditManagementCarBuffer, 0, 1) \\
 \\
approvalP_{b}(q, A) &= capitalThrottle_{b}(q, A) \\
&\quad \cdot portfolioThrottle_{b}(q, A) \\
 \\
portfolioThrottle_{b}(q, A) &=  \\
&\quad 1 - \mathrm{clamp}( \\
&\quad \max(-wedge_{b}(q, A), 0) \\
&\quad \cdot (1 - portfolioWedgePriceShare) \\
&\quad \cdot portfolioWedgeQuantitySensitivity, \\
&\quad 0, \\
&\quad 1 \\
&\quad )
\end{aligned}
$$

The proposal is approved when the hard regulatory gates pass and the replay
draw is below $approvalP_{b}$. A bank at or above $managementCAR_{b}$ has
$capitalThrottle_{b} = 1$; portfolio preference can still reduce approval if the
risk-adjusted sovereign alternative dominates private credit. A bank between
the hard floor and the management target reduces new risk-weighted credit supply
smoothly. NPL pressure is not independently multiplied into the approval
probability; it enters through loan pricing, expected-loss/provisioning terms,
and the resulting capital path. Reserve requirements are not a per-loan approval
gate. They are handled through LCR/NSFR, reserve settlement, standing
facilities, and bank P&L.

This approval rule is a regulatory heuristic and behavioral credit-supply
surface, not an SFC identity. The resulting origination, repayment, default,
interest, and rejected-demand quantities are what later enter ledger/SFC and
diagnostic surfaces.

## Regulatory Ratios

Risk-weighted assets use an explicit regulatory perimeter. Default weights are
100 percent for firm and unsecured consumer loans, 35 percent for mortgages, 50
percent for bank-held corporate bonds, 20 percent for positive interbank assets,
and 0 percent for domestic sovereign bonds and NBP reserves. Two floors prevent
economically meaningless zero-RWA CAR for live shells:

$$
\begin{aligned}
WeightedExposureRWA_{b} &=  \\
&\quad firmLoanRiskWeight \cdot L^{B,F}_{b,t} \\
&\quad + consumerLoanRiskWeight \cdot L^{B,H}_{b,t} \\
&\quad + mortgageLoanRiskWeight \cdot M^{B}_{b,t} \\
&\quad + corpBondRiskWeight \cdot CB^{B}_{b,t} \\
&\quad + interbankAssetRiskWeight \cdot \max(IB_{b,t}, 0) \\
&\quad + sovereignRiskWeight \cdot (GB^{AFS}_{b,t} + GB^{HTM}_{b,t}) \\
&\quad + reserveRiskWeight \cdot Res^{B}_{b,t} \\
 \\
OperationalRiskFloor_{b} &=  \\
&\quad rwaOperationalRiskFloor \\
&\quad \cdot (L^{B,F}_{b,t} \\
&\quad + L^{B,H}_{b,t} \\
&\quad + M^{B}_{b,t} \\
&\quad + CB^{B}_{b,t} \\
&\quad + \max(IB_{b,t}, 0) \\
&\quad + GB^{AFS}_{b,t} + GB^{HTM}_{b,t} \\
&\quad + Res^{B}_{b,t}) \\
 \\
CapitalBackstopFloor_{b} &=  \\
&\quad rwaCapitalBackstop \cdot \max(K^{B}_{b,t}, 0) \\
 \\
RWA_{b} &=  \\
&\quad \max(WeightedExposureRWA_{b}, \\
&\quad OperationalRiskFloor_{b}, \\
&\quad CapitalBackstopFloor_{b}) \\
 \\
CAR_{b} &= K^{B}_{b,t} / RWA_{b}
\end{aligned}
$$

The simplified LCR is:

$$
\begin{aligned}
HQLA_{b} &=  \\
&\quad Res^{B}_{b,t} \\
&\quad + GB^{AFS}_{b,t} \\
&\quad + GB^{HTM}_{b,t} \\
 \\
NetCashOutflows_{b} &=  \\
&\quad D^{B,demand}_{b,t} \cdot demandDepositRunoff \\
 \\
LCR_{b} &= HQLA_{b} / NetCashOutflows_{b}
\end{aligned}
$$

The simplified NSFR is:

$$
\begin{aligned}
ASF_{b} &=  \\
&\quad K^{B}_{b,t} \\
&\quad + 0.95 \cdot D^{B,term}_{b,t} \\
&\quad + 0.90 \cdot D^{B,demand}_{b,t} \\
 \\
RSF_{b} &=  \\
&\quad 0.50 \cdot LoansShort_{b} \\
&\quad + 0.65 \cdot LoansMedium_{b} \\
&\quad + 0.85 \cdot LoansLong_{b} \\
&\quad + 0.05 \cdot (GB^{AFS}_{b,t} + GB^{HTM}_{b,t}) \\
&\quad + 0.50 \cdot CB^{B}_{b,t} \\
 \\
NSFR_{b} &= ASF_{b} / RSF_{b}
\end{aligned}
$$

Ratios with near-zero denominators return a safe high ratio rather than a
spurious failure.

The aggregate bank NPL ratio used by interbank, pricing, and corporate-bond
feedback is the firm-loan NPL ratio:

$$
\begin{aligned}
NPLRatio &=  \\
&\quad FirmNplStock / FirmLoans
\end{aligned}
$$

Consumer loan stock is included in the ECL covered-loan base, but consumer
defaults and consumer NPL are carried separately from ECL Stage 3 migration.
Realized consumer-credit losses enter bank-capital diagnostics through the
consumer-credit loss surfaces. Per-bank and aggregate CAR, NPL, LCR, and NSFR
values are regulatory diagnostic and decision inputs. They are not themselves
monetary flows.

## IFRS 9 ECL Staging

Each bank carries a three-stage ECL stock:

$$
\begin{aligned}
ECL_{b} &= (S1_{b}, S2_{b}, S3_{b})
\end{aligned}
$$

with allowance:

$$
\begin{aligned}
Allowance_{b} &=  \\
&\quad S1_{b} \cdot eclRate1 \\
&\quad + S2_{b} \cdot eclRate2 \\
&\quad + S3_{b} \cdot eclRate3
\end{aligned}
$$

Opening staging currently assigns the covered firm and consumer loan book to
Stage 1:

$$
\begin{aligned}
ECL_{b,0} &= (coveredLoans_{b}, 0, 0)
\end{aligned}
$$

Monthly macro-driven Stage 1 to Stage 2 migration is:

$$
\begin{aligned}
migrationRate_{\tau} &=  \\
&\quad eclMigrationSensitivity \\
&\quad \cdot \max(0, unemployment_{\tau} - referenceUnemployment_{\tau}) \\
&\quad + eclGdpSensitivity \\
&\quad \cdot \max(0, -gdpGrowthMonthly_{\tau})
\end{aligned}
$$

clamped to `[0, eclMaxMigration]`.

Stage transitions are:

$$
\begin{aligned}
S1ToS2_{b} &= S1_{b,t} \cdot migrationRate_{\tau} \\
S2ToS3_{b} &= newDefaults_{b} \\
S3Cure_{b} &= S3_{b,t} \cdot eclCureRate \\
 \\
S3_{b,\tau} &= \max(S3_{b,t} + S2ToS3_{b} - S3Cure_{b}, 0) \\
S2_{b,\tau} &= \max(S2_{b,t} + S1ToS2_{b} - S2ToS3_{b} + S3Cure_{b}, 0) \\
S1_{b,\tau} &= \max(TotalCoveredLoans_{b,\tau} - S2_{b,\tau} - S3_{b,\tau}, 0)
\end{aligned}
$$

In the current runtime, $newDefaults_{b}$ is the bank's new firm-loan NPL flow.
Consumer-credit, personal-insolvency, liquidity-bridge, and mortgage defaults
do not enter $S2ToS3_{b}$; they are tracked separately as product-specific
diagnostics and, for ordinary consumer-loan, personal-insolvency, and mortgage
defaults, realized loss terms in the bank-capital waterfall. Liquidity-bridge
charge-off is a same-month bridge settlement product by default, so it has gross
and recovery diagnostics but no capital loss unless a future calibration lowers
the bridge settlement/recovery share. These household-credit products do not
draw ECL allowance until a corresponding product-level allowance stock exists.
This keeps the macro ECL provision change separate from realized default loss
and prevents double counting.

The provision change is:

$$
\begin{aligned}
EclProvisionChange_{b} &=  \\
&\quad Allowance_{b,\tau} - Allowance_{b,t}
\end{aligned}
$$

A positive provision change reduces bank capital through the bank-capital
waterfall. ECL is an accounting provision channel; realized credit losses remain
separate diagnostics.

## Interbank, NBP Facilities, And Monetary Aggregates

The interbank rate is a corridor rate between the deposit facility and lombard
facility:

$$
\begin{aligned}
creditStress &=  \\
&\quad \mathrm{clamp}(AggregateNplRatio / stressThreshold, 0, 1) \\
 \\
liquidityRatio &=  \\
&\quad \mathrm{clamp}(ExcessReserves / RequiredReserves, 0, 1) \\
 \\
interbankRate &=  \\
&\quad depositFacilityRate \\
&\quad + (1 - liquidityRatio) \\
&\quad \cdot creditStress \\
&\quad \cdot (lombardRate - depositFacilityRate)
\end{aligned}
$$

Interbank clearing matches banks with reserve surpluses to banks with reserve
deficits. A hoarding factor, driven by aggregate NPL stress, reduces lending
supply. If no matching is possible, interbank loans are cleared while existing
reserves are preserved.

NBP reserve-side settlement combines $reserveInterest_{b}$,
$standingFacilityIncome_{b}$, $interbankInterest_{b}$, and
$fxPlnInjection_{b}$ on bank reserve balances. If a drain would push reserves below zero, the
shortfall is surfaced as an explicit standing-facility backstop and reserves
remain non-negative.

Monetary aggregates are:

$$
\begin{aligned}
M0 &= \sum_{b \in live} Res^{B}_{b} \\
M1 &= \sum_{b \in live} D^{B,demand}_{b} \\
M2 &= M1 + \sum_{b \in live} D^{B,term}_{b} \\
M3 &= M2 + TfiAum + CorpBondsOutstanding \\
CreditMultiplier &= M2 / \max(M0, 1 \mathrm{PLN})
\end{aligned}
$$

## Government-Bond And Corporate-Bond Waterfalls

Government-bond issuance or redemption first changes commercial-bank portfolios.
The banking bond waterfall then sells actual available bank-held bonds to
foreign, NBP/QE, PPK, insurance, and TFI buyers in sequence:

$$
\begin{aligned}
BankPrimaryGovBondChange_{\tau} &=  \\
&\quad GovBondOutstanding_{\tau} - GovBondOutstanding_{t} \\
 \\
AvailableBankBonds_{\tau} \\
{} \to \text{foreign purchase} \\
{} \to \text{NBP QE purchase} \\
{} \to \text{PPK purchase} \\
{} \to \text{insurance purchase} \\
{} \to \text{TFI purchase}
\end{aligned}
$$

The runtime evidence records the per-bank source of each movement through
`GovBondRuntimeMovements`.

Corporate bonds are issuer and holder-resolved ledger stocks. The market step
computes yield, coupon, amortization, default, and issuance:

$$
\begin{aligned}
CorpBondYield_{\tau} &=  \\
&\quad \max(GovBondMarketYield_{\tau} + creditSpread_{\tau}, \\
&\quad MinCorpBondYield) \\
 \\
creditSpread_{\tau} &=  \\
&\quad \min(baseSpread \cdot (1 + NPLRatio_{\tau} \cdot nplSensitivity), \\
&\quad maxSpread) \\
 \\
Coupon_{\tau} &=  \\
&\quad CorpBondHoldings_{t} \cdot CorpBondYield_{\tau} / 12 \\
 \\
Amortization_{\tau} &=  \\
&\quad CorpBondsOutstanding_{t} / maturityMonths \\
 \\
DefaultLoss_{\tau} &=  \\
&\quad GrossDefault_{\tau} \cdot (1 - recovery)
\end{aligned}
$$

New issuance is limited by investor appetite and bank CAR headroom. Bank-held
corporate-bond holdings are settled after amortization/default reductions and
new issuance allocation. Bank-held corporate-bond default losses enter the bank
capital waterfall.

## Bank Capital Waterfall

For each bank, ordinary retained income and realized losses update
`BankState.capital`:

$$
\begin{aligned}
grossIncome_{b} &=  \\
&\quad firmLoanInterest_{b} \\
&\quad + govBondIncome_{b} \\
&\quad - depositInterest_{b} \\
&\quad + reserveInterest_{b} \\
&\quad + standingFacilityIncome_{b} \\
&\quad + interbankInterest_{b} \\
&\quad + mortgageInterestIncome_{b} \\
&\quad + consumerInterestIncome_{b} \\
&\quad + corpBondCoupon_{b} \\
 \\
losses_{b} &=  \\
&\quad firmNplLoss_{b} \\
&\quad + mortgageNplLoss_{b} \\
&\quad + consumerNplLoss_{b} \\
&\quad + corpBondDefaultLoss_{b} \\
&\quad + bfgLevy_{b} \\
&\quad + polishBankLevyTax_{b} \\
&\quad + unrealizedBondLoss_{b} \\
 \\
retainedIncome_{b} &=  \\
&\quad grossIncome_{b} \cdot profitRetention \\
 \\
K^{B}_{b,\tau,ordinary} &=  \\
&\quad K^{B}_{b,t} \\
&\quad - losses_{b} \\
&\quad + retainedIncome_{b}
\end{aligned}
$$

The aggregate diagnostic identity is:

$$
\begin{aligned}
DeltaBankCapital &=  \\
&\quad RetainedIncome \\
&\quad - RealizedCreditLoss \\
&\quad - BfgLevy \\
&\quad - PolishBankLevyTax \\
&\quad - UnrealizedBondLoss \\
&\quad - HtmRealizedLoss \\
&\quad - EclProvisionChange \\
&\quad - InterbankContagionLoss \\
&\quad - CapitalDestruction \\
&\quad + ReconciliationResidual \\
&\quad + WaterfallResidual
\end{aligned}
$$

where:

$$
\begin{aligned}
RealizedCreditLoss &=  \\
&\quad FirmNplLoss \\
&\quad + MortgageNplLoss \\
&\quad + ConsumerNplLoss \\
&\quad + BankHeldCorporateBondDefaultLoss
\end{aligned}
$$

`ConsumerNplLoss` is an aggregate household-credit loss diagnostic:

$$
\begin{aligned}
ConsumerNplLoss &=  \\
&\quad ConsumerLoanNetCapitalLoss \\
&\quad + ConsumerInsolvencyNetCapitalLoss \\
&\quad + LiquidityBridgeNetCapitalLoss
\end{aligned}
$$

For each product channel, diagnostics expose gross default, recovery, and
allowance draw. The loss measures are:

$$
\begin{aligned}
ExpectedLoss &= GrossDefault - Recovery \\
NetCapitalLoss &= ExpectedLoss - AllowanceDraw
\end{aligned}
$$

Current household-credit and mortgage product channels set $AllowanceDraw = 0$
by contract because no matching product-level allowance stock is built. Firm
defaults may draw the Stage 3 ECL allowance because the firm default flow is the
runtime `EclStaging.step` Stage 3 input.

`WaterfallResidual` should remain near zero. A material value means the bank
capital diagnostic surface is missing an explanatory term. `ReconciliationResidual`
is the named exactness patch distributed across live banks after aggregate bank
stocks are reconciled. `PreReconciliationResidual` is the signed capital gap
before that patch. The Monte Carlo `BankCapital_PreRecon*` columns are signed
diagnostic slots for that gap; because ordinary bank-capital terms are sourced
from the executed per-bank update, any remaining gap should normally appear in
`BankCapital_PreReconUnexplained`.

`DepositBailInLoss` is carried beside bank-capital diagnostics for resolution
analysis, but it is not an equity-capital P&L term. It is a depositor-side
deposit haircut.

## Failure, Resolution, Bail-In, And Stress Semantics

Failure detection checks active banks after ordinary banking settlement and bond
waterfall. A bank newly fails when any primary trigger applies:

$$
\begin{aligned}
NegativeCapital_{b,\tau} &\Longleftrightarrow K^{B}_{b,\tau} < 0 \\
 \\
CarBreach_{b,\tau} &\Longleftrightarrow CAR_{b,\tau} < effectiveMinCar_{b}(ccyb_{\tau}) \\
 \\
LiquidityBreach_{b,\tau} &\Longleftrightarrow LCR_{b,\tau} < 0.5 \cdot lcrMin
\end{aligned}
$$

The CAR breach trigger requires the CAR inequality to hold for three consecutive
monthly checks.

Trigger priority for diagnostics is negative capital, then CAR breach, then
liquidity breach. Failed banks have capital set to zero, which records capital
destruction through the BankCapital SFC identity.

If a primary failure occurs, interbank contagion applies counterparty losses
from failed-bank exposures. A secondary failure check then re-runs on the
post-contagion bank rows.

Bail-in is event-based:

$$
\begin{aligned}
UnprocessedDeposits_{b} &=  \\
&\quad \max(D^{B}_{b} - BailedInDeposits_{b}, 0) \\
 \\
Guaranteed_{b} &=  \\
&\quad \min(UnprocessedDeposits_{b}, bfgDepositGuarantee) \\
 \\
Uninsured_{b} &=  \\
&\quad UnprocessedDeposits_{b} - Guaranteed_{b} \\
 \\
BailInLoss_{b} &=  \\
&\quad Uninsured_{b} \cdot bailInDepositHaircut
\end{aligned}
$$

The haircut reduces deposits and is SFC-validated through the BankDeposits
identity, not through bank-equity ownership.

Purchase-and-assumption resolution then transfers failed-bank balance-sheet rows
to the healthiest surviving bank, selected by strongest CAR/capital among
risk-bearing live rows. The transferred stock families are deposits, performing
firm loans, government bonds, consumer loans, interbank position, and
corporate-bond holdings:

$$
\begin{aligned}
TransferredStocks_{b,\tau} = \{&
D^{B}_{b,\tau},
L^{B,F,\mathrm{performing}}_{b,\tau},
GB^{AFS}_{b,\tau},
GB^{HTM}_{b,\tau}, \\
&L^{B,H}_{b,\tau},
IB_{b,\tau},
CB^{B}_{b,\tau}\}
\end{aligned}
$$

The failed bank row remains an explicit inert shell: ordinary lending, P&L, ECL
migration, NPL write-off, and deposit-flow updates are skipped in later months
unless an explicit resolution or reconciliation mechanism touches that row.

If every bank has failed while deposits still need resolution, the model fails
fast. Choosing a failed bank as an absorber would implicitly introduce a bridge
bank recapitalization or nationalization mechanism. Such a mechanism must be
modeled explicitly, with fiscal/SFC flows, before all-failed resolution paths
can be treated as valid.

Normal validation profiles should not produce ordinary bank failures or
all-failed paths. Stress and exploratory diagnostics may exercise these channels
as stress semantics, but the run output must label them as such.

## Insurance, NBFI, TFI, And Quasi-Fiscal Interfaces

Insurance, NBFI/TFI, and quasi-fiscal equations are specified in
[institutional-sector-equations.md](institutional-sector-equations.md). Their
interfaces matter for bank and financial-stability analysis:

| Sector | Banking/financial interface |
| --- | --- |
| Insurance | Premiums and claims move deposits; reserve investment income uses government bonds, corporate bonds, and equity; insurance government-bond purchases participate in the bank bond waterfall. |
| TFI/NBFI | TFI fund inflows drain bank deposits; TFI government-bond demand participates in the bank bond waterfall; NBFI credit origination rises with bank NPL tightness. |
| Corporate bonds | Firm issuance can substitute for bank loans; bank holdings enter the explicit RWA perimeter with the configured corporate-bond risk weight; bank-held corporate-bond default loss hits bank capital. |
| Quasi-fiscal BGK/PFR | Quasi-fiscal bank-held bond issuance and amortization affect bank bond holdings; subsidized lending has a matching bank-deposit creation/destruction leg. |
| NBP | Reserve remuneration, standing facility, QE purchases, FX intervention, and reserve-settlement backstops connect the central bank to commercial-bank reserves and income. |

NBFI credit tightness is:

$$
\begin{aligned}
BankTightness_{\tau} &=  \\
&\quad \mathrm{clamp}((BankNplRatio_{\tau} - 0.03) / 0.03, 0, 1)
\end{aligned}
$$

and non-bank credit origination is:

$$
\begin{aligned}
NbfiOrigination_{\tau} &=  \\
&\quad NbfiLoanStock_{t} \cdot creditBaseRate \\
&\quad \cdot (1 + countercyclical \cdot BankTightness_{\tau})
\end{aligned}
$$

Quasi-fiscal issuance and absorption are:

$$
\begin{aligned}
QfIssuance_{\tau} &=  \\
&\quad \max((GovCapitalSpend_{\tau} + EuProjectCapital_{\tau}) \\
&\quad \cdot qfIssuanceShare, \\
&\quad 0) \\
 \\
QfNbpAbsorption_{\tau} &=
\begin{cases}
QfIssuance_{\tau} \cdot qfNbpAbsorptionShare, & \text{if NBP QE is active}, \\
0, & \text{otherwise},
\end{cases} \\
 \\
QfBankBondIssuance_{\tau} &=  \\
&\quad QfIssuance_{\tau} - QfNbpAbsorption_{\tau}
\end{aligned}
$$

These financial-sector satellites are aggregate institutional agents. They
affect bank deposits, securities holdings, credit aggregates, capital losses,
and SFC identities, but they are not heterogeneous commercial banks.

## SFC And Ledger Mapping

Hard accounting and SFC identities include:

| Surface | Meaning |
| --- | --- |
| BankCapital | Retained income, realized losses, provisions, valuation losses, interbank contagion, and capital destruction explain `BankState.capital` movement. |
| BankDeposits | Household, firm, JST, insurance, TFI/NBFI, quasi-fiscal, bail-in, and resolution-side deposit movements reconcile deposit liabilities. |
| InterbankNetting | Interbank position movements and contagion exposure evidence remain explicit. |
| BondClearing | Government, corporate, quasi-fiscal, NBP, PPK, insurance, TFI, foreign, and bank holder movements reconcile supported security stocks. |
| Insurance reserves | Premiums, claims, investment income, and reserve changes reconcile insurance reserve balances. |
| NbfiCredit | NBFI origination, repayment, default, and stock movement reconcile the non-bank credit book. |
| QuasiFiscal | Quasi-fiscal bonds, holder split, subsidized lending, and deposit-side credit legs reconcile public financial vehicles. |

Regulatory ratios, pricing spreads, approval probabilities, ECL migration
heuristics, and failure triggers are model rules. They become SFC-relevant only
through the realized flows or stock changes they generate.

## Output Columns And Diagnostics

Main Monte Carlo output families:

| Family | Representative columns |
| --- | --- |
| Monetary aggregates | `M0`, `M1`, `M2`, `M3`, `CreditMultiplier` |
| Rates | `InterbankRate`, `WIBOR_1M`, `WIBOR_3M`, `WIBOR_6M` |
| Regulatory ratios and portfolio composition | `MinBankCAR`, `MaxBankNPL`, `MinBankLCR`, `MinBankNSFR`, `BankGovBondShareOfAssets`, `BankPrivateCreditToGovBondHoldings` |
| Failure and resolution | `BankFailures`, `BankFailure_*`, `BankResolution_*`, `BailInLoss`, `BfgLevyTotal`, `PolishBankLevyTaxTotal` |
| Capital waterfall | `BankCapital_*`, `BankCreditLoss_*`, `BankReconciliation_*` |
| ECL | `BankEcl_*`, `EclStage1`, `EclStage2`, `EclStage3` |
| Corporate bonds | `CorpBondOutstanding`, `CorpBondYield`, `CorpBondIssuance`, `CorpBondSpread`, `BankCorpBondHoldings`, `CorpBondAbsorptionRate` |
| Insurance | `InsLifeReserves`, `InsNonLifeReserves`, `InsLifePremium`, `InsNonLifePremium`, `InsLifeClaims`, `InsNonLifeClaims` |
| NBFI/TFI | `NbfiTfiAum`, `NbfiLoanStock`, `NbfiOrigination`, `NbfiRepayment`, `NbfiDefaults`, `NbfiNetStockFlow`, `NbfiBankTightness`, `NbfiDepositDrain` |
| Quasi-fiscal | `QfBondsOutstanding`, `QfNbpHoldings`, `QfLoanPortfolio`, `QfIssuance` |

Focused diagnostics:

| Diagnostic | Role |
| --- | --- |
| [Bank balance-sheet benchmark](bank-balance-sheet-benchmark.md) | Opening capital, credit, deposit, liquidity, ECL, and concentration guardrails. |
| [Bank failure ablations](bank-failure-ablations.md) | Failure-channel and stress-driver decomposition. |
| [HH-bank lead-lag diagnostics](hh-bank-lead-lag-diagnostics.md) | Household distress and banking stress lead-lag surface. |
| [Loan origination quality diagnostics](loan-origination-quality-diagnostics.md) | Whether later bad outcomes are explained by borrower quality, bank gating, or capital stress. |
| [Nightly diagnostics](nightly-diagnostics.md) | Normal/stress/exploratory profile health summaries and failure policy. |

## Validation Coverage

| Layer | Coverage |
| --- | --- |
| Unit and property tests | Banking sector specs, bank regulatory metrics, interbank, capital semantics, monetary plumbing, NBP runtime contracts, flow emitters, insurance, NBFI, quasi-fiscal, and SFC projection specs. |
| Integration tests | Monte Carlo TSV integration, bank benchmark specs, SFC exactness tests, scenario diagnostics, and generated-output guards. |
| Generated artifacts | SFC matrix evidence, flow-mechanism semantics, and stock-flow reconciliation documents expose accounting routes. |
| Nightly diagnostics | Normal profiles should keep bank failure/all-failed semantics within expected bounds; stress/exploratory profiles report failure-channel behavior explicitly. |

## Current Limitations

- Bank capital is not holder-resolved bank equity. It is regulatory/accounting
  state, persisted on bank rows and SFC-validated through BankCapital.
- The unretained share of bank gross income has no owner-side dividend receiver
  in the current model. It is an explicit unowned outflow limitation, not a
  hidden transfer to bank shareholders.
- Deposit insurance is not modeled at individual account granularity. Bail-in
  uses an aggregate guarantee/haircut rule for newly failed bank rows.
- There is no bridge-bank, nationalization, or public recapitalization
  mechanism. All-failed unresolved-deposit paths fail fast.
- Insurance, NBFI/TFI, and quasi-fiscal vehicles are aggregate institutional
  agents. Their holder-resolution detail is intentionally coarser than the bank
  balance-sheet rows.
- ECL parameters, failure thresholds, bank benchmark bands, NBFI renewal rates,
  and non-bank portfolio rules are calibration assumptions with documented
  guardrails, not econometrically identified structural estimates.
