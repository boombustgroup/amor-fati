# Behavioral Equations And Decision Rules

This document is the paper-facing rule book for Amor Fati's current
SFC-ABM implementation. It describes implemented equations and algorithmic
rules, not a normative target model.

The document complements the canonical reviewer path in
[docs/model-specification.md](model-specification.md#reviewer-reading-path)
with rule-level detail. Its companion links are local source anchors, not a
separate top-level reading order:

- `docs/odd-model-documentation.md`, which describes the model using the ODD
  and ODD+D structure;
- `docs/sfc-matrix-evidence.md`, which documents the symbolic Balance Sheet
  Matrix (BSM), Transactions Flow Matrix (TFM), and stock-flow reconciliation
  evidence.

## Scope And Notation

The canonical publication notation and model state vector live in
[`docs/model-notation-and-state-vector.md`](model-notation-and-state-vector.md).
This document uses the same stock, flow, rate, share, stochasticity, and
runtime-ownership conventions, then adds local shorthand where it makes the
implemented rules easier to read.

All monetary quantities are PLN unless noted otherwise. Rates are annual unless
the implementation explicitly applies `.monthly`. Shares are dimensionless
values in `[0, 1]`. The runtime ledger owns supported financial stocks; agent
modules receive projections of those stocks, compute decisions, and return
closing projections plus flow variables.

Useful shorthand:

| Symbol | Meaning |
| --- | --- |
| `h` | household |
| `f` | firm |
| `b` | bank |
| `s` | production sector |
| `Y` | output or revenue proxy, depending on context |
| `K` | physical capital stock |
| `L` | labor input or loan stock, depending on context |
| `DR` | firm digital readiness |
| `NPL` | non-performing loan stock or ratio |
| `CAR` | capital adequacy ratio |
| `LCR` | liquidity coverage ratio |
| `NSFR` | net stable funding ratio |

The output-column references below refer to the Monte Carlo time-series schema
in `modules/montecarlo/src/main/scala/com/boombustgroup/amorfati/montecarlo/timeseries/McTimeseriesSchema.scala`.
Those TSV outputs are the primary numeric evidence surface for these rules.

## Timing Contract

One model month follows the deterministic same-month economics pipeline below,
then emits runtime ledger flows and validates SFC identities.
The formal $X_{t} \to X_{\tau}$ transition contract is documented in
[monthly-transition-function.md](monthly-transition-function.md).

| Module | Rule surface |
| --- | --- |
| [`engine/economics/FiscalConstraintEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/FiscalConstraintEconomics.scala) | minimum wage, reservation wage, lending base rate |
| [`engine/economics/LaborEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/LaborEconomics.scala) | wage clearing, employment, immigration, demographics |
| [`engine/economics/HouseholdIncomeEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/HouseholdIncomeEconomics.scala) and [`agents/Household.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Household.scala) | household income, consumption, saving, credit, retraining, bankruptcy |
| [`engine/economics/DemandEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/DemandEconomics.scala) | sector demand, government purchases, fiscal-rule constraint |
| [`engine/economics/FirmEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/FirmEconomics.scala) and [`agents/Firm.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Firm.scala) | production, pricing markup contribution, technology, labor, investment, defaults, entry |
| [`engine/economics/HouseholdFinancialEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/HouseholdFinancialEconomics.scala) | mortgages, deposit interest, remittances, tourism, consumer credit aggregation |
| [`engine/economics/PriceEquityEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/PriceEquityEconomics.scala) | GDP proxy, inflation, equity, macroprudential, EU funds |
| [`engine/economics/OpenEconEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/OpenEconEconomics.scala) | external sector, NBP rate, bond yield, QE, insurance, NBFI |
| [`engine/economics/BankingEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/BankingEconomics.scala), [`engine/economics/banking/BankingStepRunner.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/banking/BankingStepRunner.scala), and [`agents/Banking.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Banking.scala) | bank P&L, rates, interbank, bond waterfall, failure and resolution |

## Rule-To-Output Map

| Rule group | Main implementation anchors | Representative output columns |
| --- | --- | --- |
| Household income, consumption, savings, credit | [`agents/Household.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Household.scala), [`engine/economics/HouseholdIncomeEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/HouseholdIncomeEconomics.scala), [`engine/economics/HouseholdFinancialEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/HouseholdFinancialEconomics.scala) | `MarketWage`, `Unemployment`, `EffectivePitRate`, `ConsumerLoans`, `ConsumerOrigination`, `ConsumerApprovedOrigination`, `ConsumerCreditDemand`, `ConsumerRejectedOrigination`, `ConsumerBankRejectedOrigination`, `ConsumerCredit_RejectedPortfolioPreference`, `ConsumerDebtService`, `ConsumerPrincipal`, `ConsumerDefault`, `ConsumerLoanDefault`, `LiquidityBridgeChargeOff`, `ConsumerCredit_NetStockFlow`, `ConsumerCredit_NplRatioGross`, `ConsumerCredit_RejectedPortfolioPreferenceToDemand`, `ConsumerCredit_RejectedPortfolioPreferenceToBankRejected`, `HouseholdDistress_Current`, `HouseholdDistress_LiquidityStress`, `HouseholdDistress_Arrears`, `HouseholdDistress_Restructuring`, `HouseholdDistress_Defaulted`, `HouseholdDistress_Bankruptcy`, `HouseholdDistress_ActiveShare`, `HouseholdLiquidity_ShortfallFinancing`, `HouseholdLiquidity_NetDemandDeposit`, `HouseholdLiquidity_PositiveDemandDeposits`, `HouseholdLiquidity_ImplicitOverdraft`, `HouseholdLiquidity_NegativeDepositShare`, `HouseholdLiquidity_DepositP01`-`P99`, `SectorMobilityRate`, `VoluntaryQuits`, `DiasporaRemittanceInflow`, `RemittanceOutflow`, `TourismExport`, `TourismImport` |
| Labor, wages, demographics, social funds | [`engine/economics/LaborEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/LaborEconomics.scala), [`agents/SocialSecurity.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/SocialSecurity.scala), [`agents/EarmarkedFunds.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/EarmarkedFunds.scala) | `MarketWage`, `Unemployment`, `WorkingAgePop`, `NRetirees`, `MonthlyRetirements`, `ZusContributions`, `ZusPensionPayments`, `NfzContributions`, `NfzSpending`, `PpkContributions`, `FpContributions`, `FgspSpending` |
| Demand allocation and fiscal constraint | [`engine/economics/DemandEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/DemandEconomics.scala), [`engine/markets/FiscalRules.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/markets/FiscalRules.scala), [`engine/markets/FiscalBudget.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/markets/FiscalBudget.scala) | `GovCurrentSpend`, `GovCapitalSpendDomestic`, `FiscalRuleBinding`, `GovSpendingCutRatio`, `DebtToGdp`, `DeficitToGdp`, `PublicCapitalStock` |
| Firm production, investment, technology, financing, default, entry | [`agents/Firm.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Firm.scala), [`engine/economics/FirmEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/FirmEconomics.scala), [`engine/mechanisms/FirmEntry.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/mechanisms/FirmEntry.scala) | `TotalAdoption`, `AutoRatio`, `HybridRatio`, `Automation_TechCapex`, `Automation_TechImports`, `Automation_TechLoans`, `Automation_UpgradeFailures`, `Automation_AiDebtTrap`, `Automation_NewFullAi`, `Automation_NewHybrid`, `Adoption_MicroShare`, `Adoption_SmallShare`, `Adoption_MediumShare`, `Adoption_LargeShare`, `Adoption_CashQ1`-`Q4`, `Adoption_DebtQ1`-`Q4`, sector `*_Auto`, sector `*_Sigma`, `GrossInvestment`, `FirmCredit_NewLoans`, `FirmCredit_PrincipalRepaid`, `FirmCredit_GrossDefault`, `FirmCredit_NetStockFlow`, `FirmCredit_CreditDemand`, `FirmCredit_BankRejected`, `AggCapitalStock`, `AggInventoryStock`, `InventoryChange`, `AggEnergyCost`, `GreenInvestment`, `FirmBirths`, `FirmDeaths`, `NetEntry`, `LivingFirmCount`, `CorpBondIssuance`, `EquityIssuanceTotal` |
| Banking and monetary plumbing | [`agents/Banking.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Banking.scala), [`engine/economics/BankingEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/BankingEconomics.scala), [`engine/economics/banking/BankingStepRunner.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/banking/BankingStepRunner.scala), [`agents/EclStaging.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/EclStaging.scala), [`agents/DepositMobility.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/DepositMobility.scala), [`agents/InterbankContagion.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/InterbankContagion.scala) | `NPL`, `MinBankCAR`, `MaxBankNPL`, `MinBankLCR`, `MinBankNSFR`, `BankFailures`, `BankFailure_*`, `BankEcl_*`, `BankCreditLoss_*`, `BankReconciliation_*`, `InterbankRate`, `WIBOR_1M`, `WIBOR_3M`, `WIBOR_6M`, `BankAfsBonds`, `BankHtmBonds`, `BankGovBondShareOfAssets`, `BankPrivateCreditToGovBondHoldings`, `BfgLevyTotal`, `PolishBankLevyTaxTotal`, `BailInLoss`, `M0`, `M1`, `M2`, `M3`, `CreditMultiplier` |
| Housing and mortgages | [`engine/markets/HousingMarket.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/markets/HousingMarket.scala), [`engine/economics/banking/BankingStepRunner.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/banking/BankingStepRunner.scala) | `HousingPriceIndex`, regional `*Hpi`, `MortgageStock`, `MortgageOrigination`, `MortgageRepayment`, `MortgageDefault`, `MortgageNetStockFlow`, `MortgageOriginationToStock`, `MortgageRepaymentToStock`, `MortgageDefaultToStock`, `MortgageNetStockFlowToStock`, `MortgageOriginationSupplyConstrained`, `MortgageToGdp`, `AnnualizedGdpProxy` |
| Fiscal, NBP, bond market, external sector | [`agents/Nbp.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Nbp.scala), [`engine/markets/OpenEconomy.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/markets/OpenEconomy.scala), [`engine/economics/OpenEconEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/OpenEconEconomics.scala), [`engine/markets/CorporateBondMarket.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/markets/CorporateBondMarket.scala), [`engine/markets/BondAuction.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/markets/BondAuction.scala) | `RefRate`, `GovBondMarketYield`, `GovDebtWeightedCoupon`, `BondsOutstanding`, `NbpBondHoldings`, `ForeignBondHoldings`, `QeActive`, `NbpRemittance`, `NbpBondIncome`, `ReserveInterest`, `StandingFacilityNet`, `FxReserves`, `FxInterventionAmt`, `CurrentAccount`, `CapitalAccount`, `TradeBalance_OE`, `Exports_OE`, `TotalImports_OE`, `NFA`, `FDI` |
| Insurance, NBFI, quasi-fiscal, local government | [`agents/Insurance.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Insurance.scala), [`agents/Nbfi.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Nbfi.scala), [`agents/QuasiFiscal.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/QuasiFiscal.scala), [`agents/Jst.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Jst.scala) | `InsLifeReserves`, `InsNonLifeReserves`, `InsLifePremium`, `InsNonLifePremium`, `InsLifeClaims`, `InsNonLifeClaims`, `NbfiTfiAum`, `NbfiLoanStock`, `NbfiOrigination`, `NbfiRepayment`, `NbfiDefaults`, `NbfiNetStockFlow`, `NbfiBankTightness`, `NbfiDepositDrainToAum`, `QfBondsOutstanding`, `QfIssuance`, `QfLoanPortfolio`, `Esa2010DebtToGdp`, `JstRevenue`, `JstSpending`, `JstDebt`, `JstDeposits` |

## Household Rules

The consolidated publication-facing household-sector section lives in
[household-equations.md](household-equations.md). The material below remains
the implementation-oriented rule catalog and output-column map.

Implementation anchors:

- [`agents/Household.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Household.scala)
- [`engine/economics/HouseholdIncomeEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/HouseholdIncomeEconomics.scala)
- [`engine/economics/HouseholdFinancialEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/HouseholdFinancialEconomics.scala)

### State And Status

Each household carries:

- employment status: `Employed(firmId, sectorIdx, wage)`, `Unemployed(months)`,
  `Retraining(monthsLeft, targetSector, cost)`, or `Bankrupt`;
- behavioral traits: skill, health penalty, marginal propensity to consume
  (MPC), social-neighbor ids, education, task routineness, wage scar,
  dependent children, contract type, region, immigrant status;
- financial-distress state: `Current`, `LiquidityStress`, `Arrears`,
  `Restructuring`, `Defaulted`, or `Bankruptcy`;
- bank routing id;
- projected ledger financial stocks: non-negative demand deposit, mortgage
  loan, consumer loan, and listed equity.

### Income, Tax, Transfers

For a household `h`, monthly base income is:

$$
\begin{aligned}
baseIncome_{h} &=
\begin{cases}
wage_{h}, & \text{if employed}, \\
\mathrm{unemploymentBenefit}(months_{h}), & \text{if unemployed}, \\
0, & \text{if retraining or bankrupt}.
\end{cases}
\end{aligned}
$$

Deposit interest is paid on demand deposits using the household's bank deposit
rate when bank-specific rates are available:

$$
\begin{aligned}
depositInterest_{h} &=
\begin{cases}
demandDeposit_{h} \cdot depositRate_{b}^{\mathrm{monthly}}, & \text{if bank deposit rates are available}, \\
0, & \text{otherwise}.
\end{cases}
\end{aligned}
$$

Monthly PIT is progressive and annualized in the household module:

$$
\begin{aligned}
pitBase_{h} &= grossIncome_{h} - employeeZus_{h} \\
annualizedBase_{h} &= 12 \cdot pitBase_{h} \\
pit_{h} &= \max(\mathrm{grossTax}(annualizedBase_{h}) - annualTaxCredit, 0) / 12
\end{aligned}
$$

The current social-transfer rule is a lump-sum 800+ payment:

$$
\begin{aligned}
socialTransfer_{h} &= dependentChildren_{h} \cdot social800 \\
income_{h} &= grossIncome_{h} - pit_{h} + socialTransfer_{h}
\end{aligned}
$$

### Consumption, Saving, And Wealth Effects

Mortgage debt service separates scheduled principal repayment from variable-rate
interest. When bank-specific rates are unavailable, the fallback rate is the
policy rate plus the housing mortgage spread:

$$
\begin{aligned}
mortgageRate_{h} &=
\begin{cases}
lendingRate_{b}, & \text{if bank-specific } lendingRate_{b} \text{ exists}, \\
\mathrm{nbp.referenceRate} + \mathrm{housing.mortgageSpread}, & \text{otherwise},
\end{cases} \\
remainingMortgageMonths_{h} &=
\begin{cases}
remainingMortgageMonths_{h}, & mortgageLoan_{h} > 0 \land remainingMortgageMonths_{h} > 0, \\
\mathrm{housing.mortgageMaturity}, & \text{if } mortgageLoan_{h} > 0 \text{ and no contract state exists}, \\
0, & \text{otherwise},
\end{cases} \\
mortgagePrincipal_{h} &=
\begin{cases}
mortgageLoan_{h} / remainingMortgageMonths_{h}, & mortgageLoan_{h} > 0, \\
0, & \text{otherwise},
\end{cases} \\
mortgageInterest_{h} &= mortgageLoan_{h} \cdot mortgageRate_{h}^{\mathrm{monthly}} \\
scheduledMortgagePayment_{h} &=  \\
&\quad mortgagePrincipal_{h} + mortgageInterest_{h} \\
mortgageLoan'_{h} &= \max(mortgageLoan_{h} - mortgagePrincipal_{h}, 0) \\
remainingMortgageMonths'_{h} &=
\begin{cases}
\max(remainingMortgageMonths_{h} - 1, 1), & \text{if } mortgageLoan'_{h} > 0, \\
0, & \text{otherwise}.
\end{cases}
\end{aligned}
$$

$scheduledMortgagePayment_{h}$ is a household budget burden. It is not a single
bank-income flow: principal reduces the mortgage stock, while interest enters
bank income.

Aggregate mortgage origination is calibrated from the outstanding mortgage
book, not from the full residential-property value:

$$
\begin{aligned}
baseMortgageOrigination &= mortgageStock \cdot \mathrm{housing.originationRate}
\end{aligned}
$$

The aggregate housing-credit stock identity exposed by the timeseries is:

$$
\begin{aligned}
MortgageNetStockFlow &=  \\
&\quad MortgageOrigination - MortgageRepayment - MortgageDefault \\
MortgageToGdp &= MortgageStock / AnnualizedGdpProxy
\end{aligned}
$$

`MortgageOriginationToStock`, `MortgageRepaymentToStock`, and
`MortgageDefaultToStock` decompose mortgage-book runoff into new lending,
scheduled amortization, and gross default. `MortgageToGdp` can therefore drift
because the mortgage numerator shrinks, because `AnnualizedGdpProxy` grows, or
because both move together. In the current baseline, `BankingEconomics` calls
`HousingMarket.processOrigination` with `bankCapacity = true`, so bank failures
do not directly impose a mortgage-supply cap; if that gate is wired later,
`MortgageOriginationSupplyConstrained` is the corresponding output flag.

Immigrant households send remittances:

$$
\begin{aligned}
remittance_{h} &= income_{h} \cdot \mathrm{immigration.remitRate}
\end{aligned}
$$

The disposable budget is computed after rent, secured debt service, remittance,
and consumer-credit service:

$$
\begin{aligned}
obligations_{h} &= rent_{h} + scheduledMortgagePayment_{h} + remittance_{h} \\
disposablePreCredit_{h} &= \max(income_{h} - obligations_{h}, 0) \\
approvedConsumerLoan_{h} &= \mathrm{consumerCreditRule}(\ldots) \\
fullObligations_{h} &= obligations_{h} + consumerCreditDebtService_{h} \\
disposable_{h} &= \max(income_{h} - fullObligations_{h}, 0) \\
savingsDrawdown_{h} &= \mathrm{savingsBufferDrawdown}(\ldots) \\
consumptionBudget_{h} &= disposable_{h} + approvedConsumerLoan_{h} + savingsDrawdown_{h} \\
desiredConsumption_{h} &= mpc_{h} \cdot consumptionBudget_{h}
\end{aligned}
$$

If more than the neighbor-distress threshold of social neighbors are bankrupt
or unemployed, the household applies a precautionary consumption multiplier.
Positive equity revaluation and housing wealth effects then add to consumption.
Before residual bridge/default settlement, the household budget waterfall pays
the non-discretionary part of desired consumption up to
`household.basicConsumptionFloor`, then rent, mortgage service, remittances and
consumer-debt service, and only then the affordable discretionary part of
consumption:

$$
\begin{aligned}
basicNeed_{h} &= \min(desiredConsumption_{h}, \mathrm{household.basicConsumptionFloor}) \\
unmetBasicConsumption_{h} &= \max(basicNeed_{h} - availableCash_{h}, 0) \\
discretionaryConsumptionCompression_{h} &=  \\
&\quad \max(desiredConsumption_{h} - basicNeed_{h} - affordableDiscretionaryConsumption_{h}, 0) \\
consumption_{h} &= paidBasicConsumption_{h} + affordableDiscretionaryConsumption_{h}
\end{aligned}
$$

Household liquidity is first computed as a raw closing liquid-balance signal:

$$
\begin{aligned}
rawLiquidBalance'_{h} &=  \\
&\quad demandDeposit_{h} + income_{h} - fullObligations_{h} + approvedConsumerLoan_{h} \\
&\quad - consumption_{h} - retrainingCost_{h}
\end{aligned}
$$

The persisted demand-deposit asset is then floored at zero. Any negative raw
balance is not treated as a negative deposit. It is routed through a distinct
liquidity-shortfall financing mechanism so underwritten credit and residual
settlement remain separately auditable. Before the bridge is booked, any
remaining negative raw balance is offered to the same underwritten
consumer-credit rule up to unused DTI principal capacity. Unmet basic consumption
is deprivation, not bridge-financed debt; discretionary consumption compression
absorbs stress before $liquidityShortfallFinancing_{h}$ is created:

$$
\begin{aligned}
liquidityShortfallFinancing_{h} &= \max(-rawLiquidBalance'_{h}, 0) \\
demandDeposit'_{h} &= \max(rawLiquidBalance'_{h}, 0) \\
consumerLoan'_{h} &=  \\
&\quad consumerLoanAfterScheduledCredit_{h} \\
totalConsumerOrigination_{h} &=  \\
&\quad approvedConsumerLoan_{h}
\end{aligned}
$$

If the household enters persistent default or a personal-insolvency filing,
ordinary unsecured consumer-loan principal default is capped by current
consumer-debt arrears, debt-service multiples, and an outstanding-principal
share. The shortfall-financing leg remains a separate same-month bridge
charge-off.
`HhCcOrigination` carries only the underwritten loan; `HhLiquidityShortfallFinancing`
carries the residual settlement leg.

For diagnostics, `ConsumerDefault` reports ordinary outstanding consumer-loan
principal default. `LiquidityBridgeChargeOff` isolates the same-month bridge
write-off used to keep demand deposits non-negative.
The timeseries also exposes `ConsumerPrincipal` plus `ConsumerCredit_*`
stock-flow diagnostics so the monthly consumer-loan stock delta can be separated
into approved origination, principal repayment, and ordinary default.
`HhCcOrigination` is the underwritten consumer-loan origination channel,
`HhLiquidityShortfallFinancing` is the non-underwritten settlement channel for
residual liquidity gaps, and `LiquidityBridgeChargeOff` reports the separate
non-underwritten bridge charge-off. The bridge is excluded from the persisted
consumer-loan stock and from ordinary consumer-credit default diagnostics.
`ConsumerNplRatio` keeps the legacy performing-loan
denominator, while `ConsumerCredit_NplRatioGross` reports the same NPL stock
against a gross-book denominator. `ConsumerCredit_RejectedPortfolioPreference*`
isolates the bank-side rejection share attributable to the loan-vs-sovereign
portfolio-choice wedge after household affordability has already passed.

### MPC Adaptation

The state-dependent MPC follows a buffer-stock rule:

$$
\begin{aligned}
targetSavings_{h} &= income_{h} \cdot bufferTargetMonths \\
bufferRatio_{h} &= demandDeposit_{h} / targetSavings_{h} \\
deviation_{h} &= bufferRatio_{h} - 1 \\
bufferAdj_{h} &= \mathrm{clamp}(1 - bufferSensitivity \cdot deviation_{h}, 0, 1) \\
unemployedAdj_{h} &=
\begin{cases}
1 + mpcUnemployedBoost, & \text{if unemployed}, \\
1, & \text{otherwise},
\end{cases} \\
mpc'_{h} &= \mathrm{clamp}(mpc_{h} \cdot bufferAdj_{h} \cdot unemployedAdj_{h}, MpcFloor, MpcCeiling)
\end{aligned}
$$

Savings drawdown is more aggressive for unemployed or retraining households,
but still keeps a protected buffer floor.

### Consumer Credit

Underwritten consumer credit is available only to employed households and must
pass both household-side affordability and bank-side supply. A household is
eligible when disposable income is stressed relative to wage, the
financial-distress state still permits new credit, a stochastic eligibility draw
succeeds, and the resulting debt-service-to-income ratio has room below
`ccMaxDti`. The DTI headroom is a monthly payment-capacity constraint, so the
requested principal is computed by dividing monthly payment headroom by the
monthly consumer-credit payment factor. The household's bank then applies the
product-aware bank-credit approval engine with `ConsumerLoan` as the requested
product:

$$
\begin{aligned}
stressed_{h} &= disposablePreCredit_{h} < wage_{h} \cdot DisposableWageThreshold \\
paymentHeadroom_{h} &= income_{h} \cdot \max(ccMaxDti - existingDti_{h}, 0) \\
paymentFactor_{h} &= ccAmortRate + consumerCreditRate_{h}^{\mathrm{monthly}} \\
principalCapacity_{h} &= \min(paymentHeadroom_{h} / paymentFactor_{h}, ccMaxLoan) \\
liquidityNeed_{h} &= \max(\text{disposable-stress gap}, \text{essential-consumption gap}) \\
consumerCreditDemand_{h} &= \min(liquidityNeed_{h}, principalCapacity_{h}) \\
bankSupplyOk_{b} &= \mathrm{creditApproval}(bank_{b}, ConsumerLoan, consumerCreditDemand_{h}).approved \\
approvedConsumerLoan_{h} &=
\begin{cases}
consumerCreditDemand_{h}, & \text{if eligible and } bankSupplyOk_{b}, \\
0, & \text{otherwise},
\end{cases} \\
rejectedConsumerCreditDemand_{h} &= consumerCreditDemand_{h} - approvedConsumerLoan_{h} \\
bankRejectedConsumerCreditDemand_{h} &=
\begin{cases}
consumerCreditDemand_{h}, & \text{if eligible and not } bankSupplyOk_{b}, \\
0, & \text{otherwise},
\end{cases} \\
portfolioRejectedConsumerCreditDemand_{h} &=
\begin{cases}
bankRejectedConsumerCreditDemand_{h}, & \text{if } rejectionReason_{h} = \mathrm{portfolioPreference}, \\
0, & \text{otherwise}.
\end{cases}
\end{aligned}
$$

`Current` and first-month `LiquidityStress` households can still pass the normal
underwriting rule. `Arrears`, `Restructuring`, `Defaulted`, and `Bankruptcy`
households report credit demand where relevant, but new underwritten
consumer-credit approval is blocked before bank-side approval. Failed or
prudentially constrained banks reject otherwise eligible consumer-credit demand;
that rejected subset is reported separately as `ConsumerBankRejectedOrigination`,
with household snapshot rows carrying the bank-side rejection reason and
prudential audit ratios when available.

Liquidity-shortfall financing is not a discretionary credit decision and does
not expand the consumption budget. It is a settlement leg booked after monthly
income, obligations, approved credit, consumption, and retraining cost have
already determined the raw closing liquid balance. This keeps
$\mathrm{demandDeposit} \ge 0$ while preserving the liability-side
stock-flow identity.

Consumer-loan service separates principal repayment from interest:

$$
\begin{aligned}
consumerPrincipal_{h} &= consumerLoan_{h} \cdot ccAmortRate \\
consumerInterest_{h} &= consumerLoan_{h} \cdot (lendingRate_{b} + ccSpread)^{\mathrm{monthly}} \\
consumerDebtService_{h} &= consumerPrincipal_{h} + consumerInterest_{h} \\
consumerLoan'_{h} &= \max(consumerLoan_{h} + approvedConsumerLoan_{h} - consumerPrincipal_{h}, 0)
\end{aligned}
$$

$consumerDebtService_{h}$ is the household instalment burden used in DTI and
liquidity stress diagnostics. Only $consumerInterest_{h}$ is bank income;
$consumerPrincipal_{h}$ reduces the consumer-loan stock.

Household bankruptcy sets household equity to zero and records a bounded
ordinary consumer-loan principal default; remaining unsecured principal stays in
the consumer-loan stock for subsequent workout rather than being fully erased in
one month.

### Labor Mobility, Retraining, And Bankruptcy

Employed households may voluntarily search across sectors. The target sector is
selected from sector wage and vacancy signals, adjusted by a friction matrix.
If friction is low, the household quits into unemployment; if friction is high
and deposits cover the retraining cost, the household enters retraining.

Unemployed households become retraining-eligible after the unemployment
threshold. Retraining success depends on skill, health penalty, education, and
sectoral friction:

$$
\begin{aligned}
successProb_{h} &=  \\
&\quad retrainingBaseSuccess \cdot skill_{h} \cdot (1 - healthPenalty_{h}) \\
&\quad \cdot educationMultiplier_{h} \cdot (1 - friction \cdot frictionSuccessDiscount)
\end{aligned}
$$

Long unemployment reduces skill, increases health penalty, and accumulates a
wage scar. Re-employment decays the wage scar gradually.

Financial distress is tracked as consecutive months with savings below a
distress floor:

$$
\begin{aligned}
bankruptcyFloor_{h} &=  \\
&\quad \max(rent_{h} + scheduledMortgagePayment_{h} + consumerDebtService_{h}, rent_{h}) \\
&\quad \cdot bankruptcyThreshold
\end{aligned}
$$

The resulting financial-distress state is separate from labor-market status:

$$
\begin{aligned}
\mathrm{Current} \\
{} \to \text{LiquidityStress       after first active distress month} \\
{} \to \text{Arrears               after repeated distress} \\
{} \to \text{Defaulted             at bankruptcyDistressMonths} \\
{} \to \text{Bankruptcy            only when the personal-insolvency hazard files} \\
 \\
Arrears/Defaulted/Bankruptcy with no new distress \to Restructuring \\
Restructuring with no new distress                \to Current \\
LiquidityStress with no new distress              \to Current
\end{aligned}
$$

The state affects behavior before the next monthly budget is closed. Non-current
states apply precautionary consumption compression, bounded by the basic
consumption floor. `Arrears`, `Restructuring`, `Defaulted`, and `Bankruptcy`
also block new underwritten consumer credit. `Bankruptcy` is a personal
insolvency/write-off state reached by a household-level hazard after
`personalInsolvencyMinDistressMonths`. The hazard rises with distress duration
toward `personalInsolvencyDistressMonths` and with arrears/debt-service burden.
Distressed workout and personal-insolvency defaults are capped by debt-service
multiples and outstanding-principal shares, so remaining unsecured consumer
credit stays in stock instead of being erased solely because a counter crossed
12 months. Listed equity is still written off on a personal-insolvency filing,
but the household is not removed from the labor force. The write-off remains
SFC-consistent through `ConsumerDefault`, `ConsumerLoanDefault`, and
`LiquidityBridgeChargeOff`.

## Labor, Demographics, And Social Funds

Implementation anchors:

- [`engine/economics/FiscalConstraintEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/FiscalConstraintEconomics.scala)
- [`engine/economics/LaborEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/LaborEconomics.scala)
- [`engine/markets/LaborMarket.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/markets/LaborMarket.scala)
- [`engine/markets/RegionalClearing.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/markets/RegionalClearing.scala)
- [`agents/SocialSecurity.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/SocialSecurity.scala)
- [`agents/EarmarkedFunds.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/EarmarkedFunds.scala)

### Minimum Wage And Reservation Wage

On configured adjustment months, the minimum wage is indexed to cumulative
inflation since the last adjustment and partially converges toward a target
share of the market wage:

$$
\begin{aligned}
inflIndexed &= previousMinWage \cdot (1 + \max(cumulativeInflation, 0)) \\
targetMinWage &= marketWage \cdot minWageTargetRatio \\
newMinWage &= \max(previousMinWage, \\
&\quad inflIndexed + \max(targetMinWage - inflIndexed, 0) \\
&\quad \cdot minWageConvergenceSpeed) \\
reservationWage &= newMinWage
\end{aligned}
$$

### Wage Clearing

Labor demand is the sum of workers planned by living firms. Regional labor
clearing computes regional and national wages. The national wage is then
adjusted for expected inflation pressure and union rigidity:

$$
\begin{aligned}
wageAfterExpectations &=  \\
&\quad \max(reservationWage, \\
&\quad rawWage \cdot (1 + expWagePassthrough \\
&\quad \cdot \max(expectedInflation - targetInflation, 0) / 12)) \\
 \\
newWage &=  \\
&\quad \max(reservationWage, \\
&\quad wageAfterExpectations \\
&\quad + (previousMarketWage - wageAfterExpectations) \\
&\quad \cdot unionRigidity \cdot aggregateUnionDensity)
\end{aligned}
$$

Employment is capped by working-age population. An operational hiring-slack
factor reduces firm hiring when aggregate desired labor exceeds available labor.

### Demographics And Payroll Funds

Monthly demographics update retirements and working-age population:

$$
\begin{aligned}
retirements &= demRetirementRate \cdot employed \\
workingAgePop' &= \max(0, workingAgePop - retirements - workingAgeDecline \\
&\quad + netMigration) \\
retirees' &= retirees + retirements
\end{aligned}
$$

ZUS, NFZ, PPK, FP, and FGSP contributions are payroll-proportional, with
contract-type adjustments for household workers. ZUS and NFZ deficits are
covered by government subventions. NFZ spending is per-capita and rises with
retirees through an aging elasticity. PPK contributions create a government-bond
purchase request in the downstream bond waterfall.

Earmarked funds follow:

$$
\begin{aligned}
\mathrm{FPContrib}_{\tau} &= payroll \cdot fpRate \\
\mathrm{FPSpending}_{\tau} &= unemploymentBenefits + employed \cdot fpAlmpSpendPerWorker \\
\mathrm{PFRONContrib}_{\tau} &= \text{configured monthly revenue} \\
\mathrm{PFRONSpending}_{\tau} &= \text{configured monthly spending} \\
\mathrm{FGSPSpending}_{\tau} &= bankruptFirms \cdot avgFirmWorkers \cdot fgspPayoutPerWorker \\
\mathrm{governmentSubvention}_{\tau} &= \sum_i \max(spending_{i} - contributions_{i}, 0)
\end{aligned}
$$

## Demand, Prices, GDP, And Equity

Implementation anchors:

- [`engine/economics/DemandEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/DemandEconomics.scala)
- [`engine/economics/PriceEquityEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/PriceEquityEconomics.scala)
- [`engine/markets/FiscalRules.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/markets/FiscalRules.scala)
- [`engine/markets/PriceLevel.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/markets/PriceLevel.scala)
- [`engine/markets/EquityMarket.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/markets/EquityMarket.scala)
- [`engine/mechanisms/EuFunds.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/mechanisms/EuFunds.scala)
- [`engine/mechanisms/Macroprudential.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/mechanisms/Macroprudential.scala)

### Government Purchases And Fiscal Rules

Raw government purchases are price-indexed base spending plus an automatic
stabilizer based on unemployment above NAIRU:

$$
\begin{aligned}
unempGap &= \max(unemploymentRate - NAIRU, 0) \\
rawGovPurchases &= govBaseSpending \cdot \max(priceLevel, 1) \\
&\quad + govBaseSpending \cdot unempGap \cdot govAutoStabMult
\end{aligned}
$$

Fiscal rules then constrain spending through:

- the stabilizing expenditure rule (SRW), blending spending toward an
  inflation-plus-real-growth ceiling;
- the SGP deficit limit;
- the 55 percent debt/GDP caution threshold;
- the 60 percent debt/GDP constitutional debt ceiling.

The most restrictive rule determines `FiscalRuleBinding` and the realized
spending-cut ratio.

### Sector Demand

Sector demand is a flow-of-funds allocation of domestic consumption,
government purchases, lagged investment demand, and exports:

$$
\begin{aligned}
demand_{s} &=  \\
&\quad consWeight_{s} \cdot domesticConsumption \\
&\quad + govWeight_{s} \cdot govPurchases \\
&\quad + investWeight_{s} \cdot laggedInvestmentDemand \\
&\quad + exports_{s}
\end{aligned}
$$

Demand pressure is demand divided by nominal sector capacity. Excess demand in
capacity-constrained sectors is redistributed to sectors with slack. The
smoothed hiring signal persists across months to prevent one-month whipsaw in
firm hiring.

### GDP, Inflation, Equity, Macroprudential

The monthly GDP proxy is:

$$
\begin{aligned}
gdp &=  \\
&\quad domesticConsumption \\
&\quad + governmentDemandContribution \\
&\quad + euProjectContribution \\
&\quad + exports \\
&\quad + domesticGrossFixedCapitalFormation \\
&\quad + inventoryChange
\end{aligned}
$$

Inflation combines price-level dynamics from expected inflation, demand,
wage growth, and exchange-rate deviation, plus firm markup inflation.

The equity market updates from reference rate, inflation, GDP growth, and firm
profits. New listed equity issuance is added after the index update. Dividends
are split into domestic, foreign, government, and tax components using foreign
ownership and state-owned-firm profit signals.

Macroprudential policy updates the credit-to-GDP gap and countercyclical
capital buffer, which feeds bank approval and failure thresholds.

EU funds follow a programmed monthly absorption path. Domestic co-financing and
the capital share of the project envelope feed government demand and public
capital.

## Firm Rules

The consolidated publication-facing firm-sector section lives in
[firm-equations.md](firm-equations.md). The material below remains the
implementation-oriented rule catalog and output-column map.

Implementation anchors:

- [`agents/Firm.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Firm.scala)
- [`engine/economics/FirmEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/FirmEconomics.scala)
- [`engine/markets/IntermediateMarket.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/markets/IntermediateMarket.scala)
- [`engine/markets/CorporateBondMarket.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/markets/CorporateBondMarket.scala)
- [`engine/mechanisms/FirmEntry.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/mechanisms/FirmEntry.scala)
- [`agents/StateOwned.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/StateOwned.scala)

### Technology State And Capacity

Firm technology is one of:

- traditional, with explicit worker count;
- hybrid, with worker count and AI efficiency;
- automated, with AI efficiency and skeleton crew;
- bankrupt, an inactive state.

Capacity starts from sector revenue multipliers and firm-size scaling. The
labor-effective component is:

$$
\begin{aligned}
laborEff &=
\begin{cases}
workers / initialSize, & \text{if traditional}, \\
0.4 \cdot workers / initialSize + 0.6 \cdot aiEfficiency, & \text{if hybrid}, \\
aiEfficiency, & \text{if automated}, \\
0, & \text{if bankrupt}.
\end{cases}
\end{aligned}
$$

When physical capital is active, capacity uses a CES production function:

$$
\begin{aligned}
capacity_{f} &=  \\
&\quad baseRevenue \cdot sizeScale_{f} \cdot sectorRevenueMultiplier_{s} \\
&\quad \cdot \mathrm{CES}(K_{f} / targetK_{f}, laborEff_{f}, \sigma_{s})
\end{aligned}
$$

Near the Cobb-Douglas boundary, the CES helper degrades to a Cobb-Douglas
form. Sector sigma also enters technology-adoption thresholds and evolves by
learning-by-doing in `PriceEquityEconomics`.

### Monthly P&L

Revenue is capacity times sector demand and the price level:

$$
\begin{aligned}
revenue_{f} &= priceLevel \cdot capacity_{f} \cdot sectorDemandMultiplier_{s}
\end{aligned}
$$

Costs include labor, residual domestic operating costs, depreciation, AI or
hybrid maintenance, bank and corporate-bond interest, inventory carrying cost,
energy and ETS cost, and foreign-owned profit shifting:

$$
\begin{aligned}
profitBeforeTax_{f} &=  \\
&\quad revenue_{f} \\
&\quad - laborCost_{f} \\
&\quad - otherCosts_{f} \\
&\quad - depreciation_{f} \\
&\quad - aiMaintenance_{f} \\
&\quad - interest_{f} \\
&\quad - inventoryCost_{f} \\
&\quad - energyCost_{f} \\
&\quad - profitShiftCost_{f}
\end{aligned}
$$

CIT uses loss carryforward. Losses accumulate when profit is negative; positive
profit can offset accumulated losses up to the configured share, and remaining
losses decay gradually.

### Hiring, Firing, And Labor Matching

Traditional firms compute desired workers by searching for the largest
headcount where marginal revenue exceeds wage cost:

$$
\begin{aligned}
\mathrm{MR}(workers) &= (\mathrm{capacity}(workers) - \mathrm{capacity}(workers - 1)) \\
&\quad \cdot demandMultiplier_{s} \cdot priceLevel
\end{aligned}
$$

Desired workers are the largest headcount where
$\mathrm{MR}(workers) > wageCost_{s}$.

The target is bounded by minimum retained workers, a multiple of initial size,
aggregate labor slack, hiring-signal persistence, monthly hiring headroom, and
liquidity constraints. Actual monthly adjustment closes only a fraction of the
gap. Downsizing pays severance and can still lead to bankruptcy if solvency is
not restored.

`FirmEconomics.processLaborMarket` separates displaced workers, matches
unemployed workers to vacancies by skill and region, updates wages, removes
return migrants, and adds new immigrant households.

### Technology Adoption

Traditional firms evaluate full-AI and hybrid upgrade candidates. A candidate
is feasible only if:

- estimated post-upgrade costs clear the profitability threshold;
- the firm can pay the down payment;
- digital readiness exceeds the path-specific minimum;
- the relationship bank can lend the required amount.

Adoption probabilities combine risk profile, digital readiness, local
neighbor adoption, global adoption pressure, loss-making desperation, strategic
early adoption, and an adoption-willingness ramp. Hybrid firms may later
upgrade to full automation.

Implementation failures can be catastrophic or partial. Failed upgrades impose
partial capex, loan, and down-payment costs; catastrophic failure bankrupts the
firm.

### Investment, Green Capital, Inventories, FDI, Informality

Physical capital investment follows depreciation plus partial adjustment toward
a sector target capital-labor ratio:

$$
\begin{aligned}
desiredInvestment_{f} &=  \\
&\quad depreciation_{f} \\
&\quad + \max(targetK_{f} - postDepreciationK_{f}, 0) \cdot capitalAdjustSpeed \\
targetDebtNeed_{f} &= desiredInvestment_{f} \cdot investmentDebtTargetShare \\
shortfallDebtNeed_{f} &=  \\
&\quad \max(desiredInvestment_{f} - \max(cash_{f}, 0), 0) \cdot investmentCreditShare \\
creditNeed_{f} &= \min(\max(targetDebtNeed_{f}, shortfallDebtNeed_{f}), desiredInvestment_{f}) \\
cashInvestment_{f} &=  \\
&\quad \min(desiredInvestment_{f} - approvedCredit_{f}, \max(cash_{f}, 0)) \\
actualInvestment_{f} &= \min(cashInvestment_{f} + approvedCredit_{f}, desiredInvestment_{f})
\end{aligned}
$$

The target-debt leg prevents cash-rich firms from mechanically self-financing
all investment before approaching their relationship bank. If the target-debt
request is rejected and the firm still has enough cash, investment can proceed
with internal funds; rejected credit remains visible in diagnostics.

The timeseries exposes `FirmCredit_*` diagnostics for the stock-flow and
financing surface. `FirmCredit_NewLoans`, `FirmCredit_PrincipalRepaid`,
`FirmCredit_GrossDefault`, `FirmCredit_NplRecovery`, and
`FirmCredit_NetStockFlow` decompose the monthly bank-firm-loan book flow. The
`Investment*` columns split physical-investment credit demand, approval, bank
rejection, and cash-financed investment. Demand and approval diagnostics are
measured before equity and corporate-bond channel substitution; final bank-loan
origination is `FirmCredit_NewLoans`. The `Tech*` columns perform the same audit
for automation and hybrid-upgrade credit; candidate diagnostics include
otherwise feasible rejected candidates and should not be read as selected-loan
approval rates.

Green investment mirrors this logic with green capital-labor targets, green
depreciation, and a separate share of available cash.

Inventory changes combine unsold production, spoilage, adjustment toward a
target inventory ratio, and stress liquidation when cash is negative:

$$
\begin{aligned}
unsoldValue_{f} &= \max(productionValue_{f} - salesValue_{f}, 0) \\
targetInventory_{f} &= revenue_{f} \cdot inventoryTargetRatio_{s} \\
rawInventoryChange_{f} &=  \\
&\quad unsoldValue_{f} \\
&\quad + (targetInventory_{f} - postSpoilageInventory_{f}) \cdot inventoryAdjustSpeed
\end{aligned}
$$

Foreign-owned firms shift a share of positive gross profit and repatriate a
share of after-tax profit subject to available cash. Informal economy logic
reduces paid CIT by a sector shadow share times the CIT evasion rate.

### Financing, Default, Entry, Exit

New technology loan demand is split through financing channels:

```text
listed equity issuance first for large firms
corporate-bond issuance next for medium and large firms
bank loan for the remainder
```

Corporate bond absorption depends on the corporate-bond market state and bank
capital. Unsold bond issuance reverts to bank loans.

Newly bankrupt firms generate bank NPLs and corporate-bond defaults. Bank loan
loss is:

$$
\begin{aligned}
nplLoss &= newNplDebt \cdot (1 - loanRecovery)
\end{aligned}
$$

Firm entry replaces bankrupt slots and can add net entry when macro conditions
allow. Sector entry weights depend on profit signals and entry barriers. New
AI-native entrants become possible once aggregate adoption crosses the entry
threshold and a probability draw succeeds.

## Banking Rules

The consolidated publication-facing banking and financial-stability section
lives in [banking-and-financial-sector-equations.md](banking-and-financial-sector-equations.md).
The material below remains the implementation-oriented banking rule catalog.

Implementation anchors:

- [`agents/Banking.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Banking.scala)
- [`engine/economics/BankingEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/BankingEconomics.scala)
- [`engine/economics/banking/BankingStepRunner.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/banking/BankingStepRunner.scala)
- [`agents/EclStaging.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/EclStaging.scala)
- [`agents/DepositMobility.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/DepositMobility.scala)
- [`agents/InterbankContagion.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/InterbankContagion.scala)
- [`engine/ledger/CorporateBondOwnership.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/ledger/CorporateBondOwnership.scala)

### Balance-Sheet Ratios

Aggregate and per-bank diagnostics are computed from operational bank state and
ledger-owned financial stocks.

Risk-weighted assets are:

$$
\begin{aligned}
RWA_{b} &= firmLoans_{b} + consumerLoans_{b} + 0.50 \cdot corporateBondHoldings_{b} \\
CAR_{b} &= capital_{b} / RWA_{b}
\end{aligned}
$$

When the denominator is effectively zero, the implementation returns a safe
ratio floor.

Opening IFRS 9 ECL staging is seeded from the bank-owned firm and consumer loan
book as all-performing Stage 1 exposure. The opening allowance is therefore a
pre-existing baseline allowance, while `BankCapital_EclProvisionChange` measures
monthly movement in that allowance. Stage 1 to Stage 2 migration is driven by
stress deterioration, not by the absolute opening unemployment level. The
unemployment trigger uses the increase over the carried reference unemployment
rate, normally the previous month's pipeline value bootstrapped from the model
start Poland baseline, plus any same-month GDP contraction:

$$
\begin{aligned}
eclMigrationRate &=  \\
&\quad eclMigrationSensitivity \cdot \max(0, unemployment_{t} - unemployment_{ref}) \\
&\quad + eclGdpSensitivity \cdot \max(0, -gdpGrowth_{t})
\end{aligned}
$$

The result is clamped by `banking.eclMaxMigration`. A stable baseline can remain
above NAIRU without mechanically migrating the loan book to Stage 2, while
unemployment deterioration and GDP contraction still raise lifetime-ECL
provisions.

The monthly bank-capital diagnostic waterfall is:

$$
\begin{aligned}
deltaCapital &=  \\
&\quad retainedIncome \\
&\quad - realizedCreditLoss \\
&\quad - bfgLevy \\
&\quad - polishBankLevyTax \\
&\quad - unrealizedBondLoss \\
&\quad - htmRealizedLoss \\
&\quad - eclProvisionChange \\
&\quad - interbankContagionLoss \\
&\quad - capitalDestruction \\
 \\
waterfallResidual &=  \\
&\quad observedDeltaCapital \\
&\quad - deltaCapital \\
&\quad - reconciliationResidual \\
 \\
realizedCreditLoss &=  \\
&\quad firmNplLoss \\
&\quad + mortgageNplLoss \\
&\quad + consumerNplLoss \\
&\quad + bankHeldCorporateBondDefaultLoss
\end{aligned}
$$

`BankCapital_InterbankContagionLoss` is reported as its own waterfall term:
failed-bank interbank exposures reduce exposed counterparty bank capital before
secondary failure checks. It is not routed through
`BankCapital_ReconciliationResidual`.

`BankCapital_ReconciliationResidual` is reported separately as an exactness
correction to the per-bank allocation. `BankCapital_PreReconciliationResidual`
is the signed capital gap before that correction. `BankCapital_PreRecon*`
columns are signed diagnostic slots for that gap; because ordinary bank-capital
terms are sourced from the executed per-bank update, any remaining gap should
normally appear in `BankCapital_PreReconUnexplained`. `BankCapital_WaterfallResidual`
is the remaining unexplained capital delta after that correction and should
remain near zero unless a diagnostic term is missing.
`BankReconciliation_*` columns then inspect the most impacted bank row after the
sector residual is distributed across live banks. They report the target bank id,
capital before/after, CAR before/after, the allocated residual as a share of
pre-patch capital, and whether the residual is material. A residual is marked
material when the most impacted allocation is at least 1 bp of that bank's
pre-patch capital.
`BankReconciliation_CrossedFailureThreshold` is `1` only when the patch moves
any bank from no failure trigger to a post-patch failure trigger; the post-patch
reason code uses the same `0..5` reason-code mapping as
`BankFailure_FirstNewReasonCode`. When the patch creates post-residual failure
reasons, the banking stage runs a final failure, bail-in, and P&A resolution pass
before month close; an active bank must not finish the month with
residual-induced negative capital or a valid failure
trigger.
`BankCapital_DepositBailInLoss` is also reported for resolution analysis, but it
is a depositor haircut rather than an equity-capital P&L term.

Every production writer of `BankState.capital` is classified in
`engine.economics.banking.BankCapitalSemantics.writeSites`. The guardrail test fails when a new writer
appears without an SFC category and absorber statement. `BankCapital_ReconciliationResidual`
is the only named residual writer; all other capital changes must be assigned
to an opening calibration, ordinary P&L, provision, valuation, contagion,
or failure-destruction category.

Realized credit-loss rates are emitted as `BankCreditLoss_*`. They use closing
exposure stocks as denominators, so the columns can be joined directly with
monthly `BankFailure_*` rows. The block separates firm-loan, mortgage,
consumer-loan, liquidity-bridge charge-off, and bank-held corporate-bond
channels. `BankCapital_ConsumerNplLoss` is the capital loss on ordinary
consumer-loan defaults only; same-month liquidity bridge charge-offs stay
visible as a separate gross stress/write-off diagnostic instead of being folded
into ordinary consumer-credit NPL loss. `BankCapital_EclProvisionChange` remains
a separate accounting provision term and is not included in these realized-loss
rates.

Liquidity ratios are:

$$
\begin{aligned}
HQLA_{b} &= reserves_{b} + govBondAfs_{b} + govBondHtm_{b} \\
netCashOutflows_{b} &= demandDeposits_{b} \cdot demandDepositRunoff \\
LCR_{b} &= HQLA_{b} / netCashOutflows_{b} \\
 \\
ASF_{b} &= capital_{b} + termDeposits_{b} \cdot 0.95 + demandDeposits_{b} \cdot 0.90 \\
RSF_{b} &=  \\
&\quad shortLoans_{b} \cdot 0.50 \\
&\quad + mediumLoans_{b} \cdot 0.65 \\
&\quad + longLoans_{b} \cdot 0.85 \\
&\quad + govBonds_{b} \cdot 0.05 \\
&\quad + corporateBonds_{b} \cdot 0.50 \\
NSFR_{b} &= ASF_{b} / RSF_{b}
\end{aligned}
$$

### Loan Pricing And Approval

Household deposit rates are the reference rate minus the household deposit
spread, floored at zero.

Firm lending rates first form a pre-portfolio private-credit rate:

$$
\begin{aligned}
preLoanRate_{b} &=  \\
&\quad referenceOrWiborRate \\
&\quad + baseSpread \\
&\quad + bankSpecificSpread_{b} \\
&\quad + \min(NPL_{b}(q) \cdot nplSpreadFactor, NplSpreadCap) \\
&\quad + capitalShortfallPenalty_{b}
\end{aligned}
$$

Capital pressure is measured against the same management target used by
quantity rationing:

$$
\begin{aligned}
managementCAR_{b} &= effectiveMinCar_{b}(ccyb) + creditManagementCarBuffer \\
capitalShortfallPenalty_{b} &=  \\
&\quad \max(managementCAR_{b} - CAR_{b}, 0) \cdot creditCarShortfallPenaltyScale \\
 \\
approvalThrottle_{b}(q, A) &=  \\
&\quad \mathrm{clamp}((ProjectedCAR_{b}(q, A) - effectiveMinCar_{b}(ccyb)) / \\
&\quad creditManagementCarBuffer, 0, 1)
\end{aligned}
$$

The government-bond channel is a portfolio-choice wedge. Banks compare the
risk-adjusted return on private credit with the sovereign return; sovereign
holdings are never subtracted from a loanable-funds capacity:

$$
\begin{aligned}
loanReturnRA_{b}(q, A) &=  \\
&\quad preLoanRate_{b} \\
&\quad - expectedLossCost_{b}(q) \\
&\quad - capitalCost_{b}(q) \\
&\quad - marginalPolishBankLevyCost_{b}(q, A) \\
 \\
bondReturnRA_{b} &= govBondMarketYield \\
wedge_{b}(q, A) &= loanReturnRA_{b}(q, A) - bondReturnRA_{b} \\
 \\
portfolioPricePremium_{b}(q, A) &=  \\
&\quad \max(-wedge_{b}(q, A), 0) \cdot portfolioWedgePriceShare \\
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

The final firm loan rate adds $portfolioPricePremium_{b}(q, A)$ to the
pre-portfolio rate, evaluated for pricing at $(q, A) = (\mathrm{firm}, 0)$.
Failed banks receive a fixed penalty spread and cannot lend.

Credit approval projects RWA into the requested product bucket, then requires
the projected CAR above the macroprudential effective minimum and LCR/NSFR above
regulatory minima. Banks that pass the hard gates still use
$approvalThrottle_{b}(q, A) \cdot portfolioThrottle_{b}(q, A)$ as the quantity throttle. NPL pressure
affects credit supply through risk pricing, IFRS 9 / ECL provisioning, and the
resulting capital path rather than through an independent approval-probability
penalty.
Reserve requirements are not a per-loan approval gate; reserves and government
bonds affect credit supply through LCR/NSFR, settlement cost, standing
facilities, and lending spreads.

### Interbank, Facilities, And Monetary Aggregates

The interbank rate is a corridor rate between the deposit facility and lombard
facility. It rises with aggregate credit stress and scarce excess reserves:

$$
\begin{aligned}
creditStress &= \mathrm{clamp}(aggregateNplRatio / stressThreshold, 0, 1) \\
liquidityRatio &= \mathrm{clamp}(excessReserves / requiredReserves, 0, 1) \\
interbankRate &= depositFacilityRate \\
&\quad + (1 - liquidityRatio) \cdot creditStress \\
&\quad \cdot (lombardRate - depositFacilityRate)
\end{aligned}
$$

Interbank clearing matches lenders with excess reserves to borrowers with
reserve deficits. A hoarding factor reduces lending when system NPL stress is
high.

Monetary aggregates are:

$$
\begin{aligned}
M0 &= \text{bank reserves at NBP} \\
M1 &= \text{demand deposits} \\
M2 &= demand deposits + term deposits \\
M3 &= M2 + TFI AUM + corporate bonds outstanding \\
creditMultiplier &= M2 / M0
\end{aligned}
$$

### Bank P&L, Provisioning, Failure, Resolution

Bank capital changes by losses plus retained income:

$$
\begin{aligned}
losses_{b} &=  \\
&\quad corporateNplLoss_{b} \\
&\quad + mortgageNplLoss_{b} \\
&\quad + ordinaryConsumerLoanNplLoss_{b} \\
&\quad + corporateBondDefaultLoss_{b} \\
&\quad + BFGLevy_{b} \\
&\quad + PolishBankLevyTax_{b} \\
&\quad + unrealizedAfsBondLoss_{b} \\
 \\
grossIncome_{b} &=  \\
&\quad firmLoanInterest_{b} \\
&\quad + govBondIncome_{b} \\
&\quad - depositInterest_{b} \\
&\quad + reserveInterest_{b} \\
&\quad + standingFacilityIncome_{b} \\
&\quad + interbankInterest_{b} \\
&\quad + mortgageInterestIncome_{b} \\
&\quad + consumerInterestIncome_{b} \\
&\quad + corporateBondCoupon_{b} \\
 \\
capital'_{b} &= capital_{b} - losses_{b} + grossIncome_{b} \cdot profitRetention
\end{aligned}
$$

IFRS 9 ECL staging adds provision changes based on the performing book, new
defaults, unemployment, and GDP growth. `EclStage1`, `EclStage2`, and
`EclStage3` report the aggregate staged exposure stock. `BankEcl_*` diagnostics
then separate opening allowance, closing allowance, the closing-book allowance
under an all-Stage-1 baseline for the staged ECL book, and the allowance above
that baseline. This allows bank-failure analysis to distinguish accounting
provision build-up from realized credit losses reported in `BankCreditLoss_*`.

Failure is triggered by negative capital, three consecutive months below
effective minimum CAR, or an LCR breach below half of the minimum. BFG bail-in
is event-based: it haircuts only the unprocessed uninsured deposit stock of
banks entering resolution in the current failure event set. P&A resolution then
transfers deposits, government bonds, performing loans, consumer loans, and
corporate-bond holdings to the healthiest surviving bank. Firms and households
routed to failed banks are reassigned to the absorber.
If every bank has failed while deposits still need resolution, the baseline
path fails fast. Selecting a failed row as the absorber would be an implicit
bridge-bank recapitalization or nationalization mechanism, so it must be added
as an explicit SFC/fiscal mechanism before such runs are considered valid.
In later months, a failed-bank row is an inert shell: ordinary lending, P&L,
ECL migration, NPL write-off, and deposit-flow updates are skipped unless an
explicit resolution or reconciliation mechanism changes the row.

Resolution-count diagnostics are emitted as `BankResolution_*` seed timeseries
columns. They report active bank rows, failed bank rows, newly failed banks,
distinct event-based bail-in entries, P&A-resolved bank rows, the stable
all-failed fallback flag, explicit bridge recapitalization amount, and an
invalid-active-bank invariant flag. `BankResolution_BridgeRecapitalization`
remains zero until a bridge or nationalization mechanism is implemented.
`BankResolution_InvalidActiveBankInvariant` should remain zero and means an
active bank ended the month with negative capital.

Failure-trigger diagnostics are emitted as `BankFailure_*` seed timeseries
columns. `BankFailure_NewNegativeCapital`, `BankFailure_NewCarBreach`, and
`BankFailure_NewLiquidityBreach` count newly failed banks by primary trigger; if
multiple triggers are true, the priority is negative capital, then CAR breach,
then LCR/liquidity breach. `BankFailure_FirstNewReasonCode` records the first
new failure reason in bank-id order using `0 = none`, `1 = negative capital`,
`2 = CAR breach`, `3 = LCR/liquidity breach`, `4 = all-failed fallback`, and
`5 = invariant mismatch`. `BankFailure_AllFailedFallback` is retained as a
stable diagnostic column and should remain zero under fail-fast all-failed
semantics. `BankFailure_InvariantViolation` should remain zero and means the
failure-event diagnostics do not reconcile to the new-failure count.

## Fiscal, Monetary, Bond-Market, And External Rules

The consolidated publication-facing institutional-sector section lives in
[institutional-sector-equations.md](institutional-sector-equations.md). The
material below remains the implementation-oriented fiscal, monetary,
bond-market, and external rule catalog.

Implementation anchors:

- [`engine/markets/FiscalBudget.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/markets/FiscalBudget.scala)
- [`engine/markets/FiscalRules.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/markets/FiscalRules.scala)
- [`agents/Nbp.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Nbp.scala)
- [`engine/markets/OpenEconomy.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/markets/OpenEconomy.scala)
- [`engine/economics/OpenEconEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/OpenEconEconomics.scala)
- [`engine/markets/BondAuction.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/markets/BondAuction.scala)
- [`engine/markets/CorporateBondMarket.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/markets/CorporateBondMarket.scala)
- [`engine/mechanisms/Expectations.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/mechanisms/Expectations.scala)

### Government Budget And Debt

Government budget revenue includes CIT/PIT-related revenue, VAT, net NBP
fiscal remittance, excise, customs, and state-owned-firm dividends. Spending includes
unemployment benefits, social transfers, current government purchases, capital
government purchases, debt service, social-fund subventions, earmarked-fund
subventions, and EU co-financing.

$$
\begin{aligned}
totalSpend &=  \\
&\quad unemploymentBenefits \\
&\quad + socialTransfers \\
&\quad + govCurrentSpend \\
&\quad + govCapitalSpend \\
&\quad + debtService \\
&\quad + ZUSSubvention \\
&\quad + NFZSubvention \\
&\quad + earmarkedFundSubvention \\
&\quad + euCofinancing \\
 \\
totalRevenue &=  \\
&\quad taxRevenue + governmentDividendRevenue \\
 \\
deficit &= totalSpend - totalRevenue \\
cumulativeDebt' &= cumulativeDebt + deficit \\
govBondOutstanding' &= \max(govBondOutstanding + deficit, 0)
\end{aligned}
$$

Public capital depreciates monthly and increases with domestic government
capital spending plus EU project capital.

The weighted coupon follows a rolling weighted-average-maturity rule. Each
month, a fraction of the outstanding portfolio matures and new deficit issuance
enters at the current market yield.

### NBP Policy, Bond Yield, QE, FX

The reference rate follows a smoothed Taylor-type rule:

$$
\begin{aligned}
policyInflation &=  \\
&\quad expectedInflationWeight \cdot expectedInflation \\
&\quad + (1 - expectedInflationWeight) \cdot inflation \\
 \\
taylorTarget &=  \\
&\quad neutralRate \\
&\quad + taylorAlpha \cdot (policyInflation - targetInflation) \\
&\quad - taylorDelta \cdot outputGap \\
&\quad + taylorBeta \cdot exchangeRateChange \\
 \\
referenceRate' &=  \\
&\quad \mathrm{clamp}(previousRate \cdot inertia + taylorTarget \cdot (1 - inertia), \\
&\quad rateFloor, \\
&\quad rateCeiling, \\
&\quad maxMonthlyChange)
\end{aligned}
$$

Government bond yield is:

$$
\begin{aligned}
annualGdp &= 12 \cdot monthlyGdp \\
 \\
foreignDemandDiscount_{\tau} &=
\begin{cases}
\mathrm{foreignDemandDiscount}, & \text{if } NFA > 0, \\
0, & \text{otherwise},
\end{cases} \\
 \\
govBondMarketYield &=  \\
&\quad \max(referenceRate + termPremium, bundYield + termPremium) \\
&\quad + \mathrm{fiscalRisk}(debtToGdp) \\
&\quad - \mathrm{qeCompression}(qeCumulative / annualGdp) \\
&\quad - foreignDemandDiscount_{\tau} \\
&\quad + credibilityPremium
\end{aligned}
$$

QE activates near the lower bound when realized or expected inflation is below
target by the configured threshold. QE purchases are requested by NBP but
settled through the bond waterfall, so actual sold bonds leave banks and enter
NBP holdings exactly. The requested purchase is capped against the same
annualized GDP basis used by bond-market ratios:

$$
\begin{aligned}
qeRequest &=  \\
&\quad \min(\max(qeMaxGdpShare \cdot annualGdp - nbpGovBondHoldings, 0), \\
&\quad bankGovBondHoldings, \\
&\quad qePace)
\end{aligned}
$$

The SGP fiscal correction applies to discretionary government purchases after
subtracting lagged non-purchase outlays from the 3% deficit path:

$$
\begin{aligned}
prevNonPurchaseSpend &=  \\
&\quad \max(prevRevenue + prevDeficit - prevGovSpend, 0) \\
 \\
maxDiscretionarySpend &=  \\
&\quad \max(prevRevenue + monthlyGdp \cdot sgpDeficitLimit - prevNonPurchaseSpend, 0) \\
 \\
deficitOvershootScale &=  \\
&\quad \max(deficitToGdp / sgpDeficitLimit, 1) \\
 \\
sgpMonthlyCorrection &=  \\
&\quad \min((sgpCorrectionSpeed / 12) \cdot deficitOvershootScale, 1) \\
 \\
sgpAdjustedSpend &=  \\
&\quad spending - (spending - maxDiscretionarySpend) \cdot sgpMonthlyCorrection
\end{aligned}
$$

FX intervention buys or sells EUR when the exchange rate leaves the tolerance
band around the base rate. EUR purchases inject PLN reserves; EUR sales drain
reserves. The intervention also contributes a shock term to the exchange-rate
update.

### External Sector

Exports come from GVC sector exports when available; otherwise they follow
foreign GDP growth, real exchange-rate competitiveness, and automation-related
unit-labor-cost effects. GVC sector exports also include a domestic export
capacity term: each sector is anchored on its first observed real output, and
subsequent export demand realization scales with the sector's real-output ratio
using `gvc.exportCapacityElasticity`. This keeps the empirical opening export
base intact while allowing exports to move with domestic supply capacity over a
multi-year baseline run.

Imports include household consumption imports, technology and investment
imports, imported intermediates, and tourism imports. Imported intermediates
depend on sector output, sector import content, and nominal exchange-rate
effects.

The current account is:

$$
\begin{aligned}
openEconomyCurrentAccount &= tradeBalance + primaryIncome + secondaryIncome \\
primaryIncome &= NFA \cdot nfaReturnRate^{monthly} \\
secondaryIncome &= EUFunds - remittanceOutflow + diasporaInflow
\end{aligned}
$$

The final exported BoP then applies firm/equity owner outflows:

$$
\begin{aligned}
CurrentAccount &=  \\
&\quad TradeBalance_{OE} \\
&\quad + CurrentAccountPrimaryIncome \\
&\quad + CurrentAccountSecondaryIncome \\
&\quad - ForeignDividendOutflow \\
&\quad - FdiRepatriation \\
&\quad + CurrentAccountClosureResidual
\end{aligned}
$$

`TradeBalance_OE` is the post-adjustment trade balance; `FdiProfitShifting` has
already been booked into it as an imported service, so `FdiGrossOutflow` is not
the right subtraction term for closing the exported current account.

The capital account combines FDI, ordinary portfolio flows, carry-trade flows,
and stress capital-flight outflows. FDI rises with automation and falls with a
negative-NFA dampening term. Portfolio flows react to the domestic-foreign rate
differential and NFA risk premium.

Exchange-rate change responds to the balance-of-payments ratio, negative-NFA
risk, FX intervention shock, and PPP drift:

$$
\begin{aligned}
exchangeRateShock &=  \\
&\quad exRateAdjSpeed \cdot (-(CA + KA) / GDP + nfaRisk) \\
&\quad + fxInterventionShock \\
&\quad + pppDrift
\end{aligned}
$$

NFA updates by the current account plus a partial exchange-rate valuation effect
on foreign assets.

## Insurance, NBFI, Quasi-Fiscal, And JST Rules

The consolidated publication-facing institutional-sector section lives in
[institutional-sector-equations.md](institutional-sector-equations.md). The
material below remains the implementation-oriented insurance, NBFI,
quasi-fiscal, and JST rule catalog.

Implementation anchors:

- [`agents/Insurance.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Insurance.scala)
- [`agents/Nbfi.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Nbfi.scala)
- [`agents/QuasiFiscal.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/QuasiFiscal.scala)
- [`agents/Jst.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/agents/Jst.scala)
- [`engine/economics/OpenEconEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/OpenEconEconomics.scala)
- [`engine/economics/BankingEconomics.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/BankingEconomics.scala)
- [`engine/economics/banking/BankingStepRunner.scala`](../modules/model/src/main/scala/com/boombustgroup/amorfati/engine/economics/banking/BankingStepRunner.scala)

### Insurance

Life and non-life premiums are proportional to the employed wage bill:

$$
\begin{aligned}
lifePremium &= employed \cdot wage \cdot lifePremiumRate \\
nonLifePremium &= employed \cdot wage \cdot nonLifePremiumRate
\end{aligned}
$$

Life claims follow a loss ratio. Non-life claims widen with unemployment above
the non-life unemployment threshold:

$$
\begin{aligned}
nonLifeClaims &=  \\
&\quad nonLifePremium \cdot nonLifeLossRatio \\
&\quad \cdot (1 + \max(unemploymentRate - threshold, 0) \cdot nonLifeUnempSensitivity)
\end{aligned}
$$

Investment income comes from government bonds, corporate bonds, and equity,
minus corporate-bond default loss. Reserves update from premiums, claims, and
investment income. Government-bond and equity holdings rebalance gradually
toward target allocation shares.

### NBFI And TFI

TFI inflow is proportional to wage bill and excess fund return over deposits:

$$
\begin{aligned}
baseInflow &= employed \cdot wage \cdot tfiInflowRate \\
fundReturn &=  \\
&\quad govBondMarketYield \cdot tfiGovBondShare \\
&\quad + \mathrm{equityReturn.annualized} \cdot tfiEquityShare \\
&\quad + govBondMarketYield \cdot tfiCorpBondShare \\
netInflow &= baseInflow \cdot (1 + \mathrm{clamp}(fundReturn - depositRate, cap) \\
&\quad \cdot ExcessReturnSensitivity)
\end{aligned}
$$

TFI AUM updates by net inflow and investment income, then rebalances toward
government-bond and equity target shares. Net TFI inflow is recorded as deposit
drain from the banking system.

NBFI credit is counter-cyclical to bank tightness:

$$
\begin{aligned}
bankTightness &= \mathrm{clamp}((bankNplRatio - 0.03) / 0.03, 0, 1) \\
origination &= loanStock \cdot creditBaseRate \\
&\quad \cdot (1 + countercyclical \cdot bankTightness) \\
repayment &= loanStock / creditMaturity \\
defaults &= loanStock \cdot defaultBase \\
&\quad \cdot (1 + defaultUnempSensitivity \cdot \max(unemploymentRate - 0.05, 0)) \\
loanStock' &= \max(loanStock + origination - repayment - defaults, 0)
\end{aligned}
$$

The timeseries exposes `NbfiNetStockFlow` as `origination - repayment -
defaults`, plus `NbfiOriginationToStock`, `NbfiRepaymentToStock`, and
`NbfiDefaultsToStock` to attribute loan-book renewal or runoff. `creditBaseRate`
is a monthly stock-renewal rate calibrated against scheduled repayment and
baseline defaults, with the counter-cyclical multiplier adding origination when
bank NPL tightness rises. `NbfiDepositDrainToAum` diagnoses TFI fund-flow
pressure relative to AUM; deposit drain is a banking deposit/AUM channel, not a
direct term in the NBFI loan-stock identity.

### Quasi-Fiscal BGK/PFR

Quasi-fiscal issuance is a share of government capital programs:

$$
\begin{aligned}
issuance &= \max((govCapitalSpend + euProjectCapital) \cdot issuanceShare, 0) \\
bondAmortization &= bondsOutstanding / avgMaturityMonths \\
nbpPurchase &=
\begin{cases}
issuance \cdot nbpAbsorptionShare, & \text{if NBP QE is active}, \\
0, & \text{otherwise},
\end{cases} \\
bankPurchase &= issuance - nbpPurchase
\end{aligned}
$$

Subsidized lending is a share of issuance, with loan amortization by maturity:

$$
\begin{aligned}
lending &= issuance \cdot lendingShare \\
loanRepayment &= loanPortfolio / loanMaturityMonths \\
loanPortfolio' &= \max(loanPortfolio + lending - loanRepayment, 0)
\end{aligned}
$$

ESA 2010 debt includes central-government cumulative debt plus quasi-fiscal
bonds outstanding:

$$
\begin{aligned}
esa2010Debt &= govCumulativeDebt + quasiFiscalBondsOutstanding
\end{aligned}
$$

### JST

Local-government revenue combines PIT share, CIT share, property tax,
education subvention, and targeted grants:

$$
\begin{aligned}
jstRevenue &=  \\
&\quad pitRevenue \cdot jstPitShare \\
&\quad + centralCitRevenue \cdot jstCitShare \\
&\quad + firms \cdot jstPropertyTax / 12 \\
&\quad + gdp \cdot jstSubventionShare / 12 \\
&\quad + gdp \cdot jstDotacjeShare / 12 \\
 \\
jstSpending &= jstRevenue \cdot jstSpendingMultiplier \\
jstDeficit &= jstSpending - jstRevenue \\
jstDeposits' &= jstDeposits + jstRevenue - jstSpending \\
jstDebt' &= jstDebt + jstDeficit
\end{aligned}
$$

## Known Simplifications And Empirical Grounding Gaps

The current rule book is intentionally explicit about areas that remain
research hypotheses or calibration targets:

- Many behavioral coefficients are calibrated in `SimParams` and domain config
  files. Their current values and provenance status are documented in
  `docs/calibration-register.md`; external source mapping is documented in
  `docs/data-bridge-national-financial-accounts.md`; empirical validation
  against Polish macro and micro series is documented in
  `docs/empirical-validation-report.md`.
- Household decision rules are bounded procedural heuristics, not an estimated
  structural life-cycle model.
- Firm technology adoption and entry include network, risk, and readiness
  effects, but their coefficients still need systematic sensitivity analysis.
- Banks are ten banking-sector rows with rich balance-sheet rules, but deposit
  insurance and depositor decisions are not modeled at the individual account
  contract level.
- Government, NBP, JST, insurance, NBFI, and rest-of-world sectors are
  institutional aggregate agents rather than populations of micro-institutions.
- External-sector rules combine structural trade, GVC, remittance, tourism,
  FDI, and capital-flow channels; not every channel has an econometric estimate
  yet.
- Output columns expose many macro and meso diagnostics, but not every
  household or firm micro trajectory is written to the Monte Carlo time-series
  output by default. The Monte Carlo runner can optionally emit selected-month
  firm micro snapshots for cross-sectional analysis and selected-firm decision
  traces for adoption, financing, implementation, and labor/digital-investment
  gate audits.
- The SFC ledger and matrix artifacts document accounting structure. They do
  not by themselves validate behavioral realism.

These gaps are part of the research-readiness roadmap, not hidden assumptions.
