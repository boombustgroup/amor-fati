# Behavioral Equations And Decision Rules

This document is the paper-facing rule book for Amor Fati's current
SFC-ABM implementation. It describes implemented equations and algorithmic
rules, not a normative target model.

The document complements:

- `docs/odd-model-documentation.md`, which describes the model using the ODD
  and ODD+D structure;
- `docs/sfc-matrix-evidence.md`, which documents the symbolic Balance Sheet
  Matrix (BSM), Transactions Flow Matrix (TFM), and stock-flow reconciliation
  evidence.

## Scope And Notation

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
in `src/main/scala/com/boombustgroup/amorfati/montecarlo/McTimeseriesSchema.scala`.
Those CSV outputs are the primary numeric evidence surface for these rules.

## Timing Contract

One model month follows the deterministic economics pipeline below, then emits
runtime ledger flows and validates SFC identities.

| Stage | Module | Rule surface |
| --- | --- | --- |
| s1 | `engine/economics/FiscalConstraintEconomics.scala` | minimum wage, reservation wage, lending base rate |
| s2 | `engine/economics/LaborEconomics.scala` | wage clearing, employment, immigration, demographics |
| s3 | `engine/economics/HouseholdIncomeEconomics.scala` and `agents/Household.scala` | household income, consumption, saving, credit, retraining, bankruptcy |
| s4 | `engine/economics/DemandEconomics.scala` | sector demand, government purchases, fiscal-rule constraint |
| s5 | `engine/economics/FirmEconomics.scala` and `agents/Firm.scala` | production, pricing markup contribution, technology, labor, investment, defaults, entry |
| s6 | `engine/economics/HouseholdFinancialEconomics.scala` | mortgages, deposit interest, remittances, tourism, consumer credit aggregation |
| s7 | `engine/economics/PriceEquityEconomics.scala` | GDP proxy, inflation, equity, macroprudential, EU funds |
| s8 | `engine/economics/OpenEconEconomics.scala` | external sector, NBP rate, bond yield, QE, insurance, NBFI |
| s9 | `engine/economics/BankingEconomics.scala` and `agents/Banking.scala` | bank P&L, rates, interbank, bond waterfall, failure and resolution |

## Rule-To-Output Map

| Rule group | Main implementation anchors | Representative output columns |
| --- | --- | --- |
| Household income, consumption, savings, credit | `agents/Household.scala`, `engine/economics/HouseholdIncomeEconomics.scala`, `engine/economics/HouseholdFinancialEconomics.scala` | `MarketWage`, `Unemployment`, `EffectivePitRate`, `ConsumerLoans`, `ConsumerOrigination`, `ConsumerDebtService`, `SectorMobilityRate`, `VoluntaryQuits`, `DiasporaRemittanceInflow`, `RemittanceOutflow`, `TourismExport`, `TourismImport` |
| Labor, wages, demographics, social funds | `engine/economics/LaborEconomics.scala`, `agents/SocialSecurity.scala`, `agents/EarmarkedFunds.scala` | `MarketWage`, `Unemployment`, `WorkingAgePop`, `NRetirees`, `MonthlyRetirements`, `ZusContributions`, `ZusPensionPayments`, `NfzContributions`, `NfzSpending`, `PpkContributions`, `FpContributions`, `FgspSpending` |
| Demand allocation and fiscal constraint | `engine/economics/DemandEconomics.scala`, `engine/markets/FiscalRules.scala`, `engine/markets/FiscalBudget.scala` | `GovCurrentSpend`, `GovCapitalSpendDomestic`, `FiscalRuleBinding`, `GovSpendingCutRatio`, `DebtToGdp`, `DeficitToGdp`, `PublicCapitalStock` |
| Firm production, investment, technology, financing, default, entry | `agents/Firm.scala`, `engine/economics/FirmEconomics.scala`, `engine/mechanisms/FirmEntry.scala` | `TotalAdoption`, `AutoRatio`, `HybridRatio`, sector `*_Auto`, sector `*_Sigma`, `GrossInvestment`, `AggCapitalStock`, `AggInventoryStock`, `InventoryChange`, `AggEnergyCost`, `GreenInvestment`, `FirmBirths`, `FirmDeaths`, `NetEntry`, `LivingFirmCount`, `CorpBondIssuance`, `EquityIssuanceTotal` |
| Banking and monetary plumbing | `agents/Banking.scala`, `engine/economics/BankingEconomics.scala`, `agents/EclStaging.scala`, `agents/DepositMobility.scala`, `agents/InterbankContagion.scala` | `NPL`, `MinBankCAR`, `MaxBankNPL`, `MinBankLCR`, `MinBankNSFR`, `BankFailures`, `InterbankRate`, `WIBOR_1M`, `WIBOR_3M`, `WIBOR_6M`, `BfgLevyTotal`, `BailInLoss`, `M0`, `M1`, `M2`, `M3`, `CreditMultiplier` |
| Fiscal, NBP, bond market, external sector | `agents/Nbp.scala`, `engine/markets/OpenEconomy.scala`, `engine/economics/OpenEconEconomics.scala`, `engine/markets/CorporateBondMarket.scala`, `engine/markets/BondAuction.scala` | `RefRate`, `BondYield`, `WeightedCoupon`, `BondsOutstanding`, `NbpBondHoldings`, `ForeignBondHoldings`, `QeActive`, `FxReserves`, `FxInterventionAmt`, `CurrentAccount`, `CapitalAccount`, `TradeBalance_OE`, `Exports_OE`, `TotalImports_OE`, `NFA`, `FDI` |
| Insurance, NBFI, quasi-fiscal, local government | `agents/Insurance.scala`, `agents/Nbfi.scala`, `agents/QuasiFiscal.scala`, `agents/Jst.scala` | `InsLifeReserves`, `InsNonLifeReserves`, `InsLifePremium`, `InsNonLifePremium`, `InsLifeClaims`, `InsNonLifeClaims`, `NbfiTfiAum`, `NbfiOrigination`, `NbfiDefaults`, `NbfiBankTightness`, `QfBondsOutstanding`, `QfIssuance`, `QfLoanPortfolio`, `Esa2010DebtToGdp`, `JstRevenue`, `JstSpending`, `JstDebt`, `JstDeposits` |

## Household Rules

Implementation anchors:

- `agents/Household.scala`
- `engine/economics/HouseholdIncomeEconomics.scala`
- `engine/economics/HouseholdFinancialEconomics.scala`

### State And Status

Each household carries:

- employment status: `Employed(firmId, sectorIdx, wage)`, `Unemployed(months)`,
  `Retraining(monthsLeft, targetSector, cost)`, or `Bankrupt`;
- behavioral traits: skill, health penalty, marginal propensity to consume
  (MPC), social-neighbor ids, education, task routineness, wage scar,
  dependent children, contract type, region, immigrant status;
- bank routing id;
- projected ledger financial stocks: demand deposit, mortgage loan, consumer
  loan, and listed equity.

### Income, Tax, Transfers

For a household `h`, monthly base income is:

```text
baseIncome_h =
  wage_h                         if employed
  unemploymentBenefit(months_h)   if unemployed
  0                               if retraining or bankrupt
```

Deposit interest is paid on demand deposits using the household's bank deposit
rate when bank-specific rates are available:

```text
depositInterest_h = demandDeposit_h * depositRate_b.monthly
```

Monthly PIT is progressive and annualized in the household module:

```text
pitBase_h = grossIncome_h - employeeZus_h
annualizedBase_h = 12 * pitBase_h
pit_h = max(grossTax(annualizedBase_h) - annualTaxCredit, 0) / 12
```

The current social-transfer rule is a lump-sum 800+ payment:

```text
socialTransfer_h = dependentChildren_h * social800
income_h = grossIncome_h - pit_h + socialTransfer_h
```

### Consumption, Saving, And Wealth Effects

Mortgage debt service is variable-rate when bank-specific lending rates are
available:

```text
mortgageDebtService_h =
  mortgageLoan_h * (baseAmortRate + lendingRate_b.monthly)
```

Immigrant households send remittances:

```text
remittance_h = income_h * immigration.remitRate
```

The disposable budget is computed after rent, secured debt service, remittance,
and consumer-credit service:

```text
obligations_h = rent_h + mortgageDebtService_h + remittance_h
disposablePreCredit_h = max(income_h - obligations_h, 0)
consumerCredit_h = consumerCreditRule(...)
fullObligations_h = obligations_h + consumerCreditDebtService_h
disposable_h = max(income_h - fullObligations_h, 0)
savingsDrawdown_h = savingsBufferDrawdown(...)
consumptionBudget_h = disposable_h + newConsumerLoan_h + savingsDrawdown_h
baseConsumption_h = mpc_h * consumptionBudget_h
```

If more than the neighbor-distress threshold of social neighbors are bankrupt
or unemployed, the household applies a precautionary consumption multiplier.
Positive equity revaluation and housing wealth effects then add to consumption.

Closing demand deposits are:

```text
demandDeposit'_h =
  demandDeposit_h + income_h - fullObligations_h + newConsumerLoan_h
  - consumption_h - retrainingCost_h
```

### MPC Adaptation

The state-dependent MPC follows a buffer-stock rule:

```text
targetSavings_h = income_h * bufferTargetMonths
bufferRatio_h = demandDeposit_h / targetSavings_h
deviation_h = bufferRatio_h - 1
bufferAdj_h = clamp(1 - bufferSensitivity * deviation_h, 0, 1)
unemployedAdj_h = 1 + mpcUnemployedBoost if unemployed else 1
mpc'_h = clamp(mpc_h * bufferAdj_h * unemployedAdj_h, MpcFloor, MpcCeiling)
```

Savings drawdown is more aggressive for unemployed or retraining households,
but still keeps a protected buffer floor.

### Consumer Credit

Consumer credit is available only to employed households. A household is
eligible when disposable income is stressed relative to wage, a stochastic
eligibility draw succeeds, and the resulting debt-service-to-income ratio has
room below `ccMaxDti`:

```text
stressed_h = disposablePreCredit_h < wage_h * DisposableWageThreshold
headroom_h = income_h * max(ccMaxDti - existingDti_h, 0)
newConsumerLoan_h = min(headroom_h, ccMaxLoan) if eligible else 0
```

Consumer-loan service is:

```text
consumerDebtService_h =
  consumerLoan_h * (ccAmortRate + (lendingRate_b + ccSpread).monthly)
```

Household bankruptcy writes off the remaining unsecured consumer loan stock and
sets household equity to zero.

### Labor Mobility, Retraining, And Bankruptcy

Employed households may voluntarily search across sectors. The target sector is
selected from sector wage and vacancy signals, adjusted by a friction matrix.
If friction is low, the household quits into unemployment; if friction is high
and deposits cover the retraining cost, the household enters retraining.

Unemployed households become retraining-eligible after the unemployment
threshold. Retraining success depends on skill, health penalty, education, and
sectoral friction:

```text
successProb_h =
  retrainingBaseSuccess * skill_h * (1 - healthPenalty_h)
  * educationMultiplier_h * (1 - friction * frictionSuccessDiscount)
```

Long unemployment reduces skill, increases health penalty, and accumulates a
wage scar. Re-employment decays the wage scar gradually.

Financial distress is tracked as consecutive months with savings below a
bankruptcy floor:

```text
bankruptcyFloor_h =
  max(rent_h + mortgageDebtService_h + consumerDebtService_h, rent_h)
  * bankruptcyThreshold
```

After `bankruptcyDistressMonths`, the household enters the absorbing bankrupt
state.

## Labor, Demographics, And Social Funds

Implementation anchors:

- `engine/economics/FiscalConstraintEconomics.scala`
- `engine/economics/LaborEconomics.scala`
- `engine/markets/LaborMarket.scala`
- `engine/markets/RegionalClearing.scala`
- `agents/SocialSecurity.scala`
- `agents/EarmarkedFunds.scala`

### Minimum Wage And Reservation Wage

On configured adjustment months, the minimum wage is indexed to cumulative
inflation since the last adjustment and partially converges toward a target
share of the market wage:

```text
inflIndexed = previousMinWage * (1 + max(cumulativeInflation, 0))
targetMinWage = marketWage * minWageTargetRatio
newMinWage = max(previousMinWage,
                 inflIndexed + max(targetMinWage - inflIndexed, 0)
                 * minWageConvergenceSpeed)
reservationWage = newMinWage
```

### Wage Clearing

Labor demand is the sum of workers planned by living firms. Regional labor
clearing computes regional and national wages. The national wage is then
adjusted for expected inflation pressure and union rigidity:

```text
wageAfterExpectations =
  max(reservationWage,
      rawWage * (1 + expWagePassthrough
                 * max(expectedInflation - targetInflation, 0) / 12))

newWage =
  max(reservationWage,
      wageAfterExpectations
      + (previousMarketWage - wageAfterExpectations)
        * unionRigidity * aggregateUnionDensity)
```

Employment is capped by working-age population. An operational hiring-slack
factor reduces firm hiring when aggregate desired labor exceeds available labor.

### Demographics And Payroll Funds

Monthly demographics update retirements and working-age population:

```text
retirements = demRetirementRate * employed
workingAgePop' = max(0, workingAgePop - retirements - workingAgeDecline
                        + netMigration)
retirees' = retirees + retirements
```

ZUS, NFZ, PPK, FP, and FGSP contributions are payroll-proportional, with
contract-type adjustments for household workers. ZUS and NFZ deficits are
covered by government subventions. NFZ spending is per-capita and rises with
retirees through an aging elasticity. PPK contributions create a government-bond
purchase request in the downstream bond waterfall.

Earmarked funds follow:

```text
FP contributions = payroll * fpRate
FP spending = unemploymentBenefits + employed * fpAlmpSpendPerWorker
PFRON contributions = configured monthly revenue
PFRON spending = configured monthly spending
FGSP spending = bankruptFirms * avgFirmWorkers * fgspPayoutPerWorker
governmentSubvention = sum(max(spending_i - contributions_i, 0))
```

## Demand, Prices, GDP, And Equity

Implementation anchors:

- `engine/economics/DemandEconomics.scala`
- `engine/economics/PriceEquityEconomics.scala`
- `engine/markets/FiscalRules.scala`
- `engine/markets/PriceLevel.scala`
- `engine/markets/EquityMarket.scala`
- `engine/mechanisms/EuFunds.scala`
- `engine/mechanisms/Macroprudential.scala`

### Government Purchases And Fiscal Rules

Raw government purchases are price-indexed base spending plus an automatic
stabilizer based on unemployment above NAIRU:

```text
unempGap = max(unemploymentRate - NAIRU, 0)
rawGovPurchases = govBaseSpending * max(priceLevel, 1)
                  + govBaseSpending * unempGap * govAutoStabMult
```

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

```text
demand_s =
  consWeight_s * domesticConsumption
  + govWeight_s * govPurchases
  + investWeight_s * laggedInvestmentDemand
  + exports_s
```

Demand pressure is demand divided by nominal sector capacity. Excess demand in
capacity-constrained sectors is redistributed to sectors with slack. The
smoothed hiring signal persists across months to prevent one-month whipsaw in
firm hiring.

### GDP, Inflation, Equity, Macroprudential

The monthly GDP proxy is:

```text
gdp =
  domesticConsumption
  + governmentDemandContribution
  + euProjectContribution
  + exports
  + domesticGrossFixedCapitalFormation
  + inventoryChange
```

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

Implementation anchors:

- `agents/Firm.scala`
- `engine/economics/FirmEconomics.scala`
- `engine/markets/IntermediateMarket.scala`
- `engine/markets/CorporateBondMarket.scala`
- `engine/mechanisms/FirmEntry.scala`
- `agents/StateOwned.scala`

### Technology State And Capacity

Firm technology is one of:

- traditional, with explicit worker count;
- hybrid, with worker count and AI efficiency;
- automated, with AI efficiency and skeleton crew;
- bankrupt, an inactive state.

Capacity starts from sector revenue multipliers and firm-size scaling. The
labor-effective component is:

```text
laborEff =
  workers / initialSize                                      if traditional
  0.4 * workers / initialSize + 0.6 * aiEfficiency           if hybrid
  aiEfficiency                                               if automated
  0                                                          if bankrupt
```

When physical capital is active, capacity uses a CES production function:

```text
capacity_f =
  baseRevenue * sizeScale_f * sectorRevenueMultiplier_s
  * CES(K_f / targetK_f, laborEff_f, sigma_s)
```

Near the Cobb-Douglas boundary, the CES helper degrades to a Cobb-Douglas
form. Sector sigma also enters technology-adoption thresholds and evolves by
learning-by-doing in `PriceEquityEconomics`.

### Monthly P&L

Revenue is capacity times sector demand and the price level:

```text
revenue_f = priceLevel * capacity_f * sectorDemandMultiplier_s
```

Costs include labor, residual domestic operating costs, depreciation, AI or
hybrid maintenance, bank and corporate-bond interest, inventory carrying cost,
energy and ETS cost, and foreign-owned profit shifting:

```text
profitBeforeTax_f =
  revenue_f
  - laborCost_f
  - otherCosts_f
  - depreciation_f
  - aiMaintenance_f
  - interest_f
  - inventoryCost_f
  - energyCost_f
  - profitShiftCost_f
```

CIT uses loss carryforward. Losses accumulate when profit is negative; positive
profit can offset accumulated losses up to the configured share, and remaining
losses decay gradually.

### Hiring, Firing, And Labor Matching

Traditional firms compute desired workers by searching for the largest
headcount where marginal revenue exceeds wage cost:

```text
MR(workers) = (capacity(workers) - capacity(workers - 1))
              * demandMultiplier_s * priceLevel
desiredWorkers = largest workers where MR(workers) > wageCost_s
```

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

```text
desiredInvestment_f =
  depreciation_f
  + max(targetK_f - postDepreciationK_f, 0) * capitalAdjustSpeed
actualInvestment_f = min(desiredInvestment_f, max(cash_f, 0))
```

Green investment mirrors this logic with green capital-labor targets, green
depreciation, and a separate share of available cash.

Inventory changes combine unsold production, spoilage, adjustment toward a
target inventory ratio, and stress liquidation when cash is negative:

```text
unsoldValue_f = max(productionValue_f - salesValue_f, 0)
targetInventory_f = revenue_f * inventoryTargetRatio_s
rawInventoryChange_f =
  unsoldValue_f
  + (targetInventory_f - postSpoilageInventory_f) * inventoryAdjustSpeed
```

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

```text
nplLoss = newNplDebt * (1 - loanRecovery)
```

Firm entry replaces bankrupt slots and can add net entry when macro conditions
allow. Sector entry weights depend on profit signals and entry barriers. New
AI-native entrants become possible once aggregate adoption crosses the entry
threshold and a probability draw succeeds.

## Banking Rules

Implementation anchors:

- `agents/Banking.scala`
- `engine/economics/BankingEconomics.scala`
- `agents/EclStaging.scala`
- `agents/DepositMobility.scala`
- `agents/InterbankContagion.scala`
- `engine/ledger/CorporateBondOwnership.scala`

### Balance-Sheet Ratios

Aggregate and per-bank diagnostics are computed from operational bank state and
ledger-owned financial stocks.

Risk-weighted assets are:

```text
RWA_b = firmLoans_b + consumerLoans_b + 0.50 * corporateBondHoldings_b
CAR_b = capital_b / RWA_b
```

When the denominator is effectively zero, the implementation returns a safe
ratio floor.

Liquidity ratios are:

```text
HQLA_b = reserves_b + govBondAfs_b + govBondHtm_b
netCashOutflows_b = demandDeposits_b * demandDepositRunoff
LCR_b = HQLA_b / netCashOutflows_b

ASF_b = capital_b + termDeposits_b * 0.95 + demandDeposits_b * 0.90
RSF_b =
  shortLoans_b * 0.50
  + mediumLoans_b * 0.65
  + longLoans_b * 0.85
  + govBonds_b * 0.05
  + corporateBonds_b * 0.50
NSFR_b = ASF_b / RSF_b
```

### Loan Pricing And Approval

Household deposit rates are the reference rate minus the household deposit
spread, floored at zero.

Firm lending rates combine:

```text
lendingRate_b =
  referenceOrWiborRate
  + baseSpread
  + bankSpecificSpread_b
  + min(NPL_b * nplSpreadFactor, NplSpreadCap)
  + capitalShortfallPenalty_b
  + crowdingOutSpread_b
```

Capital penalty activates when CAR falls below `minCar * 1.5`. Crowding-out
passes part of a government-bond-yield premium into firm loan spreads. Failed
banks receive a fixed penalty spread and cannot lend.

Credit approval requires projected CAR above the macroprudential effective
minimum, LCR and NSFR above regulatory minima, and a stochastic approval draw.
The approval probability falls with NPL ratio and reserve deficit but has a
configured floor.

### Interbank, Facilities, And Monetary Aggregates

The interbank rate is a corridor rate between the deposit facility and lombard
facility. It rises with aggregate credit stress and scarce excess reserves:

```text
creditStress = clamp(aggregateNplRatio / stressThreshold, 0, 1)
liquidityRatio = clamp(excessReserves / requiredReserves, 0, 1)
interbankRate = depositFacilityRate
                + (1 - liquidityRatio) * creditStress
                  * (lombardRate - depositFacilityRate)
```

Interbank clearing matches lenders with excess reserves to borrowers with
reserve deficits. A hoarding factor reduces lending when system NPL stress is
high.

Monetary aggregates are:

```text
M0 = bank reserves at NBP
M1 = demand deposits
M2 = demand deposits + term deposits
M3 = M2 + TFI AUM + corporate bonds outstanding
creditMultiplier = M2 / M0
```

### Bank P&L, Provisioning, Failure, Resolution

Bank capital changes by losses plus retained income:

```text
losses_b =
  corporateNplLoss_b
  + mortgageNplLoss_b
  + consumerNplLoss_b
  + corporateBondDefaultLoss_b
  + BFGLevy_b
  + unrealizedAfsBondLoss_b

grossIncome_b =
  firmLoanInterest_b
  + householdDebtService_b
  + govBondIncome_b
  - depositInterest_b
  + reserveInterest_b
  + standingFacilityIncome_b
  + interbankInterest_b
  + mortgageInterestIncome_b
  + consumerDebtService_b
  + corporateBondCoupon_b

capital'_b = capital_b - losses_b + grossIncome_b * profitRetention
```

IFRS 9 ECL staging adds provision changes based on the performing book, new
defaults, unemployment, and GDP growth.

Failure is triggered by negative capital, three consecutive months below
effective minimum CAR, or an LCR breach below half of the minimum. BFG bail-in
haircuts uninsured deposits of failed banks. Purchase-and-assumption resolution
transfers deposits, government bonds, performing loans, consumer loans, and
corporate-bond holdings to the healthiest surviving bank. Firms and households
routed to failed banks are reassigned to the absorber.

## Fiscal, Monetary, Bond-Market, And External Rules

Implementation anchors:

- `engine/markets/FiscalBudget.scala`
- `engine/markets/FiscalRules.scala`
- `agents/Nbp.scala`
- `engine/markets/OpenEconomy.scala`
- `engine/economics/OpenEconEconomics.scala`
- `engine/markets/BondAuction.scala`
- `engine/markets/CorporateBondMarket.scala`
- `engine/mechanisms/Expectations.scala`

### Government Budget And Debt

Government budget revenue includes CIT/PIT-related revenue, VAT, NBP
remittance, excise, customs, and state-owned-firm dividends. Spending includes
unemployment benefits, social transfers, current government purchases, capital
government purchases, debt service, social-fund subventions, earmarked-fund
subventions, and EU co-financing.

```text
totalSpend =
  unemploymentBenefits
  + socialTransfers
  + govCurrentSpend
  + govCapitalSpend
  + debtService
  + ZUSSubvention
  + NFZSubvention
  + earmarkedFundSubvention
  + euCofinancing

totalRevenue =
  taxRevenue + governmentDividendRevenue

deficit = totalSpend - totalRevenue
cumulativeDebt' = cumulativeDebt + deficit
govBondOutstanding' = max(govBondOutstanding + deficit, 0)
```

Public capital depreciates monthly and increases with domestic government
capital spending plus EU project capital.

The weighted coupon follows a rolling weighted-average-maturity rule. Each
month, a fraction of the outstanding portfolio matures and new deficit issuance
enters at the current market yield.

### NBP Policy, Bond Yield, QE, FX

The reference rate follows a smoothed Taylor-type rule:

```text
taylorTarget =
  neutralRate
  + taylorAlpha * (inflation - targetInflation)
  - taylorDelta * outputGap
  + taylorBeta * exchangeRateChange

referenceRate' =
  clamp(previousRate * inertia + taylorTarget * (1 - inertia),
        rateFloor,
        rateCeiling,
        maxMonthlyChange)
```

Government bond yield is:

```text
bondYield =
  referenceRate
  + termPremium
  + fiscalRisk(debtToGdp)
  - qeCompression(nbpBondHoldings / GDP)
  - foreignDemandDiscount(if NFA > 0)
  + credibilityPremium
```

QE activates near the lower bound when realized or expected inflation is below
target by the configured threshold. QE purchases are requested by NBP but
settled through the bond waterfall, so actual sold bonds leave banks and enter
NBP holdings exactly.

FX intervention buys or sells EUR when the exchange rate leaves the tolerance
band around the base rate. EUR purchases inject PLN reserves; EUR sales drain
reserves. The intervention also contributes a shock term to the exchange-rate
update.

### External Sector

Exports come from GVC sector exports when available; otherwise they follow
foreign GDP growth, real exchange-rate competitiveness, and automation-related
unit-labor-cost effects.

Imports include household consumption imports, technology and investment
imports, imported intermediates, and tourism imports. Imported intermediates
depend on sector output, sector import content, and nominal exchange-rate
effects.

The current account is:

```text
currentAccount = tradeBalance + primaryIncome + secondaryIncome
primaryIncome = NFA * nfaReturnRate.monthly
secondaryIncome = EUFunds - remittanceOutflow + diasporaInflow
```

The capital account combines FDI, ordinary portfolio flows, carry-trade flows,
and stress capital-flight outflows. FDI rises with automation and falls with a
negative-NFA dampening term. Portfolio flows react to the domestic-foreign rate
differential and NFA risk premium.

Exchange-rate change responds to the balance-of-payments ratio, negative-NFA
risk, FX intervention shock, and PPP drift:

```text
exchangeRateShock =
  exRateAdjSpeed * (-(CA + KA) / GDP + nfaRisk)
  + fxInterventionShock
  + pppDrift
```

NFA updates by the current account plus a partial exchange-rate valuation effect
on foreign assets.

## Insurance, NBFI, Quasi-Fiscal, And JST Rules

Implementation anchors:

- `agents/Insurance.scala`
- `agents/Nbfi.scala`
- `agents/QuasiFiscal.scala`
- `agents/Jst.scala`
- `engine/economics/OpenEconEconomics.scala`
- `engine/economics/BankingEconomics.scala`

### Insurance

Life and non-life premiums are proportional to the employed wage bill:

```text
lifePremium = employed * wage * lifePremiumRate
nonLifePremium = employed * wage * nonLifePremiumRate
```

Life claims follow a loss ratio. Non-life claims widen with unemployment above
the non-life unemployment threshold:

```text
nonLifeClaims =
  nonLifePremium * nonLifeLossRatio
  * (1 + max(unemploymentRate - threshold, 0) * nonLifeUnempSensitivity)
```

Investment income comes from government bonds, corporate bonds, and equity,
minus corporate-bond default loss. Reserves update from premiums, claims, and
investment income. Government-bond and equity holdings rebalance gradually
toward target allocation shares.

### NBFI And TFI

TFI inflow is proportional to wage bill and excess fund return over deposits:

```text
baseInflow = employed * wage * tfiInflowRate
fundReturn =
  govBondYield * tfiGovBondShare
  + equityReturn.annualized * tfiEquityShare
  + govBondYield * tfiCorpBondShare
netInflow = baseInflow * (1 + clamp(fundReturn - depositRate, cap)
                          * ExcessReturnSensitivity)
```

TFI AUM updates by net inflow and investment income, then rebalances toward
government-bond and equity target shares. Net TFI inflow is recorded as deposit
drain from the banking system.

NBFI credit is counter-cyclical to bank tightness:

```text
bankTightness = clamp((bankNplRatio - 0.03) / 0.03, 0, 1)
origination = domesticConsumption * creditBaseRate
              * (1 + countercyclical * bankTightness)
repayment = loanStock / creditMaturity
defaults = loanStock * defaultBase
           * (1 + defaultUnempSensitivity * max(unemploymentRate - 0.05, 0))
loanStock' = max(loanStock + origination - repayment - defaults, 0)
```

### Quasi-Fiscal BGK/PFR

Quasi-fiscal issuance is a share of government capital programs:

```text
issuance = max((govCapitalSpend + euProjectCapital) * issuanceShare, 0)
bondAmortization = bondsOutstanding / avgMaturityMonths
nbpPurchase = issuance * nbpAbsorptionShare if NBP QE active else 0
bankPurchase = issuance - nbpPurchase
```

Subsidized lending is a share of issuance, with loan amortization by maturity:

```text
lending = issuance * lendingShare
loanRepayment = loanPortfolio / loanMaturityMonths
loanPortfolio' = max(loanPortfolio + lending - loanRepayment, 0)
```

ESA 2010 debt includes central-government cumulative debt plus quasi-fiscal
bonds outstanding:

```text
esa2010Debt = govCumulativeDebt + quasiFiscalBondsOutstanding
```

### JST

Local-government revenue combines PIT share, CIT share, property tax,
education subvention, and targeted grants:

```text
jstRevenue =
  pitRevenue * jstPitShare
  + centralCitRevenue * jstCitShare
  + firms * jstPropertyTax / 12
  + gdp * jstSubventionShare / 12
  + gdp * jstDotacjeShare / 12

jstSpending = jstRevenue * jstSpendingMultiplier
jstDeficit = jstSpending - jstRevenue
jstDeposits' = jstDeposits + jstRevenue - jstSpending
jstDebt' = jstDebt + jstDeficit
```

## Known Simplifications And Empirical Grounding Gaps

The current rule book is intentionally explicit about areas that remain
research hypotheses or calibration targets:

- Many behavioral coefficients are calibrated in `SimParams` and domain config
  files. Their current values and provenance status are documented in
  `docs/calibration-register.md`; external source mapping is documented in
  `docs/data-bridge-national-financial-accounts.md`; empirical validation
  against historical Polish macro and micro series is documented in
  `docs/empirical-validation-report.md`.
- Household decision rules are bounded procedural heuristics, not an estimated
  structural life-cycle model.
- Firm technology adoption and entry include network, risk, and readiness
  effects, but their coefficients still need systematic sensitivity analysis.
- Banks are seven named agents with rich balance-sheet rules, but deposit
  insurance and depositor decisions are not modeled at the individual account
  contract level.
- Government, NBP, JST, insurance, NBFI, and rest-of-world sectors are
  institutional aggregate agents rather than populations of micro-institutions.
- External-sector rules combine structural trade, GVC, remittance, tourism,
  FDI, and capital-flow channels; not every channel has an econometric estimate
  yet.
- Output columns expose many macro and meso diagnostics, but not every
  household or firm micro trajectory is written to the Monte Carlo time-series
  output by default.
- The SFC ledger and matrix artifacts document accounting structure. They do
  not by themselves validate behavioral realism.

These gaps are part of the research-readiness roadmap, not hidden assumptions.
