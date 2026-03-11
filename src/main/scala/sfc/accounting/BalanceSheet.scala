package sfc.accounting

import sfc.types.*

case class GovState(
    taxRevenue: PLN,
    deficit: PLN,
    cumulativeDebt: PLN,
    unempBenefitSpend: PLN,
    bondsOutstanding: PLN = PLN.Zero,
    bondYield: Rate = Rate.Zero,
    debtServiceSpend: PLN = PLN.Zero,
    socialTransferSpend: PLN = PLN.Zero,
    publicCapitalStock: PLN = PLN.Zero,
    govCurrentSpend: PLN = PLN.Zero,
    govCapitalSpend: PLN = PLN.Zero,
    euCofinancing: PLN = PLN.Zero,
    exciseRevenue: PLN = PLN.Zero,
    customsDutyRevenue: PLN = PLN.Zero,
    minWageLevel: PLN = PLN(4666.0),
    minWagePriceLevel: Double = 1.0,
)

case class ForexState(
    exchangeRate: Double,
    imports: PLN,
    exports: PLN,
    tradeBalance: PLN,
    techImports: PLN,
)

case class BopState(
    nfa: PLN,                              // Net foreign assets (cumulative)
    foreignAssets: PLN,                    // Gross foreign assets
    foreignLiabilities: PLN,               // Gross foreign liabilities
    currentAccount: PLN,                   // Monthly CA: trade + primary + secondary income
    capitalAccount: PLN,                   // Monthly KA: FDI + portfolio flows
    tradeBalance: PLN,                     // exports - imports
    primaryIncome: PLN,                    // Interest on NFA
    secondaryIncome: PLN,                  // EU transfers (exogenous)
    fdi: PLN,                              // FDI inflows
    portfolioFlows: PLN,                   // IRP + risk premium driven
    reserves: PLN,                         // CB foreign reserves
    exports: PLN,                          // Total exports this month
    totalImports: PLN,                     // Consumption + tech + intermediate imports
    importedIntermediates: PLN,            // Cross-border intermediate inputs
    euFundsMonthly: PLN = PLN.Zero,        // EU funds transfer this month
    euCumulativeAbsorption: PLN = PLN.Zero, // Cumulative EU funds absorbed
)
object BopState:
  val zero: BopState = BopState(
    PLN.Zero,
    PLN.Zero,
    PLN.Zero,
    PLN.Zero,
    PLN.Zero,
    PLN.Zero,
    PLN.Zero,
    PLN.Zero,
    PLN.Zero,
    PLN.Zero,
    PLN.Zero,
    PLN.Zero,
    PLN.Zero,
    PLN.Zero,
  )
