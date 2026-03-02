package sfc.sfc

import sfc.config.Config

case class GovState(
  bdpActive: Boolean,
  taxRevenue: Double,
  bdpSpending: Double,
  deficit: Double,
  cumulativeDebt: Double,
  unempBenefitSpend: Double,
  bondsOutstanding: Double = 0.0,
  bondYield: Double = 0.0,
  debtServiceSpend: Double = 0.0,
  socialTransferSpend: Double = 0.0,
  publicCapitalStock: Double = 0.0,
  govCurrentSpend: Double = 0.0,
  govCapitalSpend: Double = 0.0,
  euCofinancing: Double = 0.0,
  exciseRevenue: Double = 0.0,
  customsDutyRevenue: Double = 0.0
)

case class BankState(
  totalLoans: Double,
  nplAmount: Double,
  capital: Double,
  deposits: Double,
  govBondHoldings: Double = 0.0,
  consumerLoans: Double = 0.0,
  consumerNpl: Double = 0.0
):
  def nplRatio: Double = if totalLoans > 1.0 then nplAmount / totalLoans else 0.0
  def car: Double =
    val totalRwa = totalLoans + consumerLoans
    if totalRwa > 1.0 then capital / totalRwa else 10.0
  def lendingRate(refRate: Double): Double =
    refRate + Config.BaseSpread + Math.min(0.15, nplRatio * Config.NplSpreadFactor)
  def canLend(amount: Double): Boolean =
    val projected = capital / (totalLoans + consumerLoans + amount)
    projected >= Config.MinCar

case class ForexState(
  exchangeRate: Double,
  imports: Double,
  exports: Double,
  tradeBalance: Double,
  techImports: Double
)

/** Monetary aggregates — diagnostic, not SFC-relevant. */
case class MonetaryAggregates(
  m1: Double,              // Bank deposits (≈ narrow money)
  monetaryBase: Double,    // Reserves at NBP
  creditMultiplier: Double // m1 / monetaryBase
)
object MonetaryAggregates:
  val zero: MonetaryAggregates = MonetaryAggregates(0, 0, 0)

  def compute(deposits: Double, reserves: Double): MonetaryAggregates =
    val base = Math.max(1.0, reserves)  // floor to avoid division by zero
    MonetaryAggregates(deposits, reserves, deposits / base)

case class BopState(
  nfa: Double,                    // Net foreign assets (cumulative)
  foreignAssets: Double,          // Gross foreign assets
  foreignLiabilities: Double,     // Gross foreign liabilities
  currentAccount: Double,         // Monthly CA: trade + primary + secondary income
  capitalAccount: Double,         // Monthly KA: FDI + portfolio flows
  tradeBalance: Double,           // exports - imports
  primaryIncome: Double,          // Interest on NFA
  secondaryIncome: Double,        // EU transfers (exogenous)
  fdi: Double,                    // FDI inflows
  portfolioFlows: Double,         // IRP + risk premium driven
  reserves: Double,               // CB foreign reserves
  exports: Double,                // Total exports this month
  totalImports: Double,           // Consumption + tech + intermediate imports
  importedIntermediates: Double,  // Cross-border intermediate inputs
  euFundsMonthly: Double = 0.0,        // EU funds transfer this month
  euCumulativeAbsorption: Double = 0.0  // Cumulative EU funds absorbed
)
object BopState:
  val zero: BopState = BopState(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
