package sfc.sfc

import sfc.config.Config

case class GovState(
  bdpActive: Boolean,
  taxRevenue: Double,
  bdpSpending: Double,
  deficit: Double,
  cumulativeDebt: Double,
  unempBenefitSpend: Double
)

case class BankState(
  totalLoans: Double,
  nplAmount: Double,
  capital: Double,
  deposits: Double
):
  def nplRatio: Double = if totalLoans > 1.0 then nplAmount / totalLoans else 0.0
  def car: Double = if totalLoans > 1.0 then capital / totalLoans else 10.0
  def lendingRate(refRate: Double): Double =
    refRate + Config.BaseSpread + Math.min(0.15, nplRatio * Config.NplSpreadFactor)
  def canLend(amount: Double): Boolean =
    val projected = capital / (totalLoans + amount)
    projected >= Config.MinCar

case class ForexState(
  exchangeRate: Double,
  imports: Double,
  exports: Double,
  tradeBalance: Double,
  techImports: Double
)

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
  importedIntermediates: Double   // Cross-border intermediate inputs
)
object BopState:
  val zero: BopState = BopState(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
