package sfc.accounting

import sfc.accounting.{BopState, MonetaryAggregates}
import sfc.config.Config
import sfc.types.*

case class GovState(
  bdpActive: Boolean,
  taxRevenue: PLN,
  bdpSpending: PLN,
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
)

case class BankState(
  totalLoans: PLN,
  nplAmount: PLN,
  capital: PLN,
  deposits: PLN,
  govBondHoldings: PLN = PLN.Zero,
  consumerLoans: PLN = PLN.Zero,
  consumerNpl: PLN = PLN.Zero,
  corpBondHoldings: PLN = PLN.Zero, // #40: corporate bond holdings (bank share)
):
  def nplRatio: Double = if totalLoans.toDouble > 1.0 then (nplAmount / totalLoans) else 0.0
  def car: Double =
    val totalRwa = (totalLoans + consumerLoans + corpBondHoldings * 0.50).toDouble
    if totalRwa > 1.0 then capital.toDouble / totalRwa else 10.0
  def lendingRate(refRate: Double): Double =
    refRate + Config.BaseSpread + Math.min(0.15, nplRatio * Config.NplSpreadFactor)
  def canLend(amount: Double): Boolean =
    val projected = capital.toDouble / ((totalLoans + consumerLoans + corpBondHoldings * 0.50).toDouble + amount)
    projected >= Config.MinCar

case class ForexState(
  exchangeRate: Double,
  imports: PLN,
  exports: PLN,
  tradeBalance: PLN,
  techImports: PLN,
)

/** Monetary aggregates — diagnostic, not SFC-relevant. */
case class MonetaryAggregates(
  m1: PLN, // Bank deposits (≈ narrow money)
  monetaryBase: PLN, // Reserves at NBP
  creditMultiplier: Double, // m1 / monetaryBase
)
object MonetaryAggregates:
  val zero: MonetaryAggregates = MonetaryAggregates(PLN.Zero, PLN.Zero, 0)

  def compute(deposits: PLN, reserves: PLN): MonetaryAggregates =
    val base = Math.max(1.0, reserves.toDouble)
    MonetaryAggregates(deposits, reserves, deposits.toDouble / base)

case class BopState(
  nfa: PLN, // Net foreign assets (cumulative)
  foreignAssets: PLN, // Gross foreign assets
  foreignLiabilities: PLN, // Gross foreign liabilities
  currentAccount: PLN, // Monthly CA: trade + primary + secondary income
  capitalAccount: PLN, // Monthly KA: FDI + portfolio flows
  tradeBalance: PLN, // exports - imports
  primaryIncome: PLN, // Interest on NFA
  secondaryIncome: PLN, // EU transfers (exogenous)
  fdi: PLN, // FDI inflows
  portfolioFlows: PLN, // IRP + risk premium driven
  reserves: PLN, // CB foreign reserves
  exports: PLN, // Total exports this month
  totalImports: PLN, // Consumption + tech + intermediate imports
  importedIntermediates: PLN, // Cross-border intermediate inputs
  euFundsMonthly: PLN = PLN.Zero, // EU funds transfer this month
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
