package sfc.accounting

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
):
  def effectiveBdpPerCapita(population: Int): Double =
    if bdpActive then (bdpSpending / population.toDouble).toDouble else 0.0

/** Aggregate banking-sector balance sheet — sum over all 7 per-bank BankStates.
  *
  * Pure DTO recomputed every step via `Banking.State.aggregate`. Read-only snapshot consumed by output columns, SFC
  * identities, macro feedback loops (corporate bond absorption, insurance/NBFI asset allocation), and government fiscal
  * arithmetic. All mutation happens at the per-bank level in `Banking.BankState`; this aggregate is derived, never
  * written back.
  */
case class BankingAggregate(
  totalLoans: PLN, // Outstanding corporate loans (sum of per-bank `loans`)
  nplAmount: PLN, // Non-performing corporate loan stock (KNF Stage 3)
  capital: PLN, // Regulatory capital (Tier 1 + retained earnings)
  deposits: PLN, // Total customer deposits (households + firms)
  govBondHoldings: PLN, // Treasury bond portfolio (skarbowe papiery wartościowe)
  consumerLoans: PLN, // Outstanding unsecured household credit
  consumerNpl: PLN, // Non-performing consumer loan stock
  corpBondHoldings: PLN, // Corporate bond portfolio — bank share only (default 30%, CORPBOND_BANK_SHARE)
):
  /** Non-performing loan ratio: nplAmount / totalLoans. Returns 0.0 when loan book is empty. */
  def nplRatio: Double = if totalLoans.toDouble > 1.0 then (nplAmount / totalLoans) else 0.0

  /** Capital adequacy ratio: capital / risk-weighted assets. Corporate bonds carry 50% risk weight (Basel III, BBB
    * bucket). Returns 10.0 (well-capitalised floor) when risk-weighted assets ≤ 1 to avoid division by zero.
    */
  def car: Double =
    val totalRwa = (totalLoans + consumerLoans + corpBondHoldings * 0.50).toDouble
    if totalRwa > 1.0 then capital.toDouble / totalRwa else 10.0

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
