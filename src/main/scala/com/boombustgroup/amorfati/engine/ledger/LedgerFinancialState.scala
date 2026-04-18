package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.agents.{Banking, Firm, Household, Insurance, Nbfi, Nbp, QuasiFiscal}
import com.boombustgroup.amorfati.engine.SocialState
import com.boombustgroup.amorfati.engine.markets.CorporateBondMarket
import com.boombustgroup.amorfati.types.{FirmId, PLN}

/** Ledger-owned snapshot of ledger-contracted financial stocks used by the
  * engine.
  *
  * This file defines the runtime state that should become the source of truth
  * for financial ownership with an explicit ledger contract. It is
  * intentionally narrower than `World`: it stores ledger-backed stock balances,
  * not market-memory fields, monthly flow deltas, execution-only settlement
  * shells, or public assets that the engine still keeps outside the ledger
  * contract. Boundary code may project these balances back into `World` mirrors
  * while the migration is in progress.
  */
final case class LedgerFinancialState(
    /** Household-sector ledger-backed financial stock balances. */
    households: Vector[LedgerFinancialState.HouseholdBalances],
    /** Firm-sector ledger-backed financial stock balances. */
    firms: Vector[LedgerFinancialState.FirmBalances],
    /** Bank-sector ledger-backed financial stock balances. */
    banks: Vector[LedgerFinancialState.BankBalances],
    /** Central-government ledger-backed financial stock balances. */
    government: LedgerFinancialState.GovernmentBalances,
    /** Foreign-sector ledger-backed financial stock balances. */
    foreign: LedgerFinancialState.ForeignBalances,
    /** Central-bank ledger-backed financial stock balances. */
    nbp: LedgerFinancialState.NbpBalances,
    /** Insurance-sector ledger-backed financial stock balances. */
    insurance: LedgerFinancialState.InsuranceBalances,
    /** Social, local-government, quasi-fiscal, and investment-fund balances. */
    funds: LedgerFinancialState.FundBalances,
)

object LedgerFinancialState:

  def householdBalances(stocks: Household.FinancialStocks): HouseholdBalances =
    HouseholdBalances(
      demandDeposit = stocks.demandDeposit,
      mortgageLoan = stocks.mortgageLoan,
      consumerLoan = stocks.consumerLoan,
      equity = stocks.equity,
    )

  def refreshHouseholdBalances(
      households: Vector[Household.State],
      previous: Vector[HouseholdBalances],
  ): Vector[HouseholdBalances] =
    households.map: household =>
      previous.lift(household.id.toInt).getOrElse(initialHouseholdBalances(household))

  def firmBalances(stocks: Firm.FinancialStocks, corpBond: PLN): FirmBalances =
    FirmBalances(
      cash = stocks.cash,
      firmLoan = stocks.firmLoan,
      corpBond = corpBond,
      equity = stocks.equity,
    )

  def refreshFirmPopulationBalances(
      firms: Vector[Firm.State],
      previous: Vector[FirmBalances],
      newFirmIds: Set[FirmId],
  ): Vector[FirmBalances] =
    firms.map: firm =>
      if newFirmIds.contains(firm.id) then initialFirmBalances(firm, corpBond = PLN.Zero)
      else previous.lift(firm.id.toInt).getOrElse(initialFirmBalances(firm, corpBond = PLN.Zero))

  def refreshFirmFinancialBalances(
      balances: Vector[Firm.FinancialStocks],
      previous: Vector[FirmBalances],
  ): Vector[FirmBalances] =
    balances.zipWithIndex.map: (balance, index) =>
      firmBalances(balance, corpBond = previous.lift(index).fold(PLN.Zero)(_.corpBond))

  def bankBalances(stocks: Banking.BankFinancialStocks, corpBond: PLN): BankBalances =
    BankBalances(
      totalDeposits = stocks.totalDeposits,
      demandDeposit = stocks.demandDeposit,
      termDeposit = stocks.termDeposit,
      firmLoan = stocks.firmLoan,
      consumerLoan = stocks.consumerLoan,
      govBondAfs = stocks.govBondAfs,
      govBondHtm = stocks.govBondHtm,
      reserve = stocks.reserve,
      interbankLoan = stocks.interbankLoan,
      corpBond = corpBond,
    )

  def insuranceOpeningBalances(ledgerFinancialState: LedgerFinancialState): Insurance.OpeningBalances =
    Insurance.OpeningBalances(
      lifeReserves = ledgerFinancialState.insurance.lifeReserve,
      nonLifeReserves = ledgerFinancialState.insurance.nonLifeReserve,
      govBondHoldings = ledgerFinancialState.insurance.govBondHoldings,
      corpBondHoldings = ledgerFinancialState.insurance.corpBondHoldings,
      equityHoldings = ledgerFinancialState.insurance.equityHoldings,
    )

  def insuranceBalances(insurance: Insurance.ClosingBalances, corpBondHoldings: PLN): InsuranceBalances =
    InsuranceBalances(
      lifeReserve = insurance.lifeReserves,
      nonLifeReserve = insurance.nonLifeReserves,
      govBondHoldings = insurance.govBondHoldings,
      corpBondHoldings = corpBondHoldings,
      equityHoldings = insurance.equityHoldings,
    )

  def nbfiOpeningBalances(ledgerFinancialState: LedgerFinancialState): Nbfi.OpeningBalances =
    Nbfi.OpeningBalances(
      tfiAum = ledgerFinancialState.funds.nbfi.tfiUnit,
      tfiGovBondHoldings = ledgerFinancialState.funds.nbfi.govBondHoldings,
      corpBondHoldings = ledgerFinancialState.funds.nbfi.corpBondHoldings,
      tfiEquityHoldings = ledgerFinancialState.funds.nbfi.equityHoldings,
      nbfiLoanStock = ledgerFinancialState.funds.nbfi.nbfiLoanStock,
    )

  def nbfiFundBalances(nbfi: Nbfi.ClosingBalances, corpBondHoldings: PLN): NbfiFundBalances =
    val cashHoldings = (nbfi.tfiAum - nbfi.tfiGovBondHoldings - corpBondHoldings - nbfi.tfiEquityHoldings).max(PLN.Zero)
    NbfiFundBalances(
      tfiUnit = nbfi.tfiAum,
      govBondHoldings = nbfi.tfiGovBondHoldings,
      corpBondHoldings = corpBondHoldings,
      equityHoldings = nbfi.tfiEquityHoldings,
      cashHoldings = cashHoldings,
      nbfiLoanStock = nbfi.nbfiLoanStock,
    )

  def quasiFiscalStock(ledgerFinancialState: LedgerFinancialState): QuasiFiscal.StockState =
    QuasiFiscal.StockState(
      bondsOutstanding = ledgerFinancialState.funds.quasiFiscal.bondsOutstanding,
      loanPortfolio = ledgerFinancialState.funds.quasiFiscal.loanPortfolio,
    )

  def quasiFiscalBalances(quasiFiscal: QuasiFiscal.StockState): QuasiFiscalBalances =
    QuasiFiscalBalances(
      bondsOutstanding = quasiFiscal.bondsOutstanding,
      loanPortfolio = quasiFiscal.loanPortfolio,
    )

  def nbpBalances(balances: Nbp.FinancialBalances): NbpBalances =
    NbpBalances(
      govBondHoldings = balances.govBondHoldings,
      foreignAssets = balances.foreignAssets,
    )

  def fundBalances(
      social: SocialState,
      corporateBonds: CorporateBondMarket.StockState,
      nbfi: Nbfi.ClosingBalances,
      quasiFiscal: QuasiFiscal.StockState,
  ): FundBalances =
    FundBalances(
      zusCash = social.zus.fusBalance,
      nfzCash = social.nfz.balance,
      ppkGovBondHoldings = social.ppk.bondHoldings,
      ppkCorpBondHoldings = corporateBonds.ppkHoldings,
      fpCash = social.earmarked.fpBalance,
      pfronCash = social.earmarked.pfronBalance,
      fgspCash = social.earmarked.fgspBalance,
      jstCash = social.jst.deposits,
      corpBondOtherHoldings = corporateBonds.otherHoldings,
      nbfi = nbfiFundBalances(nbfi, corporateBonds.nbfiHoldings),
      quasiFiscal = quasiFiscalBalances(quasiFiscal),
    )

  private def initialFirmBalances(firm: Firm.State, corpBond: PLN): FirmBalances =
    firmBalances(firm.financial, corpBond)

  private def initialHouseholdBalances(household: Household.State): HouseholdBalances =
    householdBalances(household.financial)

  /** Ledger-backed financial balances owned by a single household. */
  case class HouseholdBalances(
      /** Bank demand deposits owned by the household. */
      demandDeposit: PLN,
      /** Outstanding mortgage principal owed by the household. */
      mortgageLoan: PLN,
      /** Outstanding consumer-loan principal owed by the household. */
      consumerLoan: PLN,
      /** Listed equity owned by the household. */
      equity: PLN,
  )

  /** Ledger-backed financial balances owned or issued by a single firm.
    */
  case class FirmBalances(
      /** Cash or deposit-like liquidity owned by the firm. */
      cash: PLN,
      /** Outstanding bank-loan principal owed by the firm. */
      firmLoan: PLN,
      /** Corporate bonds issued by the firm. */
      corpBond: PLN,
      /** Listed equity issued by the firm. */
      equity: PLN,
  )

  /** Ledger-backed financial balances owned or issued by a single bank.
    */
  case class BankBalances(
      /** Total customer deposits owed by the bank. */
      totalDeposits: PLN,
      /** Demand-deposit liabilities owed by the bank. */
      demandDeposit: PLN,
      /** Term-deposit liabilities owed by the bank. */
      termDeposit: PLN,
      /** Firm-loan assets owned by the bank. */
      firmLoan: PLN,
      /** Consumer-loan assets owned by the bank. */
      consumerLoan: PLN,
      /** Available-for-sale government bonds owned by the bank. */
      govBondAfs: PLN,
      /** Held-to-maturity government bonds owned by the bank. */
      govBondHtm: PLN,
      /** Reserve balance held by the bank at the NBP. */
      reserve: PLN,
      /** Interbank-loan balance owned or owed by the bank. */
      interbankLoan: PLN,
      /** Corporate bonds owned by the bank. */
      corpBond: PLN,
  )

  /** Ledger-backed financial balances issued by central government. */
  case class GovernmentBalances(
      /** Outstanding central-government bond principal. */
      govBondOutstanding: PLN,
  )

  /** Ledger-backed financial balances owned by the foreign sector. */
  case class ForeignBalances(
      /** Central-government bonds held by foreign investors. */
      govBondHoldings: PLN,
  )

  /** Ledger-backed financial balances owned by the central bank. */
  case class NbpBalances(
      /** Central-government bonds held by the NBP. */
      govBondHoldings: PLN,
      /** Foreign-asset stock held by the NBP. */
      foreignAssets: PLN,
  )

  /** Ledger-backed financial balances owned or owed by the insurance sector.
    */
  case class InsuranceBalances(
      /** Life-insurance technical reserves owed to policyholders. */
      lifeReserve: PLN,
      /** Non-life technical reserves owed to policyholders. */
      nonLifeReserve: PLN,
      /** Central-government bonds held by insurers. */
      govBondHoldings: PLN,
      /** Corporate bonds held by insurers. */
      corpBondHoldings: PLN,
      /** Listed equity held by insurers. */
      equityHoldings: PLN,
  )

  /** Ledger-backed financial balances owned or owed by an NBFI fund bucket.
    */
  case class NbfiFundBalances(
      /** Investment-fund units issued by the NBFI bucket. */
      tfiUnit: PLN,
      /** Central-government bonds held by the NBFI bucket. */
      govBondHoldings: PLN,
      /** Corporate bonds held by the NBFI bucket. */
      corpBondHoldings: PLN,
      /** Listed equity held by the NBFI bucket. */
      equityHoldings: PLN,
      /** Cash held by the NBFI bucket. */
      cashHoldings: PLN,
      /** NBFI loan portfolio stock. */
      nbfiLoanStock: PLN,
  )

  /** Ledger-backed financial balances for quasi-fiscal investment vehicles.
    */
  case class QuasiFiscalBalances(
      /** Bonds issued by quasi-fiscal vehicles. */
      bondsOutstanding: PLN,
      /** Loan portfolio owned by quasi-fiscal vehicles. */
      loanPortfolio: PLN,
  )

  /** Ledger-backed financial balances for non-agent public and fund buckets.
    */
  case class FundBalances(
      /** ZUS cash balance. */
      zusCash: PLN,
      /** NFZ cash balance. */
      nfzCash: PLN,
      /** PPK central-government bond holdings. */
      ppkGovBondHoldings: PLN,
      /** PPK corporate-bond holdings. */
      ppkCorpBondHoldings: PLN,
      /** Labour Fund cash balance. */
      fpCash: PLN,
      /** PFRON cash balance. */
      pfronCash: PLN,
      /** FGSP cash balance. */
      fgspCash: PLN,
      /** Local-government cash balance. */
      jstCash: PLN,
      /** Other fund-sector corporate-bond holdings. */
      corpBondOtherHoldings: PLN,
      /** NBFI fund-bucket financial balances. */
      nbfi: NbfiFundBalances,
      /** Quasi-fiscal vehicle financial balances. */
      quasiFiscal: QuasiFiscalBalances,
  )
