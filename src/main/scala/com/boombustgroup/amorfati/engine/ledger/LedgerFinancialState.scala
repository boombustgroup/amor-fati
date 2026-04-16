package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.agents.{Banking, Firm, Household, Insurance, Nbfi, Nbp, QuasiFiscal}
import com.boombustgroup.amorfati.engine.{SocialState, World}
import com.boombustgroup.amorfati.engine.markets.{CorporateBondMarket, FiscalBudget}
import com.boombustgroup.amorfati.types.PLN

/** Ledger-owned snapshot of ledger-contracted financial stocks used by the
  * engine.
  *
  * This file defines the runtime state that should become the source of truth
  * for financial ownership with an explicit ledger contract. It is
  * intentionally narrower than `World`: it stores ledger-backed stock balances,
  * not monthly flow deltas, execution-only settlement shells, or public assets
  * that the engine still keeps outside the ledger contract. Boundary code may
  * project these balances back into `World` mirrors while the migration is in
  * progress.
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

  def householdBalances(h: Household.State): HouseholdBalances =
    HouseholdBalances(
      demandDeposit = h.savings,
      mortgageLoan = h.debt,
      consumerLoan = h.consumerDebt,
      equity = h.equityWealth,
    )

  def firmBalances(f: Firm.State): FirmBalances =
    FirmBalances(
      cash = f.cash,
      firmLoan = f.debt,
      corpBond = f.bondDebt,
      equity = f.equityRaised,
    )

  def bankBalances(b: Banking.BankState): BankBalances =
    BankBalances(
      totalDeposits = b.deposits,
      demandDeposit = bankDemandDeposits(b),
      termDeposit = bankTermDeposits(b),
      firmLoan = b.loans,
      consumerLoan = b.consumerLoans,
      govBondAfs = b.afsBonds,
      govBondHtm = b.htmBonds,
      reserve = b.reservesAtNbp,
      interbankLoan = b.interbankNet,
      corpBond = b.corpBondHoldings,
    )

  def governmentBalances(gov: FiscalBudget.GovState): GovernmentBalances =
    GovernmentBalances(govBondOutstanding = gov.bondsOutstanding)

  def foreignBalances(gov: FiscalBudget.GovState): ForeignBalances =
    ForeignBalances(govBondHoldings = gov.foreignBondHoldings)

  def nbpBalances(nbp: Nbp.State): NbpBalances =
    NbpBalances(
      govBondHoldings = nbp.govBondHoldings,
      foreignAssets = nbp.fxReserves,
    )

  def insuranceBalances(insurance: Insurance.State): InsuranceBalances =
    InsuranceBalances(
      lifeReserve = insurance.lifeReserves,
      nonLifeReserve = insurance.nonLifeReserves,
      govBondHoldings = insurance.govBondHoldings,
      corpBondHoldings = insurance.corpBondHoldings,
      equityHoldings = insurance.equityHoldings,
    )

  def nbfiFundBalances(nbfi: Nbfi.State): NbfiFundBalances =
    NbfiFundBalances(
      tfiUnit = nbfi.tfiAum,
      govBondHoldings = nbfi.tfiGovBondHoldings,
      corpBondHoldings = nbfi.tfiCorpBondHoldings,
      equityHoldings = nbfi.tfiEquityHoldings,
      cashHoldings = nbfi.tfiCashHoldings,
      nbfiLoanStock = nbfi.nbfiLoanStock,
    )

  def quasiFiscalBalances(quasiFiscal: QuasiFiscal.State): QuasiFiscalBalances =
    QuasiFiscalBalances(
      bondsOutstanding = quasiFiscal.bondsOutstanding,
      loanPortfolio = quasiFiscal.loanPortfolio,
    )

  def fundBalances(
      social: SocialState,
      corporateBonds: CorporateBondMarket.State,
      nbfi: Nbfi.State,
      quasiFiscal: QuasiFiscal.State,
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
      nbfi = nbfiFundBalances(nbfi),
      quasiFiscal = quasiFiscalBalances(quasiFiscal),
    )

  /** Init/bootstrap-only import from the still-existing mirror state.
    *
    * Normal month transitions must carry `LedgerFinancialState` forward and
    * project it into boundary views; they must not use this method to
    * round-trip through `World`.
    */
  def bootstrapFromMirrors(
      world: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      banks: Vector[Banking.BankState],
  ): LedgerFinancialState =
    LedgerFinancialState(
      households = households.map(householdBalances),
      firms = firms.map(firmBalances),
      banks = banks.map(bankBalances),
      government = governmentBalances(world.gov),
      foreign = foreignBalances(world.gov),
      nbp = nbpBalances(world.nbp),
      insurance = insuranceBalances(world.financial.insurance),
      funds = fundBalances(world.social, world.financial.corporateBonds, world.financial.nbfi, world.financial.quasiFiscal),
    )

  private def bankDemandDeposits(bank: Banking.BankState): PLN =
    if bank.demandDeposits == PLN.Zero && bank.termDeposits == PLN.Zero then bank.deposits
    else bank.demandDeposits

  private def bankTermDeposits(bank: Banking.BankState): PLN =
    if bank.demandDeposits == PLN.Zero && bank.termDeposits == PLN.Zero then PLN.Zero
    else bank.termDeposits

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

  /** Ledger-backed financial balances owned or issued by a single firm. */
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

  /** Ledger-backed financial balances owned or issued by a single bank. */
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

  /** Ledger-backed financial balances owned or owed by the insurance sector. */
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

  /** Ledger-backed financial balances owned or owed by an NBFI fund bucket. */
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

  /** Ledger-backed financial balances for quasi-fiscal investment vehicles. */
  case class QuasiFiscalBalances(
      /** Bonds issued by quasi-fiscal vehicles. */
      bondsOutstanding: PLN,
      /** Loan portfolio owned by quasi-fiscal vehicles. */
      loanPortfolio: PLN,
  )

  /** Ledger-backed financial balances for non-agent public and fund buckets. */
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
