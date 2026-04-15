package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.types.PLN

final case class LedgerFinancialState(
    households: Vector[LedgerFinancialState.HouseholdBalances],
    firms: Vector[LedgerFinancialState.FirmBalances],
    banks: Vector[LedgerFinancialState.BankBalances],
    government: LedgerFinancialState.GovernmentBalances,
    foreign: LedgerFinancialState.ForeignBalances,
    nbp: LedgerFinancialState.NbpBalances,
    insurance: LedgerFinancialState.InsuranceBalances,
    funds: LedgerFinancialState.FundBalances,
)

object LedgerFinancialState:

  case class HouseholdBalances(
      demandDeposit: PLN,
      mortgageLoan: PLN,
      consumerLoan: PLN,
      equity: PLN,
  )

  case class FirmBalances(
      cash: PLN,
      firmLoan: PLN,
      corpBond: PLN,
      equity: PLN,
  )

  case class BankBalances(
      totalDeposits: PLN,
      demandDeposit: PLN,
      termDeposit: PLN,
      firmLoan: PLN,
      consumerLoan: PLN,
      govBondAfs: PLN,
      govBondHtm: PLN,
      reserve: PLN,
      interbankLoan: PLN,
      corpBond: PLN,
  )

  case class GovernmentBalances(
      govBondOutstanding: PLN,
  )

  case class ForeignBalances(
      govBondHoldings: PLN,
  )

  case class NbpBalances(
      govBondHoldings: PLN,
      foreignAssets: PLN,
  )

  case class InsuranceBalances(
      lifeReserve: PLN,
      nonLifeReserve: PLN,
      govBondHoldings: PLN,
      corpBondHoldings: PLN,
      equityHoldings: PLN,
  )

  case class NbfiFundBalances(
      tfiUnit: PLN,
      govBondHoldings: PLN,
      corpBondHoldings: PLN,
      equityHoldings: PLN,
      cashHoldings: PLN,
      nbfiLoanStock: PLN,
  )

  case class QuasiFiscalBalances(
      bondsOutstanding: PLN,
      loanPortfolio: PLN,
  )

  case class FundBalances(
      zusCash: PLN,
      nfzCash: PLN,
      ppkGovBondHoldings: PLN,
      ppkCorpBondHoldings: PLN,
      fpCash: PLN,
      pfronCash: PLN,
      fgspCash: PLN,
      jstCash: PLN,
      corpBondOtherHoldings: PLN,
      nbfi: NbfiFundBalances,
      quasiFiscal: QuasiFiscalBalances,
  )
