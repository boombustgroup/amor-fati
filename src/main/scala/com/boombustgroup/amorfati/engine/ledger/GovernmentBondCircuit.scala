package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.agents.Banking
import com.boombustgroup.amorfati.types.PLN

/** Government-bond issuer/holder stock view used by SFC level checks. */
case class GovernmentBondCircuit(
    outstanding: PLN,
    bankHoldings: PLN,
    foreignHoldings: PLN,
    nbpHoldings: PLN,
    insuranceHoldings: PLN,
    ppkHoldings: PLN,
    tfiHoldings: PLN,
):
  def totalHoldings: PLN =
    bankHoldings + foreignHoldings + nbpHoldings + insuranceHoldings + ppkHoldings + tfiHoldings

object GovernmentBondCircuit:
  def from(banks: Vector[Banking.BankState], ledgerFinancialState: LedgerFinancialState): GovernmentBondCircuit =
    val bankAgg = Banking.aggregateFromBanks(banks)
    GovernmentBondCircuit(
      outstanding = ledgerFinancialState.government.govBondOutstanding,
      bankHoldings = bankAgg.govBondHoldings,
      foreignHoldings = ledgerFinancialState.foreign.govBondHoldings,
      nbpHoldings = ledgerFinancialState.nbp.govBondHoldings,
      insuranceHoldings = ledgerFinancialState.insurance.govBondHoldings,
      ppkHoldings = ledgerFinancialState.funds.ppkGovBondHoldings,
      tfiHoldings = ledgerFinancialState.funds.nbfi.govBondHoldings,
    )
