package com.boombustgroup.amorfati.engine.ledger

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
    bankHoldingsByBank: Vector[PLN] = Vector.empty,
):
  def totalHoldings: PLN =
    bankHoldings + foreignHoldings + nbpHoldings + insuranceHoldings + ppkHoldings + tfiHoldings

object GovernmentBondCircuit:
  def from(ledgerFinancialState: LedgerFinancialState): GovernmentBondCircuit =
    val bankHoldingsByBank = ledgerFinancialState.banks.map(bank => bank.govBondAfs + bank.govBondHtm)
    val bankHoldings       = bankHoldingsByBank.foldLeft(PLN.Zero)(_ + _)
    GovernmentBondCircuit(
      outstanding = ledgerFinancialState.government.govBondOutstanding,
      bankHoldings = bankHoldings,
      foreignHoldings = ledgerFinancialState.foreign.govBondHoldings,
      nbpHoldings = ledgerFinancialState.nbp.govBondHoldings,
      insuranceHoldings = ledgerFinancialState.insurance.govBondHoldings,
      ppkHoldings = ledgerFinancialState.funds.ppkGovBondHoldings,
      tfiHoldings = ledgerFinancialState.funds.nbfi.govBondHoldings,
      bankHoldingsByBank = bankHoldingsByBank,
    )
