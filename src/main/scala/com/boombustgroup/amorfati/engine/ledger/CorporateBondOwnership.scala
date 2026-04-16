package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.agents.{Firm, Insurance, Nbfi}
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.markets.CorporateBondMarket
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.Distribute

/** Ledger-owned corporate bond stock mechanics.
  *
  * Corporate bond market code computes pricing, coupons, absorption and holder
  * allocation. This object owns the stock-side invariant: outstanding corporate
  * bonds are the sum of issuer liabilities carried by firms.
  */
object CorporateBondOwnership:

  def issuerOutstanding(firms: Vector[Firm.State]): PLN =
    PLN.fromRaw(firms.iterator.map(_.bondDebt.toLong).sum)

  def issuerOutstanding(ledgerFinancialState: LedgerFinancialState): PLN =
    PLN.fromRaw(ledgerFinancialState.firms.iterator.map(_.corpBond.toLong).sum)

  def holderOutstanding(ledgerFinancialState: LedgerFinancialState): PLN =
    ledgerFinancialState.banks.foldLeft(PLN.Zero)((acc, bank) => acc + bank.corpBond) +
      ledgerFinancialState.insurance.corpBondHoldings +
      ledgerFinancialState.funds.ppkCorpBondHoldings +
      ledgerFinancialState.funds.corpBondOtherHoldings +
      ledgerFinancialState.funds.nbfi.corpBondHoldings

  def stockStateFromLedger(
      ledgerFinancialState: LedgerFinancialState,
  ): CorporateBondMarket.StockState =
    CorporateBondMarket.StockState(
      outstanding = issuerOutstanding(ledgerFinancialState),
      bankHoldings = ledgerFinancialState.banks.foldLeft(PLN.Zero)((acc, bank) => acc + bank.corpBond),
      ppkHoldings = ledgerFinancialState.funds.ppkCorpBondHoldings,
      otherHoldings = ledgerFinancialState.funds.corpBondOtherHoldings,
      insuranceHoldings = ledgerFinancialState.insurance.corpBondHoldings,
      nbfiHoldings = ledgerFinancialState.funds.nbfi.corpBondHoldings,
    )

  def initializeIssuerDebt(firms: Vector[Firm.State], outstanding: PLN)(using p: SimParams): Vector[Firm.State] =
    if outstanding <= PLN.Zero || firms.isEmpty then firms
    else
      val eligible = firms.zipWithIndex.filter: (firm, _) =>
        Firm.isAlive(firm) && Firm.workerCount(firm) >= p.corpBond.minSize
      val issuers  = if eligible.nonEmpty then eligible else firms.zipWithIndex
      allocateDebt(firms, issuers, outstanding)

  def clearDefaultedIssuerDebt(firms: Vector[Firm.State], defaultedFirmIds: Set[FirmId]): Vector[Firm.State] =
    if defaultedFirmIds.isEmpty then firms
    else
      firms.map: firm =>
        if defaultedFirmIds.contains(firm.id) then firm.copy(bondDebt = PLN.Zero)
        else firm

  def applyAmortization(firms: Vector[Firm.State], amortization: PLN): Vector[Firm.State] =
    if amortization <= PLN.Zero then firms
    else
      val issuers = firms.zipWithIndex.filter: (firm, _) =>
        Firm.isAlive(firm) && firm.bondDebt > PLN.Zero
      reduceDebt(firms, issuers, amortization)

  def alignInsuranceStock(stock: Insurance.StockState, corpBondHoldings: PLN): Insurance.StockState =
    stock.copy(corpBondHoldings = corpBondHoldings)

  def alignNbfiStock(stock: Nbfi.StockState, corpBondHoldings: PLN): Nbfi.StockState =
    val cashHoldings = (stock.tfiAum - stock.tfiGovBondHoldings - corpBondHoldings - stock.tfiEquityHoldings).max(PLN.Zero)
    stock.copy(
      tfiCorpBondHoldings = corpBondHoldings,
      tfiCashHoldings = cashHoldings,
    )

  private def allocateDebt(
      firms: Vector[Firm.State],
      issuers: Vector[(Firm.State, Int)],
      amount: PLN,
  ): Vector[Firm.State] =
    if issuers.isEmpty then firms
    else
      val weights     = positiveWeights(issuers)
      val allocations = Distribute.distribute(amount.distributeRaw, weights)
      issuers.zip(allocations).foldLeft(firms) { case (acc, ((firm, index), rawAmount)) =>
        acc.updated(index, firm.copy(bondDebt = firm.bondDebt + PLN.fromRaw(rawAmount)))
      }

  private def reduceDebt(
      firms: Vector[Firm.State],
      issuers: Vector[(Firm.State, Int)],
      requestedReduction: PLN,
  ): Vector[Firm.State] =
    val totalDebt = PLN.fromRaw(issuers.iterator.map(_._1.bondDebt.toLong).sum)
    if issuers.isEmpty || totalDebt <= PLN.Zero then firms
    else
      val actualReduction = requestedReduction.min(totalDebt)
      val allocations     = Distribute.distribute(actualReduction.distributeRaw, issuers.map(_._1.bondDebt.distributeRaw).toArray)
      issuers.zip(allocations).foldLeft(firms) { case (acc, ((firm, index), rawAmount)) =>
        val reduction = PLN.fromRaw(rawAmount).min(firm.bondDebt)
        acc.updated(index, firm.copy(bondDebt = firm.bondDebt - reduction))
      }

  private def positiveWeights(issuers: Vector[(Firm.State, Int)]): Array[Long] =
    val capitalWeights = issuers.map(_._1.capitalStock.distributeRaw.max(0L)).toArray
    if capitalWeights.exists(_ > 0L) then capitalWeights
    else Array.fill(issuers.length)(1L)
