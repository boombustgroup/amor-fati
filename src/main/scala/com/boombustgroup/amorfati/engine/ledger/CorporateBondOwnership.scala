package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.agents.Firm
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.markets.CorporateBondMarket
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.Distribute

/** Ledger-owned corporate bond stock mechanics.
  *
  * Corporate bond market code computes pricing, coupons, absorption and holder
  * allocation. This object owns the stock-side invariant: outstanding corporate
  * bonds are the sum of issuer liabilities carried by ledger firm balances.
  */
object CorporateBondOwnership:

  def issuerOutstanding(firms: Vector[LedgerFinancialState.FirmBalances]): PLN =
    firms.iterator.map(_.corpBond).sumPln

  def issuerOutstanding(ledgerFinancialState: LedgerFinancialState): PLN =
    issuerOutstanding(ledgerFinancialState.firms)

  def issuerBalanceFor(ledgerFinancialState: LedgerFinancialState, firmId: FirmId): PLN =
    ledgerFinancialState.firms.lift(firmId.toInt).fold(PLN.Zero)(_.corpBond)

  def holderOutstanding(ledgerFinancialState: LedgerFinancialState): PLN =
    bankHolderOutstanding(ledgerFinancialState) +
      ledgerFinancialState.insurance.corpBondHoldings +
      ledgerFinancialState.funds.ppkCorpBondHoldings +
      ledgerFinancialState.funds.corpBondOtherHoldings +
      ledgerFinancialState.funds.nbfi.corpBondHoldings

  def bankHolderOutstanding(ledgerFinancialState: LedgerFinancialState): PLN =
    ledgerFinancialState.banks.foldLeft(PLN.Zero)((acc, bank) => acc + bank.corpBond)

  def bankHolderBalances(ledgerFinancialState: LedgerFinancialState): Vector[PLN] =
    ledgerFinancialState.banks.map(_.corpBond)

  def bankHolderFor(ledgerFinancialState: LedgerFinancialState, bankId: BankId): PLN =
    ledgerFinancialState.banks.lift(bankId.toInt).fold(PLN.Zero)(_.corpBond)

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

  def initializeIssuerBalances(
      firms: Vector[Firm.State],
      financialStocks: Vector[Firm.FinancialStocks],
      outstanding: PLN,
  )(using p: SimParams): Vector[LedgerFinancialState.FirmBalances] =
    require(
      firms.length == financialStocks.length,
      s"CorporateBondOwnership.initializeIssuerBalances requires aligned firms and financial stocks, got ${firms.length} firms and ${financialStocks.length} stock rows",
    )
    val base = financialStocks.map(stocks => LedgerFinancialState.firmBalances(stocks, corpBond = PLN.Zero))
    if outstanding <= PLN.Zero || firms.isEmpty then base
    else
      val eligible = firms.zipWithIndex.filter: (firm, _) =>
        Firm.isAlive(firm) && Firm.workerCount(firm) >= p.corpBond.minSize
      val issuers  = if eligible.nonEmpty then eligible else firms.zipWithIndex
      allocateDebt(base, issuers, outstanding)

  def applyIssuance(
      firms: Vector[LedgerFinancialState.FirmBalances],
      issuanceByFirm: Map[FirmId, PLN],
  ): Vector[LedgerFinancialState.FirmBalances] =
    if issuanceByFirm.isEmpty then firms
    else
      firms.zipWithIndex.map: (balance, index) =>
        val issuance = issuanceByFirm.getOrElse(FirmId(index), PLN.Zero)
        if issuance > PLN.Zero then balance.copy(corpBond = balance.corpBond + issuance)
        else balance

  def defaultedIssuerDebt(
      firms: Vector[LedgerFinancialState.FirmBalances],
      defaultedFirmIds: Set[FirmId],
  ): PLN =
    if defaultedFirmIds.isEmpty then PLN.Zero
    else
      defaultedFirmIds.iterator
        .flatMap(id => firms.lift(id.toInt))
        .map(_.corpBond)
        .sumPln

  def clearDefaultedIssuerDebt(
      firms: Vector[LedgerFinancialState.FirmBalances],
      defaultedFirmIds: Set[FirmId],
  ): Vector[LedgerFinancialState.FirmBalances] =
    if defaultedFirmIds.isEmpty then firms
    else
      firms.zipWithIndex.map: (balance, index) =>
        if defaultedFirmIds.contains(FirmId(index)) then balance.copy(corpBond = PLN.Zero)
        else balance

  def applyAmortization(
      firms: Vector[LedgerFinancialState.FirmBalances],
      firmStates: Vector[Firm.State],
      amortization: PLN,
  ): Vector[LedgerFinancialState.FirmBalances] =
    if amortization <= PLN.Zero then firms
    else
      val issuers = firmStates
        .zip(firms)
        .zipWithIndex
        .collect:
          case ((firm, balance), index) if Firm.isAlive(firm) && balance.corpBond > PLN.Zero => (balance, firm, index)
      reduceDebt(firms, issuers, amortization)

  private def allocateDebt(
      firms: Vector[LedgerFinancialState.FirmBalances],
      issuers: Vector[(Firm.State, Int)],
      amount: PLN,
  ): Vector[LedgerFinancialState.FirmBalances] =
    if issuers.isEmpty then firms
    else
      val weights     = positiveWeights(issuers)
      val allocations = Distribute.distribute(amount.distributeRaw, weights)
      issuers.zip(allocations).foldLeft(firms) { case (acc, ((_, index), rawAmount)) =>
        val balance = acc(index)
        acc.updated(index, balance.copy(corpBond = balance.corpBond + PLN.fromRaw(rawAmount)))
      }

  private def reduceDebt(
      firms: Vector[LedgerFinancialState.FirmBalances],
      issuers: Vector[(LedgerFinancialState.FirmBalances, Firm.State, Int)],
      requestedReduction: PLN,
  ): Vector[LedgerFinancialState.FirmBalances] =
    val totalDebt = issuers.iterator.map(_._1.corpBond).sumPln
    if issuers.isEmpty || totalDebt <= PLN.Zero then firms
    else
      val actualReduction = requestedReduction.min(totalDebt)
      val allocations     = Distribute.distribute(actualReduction.distributeRaw, issuers.map(_._1.corpBond.distributeRaw).toArray)
      issuers.zip(allocations).foldLeft(firms) { case (acc, ((balance, _, index), rawAmount)) =>
        val reduction = PLN.fromRaw(rawAmount).min(balance.corpBond)
        acc.updated(index, balance.copy(corpBond = balance.corpBond - reduction))
      }

  private def positiveWeights(issuers: Vector[(Firm.State, Int)]): Array[Long] =
    val capitalWeights = issuers.map(_._1.capitalStock.distributeRaw.max(0L)).toArray
    if capitalWeights.exists(_ > 0L) then capitalWeights
    else Array.fill(issuers.length)(1L)
