package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.agents.{Insurance, Nbfi, Nbp, QuasiFiscal}
import com.boombustgroup.amorfati.engine.SocialState
import com.boombustgroup.amorfati.engine.markets.{CorporateBondMarket, FiscalBudget}

/** One-way projection from ledger-owned financial state into boundary views.
  *
  * These methods exist only because parts of the engine still consume `World`
  * and agent mirrors. They must not rebuild `LedgerFinancialState`; the
  * ownership direction is ledger state to boundary view.
  */
object LedgerBoundaryProjection:

  def insuranceState(
      base: Insurance.State,
      ledgerFinancialState: LedgerFinancialState,
  ): Insurance.State =
    base.copy(
      reserves = base.reserves.copy(
        lifeReserves = ledgerFinancialState.insurance.lifeReserve,
        nonLifeReserves = ledgerFinancialState.insurance.nonLifeReserve,
      ),
      portfolio = base.portfolio.copy(
        govBondHoldings = ledgerFinancialState.insurance.govBondHoldings,
        corpBondHoldings = ledgerFinancialState.insurance.corpBondHoldings,
        equityHoldings = ledgerFinancialState.insurance.equityHoldings,
      ),
    )

  def govState(
      base: FiscalBudget.GovState,
      ledgerFinancialState: LedgerFinancialState,
  ): FiscalBudget.GovState =
    base.copy(
      financial = base.financial.copy(
        bondsOutstanding = ledgerFinancialState.government.govBondOutstanding,
        foreignBondHoldings = ledgerFinancialState.foreign.govBondHoldings,
      ),
    )

  def nbpState(
      base: Nbp.State,
      ledgerFinancialState: LedgerFinancialState,
  ): Nbp.State =
    base.copy(
      balance = base.balance.copy(
        govBondHoldings = ledgerFinancialState.nbp.govBondHoldings,
        fxReserves = ledgerFinancialState.nbp.foreignAssets,
      ),
    )

  def socialState(
      base: SocialState,
      ledgerFinancialState: LedgerFinancialState,
  ): SocialState =
    base.copy(
      jst = base.jst.copy(deposits = ledgerFinancialState.funds.jstCash),
      zus = base.zus.copy(fusBalance = ledgerFinancialState.funds.zusCash),
      nfz = base.nfz.copy(balance = ledgerFinancialState.funds.nfzCash),
      ppk = base.ppk.copy(bondHoldings = ledgerFinancialState.funds.ppkGovBondHoldings),
      earmarked = base.earmarked.copy(
        fp = base.earmarked.fp.copy(balance = ledgerFinancialState.funds.fpCash),
        pfron = base.earmarked.pfron.copy(balance = ledgerFinancialState.funds.pfronCash),
        fgsp = base.earmarked.fgsp.copy(balance = ledgerFinancialState.funds.fgspCash),
      ),
    )

  def nbfiState(
      base: Nbfi.State,
      ledgerFinancialState: LedgerFinancialState,
  ): Nbfi.State =
    base.copy(
      tfi = base.tfi.copy(
        tfiAum = ledgerFinancialState.funds.nbfi.tfiUnit,
        tfiGovBondHoldings = ledgerFinancialState.funds.nbfi.govBondHoldings,
        tfiCorpBondHoldings = ledgerFinancialState.funds.nbfi.corpBondHoldings,
        tfiEquityHoldings = ledgerFinancialState.funds.nbfi.equityHoldings,
        tfiCashHoldings = ledgerFinancialState.funds.nbfi.cashHoldings,
      ),
      credit = base.credit.copy(
        nbfiLoanStock = ledgerFinancialState.funds.nbfi.nbfiLoanStock,
      ),
    )

  def corporateBondState(
      base: CorporateBondMarket.State,
      ledgerFinancialState: LedgerFinancialState,
  ): CorporateBondMarket.State =
    CorporateBondOwnership.marketStateFromLedger(base, ledgerFinancialState)

  def quasiFiscalState(
      base: QuasiFiscal.State,
      ledgerFinancialState: LedgerFinancialState,
  ): QuasiFiscal.State =
    base.copy(
      bondsOutstanding = ledgerFinancialState.funds.quasiFiscal.bondsOutstanding,
      loanPortfolio = ledgerFinancialState.funds.quasiFiscal.loanPortfolio,
    )
