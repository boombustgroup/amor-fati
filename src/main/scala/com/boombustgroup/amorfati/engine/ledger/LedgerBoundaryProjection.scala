package com.boombustgroup.amorfati.engine.ledger

import com.boombustgroup.amorfati.engine.SocialState

/** One-way projection from ledger-owned financial state into boundary views.
  *
  * These methods exist only because parts of the engine still consume `World`
  * and agent mirrors. They must not rebuild `LedgerFinancialState`; the
  * ownership direction is ledger state to boundary view.
  */
object LedgerBoundaryProjection:

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
