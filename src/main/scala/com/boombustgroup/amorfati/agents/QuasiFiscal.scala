package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

/** Quasi-fiscal entities: BGK (Bank Gospodarstwa Krajowego) + PFR (Polski
  * Fundusz Rozwoju), modelled as a single consolidated agent.
  *
  * BGK/PFR issue state-guaranteed bonds that are NOT counted in MF public debt
  * but ARE counted in ESA 2010 (Eurostat) debt. During COVID, NBP bought ~106
  * mld PLN of BGK/PFR bonds (Tarcze Antykryzysowe) — de facto monetization
  * without formal QE on SPW.
  *
  * Three channels:
  *
  *   1. '''Bond issuance''' — new bonds = f(govCapitalSpend, euProjectCapital).
  *      BGK/PFR finance infrastructure and crisis programs off MF balance
  *      sheet. Bonds are absorbed by banks (competing with SPW) and NBP
  *      (quasi-QE).
  *   2. '''Lending''' — subsidized credit for firms and JST (local government).
  *      Competes with commercial bank lending. Lower rate (gov guarantee →
  *      cheaper funding), directed at policy priorities.
  *   3. '''NBP quasi-QE''' — NBP buys BGK/PFR bonds in crisis mode. Separate
  *      from SPW QE program. Monetizes deficit without appearing in MF
  *      statistics.
  *
  * SFC: quasi-fiscal bonds are a separate stock from SPW. Bond clearing
  * identity: `qfOutstanding = qfBankHoldings + qfNbpHoldings`. Lending creates
  * deposits (same as commercial bank lending).
  *
  * Calibration: NIK reports, BGK annual reports, PFR Tarcze data, NBP bond
  * purchase program data.
  */
object QuasiFiscal:

  /** Ledger-owned quasi-fiscal stock balances. */
  case class StockState(
      bondsOutstanding: PLN, // total BGK/PFR bonds outstanding
      loanPortfolio: PLN,    // outstanding subsidized loans to firms/JST
  )
  object StockState:
    val zero: StockState = StockState(PLN.Zero, PLN.Zero)

  /** Quasi-fiscal market memory and unsupported holder split. */
  case class State(
      bankHoldings: PLN,    // commercial bank holdings of quasi-fiscal bonds
      nbpHoldings: PLN,     // NBP holdings of quasi-fiscal bonds
      monthlyIssuance: PLN, // this month's new bond issuance
      monthlyLending: PLN,  // this month's new lending
  )
  object State:
    val zero: State = State(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)

  case class StepResult(
      state: State,
      stock: StockState,
  )

  /** Monthly step: issuance, lending, NBP absorption.
    *
    * Issuance = share of government capital spending routed through BGK/PFR
    * (off-balance-sheet). NBP buys a fraction when quasi-QE is active. Banks
    * absorb the rest. Lending = fraction of issuance directed to subsidized
    * credit.
    */
  def step(
      prev: State,
      prevStock: StockState,
      govCapitalSpend: PLN,
      euProjectCapital: PLN,
      nbpQeActive: Boolean,
  )(using p: SimParams): StepResult =
    // Issuance: BGK/PFR finances a share of government capital programs off-balance-sheet
    val issuance: PLN = ((govCapitalSpend + euProjectCapital) * p.quasiFiscal.issuanceShare).max(PLN.Zero)

    // Amortization: bonds mature at 1/avgMaturity per month
    val amortFrac: Share  = Share.One / p.quasiFiscal.avgMaturityMonths.max(1)
    val amortization: PLN = prevStock.bondsOutstanding * amortFrac

    // NBP absorption: in crisis mode, NBP buys a share of new issuance
    val nbpPurchase: PLN =
      if nbpQeActive then issuance * p.quasiFiscal.nbpAbsorptionShare
      else PLN.Zero

    // Banks absorb the rest
    val bankPurchase: PLN = issuance - nbpPurchase

    // Lending: fraction of outstanding portfolio directed to subsidized credit
    val lendingGrowth: PLN    = issuance * p.quasiFiscal.lendingShare
    val loanAmortFrac: Share  = Share.One / p.quasiFiscal.loanMaturityMonths.max(1)
    val lendingAmort: PLN     = prevStock.loanPortfolio * loanAmortFrac
    val newLoanPortfolio: PLN = (prevStock.loanPortfolio + lendingGrowth - lendingAmort).max(PLN.Zero)

    val newOutstanding: PLN  = (prevStock.bondsOutstanding + issuance - amortization).max(PLN.Zero)
    val newBankHoldings: PLN = (prev.bankHoldings + bankPurchase - amortization * bankShareOf(prev, prevStock)).max(PLN.Zero)
    val newNbpHoldings: PLN  = (prev.nbpHoldings + nbpPurchase - amortization * nbpShareOf(prev, prevStock)).max(PLN.Zero)

    StepResult(
      state = State(
        bankHoldings = newBankHoldings,
        nbpHoldings = newNbpHoldings,
        monthlyIssuance = issuance,
        monthlyLending = lendingGrowth,
      ),
      stock = StockState(
        bondsOutstanding = newOutstanding,
        loanPortfolio = newLoanPortfolio,
      ),
    )

  /** Bank share of outstanding (for amortization split). */
  private def bankShareOf(s: State, stock: StockState): Share =
    if stock.bondsOutstanding > PLN.Zero then Share(s.bankHoldings / stock.bondsOutstanding).clamp(Share.Zero, Share.One)
    else Share(0.5)

  /** NBP share of outstanding (for amortization split). */
  private def nbpShareOf(s: State, stock: StockState): Share =
    if stock.bondsOutstanding > PLN.Zero then Share(s.nbpHoldings / stock.bondsOutstanding).clamp(Share.Zero, Share.One)
    else Share(0.5)

  /** ESA 2010 debt: MF debt + quasi-fiscal outstanding. */
  def esa2010Debt(govCumulativeDebt: PLN, qfOutstanding: PLN): PLN =
    govCumulativeDebt + qfOutstanding
