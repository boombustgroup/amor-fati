package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

/** Earmarked funds: FP, PFRON, FGŚP.
  *
  * Three statutory funds with dedicated contribution→benefit SFC flows:
  *
  *   1. '''Fundusz Pracy (FP)''' — 2.45% employer payroll levy. Finances
  *      unemployment benefits and active labor market policy (ALMP). Currently
  *      unemployment benefits are funded from generic govSpending — FP makes
  *      the funding source explicit. ~30 mld PLN/year (MRPiPS 2024).
  *   2. '''PFRON''' — levy on employers with <6% disability employment share.
  *      Finances disability support and workplace adaptation. ~5.5 mld PLN/year
  *      (PFRON annual report 2024).
  *   3. '''FGŚP (Fundusz Gwarantowanych Świadczeń Pracowniczych)''' — 0.10%
  *      payroll. Pays unpaid wages when firms go bankrupt. Counter-cyclical:
  *      payouts spike during bankruptcy waves. ~0.5 mld PLN/year base, 10×
  *      during COVID (Tarcze). Calibration: FGŚP annual reports.
  *
  * Each fund: contributions from payroll → spending on purpose. Deficit covered
  * by government subvention. Fund cash balances are owned by
  * `LedgerFinancialState`, not by this monthly flow state.
  *
  * Pure functions. Called from LaborDemographicsStep alongside ZUS/NFZ.
  */
object EarmarkedFunds:

  /** Monthly flow state for a single earmarked fund. */
  case class FundState(
      contributions: PLN,
      spending: PLN,
  )

  case class State(
      fp: FundState,
      pfron: FundState,
      fgsp: FundState,
      totalGovSubvention: PLN, // government covers combined deficit
  ):
    def fpContributions: PLN    = fp.contributions
    def fpSpending: PLN         = fp.spending
    def pfronContributions: PLN = pfron.contributions
    def pfronSpending: PLN      = pfron.spending
    def fgspContributions: PLN  = fgsp.contributions
    def fgspSpending: PLN       = fgsp.spending
  object State:
    def apply(
        fpContributions: PLN,
        fpSpending: PLN,
        pfronContributions: PLN,
        pfronSpending: PLN,
        fgspContributions: PLN,
        fgspSpending: PLN,
        totalGovSubvention: PLN,
    ): State =
      State(
        fp = FundState(fpContributions, fpSpending),
        pfron = FundState(pfronContributions, pfronSpending),
        fgsp = FundState(fgspContributions, fgspSpending),
        totalGovSubvention = totalGovSubvention,
      )

    val zero: State = State(
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
    )

  def fpCashChange(state: State): PLN =
    state.fpContributions - state.fpSpending

  def pfronCashChange(state: State): PLN =
    state.pfronContributions - state.pfronSpending

  def fgspCashChange(state: State): PLN =
    state.fgspContributions - state.fgspSpending

  def fpCashAfter(openingCash: PLN, state: State): PLN =
    openingCash + fpCashChange(state)

  def pfronCashAfter(openingCash: PLN, state: State): PLN =
    openingCash + pfronCashChange(state)

  def fgspCashAfter(openingCash: PLN, state: State): PLN =
    openingCash + fgspCashChange(state)

  /** Monthly step: compute contributions and spending for all three funds.
    *
    * @param employed
    *   total employed workers
    * @param wage
    *   market wage
    * @param unempBenefitSpend
    *   unemployment benefits already computed (FP finances these)
    * @param nBankruptFirms
    *   firms that went bankrupt this month (FGŚP trigger)
    * @param avgFirmWorkers
    *   average workers per bankrupt firm
    */
  def step(
      employed: Int,
      wage: PLN,
      unempBenefitSpend: PLN,
      nBankruptFirms: Int,
      avgFirmWorkers: Int,
  )(using p: SimParams): State =
    step(SocialSecurity.PayrollBase.aggregate(employed, wage), unempBenefitSpend, nBankruptFirms, avgFirmWorkers)

  def step(
      payroll: SocialSecurity.PayrollBase,
      unempBenefitSpend: PLN,
      nBankruptFirms: Int,
      avgFirmWorkers: Int,
  )(using p: SimParams): State =
    // Fundusz Pracy: 2.45% employer levy → finances unemployment benefits + ALMP
    val fpContrib = payroll.fpContributions
    val fpSpend   = unempBenefitSpend + payroll.employed * p.earmarked.fpAlmpSpendPerWorker
    val fpFlow    = fpContrib - fpSpend
    val fpSubv    = if fpFlow < PLN.Zero then -fpFlow else PLN.Zero

    // PFRON: flat levy on non-compliant employers
    val pfronContrib = p.earmarked.pfronMonthlyRevenue
    val pfronSpend   = p.earmarked.pfronMonthlySpending
    val pfronFlow    = pfronContrib - pfronSpend
    val pfronSubv    = if pfronFlow < PLN.Zero then -pfronFlow else PLN.Zero

    // FGŚP: 0.10% payroll → pays wages on bankruptcy (counter-cyclical)
    val fgspContrib = payroll.fgspContributions
    val fgspSpend   = (nBankruptFirms * avgFirmWorkers) * p.earmarked.fgspPayoutPerWorker
    val fgspFlow    = fgspContrib - fgspSpend
    val fgspSubv    = if fgspFlow < PLN.Zero then -fgspFlow else PLN.Zero

    State(
      fpContributions = fpContrib,
      fpSpending = fpSpend,
      pfronContributions = pfronContrib,
      pfronSpending = pfronSpend,
      fgspContributions = fgspContrib,
      fgspSpending = fgspSpend,
      totalGovSubvention = fpSubv + pfronSubv + fgspSubv,
    )
