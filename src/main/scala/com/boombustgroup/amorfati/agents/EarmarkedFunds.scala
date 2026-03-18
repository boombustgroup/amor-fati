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
  * Each fund: contributions from payroll → spending on purpose →
  * surplus/deficit. Deficit covered by government subvention (flows into
  * GovDebt identity).
  *
  * Pure functions. Called from LaborDemographicsStep alongside ZUS/NFZ.
  */
object EarmarkedFunds:

  /** State of all three earmarked funds. */
  case class State(
      fpBalance: PLN,          // Fundusz Pracy cumulative balance
      fpContributions: PLN,    // FP contributions this month
      fpSpending: PLN,         // FP spending this month (unemp benefits + ALMP)
      pfronBalance: PLN,       // PFRON cumulative balance
      pfronContributions: PLN, // PFRON levy this month
      pfronSpending: PLN,      // PFRON disability spending this month
      fgspBalance: PLN,        // FGŚP cumulative balance
      fgspContributions: PLN,  // FGŚP contributions this month
      fgspSpending: PLN,       // FGŚP bankruptcy payouts this month
      totalGovSubvention: PLN, // government covers combined deficit
  )
  object State:
    val zero: State = State(
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
    )

  /** Monthly step: compute contributions, spending, balances for all three
    * funds.
    *
    * @param prev
    *   previous state (balances carried forward)
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
      prev: State,
      employed: Int,
      wage: PLN,
      unempBenefitSpend: PLN,
      nBankruptFirms: Int,
      avgFirmWorkers: Int,
  )(using p: SimParams): State =
    // Fundusz Pracy: 2.45% employer levy → finances unemployment benefits + ALMP
    val fpContrib = wage * p.earmarked.fpRate * employed.toDouble
    val fpSpend   = unempBenefitSpend + p.earmarked.fpAlmpSpendPerWorker * employed.toDouble
    val fpFlow    = fpContrib - fpSpend
    val fpSubv    = if fpFlow < PLN.Zero then -fpFlow else PLN.Zero

    // PFRON: flat levy on non-compliant employers
    val pfronContrib = p.earmarked.pfronMonthlyRevenue
    val pfronSpend   = p.earmarked.pfronMonthlySpending
    val pfronFlow    = pfronContrib - pfronSpend
    val pfronSubv    = if pfronFlow < PLN.Zero then -pfronFlow else PLN.Zero

    // FGŚP: 0.10% payroll → pays wages on bankruptcy (counter-cyclical)
    val fgspContrib = wage * p.earmarked.fgspRate * employed.toDouble
    val fgspSpend   = p.earmarked.fgspPayoutPerWorker * (nBankruptFirms.toDouble * avgFirmWorkers.toDouble)
    val fgspFlow    = fgspContrib - fgspSpend
    val fgspSubv    = if fgspFlow < PLN.Zero then -fgspFlow else PLN.Zero

    State(
      fpBalance = prev.fpBalance + fpFlow,
      fpContributions = fpContrib,
      fpSpending = fpSpend,
      pfronBalance = prev.pfronBalance + pfronFlow,
      pfronContributions = pfronContrib,
      pfronSpending = pfronSpend,
      fgspBalance = prev.fgspBalance + fgspFlow,
      fgspContributions = fgspContrib,
      fgspSpending = fgspSpend,
      totalGovSubvention = fpSubv + pfronSubv + fgspSubv,
    )
