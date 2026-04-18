package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

/** Social security and demographics: ZUS/FUS, PPK, demographics, BGK. */
object SocialSecurity:

  // ---------------------------------------------------------------------------
  // ZUS / FUS
  // ---------------------------------------------------------------------------

  /** ZUS/FUS monthly flow state. FUS cash balance is ledger-owned. */
  case class ZusState(
      contributions: PLN,   // this month's total contributions
      pensionPayments: PLN, // this month's total pension payments
      govSubvention: PLN,   // this month's government subvention (covers deficit)
  )
  object ZusState:
    val zero: ZusState = ZusState(PLN.Zero, PLN.Zero, PLN.Zero)

  /** Compute ZUS monthly flows. Contributions from employed workers, pensions
    * to retirees. FUS deficit covered by government subvention. FUS cash is
    * owned by `LedgerFinancialState`; use `zusCashAfter` at the economics
    * boundary to carry the stock forward.
    */
  def zusStep(employed: Int, wage: PLN, nRetirees: Int)(using p: SimParams): ZusState =
    val contributions = employed * (wage * p.social.zusContribRate * p.social.zusScale)
    val pensions      = nRetirees * p.social.zusBasePension
    val monthlyFlow   = contributions - pensions
    val govSubvention = if monthlyFlow < PLN.Zero then -monthlyFlow else PLN.Zero
    ZusState(contributions, pensions, govSubvention)

  def zusCashChange(zus: ZusState): PLN =
    zus.contributions - zus.pensionPayments

  def zusCashAfter(openingCash: PLN, zus: ZusState): PLN =
    openingCash + zusCashChange(zus)

  // ---------------------------------------------------------------------------
  // NFZ (Narodowy Fundusz Zdrowia — National Health Fund)
  // ---------------------------------------------------------------------------

  /** NFZ monthly flow state. NFZ cash balance is ledger-owned. */
  case class NfzState(
      contributions: PLN, // this month's 9% składka zdrowotna
      spending: PLN,      // this month's health sector contracts
      govSubvention: PLN, // government covers deficit when contributions < spending
  )
  object NfzState:
    val zero: NfzState = NfzState(PLN.Zero, PLN.Zero, PLN.Zero)

  /** Compute NFZ monthly flows. 9% składka zdrowotna from employed workers.
    * Spending = per-capita cost × (working-age + retirees × aging elasticity).
    * Aging drives cost pressure: retirees consume ~2.5× more healthcare.
    * Deficit covered by government subvention. NFZ cash is owned by
    * `LedgerFinancialState`; use `nfzCashAfter` at the economics boundary to
    * carry the stock forward.
    */
  def nfzStep(employed: Int, wage: PLN, workingAge: Int, nRetirees: Int)(using p: SimParams): NfzState =
    val contributions = employed * (wage * p.social.nfzContribRate)
    val spending      = workingAge * p.social.nfzPerCapitaCost + nRetirees * (p.social.nfzPerCapitaCost * p.social.nfzAgingElasticity)
    val monthlyFlow   = contributions - spending
    val govSubvention = if monthlyFlow < PLN.Zero then -monthlyFlow else PLN.Zero
    NfzState(contributions, spending, govSubvention)

  def nfzCashChange(nfz: NfzState): PLN =
    nfz.contributions - nfz.spending

  def nfzCashAfter(openingCash: PLN, nfz: NfzState): PLN =
    openingCash + nfzCashChange(nfz)

  // ---------------------------------------------------------------------------
  // PPK
  // ---------------------------------------------------------------------------

  /** PPK monthly flow state. Government-bond ownership is ledger-owned. */
  case class PpkState(
      contributions: PLN, // this month's total PPK contributions
  )
  object PpkState:
    val zero: PpkState = PpkState(PLN.Zero)

  /** Compute PPK monthly contributions. PPK buys government bonds proportional
    * to contributions × bond allocation. Does NOT affect bank deposits (PPK is
    * a pass-through bond market participant).
    */
  def ppkStep(employed: Int, wage: PLN)(using p: SimParams): PpkState =
    val contributions = employed * (wage * (p.social.ppkEmployeeRate + p.social.ppkEmployerRate))
    PpkState(contributions)

  /** PPK bond purchase this month: contributions × bond allocation. */
  def ppkBondPurchase(ppk: PpkState)(using p: SimParams): PLN =
    ppk.contributions * p.social.ppkBondAlloc

  // ---------------------------------------------------------------------------
  // Demographics
  // ---------------------------------------------------------------------------

  /** Demographics state: retirees and working-age population. */
  case class DemographicsState(
      retirees: Int,          // total retired workers receiving pensions
      workingAgePop: Int,     // effective working-age population
      monthlyRetirements: Int, // new retirements this month
  )
  object DemographicsState:
    val zero: DemographicsState = DemographicsState(0, 0, 0)

  /** Compute demographics monthly step. Monthly retirements reduce labor
    * supply; working-age population declines.
    */
  def demographicsStep(prev: DemographicsState, employed: Int, netMigration: Int)(using p: SimParams): DemographicsState =
    val retirements       = p.social.demRetirementRate.applyTo(employed)
    val workingAgeDecline = p.social.demWorkingAgeDecline.monthly.applyTo(prev.workingAgePop)
    DemographicsState(
      retirees = prev.retirees + retirements,
      workingAgePop = Math.max(0, prev.workingAgePop - retirements - workingAgeDecline + netMigration),
      monthlyRetirements = retirements,
    )
