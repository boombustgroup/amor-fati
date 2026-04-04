package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

/** Social security and demographics: ZUS/FUS, PPK, demographics, BGK. */
object SocialSecurity:

  // ---------------------------------------------------------------------------
  // ZUS / FUS
  // ---------------------------------------------------------------------------

  /** ZUS/FUS state: social insurance fund balance and monthly flows. */
  case class ZusState(
      fusBalance: PLN,      // cumulative raw surplus/deficit (contributions − pensions, before gov subvention)
      contributions: PLN,   // this month's total contributions
      pensionPayments: PLN, // this month's total pension payments
      govSubvention: PLN,   // this month's government subvention (covers deficit)
  )
  object ZusState:
    val zero: ZusState = ZusState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)

  /** Compute ZUS monthly flows. Contributions from employed workers, pensions
    * to retirees. FUS deficit covered by government subvention. `fusBalance` is
    * the operational fund balance used by SFC cash identities; it should not be
    * conflated with government debt metrics.
    */
  def zusStep(prevBalance: PLN, employed: Int, wage: PLN, nRetirees: Int)(using p: SimParams): ZusState =
    if !p.flags.zus then ZusState(prevBalance, PLN.Zero, PLN.Zero, PLN.Zero)
    else
      val contributions = employed * (wage * p.social.zusContribRate * p.social.zusScale)
      val pensions      = nRetirees * p.social.zusBasePension
      val monthlyFlow   = contributions - pensions
      val govSubvention = if monthlyFlow < PLN.Zero then -monthlyFlow else PLN.Zero
      val newBalance    = prevBalance + monthlyFlow
      ZusState(newBalance, contributions, pensions, govSubvention)

  // ---------------------------------------------------------------------------
  // NFZ (Narodowy Fundusz Zdrowia — National Health Fund)
  // ---------------------------------------------------------------------------

  /** NFZ state: health insurance fund balance and monthly flows. */
  case class NfzState(
      balance: PLN,       // cumulative surplus/deficit (contributions − spending)
      contributions: PLN, // this month's 9% składka zdrowotna
      spending: PLN,      // this month's health sector contracts
      govSubvention: PLN, // government covers deficit when contributions < spending
  )
  object NfzState:
    val zero: NfzState = NfzState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)

  /** Compute NFZ monthly flows. 9% składka zdrowotna from employed workers.
    * Spending = per-capita cost × (working-age + retirees × aging elasticity).
    * Aging drives cost pressure: retirees consume ~2.5× more healthcare.
    * Deficit covered by government subvention. This belongs to public cash
    * identity semantics rather than to government debt metrics.
    */
  def nfzStep(prevBalance: PLN, employed: Int, wage: PLN, workingAge: Int, nRetirees: Int)(using p: SimParams): NfzState =
    if !p.flags.nfz then NfzState(prevBalance, PLN.Zero, PLN.Zero, PLN.Zero)
    else
      val contributions = employed * (wage * p.social.nfzContribRate)
      val spending      = workingAge * p.social.nfzPerCapitaCost + nRetirees * (p.social.nfzPerCapitaCost * p.social.nfzAgingElasticity)
      val monthlyFlow   = contributions - spending
      val govSubvention = if monthlyFlow < PLN.Zero then -monthlyFlow else PLN.Zero
      NfzState(prevBalance + monthlyFlow, contributions, spending, govSubvention)

  // ---------------------------------------------------------------------------
  // PPK
  // ---------------------------------------------------------------------------

  /** PPK state: capital pension fund bond holdings and monthly flows. */
  case class PpkState(
      bondHoldings: PLN, // accumulated government bond holdings
      contributions: PLN, // this month's total PPK contributions
  )
  object PpkState:
    val zero: PpkState = PpkState(PLN.Zero, PLN.Zero)

  /** Compute PPK monthly contributions. PPK buys government bonds proportional
    * to contributions × bond allocation. Does NOT affect bank deposits (PPK is
    * a pass-through bond market participant).
    */
  def ppkStep(prevHoldings: PLN, employed: Int, wage: PLN)(using p: SimParams): PpkState =
    if !p.flags.ppk then PpkState(prevHoldings, PLN.Zero)
    else
      val contributions = employed * (wage * (p.social.ppkEmployeeRate + p.social.ppkEmployerRate))
      PpkState(prevHoldings, contributions)

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
  @boundaryEscape
  def demographicsStep(prev: DemographicsState, employed: Int, netMigration: Int)(using p: SimParams): DemographicsState =
    if !p.flags.demographics then prev.copy(monthlyRetirements = 0)
    else
      val retirements       = Math.max(0, (employed.toDouble * ComputationBoundary.toDouble(p.social.demRetirementRate)).toInt)
      val workingAgeDecline =
        Math.max(0, (prev.workingAgePop.toDouble * ComputationBoundary.toDouble(p.social.demWorkingAgeDecline) / 12.0).toInt)
      DemographicsState(
        retirees = prev.retirees + retirements,
        workingAgePop = Math.max(0, prev.workingAgePop - retirements - workingAgeDecline + netMigration),
        monthlyRetirements = retirements,
      )

  // BGK stub removed — replaced by QuasiFiscal agent (PR #10)
