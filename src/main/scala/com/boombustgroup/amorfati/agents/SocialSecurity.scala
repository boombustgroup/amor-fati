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
    * to retirees. FUS deficit covered by government subvention (Identity 3).
    * fusBalance tracks raw surplus/deficit (Identity 8).
    */
  def zusStep(prevBalance: PLN, employed: Int, wage: PLN, nRetirees: Int)(using p: SimParams): ZusState =
    if !p.flags.zus then ZusState(prevBalance, PLN.Zero, PLN.Zero, PLN.Zero)
    else
      val contributions = wage * (employed.toDouble * p.social.zusContribRate.toDouble * p.social.zusScale)
      val pensions      = p.social.zusBasePension * nRetirees.toDouble
      val monthlyFlow   = contributions - pensions
      val govSubvention = if monthlyFlow < PLN.Zero then -monthlyFlow else PLN.Zero
      val newBalance    = prevBalance + monthlyFlow
      ZusState(newBalance, contributions, pensions, govSubvention)

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
      val contributions = wage * (employed.toDouble *
        (p.social.ppkEmployeeRate.toDouble + p.social.ppkEmployerRate.toDouble))
      PpkState(prevHoldings, contributions)

  /** PPK bond purchase this month: contributions × bond allocation. */
  def ppkBondPurchase(ppk: PpkState)(using p: SimParams): PLN =
    ppk.contributions * p.social.ppkBondAlloc.toDouble

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
    if !p.flags.demographics then prev.copy(monthlyRetirements = 0)
    else
      val retirements       = Math.max(0, (employed.toDouble * p.social.demRetirementRate.toDouble).toInt)
      val workingAgeDecline =
        Math.max(0, (prev.workingAgePop.toDouble * p.social.demWorkingAgeDecline.toDouble / 12.0).toInt)
      DemographicsState(
        retirees = prev.retirees + retirements,
        workingAgePop = Math.max(0, prev.workingAgePop - retirements - workingAgeDecline + netMigration),
        monthlyRetirements = retirements,
      )

  // ---------------------------------------------------------------------------
  // BGK (stub)
  // ---------------------------------------------------------------------------

  /** BGK state: state development bank (stub for future use). */
  case class BgkState(
      loanPortfolio: PLN, // outstanding BGK loan portfolio
  )
  object BgkState:
    val zero: BgkState = BgkState(PLN.Zero)
