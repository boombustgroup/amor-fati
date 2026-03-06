package sfc.agents

import sfc.config.Config
import sfc.types.*

/** Social security and demographics: ZUS/FUS, PPK, demographics, BGK. */
object SocialSecurity:

  /** ZUS/FUS state: social insurance fund balance and monthly flows. */
  case class ZusState(
    fusBalance: PLN, // cumulative raw surplus/deficit (contributions - pensions, before gov subvention)
    contributions: PLN, // this month's total contributions
    pensionPayments: PLN, // this month's total pension payments
    govSubvention: PLN, // this month's government subvention (covers deficit)
  )
  object ZusState:
    val zero: ZusState = ZusState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)

  /** PPK state: capital pension fund bond holdings and monthly flows. */
  case class PpkState(
    bondHoldings: PLN, // accumulated government bond holdings
    contributions: PLN, // this month's total PPK contributions
  )
  object PpkState:
    val zero: PpkState = PpkState(PLN.Zero, PLN.Zero)

  /** Demographics state: retirees and working-age population. */
  case class DemographicsState(
    retirees: Int, // total retired workers receiving pensions
    workingAgePop: Int, // effective working-age population
    monthlyRetirements: Int, // new retirements this month
  )
  object DemographicsState:
    val zero: DemographicsState = DemographicsState(0, 0, 0)

  /** BGK state: state development bank (stub for future use). */
  case class BgkState(
    loanPortfolio: PLN = PLN.Zero,
  )
  object BgkState:
    val zero: BgkState = BgkState(PLN.Zero)

  /** Compute ZUS monthly flows. Contributions from employed workers, pensions to retirees. FUS deficit covered by
    * government subvention (flows into Identity 3). fusBalance tracks raw surplus/deficit (Identity 8).
    */
  def zusStep(prevBalance: Double, employed: Int, wage: Double, nRetirees: Int): ZusState =
    if !Config.ZusEnabled then ZusState(PLN(prevBalance), PLN.Zero, PLN.Zero, PLN.Zero)
    else
      val contributions = employed.toDouble * wage * Config.ZusContribRate * Config.ZusScale
      val pensions = nRetirees.toDouble * Config.ZusBasePension
      val monthlyFlow = contributions - pensions
      val govSubvention = if monthlyFlow < 0 then -monthlyFlow else 0.0
      val newBalance = prevBalance + monthlyFlow
      ZusState(PLN(newBalance), PLN(contributions), PLN(pensions), PLN(govSubvention))

  /** Compute PPK monthly bond purchase amount. PPK buys government bonds proportional to contributions × bond
    * allocation. Does NOT affect bank deposits (PPK is a pass-through bond market participant).
    */
  def ppkStep(prevHoldings: Double, employed: Int, wage: Double): PpkState =
    if !Config.PpkEnabled then PpkState(PLN(prevHoldings), PLN.Zero)
    else
      val contributions = employed.toDouble * wage *
        (Config.PpkEmployeeRate + Config.PpkEmployerRate)
      PpkState(PLN(prevHoldings), PLN(contributions))

  /** PPK bond purchase this month (from contributions × bond allocation). */
  def ppkBondPurchase(ppk: PpkState): Double =
    ppk.contributions.toDouble * Config.PpkBondAlloc

  /** Compute demographics monthly step. Monthly retirements reduce labor supply; working-age population declines.
    * @param netMigration
    *   net immigration (inflow - outflow), added to workingAgePop
    */
  def demographicsStep(prev: DemographicsState, employed: Int, netMigration: Int = 0): DemographicsState =
    if !Config.DemEnabled then prev.copy(monthlyRetirements = 0)
    else
      val retirements = Math.max(0, (employed.toDouble * Config.DemRetirementRate).toInt)
      val workingAgeDecline = Math.max(0, (prev.workingAgePop.toDouble * Config.DemWorkingAgeDecline / 12.0).toInt)
      DemographicsState(
        retirees = prev.retirees + retirements,
        workingAgePop = Math.max(0, prev.workingAgePop - retirements - workingAgeDecline + netMigration),
        monthlyRetirements = retirements,
      )
