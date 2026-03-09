package sfc.engine.mechanisms

import sfc.config.SimParams
import sfc.types.*

object Macroprudential:

  /** Macroprudential state: CCyB, credit-to-GDP gap, effective minimum CAR. */
  case class State(
      ccyb: Rate,              // current countercyclical capital buffer rate
      creditToGdpGap: Double,  // credit-to-GDP deviation from trend (HP-filtered)
      creditToGdpTrend: Double, // HP-filtered trend (smoothed)
  )

  object State:
    val zero: State = State(Rate.Zero, 0.0, 0.0)

  /** OSII buffer for a specific bank (based on bank ID). Systemically important
    * banks get higher buffers. PKO BP (id=0): 1.0%, Pekao (id=1): 0.5%, others:
    * 0%.
    */
  def osiiBuffer(bankId: Int)(using p: SimParams): Double =
    if !p.flags.macropru then 0.0
    else osiiBufferInternal(bankId)

  /** Internal OSII buffer (always computes, for testing). */
  private[engine] def osiiBufferInternal(bankId: Int)(using p: SimParams): Double = bankId match
    case 0 => p.banking.osiiPkoBp.toDouble // PKO BP
    case 1 => p.banking.osiiPekao.toDouble // Pekao
    case _ => 0.0

  /** Effective minimum CAR for a specific bank: base MinCar + CCyB + OSII +
    * P2R.
    */
  def effectiveMinCar(bankId: Int, ccyb: Double)(using p: SimParams): Double =
    if !p.flags.macropru then p.banking.minCar.toDouble
    else p.banking.minCar.toDouble + ccyb + osiiBufferInternal(bankId) + p2rAddon(bankId)

  /** Internal effectiveMinCar (always computes, for testing). */
  private[engine] def effectiveMinCarInternal(bankId: Int, ccyb: Double)(using p: SimParams): Double =
    p.banking.minCar.toDouble + ccyb + osiiBufferInternal(bankId) + p2rAddon(bankId)

  /** Pillar 2 Requirement (P2R) for a specific bank (KNF BION/SREP). */
  private[engine] def p2rAddon(bankId: Int)(using p: SimParams): Double =
    if bankId >= 0 && bankId < p.banking.p2rAddons.map(_.toDouble).length then p.banking.p2rAddons.map(_.toDouble)(bankId)
    else p.banking.p2rAddons.map(_.toDouble).last

  /** Update macroprudential state. Computes credit-to-GDP gap and CCyB.
    *
    * Credit-to-GDP gap: deviation of credit/GDP ratio from its HP-filtered
    * trend. CCyB activation: gap > activationGap → build buffer (up to max).
    * CCyB release: gap < releaseGap → release buffer immediately.
    *
    * HP filter approximated by exponential smoothing (λ=0.05 monthly ≈
    * quarterly HP 1600).
    */
  def step(prev: State, totalLoans: Double, gdp: Double)(using p: SimParams): State =
    if !p.flags.macropru then prev
    else stepInternal(prev, totalLoans, gdp)

  /** Internal step (always computes, for testing). */
  private[engine] def stepInternal(prev: State, totalLoans: Double, gdp: Double)(using p: SimParams): State =
    // Credit-to-GDP ratio (annualized GDP)
    val annualGdp   = Math.max(1.0, gdp * 12.0)
    val creditToGdp = totalLoans / annualGdp

    // Exponential smoothing of trend (proxy for HP filter)
    val lambda   = 0.05 // smoothing parameter
    val newTrend =
      if prev.creditToGdpTrend <= 0 then creditToGdp
      else prev.creditToGdpTrend * (1.0 - lambda) + creditToGdp * lambda

    // Gap = actual - trend
    val gap = creditToGdp - newTrend

    // CCyB policy rule:
    // - gap > activation threshold → build CCyB (gradual, up to max)
    // - gap < release threshold → release immediately (countercyclical)
    val newCcyb =
      if gap > p.banking.ccybActivationGap.toDouble then
        // Build gradually: add 0.25pp per quarter of exceedance
        Rate(Math.min(p.banking.ccybMax.toDouble, prev.ccyb.toDouble + 0.0025 / 3.0)) // ~0.25pp/quarter ÷ 3 months
      else if gap < p.banking.ccybReleaseGap then Rate.Zero // Immediate release
      else prev.ccyb                                                                  // Maintain current buffer

    State(newCcyb, gap, newTrend)

  /** Check concentration limit: bank's loan share should not exceed limit ×
    * capital. Returns true if within limit.
    */
  def withinConcentrationLimit(bankLoans: Double, bankCapital: Double, totalSystemLoans: Double)(using
      p: SimParams,
  ): Boolean =
    if !p.flags.macropru || totalSystemLoans <= 0 then true
    else withinConcentrationLimitInternal(bankLoans, bankCapital, totalSystemLoans)

  /** Internal concentration limit check (always computes, for testing). */
  private[engine] def withinConcentrationLimitInternal(
      bankLoans: Double,
      bankCapital: Double,
      totalSystemLoans: Double,
  )(using p: SimParams): Boolean =
    if totalSystemLoans <= 0 then true
    else
      val loanShare = bankLoans / totalSystemLoans
      loanShare <= p.banking.concentrationLimit.toDouble
