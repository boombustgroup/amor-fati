package sfc.engine.mechanisms

import sfc.config.SimParams
import sfc.types.*

/** Macroprudential policy toolkit: CCyB, O-SII buffers, P2R, concentration
  * limits.
  *
  * Implements the Basel III / CRD V macroprudential stack as applied by KNF
  * (Polish Financial Supervision Authority):
  *
  *   - **CCyB** (countercyclical capital buffer): activated when credit-to-GDP
  *     gap exceeds threshold, released immediately when gap falls below release
  *     level. Credit-to-GDP trend approximated via exponential smoothing
  *     (λ=0.05 monthly ≈ quarterly HP filter with λ=1600, per Drehmann & Yetman
  *     2018).
  *   - **O-SII** (Other Systemically Important Institutions): per-bank
  *     surcharges calibrated to KNF 2024 decisions (PKO BP 1.0%, Pekao 0.5%).
  *   - **P2R** (Pillar 2 Requirement): per-bank BION/SREP add-ons from KNF.
  *   - **Concentration limit**: single-name exposure cap as share of system
  *     loans.
  *
  * All functions are no-ops when MACROPRU_ENABLED=false.
  */
object Macroprudential:

  // ---- Calibration constants ----
  private val TrendSmoothing  = 0.05       // EWM λ for credit-to-GDP trend (≈ HP 1600 quarterly)
  private val CcybBuildRate   = 0.0025 / 3 // ~0.25pp per quarter ÷ 3 months
  private val AnnualizeFactor = 12.0       // monthly GDP → annual

  case class State(
      ccyb: Rate,              // countercyclical capital buffer rate
      creditToGdpGap: Double,  // credit-to-GDP deviation from HP trend
      creditToGdpTrend: Double, // HP-filtered trend (exponential smoothing)
  )

  object State:
    val zero: State = State(Rate.Zero, 0.0, 0.0)

  /** Run `body` only when macropru is enabled, otherwise return `fallback`. */
  private inline def guarded[A](fallback: A)(body: => A)(using p: SimParams): A =
    if !p.flags.macropru then fallback else body

  // ---- O-SII buffer ----

  /** O-SII buffer for a specific bank. PKO BP (id=0): 1.0%, Pekao (id=1): 0.5%,
    * others: 0%.
    */
  def osiiBuffer(bankId: Int)(using p: SimParams): Double =
    guarded(0.0)(osiiBufferImpl(bankId))

  private[engine] def osiiBufferImpl(bankId: Int)(using p: SimParams): Double = bankId match
    case 0 => p.banking.osiiPkoBp.toDouble
    case 1 => p.banking.osiiPekao.toDouble
    case _ => 0.0

  // ---- Effective minimum CAR ----

  /** Effective minimum CAR = base + CCyB + O-SII + P2R. */
  def effectiveMinCar(bankId: Int, ccyb: Double)(using p: SimParams): Double =
    guarded(p.banking.minCar.toDouble)(effectiveMinCarImpl(bankId, ccyb))

  private[engine] def effectiveMinCarImpl(bankId: Int, ccyb: Double)(using p: SimParams): Double =
    p.banking.minCar.toDouble + ccyb + osiiBufferImpl(bankId) + p2rAddon(bankId)

  /** P2R add-on from KNF BION/SREP, indexed by bank ID (last value as
    * fallback).
    */
  private[engine] def p2rAddon(bankId: Int)(using p: SimParams): Double =
    val addons = p.banking.p2rAddons
    if bankId >= 0 && bankId < addons.length then addons(bankId).toDouble
    else addons.last.toDouble

  // ---- CCyB step ----

  /** Monthly CCyB update: credit-to-GDP gap → build / release / hold. */
  def step(prev: State, totalLoans: Double, gdp: Double)(using p: SimParams): State =
    guarded(prev)(stepImpl(prev, totalLoans, gdp))

  private[engine] def stepImpl(prev: State, totalLoans: Double, gdp: Double)(using p: SimParams): State =
    val annualGdp   = Math.max(1.0, gdp * AnnualizeFactor)
    val creditToGdp = totalLoans / annualGdp

    // Exponential smoothing of trend (proxy for HP filter)
    val newTrend =
      if prev.creditToGdpTrend <= 0 then creditToGdp
      else prev.creditToGdpTrend * (1.0 - TrendSmoothing) + creditToGdp * TrendSmoothing

    val gap = creditToGdp - newTrend

    // CCyB rule: build gradually above activation gap, release immediately below release gap
    val newCcyb =
      if gap > p.banking.ccybActivationGap.toDouble then (prev.ccyb + Rate(CcybBuildRate)).min(p.banking.ccybMax)
      else if gap < p.banking.ccybReleaseGap then Rate.Zero
      else prev.ccyb

    State(newCcyb, gap, newTrend)

  // ---- Concentration limit ----

  /** Returns true if bank's loan share is within the concentration limit. */
  def withinConcentrationLimit(bankLoans: Double, bankCapital: Double, totalSystemLoans: Double)(using p: SimParams): Boolean =
    guarded(true)(withinConcentrationLimitImpl(bankLoans, bankCapital, totalSystemLoans))

  private[engine] def withinConcentrationLimitImpl(
      bankLoans: Double,
      bankCapital: Double,
      totalSystemLoans: Double,
  )(using p: SimParams): Boolean =
    if totalSystemLoans <= 0 then true
    else (bankLoans / totalSystemLoans) <= p.banking.concentrationLimit.toDouble
