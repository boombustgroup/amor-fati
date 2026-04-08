package com.boombustgroup.amorfati.engine.mechanisms

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

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
  private val TrendSmoothing: Share     = Share(0.05)            // EWM λ for credit-to-GDP trend (≈ HP 1600 quarterly)
  private val CcybBuildRate: Multiplier = Multiplier(0.0025 / 3) // ~0.25pp per quarter ÷ 3 months
  private val AnnualizeFactor           = 12                     // monthly GDP → annual

  case class State(
      ccyb: Multiplier,            // countercyclical capital buffer
      creditToGdpGap: Coefficient, // credit-to-GDP deviation from HP trend
      creditToGdpTrend: Multiplier, // HP-filtered trend (exponential smoothing)
  )

  object State:
    val zero: State = State(Multiplier.Zero, Coefficient.Zero, Multiplier.Zero)

  // ---- O-SII buffer ----

  /** O-SII buffer for a specific bank. PKO BP (id=0): 1.0%, Pekao (id=1): 0.5%,
    * others: 0%.
    */
  def osiiBuffer(bankId: Int)(using p: SimParams): Multiplier =
    osiiBufferImpl(bankId)

  private[engine] def osiiBufferImpl(bankId: Int)(using p: SimParams): Multiplier = bankId match
    case 0 => p.banking.osiiPkoBp
    case 1 => p.banking.osiiPekao
    case _ => Multiplier.Zero

  // ---- Effective minimum CAR ----

  /** Effective minimum CAR = base + CCyB + O-SII + P2R. */
  def effectiveMinCar(bankId: Int, ccyb: Multiplier)(using p: SimParams): Multiplier =
    effectiveMinCarImpl(bankId, ccyb)

  private[engine] def effectiveMinCarImpl(bankId: Int, ccyb: Multiplier)(using p: SimParams): Multiplier =
    p.banking.minCar + ccyb + osiiBufferImpl(bankId) + p2rAddon(bankId)

  /** P2R add-on from KNF BION/SREP, indexed by bank ID (last value as
    * fallback).
    */
  private[engine] def p2rAddon(bankId: Int)(using p: SimParams): Multiplier =
    val addons = p.banking.p2rAddons
    if bankId >= 0 && bankId < addons.length then addons(bankId)
    else addons.last

  // ---- CCyB step ----

  /** Monthly CCyB update: credit-to-GDP gap → build / release / hold. */
  def step(prev: State, totalLoans: PLN, gdp: PLN)(using p: SimParams): State =
    stepImpl(prev, totalLoans, gdp)

  private[engine] def stepImpl(prev: State, totalLoans: PLN, gdp: PLN)(using p: SimParams): State =
    val annualGdp   = (gdp * AnnualizeFactor).max(PLN(1.0))
    val creditToGdp = Multiplier(totalLoans / annualGdp)

    // Exponential smoothing of trend (proxy for HP filter)
    val newTrend =
      if prev.creditToGdpTrend <= Multiplier.Zero then creditToGdp
      else prev.creditToGdpTrend * (Share.One - TrendSmoothing) + creditToGdp * TrendSmoothing

    val gapMult = creditToGdp - newTrend
    val gap     = Coefficient(gapMult / Multiplier.One)

    // CCyB rule: build gradually above activation gap, release immediately below release gap
    val newCcyb =
      if gap > p.banking.ccybActivationGap then (prev.ccyb + CcybBuildRate).min(p.banking.ccybMax)
      else if gap < p.banking.ccybReleaseGap then Multiplier.Zero
      else prev.ccyb

    State(newCcyb, gap, newTrend)

  // ---- Concentration limit ----

  /** Returns true if bank's loan share is within the concentration limit. */
  def withinConcentrationLimit(bankLoans: PLN, bankCapital: PLN, totalSystemLoans: PLN)(using p: SimParams): Boolean =
    withinConcentrationLimitImpl(bankLoans, bankCapital, totalSystemLoans)

  private[engine] def withinConcentrationLimitImpl(
      bankLoans: PLN,
      @scala.annotation.unused bankCapital: PLN,
      totalSystemLoans: PLN,
  )(using p: SimParams): Boolean =
    if totalSystemLoans <= PLN.Zero then true
    else bankLoans.ratioTo(totalSystemLoans).clampToShare <= p.banking.concentrationLimit
