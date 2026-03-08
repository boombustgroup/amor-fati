package sfc.agents

import sfc.config.{Config, RunConfig}
import sfc.types.*

object Nbp:

  def updateRate(prevRate: Double, inflation: Double, exRateChange: Double, employed: Int, rc: RunConfig): Double =
    if Config.NbpSymmetric then
      // v2.0: Symmetric Taylor + output gap (dual mandate)
      val infGap = inflation - Config.NbpTargetInfl
      val unempRate = 1.0 - (employed.toDouble / Config.TotalPopulation)
      val rawOutputGap = (unempRate - Config.NbpNairu) / Config.NbpNairu
      // Cap output gap at ±0.30 — prevents extreme Taylor responses from initial tight labor
      // market while preserving dual-mandate stabilization (Svensson 2003)
      val outputGap = Math.max(-0.30, Math.min(0.30, rawOutputGap))
      val taylor = Config.NbpNeutralRate +
        Config.TaylorAlpha * infGap -
        Config.TaylorDelta * outputGap +
        Config.TaylorBeta * exRateChange
      val smoothed = prevRate * Config.TaylorInertia + taylor * (1.0 - Config.TaylorInertia)
      val effective =
        if Config.NbpMaxRateChange > 0 then
          prevRate + Math.max(-Config.NbpMaxRateChange, Math.min(Config.NbpMaxRateChange, smoothed - prevRate))
        else smoothed
      Math.max(Config.RateFloor, Math.min(Config.RateCeiling, effective))
    else
      // v1.0 legacy: asymmetric Taylor (inflation-only)
      val infGap = inflation - Config.NbpTargetInfl
      val taylor = Config.NbpNeutralRate +
        Config.TaylorAlpha * Math.max(0.0, infGap) +
        Config.TaylorBeta * Math.max(0.0, exRateChange)
      val smoothed = prevRate * Config.TaylorInertia + taylor * (1.0 - Config.TaylorInertia)
      val effective =
        if Config.NbpMaxRateChange > 0 then
          prevRate + Math.max(-Config.NbpMaxRateChange, Math.min(Config.NbpMaxRateChange, smoothed - prevRate))
        else smoothed
      Math.max(Config.RateFloor, Math.min(Config.RateCeiling, effective))

  /** Bond yield = refRate + termPremium + fiscalRiskPremium - qeCompression - foreignDemandEffect + credibilityPremium
    */
  def bondYield(
    refRate: Double,
    debtToGdp: Double,
    nbpBondGdpShare: Double,
    nfa: Double,
    credibilityPremium: Double = 0.0,
  ): Double =
    if !Config.GovBondMarket then refRate
    else
      val termPremium = Config.GovTermPremium
      val fiscalRisk = Math.min(0.10, Config.GovFiscalRiskBeta * Math.max(0.0, debtToGdp - 0.40))
      val qeCompress = 0.5 * nbpBondGdpShare
      val foreignDemand = if nfa > 0 then 0.005 else 0.0
      Math.max(0.0, refRate + termPremium + fiscalRisk - qeCompress - foreignDemand + credibilityPremium)

  /** Should NBP activate QE? Rate at floor + inflation below target - 1pp */
  def shouldActivateQe(refRate: Double, inflation: Double): Boolean =
    Config.NbpQe &&
      refRate <= Config.RateFloor + 0.0025 &&
      inflation < Config.NbpTargetInfl - 0.01

  /** Should NBP taper QE? Inflation returned above target */
  def shouldTaperQe(inflation: Double): Boolean =
    inflation > Config.NbpTargetInfl

  /** Execute monthly QE purchase. Returns (newState, purchaseAmount). */
  def executeQe(nbp: State, bankBondHoldings: Double, annualGdp: Double): (State, Double) =
    if !nbp.qeActive then (nbp, 0.0)
    else
      val maxByGdp = (PLN(Config.NbpQeMaxGdpShare * annualGdp) - nbp.govBondHoldings).toDouble
      val available = bankBondHoldings
      val purchase = Math.max(0.0, Math.min(Config.NbpQePace, Math.min(maxByGdp, available)))
      val newNbp = nbp.copy(
        govBondHoldings = nbp.govBondHoldings + PLN(purchase),
        qeCumulative = nbp.qeCumulative + PLN(purchase),
      )
      (newNbp, purchase)

  /** Compute sterilized FX intervention. NBP buys/sells EUR to dampen ER deviations beyond the tolerance band.
    * Sterilized: affects only ER, not bank deposits/capital.
    *
    * @param enabled
    *   override for Config.NbpFxIntervention (for testability)
    */
  def fxIntervention(
    prevER: Double,
    reserves: Double,
    gdp: Double,
    enabled: Boolean = Config.NbpFxIntervention,
  ): FxInterventionResult =
    if !enabled then FxInterventionResult(0.0, 0.0, reserves)
    else
      val erDev = (prevER - Config.BaseExRate) / Config.BaseExRate
      if Math.abs(erDev) <= Config.NbpFxBand then
        FxInterventionResult(0.0, 0.0, reserves) // within band → no intervention
      else
        val direction = -Math.signum(erDev) // opposite of deviation
        val maxByReserves = reserves * Config.NbpFxMaxMonthly
        // Magnitude in EUR: capped by reserves when selling EUR
        val magnitude =
          if direction < 0 then // selling EUR (strengthening PLN)
            Math.min(maxByReserves, reserves) // can't sell more than total reserves
          else // buying EUR (weakening PLN) — prints PLN, pace-limited
            maxByReserves
        val eurTraded = magnitude * direction // positive = bought EUR
        val newReserves = reserves + eurTraded
        // Effect on ER: intervention dampens the excess deviation
        val gdpEffect = if gdp > 0 then Math.abs(eurTraded) * Config.BaseExRate / gdp else 0.0
        val erEffect = direction * gdpEffect * Config.NbpFxStrength
        FxInterventionResult(erEffect, eurTraded, Math.max(0.0, newReserves))

  case class State(
    referenceRate: Rate,
    govBondHoldings: PLN = PLN.Zero,
    qeActive: Boolean = false,
    qeCumulative: PLN = PLN.Zero,
    fxReserves: PLN = PLN(Config.NbpFxReserves), // EUR-equivalent total (multi-currency)
    lastFxTraded: PLN = PLN.Zero, // monthly FX intervention amount (EUR)
  )

  /** FX intervention result. */
  case class FxInterventionResult(
    erEffect: Double, // added to erChange in OpenEconomy
    eurTraded: Double, // positive = bought EUR (weakened PLN), negative = sold EUR
    newReserves: Double, // updated reserve level
  )
