package sfc.agents

import sfc.McRunConfig
import sfc.config.SimParams
import sfc.types.*

object Nbp:

  def updateRate(
      prevRate: Double,
      inflation: Double,
      exRateChange: Double,
      employed: Int,
      totalPopulation: Int,
      rc: McRunConfig,
  )(using p: SimParams): Double =
    if p.flags.nbpSymmetric then
      // v2.0: Symmetric Taylor + output gap (dual mandate)
      val infGap       = inflation - p.monetary.targetInfl.toDouble
      val unempRate    = 1.0 - (employed.toDouble / totalPopulation)
      val rawOutputGap = (unempRate - p.monetary.nairu.toDouble) / p.monetary.nairu.toDouble
      // Cap output gap at ±0.30 — prevents extreme Taylor responses from initial tight labor
      // market while preserving dual-mandate stabilization (Svensson 2003)
      val outputGap    = Math.max(-0.30, Math.min(0.30, rawOutputGap))
      val taylor       = p.monetary.neutralRate.toDouble +
        p.monetary.taylorAlpha * infGap -
        p.monetary.taylorDelta * outputGap +
        p.monetary.taylorBeta * exRateChange
      val smoothed     = prevRate * p.monetary.taylorInertia.toDouble + taylor * (1.0 - p.monetary.taylorInertia.toDouble)
      val effective    =
        if p.monetary.maxRateChange.toDouble > 0 then
          prevRate + Math.max(
            -p.monetary.maxRateChange.toDouble,
            Math.min(p.monetary.maxRateChange.toDouble, smoothed - prevRate),
          )
        else smoothed
      Math.max(p.monetary.rateFloor.toDouble, Math.min(p.monetary.rateCeiling.toDouble, effective))
    else
      // v1.0 legacy: asymmetric Taylor (inflation-only)
      val infGap    = inflation - p.monetary.targetInfl.toDouble
      val taylor    = p.monetary.neutralRate.toDouble +
        p.monetary.taylorAlpha * Math.max(0.0, infGap) +
        p.monetary.taylorBeta * Math.max(0.0, exRateChange)
      val smoothed  = prevRate * p.monetary.taylorInertia.toDouble + taylor * (1.0 - p.monetary.taylorInertia.toDouble)
      val effective =
        if p.monetary.maxRateChange.toDouble > 0 then
          prevRate + Math.max(
            -p.monetary.maxRateChange.toDouble,
            Math.min(p.monetary.maxRateChange.toDouble, smoothed - prevRate),
          )
        else smoothed
      Math.max(p.monetary.rateFloor.toDouble, Math.min(p.monetary.rateCeiling.toDouble, effective))

  /** Bond yield = refRate + termPremium + fiscalRiskPremium - qeCompression -
    * foreignDemandEffect + credibilityPremium
    */
  def bondYield(
      refRate: Double,
      debtToGdp: Double,
      nbpBondGdpShare: Double,
      nfa: Double,
      credibilityPremium: Double = 0.0,
  )(using p: SimParams): Double =
    if !p.flags.govBondMarket then refRate
    else
      val termPremium   = p.fiscal.govTermPremium.toDouble
      val fiscalRisk    = Math.min(0.10, p.fiscal.govFiscalRiskBeta * Math.max(0.0, debtToGdp - 0.40))
      val qeCompress    = 0.5 * nbpBondGdpShare
      val foreignDemand = if nfa > 0 then 0.005 else 0.0
      Math.max(0.0, refRate + termPremium + fiscalRisk - qeCompress - foreignDemand + credibilityPremium)

  /** Should NBP activate QE? Rate at floor + inflation below target - 1pp */
  def shouldActivateQe(refRate: Double, inflation: Double)(using p: SimParams): Boolean =
    p.flags.nbpQe &&
      refRate <= p.monetary.rateFloor.toDouble + 0.0025 &&
      inflation < p.monetary.targetInfl.toDouble - 0.01

  /** Should NBP taper QE? Inflation returned above target */
  def shouldTaperQe(inflation: Double)(using p: SimParams): Boolean =
    inflation > p.monetary.targetInfl.toDouble

  /** Execute monthly QE purchase. Returns (newState, purchaseAmount). */
  def executeQe(nbp: State, bankBondHoldings: Double, annualGdp: Double)(using p: SimParams): (State, Double) =
    if !nbp.qeActive then (nbp, 0.0)
    else
      val maxByGdp  = (PLN(p.monetary.qeMaxGdpShare.toDouble * annualGdp) - nbp.govBondHoldings).toDouble
      val available = bankBondHoldings
      val purchase  = Math.max(0.0, Math.min(p.monetary.qePace.toDouble, Math.min(maxByGdp, available)))
      val newNbp    = nbp.copy(
        govBondHoldings = nbp.govBondHoldings + PLN(purchase),
        qeCumulative = nbp.qeCumulative + PLN(purchase),
      )
      (newNbp, purchase)

  /** Compute sterilized FX intervention. NBP buys/sells EUR to dampen ER
    * deviations beyond the tolerance band. Sterilized: affects only ER, not
    * bank deposits/capital.
    *
    * @param enabled
    *   override for p.flags.nbpFxIntervention (for testability)
    */
  def fxIntervention(
      prevER: Double,
      reserves: Double,
      gdp: Double,
      enabled: Boolean,
  )(using p: SimParams): FxInterventionResult =
    if !enabled then FxInterventionResult(0.0, 0.0, reserves)
    else
      val erDev = (prevER - p.forex.baseExRate) / p.forex.baseExRate
      if Math.abs(erDev) <= p.monetary.fxBand.toDouble then FxInterventionResult(0.0, 0.0, reserves) // within band → no intervention
      else
        val direction     = -Math.signum(erDev)   // opposite of deviation
        val maxByReserves = reserves * p.monetary.fxMaxMonthly.toDouble
        // Magnitude in EUR: capped by reserves when selling EUR
        val magnitude     =
          if direction < 0 then // selling EUR (strengthening PLN)
            Math.min(maxByReserves, reserves) // can't sell more than total reserves
          else // buying EUR (weakening PLN) — prints PLN, pace-limited
            maxByReserves
        val eurTraded     = magnitude * direction // positive = bought EUR
        val newReserves   = reserves + eurTraded
        // Effect on ER: intervention dampens the excess deviation
        val gdpEffect     = if gdp > 0 then Math.abs(eurTraded) * p.forex.baseExRate / gdp else 0.0
        val erEffect      = direction * gdpEffect * p.monetary.fxStrength.toDouble
        FxInterventionResult(erEffect, eurTraded, Math.max(0.0, newReserves))

  case class State(
      referenceRate: Rate,
      govBondHoldings: PLN = PLN.Zero,
      qeActive: Boolean = false,
      qeCumulative: PLN = PLN.Zero,
      fxReserves: PLN = PLN.Zero,  // EUR-equivalent total (multi-currency); set at init
      lastFxTraded: PLN = PLN.Zero, // monthly FX intervention amount (EUR)
  )

  /** FX intervention result. */
  case class FxInterventionResult(
      erEffect: Double,   // added to erChange in OpenEconomy
      eurTraded: Double,  // positive = bought EUR (weakened PLN), negative = sold EUR
      newReserves: Double, // updated reserve level
  )
