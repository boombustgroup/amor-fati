package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

/** National Bank of Poland: Taylor rule, bond yield, QE, FX intervention. */
object Nbp:

  // ---------------------------------------------------------------------------
  // Named constants
  // ---------------------------------------------------------------------------

  private val OutputGapCap          = Ratio(0.30)  // ±cap on output gap in Taylor rule (Svensson 2003)
  private val DebtThreshold         = Ratio(0.40)  // debt-to-GDP threshold for fiscal risk premium
  private val FiscalRiskCap         = Rate(0.10)   // maximum fiscal risk premium
  private val QeCompressionCoeff    = Rate(0.5)    // yield compression per unit of NBP bond/GDP share
  private val ForeignDemandDiscount = Rate(0.005)  // yield discount when NFA > 0
  private val QeActivationSlack     = Rate(0.0025) // rate proximity to floor for QE activation
  private val QeDeflationThreshold  = Rate(0.01)   // inflation must be this much below target for QE

  // ---------------------------------------------------------------------------
  // State
  // ---------------------------------------------------------------------------

  case class State(
      referenceRate: Rate,  // NBP reference rate (annualised)
      govBondHoldings: PLN, // NBP bond portfolio (QE + open market)
      qeActive: Boolean,    // whether QE programme is currently active
      qeCumulative: PLN,    // cumulative QE purchases since activation
      fxReserves: PLN,      // EUR-equivalent total reserves (multi-currency)
      lastFxTraded: PLN,    // monthly FX intervention amount (EUR, +bought/−sold)
  )

  // ---------------------------------------------------------------------------
  // QE result type
  // ---------------------------------------------------------------------------

  /** Result of monthly QE execution. */
  case class QeResult(
      state: State,  // updated NBP state
      purchased: PLN, // bond purchase amount this month
  )

  // ---------------------------------------------------------------------------
  // FX intervention result
  // ---------------------------------------------------------------------------

  /** FX intervention result. */
  case class FxInterventionResult(
      erEffect: Double, // dimensionless ER change added to erChange in OpenEconomy
      eurTraded: PLN,   // positive = bought EUR (weakened PLN), negative = sold EUR
      newReserves: PLN, // updated NBP FX reserve level (EUR)
      plnInjection: PLN, // PLN injected (+) or drained (−) from banking system reserves
  )

  // ---------------------------------------------------------------------------
  // Taylor rule
  // ---------------------------------------------------------------------------

  /** Raw Taylor target: symmetric (dual mandate) or asymmetric
    * (inflation-only).
    */
  private def taylorTarget(
      inflation: Rate,
      exRateChange: Ratio,
      employed: Int,
      totalPopulation: Int,
  )(using p: SimParams): Rate =
    val infGap    = inflation - p.monetary.targetInfl
    val unempRate = Ratio.One - Ratio.fraction(employed, totalPopulation)
    val nairu     = Ratio(p.monetary.nairu.toDouble) // Rate → Ratio: NAIRU is unemployment rate, not interest rate
    if p.flags.nbpSymmetric then
      val rawOutputGap = Ratio((unempRate - nairu) / nairu)
      val outputGap    = rawOutputGap.clamp(Ratio.Zero - OutputGapCap, OutputGapCap)
      p.monetary.neutralRate +
        infGap * p.monetary.taylorAlpha -
        (outputGap * p.monetary.taylorDelta).toRate + // Ratio × coeff → Rate contribution
        (exRateChange * p.monetary.taylorBeta).toRate // Ratio × coeff → Rate contribution
    else
      p.monetary.neutralRate +
        infGap.max(Rate.Zero) * p.monetary.taylorAlpha +
        (exRateChange.max(Ratio.Zero) * p.monetary.taylorBeta).toRate

  /** Inertia smoothing + max rate change clamping. */
  private def smoothAndClamp(prevRate: Rate, taylor: Rate)(using p: SimParams): Rate =
    val inertia  = p.monetary.taylorInertia
    val smoothed = prevRate * inertia + taylor * (Ratio.One - inertia)
    if p.monetary.maxRateChange > Rate.Zero then
      val delta = (smoothed - prevRate).clamp(-p.monetary.maxRateChange, p.monetary.maxRateChange)
      prevRate + delta
    else smoothed

  /** Floor/ceiling clamp to [rateFloor, rateCeiling]. */
  private def clampRate(rate: Rate)(using p: SimParams): Rate =
    rate.clamp(p.monetary.rateFloor, p.monetary.rateCeiling)

  /** Update NBP reference rate via Taylor rule. Symmetric (dual mandate) or
    * asymmetric (inflation-only) depending on flags.nbpSymmetric.
    */
  def updateRate(
      prevRate: Rate,
      inflation: Rate,
      exRateChange: Ratio,
      employed: Int,
      totalPopulation: Int,
  )(using p: SimParams): Rate =
    val taylor = taylorTarget(inflation, exRateChange, employed, totalPopulation)
    clampRate(smoothAndClamp(prevRate, taylor))

  // ---------------------------------------------------------------------------
  // Bond yield
  // ---------------------------------------------------------------------------

  /** Bond yield = refRate + termPremium + fiscalRisk − qeCompression −
    * foreignDemand + credibilityPremium.
    */
  def bondYield(
      refRate: Rate,
      debtToGdp: Ratio,
      nbpBondGdpShare: Ratio,
      nfa: PLN,
      credibilityPremium: Rate,
  )(using p: SimParams): Rate =
    if !p.flags.govBondMarket then refRate
    else
      val fiscalRisk    = piecewiseFiscalRisk(debtToGdp)
      val qeCompress    = QeCompressionCoeff * nbpBondGdpShare
      val foreignDemand = if nfa > PLN.Zero then ForeignDemandDiscount else Rate.Zero
      (refRate + p.fiscal.govTermPremium + fiscalRisk - qeCompress - foreignDemand + credibilityPremium).max(Rate.Zero)

  /** Piecewise fiscal risk premium: steepens at 55% and 60% debt/GDP. base
    * segment (40%+) + caution segment (55%+) + crisis segment (60%+).
    */
  private def piecewiseFiscalRisk(debtToGdp: Ratio)(using p: SimParams): Rate =
    val base      = Rate(p.fiscal.govFiscalRiskBeta * (debtToGdp - DebtThreshold).max(Ratio.Zero).toDouble)
    val caution55 =
      if debtToGdp > p.fiscal.fiscalRuleCautionThreshold then p.fiscal.fiscalRiskBeta55 * (debtToGdp - p.fiscal.fiscalRuleCautionThreshold)
      else Rate.Zero
    val crisis60  =
      if debtToGdp > p.fiscal.fiscalRuleDebtCeiling then p.fiscal.fiscalRiskBeta60 * (debtToGdp - p.fiscal.fiscalRuleDebtCeiling)
      else Rate.Zero
    (base + caution55 + crisis60).min(FiscalRiskCap)

  // ---------------------------------------------------------------------------
  // QE
  // ---------------------------------------------------------------------------

  /** Should NBP activate QE? Rate near floor + inflation well below target. */
  def shouldActivateQe(refRate: Rate, inflation: Rate)(using p: SimParams): Boolean =
    p.flags.nbpQe &&
      refRate <= p.monetary.rateFloor + QeActivationSlack &&
      inflation < p.monetary.targetInfl - QeDeflationThreshold

  /** Should NBP taper QE? Inflation returned above target. */
  def shouldTaperQe(inflation: Rate)(using p: SimParams): Boolean =
    inflation > p.monetary.targetInfl

  /** Execute monthly QE purchase. */
  def executeQe(nbp: State, bankBondHoldings: PLN, annualGdp: PLN)(using p: SimParams): QeResult =
    if !nbp.qeActive then QeResult(nbp, PLN.Zero)
    else
      val maxByGdp  = (annualGdp * p.monetary.qeMaxGdpShare - nbp.govBondHoldings).max(PLN.Zero)
      val available = bankBondHoldings
      val purchase  = PLN.Zero.max(maxByGdp.min(available).min(p.monetary.qePace))
      val newNbp    = nbp.copy(
        govBondHoldings = nbp.govBondHoldings + purchase,
        qeCumulative = nbp.qeCumulative + purchase,
      )
      QeResult(newNbp, purchase)

  // ---------------------------------------------------------------------------
  // FX intervention
  // ---------------------------------------------------------------------------

  /** FX intervention. NBP buys/sells EUR to dampen ER deviations beyond the
    * tolerance band. EUR purchase injects PLN into banking system reserves; EUR
    * sale drains PLN. The PLN injection feeds into the liquidity-aware
    * interbank rate (#9) via bank reservesAtNbp.
    */
  def fxIntervention(
      prevER: Double,
      reserves: Double,
      gdp: Double,
      enabled: Boolean,
  )(using p: SimParams): FxInterventionResult =
    if !enabled then FxInterventionResult(0.0, PLN.Zero, PLN(reserves), PLN.Zero)
    else
      val erDev = (prevER - p.forex.baseExRate) / p.forex.baseExRate
      if Math.abs(erDev) <= p.monetary.fxBand.toDouble then FxInterventionResult(0.0, PLN.Zero, PLN(reserves), PLN.Zero)
      else
        val direction     = -Math.signum(erDev)
        val maxByReserves = reserves * p.monetary.fxMaxMonthly.toDouble
        val magnitude     =
          if direction < 0 then Math.min(maxByReserves, reserves)
          else maxByReserves
        val eurTraded     = magnitude * direction
        val newReserves   = reserves + eurTraded
        val gdpEffect     = if gdp > 0 then Math.abs(eurTraded) * p.forex.baseExRate / gdp else 0.0
        val erEffect      = direction * gdpEffect * p.monetary.fxStrength.toDouble
        // PLN injection: EUR purchase → NBP pays PLN to banks (+), EUR sale → banks pay PLN to NBP (−)
        val plnInjection  = PLN(eurTraded * p.forex.baseExRate)
        FxInterventionResult(erEffect, PLN(eurTraded), PLN(newReserves).max(PLN.Zero), plnInjection)
