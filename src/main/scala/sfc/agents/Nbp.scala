package sfc.agents

import sfc.McRunConfig
import sfc.config.SimParams
import sfc.types.*

/** National Bank of Poland: Taylor rule, bond yield, QE, FX intervention. */
object Nbp:

  // ---------------------------------------------------------------------------
  // Named constants
  // ---------------------------------------------------------------------------

  private val OutputGapCap          = 0.30   // ±cap on output gap in Taylor rule (Svensson 2003)
  private val DebtThreshold         = 0.40   // debt-to-GDP threshold for fiscal risk premium
  private val FiscalRiskCap         = 0.10   // maximum fiscal risk premium
  private val QeCompressionCoeff    = 0.5    // yield compression per unit of NBP bond/GDP share
  private val ForeignDemandDiscount = 0.005  // yield discount when NFA > 0
  private val QeActivationSlack     = 0.0025 // rate proximity to floor for QE activation
  private val QeDeflationThreshold  = 0.01   // inflation must be this much below target for QE

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
      erEffect: Double,   // added to erChange in OpenEconomy
      eurTraded: Double,  // positive = bought EUR (weakened PLN), negative = sold EUR
      newReserves: Double, // updated reserve level
  )

  // ---------------------------------------------------------------------------
  // Taylor rule
  // ---------------------------------------------------------------------------

  /** Raw Taylor target: symmetric (dual mandate) or asymmetric
    * (inflation-only).
    */
  private def taylorTarget(
      inflation: Rate,
      exRateChange: Double,
      employed: Int,
      totalPopulation: Int,
  )(using p: SimParams): Double =
    val infGap = inflation.toDouble - p.monetary.targetInfl.toDouble
    if p.flags.nbpSymmetric then
      val unempRate    = 1.0 - (employed.toDouble / totalPopulation)
      val rawOutputGap = (unempRate - p.monetary.nairu.toDouble) / p.monetary.nairu.toDouble
      val outputGap    = Math.max(-OutputGapCap, Math.min(OutputGapCap, rawOutputGap))
      p.monetary.neutralRate.toDouble +
        p.monetary.taylorAlpha * infGap -
        p.monetary.taylorDelta * outputGap +
        p.monetary.taylorBeta * exRateChange
    else
      p.monetary.neutralRate.toDouble +
        p.monetary.taylorAlpha * Math.max(0.0, infGap) +
        p.monetary.taylorBeta * Math.max(0.0, exRateChange)

  /** Inertia smoothing + max rate change clamping. */
  private def smoothAndClamp(prevRate: Rate, taylor: Double)(using p: SimParams): Double =
    val smoothed = prevRate.toDouble * p.monetary.taylorInertia.toDouble + taylor * (1.0 - p.monetary.taylorInertia.toDouble)
    if p.monetary.maxRateChange.toDouble > 0 then
      prevRate.toDouble + Math.max(
        -p.monetary.maxRateChange.toDouble,
        Math.min(p.monetary.maxRateChange.toDouble, smoothed - prevRate.toDouble),
      )
    else smoothed

  /** Floor/ceiling clamp to [rateFloor, rateCeiling]. */
  private def clampRate(rate: Double)(using p: SimParams): Rate =
    Rate(Math.max(p.monetary.rateFloor.toDouble, Math.min(p.monetary.rateCeiling.toDouble, rate)))

  /** Update NBP reference rate via Taylor rule. Symmetric (dual mandate) or
    * asymmetric (inflation-only) depending on flags.nbpSymmetric.
    */
  def updateRate(
      prevRate: Rate,
      inflation: Rate,
      exRateChange: Double,
      employed: Int,
      totalPopulation: Int,
      rc: McRunConfig,
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
      debtToGdp: Double,
      nbpBondGdpShare: Double,
      nfa: PLN,
      credibilityPremium: Double,
  )(using p: SimParams): Rate =
    if !p.flags.govBondMarket then refRate
    else
      val termPremium   = p.fiscal.govTermPremium.toDouble
      val fiscalRisk    = Math.min(FiscalRiskCap, p.fiscal.govFiscalRiskBeta * Math.max(0.0, debtToGdp - DebtThreshold))
      val qeCompress    = QeCompressionCoeff * nbpBondGdpShare
      val foreignDemand = if nfa > PLN.Zero then ForeignDemandDiscount else 0.0
      Rate(Math.max(0.0, refRate.toDouble + termPremium + fiscalRisk - qeCompress - foreignDemand + credibilityPremium))

  // ---------------------------------------------------------------------------
  // QE
  // ---------------------------------------------------------------------------

  /** Should NBP activate QE? Rate near floor + inflation well below target. */
  def shouldActivateQe(refRate: Rate, inflation: Rate)(using p: SimParams): Boolean =
    p.flags.nbpQe &&
      refRate.toDouble <= p.monetary.rateFloor.toDouble + QeActivationSlack &&
      inflation.toDouble < p.monetary.targetInfl.toDouble - QeDeflationThreshold

  /** Should NBP taper QE? Inflation returned above target. */
  def shouldTaperQe(inflation: Rate)(using p: SimParams): Boolean =
    inflation > p.monetary.targetInfl

  /** Execute monthly QE purchase. */
  def executeQe(nbp: State, bankBondHoldings: PLN, annualGdp: PLN)(using p: SimParams): QeResult =
    if !nbp.qeActive then QeResult(nbp, PLN.Zero)
    else
      val maxByGdp  = (annualGdp * p.monetary.qeMaxGdpShare.toDouble - nbp.govBondHoldings).max(PLN.Zero)
      val available = bankBondHoldings
      val purchase  = PLN.Zero.max(maxByGdp.min(available).min(PLN(p.monetary.qePace.toDouble)))
      val newNbp    = nbp.copy(
        govBondHoldings = nbp.govBondHoldings + purchase,
        qeCumulative = nbp.qeCumulative + purchase,
      )
      QeResult(newNbp, purchase)

  // ---------------------------------------------------------------------------
  // FX intervention
  // ---------------------------------------------------------------------------

  /** Sterilized FX intervention. NBP buys/sells EUR to dampen ER deviations
    * beyond the tolerance band. Sterilized: affects only ER, not bank
    * deposits/capital.
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
      if Math.abs(erDev) <= p.monetary.fxBand.toDouble then FxInterventionResult(0.0, 0.0, reserves)
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
        FxInterventionResult(erEffect, eurTraded, Math.max(0.0, newReserves))
