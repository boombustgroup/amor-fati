package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.fp.FixedPointBase.bankerRound
import com.boombustgroup.amorfati.types.*

/** National Bank of Poland: Taylor rule, bond yield, QE, FX intervention. */
object Nbp:

  // ---------------------------------------------------------------------------
  // Named constants
  // ---------------------------------------------------------------------------

  private val OutputGapCap          = Coefficient(0.05) // ±5 pp unemployment gap cap in Taylor rule
  private val DebtThreshold         = Share(0.40)       // debt-to-GDP threshold for fiscal risk premium
  private val FiscalRiskCap         = Rate(0.10)        // maximum fiscal risk premium
  private val CredPremiumCap        = Rate(0.05)        // max credibility premium (5pp, ~Turkey 2018)
  private val BondYieldCap          = Rate(0.20)        // absolute yield ceiling (20%, beyond any EM precedent)
  private val QeCompressionCoeff    = Rate(0.5)         // yield compression per unit of NBP bond/GDP share
  private val ForeignDemandDiscount = Rate(0.005)       // yield discount when NFA > 0
  private val QeActivationSlack     = Rate(0.0025)      // rate proximity to floor for QE activation
  private val QeDeflationThreshold  = Rate(0.01)        // inflation must be this much below target for QE
  private val ZlbExitBuffer         = Rate(0.0025)      // expectations/inflation should clear target by this margin to exit ZLB mode
  private val ZlbQeMaxMultiplier    = Multiplier(3.0)   // QE pace can scale up to 3x in a lower-bound regime

  // ---------------------------------------------------------------------------
  // State
  // ---------------------------------------------------------------------------

  case class PolicyState(
      referenceRate: Rate, // NBP reference rate (annualised)
      qeActive: Boolean,   // whether QE programme is currently active
  )

  case class BalanceState(
      govBondHoldings: PLN, // current NBP government bond position (QE + open market), i.e. the ledger-coverable stock
      qeCumulative: PLN,    // cumulative purchases executed under the QE regime; accounting/policy metric, not a separate ledger asset
      fxReserves: PLN,      // EUR-equivalent total reserves (multi-currency)
  )

  case class MonthlyOpsState(
      lastFxTraded: PLN, // monthly FX intervention amount (EUR, +bought/−sold)
  )

  case class State(
      policy: PolicyState,
      balance: BalanceState,
      monthly: MonthlyOpsState,
  ):
    def referenceRate: Rate  = policy.referenceRate
    def qeActive: Boolean    = policy.qeActive
    def govBondHoldings: PLN = balance.govBondHoldings
    def qeCumulative: PLN    = balance.qeCumulative
    def fxReserves: PLN      = balance.fxReserves
    def lastFxTraded: PLN    = monthly.lastFxTraded

  object State:
    def apply(
        referenceRate: Rate,
        govBondHoldings: PLN,
        qeActive: Boolean,
        qeCumulative: PLN,
        fxReserves: PLN,
        lastFxTraded: PLN,
    ): State =
      State(
        policy = PolicyState(referenceRate, qeActive),
        balance = BalanceState(govBondHoldings, qeCumulative, fxReserves),
        monthly = MonthlyOpsState(lastFxTraded),
      )

  // ---------------------------------------------------------------------------
  // QE result type
  // ---------------------------------------------------------------------------

  /** Result of monthly QE execution. */
  /** QE purchase request — NBP state without bond update + requested amount.
    * Actual purchase happens in the bond waterfall (BankingEconomics).
    */
  case class QeRequest(
      nbpState: State,       // NBP state (govBondHoldings NOT yet updated)
      requestedPurchase: PLN, // how much QE wants to buy (actual capped by bank supply in waterfall)
  )

  // ---------------------------------------------------------------------------
  // FX intervention result
  // ---------------------------------------------------------------------------

  /** FX intervention result. */
  case class FxInterventionResult(
      erShock: ExchangeRateShock, // signed exchange-rate shock injected into OpenEconomy
      eurTraded: PLN,             // positive = bought EUR (weakened PLN), negative = sold EUR
      newReserves: PLN,           // updated NBP FX reserve level (EUR)
      plnInjection: PLN,          // PLN injected (+) or drained (−) from banking system reserves
  )

  // ---------------------------------------------------------------------------
  // Taylor rule
  // ---------------------------------------------------------------------------

  /** Raw Taylor target: symmetric (dual mandate) or asymmetric
    * (inflation-only).
    */
  private def taylorTarget(
      inflation: Rate,
      exRateChange: Coefficient,
      employed: Int,
      totalPopulation: Int,
  )(using p: SimParams): Rate =
    val infGap    = inflation - p.monetary.targetInfl
    val unempRate = Share.One - Share.fraction(employed, totalPopulation)
    val nairu     = p.monetary.nairu

    val rawOutputGap = Coefficient.fromRaw((unempRate - nairu).toLong)
    val outputGap    = rawOutputGap.max(-OutputGapCap).min(OutputGapCap)
    p.monetary.neutralRate +
      infGap * p.monetary.taylorAlpha -
      (outputGap * p.monetary.taylorDelta).toMultiplier.toRate + // Coefficient × Coefficient → Rate
      (exRateChange * p.monetary.taylorBeta).toMultiplier.toRate // Coefficient × Coefficient → Rate

  /** Inertia smoothing + max rate change clamping. */
  private def smoothAndClamp(prevRate: Rate, taylor: Rate)(using p: SimParams): Rate =
    val inertia  = p.monetary.taylorInertia
    val smoothed = prevRate * inertia + taylor * (Share.One - inertia)
    if p.monetary.maxRateChange > Rate.Zero then
      val delta = (smoothed - prevRate).clamp(-p.monetary.maxRateChange, p.monetary.maxRateChange)
      prevRate + delta
    else smoothed

  /** Floor/ceiling clamp to [rateFloor, rateCeiling]. */
  private def clampRate(rate: Rate)(using p: SimParams): Rate =
    rate.clamp(p.monetary.rateFloor, p.monetary.rateCeiling)

  /** Update NBP reference rate via Taylor rule. */
  def updateRate(
      prevRate: Rate,
      inflation: Rate,
      exRateChange: Coefficient,
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
      debtToGdp: Share,
      nbpBondGdpShare: Share,
      nfa: PLN,
      credibilityPremium: Rate,
  )(using p: SimParams): Rate =
    val fiscalRisk     = piecewiseFiscalRisk(debtToGdp)
    val qeCompress     = QeCompressionCoeff * nbpBondGdpShare
    val foreignDemand  = if nfa > PLN.Zero then ForeignDemandDiscount else Rate.Zero
    val cappedCredPrem = credibilityPremium.min(CredPremiumCap)
    (refRate + p.fiscal.govTermPremium + fiscalRisk - qeCompress - foreignDemand + cappedCredPrem).max(Rate.Zero).min(BondYieldCap)

  /** Piecewise fiscal risk premium: steepens at 55% and 60% debt/GDP. base
    * segment (40%+) + caution segment (55%+) + crisis segment (60%+).
    */
  private def piecewiseFiscalRisk(debtToGdp: Share)(using p: SimParams): Rate =
    val base      = (p.fiscal.govFiscalRiskBeta * (debtToGdp - DebtThreshold).max(Share.Zero)).toMultiplier.toRate
    val caution55 =
      if debtToGdp > p.fiscal.fiscalRuleCautionThreshold then
        (p.fiscal.fiscalRiskBeta55 * (debtToGdp - p.fiscal.fiscalRuleCautionThreshold)).toMultiplier.toRate
      else Rate.Zero
    val crisis60  =
      if debtToGdp > p.fiscal.fiscalRuleDebtCeiling then (p.fiscal.fiscalRiskBeta60 * (debtToGdp - p.fiscal.fiscalRuleDebtCeiling)).toMultiplier.toRate
      else Rate.Zero
    (base + caution55 + crisis60).min(FiscalRiskCap)

  // ---------------------------------------------------------------------------
  // QE
  // ---------------------------------------------------------------------------

  /** Lower-bound regime: policy rate at/near floor and nominal conditions still
    * materially below target.
    */
  def inLowerBoundRegime(refRate: Rate, inflation: Rate, expectedInflation: Rate)(using p: SimParams): Boolean =
    refRate <= p.monetary.rateFloor + QeActivationSlack &&
      (inflation < p.monetary.targetInfl - QeDeflationThreshold ||
        expectedInflation < p.monetary.targetInfl - QeDeflationThreshold)

  /** Should NBP activate QE? Rate near floor + realized or expected inflation
    * well below target.
    */
  def shouldActivateQe(refRate: Rate, inflation: Rate, expectedInflation: Rate)(using p: SimParams): Boolean =
    inLowerBoundRegime(refRate, inflation, expectedInflation)

  /** Should NBP taper QE? Exit only after both realized and expected inflation
    * have recovered above target.
    */
  def shouldTaperQe(inflation: Rate, expectedInflation: Rate)(using p: SimParams): Boolean =
    inflation > p.monetary.targetInfl + ZlbExitBuffer &&
      expectedInflation > p.monetary.targetInfl

  /** Compute QE purchase request. Does NOT update govBondHoldings — the bond
    * waterfall in BankingEconomics handles the actual transfer so that SFC
    * clears by construction (actualSold from banks = actualSold to NBP).
    *
    * `qeCumulative` is intentionally distinct from current `govBondHoldings`:
    * it tracks purchases attributed to the QE regime, not the full NBP bond
    * stock.
    */
  def executeQe(nbp: State, bankBondHoldings: PLN, annualGdp: PLN, inflation: Rate, expectedInflation: Rate)(using p: SimParams): QeRequest =
    if !nbp.qeActive then QeRequest(nbp, PLN.Zero)
    else
      val maxByGdp  = (annualGdp * p.monetary.qeMaxGdpShare - nbp.govBondHoldings).max(PLN.Zero)
      val available = bankBondHoldings
      val pace      = qePurchasePace(nbp.referenceRate, inflation, expectedInflation)
      val purchase  = PLN.Zero.max(maxByGdp.min(available).min(pace))
      QeRequest(nbp, purchase)

  private def qePurchasePace(refRate: Rate, inflation: Rate, expectedInflation: Rate)(using p: SimParams): PLN =
    if !inLowerBoundRegime(refRate, inflation, expectedInflation) then p.monetary.qePace
    else
      val realizedShortfall = (p.monetary.targetInfl - inflation).max(Rate.Zero)
      val expectedShortfall = (p.monetary.targetInfl - expectedInflation).max(Rate.Zero)
      val severity          = realizedShortfall.max(expectedShortfall)
      val multiplier        = (Multiplier.One + severity.ratioTo(QeDeflationThreshold).toMultiplier).min(ZlbQeMaxMultiplier)
      p.monetary.qePace * multiplier

  // ---------------------------------------------------------------------------
  // FX intervention
  // ---------------------------------------------------------------------------

  /** FX intervention. NBP buys/sells EUR to dampen ER deviations beyond the
    * tolerance band. EUR purchase injects PLN into banking system reserves; EUR
    * sale drains PLN. The PLN injection feeds into the liquidity-aware
    * interbank rate (#9) via bank reservesAtNbp.
    */
  def fxIntervention(
      prevER: ExchangeRate,
      reserves: PLN,
      gdp: PLN,
      enabled: Boolean,
  )(using p: SimParams): FxInterventionResult =
    val baseRate = ExchangeRate(p.forex.baseExRate)
    if !enabled then FxInterventionResult(ExchangeRateShock.Zero, PLN.Zero, reserves, PLN.Zero)
    else
      val erDeviation = prevER.deviationFrom(baseRate)
      if erDeviation.abs.toScalar <= p.monetary.fxBand.toScalar then FxInterventionResult(ExchangeRateShock.Zero, PLN.Zero, reserves, PLN.Zero)
      else
        val direction      = -erDeviation.sign
        val maxByReserves  = reserves * p.monetary.fxMaxMonthly
        val tradeMagnitude = if direction < 0 then maxByReserves.min(reserves) else maxByReserves
        val eurTraded      = if direction < 0 then -tradeMagnitude else tradeMagnitude
        val newReserves    = (reserves + eurTraded).max(PLN.Zero)
        val tradedPlnValue = PLN.fromRaw(bankerRound(BigInt(eurTraded.abs.toLong) * BigInt(prevER.toLong)))
        val gdpEffect      = if gdp > PLN.Zero then tradedPlnValue.ratioTo(gdp) else Scalar.Zero
        val unsignedShock  = ExchangeRateShock.fromRaw((gdpEffect * p.monetary.fxStrength).toLong)
        val erShock        = if direction < 0 then -unsignedShock else unsignedShock
        // PLN injection: EUR purchase → NBP pays PLN to banks (+), EUR sale → banks pay PLN to NBP (−)
        val plnInjection   = PLN.fromRaw(bankerRound(BigInt(eurTraded.toLong) * BigInt(prevER.toLong)))
        FxInterventionResult(erShock, eurTraded, newReserves, plnInjection)
