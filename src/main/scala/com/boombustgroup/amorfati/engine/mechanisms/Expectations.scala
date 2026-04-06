package com.boombustgroup.amorfati.engine.mechanisms

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

/** Adaptive-anchoring inflation expectations with endogenous credibility.
  *
  * Model: Demertzis, Marcellino & Viegi (2012) "A credibility proxy" augmented
  * with asymmetric credibility dynamics (harder to build than lose).
  *
  * Expected inflation = cred × target + (1 − cred) × adaptive, where adaptive
  * follows a simple learning rule (λ-weighted forecast error). Credibility
  * evolves asymmetrically: builds slowly when |π − π*| < threshold, erodes
  * faster when |π − π*| > threshold.
  *
  * When NBP forward guidance is enabled, the expected policy rate blends a
  * Taylor-rule forward guidance signal (60%) with the adaptive rate (40%).
  */
object Expectations:

  // ---- Calibration constants ----
  private val MinCredibility             = 0.01 // floor on credibility index
  private val OutputGapClamp             = 0.05 // ±5 pp unemployment gap clamp
  private val FgBlendWeight              = 0.6  // weight on forward guidance in expected rate
  private val UndershootLearningWeight   = 0.35 // below-target inflation updates expectations more slowly
  private val UndershootCredibilityScale = 0.35 // below-target inflation erodes credibility less than overshooting
  private val MaxTargetUndershoot        = 0.03 // expectations can drift at most 3pp below target at zero credibility

  case class State(
      expectedInflation: Rate,  // πᵉ: anchored inflation expectation
      expectedRate: Rate,       // rᵉ: expected policy rate
      credibility: Share,       // κ ∈ [0.01, 1.0]: CB credibility index
      forecastError: Rate,      // eₜ = πₜ − πᵉₜ₋₁
      forwardGuidanceRate: Rate, // Taylor-rule implied rate (= currentRate when FG off)
  )

  def initial(using p: SimParams): State = State(
    expectedInflation = p.monetary.targetInfl,
    expectedRate = p.monetary.initialRate,
    credibility = p.labor.expCredibilityInit,
    forecastError = Rate.Zero,
    forwardGuidanceRate = p.monetary.initialRate,
  )

  /** Monthly update: forecast error → adaptive learning → anchoring →
    * credibility → FG.
    */
  @boundaryEscape
  def step(prev: State, realizedInflation: Double, currentRate: Double, unemployment: Double)(using p: SimParams): State =
    import ComputationBoundary.toDouble
    val target          = toDouble(p.monetary.targetInfl)
    val lambda          = toDouble(p.labor.expLambda)
    val prevCredibility = toDouble(prev.credibility)

    // Adaptive learning: πᵉ_adaptive = πᵉₜ₋₁ + λ(πₜ − πᵉₜ₋₁)
    val error          = realizedInflation - toDouble(prev.expectedInflation)
    val learningWeight = if realizedInflation < target then UndershootLearningWeight else 1.0
    val adaptive       = toDouble(prev.expectedInflation) + lambda * learningWeight * error

    // Anchoring: blend target with adaptive based on credibility
    val anchored = prevCredibility * target + (1.0 - prevCredibility) * adaptive
    val expected = lowerAnchor(anchored, target, prevCredibility)

    val newCred = updateCredibility(prevCredibility, realizedInflation, target)
    val fgRate  = forwardGuidance(expected, target, unemployment, currentRate)

    // Expected rate: adaptive learning on policy rate, blended with FG when enabled
    val adaptiveRate = toDouble(prev.expectedRate) + lambda * (currentRate - toDouble(prev.expectedRate))
    val expRate      =
      if true then FgBlendWeight * fgRate + (1.0 - FgBlendWeight) * adaptiveRate
      else adaptiveRate

    State(
      expectedInflation = Rate(expected),
      expectedRate = Rate(expRate),
      credibility = Share(newCred),
      forecastError = Rate(error),
      forwardGuidanceRate = Rate(fgRate),
    )

  /** Asymmetric credibility update: builds via (1−κ) scaling, erodes via κ
    * scaling.
    */
  @boundaryEscape
  private def updateCredibility(cred: Double, realizedInflation: Double, target: Double)(using p: SimParams): Double =
    import ComputationBoundary.toDouble
    val absDeviation = Math.abs(realizedInflation - target)
    val threshold    = toDouble(p.labor.expCredibilityThreshold)
    val speed        = toDouble(p.labor.expCredibilitySpeed)
    val raw          =
      if absDeviation <= threshold then cred + speed * (1.0 - cred) * (threshold - absDeviation) / threshold
      else
        val erosionScale = if realizedInflation < target then UndershootCredibilityScale else 1.0
        cred - speed * cred * erosionScale * (absDeviation - threshold) / threshold
    Math.max(MinCredibility, Math.min(1.0, raw))

  /** Keep disinflationary episodes from fully de-anchoring expectations in the
    * baseline regime. Low credibility can pull expectations below target, but
    * only within a bounded undershoot band.
    */
  private def lowerAnchor(expected: Double, target: Double, credibility: Double): Double =
    val lowerBound = target - (1.0 - credibility) * MaxTargetUndershoot
    Math.max(lowerBound, expected)

  /** Taylor-rule forward guidance: r_fg = r* + α(πᵉ − π*) − δ·gap. */
  @boundaryEscape
  private def forwardGuidance(expected: Double, target: Double, unemployment: Double, currentRate: Double)(using p: SimParams): Double =
    import ComputationBoundary.toDouble
    val _            = currentRate
    val nairu        = toDouble(p.monetary.nairu)
    val rawOutputGap = unemployment - nairu
    val outputGap    = Math.max(-OutputGapClamp, Math.min(OutputGapClamp, rawOutputGap))
    val rawFg        = toDouble(p.monetary.neutralRate) + toDouble(p.monetary.taylorAlpha) * (expected - target) - toDouble(p.monetary.taylorDelta) * outputGap
    Math.max(toDouble(p.monetary.rateFloor), Math.min(toDouble(p.monetary.rateCeiling), rawFg))
