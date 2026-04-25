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
  private val MinCredibility             = Share("0.01")       // floor on credibility index
  private val OutputGapClamp             = Coefficient("0.05") // ±5 pp unemployment gap clamp
  private val FgBlendWeight              = Share("0.6")        // weight on forward guidance in expected rate
  private val UndershootLearningWeight   = Coefficient("0.35") // below-target inflation updates expectations more slowly
  private val UndershootCredibilityScale = Share("0.35")       // below-target inflation erodes credibility less than overshooting
  private val MaxTargetUndershoot        = Rate("0.03")        // expectations can drift at most 3pp below target at zero credibility

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
  def step(prev: State, realizedInflation: Rate, currentRate: Rate, unemployment: Share)(using p: SimParams): State =
    val target          = p.monetary.targetInfl
    val lambda          = p.labor.expLambda
    val prevCredibility = prev.credibility

    // Adaptive learning: πᵉ_adaptive = πᵉₜ₋₁ + λ(πₜ − πᵉₜ₋₁)
    val error          = realizedInflation - prev.expectedInflation
    val learningWeight = if realizedInflation < target then UndershootLearningWeight else Coefficient.One
    val adaptive       = prev.expectedInflation + error * lambda * learningWeight

    // Anchoring: blend target with adaptive based on credibility
    val anchored = target * prevCredibility + adaptive * prevCredibility.complement
    val expected = lowerAnchor(anchored, target, prevCredibility)

    val newCred = updateCredibility(prevCredibility, realizedInflation, target)
    val fgRate  = forwardGuidance(expected, target, unemployment, currentRate)

    // Expected rate: adaptive learning on policy rate, blended with FG when enabled
    val adaptiveRate = prev.expectedRate + (currentRate - prev.expectedRate) * lambda
    val expRate      = fgRate * FgBlendWeight + adaptiveRate * FgBlendWeight.complement

    State(
      expectedInflation = expected,
      expectedRate = expRate,
      credibility = newCred,
      forecastError = error,
      forwardGuidanceRate = fgRate,
    )

  /** Asymmetric credibility update: builds via (1−κ) scaling, erodes via κ
    * scaling.
    */
  private def updateCredibility(cred: Share, realizedInflation: Rate, target: Rate)(using p: SimParams): Share =
    val absDeviation = (realizedInflation - target).abs
    val threshold    = p.labor.expCredibilityThreshold
    val speed        = p.labor.expCredibilitySpeed.toShare
    val raw          =
      if absDeviation <= threshold then
        val improvement = speed * (threshold - absDeviation).ratioTo(threshold).clampToShare * cred.complement
        cred + improvement
      else
        val erosionScale = if realizedInflation < target then UndershootCredibilityScale else Share.One
        val erosion      = speed * (absDeviation - threshold).ratioTo(threshold).clampToShare * cred * erosionScale
        cred - erosion
    raw.clamp(MinCredibility, Share.One)

  /** Keep disinflationary episodes from fully de-anchoring expectations in the
    * baseline regime. Low credibility can pull expectations below target, but
    * only within a bounded undershoot band.
    */
  private def lowerAnchor(expected: Rate, target: Rate, credibility: Share): Rate =
    val lowerBound = target - MaxTargetUndershoot * credibility.complement
    expected.max(lowerBound)

  /** Taylor-rule forward guidance: r_fg = r* + α(πᵉ − π*) − δ·gap. */
  private def forwardGuidance(expected: Rate, target: Rate, unemployment: Share, currentRate: Rate)(using p: SimParams): Rate =
    val _            = currentRate
    val rawOutputGap = unemployment.toCoefficient - p.monetary.nairu.toCoefficient
    val outputGap    = rawOutputGap.clamp(-OutputGapClamp, OutputGapClamp)
    val rawFg        =
      p.monetary.neutralRate +
        (expected - target) * p.monetary.taylorAlpha -
        (p.monetary.taylorDelta * outputGap).toRate
    rawFg.clamp(p.monetary.rateFloor, p.monetary.rateCeiling)
