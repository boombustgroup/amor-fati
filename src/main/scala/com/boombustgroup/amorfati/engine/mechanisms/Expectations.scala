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
  private val MinCredibility = 0.01 // floor on credibility index
  private val OutputGapClamp = 0.30 // ±30% clamp on output gap
  private val FgBlendWeight  = 0.6  // weight on forward guidance in expected rate

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
  def step(prev: State, realizedInflation: Double, currentRate: Double, unemployment: Double)(using p: SimParams): State =
    val target = p.monetary.targetInfl.toDouble
    val lambda = p.labor.expLambda.toDouble

    // Adaptive learning: πᵉ_adaptive = πᵉₜ₋₁ + λ(πₜ − πᵉₜ₋₁)
    val error    = realizedInflation - prev.expectedInflation.toDouble
    val adaptive = prev.expectedInflation.toDouble + lambda * error

    // Anchoring: blend target with adaptive based on credibility
    val cred     = prev.credibility.toDouble
    val expected = cred * target + (1.0 - cred) * adaptive

    val newCred = updateCredibility(cred, realizedInflation, target)
    val fgRate  = forwardGuidance(expected, target, unemployment, currentRate)

    // Expected rate: adaptive learning on policy rate, blended with FG when enabled
    val adaptiveRate = prev.expectedRate.toDouble + lambda * (currentRate - prev.expectedRate.toDouble)
    val expRate      =
      if p.flags.nbpForwardGuidance then FgBlendWeight * fgRate + (1.0 - FgBlendWeight) * adaptiveRate
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
  private def updateCredibility(cred: Double, realizedInflation: Double, target: Double)(using p: SimParams): Double =
    val absDeviation = Math.abs(realizedInflation - target)
    val threshold    = p.labor.expCredibilityThreshold.toDouble
    val speed        = p.labor.expCredibilitySpeed.toDouble
    val raw          =
      if absDeviation <= threshold then cred + speed * (1.0 - cred) * (threshold - absDeviation) / threshold
      else cred - speed * cred * (absDeviation - threshold) / threshold
    Math.max(MinCredibility, Math.min(1.0, raw))

  /** Taylor-rule forward guidance: r_fg = r* + α(πᵉ − π*) − δ·gap. */
  private def forwardGuidance(expected: Double, target: Double, unemployment: Double, currentRate: Double)(using p: SimParams): Double =
    if !p.flags.nbpForwardGuidance then currentRate
    else
      val nairu        = p.monetary.nairu.toDouble // Share → Double: function operates in Double space
      val rawOutputGap = (unemployment - nairu) / nairu
      val outputGap    = Math.max(-OutputGapClamp, Math.min(OutputGapClamp, rawOutputGap))
      val rawFg        = p.monetary.neutralRate.toDouble + p.monetary.taylorAlpha * (expected - target) - p.monetary.taylorDelta * outputGap
      Math.max(p.monetary.rateFloor.toDouble, Math.min(p.monetary.rateCeiling.toDouble, rawFg))
