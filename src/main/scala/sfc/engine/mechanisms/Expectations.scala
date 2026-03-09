package sfc.engine.mechanisms

import sfc.config.{RunConfig, SimParams}
import sfc.types.*

object Expectations:
  case class State(
      expectedInflation: Rate = Rate(0.025),
      expectedRate: Rate = Rate(0.0575),
      credibility: Ratio = Ratio(0.8),
      forecastError: Rate = Rate.Zero,
      forwardGuidanceRate: Rate = Rate(0.0575),
  )

  def zero: State = State()

  def initial(using p: SimParams): State = State(
    expectedInflation = Rate(p.monetary.targetInfl.toDouble),
    expectedRate = Rate(p.monetary.initialRate.toDouble),
    credibility = Ratio(p.labor.expCredibilityInit.toDouble),
    forecastError = Rate.Zero,
    forwardGuidanceRate = Rate(p.monetary.initialRate.toDouble),
  )

  def step(prev: State, realizedInflation: Double, currentRate: Double, unemployment: Double, rc: RunConfig)(using
      p: SimParams,
  ): State =

    // 1. Forecast error
    val error = realizedInflation - prev.expectedInflation.toDouble

    // 2. Adaptive update
    val adaptive = prev.expectedInflation.toDouble + p.labor.expLambda.toDouble * error

    // 3. Anchoring: blend target with adaptive based on credibility
    val target   = p.monetary.targetInfl.toDouble
    val cred     = prev.credibility.toDouble
    val expected = cred * target + (1.0 - cred) * adaptive

    // 4. Credibility update (asymmetric: harder to build than to lose)
    val absDeviation   = Math.abs(realizedInflation - target)
    val threshold      = p.labor.expCredibilityThreshold.toDouble
    val speed          = p.labor.expCredibilitySpeed.toDouble
    val rawCredibility =
      if absDeviation <= threshold then
        // Below threshold: build credibility
        cred + speed * (1.0 - cred) *
          (threshold - absDeviation) / threshold
      else
        // Above threshold: erode credibility
        cred - speed * cred *
          (absDeviation - threshold) / threshold
    val newCredibility = Math.max(0.01, Math.min(1.0, rawCredibility))

    // 5. Forward guidance rate (when enabled)
    val neutral      = p.monetary.neutralRate.toDouble
    val alpha        = p.monetary.taylorAlpha
    val delta        = p.monetary.taylorDelta
    val nairu        = p.monetary.nairu.toDouble
    val rawOutputGap = (unemployment - nairu) / nairu
    val outputGap    = Math.max(-0.30, Math.min(0.30, rawOutputGap))

    val fgRate = if p.flags.nbpForwardGuidance then
      val rawFg = neutral + alpha * (expected - target) - delta * outputGap
      Math.max(p.monetary.rateFloor.toDouble, Math.min(p.monetary.rateCeiling.toDouble, rawFg))
    else currentRate

    // 6. Expected rate
    val adaptiveRate =
      prev.expectedRate.toDouble + p.labor.expLambda.toDouble * (currentRate - prev.expectedRate.toDouble)
    val expRate      =
      if p.flags.nbpForwardGuidance then 0.6 * fgRate + 0.4 * adaptiveRate
      else adaptiveRate

    State(
      expectedInflation = Rate(expected),
      expectedRate = Rate(expRate),
      credibility = Ratio(newCredibility),
      forecastError = Rate(error),
      forwardGuidanceRate = Rate(fgRate),
    )
