package sfc.engine

import sfc.config.{Config, RunConfig}
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

  def initial: State = State(
    expectedInflation = Rate(Config.NbpTargetInfl),
    expectedRate = Rate(Config.NbpInitialRate),
    credibility = Ratio(Config.ExpCredibilityInit),
    forecastError = Rate.Zero,
    forwardGuidanceRate = Rate(Config.NbpInitialRate),
  )

  def step(prev: State, realizedInflation: Double, currentRate: Double, unemployment: Double, rc: RunConfig): State =

    // 1. Forecast error
    val error = realizedInflation - prev.expectedInflation.toDouble

    // 2. Adaptive update
    val adaptive = prev.expectedInflation.toDouble + Config.ExpLambda * error

    // 3. Anchoring: blend target with adaptive based on credibility
    val target = if rc.isEurozone then Config.EcbTargetInfl else Config.NbpTargetInfl
    val cred = prev.credibility.toDouble
    val expected = cred * target + (1.0 - cred) * adaptive

    // 4. Credibility update (asymmetric: harder to build than to lose)
    val absDeviation = Math.abs(realizedInflation - target)
    val threshold = Config.ExpCredibilityThreshold
    val speed = Config.ExpCredibilitySpeed
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
    val neutral = if rc.isEurozone then Config.EcbNeutralRate else Config.NbpNeutralRate
    val alpha = if rc.isEurozone then Config.EcbAlpha else Config.TaylorAlpha
    val delta = if rc.isEurozone then 0.0 else Config.TaylorDelta
    val nairu = Config.NbpNairu
    val rawOutputGap = (unemployment - nairu) / nairu
    val outputGap = Math.max(-0.30, Math.min(0.30, rawOutputGap))

    val fgRate = if Config.NbpForwardGuidance then
      val rawFg = neutral + alpha * (expected - target) - delta * outputGap
      Math.max(Config.RateFloor, Math.min(Config.RateCeiling, rawFg))
    else currentRate

    // 6. Expected rate
    val adaptiveRate = prev.expectedRate.toDouble + Config.ExpLambda * (currentRate - prev.expectedRate.toDouble)
    val expRate =
      if Config.NbpForwardGuidance then 0.6 * fgRate + 0.4 * adaptiveRate
      else adaptiveRate

    State(
      expectedInflation = Rate(expected),
      expectedRate = Rate(expRate),
      credibility = Ratio(newCredibility),
      forecastError = Rate(error),
      forwardGuidanceRate = Rate(fgRate),
    )
