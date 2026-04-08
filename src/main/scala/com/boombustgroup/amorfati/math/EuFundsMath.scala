package com.boombustgroup.amorfati.math

import com.boombustgroup.amorfati.types.{PLN, Scalar}

/** Numeric helpers for EU-funds timing and envelope scaling.
  *
  * This keeps floating-point Beta-distribution math outside the typed
  * mechanisms package while returning typed fixed-point results to callers.
  */
object EuFundsMath:
  /** Lanczos coefficients used by [[lnGamma]] for the Beta density
    * normalization term.
    */
  private val LnGammaCoef = Vector(
    76.18009172947146, -86.50532032941677, 24.01409824083091, -1.231739572450155, 0.1208650973866179e-2, -0.5395239384953e-5,
  )

  /** Scales the total EU envelope into PLN for the simulated economy size. */
  def totalEnvelopePln(totalEur: Double, baseExRate: Double, firmsCount: Int, referenceEconomy: Int): PLN =
    require(referenceEconomy > 0, s"referenceEconomy must be positive: $referenceEconomy")
    require(totalEur.isFinite, s"totalEur must be finite: $totalEur")
    require(totalEur >= 0.0, s"totalEur must be non-negative: $totalEur")
    require(baseExRate.isFinite, s"baseExRate must be finite: $baseExRate")
    require(baseExRate > 0.0, s"baseExRate must be positive: $baseExRate")
    require(firmsCount > 0, s"firmsCount must be positive: $firmsCount")
    PLN(totalEur * baseExRate * firmsCount.toDouble / referenceEconomy)

  /** Returns the normalized monthly absorption weight from a Beta(alpha, beta)
    * draw-down profile.
    */
  def monthlyWeight(month: Int, startMonth: Int, periodMonths: Int, alpha: Double, beta: Double): Scalar =
    require(periodMonths > 0, s"periodMonths must be positive: $periodMonths")
    require(alpha > 0.0 && alpha.isFinite, s"alpha must be finite and positive: $alpha")
    require(beta > 0.0 && beta.isFinite, s"beta must be finite and positive: $beta")
    val t = monthOffset(month, startMonth, periodMonths)
    require(t.isFinite, s"monthOffset must be finite for month=$month, startMonth=$startMonth, periodMonths=$periodMonths")
    if t <= 0.0 || t >= 1.0 then Scalar.Zero
    else Scalar(betaPdf(t, alpha, beta) / periodMonths)

  /** Converts a simulation month into a [0, 1] position within the programming
    * period.
    */
  private def monthOffset(month: Int, startMonth: Int, periodMonths: Int): Double =
    (month - startMonth).toDouble / periodMonths

  /** Beta probability density used to shape the timing of fund absorption. */
  private def betaPdf(x: Double, alpha: Double, beta: Double): Double =
    require(x > 0.0 && x < 1.0 && x.isFinite, s"x must be finite and in (0, 1): $x")
    require(alpha > 0.0 && alpha.isFinite, s"alpha must be finite and positive: $alpha")
    require(beta > 0.0 && beta.isFinite, s"beta must be finite and positive: $beta")
    val logGammaAlpha = lnGamma(alpha)
    val logGammaBeta  = lnGamma(beta)
    val logGammaSum   = lnGamma(alpha + beta)
    require(logGammaAlpha.isFinite, s"lnGamma(alpha) must be finite: $logGammaAlpha")
    require(logGammaBeta.isFinite, s"lnGamma(beta) must be finite: $logGammaBeta")
    require(logGammaSum.isFinite, s"lnGamma(alpha + beta) must be finite: $logGammaSum")
    val logB          = logGammaAlpha + logGammaBeta - logGammaSum
    val pdf           = Math.exp((alpha - 1.0) * Math.log(x) + (beta - 1.0) * Math.log(1.0 - x) - logB)
    require(pdf.isFinite, s"betaPdf must be finite for x=$x, alpha=$alpha, beta=$beta")
    pdf

  /** Log-gamma via Lanczos approximation, used for the Beta normalization
    * constant.
    */
  private def lnGamma(z: Double): Double =
    val g   = 5.0
    val x   = z - 1.0
    val tmp = (x + 0.5) * Math.log(x + g + 0.5) - (x + g + 0.5)
    val ser = LnGammaCoef.zipWithIndex.foldLeft(1.000000000190015) { case (acc, (c, j)) => acc + c / (x + 1.0 + j) }
    tmp + Math.log(2.5066282746310005 * ser)
