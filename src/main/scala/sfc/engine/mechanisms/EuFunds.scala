package sfc.engine.mechanisms

import sfc.config.SimParams

object EuFunds:

  /** Monthly EU transfer in PLN, following a Beta(α,β) absorption curve. */
  def monthlyTransfer(month: Int)(using p: SimParams): Double =
    val totalPln = p.fiscal.euFundsTotalEur * p.forex.baseExRate *
      (p.pop.firmsCount.toDouble / 10000.0)
    val T        = p.fiscal.euFundsPeriodMonths
    val t        = (month - p.fiscal.euFundsStartMonth).toDouble / T
    if t <= 0.0 || t >= 1.0 then 0.0
    else totalPln * betaPdf(t, p.fiscal.euFundsAlpha, p.fiscal.euFundsBeta) / T

  /** Domestic co-financing (gov budget). */
  def cofinancing(euMonthly: Double)(using p: SimParams): Double =
    euMonthly * p.fiscal.euCofinanceRate.toDouble / (1.0 - p.fiscal.euCofinanceRate.toDouble)

  /** Capital portion of total EU project spending (EU + cofin). */
  def capitalInvestment(euMonthly: Double, cofin: Double)(using p: SimParams): Double =
    (euMonthly + cofin) * p.fiscal.euCapitalShare.toDouble

  /** Beta probability density function. */
  private[engine] def betaPdf(x: Double, a: Double, b: Double): Double =
    if x <= 0.0 || x >= 1.0 then 0.0
    else
      val logB = lnGamma(a) + lnGamma(b) - lnGamma(a + b)
      Math.exp((a - 1.0) * Math.log(x) + (b - 1.0) * Math.log(1.0 - x) - logB)

  /** Log-gamma via Lanczos approximation (6 coefficients, Numerical Recipes).
    */
  private[engine] def lnGamma(z: Double): Double =
    val g    = 5.0
    val coef = Array(
      76.18009172947146, -86.50532032941677, 24.01409824083091, -1.231739572450155, 0.1208650973866179e-2, -0.5395239384953e-5,
    )
    val x    = z - 1.0
    var tmp  = x + g + 0.5
    tmp = (x + 0.5) * Math.log(tmp) - tmp
    var ser  = 1.000000000190015
    for j <- coef.indices do ser += coef(j) / (x + 1.0 + j)
    tmp + Math.log(2.5066282746310005 * ser)
