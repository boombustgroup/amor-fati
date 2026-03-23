package com.boombustgroup.amorfati.engine.mechanisms

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

/** EU structural funds absorption with Beta-curve timing.
  *
  * Models the disbursement profile of EU cohesion/structural funds over a
  * multi-year programming period. Absorption follows a Beta(α,β) density
  * (front-loaded when α < β, matching empirical EU fund draw-down patterns).
  *
  * Calibration: MFiPR KPO/FENIKS 2021-2027 programming period. Total envelope:
  * 76 bln EUR, period: 84 months, α=2, β=5 (peak ~20% into the period),
  * co-financing rate: 15%, capital share of spending: 60%.
  *
  * The total is scaled by (firmsCount / ReferenceEconomy) to keep per-firm
  * flows proportional regardless of simulation population size.
  */
object EuFunds:

  private val ReferenceEconomy = 10000.0 // baseline firm count for calibration scaling

  /** Monthly EU transfer in PLN, following a Beta(α,β) absorption curve. */
  def monthlyTransfer(month: Int)(using p: SimParams): Double =
    val totalPln = p.fiscal.euFundsTotalEur * p.forex.baseExRate * (p.pop.firmsCount.toDouble / ReferenceEconomy)
    val T        = p.fiscal.euFundsPeriodMonths
    val t        = (month - p.fiscal.euFundsStartMonth).toDouble / T
    if t <= 0.0 || t >= 1.0 then 0.0
    else totalPln * betaPdf(t, p.fiscal.euFundsAlpha, p.fiscal.euFundsBeta) / T

  /** Domestic co-financing from gov budget: cofin = eu × rate / (1 − rate). */
  @boundaryEscape
  def cofinancing(euMonthly: Double)(using p: SimParams): Double =
    import ComputationBoundary.toDouble
    euMonthly * toDouble(p.fiscal.euCofinanceRate) / (1.0 - toDouble(p.fiscal.euCofinanceRate))

  /** Capital portion of total EU project spending (EU + cofin). */
  @boundaryEscape
  def capitalInvestment(euMonthly: Double, cofin: Double)(using p: SimParams): Double =
    import ComputationBoundary.toDouble
    (euMonthly + cofin) * toDouble(p.fiscal.euCapitalShare)

  /** Beta probability density function: f(x; a, b) = x^(a-1)(1-x)^(b-1) /
    * B(a,b).
    */
  private[engine] def betaPdf(x: Double, a: Double, b: Double): Double =
    if x <= 0.0 || x >= 1.0 then 0.0
    else
      val logB = lnGamma(a) + lnGamma(b) - lnGamma(a + b)
      Math.exp((a - 1.0) * Math.log(x) + (b - 1.0) * Math.log(1.0 - x) - logB)

  /** Log-gamma via Lanczos approximation (6 coefficients, Numerical Recipes
    * §6.1).
    */
  private[engine] def lnGamma(z: Double): Double =
    val g    = 5.0
    val coef = Vector(
      76.18009172947146, -86.50532032941677, 24.01409824083091, -1.231739572450155, 0.1208650973866179e-2, -0.5395239384953e-5,
    )
    val x    = z - 1.0
    val tmp  = (x + 0.5) * Math.log(x + g + 0.5) - (x + g + 0.5)
    val ser  = coef.zipWithIndex.foldLeft(1.000000000190015) { case (acc, (c, j)) => acc + c / (x + 1.0 + j) }
    tmp + Math.log(2.5066282746310005 * ser)
