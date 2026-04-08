package com.boombustgroup.amorfati.math

import com.boombustgroup.amorfati.types.{PLN, Scalar}

object EuFundsMath:

  def totalEnvelopePln(totalEur: Double, baseExRate: Double, firmsCount: Int, referenceEconomy: Int): PLN =
    PLN(totalEur * baseExRate * firmsCount.toDouble / referenceEconomy)

  def monthlyWeight(month: Int, startMonth: Int, periodMonths: Int, alpha: Double, beta: Double): Scalar =
    val t = monthOffset(month, startMonth, periodMonths)
    if t <= 0.0 || t >= 1.0 then Scalar.Zero
    else Scalar(betaPdf(t, alpha, beta) / periodMonths)

  private def monthOffset(month: Int, startMonth: Int, periodMonths: Int): Double =
    (month - startMonth).toDouble / periodMonths

  private def betaPdf(x: Double, alpha: Double, beta: Double): Double =
    if x <= 0.0 || x >= 1.0 then 0.0
    else
      val logB = lnGamma(alpha) + lnGamma(beta) - lnGamma(alpha + beta)
      Math.exp((alpha - 1.0) * Math.log(x) + (beta - 1.0) * Math.log(1.0 - x) - logB)

  private def lnGamma(z: Double): Double =
    val g    = 5.0
    val coef = Vector(
      76.18009172947146, -86.50532032941677, 24.01409824083091, -1.231739572450155, 0.1208650973866179e-2, -0.5395239384953e-5,
    )
    val x    = z - 1.0
    val tmp  = (x + 0.5) * Math.log(x + g + 0.5) - (x + g + 0.5)
    val ser  = coef.zipWithIndex.foldLeft(1.000000000190015) { case (acc, (c, j)) => acc + c / (x + 1.0 + j) }
    tmp + Math.log(2.5066282746310005 * ser)
