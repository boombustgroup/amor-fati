package com.boombustgroup.amorfati.math

import com.boombustgroup.amorfati.fp.FixedPointBase
import com.boombustgroup.amorfati.types.*

/** Fixed-point helpers for EU-funds timing and envelope scaling. */
object EuFundsMath:

  /** Scales the total EU envelope into PLN for the simulated economy size. */
  def totalEnvelopePln(totalEur: Multiplier, baseExRate: ExchangeRate, firmsCount: Int, referenceEconomy: Int): PLN =
    require(referenceEconomy > 0, s"referenceEconomy must be positive: $referenceEconomy")
    require(totalEur >= Multiplier.Zero, s"totalEur must be non-negative: $totalEur")
    require(baseExRate.toLong > 0L, s"baseExRate must be positive: $baseExRate")
    require(firmsCount > 0, s"firmsCount must be positive: $firmsCount")
    PLN.fromRaw(FixedPointBase.multiplyRaw(totalEur.toLong, baseExRate.toLong)) * firmsCount / referenceEconomy

  /** Returns the normalized monthly absorption weight from a Beta(alpha, beta)
    * draw-down profile.
    */
  def monthlyWeight(month: Int, startMonth: Int, periodMonths: Int, alpha: Scalar, beta: Scalar): Scalar =
    require(periodMonths > 0, s"periodMonths must be positive: $periodMonths")
    require(alpha > Scalar.Zero, s"alpha must be positive: $alpha")
    require(beta > Scalar.Zero, s"beta must be positive: $beta")
    val monthIndex = month - startMonth
    if monthIndex < 0 || monthIndex >= periodMonths then Scalar.Zero
    else
      val rawWeights = (0 until periodMonths).map: idx =>
        betaMidpointDensity(idx, periodMonths, alpha, beta)
      val totalMass  = rawWeights.foldLeft(Scalar.Zero)(_ + _)
      require(totalMass > Scalar.Zero, s"monthlyWeight normalization mass must be positive: $totalMass")
      rawWeights(monthIndex).ratioTo(totalMass)

  private def betaMidpointDensity(idx: Int, periodMonths: Int, alpha: Scalar, beta: Scalar): Scalar =
    val midpoint = Scalar.fraction(2 * idx + 1, 2 * periodMonths)
    midpoint.pow(alpha - Scalar.One) * (Scalar.One - midpoint).pow(beta - Scalar.One)
