package com.boombustgroup.amorfati.engine.markets

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

/** Aggregate price level: Phillips-curve-style monthly inflation update.
  *
  * Three channels: demand-pull (output gap proxy via demandMult), cost-push
  * (wage growth pass-through), and import push (exchange rate deviation ×
  * import propensity).
  *
  * A soft floor at −1.5%/month with 30% pass-through approximates downward
  * nominal rigidity (cf. Bewley 1999). These are calibration choices, not
  * empirical estimates — a micro-founded pricing module (menu costs, Calvo
  * staggering) would be a better long-term replacement.
  *
  * Inflation is exponentially smoothed (λ=0.3) to avoid month-to-month noise.
  */
object PriceLevel:

  // ---- Calibration constants ----
  private val DemandPullWeight = Coefficient(0.15)   // sensitivity of inflation to demand gap
  private val CostPushWeight   = Coefficient(0.25)   // wage growth pass-through to prices
  private val ImportPushWeight = Coefficient(0.25)   // FX depreciation pass-through
  private val DeflationFloor   = Coefficient(-0.015) // soft floor: −1.5%/month
  private val FloorPassThrough = Coefficient(0.3)    // beyond floor, 30% pass-through
  private val SmoothingLambda  = Share(0.3)          // EWM weight on new observation
  private val MinPriceLevel    = PriceIndex(0.30)    // absolute floor on price index

  /** Result of a monthly price-level update. */
  case class Result(inflation: Rate, priceLevel: PriceIndex)

  def update(
      prevInflation: Rate,
      prevPrice: PriceIndex,
      demandMult: Multiplier,
      wageGrowth: Coefficient,
      exRateDeviation: ExchangeRateShock,
  )(using p: SimParams): Result =
    val demandPull: Coefficient    = demandMult.deviationFromOne * DemandPullWeight
    val costPush: Coefficient      = wageGrowth * CostPushWeight
    val rawImportPush: Coefficient =
      exRateDeviation.max(ExchangeRateShock.Zero).toCoefficient * p.forex.importPropensity * ImportPushWeight
    val importPush: Coefficient    = rawImportPush.min(p.openEcon.importPushCap.toCoefficient)

    val rawMonthly: Coefficient = demandPull + costPush + importPush
    val monthly: Coefficient    = softFloor(rawMonthly)
    val annualized: Rate        = monthly.toRate.annualize
    val smoothed: Rate          = prevInflation * (Share.One - SmoothingLambda) + annualized * SmoothingLambda
    val newPrice: PriceIndex    = prevPrice.applyGrowth(monthly).max(MinPriceLevel)
    Result(smoothed, newPrice)

  /** Soft deflation floor: beyond −1.5%/month, only 30% passes through. */
  private def softFloor(raw: Coefficient): Coefficient =
    if raw >= DeflationFloor then raw
    else DeflationFloor + (raw - DeflationFloor) * FloorPassThrough
