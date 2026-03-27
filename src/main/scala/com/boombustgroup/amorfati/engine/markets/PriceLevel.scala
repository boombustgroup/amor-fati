package com.boombustgroup.amorfati.engine.markets

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

/** Aggregate price level: Phillips-curve-style monthly inflation update.
  *
  * Four channels: demand-pull (output gap proxy via demandMult), cost-push
  * (wage growth pass-through), import push (exchange rate deviation × import
  * propensity), and tech deflation (automation + hybrid digitalization).
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
  private val DemandPullWeight = 0.15   // sensitivity of inflation to demand gap
  private val CostPushWeight   = 0.25   // wage growth pass-through to prices
  private val ImportPushWeight = 0.25   // FX depreciation pass-through
  private val AutoDeflation    = 0.060  // monthly tech deflation per unit automation ratio
  private val HybridDeflation  = 0.018  // monthly tech deflation per unit hybrid ratio
  private val DeflationFloor   = -0.015 // soft floor: −1.5%/month
  private val FloorPassThrough = 0.3    // beyond floor, 30% pass-through
  private val SmoothingLambda  = 0.3    // EWM weight on new observation
  private val MinPriceLevel    = 0.30   // absolute floor on price index

  /** Result of a monthly price-level update. */
  case class Result(inflation: Rate, priceLevel: Double)

  @boundaryEscape
  def update(
      prevInflation: Rate,
      prevPrice: Double,
      demandMult: Double,
      wageGrowth: Double,
      exRateDeviation: Double,
      autoRatio: Double,
      hybridRatio: Double,
  )(using p: SimParams): Result =
    import ComputationBoundary.toDouble
    val demandPull    = (demandMult - 1.0) * DemandPullWeight
    val costPush      = wageGrowth * CostPushWeight
    val rawImportPush = Math.max(0.0, exRateDeviation) * toDouble(p.forex.importPropensity) * ImportPushWeight
    val importPush    = Math.min(rawImportPush, toDouble(p.openEcon.importPushCap))
    val techDeflation = autoRatio * AutoDeflation + hybridRatio * HybridDeflation

    val rawMonthly = demandPull + costPush + importPush - techDeflation
    val monthly    = softFloor(rawMonthly)
    val annualized = monthly * 12.0
    val smoothed   = toDouble(prevInflation) * (1.0 - SmoothingLambda) + annualized * SmoothingLambda
    val newPrice   = Math.max(MinPriceLevel, prevPrice * (1.0 + monthly))
    Result(Rate(smoothed), newPrice)

  /** Soft deflation floor: beyond −1.5%/month, only 30% passes through. */
  private def softFloor(raw: Double): Double =
    if raw >= DeflationFloor then raw
    else DeflationFloor + (raw - DeflationFloor) * FloorPassThrough
