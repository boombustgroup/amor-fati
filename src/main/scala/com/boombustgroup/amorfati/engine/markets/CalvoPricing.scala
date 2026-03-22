package com.boombustgroup.amorfati.engine.markets

import com.boombustgroup.amorfati.agents.Firm
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

import scala.util.Random

/** Calvo staggered pricing: micro-founded price stickiness.
  *
  * Each firm has a markup over marginal cost. Each month, a fraction `θ` (Calvo
  * probability) of firms can reset their markup; the rest keep it unchanged.
  * This generates:
  *
  *   - '''Price stickiness''' — not all firms react to shocks simultaneously.
  *     ~15% reset per month (θ = 0.15) → average price duration ~6.7 months
  *     (Alvarez et al. 2006, Bils & Klenow 2004 for European calibration).
  *   - '''Endogenous markup''' — firms that reset choose markup based on
  *     demand/capacity ratio (high demand → higher markup) and cost pressure
  *     (wage growth → pass-through to price). Strategic complementarity: markup =
  *     blend of own optimal and sector average.
  *   - '''Heterogeneous inflation''' — aggregate inflation emerges from the
  *     distribution of individual firm price changes, not from a single
  *     Phillips curve equation.
  *
  * This module computes per-firm markup updates and returns the aggregate
  * inflation contribution from markup dynamics. The existing PriceLevel module
  * continues to handle demand-pull / cost-push fundamentals — CalvoPricing adds
  * the micro-founded sticky adjustment.
  *
  * Calibration: Alvarez et al. 2006 (EU price duration), NBP inflation reports,
  * GUS producer price survey.
  */
object CalvoPricing:

  /** Per-firm markup update result. */
  case class FirmMarkupResult(
      newMarkup: Multiplier,
      priceChanged: Boolean,
  )

  /** Compute optimal markup for a firm that resets its price.
    *
    * markup = baseMarkup × (1 + demandPressure × sensitivity) where
    * demandPressure = (sectorDemandMult − 1)
    *
    * Clamped to [minMarkup, maxMarkup] to prevent extreme pricing.
    */
  @computationBoundary
  private[amorfati] def optimalMarkup(
      sectorDemandMult: Double,
      wageGrowthMonthly: Double,
  )(using p: SimParams): Multiplier =
    import ComputationBoundary.toDouble
    val demandPressure = sectorDemandMult - 1.0
    val costPressure   = wageGrowthMonthly * toDouble(p.pricing.costPassthrough)
    val raw            = toDouble(p.pricing.baseMarkup) * (1.0 + demandPressure * toDouble(p.pricing.demandSensitivity) + costPressure)
    Multiplier(raw.max(toDouble(p.pricing.minMarkup)).min(toDouble(p.pricing.maxMarkup)))

  /** Update a single firm's markup via Calvo lottery.
    *
    * With probability θ, the firm resets to optimal markup. With probability
    * 1−θ, the firm keeps its current markup.
    */
  def updateFirmMarkup(
      currentMarkup: Multiplier,
      sectorDemandMult: Double,
      wageGrowthMonthly: Double,
      rng: Random,
  )(using p: SimParams): FirmMarkupResult =
    if p.pricing.calvoTheta.sampleBelow(rng) then FirmMarkupResult(optimalMarkup(sectorDemandMult, wageGrowthMonthly), priceChanged = true)
    else FirmMarkupResult(currentMarkup, priceChanged = false)

  /** Compute aggregate inflation adjustment from markup dynamics.
    *
    * Returns monthly inflation contribution = revenue-weighted average markup
    * change across all firms that changed prices this month.
    */
  def aggregateMarkupInflation(
      firms: Vector[Firm.State],
      prevFirms: Vector[Firm.State],
  )(using SimParams): Double =
    if firms.isEmpty then 0.0
    else
      val totalRevenue = PLN.fromRaw(firms.map(f => Firm.computeCapacity(f).toLong).sum)
      if totalRevenue <= PLN.Zero then 0.0
      else
        val weightedChange = PLN.fromRaw(
          firms
            .zip(prevFirms)
            .map: (curr, prev) =>
              (Firm.computeCapacity(curr) * (curr.markup - prev.markup)).toLong
            .sum,
        )
        weightedChange / totalRevenue
