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
      newMarkup: Multiplier, // updated markup (unchanged if not selected by Calvo lottery)
      priceChanged: Boolean,
  )

  /** Compute optimal markup for a firm that resets its price.
    *
    * markup = baseMarkup × (1 + demandPressure × sensitivity) where
    * demandPressure = (sectorDemandMult − 1)
    *
    * Clamped to [minMarkup, maxMarkup] to prevent extreme pricing.
    */
  private[amorfati] def optimalMarkup(
      sectorDemandMult: Double,
      wageGrowthMonthly: Double,
  )(using p: SimParams): Multiplier =
    val demandPressure = sectorDemandMult - 1.0
    val costPressure   = wageGrowthMonthly * p.pricing.costPassthrough.toDouble
    val raw            = p.pricing.baseMarkup.toDouble * (1.0 + demandPressure * p.pricing.demandSensitivity.toDouble + costPressure)
    Multiplier(raw.max(p.pricing.minMarkup.toDouble).min(p.pricing.maxMarkup.toDouble))

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
    if rng.nextDouble() < p.pricing.calvoTheta.toDouble then FirmMarkupResult(optimalMarkup(sectorDemandMult, wageGrowthMonthly), priceChanged = true)
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
      val totalRevenue = firms.kahanSumBy(f => Firm.computeCapacity(f).toDouble)
      if totalRevenue <= 0 then 0.0
      else
        val weightedChange = firms
          .zip(prevFirms)
          .kahanSumBy: (curr, prev) =>
            Firm.computeCapacity(curr).toDouble * (curr.markup.toDouble - prev.markup.toDouble)
        weightedChange / totalRevenue
