package com.boombustgroup.amorfati.engine.markets

import com.boombustgroup.amorfati.agents.Firm
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

import com.boombustgroup.amorfati.random.RandomStream

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

  private val MaxDemandMarkupLift = Coefficient.decimal(10, 2)
  private val MaxCostMarkupLift   = Coefficient.decimal(2, 2)
  private val MaxEnergyMarkupLift = Coefficient.decimal(3, 2)

  /** Per-firm markup update result. */
  case class FirmMarkupResult(
      newMarkup: Multiplier,
      priceChanged: Boolean,
  )

  /** Compute optimal markup for a firm that resets its price.
    *
    * markup = baseMarkup × (1 + demandPressure × sensitivity) where
    * demandPressure = (sectorDemandPressure − 1)
    *
    * Clamped to [minMarkup, maxMarkup] to prevent extreme pricing.
    */
  private[amorfati] def optimalMarkup(
      sectorDemandPressure: Multiplier,
      wageGrowthMonthly: Coefficient,
      energyPressure: Coefficient,
  )(using p: SimParams): Multiplier =
    val demandPressure = ((sectorDemandPressure - Multiplier.One).max(Multiplier.Zero).toScalar * p.pricing.demandSensitivity.toScalar).toCoefficient
    val costPressure   = (wageGrowthMonthly.max(Coefficient.Zero).toScalar * p.pricing.costPassthrough.toScalar).toCoefficient
    val energyMarkup   = energyPressure.max(Coefficient.Zero)
    val boundedDemand  = demandPressure.min(MaxDemandMarkupLift)
    val boundedCost    = costPressure.min(MaxCostMarkupLift)
    val boundedEnergy  = energyMarkup.min(MaxEnergyMarkupLift)
    (p.pricing.baseMarkup * (Coefficient.One + boundedDemand + boundedCost + boundedEnergy).toMultiplier)
      .clamp(p.pricing.minMarkup, p.pricing.maxMarkup)

  /** Commodity-cost pressure translated into markup pressure.
    *
    * Pressure rises with the level of the commodity price index relative to
    * base, sector energy intensity, and the fraction of the shock firms are
    * allowed to pass through to prices.
    */
  private[amorfati] def energyCostPressure(
      commodityPrice: PriceIndex,
      energyCostShare: Share,
      passthrough: Share,
  ): Coefficient =
    commodityPrice.toMultiplier.deviationFromOne.max(Coefficient.Zero) * energyCostShare.toCoefficient * passthrough.toCoefficient

  /** Update a single firm's markup via Calvo lottery.
    *
    * With probability θ, the firm resets to optimal markup. With probability
    * 1−θ, the firm keeps its current markup.
    */
  def updateFirmMarkup(
      currentMarkup: Multiplier,
      sectorDemandPressure: Multiplier,
      wageGrowthMonthly: Coefficient,
      energyPressure: Coefficient,
      rng: RandomStream,
  )(using p: SimParams): FirmMarkupResult =
    if p.pricing.calvoTheta.sampleBelow(rng) then FirmMarkupResult(optimalMarkup(sectorDemandPressure, wageGrowthMonthly, energyPressure), priceChanged = true)
    else FirmMarkupResult(currentMarkup, priceChanged = false)

  /** Compute aggregate inflation adjustment from markup dynamics.
    *
    * Returns monthly inflation contribution = capacity-weighted average markup
    * change across all firms that changed prices this month.
    */
  def aggregateMarkupInflation(
      firms: Vector[Firm.State],
      prevFirms: Vector[Firm.State],
  )(using SimParams): Rate =
    require(
      firms.lengthCompare(prevFirms.length) == 0,
      "firms and prevFirms must have the same length",
    )
    if firms.isEmpty then Rate.Zero
    else
      val totalRevenue = firms.foldLeft(PLN.Zero)((acc, f) => acc + Firm.computeCapacity(f))
      if totalRevenue <= PLN.Zero then Rate.Zero
      else
        val weightedChange = firms
          .zip(prevFirms)
          .foldLeft(PLN.Zero): (acc, pair) =>
            val (curr, prev) = pair
            acc + (Firm.computeCapacity(curr) * (curr.markup - prev.markup))
        weightedChange.ratioTo(totalRevenue).toRate
