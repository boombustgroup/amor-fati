package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.engine.markets.FiscalRules
import com.boombustgroup.amorfati.types.*

/** Pure economic logic for aggregate demand formation — no state mutation, no
  * flows.
  *
  * Allocates household consumption, government purchases, investment, and
  * export demand across sectors via flow-of-funds weights. Excess demand in
  * capacity-constrained sectors spills over to sectors with slack.
  *
  * Extracted from DemandStep (Calculus vs Accounting split).
  */
object DemandEconomics:

  // ---- Calibration constants ----
  private val RealRateElasticity     = 0.02 // demand sensitivity to real interest rate gap
  private val PressureMaxBoost       = 0.75 // hiring signal can rise above 1.0, but only moderately
  private val PressureSaturationRate = 1.25 // how quickly excess-demand pressure saturates above capacity
  private val HiringSignalSmoothing  = 0.65 // persistence in sector hiring plans; avoids month-to-month whipsaw

  case class Input(
      w: World,                   // current world state
      employed: Int,              // employment count
      living: Vector[Firm.State], // living firm population
      domesticCons: PLN,          // domestic component of household consumption
  )

  case class Output(
      govPurchases: PLN,                       // total government purchases this month
      sectorMults: Vector[Double],             // per-sector demand multiplier (0 = no demand, 1 = full capacity)
      sectorDemandPressure: Vector[Double],    // uncapped demand/capacity ratios used for hiring pressure
      sectorHiringSignal: Vector[Double],      // smoothed sector hiring signal used by firm labor planning
      avgDemandMult: Double,                   // economy-wide average demand multiplier
      sectorCap: Vector[Double],               // per-sector nominal production capacity
      laggedInvestDemand: PLN,                 // lagged investment demand for deposit flow calculation
      fiscalRuleStatus: FiscalRules.RuleStatus, // fiscal rule compliance diagnostics
  )

  def compute(in: Input)(using p: SimParams): Output =
    val rawGovPurchases    = computeGovPurchases(in)
    val fiscalResult       = applyFiscalRules(in, rawGovPurchases)
    val govPurchases       = fiscalResult.constrainedGovPurchases
    val sectorCap          = computeSectorCapacity(in)
    val sectorExports      = computeSectorExports(in)
    val laggedInvestDemand = computeLaggedInvestDemand(in)
    val sectorDemand       = computeSectorDemand(in, govPurchases, sectorExports, laggedInvestDemand)
    val rawPressure        = computeRawDemandPressure(sectorDemand, sectorCap, in.w.priceLevel)
    val sectorPressure     = stabilizeDemandPressure(rawPressure)
    val sectorHiringSignal = smoothHiringSignal(in.w.pipeline.sectorHiringSignal, sectorPressure)
    val sectorMults        = applySpillover(rawPressure, sectorCap, in.w.priceLevel)
    val avgDemandMult      = computeAvgDemandMult(sectorMults, sectorCap, in)
    Output(govPurchases, sectorMults, sectorPressure, sectorHiringSignal, avgDemandMult, sectorCap, laggedInvestDemand, fiscalResult.status)

  /** Government purchases: base spending x price level plus a small automatic
    * stabilizer tied to labor-market slack. Revenue windfalls are not
    * mechanically recycled back into demand.
    */
  private def computeGovPurchases(in: Input)(using p: SimParams): PLN =
    val unempRate = Share.One - Share.fraction(in.employed, in.w.derivedTotalPopulation)
    val unempGap  = (unempRate - p.monetary.nairu).max(Share.Zero)
    val stimulus  = p.fiscal.govBaseSpending * unempGap * p.fiscal.govAutoStabMult
    val target    = p.fiscal.govBaseSpending * Multiplier(Math.max(1.0, in.w.priceLevel)) + stimulus
    target

  /** Apply fiscal rules to raw government purchases.
    *
    * `prevGovSpend` intentionally tracks only `GovState.domesticBudgetDemand`.
    * This excludes the separate domestic co-financing outlay and the total EU
    * project envelope, which are reported separately on `GovState`.
    */
  private def applyFiscalRules(in: Input, rawTarget: PLN)(using p: SimParams): FiscalRules.Output =
    val prevGovSpend = in.w.gov.domesticBudgetDemand
    val unempRate    = Share.One - Share.fraction(in.employed, in.w.derivedTotalPopulation)
    val outputGap    = Coefficient((unempRate - p.monetary.nairu) / p.monetary.nairu)

    FiscalRules.constrain(
      FiscalRules.Input(
        rawGovPurchases = rawTarget,
        prevGovSpend = prevGovSpend,
        cumulativeDebt = in.w.gov.cumulativeDebt,
        monthlyGdp = in.w.cachedMonthlyGdpProxy,
        prevRevenue = in.w.gov.taxRevenue,
        prevDeficit = in.w.gov.deficit,
        inflation = in.w.inflation,
        outputGap = outputGap,
      ),
    )

  /** Per-sector nominal production capacity: sum of firm capacities. */
  @boundaryEscape
  private def computeSectorCapacity(in: Input)(using p: SimParams): Vector[Double] =
    import ComputationBoundary.toDouble
    (0 until p.sectorDefs.length)
      .map: s =>
        in.living.filter(_.sector.toInt == s).map(f => toDouble(Firm.computeCapacity(f))).sum
      .toVector

  /** Per-sector export demand: from GVC foreign firms when enabled, otherwise
    * from lagged aggregate exports split by fixed shares. Falls back to
    * aggregate split when GVC sector exports are zero (init month).
    */
  private def computeSectorExports(in: Input)(using p: SimParams): Vector[PLN] =
    val gvcExports = in.w.external.gvc.sectorExports
    if gvcExports.exists(_ > PLN.Zero) then gvcExports
    else p.fiscal.fofExportShares.map(_ * in.w.forex.exports)

  /** Lagged domestic investment demand (net of import content). */
  private def computeLaggedInvestDemand(in: Input)(using p: SimParams): PLN =
    in.w.real.grossInvestment * (Share.One - p.capital.importShare) +
      in.w.real.aggGreenInvestment * (Share.One - p.climate.greenImportShare)

  /** Per-sector total demand: consumption + gov purchases + investment +
    * exports, allocated via flow-of-funds weights.
    */
  @boundaryEscape
  private def computeSectorDemand(
      in: Input,
      govPurchases: PLN,
      sectorExports: Vector[PLN],
      laggedInvestDemand: PLN,
  )(using p: SimParams): Vector[Double] =
    import ComputationBoundary.toDouble
    (0 until p.sectorDefs.length)
      .map: s =>
        toDouble(
          p.fiscal.fofConsWeights(s) * in.domesticCons +
            p.fiscal.fofGovWeights(s) * govPurchases +
            p.fiscal.fofInvestWeights(s) * laggedInvestDemand +
            sectorExports(s),
        )
      .toVector

  /** Redistribute excess demand from capacity-constrained sectors to sectors
    * with slack. Sectors above capacity are capped at 1.0; their excess flows
    * proportionally into below-capacity sectors.
    */
  private def computeRawDemandPressure(
      sectorDemand: Vector[Double],
      sectorCap: Vector[Double],
      priceLevel: Double,
  ): Vector[Double] =
    sectorDemand.indices
      .map: s =>
        if sectorCap(s) > 0 then sectorDemand(s) / (sectorCap(s) * priceLevel) else 0.0
      .toVector

  private def stabilizeDemandPressure(rawPressure: Vector[Double]): Vector[Double] =
    rawPressure.map(stabilizedPressure)

  private def smoothHiringSignal(prevSignal: Vector[Double], currentSignal: Vector[Double]): Vector[Double] =
    currentSignal.indices
      .map: i =>
        val prev = prevSignal.lift(i).getOrElse(1.0)
        prev * HiringSignalSmoothing + currentSignal(i) * (1.0 - HiringSignalSmoothing)
      .toVector

  private def stabilizedPressure(raw: Double): Double =
    if raw <= 1.0 then raw
    else 1.0 + PressureMaxBoost * (1.0 - Math.exp(-PressureSaturationRate * (raw - 1.0)))

  private def applySpillover(
      rawMults: Vector[Double],
      sectorCap: Vector[Double],
      priceLevel: Double,
  ): Vector[Double] =
    val excessDemand    = rawMults.indices
      .map: s =>
        if rawMults(s) > 1.0 then (rawMults(s) - 1.0) * sectorCap(s) * priceLevel else 0.0
      .sum
    val deficitCapacity = rawMults.indices
      .map: s =>
        if rawMults(s) < 1.0 then (1.0 - rawMults(s)) * sectorCap(s) * priceLevel else 0.0
      .sum
    val spilloverFrac   = if deficitCapacity > 0 then Math.min(1.0, excessDemand / deficitCapacity) else 0.0
    rawMults.indices
      .map: s =>
        if rawMults(s) > 1.0 then 1.0
        else rawMults(s) + spilloverFrac * (1.0 - rawMults(s))
      .toVector

  /** Economy-wide average demand multiplier, adjusted for real rate effect when
    * expectations mechanism is active.
    *
    * Uses post-spillover sector multipliers (capped at 1.0 per sector) weighted
    * by sector capacity — consistent with the demand firms actually see.
    */
  @boundaryEscape
  private def computeAvgDemandMult(
      sectorMults: Vector[Double],
      sectorCap: Vector[Double],
      in: Input,
  ): Double =
    import ComputationBoundary.toDouble
    val totalCapacity = sectorCap.sum
    val baseMult      =
      if totalCapacity > 0 then sectorMults.indices.map(s => sectorMults(s) * sectorCap(s)).sum / totalCapacity
      else 1.0
    val realRate      = toDouble(in.w.nbp.referenceRate - in.w.mechanisms.expectations.expectedInflation)
    baseMult + (-realRate * RealRateElasticity)
