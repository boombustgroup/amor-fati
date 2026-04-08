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
  private val RealRateElasticity     = Scalar(0.02)     // demand sensitivity to real interest rate gap
  private val PressureMaxBoost       = Multiplier(0.75) // hiring signal can rise above 1.0, but only moderately
  private val PressureSaturationRate = Scalar(1.25)     // how quickly excess-demand pressure saturates above capacity
  private val HiringSignalSmoothing  = Share(0.65)      // persistence in sector hiring plans; avoids month-to-month whipsaw

  case class Input(
      w: World,                   // current world state
      employed: Int,              // employment count
      living: Vector[Firm.State], // living firm population
      domesticCons: PLN,          // domestic component of household consumption
  )

  case class Output(
      govPurchases: PLN,                        // total government purchases this month
      sectorMults: Vector[Multiplier],          // per-sector demand multiplier (0 = no demand, 1 = full capacity)
      sectorDemandPressure: Vector[Multiplier], // uncapped demand/capacity ratios used for hiring pressure
      sectorHiringSignal: Vector[Multiplier],   // smoothed sector hiring signal used by firm labor planning
      avgDemandMult: Multiplier,                // economy-wide average demand multiplier
      sectorCapReal: Vector[PLN],               // per-sector real production capacity before price-level repricing
      laggedInvestDemand: PLN,                  // lagged investment demand for deposit flow calculation
      fiscalRuleStatus: FiscalRules.RuleStatus, // fiscal rule compliance diagnostics
  )

  def compute(in: Input)(using p: SimParams): Output =
    val rawGovPurchases    = computeGovPurchases(in)
    val fiscalResult       = applyFiscalRules(in, rawGovPurchases)
    val govPurchases       = fiscalResult.constrainedGovPurchases
    val sectorCapReal      = computeSectorCapacity(in)
    val sectorExports      = computeSectorExports(in)
    val laggedInvestDemand = computeLaggedInvestDemand(in)
    val sectorDemand       = computeSectorDemand(in, govPurchases, sectorExports, laggedInvestDemand)
    val rawPressure        = computeRawDemandPressure(sectorDemand, sectorCapReal, in.w.priceLevel)
    val sectorPressure     = stabilizeDemandPressure(rawPressure)
    val sectorHiringSignal = smoothHiringSignal(in.w.pipeline.sectorHiringSignal, sectorPressure)
    val sectorMults        = applySpillover(rawPressure, sectorCapReal, in.w.priceLevel)
    val avgDemandMult      = computeAvgDemandMult(sectorMults, sectorCapReal, in)
    Output(govPurchases, sectorMults, sectorPressure, sectorHiringSignal, avgDemandMult, sectorCapReal, laggedInvestDemand, fiscalResult.status)

  /** Government purchases: base spending x price level plus a small automatic
    * stabilizer tied to labor-market slack. Revenue windfalls are not
    * mechanically recycled back into demand.
    */
  private def computeGovPurchases(in: Input)(using p: SimParams): PLN =
    val unempRate = Share.One - Share.fraction(in.employed, in.w.derivedTotalPopulation)
    val unempGap  = (unempRate - p.monetary.nairu).max(Share.Zero)
    val stimulus  = p.fiscal.govBaseSpending * unempGap * p.fiscal.govAutoStabMult
    val target    = p.fiscal.govBaseSpending * in.w.priceLevel.toMultiplier.max(Multiplier.One) + stimulus
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

  /** Per-sector real production capacity before repricing by CPI. */
  private def computeSectorCapacity(in: Input)(using p: SimParams): Vector[PLN] =
    val caps = Array.fill(p.sectorDefs.length)(PLN.Zero)
    in.living.foreach: f =>
      val s = f.sector.toInt
      caps(s) = caps(s) + Firm.computeCapacity(f)
    caps.toVector

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
  private def computeSectorDemand(
      in: Input,
      govPurchases: PLN,
      sectorExports: Vector[PLN],
      laggedInvestDemand: PLN,
  )(using p: SimParams): Vector[PLN] =
    (0 until p.sectorDefs.length)
      .map: s =>
        p.fiscal.fofConsWeights(s) * in.domesticCons +
          p.fiscal.fofGovWeights(s) * govPurchases +
          p.fiscal.fofInvestWeights(s) * laggedInvestDemand +
          sectorExports(s)
      .toVector

  /** Redistribute excess demand from capacity-constrained sectors to sectors
    * with slack. Sectors above capacity are capped at 1.0; their excess flows
    * proportionally into below-capacity sectors.
    */
  private def computeRawDemandPressure(
      sectorDemand: Vector[PLN],
      sectorCapReal: Vector[PLN],
      priceLevel: PriceIndex,
  ): Vector[Multiplier] =
    sectorDemand.indices
      .map: s =>
        val nominalCap = sectorCapReal(s) * priceLevel.toMultiplier
        if nominalCap > PLN.Zero then sectorDemand(s).ratioTo(nominalCap).toMultiplier else Multiplier.Zero
      .toVector

  private def stabilizeDemandPressure(rawPressure: Vector[Multiplier]): Vector[Multiplier] =
    rawPressure.map(stabilizedPressure)

  private def smoothHiringSignal(prevSignal: Vector[Multiplier], currentSignal: Vector[Multiplier]): Vector[Multiplier] =
    currentSignal.indices
      .map: i =>
        val prev = prevSignal.lift(i).getOrElse(Multiplier.One)
        prev * HiringSignalSmoothing + currentSignal(i) * (Share.One - HiringSignalSmoothing)
      .toVector

  private def stabilizedPressure(raw: Multiplier): Multiplier =
    if raw <= Multiplier.One then raw
    else
      val excess = raw.deviationFromOne.max(Coefficient.Zero)
      Multiplier.One + (PressureMaxBoost * (Multiplier.One - (-(excess.toScalar * PressureSaturationRate)).toCoefficient.exp))

  private def applySpillover(
      rawMults: Vector[Multiplier],
      sectorCapReal: Vector[PLN],
      priceLevel: PriceIndex,
  ): Vector[Multiplier] =
    val nominalCapBySector = sectorCapReal.map(_ * priceLevel.toMultiplier)
    val excessDemand       = rawMults.indices
      .map: s =>
        if rawMults(s) > Multiplier.One then nominalCapBySector(s) * rawMults(s).deviationFromOne else PLN.Zero
      .foldLeft(PLN.Zero)(_ + _)
    val deficitCapacity    = rawMults.indices
      .map: s =>
        if rawMults(s) < Multiplier.One then nominalCapBySector(s) * (Multiplier.One - rawMults(s)).toCoefficient else PLN.Zero
      .foldLeft(PLN.Zero)(_ + _)
    val spilloverFrac      =
      if deficitCapacity > PLN.Zero then excessDemand.ratioTo(deficitCapacity).toShare.clamp(Share.Zero, Share.One)
      else Share.Zero
    rawMults.indices
      .map: s =>
        if rawMults(s) > Multiplier.One then Multiplier.One
        else rawMults(s) + spilloverFrac * (Multiplier.One - rawMults(s))
      .toVector

  /** Economy-wide average demand multiplier, adjusted for real rate effect when
    * expectations mechanism is active.
    *
    * Uses post-spillover sector multipliers (capped at 1.0 per sector) weighted
    * by sector capacity — consistent with the demand firms actually see.
    */
  private def computeAvgDemandMult(
      sectorMults: Vector[Multiplier],
      sectorCapReal: Vector[PLN],
      in: Input,
  ): Multiplier =
    val totalCapacity = sectorCapReal.foldLeft(PLN.Zero)(_ + _)
    val baseMult      =
      if totalCapacity > PLN.Zero then
        sectorMults.indices.foldLeft(PLN.Zero): (acc, s) =>
          acc + (sectorCapReal(s) * sectorMults(s))
      else PLN.Zero
    val weightedBase  =
      if totalCapacity > PLN.Zero then baseMult.ratioTo(totalCapacity).toMultiplier
      else Multiplier.One
    val realRateAdj   = ((in.w.nbp.referenceRate - in.w.mechanisms.expectations.expectedInflation).toScalar * RealRateElasticity).toCoefficient
    (Coefficient.One + weightedBase.deviationFromOne - realRateAdj).toMultiplier
