package com.boombustgroup.amorfati.engine.steps

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.amorfati.util.KahanSum.*

/** Aggregate demand formation: allocates household consumption, government
  * purchases, investment, and export demand across sectors via flow-of-funds
  * weights. Excess demand in capacity-constrained sectors spills over to
  * sectors with slack.
  */
object DemandStep:

  // ---- Calibration constants ----
  private val GovSpendingFloor   = 0.98 // gov spending cannot drop below 98% of previous period
  private val RealRateElasticity = 0.02 // demand sensitivity to real interest rate gap

  case class Input(
      w: World,                         // current world state
      s2: LaborDemographicsStep.Output, // labor/demographics (employment, living firms)
      s3: HouseholdIncomeStep.Output,   // household income (domestic consumption)
  )

  case class Output(
      govPurchases: PLN,           // total government purchases this month
      sectorMults: Vector[Double], // per-sector demand multiplier (0 = no demand, 1 = full capacity)
      avgDemandMult: Double,       // economy-wide average demand multiplier
      sectorCap: Vector[Double],   // per-sector nominal production capacity
      laggedInvestDemand: PLN,     // lagged investment demand for deposit flow calculation
  )

  def run(in: Input)(using p: SimParams): Output =
    val govPurchases       = computeGovPurchases(in)
    val sectorCap          = computeSectorCapacity(in)
    val sectorExports      = computeSectorExports(in)
    val laggedInvestDemand = computeLaggedInvestDemand(in)
    val sectorDemand       = computeSectorDemand(in, govPurchases, sectorExports, laggedInvestDemand)
    val sectorMults        = applySpillover(sectorDemand, sectorCap, in.w.priceLevel)
    val avgDemandMult      = computeAvgDemandMult(sectorDemand, sectorCap, in)
    Output(PLN(govPurchases), sectorMults, avgDemandMult, sectorCap, PLN(laggedInvestDemand))

  /** Government purchases: base spending × price level + fiscal recycling (tax
    * revenue + ZUS surplus) + automatic fiscal stimulus (unemployment gap ×
    * multiplier). Floored at 98% of previous period.
    */
  private def computeGovPurchases(in: Input)(using p: SimParams): Double =
    val zusNetSurplus =
      if p.flags.zus then Math.max(0.0, in.w.social.zus.contributions.toDouble - in.w.social.zus.pensionPayments.toDouble)
      else 0.0
    val unempRate     = 1.0 - in.s2.employed.toDouble / in.w.totalPopulation
    val unempGap      = Math.max(0.0, unempRate - p.monetary.nairu.toDouble)
    val stimulus      = p.fiscal.govBaseSpending.toDouble * unempGap * p.fiscal.govAutoStabMult
    val target        = p.fiscal.govBaseSpending.toDouble * Math.max(1.0, in.w.priceLevel) +
      p.fiscal.govFiscalRecyclingRate.toDouble * (in.w.gov.taxRevenue.toDouble + zusNetSurplus) + stimulus
    val prevGovSpend  = in.w.gov.govCurrentSpend.toDouble + in.w.gov.govCapitalSpend.toDouble
    if prevGovSpend > 0 then Math.max(target, prevGovSpend * GovSpendingFloor)
    else target

  /** Per-sector nominal production capacity: sum of firm capacities. */
  private def computeSectorCapacity(in: Input)(using p: SimParams): Vector[Double] =
    (0 until p.sectorDefs.length)
      .map: s =>
        in.s2.living.filter(_.sector.toInt == s).kahanSumBy(f => Firm.computeCapacity(f).toDouble)
      .toVector

  /** Per-sector export demand: from GVC foreign firms when enabled, otherwise
    * from lagged aggregate exports split by fixed shares.
    */
  private def computeSectorExports(in: Input)(using p: SimParams): Vector[Double] =
    if p.flags.gvc && p.flags.openEcon then in.w.external.gvc.sectorExports.map(_.toDouble)
    else p.fiscal.fofExportShares.map(_.toDouble).map(_ * in.w.forex.exports.toDouble)

  /** Lagged domestic investment demand (net of import content). */
  private def computeLaggedInvestDemand(in: Input)(using p: SimParams): Double =
    in.w.real.grossInvestment.toDouble * (1.0 - p.capital.importShare.toDouble) +
      in.w.real.aggGreenInvestment.toDouble * (1.0 - p.climate.greenImportShare.toDouble)

  /** Per-sector total demand: consumption + gov purchases + investment +
    * exports, allocated via flow-of-funds weights.
    */
  private def computeSectorDemand(
      in: Input,
      govPurchases: Double,
      sectorExports: Vector[Double],
      laggedInvestDemand: Double,
  )(using p: SimParams): Vector[Double] =
    (0 until p.sectorDefs.length)
      .map: s =>
        p.fiscal.fofConsWeights(s).toDouble * in.s3.domesticCons.toDouble +
          p.fiscal.fofGovWeights(s).toDouble * govPurchases +
          p.fiscal.fofInvestWeights(s).toDouble * laggedInvestDemand +
          sectorExports(s)
      .toVector

  /** Redistribute excess demand from capacity-constrained sectors to sectors
    * with slack. Sectors above capacity are capped at 1.0; their excess flows
    * proportionally into below-capacity sectors.
    */
  private def applySpillover(
      sectorDemand: Vector[Double],
      sectorCap: Vector[Double],
      priceLevel: Double,
  ): Vector[Double] =
    val rawMults        = sectorDemand.indices
      .map: s =>
        if sectorCap(s) > 0 then sectorDemand(s) / (sectorCap(s) * priceLevel) else 0.0
      .toVector
    val excessDemand    = rawMults.indices
      .map: s =>
        if rawMults(s) > 1.0 then (rawMults(s) - 1.0) * sectorCap(s) * priceLevel else 0.0
      .kahanSum
    val deficitCapacity = rawMults.indices
      .map: s =>
        if rawMults(s) < 1.0 then (1.0 - rawMults(s)) * sectorCap(s) * priceLevel else 0.0
      .kahanSum
    val spilloverFrac   = if deficitCapacity > 0 then Math.min(1.0, excessDemand / deficitCapacity) else 0.0
    rawMults.indices
      .map: s =>
        if rawMults(s) > 1.0 then 1.0
        else rawMults(s) + spilloverFrac * (1.0 - rawMults(s))
      .toVector

  /** Economy-wide average demand multiplier, adjusted for real rate effect when
    * expectations mechanism is active.
    */
  private def computeAvgDemandMult(
      sectorDemand: Vector[Double],
      sectorCap: Vector[Double],
      in: Input,
  )(using p: SimParams): Double =
    val totalDemand   = sectorDemand.kahanSum
    val totalCapacity = sectorCap.kahanSum
    val baseMult      = if totalCapacity > 0 then totalDemand / (totalCapacity * in.w.priceLevel) else 1.0
    val realRateAdj   =
      if p.flags.expectations then
        val realRate = in.w.nbp.referenceRate.toDouble - in.w.mechanisms.expectations.expectedInflation.toDouble
        -realRate * RealRateElasticity
      else 0.0
    baseMult + realRateAdj
