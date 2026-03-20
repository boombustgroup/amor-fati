package com.boombustgroup.amorfati.engine.steps

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.engine.markets.FiscalRules
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
      govPurchases: PLN,                       // total government purchases this month
      sectorMults: Vector[Double],             // per-sector demand multiplier (0 = no demand, 1 = full capacity)
      avgDemandMult: Double,                   // economy-wide average demand multiplier
      sectorCap: Vector[Double],               // per-sector nominal production capacity
      laggedInvestDemand: PLN,                 // lagged investment demand for deposit flow calculation
      fiscalRuleStatus: FiscalRules.RuleStatus, // fiscal rule compliance diagnostics
  )

  def run(in: Input)(using p: SimParams): Output =
    val rawGovPurchases    = computeGovPurchases(in)
    val fiscalResult       = applyFiscalRules(in, rawGovPurchases)
    val govPurchases       = fiscalResult.constrainedGovPurchases
    val sectorCap          = computeSectorCapacity(in)
    val sectorExports      = computeSectorExports(in)
    val laggedInvestDemand = computeLaggedInvestDemand(in)
    val sectorDemand       = computeSectorDemand(in, govPurchases, sectorExports, laggedInvestDemand)
    val sectorMults        = applySpillover(sectorDemand, sectorCap, in.w.priceLevel)
    val avgDemandMult      = computeAvgDemandMult(sectorMults, sectorCap, in)
    Output(govPurchases, sectorMults, avgDemandMult, sectorCap, laggedInvestDemand, fiscalResult.status)

  /** Government purchases: base spending × price level + fiscal recycling (tax
    * revenue + ZUS surplus) + automatic fiscal stimulus (unemployment gap ×
    * multiplier). Floored at 98% of previous period.
    */
  private def computeGovPurchases(in: Input)(using p: SimParams): PLN =
    val zusNetSurplus =
      if p.flags.zus then (in.w.social.zus.contributions - in.w.social.zus.pensionPayments).max(PLN.Zero)
      else PLN.Zero
    val unempRate     = Ratio(1.0 - in.s2.employed.toDouble / in.w.totalPopulation)
    val unempGap      = (unempRate - p.monetary.nairu).max(Ratio.Zero)
    val stimulus      = p.fiscal.govBaseSpending * unempGap * p.fiscal.govAutoStabMult
    val target        = p.fiscal.govBaseSpending * Math.max(1.0, in.w.priceLevel) +
      (in.w.gov.taxRevenue + zusNetSurplus) * p.fiscal.govFiscalRecyclingRate + stimulus
    target

  /** Apply fiscal rules to raw government purchases. The 98% floor is applied
    * only when no Art. 216/86 rule is binding.
    */
  private def applyFiscalRules(in: Input, rawTarget: PLN)(using p: SimParams): FiscalRules.Output =
    val prevGovSpend = in.w.gov.govCurrentSpend + in.w.gov.govCapitalSpend
    val unempRate    = Ratio.One - Ratio.fraction(in.s2.employed, in.w.totalPopulation)
    val outputGap    = Ratio((unempRate - p.monetary.nairu) / p.monetary.nairu)

    val floored =
      if prevGovSpend > PLN.Zero then rawTarget.max(prevGovSpend * GovSpendingFloor)
      else rawTarget

    val result = FiscalRules.constrain(
      FiscalRules.Input(
        rawGovPurchases = floored,
        prevGovSpend = prevGovSpend,
        cumulativeDebt = in.w.gov.cumulativeDebt,
        monthlyGdp = PLN(in.w.gdpProxy),
        prevRevenue = in.w.gov.taxRevenue,
        prevDeficit = in.w.gov.deficit,
        inflation = in.w.inflation,
        outputGap = outputGap,
      ),
    )

    // When Art. 216/86 is binding, override the 98% floor — fiscal rules take precedence
    if result.status.bindingRule >= 3 then result
    else
      val withFloor = result.constrainedGovPurchases.max(
        if prevGovSpend > PLN.Zero then prevGovSpend * GovSpendingFloor else PLN.Zero,
      )
      result.copy(constrainedGovPurchases = withFloor)

  /** Per-sector nominal production capacity: sum of firm capacities. */
  private def computeSectorCapacity(in: Input)(using p: SimParams): Vector[Double] =
    (0 until p.sectorDefs.length)
      .map: s =>
        in.s2.living.filter(_.sector.toInt == s).kahanSumBy(f => Firm.computeCapacity(f).toDouble)
      .toVector

  /** Per-sector export demand: from GVC foreign firms when enabled, otherwise
    * from lagged aggregate exports split by fixed shares. Falls back to
    * aggregate split when GVC sector exports are zero (init month).
    */
  private def computeSectorExports(in: Input)(using p: SimParams): Vector[PLN] =
    if p.flags.gvc && p.flags.openEcon then
      val gvcExports = in.w.external.gvc.sectorExports
      if gvcExports.exists(_ > PLN.Zero) then gvcExports
      else p.fiscal.fofExportShares.map(_ * in.w.forex.exports)
    else p.fiscal.fofExportShares.map(_ * in.w.forex.exports)

  /** Lagged domestic investment demand (net of import content). */
  private def computeLaggedInvestDemand(in: Input)(using p: SimParams): PLN =
    in.w.real.grossInvestment * (1.0 - p.capital.importShare.toDouble) +
      in.w.real.aggGreenInvestment * (1.0 - p.climate.greenImportShare.toDouble)

  /** Per-sector total demand: consumption + gov purchases + investment +
    * exports, allocated via flow-of-funds weights.
    */
  private def computeSectorDemand(
      in: Input,
      govPurchases: PLN,
      sectorExports: Vector[PLN],
      laggedInvestDemand: PLN,
  )(using p: SimParams): Vector[Double] =
    (0 until p.sectorDefs.length)
      .map: s =>
        (p.fiscal.fofConsWeights(s) * in.s3.domesticCons +
          p.fiscal.fofGovWeights(s) * govPurchases +
          p.fiscal.fofInvestWeights(s) * laggedInvestDemand +
          sectorExports(s)).toDouble
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
    val spilloverFrac   = if deficitCapacity > 0 then Ratio(excessDemand / deficitCapacity).min(Ratio.One).toDouble else 0.0
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
  private def computeAvgDemandMult(
      sectorMults: Vector[Double],
      sectorCap: Vector[Double],
      in: Input,
  )(using p: SimParams): Double =
    val totalCapacity = sectorCap.kahanSum
    val baseMult      =
      if totalCapacity > 0 then sectorMults.indices.kahanSumBy(s => sectorMults(s) * sectorCap(s)) / totalCapacity
      else 1.0
    val realRateAdj   =
      if p.flags.expectations then
        val realRate = (in.w.nbp.referenceRate - in.w.mechanisms.expectations.expectedInflation).toDouble
        -realRate * RealRateElasticity
      else 0.0
    baseMult + realRateAdj
