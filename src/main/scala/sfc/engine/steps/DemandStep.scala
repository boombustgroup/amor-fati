package sfc.engine.steps

import sfc.agents.*
import sfc.config.SimParams
import sfc.engine.World
import sfc.types.*
import sfc.util.KahanSum.*

/** Aggregate demand formation: allocates household consumption, government
  * purchases, investment, and export demand across sectors via flow-of-funds
  * weights. Excess demand in capacity-constrained sectors spills over to
  * sectors with slack, and forward-looking real rate effects dampen or boost
  * aggregate demand when the expectations mechanism is active.
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
    val zusNetSurplus      =
      if p.flags.zus then Math.max(0.0, in.w.social.zus.contributions.toDouble - in.w.social.zus.pensionPayments.toDouble)
      else 0.0
    val unempRateForFiscal = 1.0 - in.s2.employed.toDouble / in.w.totalPopulation
    val unempGap           = Math.max(0.0, unempRateForFiscal - p.monetary.nairu.toDouble)
    val fiscalStimulus     = p.fiscal.govBaseSpending.toDouble * unempGap * p.fiscal.govAutoStabMult
    val targetGovPurchases = p.fiscal.govBaseSpending.toDouble * Math.max(1.0, in.w.priceLevel) +
      p.fiscal.govFiscalRecyclingRate.toDouble * (in.w.gov.taxRevenue.toDouble + zusNetSurplus) + fiscalStimulus
    val prevGovSpend       = in.w.gov.govCurrentSpend.toDouble + in.w.gov.govCapitalSpend.toDouble
    val govPurchases       =
      if prevGovSpend > 0 then Math.max(targetGovPurchases, prevGovSpend * GovSpendingFloor)
      else targetGovPurchases
    val laggedExports      = in.w.forex.exports.toDouble
    val sectorCap          = (0 until p.sectorDefs.length).map { s =>
      in.s2.living.filter(_.sector.toInt == s).kahanSumBy(f => Firm.computeCapacity(f).toDouble)
    }.toVector
    val sectorExports      =
      if p.flags.gvc && p.flags.openEcon then in.w.external.gvc.sectorExports.map(_.toDouble)
      else p.fiscal.fofExportShares.map(_.toDouble).map(_ * laggedExports)
    val laggedInvestDemand = in.w.real.grossInvestment.toDouble * (1.0 - p.capital.importShare.toDouble) +
      in.w.real.aggGreenInvestment.toDouble * (1.0 - p.climate.greenImportShare.toDouble)
    val sectorDemand       = (0 until p.sectorDefs.length).map { s =>
      p.fiscal.fofConsWeights.map(_.toDouble)(s) * in.s3.domesticCons.toDouble +
        p.fiscal.fofGovWeights.map(_.toDouble)(s) * govPurchases +
        p.fiscal.fofInvestWeights.map(_.toDouble)(s) * laggedInvestDemand +
        sectorExports(s)
    }.toVector
    val fofTotalDemand     = sectorDemand.kahanSum
    val totalCapacity      = sectorCap.kahanSum
    val rawSectorMults     = sectorDemand.indices.map { s =>
      if sectorCap(s) > 0 then sectorDemand(s) / (sectorCap(s) * in.w.priceLevel) else 0.0
    }.toVector
    val excessDemand       = rawSectorMults.indices.map { s =>
      if rawSectorMults(s) > 1.0 then (rawSectorMults(s) - 1.0) * sectorCap(s) * in.w.priceLevel else 0.0
    }.kahanSum
    val deficitCapacity    = rawSectorMults.indices.map { s =>
      if rawSectorMults(s) < 1.0 then (1.0 - rawSectorMults(s)) * sectorCap(s) * in.w.priceLevel else 0.0
    }.kahanSum
    val spilloverFrac      = if deficitCapacity > 0 then Math.min(1.0, excessDemand / deficitCapacity) else 0.0
    val sectorMults        = rawSectorMults.indices.map { s =>
      if rawSectorMults(s) > 1.0 then 1.0
      else rawSectorMults(s) + spilloverFrac * (1.0 - rawSectorMults(s))
    }.toVector
    val realRateEffect     = if p.flags.expectations then
      val realRate = in.w.nbp.referenceRate.toDouble - in.w.mechanisms.expectations.expectedInflation.toDouble
      -realRate * RealRateElasticity
    else 0.0
    val avgDemandMult      =
      (if totalCapacity > 0 then fofTotalDemand / (totalCapacity * in.w.priceLevel) else 1.0) + realRateEffect

    Output(PLN(govPurchases), sectorMults, avgDemandMult, sectorCap, PLN(laggedInvestDemand))
