package sfc.engine.steps

import sfc.agents.*
import sfc.config.{SectorDefs, SimParams}
import sfc.engine.World
import sfc.types.*
import sfc.util.KahanSum.*

object DemandStep:

  case class Input(
      w: World,
      s2: LaborDemographicsStep.Output,
      s3: HouseholdIncomeStep.Output,
  )

  case class Output(
      govPurchases: Double,
      sectorMults: Vector[Double],
      avgDemandMult: Double,
      sectorCap: Vector[Double],
      laggedInvestDemand: Double,
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
      if prevGovSpend > 0 then Math.max(targetGovPurchases, prevGovSpend * 0.98)
      else targetGovPurchases
    val laggedExports      = in.w.forex.exports.toDouble
    val sectorCap          = (0 until SectorDefs.length).map { s =>
      in.s2.living.filter(_.sector.toInt == s).kahanSumBy(f => Firm.computeCapacity(f).toDouble)
    }.toVector
    val sectorExports      =
      if p.flags.gvc && p.flags.openEcon then in.w.external.gvc.sectorExports.map(_.toDouble)
      else p.fiscal.fofExportShares.map(_.toDouble).map(_ * laggedExports)
    val laggedInvestDemand = in.w.real.grossInvestment.toDouble * (1.0 - p.capital.importShare.toDouble) +
      in.w.real.aggGreenInvestment.toDouble * (1.0 - p.climate.greenImportShare.toDouble)
    val sectorDemand       = (0 until SectorDefs.length).map { s =>
      p.fiscal.fofConsWeights.map(_.toDouble)(s) * in.s3.domesticCons +
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
      -realRate * 0.02
    else 0.0
    val avgDemandMult      =
      (if totalCapacity > 0 then fofTotalDemand / (totalCapacity * in.w.priceLevel) else 1.0) + realRateEffect

    Output(govPurchases, sectorMults, avgDemandMult, sectorCap, laggedInvestDemand)
