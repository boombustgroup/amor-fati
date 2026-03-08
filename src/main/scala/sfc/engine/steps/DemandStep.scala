package sfc.engine.steps

import sfc.agents.*
import sfc.config.{Config, SECTORS}
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

  def run(in: Input): Output =
    val zusNetSurplus =
      if Config.ZusEnabled then Math.max(0.0, in.w.zus.contributions.toDouble - in.w.zus.pensionPayments.toDouble)
      else 0.0
    val unempRateForFiscal = 1.0 - in.s2.employed.toDouble / Config.TotalPopulation
    val unempGap = Math.max(0.0, unempRateForFiscal - Config.NbpNairu)
    val fiscalStimulus = Config.GovBaseSpending * unempGap * Config.GovAutoStabMult
    val targetGovPurchases = Config.GovBaseSpending * Math.max(1.0, in.w.priceLevel) +
      Config.GovFiscalRecyclingRate * (in.w.gov.taxRevenue.toDouble + zusNetSurplus) + fiscalStimulus
    val prevGovSpend = in.w.gov.govCurrentSpend.toDouble + in.w.gov.govCapitalSpend.toDouble
    val govPurchases =
      if prevGovSpend > 0 then Math.max(targetGovPurchases, prevGovSpend * 0.98)
      else targetGovPurchases
    val laggedExports = in.w.forex.exports.toDouble
    val sectorCap = (0 until SECTORS.length).map { s =>
      in.s2.living.filter(_.sector.toInt == s).kahanSumBy(f => Firm.capacity(f).toDouble)
    }.toVector
    val sectorExports =
      if Config.GvcEnabled && Config.OeEnabled then in.w.gvc.sectorExports.map(_.toDouble)
      else Config.FofExportShares.map(_ * laggedExports)
    val laggedInvestDemand = in.w.grossInvestment.toDouble * (1.0 - Config.PhysCapImportShare) +
      in.w.aggGreenInvestment.toDouble * (1.0 - Config.GreenImportShare)
    val sectorDemand = (0 until SECTORS.length).map { s =>
      Config.FofConsWeights(s) * in.s3.domesticCons +
        Config.FofGovWeights(s) * govPurchases +
        Config.FofInvestWeights(s) * laggedInvestDemand +
        sectorExports(s)
    }.toVector
    val fofTotalDemand = sectorDemand.kahanSum
    val totalCapacity = sectorCap.kahanSum
    val rawSectorMults = sectorDemand.indices.map { s =>
      if sectorCap(s) > 0 then sectorDemand(s) / (sectorCap(s) * in.w.priceLevel) else 0.0
    }.toVector
    val excessDemand = rawSectorMults.indices.map { s =>
      if rawSectorMults(s) > 1.0 then (rawSectorMults(s) - 1.0) * sectorCap(s) * in.w.priceLevel else 0.0
    }.kahanSum
    val deficitCapacity = rawSectorMults.indices.map { s =>
      if rawSectorMults(s) < 1.0 then (1.0 - rawSectorMults(s)) * sectorCap(s) * in.w.priceLevel else 0.0
    }.kahanSum
    val spilloverFrac = if deficitCapacity > 0 then Math.min(1.0, excessDemand / deficitCapacity) else 0.0
    val sectorMults = rawSectorMults.indices.map { s =>
      if rawSectorMults(s) > 1.0 then 1.0
      else rawSectorMults(s) + spilloverFrac * (1.0 - rawSectorMults(s))
    }.toVector
    val realRateEffect = if Config.ExpEnabled then
      val realRate = in.w.nbp.referenceRate.toDouble - in.w.expectations.expectedInflation.toDouble
      -realRate * 0.02
    else 0.0
    val avgDemandMult =
      (if totalCapacity > 0 then fofTotalDemand / (totalCapacity * in.w.priceLevel) else 1.0) + realRateEffect

    Output(govPurchases, sectorMults, avgDemandMult, sectorCap, laggedInvestDemand)
