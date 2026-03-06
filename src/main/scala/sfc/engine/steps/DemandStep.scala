package sfc.engine.steps

import sfc.agents.*
import sfc.config.{Config, SECTORS}
import sfc.types.*
import sfc.util.KahanSum.*

object DemandStep:

  case class Input(
    employed: Int,
    domesticCons: Double,
    living: Array[Firm.State],
    priceLevel: Double,
    zusContributions: Double,
    zusPensionPayments: Double,
    govTaxRevenue: Double,
    govCurrentSpend: Double,
    govCapitalSpend: Double,
    nbpReferenceRate: Double,
    expectedInflation: Double,
    forexExports: Double,
    gvcEnabled: Boolean,
    gvcSectorExports: Vector[PLN],
    grossInvestment: Double,
    aggGreenInvestment: Double
  )

  case class Output(
    govPurchases: Double,
    sectorMults: Vector[Double],
    avgDemandMult: Double,
    sectorCap: Vector[Double],
    laggedInvestDemand: Double
  )

  def run(in: Input): Output =
    val zusNetSurplus = if Config.ZusEnabled then
      Math.max(0.0, in.zusContributions - in.zusPensionPayments) else 0.0
    val unempRateForFiscal = 1.0 - in.employed.toDouble / Config.TotalPopulation
    val unempGap = Math.max(0.0, unempRateForFiscal - Config.NbpNairu)
    val fiscalStimulus = Config.GovBaseSpending * unempGap * Config.GovAutoStabMult
    val targetGovPurchases = Config.GovBaseSpending * Math.max(1.0, in.priceLevel) +
      Config.GovFiscalRecyclingRate * (in.govTaxRevenue + zusNetSurplus) + fiscalStimulus
    val prevGovSpend = in.govCurrentSpend + in.govCapitalSpend
    val govPurchases = if prevGovSpend > 0 then
      Math.max(targetGovPurchases, prevGovSpend * 0.98)
    else targetGovPurchases
    val laggedExports = in.forexExports
    val sectorCap = (0 until SECTORS.length).map { s =>
      in.living.filter(_.sector.toInt == s).kahanSumBy(f => Firm.capacity(f).toDouble)
    }.toVector
    val sectorExports = if in.gvcEnabled && Config.OeEnabled then
      in.gvcSectorExports.map(_.toDouble)
    else
      Config.FofExportShares.map(_ * laggedExports)
    val laggedInvestDemand = in.grossInvestment * (1.0 - Config.PhysCapImportShare) +
      in.aggGreenInvestment * (1.0 - Config.GreenImportShare)
    val sectorDemand = (0 until SECTORS.length).map { s =>
      Config.FofConsWeights(s) * in.domesticCons +
      Config.FofGovWeights(s) * govPurchases +
      Config.FofInvestWeights(s) * laggedInvestDemand +
      sectorExports(s)
    }.toVector
    val fofTotalDemand = sectorDemand.kahanSum
    val totalCapacity = sectorCap.kahanSum
    val rawSectorMults = sectorDemand.indices.map { s =>
      if sectorCap(s) > 0 then sectorDemand(s) / (sectorCap(s) * in.priceLevel) else 0.0
    }.toVector
    val excessDemand = rawSectorMults.indices.map { s =>
      if rawSectorMults(s) > 1.0 then (rawSectorMults(s) - 1.0) * sectorCap(s) * in.priceLevel else 0.0
    }.kahanSum
    val deficitCapacity = rawSectorMults.indices.map { s =>
      if rawSectorMults(s) < 1.0 then (1.0 - rawSectorMults(s)) * sectorCap(s) * in.priceLevel else 0.0
    }.kahanSum
    val spilloverFrac = if deficitCapacity > 0 then Math.min(1.0, excessDemand / deficitCapacity) else 0.0
    val sectorMults = rawSectorMults.indices.map { s =>
      if rawSectorMults(s) > 1.0 then 1.0
      else rawSectorMults(s) + spilloverFrac * (1.0 - rawSectorMults(s))
    }.toVector
    val realRateEffect = if Config.ExpEnabled then
      val realRate = in.nbpReferenceRate - in.expectedInflation
      -realRate * 0.02
    else 0.0
    val avgDemandMult = (if totalCapacity > 0 then fofTotalDemand / (totalCapacity * in.priceLevel) else 1.0) + realRateEffect

    Output(govPurchases, sectorMults, avgDemandMult, sectorCap, laggedInvestDemand)
