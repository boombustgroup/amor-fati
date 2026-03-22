package com.boombustgroup.amorfati.engine.markets

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.amorfati.util.KahanSum.*

/** Global Value Chain trade: per-sector exports/imports with partner
  * differentiation.
  *
  * Models Poland's deep external sector as 12 foreign firm proxies (6 sectors ×
  * 2 partners: EU, non-EU). Each proxy carries base export demand, import
  * supply, price index, and supply-chain disruption state.
  *
  * Export demand: foreign GDP growth × real ER elasticity × automation boost ×
  * (1 − disruption). Import demand: sector output × GVC depth × differentiated
  * ER pass-through (EU vs non-EU). Disruptions recover at configurable rate.
  *
  * Calibration: GUS foreign trade data 2024, NBP BoP statistics, OECD TiVA.
  */
object GvcTrade:

  // --- Named constants ---
  private val NumPartners           = 2
  private val AutomationExportBoost = 0.15 // export uplift per unit automation ratio
  private val MinErEffect           = 0.1  // floor on ER pass-through multiplier

  case class ForeignFirm(
      id: Int,
      sectorId: Int,
      partnerId: Int, // 0=EU, 1=Non-EU
      baseExportDemand: PLN,
      baseImportSupply: PLN,
      priceIndex: Double,
      disruption: Share,
  )

  case class State(
      foreignFirms: Vector[ForeignFirm],
      totalExports: PLN = PLN.Zero,
      totalIntermImports: PLN = PLN.Zero,
      sectorExports: Vector[PLN] = Vector.fill(6)(PLN.Zero),
      sectorImports: Vector[PLN] = Vector.fill(6)(PLN.Zero),
      disruptionIndex: Share = Share.Zero,
      foreignPriceIndex: PriceIndex = PriceIndex.Base,
      tradeConcentration: Share = Share.Zero,
      exportDemandShockMag: Share = Share.Zero,
      importCostIndex: PriceIndex = PriceIndex.Base,
      commodityPriceIndex: PriceIndex = PriceIndex.Base,
  )

  def zero: State = State(Vector.empty)

  def initial(using p: SimParams): State =
    val euShare       = p.gvc.euTradeShare.toDouble
    val nonEuShare    = 1.0 - euShare
    val partnerShares = Vector(euShare, nonEuShare)
    val exportBase    = p.openEcon.exportBase.toDouble
    val exportShares  = p.gvc.exportShares.map(_.toDouble)
    val depths        = p.gvc.depth.map(_.toDouble)

    val nSectors = exportShares.size
    val firms    = for
      s  <- (0 until nSectors).toVector
      pi <- (0 until NumPartners).toVector
    yield
      val ps = partnerShares(pi)
      ForeignFirm(
        id = s * NumPartners + pi,
        sectorId = s,
        partnerId = pi,
        baseExportDemand = PLN(exportBase * exportShares(s) * ps),
        baseImportSupply = PLN(exportBase * depths(s) * ps),
        priceIndex = 1.0,
        disruption = Share.Zero,
      )

    State(firms, foreignPriceIndex = PriceIndex.Base, tradeConcentration = hhi(euShare))

  case class StepInput(
      prev: State,
      sectorOutputs: Vector[Double],
      priceLevel: Double,
      exchangeRate: Double,
      autoRatio: Double,
      month: Int,
      rng: scala.util.Random,
  )

  def step(in: StepInput)(using p: SimParams): State =
    val nSectors            = in.prev.sectorExports.size
    val monthlyInflation    = p.gvc.foreignInflation.monthly
    val monthlyInflationRaw = monthlyInflation.toDouble // for pre-existing helpers taking Double
    val newForeignPrice     = in.prev.foreignPriceIndex * Multiplier(monthlyInflationRaw + 1.0)
    // Commodity price: GBM drift + volatility + optional shock
    val commodityDrift      = p.gvc.commodityDrift.monthly
    val commodityNoise      = p.gvc.commodityVolatility.toDouble * in.rng.nextGaussian()
    val commodityShock      =
      if p.gvc.commodityShockMonth > 0 && in.month == p.gvc.commodityShockMonth then p.gvc.commodityShockMag
      else Multiplier.Zero
    val commodityGrowth     = Multiplier(commodityShock.toDouble + commodityDrift.toDouble + 1.0 + commodityNoise)
    val newCommodity        = in.prev.commodityPriceIndex * commodityGrowth
    val newImportCost       = newForeignPrice * newCommodity
    val shockActive         = p.gvc.demandShockMonth > 0 && in.month >= p.gvc.demandShockMonth
    val shockMag            = if shockActive then p.gvc.demandShockSize else Share.Zero
    val updatedFirms        = evolveFirms(in.prev.foreignFirms, monthlyInflationRaw, shockActive, in.month)
    val foreignGdpFactor    = Math.pow(1.0 + p.gvc.foreignGdpGrowth.monthly.toDouble, in.month.toDouble)
    val erEffect            = realExchangeRateEffect(in.priceLevel, in.exchangeRate)
    val exports             = computeSectorExports(updatedFirms, nSectors, foreignGdpFactor, erEffect, in.autoRatio)
    val imports             = computeSectorImports(updatedFirms, nSectors, in.sectorOutputs, in.priceLevel, in.exchangeRate)
    val euShare             = p.gvc.euTradeShare.toDouble

    State(
      foreignFirms = updatedFirms,
      totalExports = kahanSumPln(exports),
      totalIntermImports = kahanSumPln(imports),
      sectorExports = exports,
      sectorImports = imports,
      disruptionIndex = weightedDisruption(updatedFirms),
      foreignPriceIndex = newForeignPrice,
      tradeConcentration = hhi(euShare),
      exportDemandShockMag = shockMag,
      importCostIndex = newImportCost,
      commodityPriceIndex = newCommodity,
    )

  // --- Private helpers ---

  /** Evolve foreign firms: apply demand shock, recover disruptions, update
    * price.
    */
  private def evolveFirms(
      firms: Vector[ForeignFirm],
      monthlyInflation: Double,
      shockActive: Boolean,
      month: Int,
  )(using p: SimParams): Vector[ForeignFirm] =
    val recoveryRate = p.gvc.disruptionRecovery.toDouble
    firms.map: ff =>
      val afterShock =
        if shockActive && month == p.gvc.demandShockMonth &&
          p.gvc.demandShockSectors.contains(ff.sectorId)
        then ff.copy(baseExportDemand = ff.baseExportDemand * Share(1.0 - p.gvc.demandShockSize.toDouble))
        else ff
      afterShock.copy(
        disruption = Share(afterShock.disruption.toDouble * (1.0 - recoveryRate)),
        priceIndex = afterShock.priceIndex * (1.0 + monthlyInflation),
      )

  /** Real exchange rate effect on exports (Marshall-Lerner). */
  private def realExchangeRateEffect(priceLevel: Double, exchangeRate: Double)(using p: SimParams): Double =
    val nominalER = exchangeRate / p.forex.baseExRate
    val realPrice = if priceLevel > 0 && nominalER > 0 then priceLevel / nominalER else 1.0
    Math.pow(1.0 / Math.max(MinErEffect, realPrice), p.openEcon.exportPriceElasticity.toDouble)

  /** Per-sector export demand. */
  private def computeSectorExports(
      firms: Vector[ForeignFirm],
      nSectors: Int,
      foreignGdpFactor: Double,
      erEffect: Double,
      autoRatio: Double,
  ): Vector[PLN] =
    val autoBoost = 1.0 + autoRatio * AutomationExportBoost
    (0 until nSectors)
      .map: s =>
        val sectorFirms = firms.filter(_.sectorId == s)
        val demand      = sectorFirms.kahanSumBy(_.baseExportDemand.toDouble) * foreignGdpFactor
        val disruption  = avgSectorDisruption(sectorFirms)
        PLN(demand * erEffect * autoBoost * (1.0 - disruption))
      .toVector

  /** Per-sector intermediate import demand with differentiated ER pass-through.
    */
  private def computeSectorImports(
      firms: Vector[ForeignFirm],
      nSectors: Int,
      sectorOutputs: Vector[Double],
      priceLevel: Double,
      exchangeRate: Double,
  )(using p: SimParams): Vector[PLN] =
    val depths      = p.gvc.depth.map(_.toDouble)
    val erDeviation = exchangeRate / p.forex.baseExRate - 1.0
    (0 until nSectors)
      .map: s =>
        val realOutput  = if priceLevel > 0 then sectorOutputs(s) / priceLevel else sectorOutputs(s)
        val baseDemand  = realOutput * depths(s)
        val sectorFirms = firms.filter(_.sectorId == s)
        val erEffect    = partnerWeightedErEffect(sectorFirms, erDeviation)
        val disruption  = avgSectorDisruption(sectorFirms)
        PLN(baseDemand * Math.max(MinErEffect, erEffect) * (1.0 - disruption))
      .toVector

  /** Weighted ER pass-through across EU/non-EU partners for a sector. */
  private def partnerWeightedErEffect(sectorFirms: Vector[ForeignFirm], erDeviation: Double)(using p: SimParams): Double =
    val totalSupply = sectorFirms.kahanSumBy(_.baseImportSupply.toDouble)
    if totalSupply > 0 then
      val euWeight    = sectorFirms.filter(_.partnerId == 0).kahanSumBy(_.baseImportSupply.toDouble) / totalSupply
      val nonEuWeight = 1.0 - euWeight
      1.0 + euWeight * erDeviation * p.gvc.euErPassthrough.toDouble +
        nonEuWeight * erDeviation * p.gvc.erPassthrough.toDouble
    else 1.0

  /** Average disruption across firms in a sector. */
  private def avgSectorDisruption(sectorFirms: Vector[ForeignFirm]): Double =
    if sectorFirms.nonEmpty then sectorFirms.kahanSumBy(_.disruption.toDouble) / sectorFirms.length
    else 0.0

  /** Demand-weighted disruption index across all firms. */
  private def weightedDisruption(firms: Vector[ForeignFirm]): Share =
    if firms.isEmpty then Share.Zero
    else
      val totalDemand = firms.kahanSumBy(_.baseExportDemand.toDouble)
      if totalDemand > 0 then Share(firms.kahanSumBy(ff => ff.disruption.toDouble * ff.baseExportDemand.toDouble) / totalDemand)
      else Share.Zero

  /** Herfindahl-Hirschman Index for two-partner concentration. */
  private def hhi(euShare: Double): Share =
    Share(euShare * euShare + (1.0 - euShare) * (1.0 - euShare))

  /** Kahan-sum a vector of PLN values. */
  private def kahanSumPln(vs: Vector[PLN]): PLN =
    PLN(vs.map(_.toDouble).kahanSum)
