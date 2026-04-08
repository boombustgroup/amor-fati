package com.boombustgroup.amorfati.engine.markets

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

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

  private def priceLevelValue(priceLevel: PriceIndex): Double =
    priceLevel.toMultiplier / Multiplier.One

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

  @boundaryEscape
  def initial(using p: SimParams): State =
    import ComputationBoundary.toDouble
    val euShare       = toDouble(p.gvc.euTradeShare)
    val nonEuShare    = 1.0 - euShare
    val partnerShares = Vector(euShare, nonEuShare)
    val exportBase    = toDouble(p.openEcon.exportBase)
    val exportShares  = p.gvc.exportShares.map(s => toDouble(s))
    val depths        = p.gvc.depth.map(s => toDouble(s))

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
      priceLevel: PriceIndex,
      exchangeRate: Double,
      autoRatio: Double,
      month: Int,
      rng: scala.util.Random,
  )

  @boundaryEscape
  def step(in: StepInput)(using p: SimParams): State =
    import ComputationBoundary.toDouble
    val nSectors            = in.prev.sectorExports.size
    val monthlyInflationRaw = toDouble(p.gvc.foreignInflation.monthly)
    val newForeignPrice     = in.prev.foreignPriceIndex * Multiplier(monthlyInflationRaw + 1.0)
    // Commodity price: GBM drift + volatility + optional shock
    val commodityDrift      = toDouble(p.gvc.commodityDrift.monthly)
    val commodityNoise      = toDouble(p.gvc.commodityVolatility) * in.rng.nextGaussian()
    val commodityShock      =
      if p.gvc.commodityShockMonth > 0 && in.month == p.gvc.commodityShockMonth then toDouble(p.gvc.commodityShockMag)
      else 0.0
    val commodityGrowth     = Multiplier(commodityShock + commodityDrift + 1.0 + commodityNoise)
    val newCommodity        = in.prev.commodityPriceIndex * commodityGrowth
    val newImportCost       = newForeignPrice * newCommodity
    val shockActive         = p.gvc.demandShockMonth > 0 && in.month >= p.gvc.demandShockMonth
    val shockMag            = if shockActive then p.gvc.demandShockSize else Share.Zero
    val updatedFirms        = evolveFirms(in.prev.foreignFirms, monthlyInflationRaw, shockActive, in.month)
    val foreignGdpFactor    = Math.pow(1.0 + toDouble(p.gvc.foreignGdpGrowth.monthly), in.month.toDouble)
    val erEffect            = realExchangeRateEffect(in.priceLevel, in.exchangeRate)
    val exports             = computeSectorExports(updatedFirms, nSectors, foreignGdpFactor, erEffect, in.autoRatio)
    val imports             = computeSectorImports(updatedFirms, nSectors, in.sectorOutputs, in.priceLevel, in.exchangeRate)
    val euShare             = toDouble(p.gvc.euTradeShare)

    State(
      foreignFirms = updatedFirms,
      totalExports = sumPln(exports),
      totalIntermImports = sumPln(imports),
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
  @boundaryEscape
  private def evolveFirms(
      firms: Vector[ForeignFirm],
      monthlyInflation: Double,
      shockActive: Boolean,
      month: Int,
  )(using p: SimParams): Vector[ForeignFirm] =
    import ComputationBoundary.toDouble
    val recoveryRate = toDouble(p.gvc.disruptionRecovery)
    firms.map: ff =>
      val afterShock =
        if shockActive && month == p.gvc.demandShockMonth &&
          p.gvc.demandShockSectors.contains(ff.sectorId)
        then ff.copy(baseExportDemand = ff.baseExportDemand * (Share.One - p.gvc.demandShockSize))
        else ff
      afterShock.copy(
        disruption = Share(toDouble(afterShock.disruption) * (1.0 - recoveryRate)),
        priceIndex = afterShock.priceIndex * (1.0 + monthlyInflation),
      )

  /** Real exchange rate effect on exports (Marshall-Lerner). */
  @boundaryEscape
  private def realExchangeRateEffect(priceLevel: PriceIndex, exchangeRate: Double)(using p: SimParams): Double =
    import ComputationBoundary.toDouble
    val nominalER = exchangeRate / p.forex.baseExRate
    val price     = priceLevelValue(priceLevel)
    val realPrice = if price > 0 && nominalER > 0 then price / nominalER else 1.0
    Math.pow(1.0 / Math.max(MinErEffect, realPrice), toDouble(p.openEcon.exportPriceElasticity))

  /** Per-sector export demand. */
  @boundaryEscape
  private def computeSectorExports(
      firms: Vector[ForeignFirm],
      nSectors: Int,
      foreignGdpFactor: Double,
      erEffect: Double,
      autoRatio: Double,
  ): Vector[PLN] =
    import ComputationBoundary.toDouble
    val autoBoost = 1.0 + autoRatio * AutomationExportBoost
    (0 until nSectors)
      .map: s =>
        val sectorFirms = firms.filter(_.sectorId == s)
        val demand      = sectorFirms.map(ff => toDouble(ff.baseExportDemand)).sum * foreignGdpFactor
        val disruption  = avgSectorDisruption(sectorFirms)
        PLN(demand * erEffect * autoBoost * (1.0 - disruption))
      .toVector

  /** Per-sector intermediate import demand with differentiated ER pass-through.
    */
  @boundaryEscape
  private def computeSectorImports(
      firms: Vector[ForeignFirm],
      nSectors: Int,
      sectorOutputs: Vector[Double],
      priceLevel: PriceIndex,
      exchangeRate: Double,
  )(using p: SimParams): Vector[PLN] =
    import ComputationBoundary.toDouble
    val depths      = p.gvc.depth.map(s => toDouble(s))
    val erDeviation = exchangeRate / p.forex.baseExRate - 1.0
    val price       = priceLevelValue(priceLevel)
    (0 until nSectors)
      .map: s =>
        val realOutput  = if price > 0 then sectorOutputs(s) / price else sectorOutputs(s)
        val baseDemand  = realOutput * depths(s)
        val sectorFirms = firms.filter(_.sectorId == s)
        val erEffect    = partnerWeightedErEffect(sectorFirms, erDeviation)
        val disruption  = avgSectorDisruption(sectorFirms)
        PLN(baseDemand * Math.max(MinErEffect, erEffect) * (1.0 - disruption))
      .toVector

  /** Weighted ER pass-through across EU/non-EU partners for a sector. */
  @boundaryEscape
  private def partnerWeightedErEffect(sectorFirms: Vector[ForeignFirm], erDeviation: Double)(using p: SimParams): Double =
    import ComputationBoundary.toDouble
    val totalSupply = sectorFirms.map(ff => toDouble(ff.baseImportSupply)).sum
    if totalSupply > 0 then
      val euWeight    = sectorFirms.filter(_.partnerId == 0).map(ff => toDouble(ff.baseImportSupply)).sum / totalSupply
      val nonEuWeight = 1.0 - euWeight
      1.0 + euWeight * erDeviation * toDouble(p.gvc.euErPassthrough) +
        nonEuWeight * erDeviation * toDouble(p.gvc.erPassthrough)
    else 1.0

  /** Average disruption across firms in a sector. */
  @boundaryEscape
  private def avgSectorDisruption(sectorFirms: Vector[ForeignFirm]): Double =
    import ComputationBoundary.toDouble
    if sectorFirms.nonEmpty then sectorFirms.map(ff => toDouble(ff.disruption)).sum / sectorFirms.length
    else 0.0

  /** Demand-weighted disruption index across all firms. */
  @boundaryEscape
  private def weightedDisruption(firms: Vector[ForeignFirm]): Share =
    import ComputationBoundary.toDouble
    if firms.isEmpty then Share.Zero
    else
      val totalDemand = firms.map(ff => toDouble(ff.baseExportDemand)).sum
      if totalDemand > 0 then Share(firms.map(ff => toDouble(ff.disruption) * toDouble(ff.baseExportDemand)).sum / totalDemand)
      else Share.Zero

  /** Herfindahl-Hirschman Index for two-partner concentration. */
  private def hhi(euShare: Double): Share =
    Share(euShare * euShare + (1.0 - euShare) * (1.0 - euShare))

  /** Sum a vector of PLN values (exact Long addition). */
  private def sumPln(vs: Vector[PLN]): PLN =
    PLN.fromRaw(vs.map(_.toLong).sum)
