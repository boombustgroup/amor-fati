package com.boombustgroup.amorfati.engine.markets

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.random.RandomStream
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.amorfati.util.Distributions

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

  private val NumPartners           = 2
  private val AutomationExportBoost = Coefficient("0.15")
  private val MinErEffect           = Multiplier("0.1")

  case class ForeignFirm(
      id: Int,
      sectorId: Int,
      partnerId: Int, // 0=EU, 1=Non-EU
      baseExportDemand: PLN,
      baseImportSupply: PLN,
      priceIndex: PriceIndex,
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
    val euShare       = p.gvc.euTradeShare
    val nonEuShare    = Share.One - euShare
    val partnerShares = Vector(euShare, nonEuShare)
    val exportBase    = p.openEcon.exportBase
    val exportShares  = p.gvc.exportShares
    val depths        = p.gvc.depth

    val firms = for
      s  <- exportShares.indices.toVector
      pi <- (0 until NumPartners).toVector
    yield
      val partnerShare = partnerShares(pi)
      ForeignFirm(
        id = s * NumPartners + pi,
        sectorId = s,
        partnerId = pi,
        baseExportDemand = (exportBase * exportShares(s)) * partnerShare,
        baseImportSupply = (exportBase * depths(s)) * partnerShare,
        priceIndex = PriceIndex.Base,
        disruption = Share.Zero,
      )

    State(
      foreignFirms = firms,
      totalExports = sumPln(exportShares.map(_ * exportBase)),
      totalIntermImports = sumPln(depths.map(_ * exportBase)),
      sectorExports = exportShares.map(_ * exportBase),
      sectorImports = depths.map(_ * exportBase),
      foreignPriceIndex = PriceIndex.Base,
      tradeConcentration = hhi(euShare),
      importCostIndex = PriceIndex.Base,
      commodityPriceIndex = PriceIndex.Base,
    )

  case class StepInput(
      prev: State,
      sectorOutputs: Vector[PLN],
      priceLevel: PriceIndex,
      exchangeRate: ExchangeRate,
      autoRatio: Share,
      month: ExecutionMonth,
      rng: RandomStream,
  )

  def step(in: StepInput)(using p: SimParams): State =
    val nSectors         = in.sectorOutputs.length
    require(
      in.prev.sectorExports.lengthCompare(nSectors) == 0,
      s"GvcTrade prev.sectorExports length ${in.prev.sectorExports.length} must match sectorOutputs length $nSectors",
    )
    require(
      in.prev.sectorImports.lengthCompare(nSectors) == 0,
      s"GvcTrade prev.sectorImports length ${in.prev.sectorImports.length} must match sectorOutputs length $nSectors",
    )
    val monthlyInflation = p.gvc.foreignInflation.monthly
    val newForeignPrice  = in.prev.foreignPriceIndex.applyGrowth(monthlyInflation.toCoefficient)
    val commodityDrift   = p.gvc.commodityDrift.monthly.toCoefficient
    val commodityNoise   = Coefficient.fromRaw(Distributions.gaussianNoiseRaw(p.gvc.commodityVolatility.toScalar, in.rng))
    val commodityShock   =
      if p.gvc.commodityShockMonth > 0 && in.month.toInt == p.gvc.commodityShockMonth then p.gvc.commodityShockMag.toCoefficient
      else Coefficient.Zero
    val commodityGrowth  = (commodityShock + commodityDrift + commodityNoise).growthMultiplier
    val newCommodity     = in.prev.commodityPriceIndex * commodityGrowth
    val newImportCost    = newForeignPrice * newCommodity
    val shockActive      = p.gvc.demandShockMonth > 0 && in.month.toInt >= p.gvc.demandShockMonth
    val shockMag         = if shockActive then p.gvc.demandShockSize else Share.Zero
    val updatedFirms     = evolveFirms(in.prev.foreignFirms, monthlyInflation, shockActive, in.month)
    val elapsedMonths    = in.month.previousCompleted.toInt
    val foreignGdpFactor = compoundedGrowth(p.gvc.foreignGdpGrowth.monthly.growthMultiplier, elapsedMonths)
    val erEffect         = realExchangeRateEffect(in.priceLevel, in.exchangeRate)
    val exports          = computeSectorExports(updatedFirms, nSectors, foreignGdpFactor, erEffect, in.autoRatio)
    val imports          = computeSectorImports(updatedFirms, nSectors, in.sectorOutputs, in.priceLevel, in.exchangeRate)

    State(
      foreignFirms = updatedFirms,
      totalExports = sumPln(exports),
      totalIntermImports = sumPln(imports),
      sectorExports = exports,
      sectorImports = imports,
      disruptionIndex = weightedDisruption(updatedFirms),
      foreignPriceIndex = newForeignPrice,
      tradeConcentration = hhi(p.gvc.euTradeShare),
      exportDemandShockMag = shockMag,
      importCostIndex = newImportCost,
      commodityPriceIndex = newCommodity,
    )

  private def evolveFirms(
      firms: Vector[ForeignFirm],
      monthlyInflation: Rate,
      shockActive: Boolean,
      month: ExecutionMonth,
  )(using p: SimParams): Vector[ForeignFirm] =
    val recoveryRate = p.gvc.disruptionRecovery
    firms.map: ff =>
      val afterShock =
        if shockActive && month.toInt == p.gvc.demandShockMonth &&
          p.gvc.demandShockSectors.contains(ff.sectorId)
        then ff.copy(baseExportDemand = ff.baseExportDemand * (Share.One - p.gvc.demandShockSize))
        else ff
      afterShock.copy(
        disruption = afterShock.disruption * (Share.One - recoveryRate),
        priceIndex = afterShock.priceIndex.applyGrowth(monthlyInflation.toCoefficient),
      )

  private def realExchangeRateEffect(priceLevel: PriceIndex, exchangeRate: ExchangeRate)(using p: SimParams): Scalar =
    val nominalER = exchangeRate.ratioTo(p.forex.baseExRate)
    val realPrice = priceLevel.toMultiplier.ratioTo(nominalER).max(MinErEffect.toScalar)
    realPrice.reciprocal.pow(p.openEcon.exportPriceElasticity.toScalar)

  private def computeSectorExports(
      firms: Vector[ForeignFirm],
      nSectors: Int,
      foreignGdpFactor: Multiplier,
      erEffect: Scalar,
      autoRatio: Share,
  ): Vector[PLN] =
    val autoBoost = (autoRatio * AutomationExportBoost).growthMultiplier
    (0 until nSectors)
      .map: s =>
        val sectorFirms = firms.filter(_.sectorId == s)
        val demand      = sumPln(sectorFirms.map(_.baseExportDemand)) * foreignGdpFactor
        val disruption  = weightedSectorDisruption(sectorFirms, _.baseExportDemand)
        ((erEffect * demand) * autoBoost) * (Share.One - disruption)
      .toVector

  private def computeSectorImports(
      firms: Vector[ForeignFirm],
      nSectors: Int,
      sectorOutputs: Vector[PLN],
      priceLevel: PriceIndex,
      exchangeRate: ExchangeRate,
  )(using p: SimParams): Vector[PLN] =
    val erDeviation = exchangeRate.deviationFrom(p.forex.baseExRate).toCoefficient
    (0 until nSectors)
      .map: s =>
        val realOutput  = sectorOutputs(s) / priceLevel.toMultiplier
        val baseDemand  = realOutput * p.gvc.depth(s)
        val sectorFirms = firms.filter(_.sectorId == s)
        val erEffect    = partnerWeightedErEffect(sectorFirms, erDeviation)
        val disruption  = weightedSectorDisruption(sectorFirms, _.baseImportSupply)
        (baseDemand * erEffect.max(MinErEffect)) * (Share.One - disruption)
      .toVector

  private def partnerWeightedErEffect(sectorFirms: Vector[ForeignFirm], erDeviation: Coefficient)(using p: SimParams): Multiplier =
    val totalSupply = sumPln(sectorFirms.map(_.baseImportSupply))
    if totalSupply > PLN.Zero then
      val euSupply    = sumPln(sectorFirms.filter(_.partnerId == 0).map(_.baseImportSupply))
      val euWeight    = euSupply.ratioTo(totalSupply).toShare
      val nonEuWeight = Share.One - euWeight
      val passthrough = (euWeight * p.gvc.euErPassthrough) + (nonEuWeight * p.gvc.erPassthrough)
      (passthrough * erDeviation).growthMultiplier
    else Multiplier.One

  private def weightedSectorDisruption(
      sectorFirms: Vector[ForeignFirm],
      weight: ForeignFirm => PLN,
  ): Share =
    val totalWeight = sumPln(sectorFirms.map(weight))
    if totalWeight > PLN.Zero then sumPln(sectorFirms.map(ff => ff.disruption * weight(ff))).ratioTo(totalWeight).toShare
    else Share.Zero

  private def weightedDisruption(firms: Vector[ForeignFirm]): Share =
    if firms.isEmpty then Share.Zero
    else
      val totalDemand = sumPln(firms.map(_.baseExportDemand))
      if totalDemand > PLN.Zero then sumPln(firms.map(ff => ff.disruption * ff.baseExportDemand)).ratioTo(totalDemand).toShare
      else Share.Zero

  private def hhi(euShare: Share): Share =
    (euShare * euShare) + ((Share.One - euShare) * (Share.One - euShare))

  private def compoundedGrowth(monthlyFactor: Multiplier, periods: Int): Multiplier =
    if periods <= 0 then Multiplier.One
    else Iterator.fill(periods)(monthlyFactor).foldLeft(Multiplier.One)(_ * _)

  private def sumPln(vs: Vector[PLN]): PLN =
    vs.foldLeft(PLN.Zero)(_ + _)
