package sfc.engine

import sfc.config.{Config, RunConfig}

case class ForeignFirm(
  id: Int,
  sectorId: Int,
  partnerId: Int,              // 0=EU, 1=Non-EU
  baseExportDemand: Double,
  baseImportSupply: Double,
  priceIndex: Double,
  disruption: Double
)

case class GvcState(
  foreignFirms: Vector[ForeignFirm],
  totalExports: Double = 0.0,
  totalIntermImports: Double = 0.0,
  sectorExports: Vector[Double] = Vector.fill(6)(0.0),
  sectorImports: Vector[Double] = Vector.fill(6)(0.0),
  disruptionIndex: Double = 0.0,
  foreignPriceIndex: Double = 1.0,
  tradeConcentration: Double = 0.0,
  exportDemandShockMag: Double = 0.0,
  importCostIndex: Double = 1.0
)

object ExternalSector:

  def zero: GvcState = GvcState(Vector.empty)

  def initial: GvcState =
    val euShare = Config.GvcEuTradeShare
    val nonEuShare = 1.0 - euShare
    val partnerShares = Vector(euShare, nonEuShare)

    val firms = for
      s <- (0 until 6).toVector
      p <- (0 until 2).toVector
    yield
      val ps = partnerShares(p)
      ForeignFirm(
        id = s * 2 + p,
        sectorId = s,
        partnerId = p,
        baseExportDemand = Config.OeExportBase * Config.GvcExportShares(s) * ps,
        baseImportSupply = Config.OeExportBase * Config.GvcDepth(s) * ps,
        priceIndex = 1.0,
        disruption = 0.0
      )

    GvcState(firms, foreignPriceIndex = 1.0,
      tradeConcentration = euShare * euShare + nonEuShare * nonEuShare)

  def step(prev: GvcState, sectorOutputs: Vector[Double], priceLevel: Double,
           exchangeRate: Double, autoRatio: Double, month: Int,
           rc: RunConfig): GvcState =

    // 1. Evolve foreign price
    val newForeignPrice = prev.foreignPriceIndex * (1.0 + Config.GvcForeignInflation / 12.0)

    // 2. Apply demand shock + recover disruptions
    val shockActive = Config.GvcDemandShockMonth > 0 && month >= Config.GvcDemandShockMonth
    val shockMag = if shockActive then Config.GvcDemandShockSize else 0.0

    val updatedFirms = prev.foreignFirms.map { ff =>
      val afterShock = if shockActive && month == Config.GvcDemandShockMonth &&
                          Config.GvcDemandShockSectors.contains(ff.sectorId) then
        ff.copy(baseExportDemand = ff.baseExportDemand * (1.0 - Config.GvcDemandShockSize))
      else ff

      // Recover disruptions
      val newDisruption = afterShock.disruption * (1.0 - Config.GvcDisruptionRecovery)
      // Evolve price
      val newPrice = afterShock.priceIndex * (1.0 + Config.GvcForeignInflation / 12.0)
      afterShock.copy(disruption = newDisruption, priceIndex = newPrice)
    }

    // 3. Foreign GDP growth factor
    val foreignGdpFactor = Math.pow(1.0 + Config.GvcForeignGdpGrowth / 12.0, month.toDouble)

    // 4. Real exchange rate effect (same formula as OpenEconomy)
    val realExRateEffect = if rc.isEurozone then 1.0
    else
      val nominalER = exchangeRate / Config.BaseExRate
      val realPrice = if priceLevel > 0 && nominalER > 0 then priceLevel / nominalER else 1.0
      Math.pow(1.0 / Math.max(0.1, realPrice), Config.OeExportPriceElasticity)

    // 5. Sector-specific exports
    val sectorExports = (0 until 6).map { s =>
      val sectorFirms = updatedFirms.filter(_.sectorId == s)
      val demand = sectorFirms.map(_.baseExportDemand).sum * foreignGdpFactor
      // Per-sector automation ratio from sectorOutputs (proxy)
      val sectorAutoBoost = 1.0 + autoRatio * 0.15
      val avgDisruption = if sectorFirms.nonEmpty then
        sectorFirms.map(_.disruption).sum / sectorFirms.length
      else 0.0
      demand * realExRateEffect * sectorAutoBoost * (1.0 - avgDisruption)
    }.toVector
    val totalExports = sectorExports.sum

    // 6. Sector-specific intermediate imports
    val sectorImports = (0 until 6).map { s =>
      val realOutput = if priceLevel > 0 then sectorOutputs(s) / priceLevel
                       else sectorOutputs(s)
      val baseDemand = realOutput * Config.GvcDepth(s)
      val sectorFirms = updatedFirms.filter(_.sectorId == s)
      // Weighted ER pass-through across partners
      val erEffect = if rc.isEurozone then
        // EU partner: zero pass-through (single currency); non-EU: full
        val nonEuFirms = sectorFirms.filter(_.partnerId == 1)
        val euFirms = sectorFirms.filter(_.partnerId == 0)
        val totalSupply = sectorFirms.map(_.baseImportSupply).sum
        if totalSupply > 0 then
          val nonEuWeight = nonEuFirms.map(_.baseImportSupply).sum / totalSupply
          1.0 + nonEuWeight * (exchangeRate / Config.BaseExRate - 1.0) * Config.GvcErPassthrough
        else 1.0
      else
        // PLN: differentiated pass-through by partner
        val totalSupply = sectorFirms.map(_.baseImportSupply).sum
        if totalSupply > 0 then
          val euWeight = sectorFirms.filter(_.partnerId == 0).map(_.baseImportSupply).sum / totalSupply
          val nonEuWeight = 1.0 - euWeight
          val erDeviation = exchangeRate / Config.BaseExRate - 1.0
          1.0 + euWeight * erDeviation * Config.GvcEuErPassthrough +
                nonEuWeight * erDeviation * Config.GvcErPassthrough
        else 1.0

      val avgDisruption = if sectorFirms.nonEmpty then
        sectorFirms.map(_.disruption).sum / sectorFirms.length
      else 0.0
      baseDemand * Math.max(0.1, erEffect) * (1.0 - avgDisruption)
    }.toVector
    val totalIntermImports = sectorImports.sum

    // 7. Metrics
    val weightedDisruption = if updatedFirms.nonEmpty then
      val totalDemand = updatedFirms.map(_.baseExportDemand).sum
      if totalDemand > 0 then
        updatedFirms.map(ff => ff.disruption * ff.baseExportDemand).sum / totalDemand
      else 0.0
    else 0.0

    val importCostIndex = if prev.foreignPriceIndex > 0 then
      newForeignPrice / 1.0  // relative to base
    else 1.0

    GvcState(
      foreignFirms = updatedFirms,
      totalExports = totalExports,
      totalIntermImports = totalIntermImports,
      sectorExports = sectorExports,
      sectorImports = sectorImports,
      disruptionIndex = weightedDisruption,
      foreignPriceIndex = newForeignPrice,
      tradeConcentration = Config.GvcEuTradeShare * Config.GvcEuTradeShare +
        (1.0 - Config.GvcEuTradeShare) * (1.0 - Config.GvcEuTradeShare),
      exportDemandShockMag = shockMag,
      importCostIndex = importCostIndex
    )
