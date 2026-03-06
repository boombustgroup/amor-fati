package sfc.engine

import sfc.config.{Config, RunConfig}
import sfc.types.*
import sfc.util.KahanSum.*

case class ForeignFirm(
  id: Int,
  sectorId: Int,
  partnerId: Int, // 0=EU, 1=Non-EU
  baseExportDemand: PLN,
  baseImportSupply: PLN,
  priceIndex: Double,
  disruption: Ratio,
)

object GvcTrade:

  case class State(
    foreignFirms: Vector[ForeignFirm],
    totalExports: PLN = PLN.Zero,
    totalIntermImports: PLN = PLN.Zero,
    sectorExports: Vector[PLN] = Vector.fill(6)(PLN.Zero),
    sectorImports: Vector[PLN] = Vector.fill(6)(PLN.Zero),
    disruptionIndex: Ratio = Ratio.Zero,
    foreignPriceIndex: Double = 1.0,
    tradeConcentration: Ratio = Ratio.Zero,
    exportDemandShockMag: Ratio = Ratio.Zero,
    importCostIndex: Double = 1.0,
  )

  def zero: State = State(Vector.empty)

  def initial: State =
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
        baseExportDemand = PLN(Config.OeExportBase * Config.GvcExportShares(s) * ps),
        baseImportSupply = PLN(Config.OeExportBase * Config.GvcDepth(s) * ps),
        priceIndex = 1.0,
        disruption = Ratio.Zero,
      )

    State(firms, foreignPriceIndex = 1.0, tradeConcentration = Ratio(euShare * euShare + nonEuShare * nonEuShare))

  def step(
    prev: State,
    sectorOutputs: Vector[Double],
    priceLevel: Double,
    exchangeRate: Double,
    autoRatio: Double,
    month: Int,
    rc: RunConfig,
  ): State =

    // 1. Evolve foreign price
    val newForeignPrice = prev.foreignPriceIndex * (1.0 + Config.GvcForeignInflation / 12.0)

    // 2. Apply demand shock + recover disruptions
    val shockActive = Config.GvcDemandShockMonth > 0 && month >= Config.GvcDemandShockMonth
    val shockMag = if shockActive then Config.GvcDemandShockSize else 0.0

    val updatedFirms = prev.foreignFirms.map { ff =>
      val afterShock =
        if shockActive && month == Config.GvcDemandShockMonth &&
          Config.GvcDemandShockSectors.contains(ff.sectorId)
        then ff.copy(baseExportDemand = ff.baseExportDemand * (1.0 - Config.GvcDemandShockSize))
        else ff

      // Recover disruptions
      val newDisruption = Ratio(afterShock.disruption.toDouble * (1.0 - Config.GvcDisruptionRecovery))
      // Evolve price
      val newPrice = afterShock.priceIndex * (1.0 + Config.GvcForeignInflation / 12.0)
      afterShock.copy(disruption = newDisruption, priceIndex = newPrice)
    }

    // 3. Foreign GDP growth factor
    val foreignGdpFactor = Math.pow(1.0 + Config.GvcForeignGdpGrowth / 12.0, month.toDouble)

    // 4. Real exchange rate effect (same formula as OpenEconomy)
    val realExRateEffect =
      if rc.isEurozone then 1.0
      else
        val nominalER = exchangeRate / Config.BaseExRate
        val realPrice = if priceLevel > 0 && nominalER > 0 then priceLevel / nominalER else 1.0
        Math.pow(1.0 / Math.max(0.1, realPrice), Config.OeExportPriceElasticity)

    // 5. Sector-specific exports
    val sectorExports = (0 until 6).map { s =>
      val sectorFirms = updatedFirms.filter(_.sectorId == s)
      val demand = sectorFirms.kahanSumBy(_.baseExportDemand.toDouble) * foreignGdpFactor
      // Per-sector automation ratio from sectorOutputs (proxy)
      val sectorAutoBoost = 1.0 + autoRatio * 0.15
      val avgDisruption =
        if sectorFirms.nonEmpty then sectorFirms.kahanSumBy(_.disruption.toDouble) / sectorFirms.length
        else 0.0
      PLN(demand * realExRateEffect * sectorAutoBoost * (1.0 - avgDisruption))
    }.toVector
    val totalExports = PLN(sectorExports.map(_.toDouble).kahanSum)

    // 6. Sector-specific intermediate imports
    val sectorImports = (0 until 6).map { s =>
      val realOutput =
        if priceLevel > 0 then sectorOutputs(s) / priceLevel
        else sectorOutputs(s)
      val baseDemand = realOutput * Config.GvcDepth(s)
      val sectorFirms = updatedFirms.filter(_.sectorId == s)
      // Weighted ER pass-through across partners
      val erEffect = if rc.isEurozone then
        // EU partner: zero pass-through (single currency); non-EU: full
        val nonEuFirms = sectorFirms.filter(_.partnerId == 1)
        val totalSupply = sectorFirms.kahanSumBy(_.baseImportSupply.toDouble)
        if totalSupply > 0 then
          val nonEuWeight = nonEuFirms.kahanSumBy(_.baseImportSupply.toDouble) / totalSupply
          1.0 + nonEuWeight * (exchangeRate / Config.BaseExRate - 1.0) * Config.GvcErPassthrough
        else 1.0
      else
        // PLN: differentiated pass-through by partner
        val totalSupply = sectorFirms.kahanSumBy(_.baseImportSupply.toDouble)
        if totalSupply > 0 then
          val euWeight = sectorFirms.filter(_.partnerId == 0).kahanSumBy(_.baseImportSupply.toDouble) / totalSupply
          val nonEuWeight = 1.0 - euWeight
          val erDeviation = exchangeRate / Config.BaseExRate - 1.0
          1.0 + euWeight * erDeviation * Config.GvcEuErPassthrough +
            nonEuWeight * erDeviation * Config.GvcErPassthrough
        else 1.0

      val avgDisruption =
        if sectorFirms.nonEmpty then sectorFirms.kahanSumBy(_.disruption.toDouble) / sectorFirms.length
        else 0.0
      PLN(baseDemand * Math.max(0.1, erEffect) * (1.0 - avgDisruption))
    }.toVector
    val totalIntermImports = PLN(sectorImports.map(_.toDouble).kahanSum)

    // 7. Metrics
    val weightedDisruption = if updatedFirms.nonEmpty then
      val totalDemand = updatedFirms.kahanSumBy(_.baseExportDemand.toDouble)
      if totalDemand > 0 then
        updatedFirms.kahanSumBy(ff => ff.disruption.toDouble * ff.baseExportDemand.toDouble) / totalDemand
      else 0.0
    else 0.0

    val importCostIndex =
      if prev.foreignPriceIndex > 0 then newForeignPrice / 1.0 // relative to base
      else 1.0

    State(
      foreignFirms = updatedFirms,
      totalExports = totalExports,
      totalIntermImports = totalIntermImports,
      sectorExports = sectorExports,
      sectorImports = sectorImports,
      disruptionIndex = Ratio(weightedDisruption),
      foreignPriceIndex = newForeignPrice,
      tradeConcentration = Ratio(
        Config.GvcEuTradeShare * Config.GvcEuTradeShare +
          (1.0 - Config.GvcEuTradeShare) * (1.0 - Config.GvcEuTradeShare),
      ),
      exportDemandShockMag = Ratio(shockMag),
      importCostIndex = importCostIndex,
    )
