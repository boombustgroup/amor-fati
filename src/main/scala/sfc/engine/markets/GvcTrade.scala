package sfc.engine.markets

import sfc.config.{RunConfig, SimParams}
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

  def initial(using p: SimParams): State =
    val euShare       = p.gvc.euTradeShare.toDouble
    val nonEuShare    = 1.0 - euShare
    val partnerShares = Vector(euShare, nonEuShare)

    val firms = for
      s  <- (0 until 6).toVector
      pi <- (0 until 2).toVector
    yield
      val ps = partnerShares(pi)
      ForeignFirm(
        id = s * 2 + pi,
        sectorId = s,
        partnerId = pi,
        baseExportDemand = PLN(p.openEcon.exportBase.toDouble * p.gvc.exportShares.map(_.toDouble)(s) * ps),
        baseImportSupply = PLN(p.openEcon.exportBase.toDouble * p.gvc.depth.map(_.toDouble)(s) * ps),
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
  )(using p: SimParams): State =

    // 1. Evolve foreign price
    val newForeignPrice = prev.foreignPriceIndex * (1.0 + p.gvc.foreignInflation.toDouble / 12.0)

    // 2. Apply demand shock + recover disruptions
    val shockActive = p.gvc.demandShockMonth > 0 && month >= p.gvc.demandShockMonth
    val shockMag    = if shockActive then p.gvc.demandShockSize.toDouble else 0.0

    val updatedFirms = prev.foreignFirms.map { ff =>
      val afterShock =
        if shockActive && month == p.gvc.demandShockMonth &&
          p.gvc.demandShockSectors.contains(ff.sectorId)
        then ff.copy(baseExportDemand = ff.baseExportDemand * (1.0 - p.gvc.demandShockSize.toDouble))
        else ff

      // Recover disruptions
      val newDisruption = Ratio(afterShock.disruption.toDouble * (1.0 - p.gvc.disruptionRecovery.toDouble))
      // Evolve price
      val newPrice      = afterShock.priceIndex * (1.0 + p.gvc.foreignInflation.toDouble / 12.0)
      afterShock.copy(disruption = newDisruption, priceIndex = newPrice)
    }

    // 3. Foreign GDP growth factor
    val foreignGdpFactor = Math.pow(1.0 + p.gvc.foreignGdpGrowth.toDouble / 12.0, month.toDouble)

    // 4. Real exchange rate effect (same formula as OpenEconomy)
    val realExRateEffect =
      val nominalER = exchangeRate / p.forex.baseExRate
      val realPrice = if priceLevel > 0 && nominalER > 0 then priceLevel / nominalER else 1.0
      Math.pow(1.0 / Math.max(0.1, realPrice), p.openEcon.exportPriceElasticity)

    // 5. Sector-specific exports
    val sectorExports = (0 until 6).map { s =>
      val sectorFirms     = updatedFirms.filter(_.sectorId == s)
      val demand          = sectorFirms.kahanSumBy(_.baseExportDemand.toDouble) * foreignGdpFactor
      // Per-sector automation ratio from sectorOutputs (proxy)
      val sectorAutoBoost = 1.0 + autoRatio * 0.15
      val avgDisruption   =
        if sectorFirms.nonEmpty then sectorFirms.kahanSumBy(_.disruption.toDouble) / sectorFirms.length
        else 0.0
      PLN(demand * realExRateEffect * sectorAutoBoost * (1.0 - avgDisruption))
    }.toVector
    val totalExports  = PLN(sectorExports.map(_.toDouble).kahanSum)

    // 6. Sector-specific intermediate imports
    val sectorImports      = (0 until 6).map { s =>
      val realOutput  =
        if priceLevel > 0 then sectorOutputs(s) / priceLevel
        else sectorOutputs(s)
      val baseDemand  = realOutput * p.gvc.depth.map(_.toDouble)(s)
      val sectorFirms = updatedFirms.filter(_.sectorId == s)
      // Weighted ER pass-through across partners
      // PLN: differentiated pass-through by partner
      val erEffect    =
        val totalSupply = sectorFirms.kahanSumBy(_.baseImportSupply.toDouble)
        if totalSupply > 0 then
          val euWeight    = sectorFirms.filter(_.partnerId == 0).kahanSumBy(_.baseImportSupply.toDouble) / totalSupply
          val nonEuWeight = 1.0 - euWeight
          val erDeviation = exchangeRate / p.forex.baseExRate - 1.0
          1.0 + euWeight * erDeviation * p.gvc.euErPassthrough.toDouble +
            nonEuWeight * erDeviation * p.gvc.erPassthrough.toDouble
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
      if totalDemand > 0 then updatedFirms.kahanSumBy(ff => ff.disruption.toDouble * ff.baseExportDemand.toDouble) / totalDemand
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
        p.gvc.euTradeShare.toDouble * p.gvc.euTradeShare.toDouble +
          (1.0 - p.gvc.euTradeShare.toDouble) * (1.0 - p.gvc.euTradeShare.toDouble),
      ),
      exportDemandShockMag = Ratio(shockMag),
      importCostIndex = importCostIndex,
    )
