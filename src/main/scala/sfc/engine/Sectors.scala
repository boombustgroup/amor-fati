package sfc.engine

import sfc.accounting.*
import sfc.config.*
import sfc.types.*

object Sectors:
  private def laborSupplyRatio(wage: Double, resWage: Double): Double =
    val x = Config.LaborSupplySteepness * (wage / resWage - 1.0)
    1.0 / (1.0 + Math.exp(-x))

  def updateLaborMarket(prevWage: Double, resWage: Double, laborDemand: Int): (Double, Int) =
    val supplyAtPrev = (Config.TotalPopulation * laborSupplyRatio(prevWage, resWage)).toInt
    val excessDemand = (laborDemand - supplyAtPrev).toDouble / Config.TotalPopulation
    val wageGrowth = excessDemand * Config.WageAdjSpeed
    val newWage = Math.max(resWage, prevWage * (1.0 + wageGrowth))
    val newSupply = (Config.TotalPopulation * laborSupplyRatio(newWage, resWage)).toInt
    val employed = Math.min(laborDemand, newSupply)
    (newWage, employed)

  def updateInflation(
    prevInflation: Double,
    prevPrice: Double,
    demandMult: Double,
    wageGrowth: Double,
    exRateDeviation: Double,
    autoRatio: Double,
    hybridRatio: Double,
    rc: RunConfig,
  ): (Double, Double) =
    val demandPull = (demandMult - 1.0) * 0.15
    val costPush = wageGrowth * 0.25
    // EUR: no exchange rate pass-through (single currency area)
    val rawImportPush =
      if rc.isEurozone then 0.0
      else Math.max(0.0, exRateDeviation) * Config.ImportPropensity * 0.25
    val importPush =
      if Config.OeEnabled then Math.min(rawImportPush, Config.OeImportPushCap)
      else rawImportPush
    val techDeflation = autoRatio * 0.060 + hybridRatio * 0.018
    // Soft floor: beyond -1.5%/month, deflation passes through at 30% rate
    // (models downward price stickiness -- Bewley 1999, Schmitt-Grohe & Uribe 2016)
    val rawMonthly = demandPull + costPush + importPush - techDeflation
    val monthly =
      if rawMonthly >= -0.015 then rawMonthly
      else -0.015 + (rawMonthly + 0.015) * 0.3
    val annualized = monthly * 12.0
    val smoothed = prevInflation * 0.7 + annualized * 0.3
    val newPrice = Math.max(0.30, prevPrice * (1.0 + monthly))
    (smoothed, newPrice)

  def updateCbRate(prevRate: Double, inflation: Double, exRateChange: Double, employed: Int, rc: RunConfig): Double =
    if rc.isEurozone then
      // ECB Taylor rule reacting to Eurozone-wide inflation (exogenous to Poland)
      val infGap = Config.EuroInflation - Config.EcbTargetInfl
      val taylor = Config.EcbNeutralRate +
        Config.EcbAlpha * Math.max(0.0, infGap)
      val smoothed = prevRate * Config.EcbInertia + taylor * (1.0 - Config.EcbInertia)
      Math.max(Config.RateFloor, Math.min(Config.RateCeiling, smoothed))
    else if Config.NbpSymmetric then
      // v2.0: Symmetric Taylor + output gap (dual mandate)
      val infGap = inflation - Config.NbpTargetInfl
      val unempRate = 1.0 - (employed.toDouble / Config.TotalPopulation)
      val rawOutputGap = (unempRate - Config.NbpNairu) / Config.NbpNairu
      // Cap output gap at ±0.30 — prevents extreme Taylor responses from initial tight labor
      // market while preserving dual-mandate stabilization (Svensson 2003)
      val outputGap = Math.max(-0.30, Math.min(0.30, rawOutputGap))
      val taylor = Config.NbpNeutralRate +
        Config.TaylorAlpha * infGap -
        Config.TaylorDelta * outputGap +
        Config.TaylorBeta * exRateChange
      val smoothed = prevRate * Config.TaylorInertia + taylor * (1.0 - Config.TaylorInertia)
      val effective =
        if Config.NbpMaxRateChange > 0 then
          prevRate + Math.max(-Config.NbpMaxRateChange, Math.min(Config.NbpMaxRateChange, smoothed - prevRate))
        else smoothed
      Math.max(Config.RateFloor, Math.min(Config.RateCeiling, effective))
    else
      // v1.0 legacy: asymmetric Taylor (inflation-only)
      val infGap = inflation - Config.NbpTargetInfl
      val taylor = Config.NbpNeutralRate +
        Config.TaylorAlpha * Math.max(0.0, infGap) +
        Config.TaylorBeta * Math.max(0.0, exRateChange)
      val smoothed = prevRate * Config.TaylorInertia + taylor * (1.0 - Config.TaylorInertia)
      val effective =
        if Config.NbpMaxRateChange > 0 then
          prevRate + Math.max(-Config.NbpMaxRateChange, Math.min(Config.NbpMaxRateChange, smoothed - prevRate))
        else smoothed
      Math.max(Config.RateFloor, Math.min(Config.RateCeiling, effective))

  def updateForeign(
    prev: ForexState,
    importConsumption: Double,
    techImports: Double,
    autoRatio: Double,
    domesticRate: Double,
    gdp: Double,
    rc: RunConfig,
  ): ForexState =
    val techComp = 1.0 + autoRatio * Config.ExportAutoBoost
    val totalImp = importConsumption + techImports
    if rc.isEurozone then
      // EUR: fixed exchange rate, exports depend on relative price competitiveness
      // No capital account arbitrage (single monetary zone)
      val exports = Config.ExportBase * techComp
      val tradeBal = exports - totalImp
      ForexState(Config.BaseExRate, PLN(totalImp), PLN(exports), PLN(tradeBal), PLN(techImports))
    else
      // PLN: floating exchange rate, BoP-driven adjustment
      val exComp = prev.exchangeRate / Config.BaseExRate
      val exports = Config.ExportBase * exComp * techComp
      val tradeBal = exports - totalImp
      val rateDiff = domesticRate - Config.ForeignRate
      val capAcct = rateDiff * Config.IrpSensitivity * gdp
      val bop = tradeBal + capAcct
      val bopRatio = if gdp > 0 then bop / gdp else 0.0
      val exRateChg = -Config.ExRateAdjSpeed * bopRatio
      val newRate = Math.max(3.0, Math.min(8.0, prev.exchangeRate * (1.0 + exRateChg)))
      ForexState(newRate, PLN(totalImp), PLN(exports), PLN(tradeBal), PLN(techImports))

  def updateGov(
    prev: GovState,
    citPaid: Double,
    vat: Double,
    bdpActive: Boolean,
    bdpAmount: Double,
    priceLevel: Double,
    unempBenefitSpend: Double,
    debtService: Double = 0.0,
    nbpRemittance: Double = 0.0,
    zusGovSubvention: Double = 0.0,
    socialTransferSpend: Double = 0.0,
    euCofinancing: Double = 0.0,
    euProjectCapital: Double = 0.0,
    exciseRevenue: Double = 0.0,
    customsDutyRevenue: Double = 0.0,
    govPurchasesActual: Double = 0.0,
  ): GovState =
    val bdpSpend = if bdpActive then Config.TotalPopulation.toDouble * bdpAmount else 0.0
    val govBaseRaw =
      if govPurchasesActual > 0 then govPurchasesActual
      else Config.GovBaseSpending * priceLevel
    val (govCurrent, govCapital) =
      if Config.GovInvestEnabled then (govBaseRaw * (1.0 - Config.GovInvestShare), govBaseRaw * Config.GovInvestShare)
      else (govBaseRaw, 0.0)
    val totalSpend =
      bdpSpend + unempBenefitSpend + socialTransferSpend + govCurrent + govCapital + debtService + zusGovSubvention + euCofinancing
    val totalRev = citPaid + vat + nbpRemittance + exciseRevenue + customsDutyRevenue
    val deficit = totalSpend - totalRev
    val newBondsOutstanding =
      if Config.GovBondMarket then Math.max(0.0, prev.bondsOutstanding.toDouble + deficit)
      else prev.bondsOutstanding.toDouble
    val newCapitalStock =
      if Config.GovInvestEnabled then
        prev.publicCapitalStock.toDouble * (1.0 - Config.GovDepreciationRate / 12.0) + govCapital + euProjectCapital
      else 0.0
    GovState(
      bdpActive,
      PLN(totalRev),
      PLN(bdpSpend),
      PLN(deficit),
      PLN(prev.cumulativeDebt.toDouble + deficit),
      PLN(unempBenefitSpend),
      PLN(newBondsOutstanding),
      prev.bondYield,
      PLN(debtService),
      PLN(socialTransferSpend),
      PLN(newCapitalStock),
      PLN(govCurrent),
      PLN(govCapital + euProjectCapital),
      PLN(euCofinancing),
      PLN(exciseRevenue),
      PLN(customsDutyRevenue),
    )
