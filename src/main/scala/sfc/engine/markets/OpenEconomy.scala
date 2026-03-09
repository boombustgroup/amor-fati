package sfc.engine.markets

import sfc.accounting.{BopState, ForexState}
import sfc.agents.Nbp
import sfc.config.{RunConfig, SectorDefs, SimParams}
import sfc.types.PLN
import sfc.util.KahanSum.*

object OpenEconomy:

  def updateForeign(
      prev: ForexState,
      importConsumption: Double,
      techImports: Double,
      autoRatio: Double,
      domesticRate: Double,
      gdp: Double,
      rc: RunConfig,
  )(using p: SimParams): ForexState =
    val techComp  = 1.0 + autoRatio * p.forex.exportAutoBoost.toDouble
    val totalImp  = importConsumption + techImports
    // PLN: floating exchange rate, BoP-driven adjustment
    val exComp    = prev.exchangeRate / p.forex.baseExRate
    val exports   = p.forex.exportBase.toDouble * exComp * techComp
    val tradeBal  = exports - totalImp
    val rateDiff  = domesticRate - p.forex.foreignRate.toDouble
    val capAcct   = rateDiff * p.forex.irpSensitivity * gdp
    val bop       = tradeBal + capAcct
    val bopRatio  = if gdp > 0 then bop / gdp else 0.0
    val exRateChg = -p.forex.exRateAdjSpeed.toDouble * bopRatio
    val newRate   = Math.max(3.0, Math.min(8.0, prev.exchangeRate * (1.0 + exRateChg)))
    ForexState(newRate, PLN(totalImp), PLN(exports), PLN(tradeBal), PLN(techImports))

  case class Result(
      forex: ForexState,
      bop: BopState,
      importedIntermediates: Vector[Double], // per-sector import cost (6 elements)
      valuationEffect: Double,               // exact valuation effect used in NFA update
      fxIntervention: Nbp.FxInterventionResult = Nbp.FxInterventionResult(0.0, 0.0, 0.0),
  )

  def step(
      prevBop: BopState,
      prevForex: ForexState,
      importCons: Double,
      techImports: Double,
      autoRatio: Double,
      domesticRate: Double,
      gdp: Double,
      priceLevel: Double,
      sectorOutputs: Vector[Double],
      month: Int,
      rc: RunConfig,
      nbpFxReserves: Double = 0.0,
      gvcExports: Option[Double] = None,
      gvcIntermImports: Option[Vector[Double]] = None,
      remittanceOutflow: Double = 0.0,
      euFundsMonthly: Double = 0.0,
      diasporaInflow: Double = 0.0,
      tourismExport: Double = 0.0,
      tourismImport: Double = 0.0,
  )(using p: SimParams): Result =

    val nSectors = SectorDefs.length

    // A. Export demand (structural)
    // Foreign buyers care about the real exchange rate: PL / (ER/baseER).
    // When PL rises but ER depreciates proportionally, real price unchanged.
    // This replaces the separate priceCompetitiveness × erCompetitiveness terms
    // which double-counted and let PL crush exports even when ER compensated.
    val foreignGdpFactor       = Math.pow(1.0 + p.openEcon.foreignGdpGrowth.toDouble / 12.0, month.toDouble)
    val ulcEffect              = 1.0 + autoRatio * p.openEcon.ulcExportBoost
    val realExRate             =
      val nominalER = prevForex.exchangeRate / p.forex.baseExRate // >1 when PLN weaker
      val realPrice =
        if priceLevel > 0 && nominalER > 0 then priceLevel / nominalER
        else 1.0
      Math.pow(1.0 / Math.max(0.1, realPrice), p.openEcon.exportPriceElasticity)
    val exports                = gvcExports.getOrElse {
      p.openEcon.exportBase.toDouble * foreignGdpFactor * realExRate * ulcEffect
    }
    val totalExportsIncTourism = exports + tourismExport

    // B. Imported intermediates (cross-border I-O)
    // Intermediate imports = real domestic output × import share × ER net effect.
    // sectorOutputs is nominal (includes priceLevel), so divide by PL to get real output.
    // PLN import bill: quantity × foreign price × ER. With demand elasticity ε:
    //   bill ∝ realOutput × (ER/baseER)^(1-ε).  For ε<1 bill rises with depreciation.
    val erNetEffect         =
      Math.pow(prevForex.exchangeRate / p.forex.baseExRate, 1.0 - p.openEcon.erElasticity)
    val importedInterm      = gvcIntermImports.getOrElse {
      (0 until nSectors).map { s =>
        val realOutput =
          if priceLevel > 0 then sectorOutputs(s) / priceLevel
          else sectorOutputs(s)
        realOutput * p.openEcon.importContent.map(_.toDouble)(s) * erNetEffect
      }.toVector
    }
    val totalImportedInterm = importedInterm.kahanSum

    // C. Total imports
    val totalImports = importCons + techImports + totalImportedInterm + tourismImport

    // D. Trade balance
    val tradeBalance = totalExportsIncTourism - totalImports

    // E. Current account
    val primaryIncome   = prevBop.nfa.toDouble * p.openEcon.nfaReturnRate.toDouble / 12.0
    val secondaryIncome = euFundsMonthly - remittanceOutflow + diasporaInflow
    val currentAccount  = tradeBalance + primaryIncome + secondaryIncome

    // F. Capital account
    val nfaGdpRatio    = if gdp > 0 then prevBop.nfa.toDouble / (gdp * 12.0) else 0.0
    val fdi            = p.openEcon.fdiBase.toDouble * (1.0 + autoRatio * 0.3) * (1.0 - Math.max(0.0, -nfaGdpRatio) * 0.5)
    val portfolioFlows =
      val rateDiff    = domesticRate - p.forex.foreignRate.toDouble
      val riskPremium = -p.openEcon.riskPremiumSensitivity * nfaGdpRatio
      val monthlyGdp  = if gdp > 0 then gdp else 1.0
      (rateDiff + riskPremium) * p.openEcon.portfolioSensitivity * monthlyGdp
    val capitalAccount = fdi + portfolioFlows

    // G. BoP identity: CA + KA + ΔReserves = 0
    val deltaReserves = -(currentAccount + capitalAccount)

    // H. Exchange rate + FX intervention
    val fxResult  = Nbp.fxIntervention(prevForex.exchangeRate, nbpFxReserves, gdp, p.flags.nbpFxIntervention)
    val newExRate =
      val bopGdpRatio = if gdp > 0 then (currentAccount + capitalAccount) / gdp else 0.0
      val nfaRisk     = p.openEcon.riskPremiumSensitivity * Math.min(0.0, nfaGdpRatio)
      val erChange    = p.forex.exRateAdjSpeed.toDouble * (-bopGdpRatio + nfaRisk) + fxResult.erEffect
      Math.max(p.openEcon.erFloor, Math.min(p.openEcon.erCeiling, prevForex.exchangeRate * (1.0 + erChange)))

    // I. NFA update
    val valuationEffect =
      val erChange = (newExRate - prevForex.exchangeRate) / prevForex.exchangeRate
      prevBop.foreignAssets.toDouble * erChange * 0.3 // Partial pass-through of ER valuation
    val newNfa          = prevBop.nfa.toDouble + currentAccount + valuationEffect

    // Gross positions update
    val newForeignAssets      = prevBop.foreignAssets.toDouble + Math.max(0.0, capitalAccount)
    val newForeignLiabilities = prevBop.foreignLiabilities.toDouble + Math.max(0.0, -capitalAccount)

    val newForex =
      ForexState(newExRate, PLN(totalImports), PLN(totalExportsIncTourism), PLN(tradeBalance), PLN(techImports))

    val newBop = BopState(
      nfa = PLN(newNfa),
      foreignAssets = PLN(newForeignAssets),
      foreignLiabilities = PLN(newForeignLiabilities),
      currentAccount = PLN(currentAccount),
      capitalAccount = PLN(capitalAccount),
      tradeBalance = PLN(tradeBalance),
      primaryIncome = PLN(primaryIncome),
      secondaryIncome = PLN(secondaryIncome),
      fdi = PLN(fdi),
      portfolioFlows = PLN(portfolioFlows),
      reserves = PLN(prevBop.reserves.toDouble + deltaReserves),
      exports = PLN(totalExportsIncTourism),
      totalImports = PLN(totalImports),
      importedIntermediates = PLN(totalImportedInterm),
    )

    Result(newForex, newBop, importedInterm, valuationEffect, fxResult)
