package sfc.engine

import sfc.config.{Config, SECTORS, RunConfig}
import sfc.sfc.{ForexState, BopState}
import sfc.agents.CentralBankLogic

case class OpenEconResult(
  forex: ForexState,
  bop: BopState,
  importedIntermediates: Vector[Double],  // per-sector import cost (6 elements)
  valuationEffect: Double,                // exact valuation effect used in NFA update
  fxIntervention: CentralBankLogic.FxInterventionResult =
    CentralBankLogic.FxInterventionResult(0.0, 0.0, 0.0)
)

object OpenEconomy:

  def step(prevBop: BopState, prevForex: ForexState,
           importCons: Double, techImports: Double,
           autoRatio: Double, domesticRate: Double, gdp: Double,
           priceLevel: Double, sectorOutputs: Vector[Double],
           month: Int, rc: RunConfig,
           nbpFxReserves: Double = 0.0,
           gvcExports: Option[Double] = None,
           gvcIntermImports: Option[Vector[Double]] = None): OpenEconResult =

    val nSectors = 6

    // A. Export demand (structural)
    // Foreign buyers care about the real exchange rate: PL / (ER/baseER).
    // When PL rises but ER depreciates proportionally, real price unchanged.
    // This replaces the separate priceCompetitiveness × erCompetitiveness terms
    // which double-counted and let PL crush exports even when ER compensated.
    val foreignGdpFactor = Math.pow(1.0 + Config.OeForeignGdpGrowth / 12.0, month.toDouble)
    val ulcEffect = 1.0 + autoRatio * Config.OeUlcExportBoost
    val realExRate = if rc.isEurozone then 1.0
      else
        val nominalER = prevForex.exchangeRate / Config.BaseExRate  // >1 when PLN weaker
        val realPrice = if priceLevel > 0 && nominalER > 0 then priceLevel / nominalER
                        else 1.0
        Math.pow(1.0 / Math.max(0.1, realPrice), Config.OeExportPriceElasticity)
    val exports = gvcExports.getOrElse {
      Config.OeExportBase * foreignGdpFactor * realExRate * ulcEffect
    }

    // B. Imported intermediates (cross-border I-O)
    // Intermediate imports = real domestic output × import share × ER net effect.
    // sectorOutputs is nominal (includes priceLevel), so divide by PL to get real output.
    // PLN import bill: quantity × foreign price × ER. With demand elasticity ε:
    //   bill ∝ realOutput × (ER/baseER)^(1-ε).  For ε<1 bill rises with depreciation.
    val erNetEffect = if rc.isEurozone then 1.0
      else Math.pow(prevForex.exchangeRate / Config.BaseExRate,
                    1.0 - Config.OeErElasticity)
    val importedInterm = gvcIntermImports.getOrElse {
      (0 until nSectors).map { s =>
        val realOutput = if priceLevel > 0 then sectorOutputs(s) / priceLevel
                         else sectorOutputs(s)
        realOutput * Config.OeImportContent(s) * erNetEffect
      }.toVector
    }
    val totalImportedInterm = importedInterm.sum

    // C. Total imports
    val totalImports = importCons + techImports + totalImportedInterm

    // D. Trade balance
    val tradeBalance = exports - totalImports

    // E. Current account
    val primaryIncome = prevBop.nfa * Config.OeNfaReturnRate / 12.0
    val secondaryIncome = Config.OeEuTransfers
    val currentAccount = tradeBalance + primaryIncome + secondaryIncome

    // F. Capital account
    val nfaGdpRatio = if gdp > 0 then prevBop.nfa / (gdp * 12.0) else 0.0
    val fdi = if rc.isEurozone then
      Config.OeFdiBase * (1.0 + autoRatio * 0.5) // EUR: more FDI from tech adoption
    else
      Config.OeFdiBase * (1.0 + autoRatio * 0.3) * (1.0 - Math.max(0.0, -nfaGdpRatio) * 0.5)
    val portfolioFlows = if rc.isEurozone then 0.0  // No capital arbitrage in single currency
    else
      val rateDiff = domesticRate - Config.ForeignRate
      val riskPremium = -Config.OeRiskPremiumSensitivity * nfaGdpRatio
      val monthlyGdp = if gdp > 0 then gdp else 1.0
      (rateDiff + riskPremium) * Config.OePortfolioSensitivity * monthlyGdp
    val capitalAccount = fdi + portfolioFlows

    // G. BoP identity: CA + KA + ΔReserves = 0
    val deltaReserves = -(currentAccount + capitalAccount)

    // H. Exchange rate + FX intervention
    val fxResult = CentralBankLogic.fxIntervention(prevForex.exchangeRate, nbpFxReserves, gdp)
    val newExRate = if rc.isEurozone then Config.BaseExRate
    else
      val bopGdpRatio = if gdp > 0 then (currentAccount + capitalAccount) / gdp else 0.0
      val nfaRisk = Config.OeRiskPremiumSensitivity * Math.min(0.0, nfaGdpRatio)
      val erChange = Config.ExRateAdjSpeed * (-bopGdpRatio + nfaRisk) + fxResult.erEffect
      Math.max(Config.OeErFloor,
        Math.min(Config.OeErCeiling, prevForex.exchangeRate * (1.0 + erChange)))

    // I. NFA update
    val valuationEffect = if rc.isEurozone then 0.0
    else
      val erChange = (newExRate - prevForex.exchangeRate) / prevForex.exchangeRate
      prevBop.foreignAssets * erChange * 0.3  // Partial pass-through of ER valuation
    val newNfa = prevBop.nfa + currentAccount + valuationEffect

    // Gross positions update
    val newForeignAssets = prevBop.foreignAssets + Math.max(0.0, capitalAccount)
    val newForeignLiabilities = prevBop.foreignLiabilities + Math.max(0.0, -capitalAccount)

    val newForex = ForexState(newExRate, totalImports, exports, tradeBalance, techImports)

    val newBop = BopState(
      nfa = newNfa,
      foreignAssets = newForeignAssets,
      foreignLiabilities = newForeignLiabilities,
      currentAccount = currentAccount,
      capitalAccount = capitalAccount,
      tradeBalance = tradeBalance,
      primaryIncome = primaryIncome,
      secondaryIncome = secondaryIncome,
      fdi = fdi,
      portfolioFlows = portfolioFlows,
      reserves = prevBop.reserves + deltaReserves,
      exports = exports,
      totalImports = totalImports,
      importedIntermediates = totalImportedInterm
    )

    OpenEconResult(newForex, newBop, importedInterm, valuationEffect, fxResult)
