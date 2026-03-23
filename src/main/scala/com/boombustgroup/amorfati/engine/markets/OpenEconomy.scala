package com.boombustgroup.amorfati.engine.markets

import com.boombustgroup.amorfati.agents.Nbp
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

/** Open economy: trade, BoP, exchange rate, NFA dynamics.
  *
  * Structural trade model with real exchange rate elasticity (Marshall-Lerner),
  * imported intermediates via per-sector import content, and ULC-driven export
  * competitiveness. BoP identity: CA + KA + ΔReserves = 0.
  *
  * Exchange rate: floating, BoP-driven adjustment with NFA risk premium. FDI:
  * base flow with automation boost and NFA dampening. Portfolio flows: interest
  * rate differential + risk premium. NFA: CA + valuation effect (partial ER
  * pass-through on foreign assets).
  *
  * Calibration: NBP BoP statistics 2024, GUS national accounts.
  */
object OpenEconomy:

  // ---------------------------------------------------------------------------
  // State types
  // ---------------------------------------------------------------------------

  case class ForexState(
      exchangeRate: Double,
      imports: PLN,
      exports: PLN,
      tradeBalance: PLN,
      techImports: PLN,
  )

  case class BopState(
      nfa: PLN,
      foreignAssets: PLN,
      foreignLiabilities: PLN,
      currentAccount: PLN,
      capitalAccount: PLN,
      tradeBalance: PLN,
      primaryIncome: PLN,
      secondaryIncome: PLN,
      fdi: PLN,
      portfolioFlows: PLN,
      reserves: PLN,
      exports: PLN,
      totalImports: PLN,
      importedIntermediates: PLN,
      euFundsMonthly: PLN = PLN.Zero,
      euCumulativeAbsorption: PLN = PLN.Zero,
      carryTradeStock: PLN = PLN.Zero,
  )
  object BopState:
    val zero: BopState = BopState(
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
    )

  // --- Named constants ---
  private val MonthsPerYear        = 12.0
  private val MinRealPrice         = 0.1
  private val FdiAutoBoost         = 0.3
  private val FdiNfaDampening      = 0.5
  private val ValuationPassThrough = 0.3

  @boundaryEscape
  def updateForeign(
      prev: ForexState,
      importConsumption: PLN,
      techImports: PLN,
      autoRatio: Share,
      domesticRate: Rate,
      gdp: PLN,
  )(using p: SimParams): ForexState =
    import ComputationBoundary.toDouble
    val techComp  = 1.0 + toDouble(autoRatio) * toDouble(p.forex.exportAutoBoost)
    val totalImp  = importConsumption + techImports
    val exComp    = prev.exchangeRate / p.forex.baseExRate
    val exports   = p.forex.exportBase * Multiplier(exComp * techComp)
    val tradeBal  = exports - totalImp
    val rateDiff  = toDouble(domesticRate - p.forex.foreignRate)
    val capAcct   = PLN(toDouble(gdp) * rateDiff * toDouble(p.forex.irpSensitivity))
    val bop       = tradeBal + capAcct
    val bopRatio  = if gdp > PLN.Zero then bop / gdp else 0.0
    val exRateChg = -toDouble(p.forex.exRateAdjSpeed) * bopRatio
    val newRate   = Math.max(3.0, Math.min(8.0, prev.exchangeRate * (1.0 + exRateChg)))
    ForexState(newRate, totalImp, exports, tradeBal, techImports)

  case class Result(
      forex: ForexState,
      bop: BopState,
      importedIntermediates: Vector[PLN],
      valuationEffect: PLN,
      fxIntervention: Nbp.FxInterventionResult = Nbp.FxInterventionResult(0.0, PLN.Zero, PLN.Zero, PLN.Zero),
  )

  case class StepInput(
      prevBop: BopState,
      prevForex: ForexState,
      importCons: PLN,
      techImports: PLN,
      autoRatio: Share,
      domesticRate: Rate,
      gdp: PLN,
      priceLevel: Double,
      sectorOutputs: Vector[PLN],
      month: Int,
      inflation: Rate = Rate.Zero,
      nbpFxReserves: PLN = PLN.Zero,
      gvcExports: Option[PLN] = None,
      gvcIntermImports: Option[Vector[PLN]] = None,
      remittanceOutflow: PLN = PLN.Zero,
      euFundsMonthly: PLN = PLN.Zero,
      diasporaInflow: PLN = PLN.Zero,
      tourismExport: PLN = PLN.Zero,
      tourismImport: PLN = PLN.Zero,
      bondYield: Rate = Rate.Zero,
      prevBidToCover: Multiplier = Multiplier(2.0),
  )

  private case class CurrentAccountResult(ca: PLN, primaryIncome: PLN, secondaryIncome: PLN)

  private case class CapitalAccountResult(
      total: PLN,
      fdi: PLN,
      portfolioFlows: PLN,
      carryTradeStock: PLN = PLN.Zero,
  )

  def step(in: StepInput)(using p: SimParams): Result =
    val exports             = computeExports(in)
    val totalExportsIncTour = exports + in.tourismExport
    val importedInterm      = computeImportedIntermediates(in)
    val totalImportedInterm = PLN.fromRaw(importedInterm.map(_.toLong).sum)
    val totalImports        = in.importCons + in.techImports + totalImportedInterm + in.tourismImport
    val tradeBalance        = totalExportsIncTour - totalImports
    val caResult            = computeCurrentAccount(in, tradeBalance)
    val kaResult            = computeCapitalAccount(in)
    val deltaReserves       = -(caResult.ca + kaResult.total)
    val fxResult            = Nbp.fxIntervention(
      in.prevForex.exchangeRate,
      ComputationBoundary.toDouble(in.nbpFxReserves),
      ComputationBoundary.toDouble(in.gdp),
      p.flags.nbpFxIntervention,
    )
    val newExRate           = computeExchangeRate(in, caResult.ca, kaResult.total, fxResult.erEffect)
    val valEffect           = computeValuationEffect(in.prevBop, in.prevForex.exchangeRate, newExRate)
    val newNfa              = in.prevBop.nfa + caResult.ca + valEffect

    val newForex = ForexState(newExRate, totalImports, totalExportsIncTour, tradeBalance, in.techImports)

    val newBop = BopState(
      nfa = newNfa,
      foreignAssets = in.prevBop.foreignAssets + kaResult.total.max(PLN.Zero),
      foreignLiabilities = in.prevBop.foreignLiabilities + (-kaResult.total).max(PLN.Zero),
      currentAccount = caResult.ca,
      capitalAccount = kaResult.total,
      tradeBalance = tradeBalance,
      primaryIncome = caResult.primaryIncome,
      secondaryIncome = caResult.secondaryIncome,
      fdi = kaResult.fdi,
      portfolioFlows = kaResult.portfolioFlows,
      reserves = in.prevBop.reserves + deltaReserves,
      exports = totalExportsIncTour,
      totalImports = totalImports,
      importedIntermediates = totalImportedInterm,
      carryTradeStock = kaResult.carryTradeStock,
    )

    Result(newForex, newBop, importedInterm, valEffect, fxResult)

  // --- Private helpers ---

  @boundaryEscape
  private def computeExports(in: StepInput)(using p: SimParams): PLN =
    import ComputationBoundary.toDouble
    in.gvcExports.getOrElse:
      val foreignGdpFactor = Math.pow(1.0 + toDouble(p.openEcon.foreignGdpGrowth) / MonthsPerYear, in.month.toDouble)
      val ulcEffect        = 1.0 + toDouble(in.autoRatio) * toDouble(p.openEcon.ulcExportBoost)
      val realExRate       = realExchangeRateEffect(in.prevForex.exchangeRate, in.priceLevel)
      p.openEcon.exportBase * Multiplier(foreignGdpFactor * realExRate * ulcEffect)

  @boundaryEscape
  private def computeImportedIntermediates(in: StepInput)(using p: SimParams): Vector[PLN] =
    import ComputationBoundary.toDouble
    in.gvcIntermImports.getOrElse:
      val nSectors    = p.sectorDefs.length
      val erNetEffect = Math.pow(in.prevForex.exchangeRate / p.forex.baseExRate, 1.0 - toDouble(p.openEcon.erElasticity))
      (0 until nSectors)
        .map: s =>
          val realOutput = if in.priceLevel > 0 then PLN(toDouble(in.sectorOutputs(s)) / in.priceLevel) else in.sectorOutputs(s)
          realOutput * p.openEcon.importContent(s) * Multiplier(erNetEffect)
        .toVector

  private def computeCurrentAccount(in: StepInput, tradeBalance: PLN)(using p: SimParams): CurrentAccountResult =
    val primaryIncome   = in.prevBop.nfa * p.openEcon.nfaReturnRate.monthly
    val secondaryIncome = in.euFundsMonthly - in.remittanceOutflow + in.diasporaInflow
    val ca              = tradeBalance + primaryIncome + secondaryIncome
    CurrentAccountResult(ca, primaryIncome, secondaryIncome)

  @boundaryEscape
  private def computeCapitalAccount(in: StepInput)(using p: SimParams): CapitalAccountResult =
    import ComputationBoundary.toDouble
    val annualGdp         = in.gdp * Multiplier(MonthsPerYear)
    val nfaGdpRatio       = if in.gdp > PLN.Zero then in.prevBop.nfa / annualGdp else 0.0
    val fdi               = p.openEcon.fdiBase * Multiplier(
      (1.0 + toDouble(in.autoRatio) * FdiAutoBoost) *
        (1.0 - Math.max(0.0, -nfaGdpRatio) * FdiNfaDampening),
    )
    val portfolioFlows    =
      val rateDiff    = toDouble(in.domesticRate - p.forex.foreignRate)
      val riskPremium = -toDouble(p.openEcon.riskPremiumSensitivity) * nfaGdpRatio
      val monthlyGdp  = if in.gdp > PLN.Zero then in.gdp else PLN(1.0)
      PLN(toDouble(monthlyGdp) * (rateDiff + riskPremium) * toDouble(p.openEcon.portfolioSensitivity))
    val yieldSpread       = in.bondYield - p.forex.foreignRate
    val capitalFlight     = CapitalFlows.compute(
      month = in.month,
      yieldSpread = yieldSpread,
      bidToCover = in.prevBidToCover,
      prevCarry = CapitalFlows.CarryState(in.prevBop.carryTradeStock),
      monthlyGdp = if in.gdp > PLN.Zero then in.gdp else PLN(1.0),
    )
    val adjustedPortfolio = portfolioFlows + capitalFlight.totalAdjustment
    CapitalAccountResult(fdi + adjustedPortfolio, fdi, adjustedPortfolio, capitalFlight.newCarryState.stock)

  @boundaryEscape
  private def realExchangeRateEffect(exchangeRate: Double, priceLevel: Double)(using p: SimParams): Double =
    import ComputationBoundary.toDouble
    val nominalER = exchangeRate / p.forex.baseExRate
    val realPrice = if priceLevel > 0 && nominalER > 0 then priceLevel / nominalER else 1.0
    Math.pow(1.0 / Math.max(MinRealPrice, realPrice), toDouble(p.openEcon.exportPriceElasticity))

  @boundaryEscape
  private def computeExchangeRate(
      in: StepInput,
      ca: PLN,
      capitalAccount: PLN,
      fxErEffect: Double,
  )(using p: SimParams): Double =
    import ComputationBoundary.toDouble
    val annualGdp   = in.gdp * Multiplier(MonthsPerYear)
    val nfaGdpRatio = if in.gdp > PLN.Zero then in.prevBop.nfa / annualGdp else 0.0
    val bopGdpRatio = if in.gdp > PLN.Zero then (ca + capitalAccount) / in.gdp else 0.0
    val nfaRisk     = toDouble(p.openEcon.riskPremiumSensitivity) * Math.min(0.0, nfaGdpRatio)
    val pppDrift    = toDouble(((in.inflation - p.gvc.foreignInflation) * p.openEcon.pppSpeed).monthly)
    val erChange    = toDouble(p.forex.exRateAdjSpeed) * (-bopGdpRatio + nfaRisk) + fxErEffect + pppDrift
    Math.max(p.openEcon.erFloor, Math.min(p.openEcon.erCeiling, in.prevForex.exchangeRate * (1.0 + erChange)))

  @boundaryEscape
  private def computeValuationEffect(prevBop: BopState, prevExRate: Double, newExRate: Double): PLN =
    import ComputationBoundary.toDouble
    val erChange = (newExRate - prevExRate) / prevExRate
    PLN(toDouble(prevBop.foreignAssets) * erChange * ValuationPassThrough)
