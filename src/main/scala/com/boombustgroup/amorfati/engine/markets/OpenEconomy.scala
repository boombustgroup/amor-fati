package com.boombustgroup.amorfati.engine.markets

import com.boombustgroup.amorfati.agents.Nbp
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
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
  private val MonthsPerYear        = 12
  private val MinRealPrice         = Multiplier(0.1)
  private val FdiAutoBoost         = Coefficient(0.3)
  private val FdiNfaDampening      = Coefficient(0.5)
  private val ValuationPassThrough = Coefficient(0.3)
  private val MinErShock           = ExchangeRateShock(-0.9999)

  case class ForexState(
      exchangeRate: ExchangeRate,
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

  case class Result(
      forex: ForexState,
      bop: BopState,
      importedIntermediates: Vector[PLN],
      valuationEffect: PLN,
      fxIntervention: Nbp.FxInterventionResult = Nbp.FxInterventionResult(ExchangeRateShock.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
  )

  case class StepInput(
      prevBop: BopState,
      prevForex: ForexState,
      importCons: PLN,
      techImports: PLN,
      autoRatio: Share,
      domesticRate: Rate,
      gdp: PLN,
      priceLevel: PriceIndex,
      sectorOutputs: Vector[PLN],
      month: ExecutionMonth,
      inflation: Rate = Rate.Zero,
      nbpFxReserves: PLN,
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
    val totalImportedInterm = sumPln(importedInterm)
    val totalImports        = in.importCons + in.techImports + totalImportedInterm + in.tourismImport
    val tradeBalance        = totalExportsIncTour - totalImports
    val caResult            = computeCurrentAccount(in, tradeBalance)
    val kaResult            = computeCapitalAccount(in)
    val deltaReserves       = -(caResult.ca + kaResult.total)
    val fxResult            = Nbp.fxIntervention(
      in.prevForex.exchangeRate,
      in.nbpFxReserves,
      in.gdp,
      true,
    )
    val newExRate           = computeExchangeRate(in, caResult.ca, kaResult.total, fxResult.erShock)
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

  private def computeExports(in: StepInput)(using p: SimParams): PLN =
    in.gvcExports.getOrElse:
      val foreignGdpFactor = compoundedGrowth(p.openEcon.foreignGdpGrowth.monthly.growthMultiplier, in.month.toInt)
      val ulcEffect        = (in.autoRatio * p.openEcon.ulcExportBoost).growthMultiplier
      val realExRate       = realExchangeRateEffect(in.prevForex.exchangeRate, in.priceLevel)
      ((realExRate * p.openEcon.exportBase) * foreignGdpFactor) * ulcEffect

  private def computeImportedIntermediates(in: StepInput)(using p: SimParams): Vector[PLN] =
    in.gvcIntermImports.getOrElse:
      val nSectors    = p.sectorDefs.length
      val nominalER   = in.prevForex.exchangeRate.ratioTo(p.forex.baseExRate)
      val erNetEffect = nominalER.pow((Coefficient.One - p.openEcon.erElasticity).toScalar)
      (0 until nSectors)
        .map: s =>
          val realOutput = in.sectorOutputs(s) / in.priceLevel.toMultiplier
          (realOutput * p.openEcon.importContent(s)) * erNetEffect
        .toVector

  private def computeCurrentAccount(in: StepInput, tradeBalance: PLN)(using p: SimParams): CurrentAccountResult =
    val primaryIncome   = in.prevBop.nfa * p.openEcon.nfaReturnRate.monthly
    val secondaryIncome = in.euFundsMonthly - in.remittanceOutflow + in.diasporaInflow
    val ca              = tradeBalance + primaryIncome + secondaryIncome
    CurrentAccountResult(ca, primaryIncome, secondaryIncome)

  private def computeCapitalAccount(in: StepInput)(using p: SimParams): CapitalAccountResult =
    val annualGdp         = in.gdp * MonthsPerYear
    val nfaGdpRatio       = if in.gdp > PLN.Zero then in.prevBop.nfa.ratioTo(annualGdp).toCoefficient else Coefficient.Zero
    val autoBoost         = (in.autoRatio * FdiAutoBoost).growthMultiplier
    val negativeNfaRatio  = (-nfaGdpRatio).max(Coefficient.Zero)
    val nfaDampening      = (-(negativeNfaRatio * FdiNfaDampening)).growthMultiplier
    val fdi               = (p.openEcon.fdiBase * autoBoost) * nfaDampening
    val portfolioFlows    =
      val rateDiff      = (in.domesticRate - p.forex.foreignRate).toCoefficient
      val riskPremium   = -(p.openEcon.riskPremiumSensitivity * nfaGdpRatio)
      val monthlyGdp    = if in.gdp > PLN.Zero then in.gdp else PLN.fromLong(1)
      val portfolioRate = (rateDiff + riskPremium) * p.openEcon.portfolioSensitivity
      monthlyGdp * portfolioRate
    val yieldSpread       = in.bondYield - p.forex.foreignRate
    val capitalFlight     = CapitalFlows.compute(
      month = in.month.toInt,
      yieldSpread = yieldSpread,
      bidToCover = in.prevBidToCover,
      prevCarry = CapitalFlows.CarryState(in.prevBop.carryTradeStock),
      monthlyGdp = if in.gdp > PLN.Zero then in.gdp else PLN.fromLong(1),
    )
    val adjustedPortfolio = portfolioFlows + capitalFlight.totalAdjustment
    CapitalAccountResult(fdi + adjustedPortfolio, fdi, adjustedPortfolio, capitalFlight.newCarryState.stock)

  private def realExchangeRateEffect(exchangeRate: ExchangeRate, priceLevel: PriceIndex)(using p: SimParams): Scalar =
    val nominalER = exchangeRate.ratioTo(p.forex.baseExRate)
    val realPrice = priceLevel.toMultiplier.ratioTo(nominalER).max(MinRealPrice.toScalar)
    realPrice.reciprocal.pow(p.openEcon.exportPriceElasticity.toScalar)

  private def computeExchangeRate(
      in: StepInput,
      ca: PLN,
      capitalAccount: PLN,
      fxErShock: ExchangeRateShock,
  )(using p: SimParams): ExchangeRate =
    val annualGdp   = in.gdp * MonthsPerYear
    val nfaGdpRatio = if in.gdp > PLN.Zero then in.prevBop.nfa.ratioTo(annualGdp).toCoefficient else Coefficient.Zero
    val bopGdpRatio = if in.gdp > PLN.Zero then (ca + capitalAccount).ratioTo(in.gdp).toCoefficient else Coefficient.Zero
    val nfaRisk     = p.openEcon.riskPremiumSensitivity * (-nfaGdpRatio).max(Coefficient.Zero)
    val pppDrift    = ((in.inflation - p.gvc.foreignInflation) * p.openEcon.pppSpeed).monthly.toCoefficient
    val erChange    = ((p.forex.exRateAdjSpeed * (-bopGdpRatio + nfaRisk)) + fxErShock.toCoefficient + pppDrift).toExchangeRateShock
      .max(MinErShock)
    in.prevForex.exchangeRate
      .applyShock(erChange)
      .clamp(p.openEcon.erFloor, p.openEcon.erCeiling)

  private def computeValuationEffect(prevBop: BopState, prevExRate: ExchangeRate, newExRate: ExchangeRate): PLN =
    val erChange = newExRate.deviationFrom(prevExRate).toCoefficient
    prevBop.foreignAssets * (erChange * ValuationPassThrough)

  private def compoundedGrowth(monthlyFactor: Multiplier, periods: Int): Multiplier =
    if periods <= 0 then Multiplier.One
    else Iterator.fill(periods)(monthlyFactor).foldLeft(Multiplier.One)(_ * _)

  private def sumPln(values: Vector[PLN]): PLN =
    values.foldLeft(PLN.Zero)(_ + _)
