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

  /** Foreign exchange snapshot — nominal EUR/PLN rate and monthly trade flows.
    *
    * Recomputed every step by [[updateForeign]] (simplified mode) or [[step]]
    * (full open-economy mode). The exchange rate feeds back into export
    * competitiveness (Marshall-Lerner), imported intermediate costs, and NFA
    * valuation. Trade balance = exports − imports (merchandise only; tourism
    * and services are tracked separately in [[BopState]]).
    *
    * Calibration: NBP daily fixing, GUS foreign trade statistics 2024.
    */
  case class ForexState(
      exchangeRate: Double, // nominal EUR/PLN mid-rate (NBP fixing convention)
      imports: PLN,         // total merchandise imports this month (consumption + tech + intermediates)
      exports: PLN,         // total merchandise exports this month (before tourism)
      tradeBalance: PLN,    // exports − imports (merchandise only)
      techImports: PLN,     // technology-related imports (automation CAPEX, R&D equipment)
  )

  /** Balance of Payments snapshot — full BPM6 decomposition.
    *
    * Accounting identity: CA + KA + ΔReserves ≡ 0 (IMF BPM6, §2.18). Updated
    * every step by [[step]]; the simplified [[updateForeign]] path does not
    * produce a BopState (World.bop stays at [[BopState.zero]]).
    *
    * Stock fields (`nfa`, `foreignAssets`, `foreignLiabilities`, `reserves`,
    * `euCumulativeAbsorption`) accumulate across months. All other fields are
    * single-month flows, reset each step.
    *
    * NFA dynamics: ΔNFA = CA + valuationEffect (ER pass-through on gross
    * foreign assets, [[computeValuationEffect]]).
    *
    * Calibration: NBP BoP statistics 2024, Eurostat EU funds absorption data.
    */
  case class BopState(
      nfa: PLN,                               // net foreign assets (cumulative stock, ΔNFA = CA + valuation)
      foreignAssets: PLN,                     // gross foreign assets (cumulative, grows with KA inflows)
      foreignLiabilities: PLN,                // gross foreign liabilities (cumulative, grows with KA outflows)
      currentAccount: PLN,                    // monthly CA: trade + primary income + secondary income
      capitalAccount: PLN,                    // monthly KA: FDI + portfolio flows (BPM6 financial account)
      tradeBalance: PLN,                      // monthly exports − imports (merchandise + tourism)
      primaryIncome: PLN,                     // monthly interest/dividends on NFA (nfa × nfaReturnRate / 12)
      secondaryIncome: PLN,                   // monthly transfers: EU funds + diaspora − remittance outflow
      fdi: PLN,                               // monthly FDI inflows (base + automation boost − NFA dampening)
      portfolioFlows: PLN,                    // monthly portfolio flows (interest rate differential + risk premium)
      reserves: PLN,                          // CB foreign reserves (cumulative, ΔRes = −(CA + KA))
      exports: PLN,                           // total exports this month (merchandise + tourism)
      totalImports: PLN,                      // total imports this month (consumption + tech + intermediates + tourism)
      importedIntermediates: PLN,             // cross-border intermediate inputs (per-sector import content × output)
      euFundsMonthly: PLN = PLN.Zero,         // EU structural/cohesion funds transferred this month
      euCumulativeAbsorption: PLN = PLN.Zero, // cumulative EU funds absorbed since t = 0
      carryTradeStock: PLN = PLN.Zero,        // outstanding carry trade positions
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
  private val MinRealPrice         = 0.1 // floor on real price to avoid explosion
  private val FdiAutoBoost         = 0.3 // FDI uplift per unit automation ratio
  private val FdiNfaDampening      = 0.5 // FDI reduction per unit negative NFA/GDP
  private val ValuationPassThrough = 0.3 // ER pass-through to foreign asset valuation

  /** Simplified foreign sector update (when OPEN_ECON disabled). */
  def updateForeign(
      prev: ForexState,
      importConsumption: PLN,
      techImports: PLN,
      autoRatio: Share,
      domesticRate: Rate,
      gdp: PLN,
  )(using p: SimParams): ForexState =
    val techComp  = 1.0 + autoRatio.toDouble * p.forex.exportAutoBoost.toDouble
    val totalImp  = importConsumption + techImports
    val exComp    = prev.exchangeRate / p.forex.baseExRate
    val exports   = p.forex.exportBase * Multiplier(exComp * techComp)
    val tradeBal  = exports - totalImp
    val rateDiff  = (domesticRate - p.forex.foreignRate).toDouble
    val capAcct   = PLN(gdp.toDouble * rateDiff * p.forex.irpSensitivity.toDouble)
    val bop       = tradeBal + capAcct
    val bopRatio  = if gdp > PLN.Zero then bop / gdp else 0.0
    val exRateChg = -p.forex.exRateAdjSpeed.toDouble * bopRatio
    val newRate   = Math.max(3.0, Math.min(8.0, prev.exchangeRate * (1.0 + exRateChg)))
    ForexState(newRate, totalImp, exports, tradeBal, techImports)

  /** Output of a single open-economy step. */
  case class Result(
      forex: ForexState,                  // updated FX state (ER, imports, exports, trade balance)
      bop: BopState,                      // full balance-of-payments snapshot
      importedIntermediates: Vector[PLN], // per-sector imported intermediate goods
      valuationEffect: PLN,               // NFA valuation change from ER movement
      fxIntervention: Nbp.FxInterventionResult // NBP FX intervention result (reserves, EUR traded)
      = Nbp.FxInterventionResult(0.0, PLN.Zero, PLN.Zero, PLN.Zero),
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
      inflation: Rate = Rate.Zero, // domestic CPI YoY for PPP drift
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

  /** Current account breakdown. */
  private case class CurrentAccountResult(ca: PLN, primaryIncome: PLN, secondaryIncome: PLN)

  /** Capital account breakdown. */
  private case class CapitalAccountResult(
      total: PLN,                     // FDI + adjusted portfolio flows
      fdi: PLN,                       // foreign direct investment
      portfolioFlows: PLN,            // portfolio flows (incl. risk-off, carry trade, auction signal)
      carryTradeStock: PLN = PLN.Zero, // outstanding carry trade positions after this month
  )

  def step(in: StepInput)(using p: SimParams): Result =
    val exports             = computeExports(in)
    val totalExportsIncTour = exports + in.tourismExport
    val importedInterm      = computeImportedIntermediates(in)
    val totalImportedInterm = kahanSumPln(importedInterm)
    val totalImports        = in.importCons + in.techImports + totalImportedInterm + in.tourismImport
    val tradeBalance        = totalExportsIncTour - totalImports
    val caResult            = computeCurrentAccount(in, tradeBalance)
    val kaResult            = computeCapitalAccount(in)
    val deltaReserves       = -(caResult.ca + kaResult.total)
    val fxResult            = Nbp.fxIntervention(in.prevForex.exchangeRate, in.nbpFxReserves.toDouble, in.gdp.toDouble, p.flags.nbpFxIntervention)
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

  // --- Trade helpers ---

  /** Export demand: foreign GDP trend × ULC competitiveness × real ER
    * elasticity. Returns total merchandise exports (PLN) before tourism. Falls
    * back to GVC module exports when flags.gvc is enabled.
    */
  private def computeExports(in: StepInput)(using p: SimParams): PLN =
    in.gvcExports.getOrElse:
      val foreignGdpFactor = Math.pow(1.0 + p.openEcon.foreignGdpGrowth.toDouble / MonthsPerYear, in.month.toDouble)
      val ulcEffect        = 1.0 + in.autoRatio.toDouble * p.openEcon.ulcExportBoost.toDouble
      val realExRate       = realExchangeRateEffect(in.prevForex.exchangeRate, in.priceLevel)
      p.openEcon.exportBase * Multiplier(foreignGdpFactor * realExRate * ulcEffect)

  /** Per-sector imported intermediates: real output × sector import content ×
    * ER effect. Returns one PLN value per sector. Falls back to GVC module
    * imports when flags.gvc is enabled.
    */
  private def computeImportedIntermediates(in: StepInput)(using p: SimParams): Vector[PLN] =
    in.gvcIntermImports.getOrElse:
      val nSectors    = p.sectorDefs.length
      val erNetEffect = Math.pow(in.prevForex.exchangeRate / p.forex.baseExRate, 1.0 - p.openEcon.erElasticity.toDouble)
      (0 until nSectors)
        .map: s =>
          val realOutput = if in.priceLevel > 0 then PLN(in.sectorOutputs(s).toDouble / in.priceLevel) else in.sectorOutputs(s)
          realOutput * p.openEcon.importContent(s) * Multiplier(erNetEffect)
        .toVector

  // --- BoP components ---

  /** Current account = trade balance + primary income (NFA return) + secondary
    * income (EU funds, remittances, diaspora). All three components returned
    * for BopState.
    */
  private def computeCurrentAccount(in: StepInput, tradeBalance: PLN)(using p: SimParams): CurrentAccountResult =
    val primaryIncome   = in.prevBop.nfa * p.openEcon.nfaReturnRate.monthly
    val secondaryIncome = in.euFundsMonthly - in.remittanceOutflow + in.diasporaInflow
    val ca              = tradeBalance + primaryIncome + secondaryIncome
    CurrentAccountResult(ca, primaryIncome, secondaryIncome)

  /** Capital account = FDI (base + automation boost - NFA dampening) +
    * portfolio flows (interest rate differential + NFA risk premium).
    */
  private def computeCapitalAccount(in: StepInput)(using p: SimParams): CapitalAccountResult =
    val annualGdp         = in.gdp * Multiplier(MonthsPerYear)
    val nfaGdpRatio       = if in.gdp > PLN.Zero then in.prevBop.nfa / annualGdp else 0.0
    val fdi               = p.openEcon.fdiBase * Multiplier((1.0 + in.autoRatio.toDouble * FdiAutoBoost) *
      (1.0 - Math.max(0.0, -nfaGdpRatio) * FdiNfaDampening))
    val portfolioFlows    =
      val rateDiff    = (in.domesticRate - p.forex.foreignRate).toDouble
      val riskPremium = -p.openEcon.riskPremiumSensitivity.toDouble * nfaGdpRatio
      val monthlyGdp  = if in.gdp > PLN.Zero then in.gdp else PLN(1.0)
      PLN(monthlyGdp.toDouble * (rateDiff + riskPremium) * p.openEcon.portfolioSensitivity.toDouble)
    // Capital flight: risk-off, carry trade, auction signal
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

  // --- Exchange rate & valuation ---

  /** Dimensionless export price multiplier from the real exchange rate
    * (Marshall-Lerner condition). Weaker PLN (higher nominal ER) → lower real
    * price → multiplier > 1 → higher exports. Used as input to computeExports;
    * does NOT determine the new nominal ER.
    */
  private def realExchangeRateEffect(exchangeRate: Double, priceLevel: Double)(using p: SimParams): Double =
    val nominalER = exchangeRate / p.forex.baseExRate
    val realPrice = if priceLevel > 0 && nominalER > 0 then priceLevel / nominalER else 1.0
    Math.pow(1.0 / Math.max(MinRealPrice, realPrice), p.openEcon.exportPriceElasticity.toDouble)

  /** New nominal exchange rate for the next period, driven by BoP flows and
    * relative PPP. ER adjusts to close the BoP gap: surplus → appreciation,
    * deficit → depreciation. PPP drift depreciates PLN when domestic inflation
    * exceeds foreign (Rogoff 1996). Includes NFA risk premium (negative NFA →
    * weaker PLN) and NBP FX intervention. Clamped to [erFloor, erCeiling].
    */
  private def computeExchangeRate(
      in: StepInput,
      ca: PLN,
      capitalAccount: PLN,
      fxErEffect: Double,
  )(using p: SimParams): Double =
    val annualGdp   = in.gdp * Multiplier(MonthsPerYear)
    val nfaGdpRatio = if in.gdp > PLN.Zero then in.prevBop.nfa / annualGdp else 0.0
    val bopGdpRatio = if in.gdp > PLN.Zero then (ca + capitalAccount) / in.gdp else 0.0
    val nfaRisk     = p.openEcon.riskPremiumSensitivity.toDouble * Math.min(0.0, nfaGdpRatio)
    // Relative PPP: higher domestic inflation → PLN depreciates (positive erChange)
    val pppDrift    = ((in.inflation - p.gvc.foreignInflation) * p.openEcon.pppSpeed).monthly.toDouble
    val erChange    = p.forex.exRateAdjSpeed.toDouble * (-bopGdpRatio + nfaRisk) + fxErEffect + pppDrift
    Math.max(p.openEcon.erFloor, Math.min(p.openEcon.erCeiling, in.prevForex.exchangeRate * (1.0 + erChange)))

  /** NFA valuation adjustment from ER movement between periods. Not a flow
    * through current account — purely a balance-sheet revaluation. Partial
    * pass-through (30%): only a fraction of ER change reprices foreign assets.
    * ΔNFA = CA + valuationEffect (accounting identity).
    */
  private def computeValuationEffect(prevBop: BopState, prevExRate: Double, newExRate: Double): PLN =
    val erChange = (newExRate - prevExRate) / prevExRate
    PLN(prevBop.foreignAssets.toDouble * erChange * ValuationPassThrough)

  /** Kahan-stable summation for PLN vectors. */
  private def kahanSumPln(vs: Vector[PLN]): PLN = PLN(vs.map(_.toDouble).kahanSum)
