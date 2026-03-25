package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.engine.markets.{CorporateBondMarket, GvcTrade, OpenEconomy}
import com.boombustgroup.amorfati.engine.mechanisms.Expectations
import com.boombustgroup.amorfati.types.*

import scala.util.Random

/** Self-contained open economy economics — calls market functions directly.
  *
  * Replaces OpenEconomyStep.run() wrapper. Same economic logic
  * (Marshall-Lerner, Taylor rule, Gordon equity, Meen housing), but does NOT
  * produce World state updates via .copy(). Instead, produces values for
  * MonthlyCalculus that feed flow mechanisms.
  *
  * Calculus: trade model, ER, monetary policy, bond yield, interbank, corp
  * bonds, insurance Plumbing: handled by flow mechanisms (OpenEconFlows,
  * BankingFlows, CorpBondFlows, etc.)
  */
object OpenEconEconomics:

  private val MaxDebtServiceGdpShare = 0.50

  /** Everything the new pipeline needs from open economy + monetary + financial
    * sector.
    */
  case class Result(
      // External sector
      exports: PLN,
      totalImports: PLN,
      fdi: PLN,
      portfolioFlows: PLN,
      primaryIncome: PLN,
      euFunds: PLN,
      newExchangeRate: Double,
      valuationEffect: PLN,
      fdiCitLoss: PLN,
      // Monetary policy
      newRefRate: Rate,
      newBondYield: Rate,
      newWeightedCoupon: Rate,
      monthlyDebtService: PLN,
      qePurchaseAmount: PLN,
      // Interbank
      reserveInterest: PLN,
      standingFacilityIncome: PLN,
      interbankInterest: PLN,
      bankBondIncome: PLN,
      nbpRemittance: PLN,
      // Corporate bonds
      corpBondCoupon: PLN,
      corpBondDefaultLoss: PLN,
      corpBondIssuance: PLN,
      corpBondAmortization: PLN,
      corpBondYield: Rate,
      // Insurance
      insLifePremium: PLN,
      insNonLifePremium: PLN,
      insLifeClaims: PLN,
      insNonLifeClaims: PLN,
      insInvestmentIncome: PLN,
      // Expectations
      newExpectations: Expectations.State,
      // GVC
      newGvc: GvcTrade.State,
      // NBP
      newNbpRefRate: Rate,
      newNbpQeActive: Boolean,
      newNbpGovBondHoldings: PLN,
      newNbpQeCumulative: PLN,
      newNbpFxReserves: PLN,
      newNbpLastFxTraded: PLN,
      // FX intervention
      fxPlnInjection: PLN,
  )

  /** Input: everything needed from previous stages. */
  case class Input(
      w: World,
      employed: Int,
      newWage: PLN,
      domesticConsumption: PLN,
      importConsumption: PLN,
      totalTechAndInvImports: PLN,
      gdp: PLN,
      newInflation: Rate,
      autoRatio: Share,
      govPurchases: PLN,
      sectorMults: Vector[Double],
      livingFirms: Vector[Firm.State],
      totalBondDefault: PLN,
      actualBondIssuance: PLN,
      corpBondAbsorption: Share,
      euMonthly: PLN,
      remittanceOutflow: PLN,
      diasporaInflow: PLN,
      tourismExport: PLN,
      tourismImport: PLN,
      equityReturn: Rate,
      investmentImports: PLN,
      profitShifting: PLN,
      fdiRepatriation: PLN,
      foreignDividendOutflow: PLN,
      month: Int,
      commodityRng: Random,
  )

  @boundaryEscape
  def compute(in: Input)(using p: SimParams): Result =
    import ComputationBoundary.toDouble

    // 1. Sector outputs (capacity × demand × price)
    val sectorOutputs = computeSectorOutputs(in)

    // 2. GVC trade
    val newGvc =
      if p.flags.gvc && p.flags.openEcon then
        GvcTrade.step(
          GvcTrade.StepInput(
            in.w.external.gvc,
            sectorOutputs.map(toDouble(_)),
            in.w.priceLevel,
            in.w.forex.exchangeRate,
            toDouble(in.autoRatio),
            in.month,
            in.commodityRng,
          ),
        )
      else in.w.external.gvc

    // 3. Forex / BoP
    val (gvcExp, gvcImp) = if p.flags.gvc && p.flags.openEcon then (Some(newGvc.totalExports), Some(newGvc.sectorImports)) else (None, None)
    val totalTechImp     = in.totalTechAndInvImports + in.investmentImports

    val (forex, bop, valEffect, fxResult) = if p.flags.openEcon then
      val oe = OpenEconomy.step(
        OpenEconomy.StepInput(
          prevBop = in.w.bop,
          prevForex = in.w.forex,
          importCons = in.importConsumption,
          techImports = totalTechImp,
          autoRatio = in.autoRatio,
          domesticRate = in.w.nbp.referenceRate,
          gdp = in.gdp,
          priceLevel = in.w.priceLevel,
          sectorOutputs = sectorOutputs,
          month = in.month,
          inflation = in.w.inflation,
          nbpFxReserves = in.w.nbp.fxReserves,
          gvcExports = gvcExp,
          gvcIntermImports = gvcImp,
          remittanceOutflow = in.remittanceOutflow,
          euFundsMonthly = in.euMonthly,
          diasporaInflow = in.diasporaInflow,
          tourismExport = in.tourismExport,
          tourismImport = in.tourismImport,
        ),
      )
      (oe.forex, oe.bop, oe.valuationEffect, oe.fxIntervention)
    else
      val fx = OpenEconomy.updateForeign(in.w.forex, in.importConsumption, totalTechImp, in.autoRatio, in.w.nbp.referenceRate, in.gdp)
      (fx, in.w.bop, PLN.Zero, Nbp.FxInterventionResult(0.0, PLN.Zero, in.w.nbp.fxReserves, PLN.Zero))

    // Adjust BoP for FDI and dividends
    val fdiCitLoss = in.profitShifting * p.fiscal.citRate

    // 4. Monetary policy (Taylor rule + expectations)
    val exRateChg   = Coefficient((forex.exchangeRate / in.w.forex.exchangeRate) - 1.0)
    val newRefRate  = Nbp.updateRate(in.w.nbp.referenceRate, in.newInflation, exRateChg, in.employed, in.w.totalPopulation)
    val unempForExp = 1.0 - in.employed.toDouble / in.w.totalPopulation
    val newExp      =
      if p.flags.expectations then Expectations.step(in.w.mechanisms.expectations, toDouble(in.newInflation), toDouble(newRefRate), unempForExp)
      else in.w.mechanisms.expectations

    // 5. Interbank
    val bsec              = in.w.bankingSector
    val reserveInterest   = Banking.computeReserveInterest(bsec.banks, in.w.nbp.referenceRate).total
    val standingFacility  = Banking.computeStandingFacilities(bsec.banks, in.w.nbp.referenceRate).total
    val interbankInterest = Banking.interbankInterestFlows(bsec.banks, bsec.interbankRate).total

    // 6. Bond yield, debt service, QE
    val annualGdp         = PLN(in.w.gdpProxy * 12.0)
    val debtToGdp         = if annualGdp > PLN.Zero then Share(in.w.gov.cumulativeDebt / annualGdp) else Share.Zero
    val nbpBondGdpShare   = if annualGdp > PLN.Zero then Share(in.w.nbp.qeCumulative / annualGdp) else Share.Zero
    val credPremium       = if p.flags.expectations then
      val deAnchor = (Share.One - in.w.mechanisms.expectations.credibility) *
        Share(toDouble((in.w.mechanisms.expectations.expectedInflation - p.monetary.targetInfl).abs))
      Rate(toDouble(deAnchor) * toDouble(p.labor.expBondSensitivity))
    else Rate.Zero
    val marketYield       = Nbp.bondYield(newRefRate, debtToGdp, nbpBondGdpShare, in.w.bop.nfa, credPremium)
    val newWeightedCoupon =
      updateWeightedCoupon(in.w.gov.weightedCoupon, marketYield, in.w.gov.bondsOutstanding, in.w.gov.deficit, p.fiscal.govAvgMaturityMonths)
    val rawDebtService    = in.w.gov.bondsOutstanding * newWeightedCoupon.monthly
    val debtService       = rawDebtService.min(PLN(in.w.gdpProxy * MaxDebtServiceGdpShare))
    val bankBondIncome    = in.w.bank.govBondHoldings * marketYield.monthly
    val nbpBondIncome     = in.w.nbp.govBondHoldings * marketYield.monthly
    val nbpRemittance     = nbpBondIncome - reserveInterest - standingFacility

    // QE
    val qeActive  =
      if Nbp.shouldActivateQe(newRefRate, in.newInflation) then true
      else if Nbp.shouldTaperQe(in.newInflation) then false
      else in.w.nbp.qeActive
    val preQeNbp  = Nbp.State(newRefRate, in.w.nbp.govBondHoldings, qeActive, in.w.nbp.qeCumulative, in.w.nbp.fxReserves, in.w.nbp.lastFxTraded)
    val qeRequest = Nbp.executeQe(preQeNbp, in.w.bank.govBondHoldings, annualGdp)

    // 7. Corporate bonds
    val corpBondAmort = CorporateBondMarket.amortization(in.w.financial.corporateBonds)
    val newCorpBonds  = CorporateBondMarket.step(
      CorporateBondMarket.StepInput(
        in.w.financial.corporateBonds,
        marketYield,
        in.w.bank.nplRatio,
        in.totalBondDefault,
        in.actualBondIssuance,
      ),
    )
    val corpCoupon    = CorporateBondMarket.computeCoupon(in.w.financial.corporateBonds)
    val corpDefaults  = CorporateBondMarket.processDefaults(in.w.financial.corporateBonds, in.totalBondDefault)

    // 8. Insurance
    val unempRate    = Share.One - Share.fraction(in.employed, in.w.totalPopulation)
    val newInsurance =
      if p.flags.insurance then
        Insurance.step(in.w.financial.insurance, in.employed, in.newWage, in.w.priceLevel, unempRate, marketYield, newCorpBonds.corpBondYield, in.equityReturn)
      else in.w.financial.insurance

    Result(
      exports = bop.exports,
      totalImports = bop.totalImports,
      fdi = bop.fdi,
      portfolioFlows = bop.portfolioFlows,
      primaryIncome = bop.primaryIncome,
      euFunds = bop.euFundsMonthly,
      newExchangeRate = forex.exchangeRate,
      valuationEffect = valEffect,
      fdiCitLoss = fdiCitLoss,
      newRefRate = newRefRate,
      newBondYield = marketYield,
      newWeightedCoupon = newWeightedCoupon,
      monthlyDebtService = debtService,
      qePurchaseAmount = qeRequest.requestedPurchase,
      reserveInterest = reserveInterest,
      standingFacilityIncome = standingFacility,
      interbankInterest = interbankInterest,
      bankBondIncome = bankBondIncome,
      nbpRemittance = nbpRemittance,
      corpBondCoupon = corpCoupon.bank,
      corpBondDefaultLoss = corpDefaults.bankLoss,
      corpBondIssuance = in.actualBondIssuance,
      corpBondAmortization = corpBondAmort,
      corpBondYield = newCorpBonds.corpBondYield,
      insLifePremium = newInsurance.lastLifePremium,
      insNonLifePremium = newInsurance.lastNonLifePremium,
      insLifeClaims = newInsurance.lastLifeClaims,
      insNonLifeClaims = newInsurance.lastNonLifeClaims,
      insInvestmentIncome = newInsurance.lastInvestmentIncome,
      newExpectations = newExp,
      newGvc = newGvc,
      newNbpRefRate = newRefRate,
      newNbpQeActive = qeActive,
      newNbpGovBondHoldings = qeRequest.nbpState.govBondHoldings,
      newNbpQeCumulative = qeRequest.nbpState.qeCumulative,
      newNbpFxReserves = fxResult.newReserves,
      newNbpLastFxTraded = fxResult.eurTraded,
      fxPlnInjection = fxResult.plnInjection,
    )

  @boundaryEscape
  private def computeSectorOutputs(in: Input)(using p: SimParams): Vector[PLN] =
    import ComputationBoundary.toDouble
    val living = in.livingFirms.filter(Firm.isAlive)
    (0 until p.sectorDefs.length)
      .map: s =>
        PLN(living.filter(_.sector.toInt == s).map(f => toDouble(Firm.computeCapacity(f)) * in.sectorMults(f.sector.toInt) * in.w.priceLevel).sum)
      .toVector

  @boundaryEscape
  private def updateWeightedCoupon(prevCoupon: Rate, marketYield: Rate, bondsOutstanding: PLN, deficit: PLN, avgMaturityMonths: Int)(using
      @scala.annotation.unused p: SimParams,
  ): Rate =
    import ComputationBoundary.toDouble
    val rolloverFrac = if avgMaturityMonths > 0 then 1.0 / avgMaturityMonths else 1.0 / 60.0
    val newIssueFrac = if bondsOutstanding > PLN.Zero then Math.max(0.0, toDouble(deficit) / toDouble(bondsOutstanding)) else 0.0
    val blendFrac    = Math.min(1.0, rolloverFrac + newIssueFrac)
    Rate(toDouble(prevCoupon) * (1.0 - blendFrac) + toDouble(marketYield) * blendFrac)
