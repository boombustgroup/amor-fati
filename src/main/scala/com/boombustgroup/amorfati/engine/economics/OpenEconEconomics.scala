package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.engine.ledger.{LedgerBoundaryProjection, LedgerFinancialState}
import com.boombustgroup.amorfati.engine.markets.{CorporateBondMarket, GvcTrade, OpenEconomy}
import com.boombustgroup.amorfati.engine.mechanisms.Expectations
import com.boombustgroup.amorfati.types.*

import com.boombustgroup.amorfati.random.RandomStream

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

  private val MaxDebtServiceGdpShare = Share(0.50)

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
      newExchangeRate: ExchangeRate,
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

  // ---------------------------------------------------------------------------
  // Bridge types — same as the deleted OpenEconomyStep nested types
  // ---------------------------------------------------------------------------

  case class MonetaryPolicy(
      newRefRate: Rate,
      newExp: Expectations.State,
      newBondYield: Rate,
      newWeightedCoupon: Rate,
      qePurchaseAmount: PLN,
      postFxNbp: Nbp.State,
      fxPlnInjection: PLN,
  )

  case class BankingFlows(
      totalReserveInterest: PLN,
      totalStandingFacilityIncome: PLN,
      totalInterbankInterest: PLN,
      bankBondIncome: PLN,
      nbpRemittance: PLN,
      monthlyDebtService: PLN,
  )

  case class ExternalSector(
      newForex: OpenEconomy.ForexState,
      newBop: OpenEconomy.BopState,
      newGvc: GvcTrade.State,
      oeValuationEffect: PLN,
      fdiCitLoss: PLN,
  )

  case class CorporateBonds(
      newCorpBonds: CorporateBondMarket.State,
      corpBondBankCoupon: PLN,
      corpBondBankDefaultLoss: PLN,
      corpBondAmort: PLN,
  )

  case class NonBankFinancials(
      newInsurance: Insurance.State,
      insNetDepositChange: PLN,
      newNbfi: Nbfi.State,
      nbfiDepositDrain: PLN,
  )

  case class StepOutput(
      monetary: MonetaryPolicy,
      banking: BankingFlows,
      external: ExternalSector,
      corpBonds: CorporateBonds,
      nonBank: NonBankFinancials,
  )

  /** Input: everything needed from previous stages. */
  case class Input(
      w: World,
      ledgerFinancialState: LedgerFinancialState,
      banks: Vector[Banking.BankState],
      employed: Int,
      newWage: PLN,
      domesticConsumption: PLN,
      importConsumption: PLN,
      totalTechAndInvImports: PLN,
      gdp: PLN,
      newInflation: Rate,
      autoRatio: Share,
      govPurchases: PLN,
      sectorMults: Vector[Multiplier],
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
      month: ExecutionMonth,
      commodityRng: RandomStream,
  )

  @boundaryEscape
  def compute(in: Input)(using p: SimParams): Result =
    import ComputationBoundary.toDouble
    val bankAgg = Banking.aggregateFromBanks(in.banks)

    // 1. Sector outputs (capacity × demand × price)
    val sectorOutputs = computeSectorOutputs(in)

    // 2. GVC trade
    val newGvc =
      GvcTrade.step(
        GvcTrade.StepInput(
          in.w.external.gvc,
          sectorOutputs,
          in.w.priceLevel,
          in.w.forex.exchangeRate,
          in.autoRatio,
          in.month,
          in.commodityRng,
        ),
      )

    // 3. Forex / BoP
    val (gvcExp, gvcImp) = (Some(newGvc.totalExports), Some(newGvc.sectorImports))
    val totalTechImp     = in.totalTechAndInvImports + in.investmentImports

    val oe                                = OpenEconomy.step(
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
        nbpFxReserves = in.ledgerFinancialState.nbp.foreignAssets,
        gvcExports = gvcExp,
        gvcIntermImports = gvcImp,
        remittanceOutflow = in.remittanceOutflow,
        euFundsMonthly = in.euMonthly,
        diasporaInflow = in.diasporaInflow,
        tourismExport = in.tourismExport,
        tourismImport = in.tourismImport,
      ),
    )
    val (forex, bop, valEffect, fxResult) = (oe.forex, oe.bop, oe.valuationEffect, oe.fxIntervention)

    // Adjust BoP for FDI and dividends
    val fdiCitLoss = in.profitShifting * p.fiscal.citRate

    // 4. Monetary policy (Taylor rule + expectations)
    val exRateChg   = forex.exchangeRate.deviationFrom(in.w.forex.exchangeRate).toCoefficient
    val newRefRate  = Nbp.updateRate(in.w.nbp.referenceRate, in.newInflation, exRateChg, in.employed, in.w.laborForcePopulation)
    val unempForExp = in.w.unemploymentRate(in.employed)
    val newExp      = Expectations.step(in.w.mechanisms.expectations, in.newInflation, newRefRate, unempForExp)

    // 5. Interbank
    val bsec              = in.w.bankingSector
    val reserveInterest   = Banking.computeReserveInterest(in.banks, in.w.nbp.referenceRate).total
    val standingFacility  = Banking.computeStandingFacilities(in.banks, in.w.nbp.referenceRate).total
    val interbankInterest = Banking.interbankInterestFlows(in.banks, bsec.interbankRate).total

    // 6. Bond yield, debt service, QE
    val annualGdp         = in.gdp * 12
    val debtToGdp         = if annualGdp > PLN.Zero then Share(toDouble(in.w.gov.cumulativeDebt) / toDouble(annualGdp)) else Share.Zero
    val nbpBondGdpShare   = if annualGdp > PLN.Zero then Share(toDouble(in.w.nbp.qeCumulative) / toDouble(annualGdp)) else Share.Zero
    val credPremium       =
      val deAnchor = (Share.One - in.w.mechanisms.expectations.credibility) *
        Share(toDouble((in.w.mechanisms.expectations.expectedInflation - p.monetary.targetInfl).abs))
      Rate(toDouble(deAnchor) * toDouble(p.labor.expBondSensitivity))
    val marketYield       = Nbp.bondYield(newRefRate, debtToGdp, nbpBondGdpShare, in.w.bop.nfa, credPremium)
    val newWeightedCoupon =
      updateWeightedCoupon(
        in.w.gov.weightedCoupon,
        marketYield,
        in.ledgerFinancialState.government.govBondOutstanding,
        in.w.gov.deficit,
        p.fiscal.govAvgMaturityMonths,
      )
    val rawDebtService    = in.ledgerFinancialState.government.govBondOutstanding * newWeightedCoupon.monthly
    val debtService       = rawDebtService.min(in.gdp * MaxDebtServiceGdpShare)
    val bankBondIncome    = bankAgg.govBondHoldings * marketYield.monthly
    val nbpBondIncome     = in.ledgerFinancialState.nbp.govBondHoldings * marketYield.monthly
    val nbpRemittance     = nbpBondIncome - reserveInterest - standingFacility

    // QE
    val qeActive  =
      if Nbp.shouldActivateQe(newRefRate, in.newInflation, newExp.expectedInflation) then true
      else if Nbp.shouldTaperQe(in.newInflation, newExp.expectedInflation) then false
      else in.w.nbp.qeActive
    val preQeNbp  = Nbp.State(
      newRefRate,
      in.ledgerFinancialState.nbp.govBondHoldings,
      qeActive,
      in.w.nbp.qeCumulative,
      in.ledgerFinancialState.nbp.foreignAssets,
      in.w.nbp.lastFxTraded,
    )
    val qeRequest = Nbp.executeQe(preQeNbp, bankAgg.govBondHoldings, in.gdp, in.newInflation, newExp.expectedInflation)

    // 7. Corporate bonds
    val prevCorpBonds = LedgerBoundaryProjection.corporateBondState(in.w.financial.corporateBonds, in.ledgerFinancialState)
    val corpBondAmort = CorporateBondMarket.amortization(prevCorpBonds)
    val newCorpBonds  = CorporateBondMarket.step(
      CorporateBondMarket.StepInput(
        prevCorpBonds,
        marketYield,
        bankAgg.nplRatio,
        in.totalBondDefault,
        in.actualBondIssuance,
      ),
    )
    val corpCoupon    = CorporateBondMarket.computeCoupon(prevCorpBonds)
    val corpDefaults  = CorporateBondMarket.processDefaults(prevCorpBonds, in.totalBondDefault)

    // 8. Insurance
    val unempRate    = in.w.unemploymentRate(in.employed)
    val newInsurance = Insurance.step(
      LedgerBoundaryProjection.insuranceState(in.w.financial.insurance, in.ledgerFinancialState),
      in.employed,
      in.newWage,
      unempRate,
      marketYield,
      newCorpBonds.corpBondYield,
      in.equityReturn,
      newCorpBonds.insuranceHoldings,
    )

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
      corpBondCoupon = corpCoupon.total,
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
    aggregateSectorOutputs(in.w.priceLevel, p.sectorDefs.length, in.livingFirms, in.sectorMults.apply)

  @boundaryEscape
  private def updateWeightedCoupon(prevCoupon: Rate, marketYield: Rate, bondsOutstanding: PLN, deficit: PLN, avgMaturityMonths: Int)(using
      @scala.annotation.unused p: SimParams,
  ): Rate =
    import ComputationBoundary.toDouble
    val rolloverFrac = if avgMaturityMonths > 0 then 1.0 / avgMaturityMonths else 1.0 / 60.0
    val newIssueFrac = if bondsOutstanding > PLN.Zero then Math.max(0.0, toDouble(deficit) / toDouble(bondsOutstanding)) else 0.0
    val blendFrac    = Math.min(1.0, rolloverFrac + newIssueFrac)
    Rate(toDouble(prevCoupon) * (1.0 - blendFrac) + toDouble(marketYield) * blendFrac)

  /** Public WAM coupon update — exposed for tests (DebtMaturitySpec). */
  private[amorfati] def updateWeightedCouponPublic(
      prevCoupon: Rate,
      marketYield: Rate,
      bondsOutstanding: PLN,
      deficit: PLN,
      avgMaturityMonths: Int,
  ): Rate =
    val rolloverFrac: Share = Share(1.0 / avgMaturityMonths.max(1))
    val deficitFrac: Share  =
      if bondsOutstanding > PLN.Zero then Share(deficit.max(PLN.Zero) / bondsOutstanding)
      else Share.Zero
    val freshFrac: Share    = (rolloverFrac + deficitFrac).min(Share.One)
    prevCoupon * (Share.One - freshFrac) + marketYield * freshFrac

  // ---------------------------------------------------------------------------
  // runStep — full open economy pipeline (migrated from OpenEconomyStep.run)
  // ---------------------------------------------------------------------------

  case class StepInput(
      w: World,
      ledgerFinancialState: LedgerFinancialState,
      s1: FiscalConstraintEconomics.Output,
      s2: LaborEconomics.Output,
      s3: HouseholdIncomeEconomics.Output,
      s4: DemandEconomics.Output,
      s5: FirmEconomics.StepOutput,
      s6: HouseholdFinancialEconomics.Output,
      s7: PriceEquityEconomics.Output,
      banks: Vector[Banking.BankState],
      commodityRng: RandomStream,
  )

  private val NbfiDepositRateSpread = 0.02

  // Internal intermediate types for sub-method returns
  private case class ForexResult(
      forex: OpenEconomy.ForexState,
      bop: OpenEconomy.BopState,
      valuationEffect: PLN,
      fxIntervention: Nbp.FxInterventionResult,
  )

  private case class ExternalResult(
      newForex: OpenEconomy.ForexState,
      newBop: OpenEconomy.BopState,
      newGvc: GvcTrade.State,
      oeValuationEffect: PLN,
      fdiCitLoss: PLN,
      fxIntervention: Nbp.FxInterventionResult,
  )

  private case class RateExpResult(
      refRate: Rate,
      expectations: Expectations.State,
  )

  private case class InterbankResult(
      reserveInterest: PLN,
      standingFacilityIncome: PLN,
      interbankInterest: PLN,
  )

  private case class BondQeResult(
      marketYield: Rate,
      newWeightedCoupon: Rate,
      bankBondIncome: PLN,
      nbpRemittance: PLN,
      monthlyDebtService: PLN,
      qePurchaseAmount: PLN,
      postFxNbp: Nbp.State,
  )

  private case class InsuranceResult(state: Insurance.State)
  private case class NbfiResult(state: Nbfi.State)

  def runStep(in: StepInput)(using p: SimParams): StepOutput =
    val bankAgg       = Banking.aggregateFromBanks(in.banks)
    val sectorOutputs = runStepSectorOutputs(in)
    val external      = runStepExternalSector(in, sectorOutputs)
    val rateAndExp    = runStepRateAndExpectations(in, external.newForex)
    val interbank     = runStepInterbankFlows(in.w, in.banks)
    val bondQe        = runStepBondYieldAndQe(in, bankAgg, rateAndExp.refRate, rateAndExp.expectations, external.fxIntervention, interbank)
    val corpBonds     = runStepCorporateBonds(in, bankAgg, bondQe.marketYield)
    val insurance     = runStepInsurance(in, bondQe.marketYield, corpBonds.newCorpBonds.corpBondYield, corpBonds.newCorpBonds.insuranceHoldings)
    val nbfi          = runStepNbfi(in, bankAgg, bondQe.postFxNbp, bondQe.marketYield, corpBonds.newCorpBonds.corpBondYield, corpBonds.newCorpBonds.nbfiHoldings)

    StepOutput(
      monetary = MonetaryPolicy(
        newRefRate = rateAndExp.refRate,
        newExp = rateAndExp.expectations,
        newBondYield = bondQe.marketYield,
        newWeightedCoupon = bondQe.newWeightedCoupon,
        qePurchaseAmount = bondQe.qePurchaseAmount,
        postFxNbp = bondQe.postFxNbp,
        fxPlnInjection = external.fxIntervention.plnInjection,
      ),
      banking = BankingFlows(
        totalReserveInterest = interbank.reserveInterest,
        totalStandingFacilityIncome = interbank.standingFacilityIncome,
        totalInterbankInterest = interbank.interbankInterest,
        bankBondIncome = bondQe.bankBondIncome,
        nbpRemittance = bondQe.nbpRemittance,
        monthlyDebtService = bondQe.monthlyDebtService,
      ),
      external = ExternalSector(
        newForex = external.newForex,
        newBop = external.newBop,
        newGvc = external.newGvc,
        oeValuationEffect = external.oeValuationEffect,
        fdiCitLoss = external.fdiCitLoss,
      ),
      corpBonds = corpBonds,
      nonBank = NonBankFinancials(
        newInsurance = insurance.state,
        insNetDepositChange = insurance.state.lastNetDepositChange,
        newNbfi = nbfi.state,
        nbfiDepositDrain = nbfi.state.lastDepositDrain,
      ),
    )

  @boundaryEscape
  private def runStepSectorOutputs(in: StepInput)(using p: SimParams): Vector[PLN] =
    aggregateSectorOutputs(in.w.priceLevel, p.sectorDefs.length, in.s5.ioFirms, in.s4.sectorMults.apply)

  private def aggregateSectorOutputs(
      priceLevel: PriceIndex,
      sectorCount: Int,
      firms: Vector[Firm.State],
      sectorMultiplier: Int => Multiplier,
  )(using p: SimParams): Vector[PLN] =
    val livingBySector = firms.iterator.filter(Firm.isAlive).toVector.groupBy(_.sector.toInt)
    Vector.tabulate(sectorCount): s =>
      livingBySector
        .getOrElse(s, Vector.empty)
        .foldLeft(PLN.Zero): (acc, f) =>
          acc + (priceLevel * (Firm.computeCapacity(f) * sectorMultiplier(f.sector.toInt)))

  @boundaryEscape
  private def runStepGvc(in: StepInput, sectorOutputs: Vector[PLN])(using p: SimParams): GvcTrade.State =
    GvcTrade.step(
      GvcTrade.StepInput(
        prev = in.w.external.gvc,
        sectorOutputs = sectorOutputs,
        priceLevel = in.w.priceLevel,
        exchangeRate = in.w.forex.exchangeRate,
        autoRatio = in.s7.autoR,
        month = in.s1.m,
        rng = in.commodityRng,
      ),
    )

  private def runStepForex(in: StepInput, sectorOutputs: Vector[PLN], newGvc: GvcTrade.State)(using p: SimParams): ForexResult =
    val (gvcExp, gvcImp) = (Some(newGvc.totalExports), Some(newGvc.sectorImports))

    val totalTechAndInvImports = in.s5.sumTechImp + in.s7.investmentImports
    val oeResult               = OpenEconomy.step(
      OpenEconomy.StepInput(
        prevBop = in.w.bop,
        prevForex = in.w.forex,
        importCons = in.s3.importCons,
        techImports = totalTechAndInvImports,
        autoRatio = in.s7.autoR,
        domesticRate = in.w.nbp.referenceRate,
        gdp = in.s7.gdp,
        inflation = in.w.inflation,
        priceLevel = in.w.priceLevel,
        sectorOutputs = sectorOutputs,
        month = in.s1.m,
        nbpFxReserves = in.ledgerFinancialState.nbp.foreignAssets,
        gvcExports = gvcExp,
        gvcIntermImports = gvcImp,
        remittanceOutflow = in.s6.remittanceOutflow,
        euFundsMonthly = in.s7.euMonthly,
        diasporaInflow = in.s6.diasporaInflow,
        tourismExport = in.s6.tourismExport,
        tourismImport = in.s6.tourismImport,
      ),
    )
    ForexResult(oeResult.forex, oeResult.bop, oeResult.valuationEffect, oeResult.fxIntervention)

  private def runStepAdjustBop(in: StepInput, bop0: OpenEconomy.BopState)(using p: SimParams): (OpenEconomy.BopState, PLN) =
    val bop1             =
      if in.s7.foreignDividendOutflow > PLN.Zero then
        bop0.copy(
          currentAccount = bop0.currentAccount - in.s7.foreignDividendOutflow,
          nfa = bop0.nfa - in.s7.foreignDividendOutflow,
        )
      else bop0
    val fdiTotalBopDebit = in.s5.sumProfitShifting + in.s5.sumFdiRepatriation
    val bop2             =
      if fdiTotalBopDebit > PLN.Zero then
        bop1.copy(
          currentAccount = bop1.currentAccount - fdiTotalBopDebit,
          nfa = bop1.nfa - fdiTotalBopDebit,
          tradeBalance = bop1.tradeBalance - in.s5.sumProfitShifting,
          totalImports = bop1.totalImports + in.s5.sumProfitShifting,
        )
      else bop1
    val fdiCitLoss       = in.s5.sumProfitShifting * p.fiscal.citRate
    val bop              = bop2.copy(
      euFundsMonthly = in.s7.euMonthly,
      euCumulativeAbsorption = in.w.bop.euCumulativeAbsorption + in.s7.euMonthly,
    )
    (bop, fdiCitLoss)

  private def runStepExternalSector(in: StepInput, sectorOutputs: Vector[PLN])(using p: SimParams): ExternalResult =
    val newGvc               = runStepGvc(in, sectorOutputs)
    val fxResult             = runStepForex(in, sectorOutputs, newGvc)
    val (newBop, fdiCitLoss) = runStepAdjustBop(in, fxResult.bop)
    ExternalResult(
      newForex = fxResult.forex,
      newBop = newBop,
      newGvc = newGvc,
      oeValuationEffect = fxResult.valuationEffect,
      fdiCitLoss = fdiCitLoss,
      fxIntervention = fxResult.fxIntervention,
    )

  @boundaryEscape
  private def runStepRateAndExpectations(in: StepInput, newForex: OpenEconomy.ForexState)(using p: SimParams): RateExpResult =
    val exRateChg       = newForex.exchangeRate.deviationFrom(in.w.forex.exchangeRate).toCoefficient
    val newRefRate      = Nbp.updateRate(
      in.w.nbp.referenceRate,
      in.s7.newInfl,
      exRateChg,
      in.s2.employed,
      in.w.laborForcePopulation,
    )
    val unempRateForExp = in.w.unemploymentRate(in.s2.employed)
    val newExp          =
      Expectations.step(in.w.mechanisms.expectations, in.s7.newInfl, newRefRate, unempRateForExp)
    RateExpResult(newRefRate, newExp)

  private def runStepInterbankFlows(w: World, banks: Vector[Banking.BankState])(using SimParams): InterbankResult =
    val bsec = w.bankingSector
    InterbankResult(
      reserveInterest = Banking.computeReserveInterest(banks, w.nbp.referenceRate).total,
      standingFacilityIncome = Banking.computeStandingFacilities(banks, w.nbp.referenceRate).total,
      interbankInterest = Banking.interbankInterestFlows(banks, bsec.interbankRate).total,
    )

  @boundaryEscape
  private def runStepBondYieldAndQe(
      in: StepInput,
      bankAgg: Banking.Aggregate,
      newRefRate: Rate,
      newExp: Expectations.State,
      fxResult: Nbp.FxInterventionResult,
      interbank: InterbankResult,
  )(using p: SimParams): BondQeResult =
    import ComputationBoundary.toDouble
    val annualGdpForBonds = in.s7.gdp * 12
    val debtToGdp         = if annualGdpForBonds > PLN.Zero then Share(toDouble(in.w.gov.cumulativeDebt) / toDouble(annualGdpForBonds)) else Share.Zero
    val nbpBondGdpShare   = if annualGdpForBonds > PLN.Zero then Share(toDouble(in.w.nbp.qeCumulative) / toDouble(annualGdpForBonds)) else Share.Zero
    val credPremium       =
      val deAnchor = (Share.One - in.w.mechanisms.expectations.credibility) *
        Share(toDouble((in.w.mechanisms.expectations.expectedInflation - p.monetary.targetInfl).abs))
      Rate(toDouble(deAnchor) * toDouble(p.labor.expBondSensitivity))
    val marketYield       = Nbp.bondYield(newRefRate, debtToGdp, nbpBondGdpShare, in.w.bop.nfa, credPremium)

    val newWeightedCoupon = updateWeightedCouponPublic(
      prevCoupon = in.w.gov.weightedCoupon,
      marketYield = marketYield,
      bondsOutstanding = in.ledgerFinancialState.government.govBondOutstanding,
      deficit = in.w.gov.deficit,
      avgMaturityMonths = p.fiscal.govAvgMaturityMonths,
    )

    val rawDebtService     = in.ledgerFinancialState.government.govBondOutstanding * newWeightedCoupon.monthly
    val monthlyDebtService = rawDebtService.min(in.s7.gdp * MaxDebtServiceGdpShare)
    val bankBondIncome     = bankAgg.govBondHoldings * marketYield.monthly
    val nbpBondIncome      = in.ledgerFinancialState.nbp.govBondHoldings * marketYield.monthly
    val nbpRemittance      = nbpBondIncome - interbank.reserveInterest - interbank.standingFacilityIncome

    val qeActivate       = Nbp.shouldActivateQe(newRefRate, in.s7.newInfl, newExp.expectedInflation)
    val qeTaper          = Nbp.shouldTaperQe(in.s7.newInfl, newExp.expectedInflation)
    val qeActive         =
      if qeActivate then true
      else if qeTaper then false
      else in.w.nbp.qeActive
    val preQeNbp         = Nbp.State(
      newRefRate,
      in.ledgerFinancialState.nbp.govBondHoldings,
      qeActive,
      in.w.nbp.qeCumulative,
      in.ledgerFinancialState.nbp.foreignAssets,
      in.w.nbp.lastFxTraded,
    )
    val qeRequest        = Nbp.executeQe(preQeNbp, bankAgg.govBondHoldings, in.s7.gdp, in.s7.newInfl, newExp.expectedInflation)
    val qePurchaseAmount = qeRequest.requestedPurchase
    val postFxNbp        = qeRequest.nbpState.copy(
      balance = qeRequest.nbpState.balance.copy(fxReserves = fxResult.newReserves),
      monthly = qeRequest.nbpState.monthly.copy(lastFxTraded = fxResult.eurTraded),
    )

    BondQeResult(marketYield, newWeightedCoupon, bankBondIncome, nbpRemittance, monthlyDebtService, qePurchaseAmount, postFxNbp)

  private def runStepCorporateBonds(in: StepInput, bankAgg: Banking.Aggregate, newBondYield: Rate)(using SimParams): CorporateBonds =
    val prevCorpBonds    = LedgerBoundaryProjection.corporateBondState(in.w.financial.corporateBonds, in.ledgerFinancialState)
    val corpBondAmort    = CorporateBondMarket.amortization(prevCorpBonds)
    val newCorpBonds     = CorporateBondMarket
      .step(
        CorporateBondMarket.StepInput(
          prev = prevCorpBonds,
          govBondYield = newBondYield,
          nplRatio = bankAgg.nplRatio,
          totalBondDefault = in.s5.totalBondDefault,
          totalBondIssuance = in.s5.actualBondIssuance,
        ),
      )
      .copy(lastAbsorptionRate = in.s5.corpBondAbsorption)
    val corpBondCoupon   = CorporateBondMarket.computeCoupon(prevCorpBonds)
    val corpBondDefaults = CorporateBondMarket.processDefaults(prevCorpBonds, in.s5.totalBondDefault)
    CorporateBonds(
      newCorpBonds = newCorpBonds,
      corpBondBankCoupon = corpBondCoupon.bank,
      corpBondBankDefaultLoss = corpBondDefaults.bankLoss,
      corpBondAmort = corpBondAmort,
    )

  private def runStepInsurance(
      in: StepInput,
      newBondYield: Rate,
      newCorpBondYield: Rate,
      settledCorpBondHoldings: PLN,
  )(using p: SimParams): InsuranceResult =
    val unempRate    = in.w.unemploymentRate(in.s2.employed)
    val newInsurance =
      Insurance.step(
        LedgerBoundaryProjection.insuranceState(in.w.financial.insurance, in.ledgerFinancialState),
        in.s2.employed,
        in.s2.newWage,
        unempRate,
        newBondYield,
        newCorpBondYield,
        in.w.financial.equity.monthlyReturn,
        settledCorpBondHoldings,
      )
    InsuranceResult(newInsurance)

  private def runStepNbfi(
      in: StepInput,
      bankAgg: Banking.Aggregate,
      postFxNbp: Nbp.State,
      newBondYield: Rate,
      newCorpBondYield: Rate,
      settledCorpBondHoldings: PLN,
  )(using p: SimParams): NbfiResult =
    val nbfiDepositRate = (postFxNbp.referenceRate - Rate(NbfiDepositRateSpread)).max(Rate.Zero)
    val nbfiUnempRate   = in.w.unemploymentRate(in.s2.employed)
    val newNbfi         =
      Nbfi.step(
        LedgerBoundaryProjection.nbfiState(in.w.financial.nbfi, in.ledgerFinancialState),
        in.s2.employed,
        in.s2.newWage,
        in.w.priceLevel,
        nbfiUnempRate,
        bankAgg.nplRatio,
        newBondYield,
        newCorpBondYield,
        in.w.financial.equity.monthlyReturn,
        nbfiDepositRate,
        in.s3.domesticCons,
        settledCorpBondHoldings,
      )
    NbfiResult(newNbfi)
