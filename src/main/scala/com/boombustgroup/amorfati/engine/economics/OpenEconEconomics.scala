package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.engine.ledger.{CorporateBondOwnership, LedgerFinancialState}
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

  case class MonetaryPolicy(
      newRefRate: Rate,
      newExp: Expectations.State,
      newBondYield: Rate,
      newWeightedCoupon: Rate,
      qePurchaseAmount: PLN,
      postFxNbp: Nbp.State,
      postFxNbpFinancialStocks: Nbp.FinancialStocks,
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
      flowBop: OpenEconomy.BopState,
      newForex: OpenEconomy.ForexState,
      newBop: OpenEconomy.BopState,
      newGvc: GvcTrade.State,
      oeValuationEffect: PLN,
      fdiCitLoss: PLN,
  )

  case class CorporateBonds(
      newCorpBonds: CorporateBondMarket.State,
      closingCorpBondProjection: CorporateBondMarket.StockState, // ledger-owned stock projection for downstream settlement
      corpBondCoupon: PLN,
      corpBondBankCoupon: PLN,
      corpBondBankDefaultLoss: PLN,
      corpBondInsuranceDefaultLoss: PLN,
      corpBondNbfiDefaultLoss: PLN,
      corpBondAmort: PLN,
  )

  case class NonBankFinancials(
      newInsurance: Insurance.State,
      newInsuranceBalances: Insurance.ClosingBalances,
      insNetDepositChange: PLN,
      newNbfi: Nbfi.State,
      newNbfiBalances: Nbfi.ClosingBalances,
      nbfiDepositDrain: PLN,
  )

  case class StepOutput(
      monetary: MonetaryPolicy,
      banking: BankingFlows,
      external: ExternalSector,
      corpBonds: CorporateBonds,
      nonBank: NonBankFinancials,
  )

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
      flowBop: OpenEconomy.BopState,
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
      postFxNbpFinancialStocks: Nbp.FinancialStocks,
  )

  private case class InsuranceResult(state: Insurance.State, closing: Insurance.ClosingBalances)
  private case class NbfiResult(state: Nbfi.State, closing: Nbfi.ClosingBalances)

  def runStep(in: StepInput)(using p: SimParams): StepOutput =
    val bankFinancialStocks = in.ledgerFinancialState.banks.map(LedgerFinancialState.projectBankFinancialStocks)
    val bankAgg             = Banking.aggregateFromBankStocks(
      in.banks,
      bankFinancialStocks,
      bankId => CorporateBondOwnership.bankHolderFor(in.ledgerFinancialState, bankId),
    )
    val sectorOutputs       = runStepSectorOutputs(in)
    val external            = runStepExternalSector(in, sectorOutputs)
    val rateAndExp          = runStepRateAndExpectations(in, external.newForex)
    val interbank           = runStepInterbankFlows(in.w, in.banks, bankFinancialStocks)
    val bondQe              = runStepBondYieldAndQe(in, bankAgg, rateAndExp.refRate, rateAndExp.expectations, external.fxIntervention, interbank)
    val corpBonds           = runStepCorporateBonds(in, bankAgg, bondQe.marketYield)
    val insurance           = runStepInsurance(
      in,
      bondQe.marketYield,
      corpBonds.newCorpBonds.corpBondYield,
      corpBonds.corpBondInsuranceDefaultLoss,
    )
    val nbfi                = runStepNbfi(
      in,
      bankAgg,
      bondQe.postFxNbp,
      bondQe.marketYield,
      corpBonds.newCorpBonds.corpBondYield,
      corpBonds.corpBondNbfiDefaultLoss,
    )

    StepOutput(
      monetary = MonetaryPolicy(
        newRefRate = rateAndExp.refRate,
        newExp = rateAndExp.expectations,
        newBondYield = bondQe.marketYield,
        newWeightedCoupon = bondQe.newWeightedCoupon,
        qePurchaseAmount = bondQe.qePurchaseAmount,
        postFxNbp = bondQe.postFxNbp,
        postFxNbpFinancialStocks = bondQe.postFxNbpFinancialStocks,
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
        flowBop = external.flowBop,
        newForex = external.newForex,
        newBop = external.newBop,
        newGvc = external.newGvc,
        oeValuationEffect = external.oeValuationEffect,
        fdiCitLoss = external.fdiCitLoss,
      ),
      corpBonds = corpBonds,
      nonBank = NonBankFinancials(
        newInsurance = insurance.state,
        newInsuranceBalances = insurance.closing,
        insNetDepositChange = insurance.state.lastNetDepositChange,
        newNbfi = nbfi.state,
        newNbfiBalances = nbfi.closing,
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
      flowBop = fxResult.bop,
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

  private def runStepInterbankFlows(w: World, banks: Vector[Banking.BankState], bankFinancialStocks: Vector[Banking.BankFinancialStocks])(using
      SimParams,
  ): InterbankResult =
    val bsec = w.bankingSector
    InterbankResult(
      reserveInterest = Banking.computeReserveInterestFromBankStocks(banks, bankFinancialStocks, w.nbp.referenceRate).total,
      standingFacilityIncome = Banking.computeStandingFacilitiesFromBankStocks(banks, bankFinancialStocks, w.nbp.referenceRate).total,
      interbankInterest = Banking.interbankInterestFlowsFromBankStocks(banks, bankFinancialStocks, bsec.interbankRate).total,
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
      qeActive,
      in.w.nbp.qeCumulative,
      in.w.nbp.lastFxTraded,
    )
    val preQeNbpStocks   = Nbp.FinancialStocks(
      govBondHoldings = in.ledgerFinancialState.nbp.govBondHoldings,
      foreignAssets = in.ledgerFinancialState.nbp.foreignAssets,
    )
    val qeRequest        = Nbp.executeQe(preQeNbp, preQeNbpStocks, bankAgg.govBondHoldings, in.s7.gdp, in.s7.newInfl, newExp.expectedInflation)
    val qePurchaseAmount = qeRequest.requestedPurchase
    val postFxNbp        = qeRequest.nbpState.copy(monthly = qeRequest.nbpState.monthly.copy(lastFxTraded = fxResult.eurTraded))
    val postFxNbpStocks  = preQeNbpStocks.copy(
      foreignAssets = fxResult.newReserves,
    )

    BondQeResult(marketYield, newWeightedCoupon, bankBondIncome, nbpRemittance, monthlyDebtService, qePurchaseAmount, postFxNbp, postFxNbpStocks)

  private def runStepCorporateBonds(in: StepInput, bankAgg: Banking.Aggregate, newBondYield: Rate)(using SimParams): CorporateBonds =
    val openingCorpBondProjection = CorporateBondOwnership.stockStateFromLedger(in.ledgerFinancialState)
    val corpBondAmort             = CorporateBondMarket.amortization(openingCorpBondProjection)
    val corpBondStep              = CorporateBondMarket
      .step(
        CorporateBondMarket.StepInput(
          prevState = in.w.financialMarkets.corporateBonds,
          prevStock = openingCorpBondProjection,
          govBondYield = newBondYield,
          nplRatio = bankAgg.nplRatio,
          totalBondDefault = in.s5.totalBondDefault,
          totalBondIssuance = in.s5.actualBondIssuance,
        ),
      )
    val newCorpBonds              = corpBondStep.state.copy(lastAbsorptionRate = in.s5.corpBondAbsorption)
    val corpBondCoupon            = CorporateBondMarket.computeCoupon(in.w.financialMarkets.corporateBonds, openingCorpBondProjection)
    val corpBondDefaults          = CorporateBondMarket.processDefaults(openingCorpBondProjection, in.s5.totalBondDefault)
    CorporateBonds(
      newCorpBonds = newCorpBonds,
      closingCorpBondProjection = corpBondStep.stock,
      corpBondCoupon = corpBondCoupon.total,
      corpBondBankCoupon = corpBondCoupon.bank,
      corpBondBankDefaultLoss = corpBondDefaults.bankLoss,
      corpBondInsuranceDefaultLoss = corpBondDefaults.insuranceLoss,
      corpBondNbfiDefaultLoss = corpBondDefaults.nbfiLoss,
      corpBondAmort = corpBondAmort,
    )

  private def runStepInsurance(
      in: StepInput,
      newBondYield: Rate,
      newCorpBondYield: Rate,
      corpBondDefaultLoss: PLN,
  )(using p: SimParams): InsuranceResult =
    val unempRate     = in.w.unemploymentRate(in.s2.employed)
    val insuranceStep =
      Insurance.step(
        Insurance.StepInput(
          opening = LedgerFinancialState.insuranceOpeningBalances(in.ledgerFinancialState),
          employed = in.s2.employed,
          wage = in.s2.newWage,
          unempRate = unempRate,
          govBondYield = newBondYield,
          corpBondYield = newCorpBondYield,
          equityReturn = in.s7.equityAfterIssuance.monthlyReturn,
          corpBondDefaultLoss = corpBondDefaultLoss,
        ),
      )
    InsuranceResult(insuranceStep.state, insuranceStep.closing)

  private def runStepNbfi(
      in: StepInput,
      bankAgg: Banking.Aggregate,
      postFxNbp: Nbp.State,
      newBondYield: Rate,
      newCorpBondYield: Rate,
      corpBondDefaultLoss: PLN,
  )(using p: SimParams): NbfiResult =
    val nbfiDepositRate = (postFxNbp.referenceRate - Rate(NbfiDepositRateSpread)).max(Rate.Zero)
    val nbfiUnempRate   = in.w.unemploymentRate(in.s2.employed)
    val nbfiStep        =
      Nbfi.step(
        Nbfi.StepInput(
          opening = LedgerFinancialState.nbfiOpeningBalances(in.ledgerFinancialState),
          employed = in.s2.employed,
          wage = in.s2.newWage,
          priceLevel = in.w.priceLevel,
          unempRate = nbfiUnempRate,
          bankNplRatio = bankAgg.nplRatio,
          govBondYield = newBondYield,
          corpBondYield = newCorpBondYield,
          equityReturn = in.s7.equityAfterIssuance.monthlyReturn,
          depositRate = nbfiDepositRate,
          domesticCons = in.s3.domesticCons,
          corpBondDefaultLoss = corpBondDefaultLoss,
        ),
      )
    NbfiResult(nbfiStep.state, nbfiStep.closing)
