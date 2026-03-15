package com.boombustgroup.amorfati.engine.steps

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.*
import com.boombustgroup.amorfati.engine.markets.{CorporateBondMarket, GvcTrade, OpenEconomy}
import com.boombustgroup.amorfati.engine.mechanisms.Expectations
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.amorfati.util.KahanSum.*

/** Open economy step: monetary policy (Taylor rule, expectations), bond market
  * (yield, QE), external sector (forex, BOP, GVC trade), corporate bonds, and
  * non-bank financial institutions (insurance, NBFI/TFI). Integrates all
  * cross-border and financial-sector flows into a single coherent update.
  */
object OpenEconomyStep:

  // ---- Calibration constants ----
  private val NbfiDepositRateSpread  = 0.02 // NBFI deposit rate spread below reference rate
  private val MaxDebtServiceGdpShare = 0.50 // max monthly debt service as fraction of GDP proxy

  case class Input(
      w: World,                          // current world state
      s1: FiscalConstraintStep.Output,   // fiscal constraint (month counter, lending base rate)
      s2: LaborDemographicsStep.Output,  // labor/demographics (employment, wage)
      s3: HouseholdIncomeStep.Output,    // household income (domestic consumption, import consumption)
      s4: DemandStep.Output,             // demand (sector multipliers, gov purchases)
      s5: FirmProcessingStep.Output,     // firm processing (loans, NPL, bond issuance, I-O firms)
      s6: HouseholdFinancialStep.Output, // household financial (debt service, remittances, tourism)
      s7: PriceEquityStep.Output,        // price/equity (inflation, GDP, equity state, macropru)
  )

  case class MonetaryPolicy(
      newRefRate: Rate,
      newExp: Expectations.State,
      newBondYield: Rate,
      qePurchaseAmount: PLN,
      postFxNbp: Nbp.State,
      fxPlnInjection: PLN, // PLN injected (+) or drained (−) by FX intervention
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

  case class Output(
      monetary: MonetaryPolicy,
      banking: BankingFlows,
      external: ExternalSector,
      corpBonds: CorporateBonds,
      nonBank: NonBankFinancials,
  )

  // Internal intermediate type for forex step results
  private case class ForexResult(
      forex: OpenEconomy.ForexState,
      bop: OpenEconomy.BopState,
      valuationEffect: PLN,
      fxIntervention: Nbp.FxInterventionResult,
  )

  // Internal intermediate type for bond market + QE results
  private case class BondQeResult(
      newBondYield: Rate,
      bankBondIncome: PLN,
      nbpRemittance: PLN,
      monthlyDebtService: PLN,
      qePurchaseAmount: PLN,
      postFxNbp: Nbp.State,
  )

  def run(in: Input)(using p: SimParams): Output =
    val sectorOutputs = computeSectorOutputs(in)
    val external      = stepExternalSector(in, sectorOutputs)
    val rateAndExp    = stepRateAndExpectations(in, external.newForex)
    val interbank     = computeInterbankFlows(in.w)
    val bondQe        = stepBondYieldAndQe(in, rateAndExp.refRate, external.fxIntervention, interbank)
    val corpBonds     = stepCorporateBonds(in, bondQe.newBondYield)
    val insurance     = stepInsurance(in, bondQe.newBondYield)
    val nbfi          = stepNbfi(in, bondQe.postFxNbp, bondQe.newBondYield)

    Output(
      monetary = MonetaryPolicy(
        newRefRate = rateAndExp.refRate,
        newExp = rateAndExp.expectations,
        newBondYield = bondQe.newBondYield,
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

  // --- Internal intermediate types for sub-method returns ---

  private case class ExternalResult(
      newForex: OpenEconomy.ForexState,
      newBop: OpenEconomy.BopState,
      newGvc: GvcTrade.State,
      oeValuationEffect: PLN,
      fdiCitLoss: PLN,
      fxIntervention: Nbp.FxInterventionResult,
  )

  private case class RateExpResult(
      refRate: Rate,                   // new NBP reference rate after Taylor rule
      expectations: Expectations.State, // updated inflation/rate expectations state
  )

  private case class InterbankResult(
      reserveInterest: PLN,        // total interest earned on required reserves at NBP
      standingFacilityIncome: PLN, // net income from NBP standing facilities (deposit/lombard)
      interbankInterest: PLN,      // net interbank market interest flows across all banks
  )

  private case class InsuranceResult(
      state: Insurance.State, // updated insurance sector state (life + non-life reserves, asset allocation)
  )

  private case class NbfiResult(
      state: Nbfi.State, // updated NBFI/TFI state (AUM, credit, deposit drain)
  )

  // --- Sub-methods ---

  private def computeSectorOutputs(in: Input)(using p: SimParams): Vector[PLN] =
    val living = in.s5.ioFirms.filter(Firm.isAlive)
    (0 until p.sectorDefs.length)
      .map: s =>
        PLN(
          living
            .filter(_.sector.toInt == s)
            .kahanSumBy(f => (Firm.computeCapacity(f) * (in.s4.sectorMults(f.sector.toInt) * in.w.priceLevel)).toDouble),
        )
      .toVector

  private def stepGvc(in: Input, sectorOutputs: Vector[PLN])(using p: SimParams): GvcTrade.State =
    if p.flags.gvc && p.flags.openEcon then
      GvcTrade.step(
        GvcTrade.StepInput(
          prev = in.w.external.gvc,
          sectorOutputs = sectorOutputs.map(_.toDouble),
          priceLevel = in.w.priceLevel,
          exchangeRate = in.w.forex.exchangeRate,
          autoRatio = in.s7.autoR.toDouble,
          month = in.s1.m,
        ),
      )
    else in.w.external.gvc

  private def stepForex(in: Input, sectorOutputs: Vector[PLN], newGvc: GvcTrade.State)(using p: SimParams): ForexResult =
    val (gvcExp, gvcImp) =
      if p.flags.gvc && p.flags.openEcon then (Some(newGvc.totalExports), Some(newGvc.sectorImports))
      else (None, None)

    val totalTechAndInvImports = in.s5.sumTechImp + in.s7.investmentImports
    if p.flags.openEcon then
      val oeResult = OpenEconomy.step(
        OpenEconomy.StepInput(
          prevBop = in.w.bop,
          prevForex = in.w.forex,
          importCons = in.s3.importCons,
          techImports = totalTechAndInvImports,
          autoRatio = in.s7.autoR,
          domesticRate = in.w.nbp.referenceRate,
          gdp = in.s7.gdp,
          priceLevel = in.w.priceLevel,
          sectorOutputs = sectorOutputs,
          month = in.s1.m,
          nbpFxReserves = in.w.nbp.fxReserves,
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
    else
      val fx = OpenEconomy.updateForeign(
        in.w.forex,
        in.s3.importCons,
        totalTechAndInvImports,
        in.s7.autoR,
        in.w.nbp.referenceRate,
        in.s7.gdp,
      )
      ForexResult(fx, in.w.bop, PLN.Zero, Nbp.FxInterventionResult(0.0, PLN.Zero, in.w.nbp.fxReserves, PLN.Zero))

  private def adjustBop(in: Input, bop0: OpenEconomy.BopState)(using p: SimParams): (OpenEconomy.BopState, PLN) =
    // Adjust BOP for foreign dividend outflow (primary income component)
    val bop1             =
      if in.s7.foreignDividendOutflow > PLN.Zero && p.flags.openEcon then
        bop0.copy(
          currentAccount = bop0.currentAccount - in.s7.foreignDividendOutflow,
          nfa = bop0.nfa - in.s7.foreignDividendOutflow,
        )
      else bop0
    // FDI composition (#33): profit shifting (service import) + repatriation (primary income debit)
    val fdiTotalBopDebit = in.s5.sumProfitShifting + in.s5.sumFdiRepatriation
    val bop2             =
      if fdiTotalBopDebit > PLN.Zero && p.flags.fdi && p.flags.openEcon then
        bop1.copy(
          currentAccount = bop1.currentAccount - fdiTotalBopDebit,
          nfa = bop1.nfa - fdiTotalBopDebit,
          tradeBalance = bop1.tradeBalance - in.s5.sumProfitShifting,
          totalImports = bop1.totalImports + in.s5.sumProfitShifting,
        )
      else bop1
    val fdiCitLoss       = in.s5.sumProfitShifting * p.fiscal.citRate.toDouble
    // EU funds tracking
    val bop              = bop2.copy(
      euFundsMonthly = in.s7.euMonthly,
      euCumulativeAbsorption = in.w.bop.euCumulativeAbsorption + in.s7.euMonthly,
    )
    (bop, fdiCitLoss)

  private def stepExternalSector(in: Input, sectorOutputs: Vector[PLN])(using p: SimParams): ExternalResult =
    val newGvc               = stepGvc(in, sectorOutputs)
    val fxResult             = stepForex(in, sectorOutputs, newGvc)
    val (newBop, fdiCitLoss) = adjustBop(in, fxResult.bop)
    ExternalResult(
      newForex = fxResult.forex,
      newBop = newBop,
      newGvc = newGvc,
      oeValuationEffect = fxResult.valuationEffect,
      fdiCitLoss = fdiCitLoss,
      fxIntervention = fxResult.fxIntervention,
    )

  private def stepRateAndExpectations(in: Input, newForex: OpenEconomy.ForexState)(using p: SimParams): RateExpResult =
    val exRateChg       = (newForex.exchangeRate / in.w.forex.exchangeRate) - 1.0
    val newRefRate      = Nbp.updateRate(
      in.w.nbp.referenceRate,
      in.s7.newInfl,
      exRateChg,
      in.s2.employed,
      in.w.totalPopulation,
    )
    val unempRateForExp = 1.0 - in.s2.employed.toDouble / in.w.totalPopulation
    val newExp          =
      if p.flags.expectations then Expectations.step(in.w.mechanisms.expectations, in.s7.newInfl.toDouble, newRefRate.toDouble, unempRateForExp)
      else in.w.mechanisms.expectations
    RateExpResult(newRefRate, newExp)

  private def computeInterbankFlows(w: World)(using SimParams): InterbankResult =
    val bsec = w.bankingSector
    InterbankResult(
      reserveInterest = Banking.computeReserveInterest(bsec.banks, w.nbp.referenceRate).total,
      standingFacilityIncome = Banking.computeStandingFacilities(bsec.banks, w.nbp.referenceRate).total,
      interbankInterest = Banking.interbankInterestFlows(bsec.banks, bsec.interbankRate).total,
    )

  private def stepBondYieldAndQe(
      in: Input,
      newRefRate: Rate,
      fxResult: Nbp.FxInterventionResult,
      interbank: InterbankResult,
  )(using p: SimParams): BondQeResult =
    val annualGdpForBonds = in.w.gdpProxy * 12.0
    val debtToGdp         = if annualGdpForBonds > 0 then in.w.gov.cumulativeDebt.toDouble / annualGdpForBonds else 0.0
    val nbpBondGdpShare   = if annualGdpForBonds > 0 then in.w.nbp.qeCumulative.toDouble / annualGdpForBonds else 0.0
    // Channel 3: De-anchored expectations -> higher bond yields
    val credPremium       = if p.flags.expectations then
      val target = p.monetary.targetInfl.toDouble
      (1.0 - in.w.mechanisms.expectations.credibility.toDouble) *
        Math.abs(in.w.mechanisms.expectations.expectedInflation.toDouble - target) *
        p.labor.expBondSensitivity.toDouble
    else 0.0
    val newBondYield      = Nbp.bondYield(newRefRate, debtToGdp, nbpBondGdpShare, in.w.bop.nfa, credPremium)

    // Debt service: use LAGGED bond stock
    val rawDebtService     = in.w.gov.bondsOutstanding * newBondYield.monthly
    val monthlyDebtService = rawDebtService.min(PLN(in.w.gdpProxy * MaxDebtServiceGdpShare))
    val bankBondIncome     = in.w.bank.govBondHoldings * newBondYield.monthly
    val nbpBondIncome      = in.w.nbp.govBondHoldings * newBondYield.monthly
    val nbpRemittance      = nbpBondIncome - interbank.reserveInterest - interbank.standingFacilityIncome

    // QE logic
    val qeActivate       = Nbp.shouldActivateQe(newRefRate, in.s7.newInfl)
    val qeTaper          = Nbp.shouldTaperQe(in.s7.newInfl)
    val qeActive         =
      if qeActivate then true
      else if qeTaper then false
      else in.w.nbp.qeActive
    val preQeNbp         = Nbp.State(newRefRate, in.w.nbp.govBondHoldings, qeActive, in.w.nbp.qeCumulative, in.w.nbp.fxReserves, in.w.nbp.lastFxTraded)
    val qeResult         = Nbp.executeQe(preQeNbp, in.w.bank.govBondHoldings, PLN(annualGdpForBonds))
    val postQeNbp        = qeResult.state
    val qePurchaseAmount = qeResult.purchased
    val postFxNbp        = postQeNbp.copy(fxReserves = fxResult.newReserves, lastFxTraded = fxResult.eurTraded)

    BondQeResult(newBondYield, bankBondIncome, nbpRemittance, monthlyDebtService, qePurchaseAmount, postFxNbp)

  private def stepCorporateBonds(in: Input, newBondYield: Rate)(using SimParams): CorporateBonds =
    val corpBondAmort    = CorporateBondMarket.amortization(in.w.financial.corporateBonds)
    val newCorpBonds     = CorporateBondMarket
      .step(
        CorporateBondMarket.StepInput(
          prev = in.w.financial.corporateBonds,
          govBondYield = newBondYield,
          nplRatio = in.w.bank.nplRatio,
          totalBondDefault = in.s5.totalBondDefault,
          totalBondIssuance = in.s5.actualBondIssuance,
        ),
      )
      .copy(lastAbsorptionRate = in.s5.corpBondAbsorption)
    val corpBondCoupon   = CorporateBondMarket.computeCoupon(in.w.financial.corporateBonds)
    val corpBondDefaults = CorporateBondMarket.processDefaults(in.w.financial.corporateBonds, in.s5.totalBondDefault)
    CorporateBonds(
      newCorpBonds = newCorpBonds,
      corpBondBankCoupon = corpBondCoupon.bank,
      corpBondBankDefaultLoss = corpBondDefaults.bankLoss,
      corpBondAmort = corpBondAmort,
    )

  private def stepInsurance(in: Input, newBondYield: Rate)(using p: SimParams): InsuranceResult =
    val unempRate    = Ratio(1.0 - in.s2.employed.toDouble / in.w.totalPopulation)
    val newInsurance =
      if p.flags.insurance then
        Insurance.step(
          in.w.financial.insurance,
          in.s2.employed,
          in.s2.newWage,
          in.w.priceLevel,
          unempRate,
          newBondYield,
          in.w.financial.corporateBonds.corpBondYield,
          in.w.financial.equity.monthlyReturn,
        )
      else in.w.financial.insurance
    InsuranceResult(newInsurance)

  private def stepNbfi(in: Input, postFxNbp: Nbp.State, newBondYield: Rate)(using p: SimParams): NbfiResult =
    val nbfiDepositRate = (postFxNbp.referenceRate - Rate(NbfiDepositRateSpread)).max(Rate.Zero)
    val nbfiUnempRate   = Ratio(1.0 - in.s2.employed.toDouble / in.w.totalPopulation)
    val newNbfi         =
      if p.flags.nbfi then
        Nbfi.step(
          in.w.financial.nbfi,
          in.s2.employed,
          in.s2.newWage,
          in.w.priceLevel,
          nbfiUnempRate,
          in.w.bank.nplRatio,
          newBondYield,
          in.w.financial.corporateBonds.corpBondYield,
          in.w.financial.equity.monthlyReturn,
          nbfiDepositRate,
          in.s3.domesticCons,
        )
      else in.w.financial.nbfi
    NbfiResult(newNbfi)
