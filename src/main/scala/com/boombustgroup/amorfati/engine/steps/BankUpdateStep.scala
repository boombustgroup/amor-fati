package com.boombustgroup.amorfati.engine.steps

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.*
import com.boombustgroup.amorfati.engine.markets.{FiscalBudget, HousingMarket}
import com.boombustgroup.amorfati.engine.mechanisms.{TaxRevenue, YieldCurve}
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.amorfati.util.KahanSum.*

/** Bank balance sheet update: capital PnL, loan/NPL dynamics, deposit flows,
  * government bond allocation (PPK, insurance, TFI), multi-bank resolution path
  * with interbank clearing, failure detection, bail-in, and firm/household
  * reassignment. Also computes tax revenue, housing mortgage flows, and
  * monetary aggregates (M1/M2/M3).
  */
object BankUpdateStep:

  // ---- Calibration constants ----
  private val NplMonthlyWriteOff = 0.05 // monthly NPL write-off rate (aggregate and per-bank)
  private val ShortLoanFrac      = 0.20 // fraction of loans in short-term maturity bucket
  private val MediumLoanFrac     = 0.30 // fraction of loans in medium-term maturity bucket
  private val LongLoanFrac       = 0.50 // fraction of loans in long-term maturity bucket

  case class Input(
      w: World,
      s1: FiscalConstraintStep.Output,
      s2: LaborDemographicsStep.Output,
      s3: HouseholdIncomeStep.Output,
      s4: DemandStep.Output,
      s5: FirmProcessingStep.Output,
      s6: HouseholdFinancialStep.Output,
      s7: PriceEquityStep.Output,
      s8: OpenEconomyStep.Output,
  )

  case class Output(
      resolvedBank: Banking.Aggregate,
      finalBankingSector: Banking.State,
      reassignedFirms: Vector[Firm.State],
      reassignedHouseholds: Vector[Household.State],
      finalPpk: SocialSecurity.PpkState,
      finalInsurance: Insurance.State,
      finalNbfi: Nbfi.State,
      newGovWithYield: FiscalBudget.GovState,
      newJst: Jst.State,
      housingAfterFlows: HousingMarket.State,
      bfgLevy: PLN,
      bailInLoss: PLN,
      multiCapDestruction: PLN,
      monAgg: Option[Banking.MonetaryAggregates],
      finalHhAgg: Household.Aggregates,
      // Tax intermediates (needed by SFC check)
      vat: PLN,
      vatAfterEvasion: PLN,
      pitAfterEvasion: PLN,
      exciseRevenue: PLN,
      exciseAfterEvasion: PLN,
      customsDutyRevenue: PLN,
      effectiveShadowShare: Ratio,
      // Housing flows (needed by SFC check)
      mortgageInterestIncome: PLN,
      mortgagePrincipal: PLN,
      mortgageDefaultLoss: PLN,
      mortgageDefaultAmount: PLN,
      // Other intermediates (needed by SFC/World assembly)
      jstDepositChange: PLN,
      investNetDepositFlow: PLN,
      actualBondChange: PLN,
  )

  // --- Intermediate result types for sub-methods ---

  private case class GovJstResult(
      newGovWithYield: FiscalBudget.GovState, // updated government state with bond yield
      newJst: Jst.State,                      // updated local government (JST) state
      jstDepositChange: PLN,                  // net JST deposit flow into banking sector
      tax: TaxRevenue.Output,                 // computed tax revenues (VAT, excise, customs, PIT)
  )

  private case class HousingResult(
      housingAfterFlows: HousingMarket.State,    // housing market state after origination and mortgage flows
      mortgageFlows: HousingMarket.MortgageFlows, // monthly mortgage cash flows (interest, principal, defaults)
  )

  private case class BondAllocations(
      finalPpk: SocialSecurity.PpkState, // PPK state after gov bond purchase
      finalInsurance: Insurance.State,   // insurance state after gov bond allocation
      finalNbfi: Nbfi.State,             // NBFI/TFI state after gov bond allocation
      ppkBondPurchase: PLN,              // PPK gov bond purchase (supply-capped)
      insBondPurchase: PLN,              // insurance sector gov bond purchase (supply-capped)
      tfiBondPurchase: PLN,              // TFI fund gov bond purchase (supply-capped)
      actualBondChange: PLN,             // net change in government bonds outstanding
  )

  private case class PerBankHhFlows(
      incomeShare: PLN,   // household income allocated to this bank
      consShare: PLN,     // household consumption allocated to this bank
      hhDebtService: PLN, // mortgage debt service payments to this bank
      depInterest: PLN,   // deposit interest paid by this bank to households
      ccDebtService: PLN, // consumer credit debt service to this bank
      ccOrigination: PLN, // new consumer credit originated at this bank
      ccDefault: PLN,     // consumer credit defaults at this bank
  )

  private case class MultiBankResult(
      finalBankingSector: Banking.State,             // final banking sector state after interbank clearing and resolution
      reassignedFirms: Vector[Firm.State],           // firms reassigned from failed banks to absorber bank
      reassignedHouseholds: Vector[Household.State], // households reassigned from failed banks to absorber bank
      bailInLoss: PLN,                               // total bail-in losses imposed on depositors
      multiCapDestruction: PLN,                      // capital destroyed by bank failures this month
      resolvedBank: Banking.Aggregate,               // aggregate banking sector after resolution
  )

  def run(in: Input)(using p: SimParams): Output =
    val govJst               = computeGovAndJst(in)
    val housing              = computeHousingFlows(in)
    val bfgLevy              =
      if p.flags.bankFailure then Banking.computeBfgLevy(in.w.bankingSector.banks).total
      else PLN.Zero
    val investNetDepositFlow = computeInvestNetDepositFlow(in)
    val newBank              =
      computeAggregateBank(in, housing.mortgageFlows, bfgLevy, investNetDepositFlow, govJst.jstDepositChange)
    val finalHhAgg           = computeHhAgg(in)
    val bonds                = computeBondAllocations(in, newBank, govJst.newGovWithYield)
    val multi                = processMultiBankPath(
      in,
      govJst.jstDepositChange,
      investNetDepositFlow,
      housing.mortgageFlows,
      bonds,
    )
    val monAgg               = computeMonetaryAggregates(multi.finalBankingSector, multi.resolvedBank)

    Output(
      resolvedBank = multi.resolvedBank,
      finalBankingSector = multi.finalBankingSector,
      reassignedFirms = multi.reassignedFirms,
      reassignedHouseholds = multi.reassignedHouseholds,
      finalPpk = bonds.finalPpk,
      finalInsurance = bonds.finalInsurance,
      finalNbfi = bonds.finalNbfi,
      newGovWithYield = govJst.newGovWithYield,
      newJst = govJst.newJst,
      housingAfterFlows = housing.housingAfterFlows,
      bfgLevy = bfgLevy,
      bailInLoss = multi.bailInLoss,
      multiCapDestruction = multi.multiCapDestruction,
      monAgg = monAgg,
      finalHhAgg = finalHhAgg,
      vat = PLN(govJst.tax.vat),
      vatAfterEvasion = PLN(govJst.tax.vatAfterEvasion),
      pitAfterEvasion = PLN(govJst.tax.pitAfterEvasion),
      exciseRevenue = PLN(govJst.tax.exciseRevenue),
      exciseAfterEvasion = PLN(govJst.tax.exciseAfterEvasion),
      customsDutyRevenue = PLN(govJst.tax.customsDutyRevenue),
      effectiveShadowShare = Ratio(govJst.tax.effectiveShadowShare),
      mortgageInterestIncome = housing.mortgageFlows.interest,
      mortgagePrincipal = housing.mortgageFlows.principal,
      mortgageDefaultLoss = housing.mortgageFlows.defaultLoss,
      mortgageDefaultAmount = housing.mortgageFlows.defaultAmount,
      jstDepositChange = govJst.jstDepositChange,
      investNetDepositFlow = investNetDepositFlow,
      actualBondChange = bonds.actualBondChange,
    )

  /** Government budget update (deficit, debt, bonds) and JST local government
    * step.
    */
  private def computeGovAndJst(in: Input)(using p: SimParams): GovJstResult =
    val tax = TaxRevenue.compute(
      TaxRevenue.Input(
        consumption = in.s3.consumption.toDouble,
        pitRevenue = in.s3.pitRevenue.toDouble,
        totalImports = in.s8.external.newBop.totalImports.toDouble,
        informalCyclicalAdj = in.w.mechanisms.informalCyclicalAdj,
      ),
    )

    val unempBenefitSpend   = in.s3.hhAgg.totalUnempBenefits
    val socialTransferSpend =
      if p.flags.social800 then in.s3.hhAgg.totalSocialTransfers
      else PLN.Zero

    val newGov          = FiscalBudget.update(
      FiscalBudget.Input(
        prev = in.w.gov,
        priceLevel = in.s7.newPrice,
        citPaid = in.s5.sumTax + in.s7.dividendTax + PLN(tax.pitAfterEvasion),
        vat = PLN(tax.vatAfterEvasion),
        nbpRemittance = in.s8.banking.nbpRemittance,
        exciseRevenue = PLN(tax.exciseAfterEvasion),
        customsDutyRevenue = PLN(tax.customsDutyRevenue),
        unempBenefitSpend = unempBenefitSpend,
        debtService = in.s8.banking.monthlyDebtService,
        zusGovSubvention = in.s2.newZus.govSubvention,
        socialTransferSpend = socialTransferSpend,
        euCofinancing = in.s7.euCofin,
        euProjectCapital = in.s7.euProjectCapital,
        govPurchasesActual = in.s4.govPurchases,
      ),
    )
    val newGovWithYield = newGov.copy(bondYield = in.s8.monetary.newBondYield)

    val nLivingFirms = in.s5.ioFirms.count(Firm.isAlive)
    val jstResult    =
      Jst.step(
        in.w.social.jst,
        newGovWithYield.taxRevenue,
        in.s3.totalIncome,
        in.s7.gdp,
        nLivingFirms,
        PLN(tax.pitAfterEvasion),
      )

    GovJstResult(
      newGovWithYield = newGovWithYield,
      newJst = jstResult.state,
      jstDepositChange = jstResult.depositChange,
      tax = tax,
    )

  /** Housing market: price step, origination, mortgage flows. */
  private def computeHousingFlows(in: Input)(using p: SimParams): HousingResult =
    val unempRate                = 1.0 - in.s2.employed.toDouble / in.w.totalPopulation
    val prevMortgageRate         = in.w.real.housing.avgMortgageRate
    val mortgageBaseRate: Double =
      if p.flags.interbankTermStructure then YieldCurve.compute(in.w.bankingSector.interbankRate.toDouble).wibor3m.toDouble
      else in.w.nbp.referenceRate.toDouble
    val mortgageRate             = mortgageBaseRate + p.housing.mortgageSpread.toDouble

    val mortgageRateTyped = Rate(mortgageRate)
    val housingAfterPrice = HousingMarket.step(
      HousingMarket.StepInput(
        prev = in.w.real.housing,
        mortgageRate = mortgageRateTyped,
        inflation = in.s7.newInfl,
        incomeGrowth = Rate(in.s2.wageGrowth.toDouble),
        employed = in.s2.employed,
        prevMortgageRate = prevMortgageRate,
      ),
    )
    val housingAfterOrig  =
      HousingMarket.processOrigination(housingAfterPrice, in.s3.totalIncome, mortgageRateTyped, true)
    val mortgageFlows     = HousingMarket.processMortgageFlows(housingAfterOrig, mortgageRateTyped, Ratio(unempRate))
    val housingAfterFlows = HousingMarket.applyFlows(housingAfterOrig, mortgageFlows)

    HousingResult(housingAfterFlows = housingAfterFlows, mortgageFlows = mortgageFlows)

  /** Investment net deposit flow: lagged demand minus current domestic
    * investment.
    */
  private def computeInvestNetDepositFlow(in: Input)(using p: SimParams): PLN =
    val currentInvestDomestic = in.s5.sumGrossInvestment * (1.0 - p.capital.importShare.toDouble) +
      in.s5.sumGreenInvestment * (1.0 - p.climate.greenImportShare.toDouble)
    in.s4.laggedInvestDemand - currentInvestDomestic

  /** Single-bank aggregate path: capital PnL, loans, deposits, consumer credit.
    */
  private def computeAggregateBank(
      in: Input,
      mortgageFlows: HousingMarket.MortgageFlows,
      bfgLevy: PLN,
      investNetDepositFlow: PLN,
      jstDepositChange: PLN,
  )(using p: SimParams): Banking.Aggregate =
    val aggCapitalPnl = Banking.computeCapitalDelta(
      Banking.CapitalPnlInput(
        prevCapital = in.w.bank.capital,
        nplLoss = in.s5.nplLoss,
        mortgageNplLoss = mortgageFlows.defaultLoss,
        consumerNplLoss = in.s6.consumerNplLoss,
        corpBondDefaultLoss = in.s8.corpBonds.corpBondBankDefaultLoss,
        bfgLevy = bfgLevy,
        intIncome = in.s5.intIncome,
        hhDebtService = in.s6.hhDebtService,
        bondIncome = in.s8.banking.bankBondIncome,
        depositInterest = in.s6.depositInterestPaid,
        reserveInterest = in.s8.banking.totalReserveInterest,
        standingFacilityIncome = in.s8.banking.totalStandingFacilityIncome,
        interbankInterest = in.s8.banking.totalInterbankInterest,
        mortgageInterestIncome = mortgageFlows.interest,
        consumerDebtService = in.s6.consumerDebtService,
        corpBondCoupon = in.s8.corpBonds.corpBondBankCoupon,
      ),
    )
    in.w.bank.copy(
      totalLoans = (in.w.bank.totalLoans + in.s5.sumNewLoans - in.s5.sumFirmPrincipal - in.s5.nplNew * p.banking.loanRecovery.toDouble).max(PLN.Zero),
      nplAmount = (in.w.bank.nplAmount + in.s5.nplNew - in.w.bank.nplAmount * NplMonthlyWriteOff).max(PLN.Zero),
      capital = aggCapitalPnl.newCapital,
      deposits = in.w.bank.deposits + (in.s3.totalIncome - in.s3.consumption) + investNetDepositFlow
        + jstDepositChange
        + in.s7.netDomesticDividends - in.s7.foreignDividendOutflow - in.s6.remittanceOutflow + in.s6.diasporaInflow
        + in.s6.tourismExport - in.s6.tourismImport
        + in.s5.sumNewLoans - in.s5.sumFirmPrincipal
        + in.s6.consumerOrigination + in.s8.nonBank.insNetDepositChange + in.s8.nonBank.nbfiDepositDrain,
      consumerLoans = (in.w.bank.consumerLoans + in.s6.consumerOrigination - in.s6.consumerPrincipal - in.s6.consumerDefaultAmt).max(PLN.Zero),
      consumerNpl = (in.w.bank.consumerNpl + in.s6.consumerDefaultAmt - in.w.bank.consumerNpl * NplMonthlyWriteOff).max(PLN.Zero),
      corpBondHoldings = in.s8.corpBonds.newCorpBonds.bankHoldings,
    )

  /** Recompute household aggregates from final households. */
  private def computeHhAgg(in: Input)(using SimParams): Household.Aggregates =
    Household.computeAggregates(
      in.s5.households,
      in.s2.newWage,
      in.s1.resWage,
      in.s3.importAdj,
      in.s3.hhAgg.retrainingAttempts,
      in.s3.hhAgg.retrainingSuccesses,
    )

  /** PPK, insurance, and TFI gov bond purchases, each capped at available
    * supply.
    */
  private def computeBondAllocations(
      in: Input,
      newBank: Banking.Aggregate,
      newGovWithYield: FiscalBudget.GovState,
  )(using p: SimParams): BondAllocations =
    val actualBondChange: PLN    = newGovWithYield.bondsOutstanding - in.w.gov.bondsOutstanding
    val bondChangeForSupply: PLN = if p.flags.govBondMarket then actualBondChange else PLN.Zero
    val bondSupplyBase           = newBank.govBondHoldings + bondChangeForSupply - in.s8.monetary.qePurchaseAmount

    // PPK bond purchases (capped at available bonds after QE)
    val ppkBondPurchase = in.s2.rawPpkBondPurchase.min(bondSupplyBase.max(PLN.Zero))
    val finalPpk        = in.s2.newPpk.copy(bondHoldings = in.w.social.ppk.bondHoldings + ppkBondPurchase)

    // Insurance gov bond purchases (capped at available bonds after QE + PPK)
    val insGovBondDelta      = in.s8.nonBank.newInsurance.govBondHoldings - in.w.financial.insurance.govBondHoldings
    val availableBondsForIns = bondSupplyBase - ppkBondPurchase
    val insBondPurchase      = insGovBondDelta.max(PLN.Zero).min(availableBondsForIns.max(PLN.Zero))
    val finalInsurance       =
      in.s8.nonBank.newInsurance.copy(govBondHoldings = in.w.financial.insurance.govBondHoldings + insBondPurchase)

    // TFI gov bond purchases (#42)
    val tfiGovBondDelta      = in.s8.nonBank.newNbfi.tfiGovBondHoldings - in.w.financial.nbfi.tfiGovBondHoldings
    val availableBondsForTfi = bondSupplyBase - ppkBondPurchase - insBondPurchase
    val tfiBondPurchase      = tfiGovBondDelta.max(PLN.Zero).min(availableBondsForTfi.max(PLN.Zero))
    val finalNbfi            =
      in.s8.nonBank.newNbfi.copy(tfiGovBondHoldings = in.w.financial.nbfi.tfiGovBondHoldings + tfiBondPurchase)

    BondAllocations(
      finalPpk,
      finalInsurance,
      finalNbfi,
      ppkBondPurchase = ppkBondPurchase,
      insBondPurchase = insBondPurchase,
      tfiBondPurchase = tfiBondPurchase,
      actualBondChange = actualBondChange,
    )

  /** Resolve per-bank household flows from tracked data or worker-share
    * fallback.
    */
  private def resolvePerBankHhFlows(
      bId: Int,
      perBankHhFlowsOpt: Option[Vector[PerBankFlow]],
      totalWorkers: Double,
      perBankWorkers: Vector[Int],
      in: Input,
  ): PerBankHhFlows =
    perBankHhFlowsOpt match
      case Some(pbf) =>
        val f = pbf(bId)
        PerBankHhFlows(
          incomeShare = f.income,
          consShare = f.consumption,
          hhDebtService = f.debtService,
          depInterest = f.depositInterest,
          ccDebtService = f.consumerDebtService,
          ccOrigination = f.consumerOrigination,
          ccDefault = f.consumerDefault,
        )
      case None      =>
        val ws = if totalWorkers > 0 then perBankWorkers(bId) / totalWorkers else 0.0
        PerBankHhFlows(
          incomeShare = in.s3.totalIncome * ws,
          consShare = in.s3.consumption * ws,
          hhDebtService = in.s6.hhDebtService * ws,
          depInterest = PLN.Zero,
          ccDebtService = in.s6.consumerDebtService * ws,
          ccOrigination = in.s6.consumerOrigination * ws,
          ccDefault = in.s6.consumerDefaultAmt * ws,
        )

  /** Compute updated state for a single bank in the multi-bank path. */
  private def updateSingleBank(
      b: Banking.BankState,
      hhFlows: PerBankHhFlows,
      workerShare: Ratio,
      mortgageFlows: HousingMarket.MortgageFlows,
      perBankReserveInt: Banking.PerBankAmounts,
      perBankStandingFac: Banking.PerBankAmounts,
      perBankInterbankInt: Banking.PerBankAmounts,
      jstDepositChange: PLN,
      investNetDepositFlow: PLN,
      in: Input,
  )(using p: SimParams): Banking.BankState =
    val bId           = b.id.toInt
    val ws            = workerShare.toDouble
    val bankNplNew    = PLN(in.s5.perBankNplDebt(bId))   // Vector[Double] → PLN at boundary
    val bankNplLoss   = bankNplNew * (1.0 - p.banking.loanRecovery.toDouble)
    val bankIntIncome = PLN(in.s5.perBankIntIncome(bId)) // Vector[Double] → PLN at boundary
    val bankBondInc   = b.govBondHoldings * in.s8.monetary.newBondYield / 12.0
    val bankResInt    = perBankReserveInt.perBank(bId)
    val bankSfInc     = perBankStandingFac.perBank(bId)
    val bankIbInt     = perBankInterbankInt.perBank(bId)
    val newLoansTotal =
      (b.loans + in.s5.perBankNewLoans(bId) - in.s5.perBankFirmPrincipal(bId) - bankNplNew * p.banking.loanRecovery.toDouble).max(PLN.Zero)

    val newDep = b.deposits + (hhFlows.incomeShare - hhFlows.consShare) +
      investNetDepositFlow * ws + jstDepositChange * ws +
      in.s7.netDomesticDividends * ws - in.s7.foreignDividendOutflow * ws -
      in.s6.remittanceOutflow * ws + in.s6.diasporaInflow * ws +
      in.s6.tourismExport * ws - in.s6.tourismImport * ws +
      in.s5.perBankNewLoans(bId) - in.s5.perBankFirmPrincipal(bId) +
      hhFlows.ccOrigination +
      in.s8.nonBank.insNetDepositChange * ws + in.s8.nonBank.nbfiDepositDrain * ws

    val bankMortgageIntIncome   = mortgageFlows.interest * ws
    val bankMortgageNplLoss     = mortgageFlows.defaultLoss * ws
    val bankCcNplLoss           = hhFlows.ccDefault * (1.0 - p.household.ccNplRecovery.toDouble)
    val bankCcPrincipal: PLN    = in.s3.perBankHhFlowsOpt match
      case Some(pbf) => pbf(bId).consumerPrincipal
      case _         =>
        val ccAmort       = p.household.ccAmortRate.toDouble
        val ccMonthlyRate = (in.s1.lendingBaseRate.toDouble + p.household.ccSpread.toDouble) / 12.0
        if ccAmort + ccMonthlyRate > 0 then hhFlows.ccDebtService * (ccAmort / (ccAmort + ccMonthlyRate))
        else PLN.Zero
    val bankCorpBondCoupon      = in.s8.corpBonds.corpBondBankCoupon * ws
    val bankCorpBondDefaultLoss = in.s8.corpBonds.corpBondBankDefaultLoss * ws
    val bankBfgLevy             =
      if p.flags.bankFailure && !b.failed then b.deposits * p.banking.bfgLevyRate.toDouble / 12.0
      else PLN.Zero

    val capitalPnl = Banking.computeCapitalDelta(
      Banking.CapitalPnlInput(
        prevCapital = b.capital,
        nplLoss = bankNplLoss,
        mortgageNplLoss = bankMortgageNplLoss,
        consumerNplLoss = bankCcNplLoss,
        corpBondDefaultLoss = bankCorpBondDefaultLoss,
        bfgLevy = bankBfgLevy,
        intIncome = bankIntIncome,
        hhDebtService = hhFlows.hhDebtService,
        bondIncome = bankBondInc,
        depositInterest = hhFlows.depInterest,
        reserveInterest = bankResInt,
        standingFacilityIncome = bankSfInc,
        interbankInterest = bankIbInt,
        mortgageInterestIncome = bankMortgageIntIncome,
        consumerDebtService = hhFlows.ccDebtService,
        corpBondCoupon = bankCorpBondCoupon,
      ),
    )

    b.copy(
      loans = newLoansTotal,
      nplAmount = (b.nplAmount + bankNplNew - b.nplAmount * NplMonthlyWriteOff).max(PLN.Zero),
      capital = capitalPnl.newCapital,
      deposits = newDep,
      demandDeposits = newDep * (1.0 - p.banking.termDepositFrac.toDouble),
      termDeposits = newDep * p.banking.termDepositFrac.toDouble,
      loansShort = newLoansTotal * ShortLoanFrac,
      loansMedium = newLoansTotal * MediumLoanFrac,
      loansLong = newLoansTotal * LongLoanFrac,
      consumerLoans = (b.consumerLoans + hhFlows.ccOrigination - bankCcPrincipal - hhFlows.ccDefault).max(PLN.Zero),
      consumerNpl = (b.consumerNpl + hhFlows.ccDefault - b.consumerNpl * NplMonthlyWriteOff).max(PLN.Zero),
      corpBondHoldings = in.s8.corpBonds.newCorpBonds.bankHoldings * ws,
    )

  /** Multi-bank update: per-bank loop, interbank clearing, bond allocation,
    * failure resolution.
    */
  private def processMultiBankPath(
      in: Input,
      jstDepositChange: PLN,
      investNetDepositFlow: PLN,
      mortgageFlows: HousingMarket.MortgageFlows,
      bonds: BondAllocations,
  )(using p: SimParams): MultiBankResult =
    val bs                  = in.w.bankingSector
    val perBankReserveInt   = Banking.computeReserveInterest(bs.banks, in.w.nbp.referenceRate)
    val perBankStandingFac  = Banking.computeStandingFacilities(bs.banks, in.w.nbp.referenceRate)
    val perBankInterbankInt = Banking.interbankInterestFlows(bs.banks, bs.interbankRate)
    val totalWorkers        = in.s5.perBankWorkers.kahanSumBy(_.toDouble)

    val updatedBanks = bs.banks.map { b =>
      val bId         = b.id.toInt
      val workerShare = Ratio(if totalWorkers > 0 then in.s5.perBankWorkers(bId) / totalWorkers else 0.0)
      val hhFlows     = resolvePerBankHhFlows(bId, in.s3.perBankHhFlowsOpt, totalWorkers, in.s5.perBankWorkers, in)
      updateSingleBank(
        b,
        hhFlows,
        workerShare,
        mortgageFlows,
        perBankReserveInt,
        perBankStandingFac,
        perBankInterbankInt,
        jstDepositChange,
        investNetDepositFlow,
        in,
      )
    }

    processInterbankAndFailures(in, updatedBanks, bs, bonds)

  /** Interbank clearing, bond allocation, QE, failure check, bail-in,
    * resolution, reassignment.
    */
  private def processInterbankAndFailures(
      in: Input,
      updatedBanks: Vector[Banking.BankState],
      bs: Banking.State,
      bonds: BondAllocations,
  )(using p: SimParams): MultiBankResult =
    val ibRate               = Banking.interbankRate(updatedBanks, in.w.nbp.referenceRate)
    val afterInterbank       = Banking.clearInterbank(updatedBanks, bs.configs)
    val afterBonds           =
      if p.flags.govBondMarket then Banking.allocateBonds(afterInterbank, bonds.actualBondChange)
      else afterInterbank
    val afterQe              = Banking.allocateQePurchases(afterBonds, in.s8.monetary.qePurchaseAmount)
    val afterPpk             = Banking.allocateQePurchases(afterQe, bonds.ppkBondPurchase)
    val afterIns             = Banking.allocateQePurchases(afterPpk, bonds.insBondPurchase)
    val afterTfi             = Banking.allocateQePurchases(afterIns, bonds.tfiBondPurchase)
    val failResult           =
      Banking.checkFailures(afterTfi, in.s1.m, p.flags.bankFailure, in.s7.newMacropru.ccyb)
    val afterFailCheck       = failResult.banks
    val anyFailed            = failResult.anyFailed
    val bailInResult         =
      if anyFailed then Banking.applyBailIn(afterFailCheck) else Banking.BailInResult(afterFailCheck, PLN.Zero)
    val resolveResult        =
      if anyFailed then Banking.resolveFailures(bailInResult.banks)
      else Banking.ResolutionResult(bailInResult.banks, BankId.NoBank)
    val afterResolve         = resolveResult.banks
    val rawAbsorberId        = resolveResult.absorberId
    val absorberId           =
      if rawAbsorberId.toInt >= 0 then rawAbsorberId
      else Banking.healthiestBankId(afterResolve)
    val multiCapDest: PLN    =
      if anyFailed then
        PLN(
          afterTfi
            .zip(afterFailCheck)
            .map { case (pre, post) =>
              if !pre.failed && post.failed then pre.capital.toDouble else 0.0
            }
            .kahanSum,
        )
      else PLN.Zero
    val curve                = if p.flags.interbankTermStructure then Some(YieldCurve.compute(ibRate.toDouble)) else None
    val finalBankingSector   = bs.copy(banks = afterResolve, interbankRate = ibRate, interbankCurve = curve)
    val reassignedFirms      =
      if anyFailed then
        in.s7.rewiredFirms.map: f =>
          if f.bankId.toInt < afterResolve.length && afterResolve(f.bankId.toInt).failed then f.copy(bankId = absorberId)
          else f
      else in.s7.rewiredFirms
    val reassignedHouseholds =
      if anyFailed then
        in.s5.households.map: h =>
          if h.bankId.toInt < afterResolve.length && afterResolve(h.bankId.toInt).failed then h.copy(bankId = absorberId)
          else h
      else in.s5.households

    MultiBankResult(
      finalBankingSector = finalBankingSector,
      reassignedFirms = reassignedFirms,
      reassignedHouseholds = reassignedHouseholds,
      bailInLoss = bailInResult.totalLoss,
      multiCapDestruction = multiCapDest,
      resolvedBank = finalBankingSector.aggregate,
    )

  /** Monetary aggregates (M1/M2/M3) when credit diagnostics enabled. */
  private def computeMonetaryAggregates(
      finalBankingSector: Banking.State,
      resolvedBank: Banking.Aggregate,
  )(using p: SimParams): Option[Banking.MonetaryAggregates] =
    if p.flags.creditDiagnostics then
      val totalReserves = PLN(finalBankingSector.banks.kahanSumBy(_.reservesAtNbp.toDouble))
      Some(Banking.MonetaryAggregates.compute(resolvedBank.deposits, totalReserves))
    else None
