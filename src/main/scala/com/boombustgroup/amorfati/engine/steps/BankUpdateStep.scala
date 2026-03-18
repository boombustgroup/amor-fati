package com.boombustgroup.amorfati.engine.steps

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.*
import com.boombustgroup.amorfati.engine.markets.{BondAuction, FiscalBudget, HousingMarket}
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
      w: World,                          // current world state (pre-step)
      s1: FiscalConstraintStep.Output,   // fiscal constraint (month, lending base rate, res wage)
      s2: LaborDemographicsStep.Output,  // labor/demographics (employment, wage, immigration)
      s3: HouseholdIncomeStep.Output,    // household income (consumption, PIT, debt service)
      s4: DemandStep.Output,             // demand (sector multipliers, gov purchases)
      s5: FirmProcessingStep.Output,     // firm processing (loans, NPL, bonds, I-O firms)
      s6: HouseholdFinancialStep.Output, // household financial (remittances, tourism, consumer credit)
      s7: PriceEquityStep.Output,        // price/equity (inflation, GDP, equity state, macropru)
      s8: OpenEconomyStep.Output,        // open economy (NBP rate, bond yield, QE, FX, BoP)
  )

  case class Output(
      resolvedBank: Banking.Aggregate,               // aggregate bank balance sheet after resolution
      finalBankingSector: Banking.State,             // full banking sector state (per-bank + interbank)
      reassignedFirms: Vector[Firm.State],           // firms with bankId reassigned after bank failure
      reassignedHouseholds: Vector[Household.State], // HH with bankId reassigned after bank failure
      finalNbp: Nbp.State,                           // NBP state after QE bond purchase (waterfall)
      finalPpk: SocialSecurity.PpkState,             // PPK state after bond purchases
      finalInsurance: Insurance.State,               // insurance state after asset allocation
      finalNbfi: Nbfi.State,                         // NBFI/TFI state after bond purchases
      newGovWithYield: FiscalBudget.GovState,        // gov state with updated bond yield
      newJst: Jst.State,                             // local government state
      housingAfterFlows: HousingMarket.State,        // housing market after mortgage flows
      bfgLevy: PLN,                                  // BFG resolution fund levy (aggregate)
      bailInLoss: PLN,                               // bail-in deposit destruction (aggregate)
      multiCapDestruction: PLN,                      // capital wiped when banks fail
      monAgg: Option[Banking.MonetaryAggregates],    // M0/M1/M2/M3 (when credit diagnostics on)
      finalHhAgg: Household.Aggregates,              // recomputed HH aggregates
      vat: PLN,                                      // gross VAT revenue
      vatAfterEvasion: PLN,                          // VAT after informal evasion
      pitAfterEvasion: PLN,                          // PIT after informal evasion
      exciseRevenue: PLN,                            // gross excise revenue
      exciseAfterEvasion: PLN,                       // excise after informal evasion
      customsDutyRevenue: PLN,                       // customs duty revenue
      effectiveShadowShare: Ratio,                   // effective shadow economy share
      mortgageInterestIncome: PLN,                   // mortgage interest income (bank share)
      mortgagePrincipal: PLN,                        // mortgage principal repaid
      mortgageDefaultLoss: PLN,                      // mortgage default loss (bank share)
      mortgageDefaultAmount: PLN,                    // gross mortgage default amount
      jstDepositChange: PLN,                         // JST deposit flow (Identity 2)
      investNetDepositFlow: PLN,                     // investment demand net deposit flow
      actualBondChange: PLN,                         // net change in gov bonds outstanding
      unrealizedBondLoss: PLN,                       // mark-to-market loss on gov bond portfolio (interest rate risk channel)
      htmRealizedLoss: PLN,                          // realized loss from HTM forced reclassification
      newQuasiFiscal: QuasiFiscal.State,             // BGK/PFR after issuance and lending
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

  /** Inputs for the bond waterfall — raw requests, not final allocations. */
  private case class BondWaterfallInputs(
      actualBondChange: PLN, // Δ gov bonds outstanding (from FiscalBudget)
      qeRequested: PLN,      // NBP QE purchase request
      ppkRequested: PLN,     // PPK bond purchase request
      insRequested: PLN,     // insurance bond purchase request (delta)
      tfiRequested: PLN,     // TFI bond purchase request (delta)
      erChange: Ratio,       // month-on-month ER change (for foreign demand)
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
      htmRealizedLoss: PLN,                          // realized loss from HTM forced reclassification
      // Bond waterfall outputs — single source of truth for buyer holdings
      finalNbp: Nbp.State,                           // NBP after QE bond purchase (govBondHoldings updated)
      finalPpk: SocialSecurity.PpkState,             // PPK after bond purchase
      finalInsurance: Insurance.State,               // insurance after bond purchase
      finalNbfi: Nbfi.State,                         // NBFI/TFI after bond purchase
      actualBondChange: PLN,                         // net change in gov bonds outstanding
      foreignBondHoldings: PLN,                      // non-resident holdings after auction
      bidToCover: Ratio,                             // bond auction bid-to-cover ratio
  )

  def run(in: Input)(using p: SimParams): Output =
    val govJst               = computeGovAndJst(in)
    val housing              = computeHousingFlows(in)
    val bfgLevy              =
      if p.flags.bankFailure then Banking.computeBfgLevy(in.w.bankingSector.banks).total
      else PLN.Zero
    val investNetDepositFlow = computeInvestNetDepositFlow(in)
    val finalHhAgg           = computeHhAgg(in)
    val wf                   = computeWaterfallInputs(in, govJst.newGovWithYield)
    val multi                = processMultiBankPath(
      in,
      govJst.jstDepositChange,
      investNetDepositFlow,
      housing.mortgageFlows,
      wf,
    )
    val monAgg               = computeMonetaryAggregates(multi.finalBankingSector, in)

    val newQuasiFiscal =
      if p.flags.quasiFiscal then
        QuasiFiscal.step(
          in.w.financial.quasiFiscal,
          govJst.newGovWithYield.govCapitalSpend,
          govJst.newGovWithYield.euCofinancing,
          in.w.nbp.qeActive,
        )
      else in.w.financial.quasiFiscal

    Output(
      resolvedBank = multi.resolvedBank,
      finalBankingSector = multi.finalBankingSector,
      reassignedFirms = multi.reassignedFirms,
      reassignedHouseholds = multi.reassignedHouseholds,
      finalNbp = multi.finalNbp,
      finalPpk = multi.finalPpk,
      finalInsurance = multi.finalInsurance,
      finalNbfi = multi.finalNbfi,
      newGovWithYield = govJst.newGovWithYield.copy(foreignBondHoldings = multi.foreignBondHoldings),
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
      actualBondChange = multi.actualBondChange,
      unrealizedBondLoss = {
        val yc = (in.s8.monetary.newBondYield - in.w.gov.bondYield).toDouble
        if yc > 0 then in.w.bank.afsBonds * (yc * p.banking.govBondDuration) else PLN.Zero
      },
      htmRealizedLoss = multi.htmRealizedLoss,
      newQuasiFiscal = newQuasiFiscal,
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
        nfzGovSubvention = in.s2.newNfz.govSubvention,
        socialTransferSpend = socialTransferSpend,
        euCofinancing = in.s7.euCofin,
        euProjectCapital = in.s7.euProjectCapital,
        govPurchasesActual = in.s4.govPurchases,
      ),
    )
    val newGovWithYield = newGov.copy(bondYield = in.s8.monetary.newBondYield, weightedCoupon = in.s8.monetary.newWeightedCoupon)

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
    val unempRate              = 1.0 - in.s2.employed.toDouble / in.w.totalPopulation
    val prevMortgageRate       = in.w.real.housing.avgMortgageRate
    val mortgageBaseRate: Rate =
      if p.flags.interbankTermStructure then
        val exp = in.w.mechanisms.expectations
        YieldCurve
          .compute(
            in.w.bankingSector.interbankRate,
            nplRatio = in.w.bank.nplRatio,
            credibility = exp.credibility,
            expectedInflation = exp.expectedInflation,
            targetInflation = p.monetary.targetInfl,
          )
          .wibor3m
      else in.w.nbp.referenceRate
    val mortgageRate           = mortgageBaseRate + p.housing.mortgageSpread
    val housingAfterPrice      = HousingMarket.step(
      HousingMarket.StepInput(
        prev = in.w.real.housing,
        mortgageRate = mortgageRate,
        inflation = in.s7.newInfl,
        incomeGrowth = Rate(in.s2.wageGrowth.toDouble),
        employed = in.s2.employed,
        prevMortgageRate = prevMortgageRate,
      ),
    )
    val housingAfterOrig       =
      HousingMarket.processOrigination(housingAfterPrice, in.s3.totalIncome, mortgageRate, true)
    val mortgageFlows          = HousingMarket.processMortgageFlows(housingAfterOrig, mortgageRate, Ratio(unempRate))
    val housingAfterFlows      = HousingMarket.applyFlows(housingAfterOrig, mortgageFlows)

    HousingResult(housingAfterFlows = housingAfterFlows, mortgageFlows = mortgageFlows)

  /** Investment net deposit flow: lagged demand minus current domestic
    * investment.
    */
  private def computeInvestNetDepositFlow(in: Input)(using p: SimParams): PLN =
    val currentInvestDomestic = in.s5.sumGrossInvestment * (1.0 - p.capital.importShare.toDouble) +
      in.s5.sumGreenInvestment * (1.0 - p.climate.greenImportShare.toDouble)
    in.s4.laggedInvestDemand - currentInvestDomestic

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

  /** Compute raw bond waterfall inputs — requests only, no final allocations.
    * Actual allocations happen in processInterbankAndFailures via sellToBuyer.
    */
  private def computeWaterfallInputs(
      in: Input,
      newGovWithYield: FiscalBudget.GovState,
  ): BondWaterfallInputs =
    val actualBondChange = newGovWithYield.bondsOutstanding - in.w.gov.bondsOutstanding
    val insRequested     = (in.s8.nonBank.newInsurance.govBondHoldings - in.w.financial.insurance.govBondHoldings).max(PLN.Zero)
    val tfiRequested     = (in.s8.nonBank.newNbfi.tfiGovBondHoldings - in.w.financial.nbfi.tfiGovBondHoldings).max(PLN.Zero)
    val prevEr           = in.w.forex.exchangeRate
    val currEr           = in.s8.external.newForex.exchangeRate
    val erChange         = if prevEr > 0.0 then Ratio((currEr - prevEr) / prevEr) else Ratio.Zero
    BondWaterfallInputs(
      actualBondChange = actualBondChange,
      qeRequested = in.s8.monetary.qePurchaseAmount,
      ppkRequested = in.s2.rawPpkBondPurchase,
      insRequested = insRequested,
      tfiRequested = tfiRequested,
      erChange = erChange,
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

    // Per-bank mark-to-market loss on AFS bonds only (HTM losses hidden until forced reclassification)
    val bankYieldChange    = (in.s8.monetary.newBondYield - in.w.gov.bondYield).toDouble
    val bankUnrealizedLoss = if bankYieldChange > 0 then b.afsBonds * (bankYieldChange * p.banking.govBondDuration) else PLN.Zero

    val capitalPnl = Banking.computeCapitalDelta(
      Banking.CapitalPnlInput(
        prevCapital = b.capital,
        nplLoss = bankNplLoss,
        mortgageNplLoss = bankMortgageNplLoss,
        consumerNplLoss = bankCcNplLoss,
        corpBondDefaultLoss = bankCorpBondDefaultLoss,
        bfgLevy = bankBfgLevy,
        unrealizedBondLoss = bankUnrealizedLoss,
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
      wf: BondWaterfallInputs,
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

    processInterbankAndFailures(in, updatedBanks, bs, wf)

  /** Interbank clearing, bond allocation, QE, failure check, bail-in,
    * resolution, reassignment.
    */
  private def processInterbankAndFailures(
      in: Input,
      updatedBanks: Vector[Banking.BankState],
      bs: Banking.State,
      wf: BondWaterfallInputs,
  )(using p: SimParams): MultiBankResult =
    val ibRate           = Banking.interbankRate(updatedBanks, in.w.nbp.referenceRate)
    val afterInterbank   = Banking.clearInterbank(updatedBanks, bs.configs)
    val afterFxInjection = distributeFxInjection(afterInterbank, in.s8.monetary.fxPlnInjection)
    // HTM forced reclassification: LCR-stressed banks reclassify HTM→AFS, realizing hidden losses
    val htmResult        = Banking.processHtmForcedSale(afterFxInjection, in.s8.monetary.newBondYield)
    val afterHtm         = htmResult.banks
    val afterBonds       =
      if p.flags.govBondMarket then Banking.allocateBonds(afterHtm, wf.actualBondChange, in.s8.monetary.newBondYield)
      else afterHtm

    // ---- Bond waterfall: single pass, SFC by construction ----
    // Each sellToBuyer removes bonds from banks and returns actualSold.
    // Buyer gets exactly old + actualSold. No speculation, no correction.
    val bankDeposits  = PLN(afterBonds.kahanSumBy(_.deposits.toDouble))
    val auctionResult = BondAuction.auction(
      newIssuance = wf.actualBondChange.max(PLN.Zero),
      bankBondCapacity = bankDeposits * p.fiscal.bankBondAbsorptionShare,
      marketYield = in.s8.monetary.newBondYield,
      erChange = wf.erChange,
    )
    val foreignSale   = Banking.sellToBuyer(afterBonds, auctionResult.foreignAbsorbed)
    val qeSale        = Banking.sellToBuyer(foreignSale.banks, wf.qeRequested)
    val ppkSale       = Banking.sellToBuyer(qeSale.banks, wf.ppkRequested)
    val insSale       = Banking.sellToBuyer(ppkSale.banks, wf.insRequested)
    val tfiSale       = Banking.sellToBuyer(insSale.banks, wf.tfiRequested)

    // Buyer holdings: old + actualSold (single source of truth)
    val finalNbp                 = in.s8.monetary.postFxNbp.copy(
      govBondHoldings = in.s8.monetary.postFxNbp.govBondHoldings + qeSale.actualSold,
      qeCumulative = in.s8.monetary.postFxNbp.qeCumulative + qeSale.actualSold,
    )
    val finalPpk                 = in.s2.newPpk.copy(bondHoldings = in.w.social.ppk.bondHoldings + ppkSale.actualSold)
    val finalInsurance           = in.s8.nonBank.newInsurance.copy(govBondHoldings = in.w.financial.insurance.govBondHoldings + insSale.actualSold)
    val finalNbfi                = in.s8.nonBank.newNbfi.copy(tfiGovBondHoldings = in.w.financial.nbfi.tfiGovBondHoldings + tfiSale.actualSold)
    val finalForeignBondHoldings = in.w.gov.foreignBondHoldings + foreignSale.actualSold

    val failResult           =
      Banking.checkFailures(tfiSale.banks, in.s1.m, p.flags.bankFailure, in.s7.newMacropru.ccyb)
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
          tfiSale.banks
            .zip(afterFailCheck)
            .map { case (pre, post) =>
              if !pre.failed && post.failed then pre.capital.toDouble else 0.0
            }
            .kahanSum,
        )
      else PLN.Zero
    val curve                =
      if p.flags.interbankTermStructure then
        val exp = in.w.mechanisms.expectations
        Some(
          YieldCurve.compute(
            ibRate,
            nplRatio = in.w.bank.nplRatio,
            credibility = exp.credibility,
            expectedInflation = exp.expectedInflation,
            targetInflation = p.monetary.targetInfl,
          ),
        )
      else None
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
      htmRealizedLoss = htmResult.totalRealizedLoss,
      finalNbp = finalNbp,
      finalPpk = finalPpk,
      finalInsurance = finalInsurance,
      finalNbfi = finalNbfi,
      actualBondChange = wf.actualBondChange,
      foreignBondHoldings = finalForeignBondHoldings,
      bidToCover = auctionResult.bidToCover,
    )

  /** Monetary aggregates (M0/M1/M2/M3) when credit diagnostics enabled. */
  private def computeMonetaryAggregates(
      finalBankingSector: Banking.State,
      in: Input,
  )(using p: SimParams): Option[Banking.MonetaryAggregates] =
    if p.flags.creditDiagnostics then
      Some(
        Banking.MonetaryAggregates.compute(
          finalBankingSector.banks,
          in.w.financial.nbfi.tfiAum,
          in.w.financial.corporateBonds.outstanding,
        ),
      )
    else None

  /** Distribute FX intervention PLN injection across banks proportional to
    * deposit market share, adjusting reservesAtNbp. EUR purchase → PLN injected
    * into banking system; EUR sale → PLN drained.
    */
  private def distributeFxInjection(banks: Vector[Banking.BankState], injection: PLN): Vector[Banking.BankState] =
    if injection == PLN.Zero then banks
    else
      val totalDeposits = banks.kahanSumBy(_.deposits.toDouble)
      if totalDeposits <= 0 then banks
      else
        banks.map: b =>
          val share = b.deposits.toDouble / totalDeposits
          b.copy(reservesAtNbp = (b.reservesAtNbp + injection * share).max(PLN.Zero))
