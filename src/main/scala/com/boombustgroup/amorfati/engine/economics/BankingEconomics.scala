package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.*
import com.boombustgroup.amorfati.engine.markets.{BondAuction, FiscalBudget, HousingMarket}
import com.boombustgroup.amorfati.engine.mechanisms.{TaxRevenue, YieldCurve}
import com.boombustgroup.amorfati.types.*

import scala.util.Random

/** Banking sector economics — aggregate values for MonthlyCalculus.
  *
  * Contains the full bank balance-sheet update pipeline: capital PnL, loan/NPL
  * dynamics, deposit flows, government bond allocation (PPK, insurance, TFI),
  * multi-bank resolution path with interbank clearing, failure detection,
  * bail-in, and firm/household reassignment. Also computes tax revenue, housing
  * mortgage flows, and monetary aggregates (M1/M2/M3).
  */
object BankingEconomics:

  // ---- Calibration constants ----
  private val NplMonthlyWriteOff = 0.05 // monthly NPL write-off rate (aggregate and per-bank)
  private val ShortLoanFrac      = 0.20 // fraction of loans in short-term maturity bucket
  private val MediumLoanFrac     = 0.30 // fraction of loans in medium-term maturity bucket
  private val LongLoanFrac       = 0.50 // fraction of loans in long-term maturity bucket

  // ---- Step-level types (formerly BankUpdateStep.Input / BankUpdateStep.Output) ----

  case class StepInput(
      w: World,                               // current world state (pre-step)
      s1: FiscalConstraintEconomics.Output,   // fiscal constraint (month, lending base rate, res wage)
      s2: LaborEconomics.Output,              // labor/demographics (employment, wage, immigration)
      s3: HouseholdIncomeEconomics.Output,    // household income (consumption, PIT, debt service)
      s4: DemandEconomics.Output,             // demand (sector multipliers, gov purchases)
      s5: FirmEconomics.StepOutput,           // firm processing (loans, NPL, bonds, I-O firms)
      s6: HouseholdFinancialEconomics.Output, // household financial (remittances, tourism, consumer credit)
      s7: PriceEquityEconomics.Output,        // price/equity (inflation, GDP, equity state, macropru)
      s8: OpenEconEconomics.StepOutput,       // open economy (NBP rate, bond yield, QE, FX, BoP)
      banks: Vector[Banking.BankState],       // explicit bank population
      depositRng: scala.util.Random,          // deterministic RNG for deposit flight decisions
  )

  case class StepOutput(
      resolvedBank: Banking.Aggregate,               // aggregate bank balance sheet after resolution
      banks: Vector[Banking.BankState],              // explicit post-step bank population
      bankingMarket: Banking.MarketState,            // banking market wrapper after interbank clearing
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
      effectiveShadowShare: Share,                   // effective shadow economy share
      mortgageInterestIncome: PLN,                   // mortgage interest income (bank share)
      mortgagePrincipal: PLN,                        // mortgage principal repaid
      mortgageDefaultLoss: PLN,                      // mortgage default loss (bank share)
      mortgageDefaultAmount: PLN,                    // gross mortgage default amount
      jstDepositChange: PLN,                         // JST deposit flow (Identity 2)
      investNetDepositFlow: PLN,                     // investment demand net deposit flow
      actualBondChange: PLN,                         // net change in gov bonds outstanding
      unrealizedBondLoss: PLN,                       // mark-to-market loss on gov bond portfolio (interest rate risk channel)
      htmRealizedLoss: PLN,                          // realized loss from HTM forced reclassification
      eclProvisionChange: PLN,                       // aggregate IFRS 9 ECL provision change
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
      erChange: Coefficient, // month-on-month ER change (for foreign demand)
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
      finalBanks: Vector[Banking.BankState],         // final explicit bank population after interbank clearing and resolution
      finalBankingMarket: Banking.MarketState,       // final banking market wrapper after interbank clearing and resolution
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
      bidToCover: Multiplier,                        // bond auction bid-to-cover ratio
  )

  // ---- Economics-level types (for MonthlyCalculus) ----

  case class Input(
      w: World,
      // Raw values from earlier calculus (avoids Step.Output dependency)
      month: Int,
      lendingBaseRate: Rate,
      resWage: PLN,
      baseMinWage: PLN,
      minWagePriceLevel: Double,
      employed: Int,
      newWage: PLN,
      laborDemand: Int,
      wageGrowth: Coefficient,
      govPurchases: PLN,
      sectorMults: Vector[Double],
      avgDemandMult: Double,
      sectorCap: Vector[Double],
      laggedInvestDemand: PLN,
      fiscalRuleStatus: com.boombustgroup.amorfati.engine.markets.FiscalRules.RuleStatus,
      // Step outputs too complex to decompose (will vanish with #131)
      laborOutput: LaborEconomics.Output,
      hhOutput: HouseholdIncomeEconomics.Output,
      firmOutput: FirmEconomics.StepOutput,
      hhFinancialOutput: HouseholdFinancialEconomics.Output,
      priceEquityOutput: PriceEquityEconomics.Output,
      openEconOutput: OpenEconEconomics.StepOutput,
      banks: Vector[Banking.BankState],
      depositRng: Random,
  )

  case class Result(
      govBondIncome: PLN,
      reserveInterest: PLN,
      standingFacilityIncome: PLN,
      interbankInterest: PLN,
      bfgLevy: PLN,
      unrealizedBondLoss: PLN,
      bailInLoss: PLN,
      nbpRemittance: PLN,
      mortgageInterestIncome: PLN,
      mortgagePrincipal: PLN,
      mortgageDefaultLoss: PLN,
      mortgageDefaultAmount: PLN,
      // Non-monetary outputs needed by WorldAssembly
      effectiveShadowShare: Share,
      jstDepositChange: PLN,
      investNetDepositFlow: PLN,
      actualBondChange: PLN,
      eclProvisionChange: PLN,
      htmRealizedLoss: PLN,
  )

  // ---- Public API ----

  def runStep(in: StepInput)(using p: SimParams): StepOutput =
    val prevBankAgg          = Banking.aggregateFromBanks(in.banks)
    val govJst               = computeGovAndJst(in)
    val housing              = computeHousingFlows(in)
    val bfgLevy              =
      if p.flags.bankFailure then Banking.computeBfgLevy(in.banks).total
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
    val monAgg               = computeMonetaryAggregates(multi.finalBanks, in)

    val newQuasiFiscal =
      if p.flags.quasiFiscal then
        QuasiFiscal.step(
          in.w.financial.quasiFiscal,
          govJst.newGovWithYield.govCapitalSpend,
          govJst.newGovWithYield.euCofinancing,
          in.w.nbp.qeActive,
        )
      else in.w.financial.quasiFiscal

    StepOutput(
      resolvedBank = multi.resolvedBank,
      banks = multi.finalBanks,
      bankingMarket = multi.finalBankingMarket,
      reassignedFirms = multi.reassignedFirms,
      reassignedHouseholds = multi.reassignedHouseholds,
      finalNbp = multi.finalNbp,
      finalPpk = multi.finalPpk,
      finalInsurance = multi.finalInsurance,
      finalNbfi = multi.finalNbfi,
      newGovWithYield = govJst.newGovWithYield.copy(
        financial = govJst.newGovWithYield.financial.copy(foreignBondHoldings = multi.foreignBondHoldings),
      ),
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
      effectiveShadowShare = Share(govJst.tax.effectiveShadowShare),
      mortgageInterestIncome = housing.mortgageFlows.interest,
      mortgagePrincipal = housing.mortgageFlows.principal,
      mortgageDefaultLoss = housing.mortgageFlows.defaultLoss,
      mortgageDefaultAmount = housing.mortgageFlows.defaultAmount,
      jstDepositChange = govJst.jstDepositChange,
      investNetDepositFlow = investNetDepositFlow,
      actualBondChange = multi.actualBondChange,
      unrealizedBondLoss = {
        val yieldChange = in.s8.monetary.newBondYield - in.w.gov.bondYield
        if yieldChange > Rate.Zero then prevBankAgg.afsBonds * yieldChange * Multiplier(p.banking.govBondDuration) else PLN.Zero
      },
      htmRealizedLoss = multi.htmRealizedLoss,
      eclProvisionChange = PLN.fromRaw:
        multi.finalBanks
          .zip(in.banks)
          .map: (curr, prev) =>
            val currProv =
              curr.eclStaging.stage1 * p.banking.eclRate1 + curr.eclStaging.stage2 * p.banking.eclRate2 + curr.eclStaging.stage3 * p.banking.eclRate3
            val prevProv =
              prev.eclStaging.stage1 * p.banking.eclRate1 + prev.eclStaging.stage2 * p.banking.eclRate2 + prev.eclStaging.stage3 * p.banking.eclRate3
            (currProv - prevProv).toLong
          .sum
      ,
      newQuasiFiscal = newQuasiFiscal,
    )

  def compute(in: Input)(using p: SimParams): Result =
    val s1 = FiscalConstraintEconomics.Output(in.month, in.baseMinWage, in.minWagePriceLevel, in.resWage, in.lendingBaseRate)
    val s4 = DemandEconomics.Output(
      in.govPurchases,
      in.sectorMults,
      in.w.pipeline.sectorDemandPressure,
      in.w.pipeline.sectorHiringSignal,
      in.avgDemandMult,
      in.sectorCap,
      in.laggedInvestDemand,
      in.fiscalRuleStatus,
    )

    val s9 = runStep(
      StepInput(
        in.w,
        s1,
        in.laborOutput,
        in.hhOutput,
        s4,
        in.firmOutput,
        in.hhFinancialOutput,
        in.priceEquityOutput,
        in.openEconOutput,
        in.banks,
        in.depositRng,
      ),
    )

    toResult(s9, in)

  private def toResult(s9: StepOutput, in: Input): Result =
    val prevBankAgg = Banking.aggregateFromBanks(in.banks)
    Result(
      govBondIncome = prevBankAgg.govBondHoldings * in.openEconOutput.monetary.newBondYield.monthly,
      reserveInterest = in.openEconOutput.banking.totalReserveInterest,
      standingFacilityIncome = in.openEconOutput.banking.totalStandingFacilityIncome,
      interbankInterest = in.openEconOutput.banking.totalInterbankInterest,
      bfgLevy = s9.bfgLevy,
      unrealizedBondLoss = s9.unrealizedBondLoss,
      bailInLoss = s9.bailInLoss,
      nbpRemittance = in.openEconOutput.banking.nbpRemittance,
      mortgageInterestIncome = s9.mortgageInterestIncome,
      mortgagePrincipal = s9.mortgagePrincipal,
      mortgageDefaultLoss = s9.mortgageDefaultLoss,
      mortgageDefaultAmount = s9.mortgageDefaultAmount,
      effectiveShadowShare = s9.effectiveShadowShare,
      jstDepositChange = s9.jstDepositChange,
      investNetDepositFlow = s9.investNetDepositFlow,
      actualBondChange = s9.actualBondChange,
      eclProvisionChange = s9.eclProvisionChange,
      htmRealizedLoss = s9.htmRealizedLoss,
    )

  // ---- Private helpers (moved from BankUpdateStep) ----

  /** Government budget update (deficit, debt, bonds) and JST local government
    * step.
    */
  private def computeGovAndJst(in: StepInput)(using p: SimParams): GovJstResult =
    val tax = TaxRevenue.compute(
      TaxRevenue.Input(
        consumption = ComputationBoundary.toDouble(in.s3.consumption),
        pitRevenue = ComputationBoundary.toDouble(in.s3.pitRevenue),
        totalImports = ComputationBoundary.toDouble(in.s8.external.newBop.totalImports),
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
        earmarkedGovSubvention = in.s2.newEarmarked.totalGovSubvention,
        socialTransferSpend = socialTransferSpend,
        euCofinancing = in.s7.euCofin,
        euProjectCapital = in.s7.euProjectCapital,
        govPurchasesActual = in.s4.govPurchases,
      ),
    )
    val newGovWithYield = newGov.copy(
      policy = newGov.policy.copy(
        bondYield = in.s8.monetary.newBondYield,
        weightedCoupon = in.s8.monetary.newWeightedCoupon,
      ),
    )

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
  private def computeHousingFlows(in: StepInput)(using p: SimParams): HousingResult =
    val population             = in.w.derivedTotalPopulation.max(1)
    val unempRate              = 1.0 - in.s2.employed.toDouble / population
    val prevMortgageRate       = in.w.real.housing.avgMortgageRate
    val mortgageBaseRate: Rate =
      if p.flags.interbankTermStructure then
        val exp = in.w.mechanisms.expectations
        YieldCurve
          .compute(
            in.w.bankingSector.interbankRate,
            nplRatio = Banking.aggregateFromBanks(in.banks).nplRatio,
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
        incomeGrowth = in.s2.wageGrowth.toMultiplier.toRate,
        employed = in.s2.employed,
        prevMortgageRate = prevMortgageRate,
      ),
    )
    val housingAfterOrig       =
      HousingMarket.processOrigination(housingAfterPrice, in.s3.totalIncome, mortgageRate, true)
    val mortgageFlows          = HousingMarket.processMortgageFlows(housingAfterOrig, mortgageRate, Share(unempRate))
    val housingAfterFlows      = HousingMarket.applyFlows(housingAfterOrig, mortgageFlows)

    HousingResult(housingAfterFlows = housingAfterFlows, mortgageFlows = mortgageFlows)

  /** Investment net deposit flow: lagged demand minus current domestic
    * investment.
    */
  private def computeInvestNetDepositFlow(in: StepInput)(using p: SimParams): PLN =
    val currentInvestDomestic = in.s5.sumGrossInvestment * (Share.One - p.capital.importShare) +
      in.s5.sumGreenInvestment * (Share.One - p.climate.greenImportShare)
    in.s4.laggedInvestDemand - currentInvestDomestic

  /** Recompute household aggregates from final households. */
  private def computeHhAgg(in: StepInput)(using SimParams): Household.Aggregates =
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
      in: StepInput,
      newGovWithYield: FiscalBudget.GovState,
  ): BondWaterfallInputs =
    val actualBondChange = newGovWithYield.bondsOutstanding - in.w.gov.bondsOutstanding
    val insRequested     = (in.s8.nonBank.newInsurance.govBondHoldings - in.w.financial.insurance.govBondHoldings).max(PLN.Zero)
    val tfiRequested     = (in.s8.nonBank.newNbfi.tfiGovBondHoldings - in.w.financial.nbfi.tfiGovBondHoldings).max(PLN.Zero)
    val prevEr           = in.w.forex.exchangeRate
    val currEr           = in.s8.external.newForex.exchangeRate
    val erChange         = if prevEr > 0.0 then Coefficient((currEr - prevEr) / prevEr) else Coefficient.Zero
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
      in: StepInput,
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
          incomeShare = in.s3.totalIncome * Share(ws),
          consShare = in.s3.consumption * Share(ws),
          hhDebtService = in.s6.hhDebtService * Share(ws),
          depInterest = PLN.Zero,
          ccDebtService = in.s6.consumerDebtService * Share(ws),
          ccOrigination = in.s6.consumerOrigination * Share(ws),
          ccDefault = in.s6.consumerDefaultAmt * Share(ws),
        )

  /** Compute updated state for a single bank in the multi-bank path. */
  private def updateSingleBank(
      b: Banking.BankState,
      hhFlows: PerBankHhFlows,
      workerShare: Share,
      mortgageFlows: HousingMarket.MortgageFlows,
      perBankReserveInt: Banking.PerBankAmounts,
      perBankStandingFac: Banking.PerBankAmounts,
      perBankInterbankInt: Banking.PerBankAmounts,
      jstDepositChange: PLN,
      investNetDepositFlow: PLN,
      in: StepInput,
  )(using p: SimParams): Banking.BankState =
    val bId           = b.id.toInt
    val bankNplNew    = PLN(in.s5.perBankNplDebt(bId))
    val bankNplLoss   = bankNplNew * (Share.One - p.banking.loanRecovery)
    val bankIntIncome = PLN(in.s5.perBankIntIncome(bId))
    val bankBondInc   = b.govBondHoldings * in.s8.monetary.newBondYield.monthly
    val bankResInt    = perBankReserveInt.perBank(bId)
    val bankSfInc     = perBankStandingFac.perBank(bId)
    val bankIbInt     = perBankInterbankInt.perBank(bId)
    val newLoansTotal =
      (b.loans + in.s5.perBankNewLoans(bId) - in.s5.perBankFirmPrincipal(bId) - bankNplNew * p.banking.loanRecovery).max(PLN.Zero)

    val newDep = b.deposits +
      hhFlows.incomeShare - hhFlows.consShare +
      investNetDepositFlow * workerShare +
      jstDepositChange * workerShare +
      in.s7.netDomesticDividends * workerShare -
      in.s7.foreignDividendOutflow * workerShare -
      in.s6.remittanceOutflow * workerShare +
      in.s6.diasporaInflow * workerShare +
      in.s6.tourismExport * workerShare -
      in.s6.tourismImport * workerShare +
      in.s5.perBankNewLoans(bId) -
      in.s5.perBankFirmPrincipal(bId) +
      hhFlows.ccOrigination +
      in.s8.nonBank.insNetDepositChange * workerShare +
      in.s8.nonBank.nbfiDepositDrain * workerShare

    val bankMortgageIntIncome     = mortgageFlows.interest * workerShare
    val bankMortgageNplLoss       = mortgageFlows.defaultLoss * workerShare
    val bankCcNplLoss             = hhFlows.ccDefault * (Share.One - p.household.ccNplRecovery)
    val bankCcStockReduction: PLN = in.s3.perBankHhFlowsOpt match
      case Some(pbf) => pbf(bId).consumerDebtService
      case _         => hhFlows.ccDebtService
    val bankCorpBondCoupon        = in.s8.corpBonds.corpBondBankCoupon * workerShare
    val bankCorpBondDefaultLoss   = in.s8.corpBonds.corpBondBankDefaultLoss * workerShare
    val bankBfgLevy               =
      if p.flags.bankFailure && !b.failed then b.deposits * p.banking.bfgLevyRate.monthly
      else PLN.Zero

    // Per-bank mark-to-market loss on AFS bonds only (HTM losses hidden until forced reclassification)
    val bankYieldChange    = in.s8.monetary.newBondYield - in.w.gov.bondYield
    val bankUnrealizedLoss = if bankYieldChange > Rate.Zero then b.afsBonds * bankYieldChange * Multiplier(p.banking.govBondDuration) else PLN.Zero

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

    // IFRS 9 ECL staging: provision change hits capital
    val unemployment = Share.One - Share.fraction(in.s2.employed, in.w.derivedTotalPopulation.max(1))
    val prevGdp      = in.w.cachedMonthlyGdpProxy
    val gdpGrowth    = if prevGdp > 0 then (ComputationBoundary.toDouble(in.s7.gdp) - prevGdp) / prevGdp else 0.0
    val eclResult    = EclStaging.step(b.eclStaging, newLoansTotal + b.consumerLoans, bankNplNew, unemployment, gdpGrowth)

    b.copy(
      loans = newLoansTotal,
      nplAmount = (b.nplAmount + bankNplNew - b.nplAmount * Share(NplMonthlyWriteOff)).max(PLN.Zero),
      capital = capitalPnl.newCapital - eclResult.provisionChange,
      eclStaging = eclResult.newStaging,
      deposits = newDep,
      demandDeposits = newDep * (Share.One - p.banking.termDepositFrac),
      termDeposits = newDep * p.banking.termDepositFrac,
      loansShort = newLoansTotal * Share(ShortLoanFrac),
      loansMedium = newLoansTotal * Share(MediumLoanFrac),
      loansLong = newLoansTotal * Share(LongLoanFrac),
      consumerLoans = (b.consumerLoans + hhFlows.ccOrigination - bankCcStockReduction - hhFlows.ccDefault).max(PLN.Zero),
      consumerNpl = (b.consumerNpl + hhFlows.ccDefault - b.consumerNpl * Share(NplMonthlyWriteOff)).max(PLN.Zero),
      corpBondHoldings = in.s8.corpBonds.newCorpBonds.bankHoldings * workerShare,
    )

  /** Multi-bank update: per-bank loop, interbank clearing, bond allocation,
    * failure resolution.
    */
  private def processMultiBankPath(
      in: StepInput,
      jstDepositChange: PLN,
      investNetDepositFlow: PLN,
      mortgageFlows: HousingMarket.MortgageFlows,
      wf: BondWaterfallInputs,
  )(using p: SimParams): MultiBankResult =
    val bs                  = in.w.bankingSector.withBanks(in.banks)
    val perBankReserveInt   = Banking.computeReserveInterest(bs.banks, in.w.nbp.referenceRate)
    val perBankStandingFac  = Banking.computeStandingFacilities(bs.banks, in.w.nbp.referenceRate)
    val perBankInterbankInt = Banking.interbankInterestFlows(bs.banks, bs.interbankRate)
    val totalWorkers        = in.s5.perBankWorkers.sum

    val workerShares: Vector[Share] =
      if totalWorkers <= 0 then Vector.fill(bs.banks.length)(Share.Zero)
      else
        val n    = bs.banks.length
        val raw  = (0 until n - 1).map(i => Share.fraction(in.s5.perBankWorkers(i), totalWorkers)).toVector
        val last = Share.One - raw.foldLeft(Share.Zero)(_ + _)
        raw :+ last

    val updatedBanks = bs.banks.map { b =>
      val bId         = b.id.toInt
      val workerShare = workerShares(bId)
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
      in: StepInput,
      updatedBanks: Vector[Banking.BankState],
      bs: Banking.State,
      wf: BondWaterfallInputs,
  )(using p: SimParams): MultiBankResult =
    val prevBankAgg      = Banking.aggregateFromBanks(in.banks)
    val ibRate           = Banking.interbankRate(updatedBanks, in.w.nbp.referenceRate)
    // Liquidity hoarding: reduce interbank lending when system NPL is high
    val hoarding         = InterbankContagion.hoardingFactor(prevBankAgg.nplRatio)
    val afterInterbank   = Banking.clearInterbank(updatedBanks, bs.configs, hoarding)
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
    val bankDeposits  = PLN.fromRaw(afterBonds.map(_.deposits.toLong).sum)
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
      balance = in.s8.monetary.postFxNbp.balance.copy(
        govBondHoldings = in.s8.monetary.postFxNbp.govBondHoldings + qeSale.actualSold,
        qeCumulative = in.s8.monetary.postFxNbp.qeCumulative + qeSale.actualSold,
      ),
    )
    val finalPpk                 = in.s2.newPpk.copy(bondHoldings = in.w.social.ppk.bondHoldings + ppkSale.actualSold)
    val finalInsurance           = in.s8.nonBank.newInsurance.copy(
      portfolio = in.s8.nonBank.newInsurance.portfolio.copy(govBondHoldings = in.w.financial.insurance.govBondHoldings + insSale.actualSold),
    )
    val finalNbfi                = in.s8.nonBank.newNbfi.copy(
      tfi = in.s8.nonBank.newNbfi.tfi.copy(tfiGovBondHoldings = in.w.financial.nbfi.tfiGovBondHoldings + tfiSale.actualSold),
    )
    val finalForeignBondHoldings = in.w.gov.foreignBondHoldings + foreignSale.actualSold

    val failResult =
      Banking.checkFailures(tfiSale.banks, in.s1.m, p.flags.bankFailure, in.s7.newMacropru.ccyb)

    // Interbank contagion: failed banks impose losses on counterparties
    val exposures      = InterbankContagion.buildExposureMatrix(tfiSale.banks)
    val afterContagion =
      if failResult.anyFailed then InterbankContagion.applyContagionLosses(failResult.banks, exposures)
      else failResult.banks
    // Re-check for secondary failures triggered by contagion losses
    val secondaryFail  = Banking.checkFailures(afterContagion, in.s1.m, p.flags.bankFailure, in.s7.newMacropru.ccyb)
    val afterFailCheck = secondaryFail.banks
    val anyFailed      = failResult.anyFailed || secondaryFail.anyFailed

    val bailInResult       =
      if anyFailed then Banking.applyBailIn(afterFailCheck) else Banking.BailInResult(afterFailCheck, PLN.Zero)
    val resolveResult      =
      if anyFailed then Banking.resolveFailures(bailInResult.banks)
      else Banking.ResolutionResult(bailInResult.banks, BankId.NoBank)
    val afterResolve       = resolveResult.banks
    val rawAbsorberId      = resolveResult.absorberId
    val absorberId         =
      if rawAbsorberId.toInt >= 0 then rawAbsorberId
      else Banking.healthiestBankId(afterResolve)
    val multiCapDest: PLN  =
      if anyFailed then
        PLN.fromRaw(
          tfiSale.banks
            .zip(afterFailCheck)
            .map { case (pre, post) =>
              if !pre.failed && post.failed then pre.capital.toLong else 0L
            }
            .sum,
        )
      else PLN.Zero
    val curve              =
      if p.flags.interbankTermStructure then
        val exp = in.w.mechanisms.expectations
        Some(
          YieldCurve.compute(
            ibRate,
            nplRatio = prevBankAgg.nplRatio,
            credibility = exp.credibility,
            expectedInflation = exp.expectedInflation,
            targetInflation = p.monetary.targetInfl,
          ),
        )
      else None
    val finalBankingMarket = Banking.MarketState(
      interbankRate = ibRate,
      configs = bs.configs,
      interbankCurve = curve,
    )
    val reassignedFirms    =
      if anyFailed then
        in.s7.rewiredFirms.map: f =>
          if f.bankId.toInt < afterResolve.length && afterResolve(f.bankId.toInt).failed then f.copy(bankId = absorberId)
          else f
      else in.s7.rewiredFirms
    val postFailureHh      =
      if anyFailed then
        in.s5.households.map: h =>
          if h.bankId.toInt < afterResolve.length && afterResolve(h.bankId.toInt).failed then h.copy(bankId = absorberId)
          else h
      else in.s5.households

    // Deposit mobility: HH may switch banks based on health signals and panic
    val mobilityResult       = DepositMobility(postFailureHh, afterResolve, anyFailed, in.depositRng)
    val reassignedHouseholds = mobilityResult.households

    // Deposit flows take effect next month when HH income/consumption routes
    // to new bankId. No immediate balance sheet transfer — consistent with
    // 1-month account transfer lag and avoids SFC flow mismatch.

    MultiBankResult(
      finalBanks = afterResolve,
      finalBankingMarket = finalBankingMarket,
      reassignedFirms = reassignedFirms,
      reassignedHouseholds = reassignedHouseholds,
      bailInLoss = bailInResult.totalLoss,
      multiCapDestruction = multiCapDest,
      resolvedBank = Banking.aggregateFromBanks(afterResolve),
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
      finalBanks: Vector[Banking.BankState],
      in: StepInput,
  )(using p: SimParams): Option[Banking.MonetaryAggregates] =
    if p.flags.creditDiagnostics then
      Some(
        Banking.MonetaryAggregates.compute(
          finalBanks,
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
      val totalDeposits = PLN.fromRaw(banks.map(_.deposits.toLong).sum)
      if totalDeposits <= PLN.Zero then banks
      else
        banks.map: b =>
          val share = Share(b.deposits / totalDeposits)
          b.copy(reservesAtNbp = (b.reservesAtNbp + injection * share).max(PLN.Zero))
