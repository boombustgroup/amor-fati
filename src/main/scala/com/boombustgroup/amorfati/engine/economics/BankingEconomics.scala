package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.*
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.ledger.{CorporateBondOwnership, LedgerFinancialState}
import com.boombustgroup.amorfati.engine.markets.{BondAuction, FiscalBudget, HousingMarket}
import com.boombustgroup.amorfati.engine.mechanisms.{TaxRevenue, YieldCurve}
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.Distribute

import com.boombustgroup.amorfati.random.RandomStream

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
      w: World,                                   // current world state (pre-step)
      ledgerFinancialState: LedgerFinancialState, // ledger-backed financial state
      s1: FiscalConstraintEconomics.Output,       // fiscal constraint (month, lending base rate, res wage)
      s2: LaborEconomics.Output,                  // labor/demographics (employment, wage, immigration)
      s3: HouseholdIncomeEconomics.Output,        // household income (consumption, PIT, debt service)
      s4: DemandEconomics.Output,                 // demand (sector multipliers, gov purchases)
      s5: FirmEconomics.StepOutput,               // firm processing (loans, NPL, bonds, I-O firms)
      s6: HouseholdFinancialEconomics.Output,     // household financial (remittances, tourism, consumer credit)
      s7: PriceEquityEconomics.Output,            // price/equity (inflation, GDP, equity state, macropru)
      s8: OpenEconEconomics.StepOutput,           // open economy (NBP rate, bond yield, QE, FX, BoP)
      banks: Vector[Banking.BankState],           // explicit bank population
      depositRng: RandomStream,                   // deterministic RNG for deposit flight decisions
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
      realizedTaxShadowShare: Share,                 // current-period realized aggregate tax-side shadow share
      mortgageInterestIncome: PLN,                   // mortgage interest income (bank share)
      mortgagePrincipal: PLN,                        // mortgage principal repaid
      mortgageDefaultLoss: PLN,                      // mortgage default loss (bank share)
      mortgageDefaultAmount: PLN,                    // gross mortgage default amount
      jstDepositChange: PLN,                         // JST deposit flow (Identity 2)
      investNetDepositFlow: PLN,                     // investment demand net deposit flow
      actualBondChange: PLN,                         // net change in gov bonds outstanding
      standingFacilityBackstop: PLN,                 // reserve shortfall funded by explicit NBP standing-facility backstop
      unrealizedBondLoss: PLN,                       // mark-to-market loss on gov bond portfolio (interest rate risk channel)
      htmRealizedLoss: PLN,                          // realized loss from HTM forced reclassification
      eclProvisionChange: PLN,                       // aggregate IFRS 9 ECL provision change
      newQuasiFiscal: QuasiFiscal.State,             // BGK/PFR after issuance and lending
      ledgerFinancialState: LedgerFinancialState,    // ledger-backed financial state at the banking stage boundary
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
      standingFacilityBackstop: PLN,                 // reserve shortfall funded by explicit NBP standing-facility backstop
      foreignBondHoldings: PLN,                      // non-resident holdings after auction
      bidToCover: Multiplier,                        // bond auction bid-to-cover ratio
  )

  private case class AggregateReconciliation(
      depositsResidual: PLN,
      capitalResidual: PLN,
  )

  private[amorfati] case class ReserveSettlementResult(
      banks: Vector[Banking.BankState],
      standingFacilityBackstop: PLN,
      residual: PLN,
  )

  private case class FxSettlementAllocation(
      allocations: Vector[PLN],
      residual: PLN,
  )

  // ---- Economics-level types (for MonthlyCalculus) ----

  case class Input(
      w: World,
      ledgerFinancialState: LedgerFinancialState,
      // Raw values from earlier calculus (avoids Step.Output dependency)
      month: ExecutionMonth,
      lendingBaseRate: Rate,
      resWage: PLN,
      baseMinWage: PLN,
      minWagePriceLevel: PriceIndex,
      employed: Int,
      newWage: PLN,
      laborDemand: Int,
      wageGrowth: Coefficient,
      govPurchases: PLN,
      avgDemandMult: Multiplier,
      sectorCapReal: Vector[PLN],
      laggedInvestDemand: PLN,
      fiscalRuleStatus: com.boombustgroup.amorfati.engine.markets.FiscalRules.RuleStatus,
      // Step outputs too complex to decompose (will vanish with #131)
      laborOutput: LaborEconomics.Output,
      operationalSignals: OperationalSignals,
      hhOutput: HouseholdIncomeEconomics.Output,
      firmOutput: FirmEconomics.StepOutput,
      hhFinancialOutput: HouseholdFinancialEconomics.Output,
      priceEquityOutput: PriceEquityEconomics.Output,
      openEconOutput: OpenEconEconomics.StepOutput,
      banks: Vector[Banking.BankState],
      depositRng: RandomStream,
  )

  case class Result(
      govBondIncome: PLN,
      reserveInterest: PLN,
      standingFacilityIncome: PLN,
      standingFacilityBackstop: PLN,
      interbankInterest: PLN,
      bfgLevy: PLN,
      unrealizedBondLoss: PLN,
      bailInLoss: PLN,
      nbpRemittance: PLN,
      pitAfterEvasion: PLN,
      vatAfterEvasion: PLN,
      exciseAfterEvasion: PLN,
      customsDutyRevenue: PLN,
      mortgageInterestIncome: PLN,
      mortgagePrincipal: PLN,
      mortgageDefaultLoss: PLN,
      mortgageDefaultAmount: PLN,
      // Non-monetary outputs needed by WorldAssembly
      realizedTaxShadowShare: Share,
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
    val bfgLevy              = Banking.computeBfgLevy(in.banks).total
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
    val issuerSettledFirms   = CorporateBondOwnership.applyAmortization(multi.reassignedFirms, in.s8.corpBonds.corpBondAmort)

    val newQuasiFiscal =
      QuasiFiscal.step(
        in.w.financial.quasiFiscal,
        govJst.newGovWithYield.govCapitalSpend,
        govJst.newGovWithYield.euCofinancing,
        in.w.nbp.qeActive,
      )

    val newGovWithForeignHoldings =
      govJst.newGovWithYield.copy(
        financial = govJst.newGovWithYield.financial.copy(foreignBondHoldings = multi.foreignBondHoldings),
      )
    val socialForLedger           = SocialState(
      jst = govJst.newJst,
      zus = in.s2.newZus,
      nfz = in.s2.newNfz,
      ppk = multi.finalPpk,
      demographics = in.s2.newDemographics,
      earmarked = in.s2.newEarmarked,
    )
    val ledgerFinancialState      =
      in.ledgerFinancialState.copy(
        households = multi.reassignedHouseholds.map(LedgerFinancialState.householdBalances),
        firms = issuerSettledFirms.map(LedgerFinancialState.firmBalances),
        banks = multi.finalBanks.map(LedgerFinancialState.bankBalances),
        government = LedgerFinancialState.governmentBalances(newGovWithForeignHoldings),
        foreign = LedgerFinancialState.foreignBalances(newGovWithForeignHoldings),
        nbp = LedgerFinancialState.nbpBalances(multi.finalNbp),
        insurance = LedgerFinancialState.insuranceBalances(multi.finalInsurance),
        funds = LedgerFinancialState.fundBalances(socialForLedger, in.s8.corpBonds.newCorpBonds, multi.finalNbfi, newQuasiFiscal),
      )
    val monAgg                    = computeMonetaryAggregates(multi.finalBanks, ledgerFinancialState)

    StepOutput(
      resolvedBank = multi.resolvedBank,
      banks = multi.finalBanks,
      bankingMarket = multi.finalBankingMarket,
      reassignedFirms = issuerSettledFirms,
      reassignedHouseholds = multi.reassignedHouseholds,
      finalNbp = multi.finalNbp,
      finalPpk = multi.finalPpk,
      finalInsurance = multi.finalInsurance,
      finalNbfi = multi.finalNbfi,
      newGovWithYield = newGovWithForeignHoldings,
      newJst = govJst.newJst,
      housingAfterFlows = housing.housingAfterFlows,
      bfgLevy = bfgLevy,
      bailInLoss = multi.bailInLoss,
      multiCapDestruction = multi.multiCapDestruction,
      monAgg = monAgg,
      finalHhAgg = finalHhAgg,
      vat = govJst.tax.vat,
      vatAfterEvasion = govJst.tax.vatAfterEvasion,
      pitAfterEvasion = govJst.tax.pitAfterEvasion,
      exciseRevenue = govJst.tax.exciseRevenue,
      exciseAfterEvasion = govJst.tax.exciseAfterEvasion,
      customsDutyRevenue = govJst.tax.customsDutyRevenue,
      realizedTaxShadowShare = govJst.tax.realizedTaxShadowShare,
      mortgageInterestIncome = housing.mortgageFlows.interest,
      mortgagePrincipal = housing.mortgageFlows.principal,
      mortgageDefaultLoss = housing.mortgageFlows.defaultLoss,
      mortgageDefaultAmount = housing.mortgageFlows.defaultAmount,
      jstDepositChange = govJst.jstDepositChange,
      investNetDepositFlow = investNetDepositFlow,
      actualBondChange = multi.actualBondChange,
      standingFacilityBackstop = multi.standingFacilityBackstop,
      unrealizedBondLoss = {
        val yieldChange = in.s8.monetary.newBondYield - in.w.gov.bondYield
        if yieldChange > Rate.Zero then prevBankAgg.afsBonds * yieldChange * p.banking.govBondDuration else PLN.Zero
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
      ledgerFinancialState = ledgerFinancialState,
    )

  def compute(in: Input)(using p: SimParams): Result =
    val s1 = FiscalConstraintEconomics.Output(in.month, in.baseMinWage, in.minWagePriceLevel, in.resWage, in.lendingBaseRate)
    val s4 = DemandEconomics.Output(
      in.govPurchases,
      in.operationalSignals.sectorDemandMult,
      in.operationalSignals.sectorDemandPressure,
      in.operationalSignals.sectorHiringSignal,
      in.avgDemandMult,
      in.sectorCapReal,
      in.laggedInvestDemand,
      in.fiscalRuleStatus,
    )

    val s9 = runStep(
      StepInput(
        in.w,
        in.ledgerFinancialState,
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

  private[engine] def toResult(s9: StepOutput, in: Input): Result =
    val prevBankAgg = Banking.aggregateFromBanks(in.banks)
    Result(
      govBondIncome = prevBankAgg.govBondHoldings * in.openEconOutput.monetary.newBondYield.monthly,
      reserveInterest = in.openEconOutput.banking.totalReserveInterest,
      standingFacilityIncome = in.openEconOutput.banking.totalStandingFacilityIncome,
      standingFacilityBackstop = s9.standingFacilityBackstop,
      interbankInterest = in.openEconOutput.banking.totalInterbankInterest,
      bfgLevy = s9.bfgLevy,
      unrealizedBondLoss = s9.unrealizedBondLoss,
      bailInLoss = s9.bailInLoss,
      nbpRemittance = in.openEconOutput.banking.nbpRemittance,
      pitAfterEvasion = s9.pitAfterEvasion,
      vatAfterEvasion = s9.vatAfterEvasion,
      exciseAfterEvasion = s9.exciseAfterEvasion,
      customsDutyRevenue = s9.customsDutyRevenue,
      mortgageInterestIncome = s9.mortgageInterestIncome,
      mortgagePrincipal = s9.mortgagePrincipal,
      mortgageDefaultLoss = s9.mortgageDefaultLoss,
      mortgageDefaultAmount = s9.mortgageDefaultAmount,
      realizedTaxShadowShare = s9.realizedTaxShadowShare,
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
        consumption = in.s3.consumption,
        pitRevenue = in.s3.pitRevenue,
        totalImports = in.s8.external.newBop.totalImports,
        informalCyclicalAdj = Share(in.w.mechanisms.informalCyclicalAdj),
      ),
    )

    val unempBenefitSpend   = in.s3.hhAgg.totalUnempBenefits
    val socialTransferSpend = in.s3.hhAgg.totalSocialTransfers

    val newGov          = FiscalBudget.update(
      FiscalBudget.Input(
        prev = in.w.gov,
        priceLevel = in.s7.newPrice,
        citPaid = in.s5.sumTax + in.s7.dividendTax + tax.pitAfterEvasion,
        govDividendRevenue = in.s7.stateOwnedGovDividends,
        vat = tax.vatAfterEvasion,
        nbpRemittance = in.s8.banking.nbpRemittance,
        exciseRevenue = tax.exciseAfterEvasion,
        customsDutyRevenue = tax.customsDutyRevenue,
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
        in.s5.sumTax,
        in.s3.totalIncome,
        in.s7.gdp,
        nLivingFirms,
        tax.pitAfterEvasion,
      )

    GovJstResult(
      newGovWithYield = newGovWithYield,
      newJst = jstResult.state,
      jstDepositChange = jstResult.depositChange,
      tax = tax,
    )

  /** Housing market: price step, origination, mortgage flows. */
  private def computeHousingFlows(in: StepInput)(using p: SimParams): HousingResult =
    val unempRate              = in.w.unemploymentRate(in.s2.employed)
    val prevMortgageRate       = in.w.real.housing.avgMortgageRate
    val mortgageBaseRate: Rate =
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
    val mortgageFlows          = HousingMarket.processMortgageFlows(housingAfterOrig, mortgageRate, unempRate)
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
    val actualBondChange = newGovWithYield.bondsOutstanding - in.ledgerFinancialState.government.govBondOutstanding
    val insRequested     =
      (in.s8.nonBank.newInsurance.govBondHoldings - in.ledgerFinancialState.insurance.govBondHoldings).max(PLN.Zero)
    val tfiRequested     =
      (in.s8.nonBank.newNbfi.tfiGovBondHoldings - in.ledgerFinancialState.funds.nbfi.govBondHoldings).max(PLN.Zero)
    val prevEr           = in.w.forex.exchangeRate
    val currEr           = in.s8.external.newForex.exchangeRate
    val erChange         = currEr.deviationFrom(prevEr).toCoefficient
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
    val bankNplNew    = in.s5.perBankNplDebt(bId)
    val bankNplLoss   = bankNplNew * (Share.One - p.banking.loanRecovery)
    val bankIntIncome = in.s5.perBankIntIncome(bId)
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
      if !b.failed then b.deposits * p.banking.bfgLevyRate.monthly
      else PLN.Zero

    // Per-bank mark-to-market loss on AFS bonds only (HTM losses hidden until forced reclassification)
    val bankYieldChange    = in.s8.monetary.newBondYield - in.w.gov.bondYield
    val bankUnrealizedLoss = if bankYieldChange > Rate.Zero then b.afsBonds * bankYieldChange * p.banking.govBondDuration else PLN.Zero

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
    val unemployment: Share              = in.w.unemploymentRate(in.s2.employed)
    val prevGdp: PLN                     = in.w.cachedMonthlyGdpProxy
    val gdpGrowth: Coefficient           =
      if prevGdp > PLN.Zero then (in.s7.gdp.ratioTo(prevGdp) - Scalar.One).toCoefficient else Coefficient.Zero
    val eclResult: EclStaging.StepResult = EclStaging.step(b.eclStaging, newLoansTotal + b.consumerLoans, bankNplNew, unemployment, gdpGrowth)

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

    processInterbankAndFailures(
      in,
      updatedBanks,
      bs,
      wf,
      perBankReserveInt,
      perBankStandingFac,
      perBankInterbankInt,
      jstDepositChange,
      investNetDepositFlow,
      mortgageFlows,
    )

  /** Interbank clearing, bond allocation, QE, failure check, bail-in,
    * resolution, reassignment.
    */
  private def processInterbankAndFailures(
      in: StepInput,
      updatedBanks: Vector[Banking.BankState],
      bs: Banking.State,
      wf: BondWaterfallInputs,
      perBankReserveInt: Banking.PerBankAmounts,
      perBankStandingFac: Banking.PerBankAmounts,
      perBankInterbankInt: Banking.PerBankAmounts,
      jstDepositChange: PLN,
      investNetDepositFlow: PLN,
      mortgageFlows: HousingMarket.MortgageFlows,
  )(using p: SimParams): MultiBankResult =
    val prevBankAgg    = Banking.aggregateFromBanks(in.banks)
    val ibRate         = Banking.interbankRate(updatedBanks, in.w.nbp.referenceRate)
    // Liquidity hoarding: reduce interbank lending when system NPL is high
    val hoarding       = InterbankContagion.hoardingFactor(prevBankAgg.nplRatio)
    val afterInterbank = Banking.clearInterbank(updatedBanks, bs.configs, hoarding)
    val nbpSettlement  = applyNbpReserveSettlement(
      afterInterbank,
      perBankReserveInt,
      perBankStandingFac,
      perBankInterbankInt,
      in.s8.monetary.fxPlnInjection,
    )
    if nbpSettlement.residual != PLN.Zero then
      throw IllegalStateException(
        s"NBP reserve settlement left unallocated FX residual ${nbpSettlement.residual} after reserve-side settlement.",
      )
    // HTM forced reclassification: LCR-stressed banks reclassify HTM→AFS, realizing hidden losses
    val htmResult      = Banking.processHtmForcedSale(nbpSettlement.banks, in.s8.monetary.newBondYield)
    val afterHtm       = htmResult.banks
    val afterBonds     =
      if wf.actualBondChange > PLN.Zero then Banking.allocateBondIssuance(afterHtm, wf.actualBondChange, in.s8.monetary.newBondYield)
      else if wf.actualBondChange < PLN.Zero then
        Banking.allocateBondRedemption(afterHtm, PLN.fromRaw(-wf.actualBondChange.toLong), in.s8.monetary.newBondYield)
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
    val finalPpk                 = in.s2.newPpk.copy(bondHoldings = in.ledgerFinancialState.funds.ppkGovBondHoldings + ppkSale.actualSold)
    val finalInsurance           = in.s8.nonBank.newInsurance.copy(
      portfolio = in.s8.nonBank.newInsurance.portfolio.copy(
        govBondHoldings = in.ledgerFinancialState.insurance.govBondHoldings + insSale.actualSold,
      ),
    )
    val finalNbfi                = in.s8.nonBank.newNbfi.copy(
      tfi = in.s8.nonBank.newNbfi.tfi.copy(
        tfiGovBondHoldings = in.ledgerFinancialState.funds.nbfi.govBondHoldings + tfiSale.actualSold,
      ),
    )
    val finalForeignBondHoldings = in.ledgerFinancialState.foreign.govBondHoldings + foreignSale.actualSold

    val failResult =
      Banking.checkFailures(tfiSale.banks, in.s1.m, true, in.s7.newMacropru.ccyb)

    // Interbank contagion: failed banks impose losses on counterparties
    val exposures      = InterbankContagion.buildExposureMatrix(tfiSale.banks)
    val afterContagion =
      if failResult.anyFailed then InterbankContagion.applyContagionLosses(failResult.banks, exposures)
      else failResult.banks
    // Re-check for secondary failures triggered by contagion losses
    val secondaryFail  = Banking.checkFailures(afterContagion, in.s1.m, true, in.s7.newMacropru.ccyb)
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
    val reconciled = reconcileAggregateExactness(
      banks = afterResolve,
      prevBankAgg = prevBankAgg,
      in = in,
      jstDepositChange = jstDepositChange,
      investNetDepositFlow = investNetDepositFlow,
      mortgageFlows = mortgageFlows,
      bailInLoss = bailInResult.totalLoss,
      multiCapDestruction = multiCapDest,
      htmRealizedLoss = htmResult.totalRealizedLoss,
    )

    MultiBankResult(
      finalBanks = reconciled,
      finalBankingMarket = finalBankingMarket,
      reassignedFirms = reassignedFirms,
      reassignedHouseholds = reassignedHouseholds,
      bailInLoss = bailInResult.totalLoss,
      multiCapDestruction = multiCapDest,
      resolvedBank = Banking.aggregateFromBanks(reconciled),
      htmRealizedLoss = htmResult.totalRealizedLoss,
      finalNbp = finalNbp,
      finalPpk = finalPpk,
      finalInsurance = finalInsurance,
      finalNbfi = finalNbfi,
      actualBondChange = wf.actualBondChange,
      standingFacilityBackstop = nbpSettlement.standingFacilityBackstop,
      foreignBondHoldings = finalForeignBondHoldings,
      bidToCover = auctionResult.bidToCover,
    )

  private def reconcileAggregateExactness(
      banks: Vector[Banking.BankState],
      prevBankAgg: Banking.Aggregate,
      in: StepInput,
      jstDepositChange: PLN,
      investNetDepositFlow: PLN,
      mortgageFlows: HousingMarket.MortgageFlows,
      bailInLoss: PLN,
      multiCapDestruction: PLN,
      htmRealizedLoss: PLN,
  )(using p: SimParams): Vector[Banking.BankState] =
    if banks.isEmpty then banks
    else
      val target         = aggregateReconciliationTarget(
        prevBankAgg = prevBankAgg,
        finalBanks = banks,
        in = in,
        jstDepositChange = jstDepositChange,
        investNetDepositFlow = investNetDepositFlow,
        mortgageFlows = mortgageFlows,
        bailInLoss = bailInLoss,
        multiCapDestruction = multiCapDestruction,
        htmRealizedLoss = htmRealizedLoss,
      )
      val actualDeposits = PLN.fromRaw(banks.iterator.map(_.deposits.toLong).sum)
      val actualCapital  = PLN.fromRaw(banks.iterator.map(_.capital.toLong).sum)
      val depResidual    = target.depositsResidual - actualDeposits
      val capResidual    = target.capitalResidual - actualCapital
      if depResidual == PLN.Zero && capResidual == PLN.Zero then banks
      else
        val targetIdx = banks.lastIndexWhere(!_.failed) match
          case -1 => banks.indices.last
          case i  => i
        banks.updated(
          targetIdx,
          reconcileSingleBank(banks(targetIdx), depResidual, capResidual),
        )

  private def aggregateReconciliationTarget(
      prevBankAgg: Banking.Aggregate,
      finalBanks: Vector[Banking.BankState],
      in: StepInput,
      jstDepositChange: PLN,
      investNetDepositFlow: PLN,
      mortgageFlows: HousingMarket.MortgageFlows,
      bailInLoss: PLN,
      multiCapDestruction: PLN,
      htmRealizedLoss: PLN,
  )(using p: SimParams): AggregateReconciliation =
    val yieldChange        = in.s8.monetary.newBondYield - in.w.gov.bondYield
    val unrealizedBondLoss =
      if yieldChange > Rate.Zero then prevBankAgg.afsBonds * yieldChange * p.banking.govBondDuration
      else PLN.Zero
    val eclProvisionChange = PLN.fromRaw:
      finalBanks
        .zip(in.banks)
        .map: (curr, prev) =>
          val currProv =
            curr.eclStaging.stage1 * p.banking.eclRate1 + curr.eclStaging.stage2 * p.banking.eclRate2 + curr.eclStaging.stage3 * p.banking.eclRate3
          val prevProv =
            prev.eclStaging.stage1 * p.banking.eclRate1 + prev.eclStaging.stage2 * p.banking.eclRate2 + prev.eclStaging.stage3 * p.banking.eclRate3
          (currProv - prevProv).toLong
        .sum
    val capitalLosses      = in.s5.nplLoss + mortgageFlows.defaultLoss + in.s6.consumerNplLoss +
      in.s8.corpBonds.corpBondBankDefaultLoss + Banking.computeBfgLevy(in.banks).total +
      unrealizedBondLoss + htmRealizedLoss + eclProvisionChange + multiCapDestruction
    val capitalGrossIncome = in.s5.intIncome + in.s6.hhDebtService +
      prevBankAgg.govBondHoldings * in.s8.monetary.newBondYield.monthly -
      in.s6.depositInterestPaid + in.s8.banking.totalReserveInterest +
      in.s8.banking.totalStandingFacilityIncome + in.s8.banking.totalInterbankInterest +
      mortgageFlows.interest + in.s6.consumerDebtService + in.s8.corpBonds.corpBondBankCoupon
    val targetCapital      = prevBankAgg.capital - capitalLosses + capitalGrossIncome * p.banking.profitRetention
    val targetDeposits     = prevBankAgg.deposits + in.s3.totalIncome - in.s3.consumption +
      investNetDepositFlow + jstDepositChange + in.s7.netDomesticDividends -
      in.s7.foreignDividendOutflow - in.s6.remittanceOutflow + in.s6.diasporaInflow +
      in.s6.tourismExport - in.s6.tourismImport - bailInLoss + in.s5.sumNewLoans -
      in.s5.sumFirmPrincipal + in.s6.consumerOrigination + in.s8.nonBank.insNetDepositChange +
      in.s8.nonBank.nbfiDepositDrain
    AggregateReconciliation(
      depositsResidual = targetDeposits,
      capitalResidual = targetCapital,
    )

  private def reconcileSingleBank(
      bank: Banking.BankState,
      depositResidual: PLN,
      capitalResidual: PLN,
  )(using p: SimParams): Banking.BankState =
    val newDeposits = bank.deposits + depositResidual
    bank.copy(
      deposits = newDeposits,
      demandDeposits = newDeposits * (Share.One - p.banking.termDepositFrac),
      termDeposits = newDeposits * p.banking.termDepositFrac,
      capital = bank.capital + capitalResidual,
    )

  /** Monetary aggregates (M0/M1/M2/M3) when credit diagnostics enabled. */
  private def computeMonetaryAggregates(
      finalBanks: Vector[Banking.BankState],
      ledgerFinancialState: LedgerFinancialState,
  ): Option[Banking.MonetaryAggregates] =
    Some(
      Banking.MonetaryAggregates.compute(
        finalBanks,
        ledgerFinancialState.funds.nbfi.tfiUnit,
        CorporateBondOwnership.issuerOutstanding(ledgerFinancialState),
      ),
    )

  /** Apply NBP-side reserve settlement deltas to bank reserve balances.
    *
    * Reserve remuneration, standing-facility settlement, interbank settlement
    * routed via NBP, and FX intervention PLN injection/drain all land on the
    * same reserve-side settlement channel. If a drain would push a bank below
    * zero reserves, the shortfall is converted into explicit standing-facility
    * borrowing while reserve balances stay non-negative.
    */
  private[amorfati] def applyNbpReserveSettlement(
      banks: Vector[Banking.BankState],
      reserveInterest: Banking.PerBankAmounts,
      standingFacilityIncome: Banking.PerBankAmounts,
      interbankInterest: Banking.PerBankAmounts,
      fxInjection: PLN,
  ): ReserveSettlementResult =
    val distributedFx                        = distributeFxInjectionByDeposits(banks, fxInjection)
    val updatedBanks                         = Vector.newBuilder[Banking.BankState]
    val (standingFacilityBackstop, residual) =
      banks.zipWithIndex.foldLeft((PLN.Zero, distributedFx.residual)) { (acc, bankAndIdx) =>
        val (accBackstop, accResidual) = acc
        val (bank, idx)                = bankAndIdx
        val delta                      =
          reserveInterest.perBank(idx) +
            standingFacilityIncome.perBank(idx) +
            interbankInterest.perBank(idx) +
            distributedFx.allocations(idx)
        val updated                    = bank.reservesAtNbp + delta
        if updated >= PLN.Zero then
          updatedBanks += bank.copy(reservesAtNbp = updated)
          (accBackstop, accResidual)
        else
          updatedBanks += bank.copy(reservesAtNbp = PLN.Zero)
          (accBackstop - updated, accResidual)
      }

    ReserveSettlementResult(updatedBanks.result(), standingFacilityBackstop, residual)

  /** Distribute FX intervention PLN injection across banks proportional to
    * deposit market share, adjusting reservesAtNbp. EUR purchase → PLN injected
    * into banking system; EUR sale → PLN drained. Any amount that cannot be
    * allocated is surfaced via `residual`.
    */
  private[amorfati] def distributeFxInjection(
      banks: Vector[Banking.BankState],
      injection: PLN,
  ): ReserveSettlementResult =
    val zeros = Banking.PerBankAmounts(Vector.fill(banks.size)(PLN.Zero), PLN.Zero)
    applyNbpReserveSettlement(
      banks,
      reserveInterest = zeros,
      standingFacilityIncome = zeros,
      interbankInterest = zeros,
      fxInjection = injection,
    )

  private def distributeFxInjectionByDeposits(
      banks: Vector[Banking.BankState],
      injection: PLN,
  ): FxSettlementAllocation =
    if injection == PLN.Zero then FxSettlementAllocation(Vector.fill(banks.size)(PLN.Zero), PLN.Zero)
    else
      val weights = banks.map(_.deposits.toLong.max(0L)).toArray
      if weights.sum <= 0L then FxSettlementAllocation(Vector.fill(banks.size)(PLN.Zero), injection)
      else
        val allocations = Distribute
          .distribute(math.abs(injection.toLong), weights)
          .iterator
          .map { rawAllocated =>
            if injection >= PLN.Zero then PLN.fromRaw(rawAllocated) else PLN.fromRaw(-rawAllocated)
          }
          .toVector
        FxSettlementAllocation(
          allocations = allocations,
          residual = injection - allocations.foldLeft(PLN.Zero)(_ + _),
        )
