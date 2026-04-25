package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.*
import com.boombustgroup.amorfati.engine.ledger.{CorporateBondOwnership, LedgerFinancialState}
import com.boombustgroup.amorfati.engine.markets.{BondAuction, CorporateBondMarket, FiscalBudget, HousingMarket}
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
      s6: HouseholdFinancialEconomics.Output,     // household financial flows (remittances, tourism, consumer credit)
      s7: PriceEquityEconomics.Output,            // price/equity (inflation, GDP, equity state, macropru)
      s8: OpenEconEconomics.StepOutput,           // open economy (NBP rate, bond yield, QE, FX, BoP)
      banks: Vector[Banking.BankState],           // explicit bank population
      depositRng: RandomStream,                   // deterministic RNG for deposit flight decisions
  )

  case class StepOutput(
      resolvedBank: Banking.Aggregate,                   // aggregate bank balance sheet after resolution
      banks: Vector[Banking.BankState],                  // explicit post-step bank population
      bankingMarket: Banking.MarketState,                // banking market wrapper after interbank clearing
      reassignedFirms: Vector[Firm.State],               // firms with bankId reassigned after bank failure
      reassignedHouseholds: Vector[Household.State],     // HH with bankId reassigned after bank failure
      finalNbp: Nbp.State,                               // NBP policy/QE state after bond-waterfall settlement
      finalPpk: SocialSecurity.PpkState,                 // PPK monthly contribution state
      finalInsurance: Insurance.State,                   // insurance monthly state
      finalInsuranceBalances: Insurance.ClosingBalances, // insurance non-corporate-bond closing balances
      finalNbfi: Nbfi.State,                             // NBFI/TFI monthly state
      finalNbfiBalances: Nbfi.ClosingBalances,           // NBFI/TFI non-corporate-bond closing balances
      newGovWithYield: FiscalBudget.GovState,            // gov state with updated bond yield
      newJst: Jst.State,                                 // local government state
      housingAfterFlows: HousingMarket.State,            // housing market after mortgage flows
      bfgLevy: PLN,                                      // BFG resolution fund levy (aggregate)
      bailInLoss: PLN,                                   // bail-in deposit destruction (aggregate)
      multiCapDestruction: PLN,                          // capital wiped when banks fail
      monAgg: Option[Banking.MonetaryAggregates],        // M0/M1/M2/M3 (when credit diagnostics on)
      finalHhAgg: Household.Aggregates,                  // recomputed HH aggregates
      vat: PLN,                                          // gross VAT revenue
      vatAfterEvasion: PLN,                              // VAT after informal evasion
      pitAfterEvasion: PLN,                              // PIT after informal evasion
      exciseRevenue: PLN,                                // gross excise revenue
      exciseAfterEvasion: PLN,                           // excise after informal evasion
      customsDutyRevenue: PLN,                           // customs duty revenue
      realizedTaxShadowShare: Share,                     // current-period realized aggregate tax-side shadow share
      mortgageInterestIncome: PLN,                       // mortgage interest income (bank share)
      mortgagePrincipal: PLN,                            // mortgage principal repaid
      mortgageDefaultLoss: PLN,                          // mortgage default loss (bank share)
      mortgageDefaultAmount: PLN,                        // gross mortgage default amount
      jstDepositChange: PLN,                             // JST deposit flow (Identity 2)
      investNetDepositFlow: PLN,                         // investment timing deposit settlement
      actualBondChange: PLN,                             // net change in gov bonds outstanding
      standingFacilityBackstop: PLN,                     // reserve shortfall funded by explicit NBP standing-facility backstop
      unrealizedBondLoss: PLN,                           // mark-to-market loss on gov bond portfolio (interest rate risk channel)
      htmRealizedLoss: PLN,                              // realized loss from HTM forced reclassification
      eclProvisionChange: PLN,                           // aggregate IFRS 9 ECL provision change
      newQuasiFiscal: QuasiFiscal.State,                 // BGK/PFR market memory after issuance and lending
      govBondRuntimeMovements: GovBondRuntimeMovements,  // holder-resolved SPW runtime movements from the bond waterfall
      ledgerFinancialState: LedgerFinancialState,        // ledger-backed financial state at the banking stage boundary
  )

  // --- Intermediate result types for sub-methods ---

  private case class GovJstResult(
      newGovWithYield: FiscalBudget.GovState, // updated government state with bond yield
      newGovBondOutstanding: PLN,             // issuer-side government bond stock owned by LedgerFinancialState
      newJst: Jst.State,                      // updated local government (JST) state
      jstCash: PLN,                           // ledger-owned JST cash after monthly deposit flow
      jstDepositChange: PLN,                  // net JST deposit flow into banking sector
      tax: TaxRevenue.Output,                 // computed tax revenues (VAT, excise, customs, PIT)
  )

  private case class HousingResult(
      housingAfterFlows: HousingMarket.State,    // housing market state after origination and mortgage flows
      mortgageFlows: HousingMarket.MortgageFlows, // monthly mortgage cash flows (interest, principal, defaults)
  )

  /** Inputs for the bond waterfall — raw requests, not final allocations. */
  private case class BondWaterfallInputs(
      actualBondChange: PLN, // Δ gov bonds outstanding settled against LedgerFinancialState
      qeRequested: PLN,      // NBP QE purchase request
      ppkRequested: PLN,     // PPK bond purchase request
      insRequested: PLN,     // insurance bond purchase request (delta)
      tfiRequested: PLN,     // TFI bond purchase request (delta)
      erChange: Coefficient, // month-on-month ER change (for foreign demand)
  )

  case class GovBondRuntimeMovements(
      primaryByBank: Vector[PLN],
      foreignPurchaseByBank: Vector[PLN],
      nbpQePurchaseByBank: Vector[PLN],
      ppkPurchaseByBank: Vector[PLN],
      insurancePurchaseByBank: Vector[PLN],
      tfiPurchaseByBank: Vector[PLN],
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

  private case class SingleBankUpdate(
      bank: Banking.BankState,
      financialStocks: Banking.BankFinancialStocks,
  )

  private case class MultiBankResult(
      finalBanks: Vector[Banking.BankState],             // final explicit bank population after interbank clearing and resolution
      finalBankCorpBondHoldings: Vector[PLN],            // ledger-owned corporate bond holdings by bank index
      finalBankLedgerBalances: Vector[LedgerFinancialState.BankBalances],
      finalBankingMarket: Banking.MarketState,           // final banking market wrapper after interbank clearing and resolution
      reassignedFirms: Vector[Firm.State],               // firms reassigned from failed banks to absorber bank
      reassignedHouseholds: Vector[Household.State],     // households reassigned from failed banks to absorber bank
      bailInLoss: PLN,                                   // total bail-in losses imposed on depositors
      multiCapDestruction: PLN,                          // capital destroyed by bank failures this month
      resolvedBank: Banking.Aggregate,                   // aggregate banking sector after resolution
      htmRealizedLoss: PLN,                              // realized loss from HTM forced reclassification
      // Bond waterfall outputs — single source of truth for buyer holdings
      finalNbp: Nbp.State,                               // NBP policy/QE state after bond-waterfall settlement
      finalNbpFinancialStocks: Nbp.FinancialStocks,      // NBP ledger-owned financial stocks after QE/FX settlement
      finalPpk: SocialSecurity.PpkState,                 // PPK monthly contribution state
      finalPpkGovBondHoldings: PLN,                      // PPK ledger-owned government bond holdings after bond purchase
      finalInsurance: Insurance.State,                   // insurance monthly state after non-bank step
      finalInsuranceBalances: Insurance.ClosingBalances, // insurance balances after government-bond purchase
      finalNbfi: Nbfi.State,                             // NBFI/TFI monthly state after non-bank step
      finalNbfiBalances: Nbfi.ClosingBalances,           // NBFI/TFI balances after government-bond purchase
      actualBondChange: PLN,                             // net change in gov bonds outstanding
      standingFacilityBackstop: PLN,                     // reserve shortfall funded by explicit NBP standing-facility backstop
      foreignBondHoldings: PLN,                          // non-resident holdings after auction
      bidToCover: Multiplier,                            // bond auction bid-to-cover ratio
      govBondRuntimeMovements: GovBondRuntimeMovements,  // holder-resolved SPW movements emitted by runtime batches
  )

  private case class AggregateReconciliation(
      depositsResidual: PLN,
      capitalResidual: PLN,
  )

  private[amorfati] case class ReserveSettlementResult(
      banks: Vector[Banking.BankState],
      financialStocks: Vector[Banking.BankFinancialStocks],
      standingFacilityBackstop: PLN,
      residual: PLN,
  )

  private case class FxSettlementAllocation(
      allocations: Vector[PLN],
      residual: PLN,
  )

  private def bankCorpBondHoldings(ledgerFinancialState: LedgerFinancialState): Banking.BankCorpBondHoldings =
    bankId => CorporateBondOwnership.bankHolderFor(ledgerFinancialState, bankId)

  def runStep(rawIn: StepInput)(using p: SimParams): StepOutput =
    val in                        = rawIn
    val openingBankStocks         = in.ledgerFinancialState.banks.map(LedgerFinancialState.projectBankFinancialStocks)
    val prevBankAgg               =
      Banking.aggregateFromBankStocks(
        in.banks,
        openingBankStocks,
        bankCorpBondHoldings(in.ledgerFinancialState),
      )
    val govJst                    = computeGovAndJst(in)
    val housing                   = computeHousingFlows(in)
    val bfgLevy                   = Banking.computeBfgLevy(in.banks, openingBankStocks).total
    val investNetDepositFlow      = computeInvestNetDepositFlow(in)
    val finalHhAgg                = computeHhAgg(in)
    val quasiFiscalStep           =
      QuasiFiscal.step(
        LedgerFinancialState.quasiFiscalStock(in.ledgerFinancialState),
        govJst.newGovWithYield.govCapitalSpend,
        govJst.newGovWithYield.euCofinancing,
        in.w.nbp.qeActive,
      )
    val newQuasiFiscal            = quasiFiscalStep.state
    val quasiFiscalDepositChange  = newQuasiFiscal.monthlyLending - newQuasiFiscal.monthlyLoanRepayment
    val wf                        = computeWaterfallInputs(in, govJst.newGovBondOutstanding)
    val multi                     = processMultiBankPath(
      in,
      govJst.jstDepositChange,
      investNetDepositFlow,
      quasiFiscalDepositChange,
      housing.mortgageFlows,
      wf,
    )
    val issuerSettledFirmBalances =
      CorporateBondOwnership.applyAmortization(in.s5.ledgerFinancialState.firms, multi.reassignedFirms, in.s8.corpBonds.corpBondAmort)

    val ledgerFinancialState =
      in.s5.ledgerFinancialState.copy(
        households = LedgerFinancialState.settleHouseholdMortgageStock(in.s5.ledgerFinancialState.households, housing.housingAfterFlows.mortgageStock),
        firms = issuerSettledFirmBalances,
        banks = multi.finalBankLedgerBalances,
        government = LedgerFinancialState.GovernmentBalances(govBondOutstanding = govJst.newGovBondOutstanding),
        foreign = LedgerFinancialState.ForeignBalances(govBondHoldings = multi.foreignBondHoldings),
        nbp = LedgerFinancialState.nbpBalances(multi.finalNbpFinancialStocks),
        insurance = LedgerFinancialState.insuranceBalances(multi.finalInsuranceBalances, in.s8.corpBonds.closingCorpBondProjection.insuranceHoldings),
        funds = LedgerFinancialState.fundBalances(
          zusCash = SocialSecurity.zusCashAfter(in.ledgerFinancialState.funds.zusCash, in.s2.newZus),
          nfzCash = SocialSecurity.nfzCashAfter(in.ledgerFinancialState.funds.nfzCash, in.s2.newNfz),
          fpCash = EarmarkedFunds.fpCashAfter(in.ledgerFinancialState.funds.fpCash, in.s2.newEarmarked),
          pfronCash = EarmarkedFunds.pfronCashAfter(in.ledgerFinancialState.funds.pfronCash, in.s2.newEarmarked),
          fgspCash = EarmarkedFunds.fgspCashAfter(in.ledgerFinancialState.funds.fgspCash, in.s2.newEarmarked),
          jstCash = govJst.jstCash,
          ppkGovBondHoldings = multi.finalPpkGovBondHoldings,
          corporateBonds = in.s8.corpBonds.closingCorpBondProjection,
          nbfi = multi.finalNbfiBalances,
          quasiFiscal = quasiFiscalStep.stock,
        ),
      )
    val monAgg               = computeMonetaryAggregates(multi.finalBanks, ledgerFinancialState)

    StepOutput(
      resolvedBank = multi.resolvedBank,
      banks = multi.finalBanks,
      bankingMarket = multi.finalBankingMarket,
      reassignedFirms = multi.reassignedFirms,
      reassignedHouseholds = multi.reassignedHouseholds,
      finalNbp = multi.finalNbp,
      finalPpk = multi.finalPpk,
      finalInsurance = multi.finalInsurance,
      finalInsuranceBalances = multi.finalInsuranceBalances,
      finalNbfi = multi.finalNbfi,
      finalNbfiBalances = multi.finalNbfiBalances,
      newGovWithYield = govJst.newGovWithYield,
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
      govBondRuntimeMovements = multi.govBondRuntimeMovements,
      ledgerFinancialState = ledgerFinancialState,
    )

  // ---- Private helpers ----

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
    val prevGov             = in.w.gov
    val prevJst             = in.w.social.jst

    val newGov                = FiscalBudget.update(
      FiscalBudget.Input(
        prev = prevGov,
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
    val newGovWithYield       = newGov.copy(
      policy = newGov.policy.copy(
        bondYield = in.s8.monetary.newBondYield,
        weightedCoupon = in.s8.monetary.newWeightedCoupon,
      ),
    )
    val newGovBondOutstanding = FiscalBudget.nextGovBondOutstanding(in.ledgerFinancialState.government.govBondOutstanding, newGov.deficit)

    val nLivingFirms = in.s5.ioFirms.count(Firm.isAlive)
    val jstResult    =
      Jst.step(
        prevJst,
        in.ledgerFinancialState.funds.jstCash,
        in.s5.sumTax,
        in.s3.totalIncome,
        in.s7.gdp,
        nLivingFirms,
        tax.pitAfterEvasion,
      )

    GovJstResult(
      newGovWithYield = newGovWithYield,
      newGovBondOutstanding = newGovBondOutstanding,
      newJst = jstResult.state,
      jstCash = jstResult.closingDeposits,
      jstDepositChange = jstResult.depositChange,
      tax = tax,
    )

  /** Housing market: price step, origination, mortgage flows. */
  private def computeHousingFlows(in: StepInput)(using p: SimParams): HousingResult =
    val unempRate              = in.w.unemploymentRate(in.s2.employed)
    val openingHousing         =
      HousingMarket.withMortgageStock(in.w.real.housing, LedgerFinancialState.householdMortgageStock(in.ledgerFinancialState))
    val prevMortgageRate       = openingHousing.avgMortgageRate
    val mortgageBaseRate: Rate =
      val exp = in.w.mechanisms.expectations
      YieldCurve
        .compute(
          in.w.bankingSector.interbankRate,
          nplRatio = Banking
            .aggregateFromBankStocks(
              in.banks,
              in.ledgerFinancialState.banks.map(LedgerFinancialState.projectBankFinancialStocks),
              bankCorpBondHoldings(in.ledgerFinancialState),
            )
            .nplRatio,
          credibility = exp.credibility,
          expectedInflation = exp.expectedInflation,
          targetInflation = p.monetary.targetInfl,
        )
        .wibor3m
    val mortgageRate           = mortgageBaseRate + p.housing.mortgageSpread
    val housingAfterPrice      = HousingMarket.step(
      HousingMarket.StepInput(
        prev = openingHousing,
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

  /** Investment timing deposit settlement: lagged domestic investment demand
    * minus current domestic investment spending.
    */
  private def computeInvestNetDepositFlow(in: StepInput)(using p: SimParams): PLN =
    val currentInvestDomestic = in.s5.sumGrossInvestment * (Share.One - p.capital.importShare) +
      in.s5.sumGreenInvestment * (Share.One - p.climate.greenImportShare)
    in.s4.laggedInvestDemand - currentInvestDomestic

  /** Re-distribute the opening unsecured consumer-loan book across banks using
    * the current household bank routing as weights, while preserving the exact
    * aggregate stock.
    *
    * The model has a single household `bankId` reused for origination, debt
    * service, and default routing. Deposit mobility / bank reassignment can
    * change that routing key without changing the aggregate unsecured loan
    * stock. If we leave the historical per-bank book untouched, a bank can end
    * up receiving more consumer-loan outflows than the stock it still carries,
    * forcing per-bank zero clipping and eventually breaking aggregate SFC
    * exactness. This helper keeps the aggregate book constant but rebalances
    * its distribution to the routing key used by the current-month household
    * flows.
    */
  private[economics] def alignConsumerLoanBookToHouseholdRouting(
      households: Vector[Household.State],
      householdBalances: Vector[LedgerFinancialState.HouseholdBalances],
      bankStocks: Vector[Banking.BankFinancialStocks],
  ): Vector[Banking.BankFinancialStocks] =
    require(
      households.length == householdBalances.length,
      s"BankingEconomics.alignConsumerLoanBookToHouseholdRouting requires aligned households and balances, got ${households.length} households and ${householdBalances.length} balance rows",
    )
    val totalBook = bankStocks.map(_.consumerLoan).sum
    if bankStocks.isEmpty || totalBook <= PLN.Zero then bankStocks
    else
      val bankWeights = Array.fill(bankStocks.length)(0L)
      households
        .zip(householdBalances)
        .foreach: (hh, balances) =>
          val bankIndex = hh.bankId.toInt
          if bankIndex >= 0 && bankIndex < bankWeights.length && balances.consumerLoan > PLN.Zero then
            bankWeights(bankIndex) += balances.consumerLoan.distributeRaw
      if !bankWeights.exists(_ > 0L) then bankStocks
      else
        val redistributed = Distribute.distribute(totalBook.distributeRaw, bankWeights).map(PLN.fromRaw).toVector
        bankStocks.zip(redistributed).map((stocks, consumerLoan) => stocks.copy(consumerLoan = consumerLoan))

  /** Recompute household aggregates from final households. */
  private def computeHhAgg(in: StepInput)(using SimParams): Household.Aggregates =
    Household.computeAggregates(
      in.s5.households,
      in.s5.ledgerFinancialState.households.map(LedgerFinancialState.projectHouseholdFinancialStocks),
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
      newGovBondOutstanding: PLN,
  ): BondWaterfallInputs =
    val actualBondChange = newGovBondOutstanding - in.ledgerFinancialState.government.govBondOutstanding
    val insRequested     =
      (in.s8.nonBank.newInsuranceBalances.govBondHoldings - in.ledgerFinancialState.insurance.govBondHoldings).max(PLN.Zero)
    val tfiRequested     =
      (in.s8.nonBank.newNbfiBalances.tfiGovBondHoldings - in.ledgerFinancialState.funds.nbfi.govBondHoldings).max(PLN.Zero)
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

  private def allocateBankCorpBondIssuance(issuance: PLN, perBankWorkers: Vector[Int]): Vector[PLN] =
    if perBankWorkers.isEmpty then Vector.empty
    else if issuance <= PLN.Zero then Vector.fill(perBankWorkers.length)(PLN.Zero)
    else
      val weights =
        val workerWeights = perBankWorkers.map(_.toLong.max(0L)).toArray
        if workerWeights.exists(_ > 0L) then workerWeights
        else Array.fill(perBankWorkers.length)(1L)
      Distribute.distribute(issuance.distributeRaw, weights).map(PLN.fromRaw).toVector

  private def reduceBankCorpBondHoldings(holdings: Vector[PLN], reduction: PLN): Vector[PLN] =
    if holdings.isEmpty then Vector.empty
    else if reduction <= PLN.Zero then holdings
    else
      val total           = holdings.sum
      val actualReduction = reduction.min(total)
      val reductions      = Distribute.distribute(actualReduction.distributeRaw, holdings.map(_.distributeRaw).toArray)
      holdings.zip(reductions).map((holding, rawReduction) => (holding - PLN.fromRaw(rawReduction)).max(PLN.Zero))

  private def settleBankCorpBondHoldings(
      previous: Vector[PLN],
      previousAggregateStock: CorporateBondMarket.StockState,
      nextAggregateStock: CorporateBondMarket.StockState,
      totalBondIssuance: PLN,
      perBankWorkers: Vector[Int],
  )(using p: SimParams): Vector[PLN] =
    val bankIssuance   = CorporateBondMarket.processIssuance(CorporateBondMarket.StockState.zero, totalBondIssuance).bankHoldings
    val bankReduction  = (previousAggregateStock.bankHoldings + bankIssuance - nextAggregateStock.bankHoldings).max(PLN.Zero)
    val afterReduction = reduceBankCorpBondHoldings(previous, bankReduction)
    val issuance       = allocateBankCorpBondIssuance(bankIssuance, perBankWorkers)
    afterReduction.zip(issuance).map(_ + _)

  /** Compute updated state for a single bank in the multi-bank path. */
  private def updateSingleBank(
      b: Banking.BankState,
      stocks: Banking.BankFinancialStocks,
      hhFlows: PerBankHhFlows,
      workerShare: Share,
      mortgageFlows: HousingMarket.MortgageFlows,
      perBankReserveInt: Banking.PerBankAmounts,
      perBankStandingFac: Banking.PerBankAmounts,
      perBankInterbankInt: Banking.PerBankAmounts,
      jstDepositChange: PLN,
      investNetDepositFlow: PLN,
      quasiFiscalDepositChange: PLN,
      in: StepInput,
  )(using p: SimParams): SingleBankUpdate =
    val bId           = b.id.toInt
    val bankNplNew    = in.s5.perBankNplDebt(bId)
    val bankNplLoss   = bankNplNew * (Share.One - p.banking.loanRecovery)
    val bankIntIncome = in.s5.perBankIntIncome(bId)
    val bankBondInc   = Banking.govBondHoldings(stocks) * in.s8.monetary.newBondYield.monthly
    val bankResInt    = perBankReserveInt.perBank(bId)
    val bankSfInc     = perBankStandingFac.perBank(bId)
    val bankIbInt     = perBankInterbankInt.perBank(bId)
    val newLoansTotal =
      (stocks.firmLoan + in.s5.perBankNewLoans(bId) - in.s5.perBankFirmPrincipal(bId) - bankNplNew * p.banking.loanRecovery).max(PLN.Zero)

    val newDep = stocks.totalDeposits +
      hhFlows.incomeShare - hhFlows.consShare +
      investNetDepositFlow * workerShare +
      jstDepositChange * workerShare +
      quasiFiscalDepositChange * workerShare +
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
      if !b.failed then stocks.totalDeposits * p.banking.bfgLevyRate.monthly
      else PLN.Zero

    // Per-bank mark-to-market loss on AFS bonds only (HTM losses hidden until forced reclassification)
    val bankYieldChange    = in.s8.monetary.newBondYield - in.w.gov.bondYield
    val bankUnrealizedLoss = if bankYieldChange > Rate.Zero then stocks.govBondAfs * bankYieldChange * p.banking.govBondDuration else PLN.Zero

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
    val eclResult: EclStaging.StepResult = EclStaging.step(b.eclStaging, newLoansTotal + stocks.consumerLoan, bankNplNew, unemployment, gdpGrowth)

    SingleBankUpdate(
      bank = b.copy(
        nplAmount = (b.nplAmount + bankNplNew - b.nplAmount * Share(NplMonthlyWriteOff)).max(PLN.Zero),
        capital = capitalPnl.newCapital - eclResult.provisionChange,
        eclStaging = eclResult.newStaging,
        loansShort = newLoansTotal * Share(ShortLoanFrac),
        loansMedium = newLoansTotal * Share(MediumLoanFrac),
        loansLong = newLoansTotal * Share(LongLoanFrac),
        consumerNpl = (b.consumerNpl + hhFlows.ccDefault - b.consumerNpl * Share(NplMonthlyWriteOff)).max(PLN.Zero),
      ),
      financialStocks = stocks.copy(
        firmLoan = newLoansTotal,
        totalDeposits = newDep,
        demandDeposit = newDep * (Share.One - p.banking.termDepositFrac),
        termDeposit = newDep * p.banking.termDepositFrac,
        consumerLoan = (stocks.consumerLoan + hhFlows.ccOrigination - bankCcStockReduction - hhFlows.ccDefault).max(PLN.Zero),
      ),
    )

  /** Multi-bank update: per-bank loop, interbank clearing, bond allocation,
    * failure resolution.
    */
  private def processMultiBankPath(
      in: StepInput,
      jstDepositChange: PLN,
      investNetDepositFlow: PLN,
      quasiFiscalDepositChange: PLN,
      mortgageFlows: HousingMarket.MortgageFlows,
      wf: BondWaterfallInputs,
  )(using p: SimParams): MultiBankResult =
    val banks               = in.banks
    val openingBankStocks   = in.ledgerFinancialState.banks.map(LedgerFinancialState.projectBankFinancialStocks)
    // We keep the opening bank-side consumer-loan stock but realign it to the
    // current household routing keys and household-level consumer-loan balances
    // carried into s5. This assumes no stage between the opening ledger snapshot
    // and s5 mutates household consumerLoan principal; only routing may drift.
    val bankStocks          = alignConsumerLoanBookToHouseholdRouting(
      in.s5.households,
      in.s5.ledgerFinancialState.households,
      openingBankStocks,
    )
    require(
      bankStocks.map(_.consumerLoan).sum == openingBankStocks.map(_.consumerLoan).sum,
      "BankingEconomics consumer-loan realignment must preserve the aggregate opening bank loan book",
    )
    val perBankReserveInt   = Banking.computeReserveInterest(banks, bankStocks, in.w.nbp.referenceRate)
    val perBankStandingFac  = Banking.computeStandingFacilities(banks, bankStocks, in.w.nbp.referenceRate)
    val perBankInterbankInt = Banking.interbankInterestFlows(banks, bankStocks, in.w.bankingSector.interbankRate)
    val totalWorkers        = in.s5.perBankWorkers.sum

    val workerShares: Vector[Share] =
      if totalWorkers <= 0 then Vector.fill(banks.length)(Share.Zero)
      else
        val n    = banks.length
        val raw  = (0 until n - 1).map(i => Share.fraction(in.s5.perBankWorkers(i), totalWorkers)).toVector
        val last = Share.One - raw.foldLeft(Share.Zero)(_ + _)
        raw :+ last

    val settledBankCorpBonds = settleBankCorpBondHoldings(
      previous = CorporateBondOwnership.bankHolderBalances(in.ledgerFinancialState),
      previousAggregateStock = CorporateBondOwnership.stockStateFromLedger(in.ledgerFinancialState),
      nextAggregateStock = in.s8.corpBonds.closingCorpBondProjection,
      totalBondIssuance = in.s5.actualBondIssuance,
      perBankWorkers = in.s5.perBankWorkers,
    )

    val updatedRows       = banks.zip(bankStocks).map { case (b, stocks) =>
      val bId         = b.id.toInt
      val workerShare = workerShares(bId)
      val hhFlows     = resolvePerBankHhFlows(bId, in.s3.perBankHhFlowsOpt, totalWorkers, in.s5.perBankWorkers, in)
      updateSingleBank(
        b,
        stocks,
        hhFlows,
        workerShare,
        mortgageFlows,
        perBankReserveInt,
        perBankStandingFac,
        perBankInterbankInt,
        jstDepositChange,
        investNetDepositFlow,
        quasiFiscalDepositChange,
        in,
      )
    }
    val updatedBanks      = updatedRows.map(_.bank)
    val updatedBankStocks = updatedRows.map(_.financialStocks)

    processInterbankAndFailures(
      in,
      updatedBanks,
      updatedBankStocks,
      in.w.bankingSector.configs,
      wf,
      perBankReserveInt,
      perBankStandingFac,
      perBankInterbankInt,
      jstDepositChange,
      investNetDepositFlow,
      quasiFiscalDepositChange,
      mortgageFlows,
      settledBankCorpBonds,
    )

  /** Interbank clearing, bond allocation, QE, failure check, bail-in,
    * resolution, reassignment.
    */
  private def processInterbankAndFailures(
      in: StepInput,
      updatedBanks: Vector[Banking.BankState],
      updatedBankStocks: Vector[Banking.BankFinancialStocks],
      bankConfigs: Vector[Banking.Config],
      wf: BondWaterfallInputs,
      perBankReserveInt: Banking.PerBankAmounts,
      perBankStandingFac: Banking.PerBankAmounts,
      perBankInterbankInt: Banking.PerBankAmounts,
      jstDepositChange: PLN,
      investNetDepositFlow: PLN,
      quasiFiscalDepositChange: PLN,
      mortgageFlows: HousingMarket.MortgageFlows,
      settledBankCorpBonds: Vector[PLN],
  )(using p: SimParams): MultiBankResult =
    val prevBankAgg    =
      Banking.aggregateFromBankStocks(
        in.banks,
        in.ledgerFinancialState.banks.map(LedgerFinancialState.projectBankFinancialStocks),
        bankCorpBondHoldings(in.ledgerFinancialState),
      )
    val ibRate         = Banking.interbankRate(updatedBanks, updatedBankStocks, in.w.nbp.referenceRate)
    // Liquidity hoarding: reduce interbank lending when system NPL is high
    val hoarding       = InterbankContagion.hoardingFactor(prevBankAgg.nplRatio)
    val afterInterbank = Banking.clearInterbank(updatedBanks, updatedBankStocks, bankConfigs, hoarding)
    val nbpSettlement  = applyNbpReserveSettlement(
      afterInterbank.banks,
      afterInterbank.financialStocks,
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
    val htmResult      = Banking.processHtmForcedSale(nbpSettlement.banks, nbpSettlement.financialStocks, in.s8.monetary.newBondYield)
    val afterHtm       = htmResult.banks
    val afterHtmStocks = htmResult.financialStocks
    val afterBonds     =
      if wf.actualBondChange > PLN.Zero then Banking.allocateBondIssuance(afterHtm, afterHtmStocks, wf.actualBondChange, in.s8.monetary.newBondYield)
      else if wf.actualBondChange < PLN.Zero then
        Banking.allocateBondRedemption(afterHtm, afterHtmStocks, PLN.fromRaw(-wf.actualBondChange.toLong), in.s8.monetary.newBondYield)
      else Banking.BankStockState(afterHtm, afterHtmStocks)
    val primaryByBank  = afterBonds.financialStocks
      .zip(afterHtmStocks)
      .map((after, before) => Banking.govBondHoldings(after) - Banking.govBondHoldings(before))

    // ---- Bond waterfall: single pass, SFC by construction ----
    // Each sellToBuyer removes bonds from banks and returns actualSold.
    // Buyer gets exactly old + actualSold. No speculation, no correction.
    val bankDeposits  = afterBonds.financialStocks.map(_.totalDeposits).sum
    val auctionResult = BondAuction.auction(
      newIssuance = wf.actualBondChange.max(PLN.Zero),
      bankBondCapacity = bankDeposits * p.fiscal.bankBondAbsorptionShare,
      marketYield = in.s8.monetary.newBondYield,
      erChange = wf.erChange,
    )
    val foreignSale   = Banking.sellToBuyer(afterBonds.banks, afterBonds.financialStocks, auctionResult.foreignAbsorbed)
    val qeSale        = Banking.sellToBuyer(foreignSale.banks, foreignSale.financialStocks, wf.qeRequested)
    val ppkSale       = Banking.sellToBuyer(qeSale.banks, qeSale.financialStocks, wf.ppkRequested)
    val insSale       = Banking.sellToBuyer(ppkSale.banks, ppkSale.financialStocks, wf.insRequested)
    val tfiSale       = Banking.sellToBuyer(insSale.banks, insSale.financialStocks, wf.tfiRequested)

    // Buyer holdings: old + actualSold (single source of truth)
    val finalNbp                 = in.s8.monetary.postFxNbp.copy(qeCumulative = in.s8.monetary.postFxNbp.qeCumulative + qeSale.actualSold)
    val finalNbpFinancialStocks  = in.s8.monetary.postFxNbpFinancialStocks.copy(
      govBondHoldings = in.s8.monetary.postFxNbpFinancialStocks.govBondHoldings + qeSale.actualSold,
    )
    val finalPpk                 = in.s2.newPpk
    val finalPpkGovBondHoldings  = in.ledgerFinancialState.funds.ppkGovBondHoldings + ppkSale.actualSold
    val finalInsurance           = in.s8.nonBank.newInsurance
    val finalInsuranceBalances   = in.s8.nonBank.newInsuranceBalances.copy(
      govBondHoldings = in.ledgerFinancialState.insurance.govBondHoldings + insSale.actualSold,
    )
    val finalNbfi                = in.s8.nonBank.newNbfi
    val finalNbfiBalances        = in.s8.nonBank.newNbfiBalances.copy(
      tfiGovBondHoldings = in.ledgerFinancialState.funds.nbfi.govBondHoldings + tfiSale.actualSold,
    )
    val finalForeignBondHoldings = in.ledgerFinancialState.foreign.govBondHoldings + foreignSale.actualSold
    val govBondRuntimeMovements  = GovBondRuntimeMovements(
      primaryByBank = primaryByBank,
      foreignPurchaseByBank = foreignSale.soldByBank,
      nbpQePurchaseByBank = qeSale.soldByBank,
      ppkPurchaseByBank = ppkSale.soldByBank,
      insurancePurchaseByBank = insSale.soldByBank,
      tfiPurchaseByBank = tfiSale.soldByBank,
    )

    val bankCorpBondHoldingsAfterSettlement = Banking.bankCorpBondHoldingsFromVector(settledBankCorpBonds)

    val failResult =
      Banking.checkFailures(tfiSale.banks, tfiSale.financialStocks, in.s1.m, true, in.s7.newMacropru.ccyb, bankCorpBondHoldingsAfterSettlement)

    // Interbank contagion: failed banks impose losses on counterparties
    val exposures       = InterbankContagion.buildExposureMatrix(tfiSale.banks, tfiSale.financialStocks)
    val afterContagion  =
      if failResult.anyFailed then InterbankContagion.applyContagionLosses(failResult.banks, exposures)
      else failResult.banks
    // Re-check for secondary failures triggered by contagion losses
    val secondaryFail   =
      Banking.checkFailures(afterContagion, tfiSale.financialStocks, in.s1.m, true, in.s7.newMacropru.ccyb, bankCorpBondHoldingsAfterSettlement)
    val afterFailCheck  = secondaryFail.banks
    val afterFailStocks = tfiSale.financialStocks
    val anyFailed       = failResult.anyFailed || secondaryFail.anyFailed

    val bailInResult                 =
      if anyFailed then Banking.applyBailIn(afterFailCheck, afterFailStocks)
      else Banking.BailInResult(afterFailCheck, afterFailStocks, PLN.Zero)
    val resolveResult                =
      if anyFailed then Banking.resolveFailures(bailInResult.banks, bailInResult.financialStocks, settledBankCorpBonds)
      else Banking.ResolutionResult(bailInResult.banks, bailInResult.financialStocks, BankId.NoBank, settledBankCorpBonds)
    val afterResolve                 = resolveResult.banks
    val afterResolveStocks           = resolveResult.financialStocks
    val afterResolveCorpBonds        = resolveResult.bankCorpBondHoldings
    val afterResolveCorpBondHoldings = Banking.bankCorpBondHoldingsFromVector(afterResolveCorpBonds)
    val rawAbsorberId                = resolveResult.absorberId
    val absorberId                   =
      if rawAbsorberId.toInt >= 0 then rawAbsorberId
      else Banking.healthiestBankId(afterResolve, afterResolveStocks, afterResolveCorpBondHoldings)
    val multiCapDest: PLN            =
      if anyFailed then
        tfiSale.banks
          .zip(afterFailCheck)
          .collect { case (pre, post) if !pre.failed && post.failed => pre.capital }
          .sum
      else PLN.Zero
    val curve                        =
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
    val finalBankingMarket           = Banking.MarketState(
      interbankRate = ibRate,
      configs = bankConfigs,
      interbankCurve = curve,
    )
    val reassignedFirms              =
      if anyFailed then
        in.s5.ioFirms.map: f =>
          if f.bankId.toInt < afterResolve.length && afterResolve(f.bankId.toInt).failed then f.copy(bankId = absorberId)
          else f
      else in.s5.ioFirms
    val postFailureHh                =
      if anyFailed then
        in.s5.households.map: h =>
          if h.bankId.toInt < afterResolve.length && afterResolve(h.bankId.toInt).failed then h.copy(bankId = absorberId)
          else h
      else in.s5.households

    // Deposit mobility: HH may switch banks based on health signals and panic
    val mobilityResult       = DepositMobility(postFailureHh, afterResolve, afterResolveStocks, anyFailed, in.depositRng, afterResolveCorpBondHoldings)
    val reassignedHouseholds = mobilityResult.households

    // Deposit flows take effect next month when HH income/consumption routes
    // to new bankId. No immediate balance sheet transfer — consistent with
    // 1-month account transfer lag and avoids SFC flow mismatch.
    val reconciled = reconcileAggregateExactness(
      banks = afterResolve,
      financialStocks = afterResolveStocks,
      prevBankAgg = prevBankAgg,
      in = in,
      jstDepositChange = jstDepositChange,
      investNetDepositFlow = investNetDepositFlow,
      quasiFiscalDepositChange = quasiFiscalDepositChange,
      mortgageFlows = mortgageFlows,
      bailInLoss = bailInResult.totalLoss,
      multiCapDestruction = multiCapDest,
      htmRealizedLoss = htmResult.totalRealizedLoss,
    )

    MultiBankResult(
      finalBanks = reconciled.banks,
      finalBankCorpBondHoldings = afterResolveCorpBonds,
      finalBankLedgerBalances = reconciled.banks
        .zip(reconciled.financialStocks)
        .map: (bank, stocks) =>
          LedgerFinancialState.bankBalances(stocks, afterResolveCorpBonds.lift(bank.id.toInt).getOrElse(PLN.Zero)),
      finalBankingMarket = finalBankingMarket,
      reassignedFirms = reassignedFirms,
      reassignedHouseholds = reassignedHouseholds,
      bailInLoss = bailInResult.totalLoss,
      multiCapDestruction = multiCapDest,
      resolvedBank = Banking.aggregateFromBankStocks(reconciled.banks, reconciled.financialStocks, afterResolveCorpBondHoldings),
      htmRealizedLoss = htmResult.totalRealizedLoss,
      finalNbp = finalNbp,
      finalNbpFinancialStocks = finalNbpFinancialStocks,
      finalPpk = finalPpk,
      finalPpkGovBondHoldings = finalPpkGovBondHoldings,
      finalInsurance = finalInsurance,
      finalInsuranceBalances = finalInsuranceBalances,
      finalNbfi = finalNbfi,
      finalNbfiBalances = finalNbfiBalances,
      actualBondChange = wf.actualBondChange,
      standingFacilityBackstop = nbpSettlement.standingFacilityBackstop,
      foreignBondHoldings = finalForeignBondHoldings,
      bidToCover = auctionResult.bidToCover,
      govBondRuntimeMovements = govBondRuntimeMovements,
    )

  private def reconcileAggregateExactness(
      banks: Vector[Banking.BankState],
      financialStocks: Vector[Banking.BankFinancialStocks],
      prevBankAgg: Banking.Aggregate,
      in: StepInput,
      jstDepositChange: PLN,
      investNetDepositFlow: PLN,
      quasiFiscalDepositChange: PLN,
      mortgageFlows: HousingMarket.MortgageFlows,
      bailInLoss: PLN,
      multiCapDestruction: PLN,
      htmRealizedLoss: PLN,
  )(using p: SimParams): Banking.BankStockState =
    if banks.isEmpty then Banking.BankStockState(banks, financialStocks)
    else
      val target         = aggregateReconciliationTarget(
        prevBankAgg = prevBankAgg,
        finalBanks = banks,
        in = in,
        jstDepositChange = jstDepositChange,
        investNetDepositFlow = investNetDepositFlow,
        quasiFiscalDepositChange = quasiFiscalDepositChange,
        mortgageFlows = mortgageFlows,
        bailInLoss = bailInLoss,
        multiCapDestruction = multiCapDestruction,
        htmRealizedLoss = htmRealizedLoss,
      )
      val actualDeposits = financialStocks.iterator.map(_.totalDeposits).sum
      val actualCapital  = banks.iterator.map(_.capital).sum
      val depResidual    = target.depositsResidual - actualDeposits
      val capResidual    = target.capitalResidual - actualCapital
      if depResidual == PLN.Zero && capResidual == PLN.Zero then Banking.BankStockState(banks, financialStocks)
      else
        val targetIdx  = banks.lastIndexWhere(!_.failed) match
          case -1 => banks.indices.last
          case i  => i
        val reconciled = reconcileSingleBank(banks(targetIdx), financialStocks(targetIdx), depResidual, capResidual)
        Banking.BankStockState(
          banks.updated(targetIdx, reconciled._1),
          financialStocks.updated(targetIdx, reconciled._2),
        )

  private def aggregateReconciliationTarget(
      prevBankAgg: Banking.Aggregate,
      finalBanks: Vector[Banking.BankState],
      in: StepInput,
      jstDepositChange: PLN,
      investNetDepositFlow: PLN,
      quasiFiscalDepositChange: PLN,
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
      in.s8.corpBonds.corpBondBankDefaultLoss +
      Banking.computeBfgLevy(in.banks, in.ledgerFinancialState.banks.map(LedgerFinancialState.projectBankFinancialStocks)).total +
      unrealizedBondLoss + htmRealizedLoss + eclProvisionChange + multiCapDestruction
    val capitalGrossIncome = in.s5.intIncome + in.s6.hhDebtService +
      prevBankAgg.govBondHoldings * in.s8.monetary.newBondYield.monthly -
      in.s6.depositInterestPaid + in.s8.banking.totalReserveInterest +
      in.s8.banking.totalStandingFacilityIncome + in.s8.banking.totalInterbankInterest +
      mortgageFlows.interest + in.s6.consumerDebtService + in.s8.corpBonds.corpBondBankCoupon
    val targetCapital      = prevBankAgg.capital - capitalLosses + capitalGrossIncome * p.banking.profitRetention
    val targetDeposits     = prevBankAgg.deposits + in.s3.totalIncome - in.s3.consumption +
      investNetDepositFlow + jstDepositChange + quasiFiscalDepositChange + in.s7.netDomesticDividends -
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
      stocks: Banking.BankFinancialStocks,
      depositResidual: PLN,
      capitalResidual: PLN,
  )(using p: SimParams): (Banking.BankState, Banking.BankFinancialStocks) =
    val newDeposits = stocks.totalDeposits + depositResidual
    (
      bank.copy(capital = bank.capital + capitalResidual),
      stocks.copy(
        totalDeposits = newDeposits,
        demandDeposit = newDeposits * (Share.One - p.banking.termDepositFrac),
        termDeposit = newDeposits * p.banking.termDepositFrac,
      ),
    )

  /** Monetary aggregates (M0/M1/M2/M3) when credit diagnostics enabled. */
  private def computeMonetaryAggregates(
      finalBanks: Vector[Banking.BankState],
      ledgerFinancialState: LedgerFinancialState,
  ): Option[Banking.MonetaryAggregates] =
    Some(
      Banking.MonetaryAggregates.computeFromBankStocks(
        finalBanks,
        ledgerFinancialState.banks.map(LedgerFinancialState.projectBankFinancialStocks),
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
      financialStocks: Vector[Banking.BankFinancialStocks],
      reserveInterest: Banking.PerBankAmounts,
      standingFacilityIncome: Banking.PerBankAmounts,
      interbankInterest: Banking.PerBankAmounts,
      fxInjection: PLN,
  ): ReserveSettlementResult =
    val distributedFx                        = distributeFxInjectionByDeposits(financialStocks, fxInjection)
    val updatedStocks                        = Vector.newBuilder[Banking.BankFinancialStocks]
    val (standingFacilityBackstop, residual) =
      banks.zip(financialStocks).zipWithIndex.foldLeft((PLN.Zero, distributedFx.residual)) { (acc, rowAndIdx) =>
        val (accBackstop, accResidual) = acc
        val ((_, stocks), idx)         = rowAndIdx
        val delta                      =
          reserveInterest.perBank(idx) +
            standingFacilityIncome.perBank(idx) +
            interbankInterest.perBank(idx) +
            distributedFx.allocations(idx)
        val updated                    = stocks.reserve + delta
        if updated >= PLN.Zero then
          updatedStocks += stocks.copy(reserve = updated)
          (accBackstop, accResidual)
        else
          updatedStocks += stocks.copy(reserve = PLN.Zero)
          (accBackstop - updated, accResidual)
      }

    ReserveSettlementResult(banks, updatedStocks.result(), standingFacilityBackstop, residual)

  /** Distribute FX intervention PLN injection across banks proportional to
    * deposit market share, adjusting reservesAtNbp. EUR purchase → PLN injected
    * into banking system; EUR sale → PLN drained. Any amount that cannot be
    * allocated is surfaced via `residual`.
    */
  private[amorfati] def distributeFxInjection(
      banks: Vector[Banking.BankState],
      financialStocks: Vector[Banking.BankFinancialStocks],
      injection: PLN,
  ): ReserveSettlementResult =
    val zeros = Banking.PerBankAmounts(Vector.fill(banks.size)(PLN.Zero), PLN.Zero)
    applyNbpReserveSettlement(
      banks,
      financialStocks,
      reserveInterest = zeros,
      standingFacilityIncome = zeros,
      interbankInterest = zeros,
      fxInjection = injection,
    )

  private def distributeFxInjectionByDeposits(
      financialStocks: Vector[Banking.BankFinancialStocks],
      injection: PLN,
  ): FxSettlementAllocation =
    if injection == PLN.Zero then FxSettlementAllocation(Vector.fill(financialStocks.size)(PLN.Zero), PLN.Zero)
    else
      val weights = financialStocks.map(_.totalDeposits.toLong.max(0L)).toArray
      if weights.sum <= 0L then FxSettlementAllocation(Vector.fill(financialStocks.size)(PLN.Zero), injection)
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
