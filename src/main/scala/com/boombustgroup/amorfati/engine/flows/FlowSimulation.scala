package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.accounting.Sfc
import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.engine.economics.*
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*

import scala.util.Random

/** New flow-based simulation pipeline.
  *
  * Three stages per month:
  *   1. CALCULUS — pure economics (CES, Phillips, Taylor, Meen, Calvo)
  *   2. TRANSLATION — map calculus results to flow mechanism inputs
  *   3. PLUMBING — emit flows, apply through verified interpreter
  *
  * Ledger execution guarantees exact conservation; SFC remains as a semantic
  * validation oracle over stocks and declared monthly flows.
  */
object FlowSimulation:

  /** Bundles the three mutable components of the simulation: the World state,
    * the firm vector, and the household vector. These always travel together
    * between simulation steps and the main loop.
    */
  case class SimState(
      world: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      banks: Vector[Banking.BankState],
      householdAggregates: Household.Aggregates,
  )

  case class ExecutionResult(
      snapshot: Map[(EntitySector, AssetType, Int), Long],
      totalWealth: Long,
  )

  /** All calculus results needed to feed flow mechanisms. */
  case class MonthlyCalculus(
      // Stage 1: Fiscal constraint
      month: Int,
      resWage: PLN,
      lendingBaseRate: Rate,
      baseMinWage: PLN,
      minWagePriceLevel: Double,
      // Stage 2: Labor market
      wage: PLN,
      employed: Int,
      laborDemand: Int,
      retirees: Int,
      workingAgePop: Int,
      nBankruptFirms: Int,
      avgFirmWorkers: Int,
      // Stage 3: HH income (aggregates)
      totalIncome: PLN,
      consumption: PLN,
      domesticConsumption: PLN,
      importConsumption: PLN,
      totalRent: PLN,
      totalPit: PLN,
      totalDebtService: PLN,
      totalDepositInterest: PLN,
      totalRemittances: PLN,
      totalUnempBenefits: PLN,
      totalSocialTransfers: PLN,
      totalCcOrigination: PLN,
      totalCcDebtService: PLN,
      totalCcDefault: PLN,
      // Stage 4: Demand
      govPurchases: PLN,
      // Stage 5: Firm
      firmTax: PLN,
      firmNewLoans: PLN,
      firmPrincipal: PLN,
      firmInterestIncome: PLN,
      firmCapex: PLN,
      firmEquityIssuance: PLN,
      firmBondIssuance: PLN,
      firmIoPayments: PLN,
      firmNplLoss: PLN,
      firmProfitShifting: PLN,
      firmFdiRepatriation: PLN,
      firmGrossInvestment: PLN,
      // Stage 7: Price / Equity
      gdp: PLN,
      inflation: Rate,
      equityDomDividends: PLN,
      equityForDividends: PLN,
      equityDivTax: PLN,
      equityIssuance: PLN,
      equityReturn: Rate,
      // Stage 8: Open economy
      exports: PLN,
      totalImports: PLN,
      tourismExport: PLN,
      tourismImport: PLN,
      fdi: PLN,
      portfolioFlows: PLN,
      primaryIncome: PLN,
      euFunds: PLN,
      diasporaInflow: PLN,
      // Stage 8: Corp bonds
      corpBondCoupon: PLN,
      corpBondDefaultLoss: PLN,
      corpBondIssuance: PLN,
      corpBondAmortization: PLN,
      // Stage 8: Mortgage
      mortgageOrigination: PLN,
      mortgageRepayment: PLN,
      mortgageInterest: PLN,
      mortgageDefault: PLN,
      // Stage 9: Banking
      bankGovBondIncome: PLN,
      bankReserveInterest: PLN,
      bankStandingFacility: PLN,
      bankInterbankInterest: PLN,
      bankBfgLevy: PLN,
      bankUnrealizedLoss: PLN,
      bankBailIn: PLN,
      bankNbpRemittance: PLN,
      // Stage 8: Gov budget
      govTaxRevenue: PLN,
      govDebtService: PLN,
      govEuCofinancing: PLN,
      govCapitalSpend: PLN,
      // Insurance
      insurancePrevGovBonds: PLN,
      insurancePrevCorpBonds: PLN,
      insurancePrevEquity: PLN,
      govBondYield: Rate,
      corpBondYield: Rate,
  )

  /** Emit ALL flows from calculus results. Pure translation — no economics
    * here.
    */
  def emitAllBatches(c: MonthlyCalculus)(using p: SimParams): Vector[BatchedFlow] =
    Vector.concat(
      // Tier 1: Social funds
      ZusFlows.emitBatches(ZusFlows.ZusInput(c.employed, c.wage, c.retirees)),
      NfzFlows.emitBatches(NfzFlows.NfzInput(c.employed, c.wage, c.workingAgePop, c.retirees)),
      PpkFlows.emitBatches(PpkFlows.PpkInput(c.employed, c.wage)),
      EarmarkedFlows.emitBatches(EarmarkedFlows.Input(c.employed, c.wage, c.totalUnempBenefits, c.nBankruptFirms, c.avgFirmWorkers)),
      JstFlows.emitBatches(JstFlows.Input(c.govTaxRevenue, c.totalIncome, c.gdp, c.laborDemand, c.totalPit)),
      // Tier 2: Agents
      HouseholdFlows.emitBatches(
        HouseholdFlows.Input(
          c.consumption,
          c.totalRent,
          c.totalPit,
          c.totalDebtService,
          c.totalDepositInterest,
          c.totalRemittances,
          c.totalCcOrigination,
          c.totalCcDebtService,
          c.totalCcDefault,
        ),
      ),
      FirmFlows.emitBatches(
        FirmFlows.Input(
          c.totalIncome,
          c.firmTax,
          c.firmPrincipal,
          c.firmNewLoans,
          c.firmInterestIncome,
          c.firmCapex,
          c.firmEquityIssuance,
          c.firmBondIssuance,
          c.firmIoPayments,
          c.firmNplLoss,
          c.firmProfitShifting,
          c.firmFdiRepatriation,
          c.firmGrossInvestment,
        ),
      ),
      GovBudgetFlows.emitBatches(
        GovBudgetFlows.Input(
          c.govTaxRevenue,
          c.govPurchases,
          c.govDebtService,
          c.totalUnempBenefits,
          c.totalSocialTransfers,
          c.govEuCofinancing,
          c.govCapitalSpend,
        ),
      ),
      InsuranceFlows.emitBatches(
        InsuranceFlows.Input(
          c.employed,
          c.wage,
          Share.One - Share.fraction(c.employed, (c.employed + 1).max(1)),
          c.insurancePrevGovBonds,
          c.insurancePrevCorpBonds,
          c.insurancePrevEquity,
          c.govBondYield,
          c.corpBondYield,
          c.equityReturn,
        ),
      ),
      // Tier 3: Financial markets
      EquityFlows.emitBatches(EquityFlows.Input(c.equityDomDividends, c.equityForDividends, c.equityDivTax, c.equityIssuance)),
      CorpBondFlows.emitBatches(CorpBondFlows.Input(c.corpBondCoupon, c.corpBondDefaultLoss, c.corpBondIssuance, c.corpBondAmortization)),
      MortgageFlows.emitBatches(MortgageFlows.Input(c.mortgageOrigination, c.mortgageRepayment, c.mortgageInterest, c.mortgageDefault)),
      OpenEconFlows.emitBatches(
        OpenEconFlows.Input(
          c.exports,
          c.totalImports,
          c.tourismExport,
          c.tourismImport,
          c.fdi,
          c.portfolioFlows,
          c.primaryIncome,
          c.euFunds,
          c.diasporaInflow,
          PLN.Zero,
        ),
      ),
      BankingFlows.emitBatches(
        BankingFlows.Input(
          c.bankGovBondIncome,
          c.bankReserveInterest,
          c.bankStandingFacility,
          c.bankInterbankInterest,
          c.bankBfgLevy,
          c.bankUnrealizedLoss,
          c.bankBailIn,
          c.bankNbpRemittance,
        ),
      ),
    )

  def emitAllFlows(c: MonthlyCalculus)(using p: SimParams): Vector[Flow] =
    AggregateBatchContract.toLegacyFlows(emitAllBatches(c))

  /** All intermediate step outputs needed for both MonthlyCalculus and
    * WorldAssembly. Computed once per step — eliminates the double-computation
    * bug where s1–s9 ran twice with diverging RNG state.
    */
  private case class FullComputation(
      calculus: MonthlyCalculus,
      fiscal: FiscalConstraintEconomics.Result,
      s2: LaborEconomics.Output,
      s3: HouseholdIncomeEconomics.Output,
      s4: DemandEconomics.Output,
      s5: FirmEconomics.StepOutput,
      s6: HouseholdFinancialEconomics.Output,
      s7: PriceEquityEconomics.Output,
      s8: OpenEconEconomics.StepOutput,
      s9: BankingEconomics.StepOutput,
  )

  /** Compute MonthlyCalculus by chaining all Economics. Uses old pipeline steps
    * for HH/Demand/Firm/PriceEquity (pure calculus). Uses self-contained
    * OpenEconEconomics for monetary/external. Uses BankingEconomics (delegates
    * to BankingEconomics.runStep) for banking.
    *
    * Returns FullComputation with both calculus AND step outputs for
    * WorldAssembly.
    */
  private def computeAll(
      w: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      banks: Vector[Banking.BankState],
      rng: Random,
  )(using p: SimParams): FullComputation =
    val fiscal          = FiscalConstraintEconomics.compute(w, banks)
    val s1              = FiscalConstraintEconomics.toOutput(fiscal)
    val labor           = LaborEconomics.compute(w, firms, households, s1)
    val s2Pre           = LaborEconomics.Output(
      labor.wage,
      labor.employed,
      labor.laborDemand,
      labor.wageGrowth,
      labor.aggregateHiringSlack,
      labor.immigration,
      labor.netMigration,
      labor.demographics,
      SocialSecurity.ZusState.zero,
      SocialSecurity.NfzState.zero,
      SocialSecurity.PpkState.zero,
      PLN.Zero,
      EarmarkedFunds.State.zero,
      labor.living,
      labor.regionalWages,
    )
    val s3              = HouseholdIncomeEconomics.compute(w, firms, households, banks, s1.lendingBaseRate, s1.resWage, s2Pre.newWage, rng)
    val s4              = DemandEconomics.compute(DemandEconomics.Input(w, s2Pre.employed, s2Pre.living, s3.domesticCons))
    val s5              = FirmEconomics.runStep(w, firms, households, banks, s1, s2Pre, s3, s4, rng)
    val postLivingFirms = s5.ioFirms.filter(Firm.isAlive)
    val s2              = LaborEconomics.reconcilePostFirmStep(w, s1, s2Pre, postLivingFirms, s5.households)
    val s6              = HouseholdFinancialEconomics.compute(w, s1.m, s2.employed, s3.hhAgg, rng)
    val s7              = PriceEquityEconomics.compute(
      PriceEquityEconomics.Input(
        w,
        s1.m,
        s2.newWage,
        s2.employed,
        s2.wageGrowth,
        s3.domesticCons,
        s4.govPurchases,
        s4.avgDemandMult,
        s4.sectorMults,
        banks,
        s5,
      ),
      rng,
    )
    val openEcon        = OpenEconEconomics.compute(
      OpenEconEconomics.Input(
        w = w,
        employed = s2.employed,
        newWage = s2.newWage,
        domesticConsumption = s3.domesticCons,
        importConsumption = s3.importCons,
        totalTechAndInvImports = s5.sumTechImp,
        gdp = s7.gdp,
        newInflation = s7.newInfl,
        autoRatio = s7.autoR,
        govPurchases = s4.govPurchases,
        sectorMults = s4.sectorMults,
        livingFirms = s5.ioFirms,
        totalBondDefault = s5.totalBondDefault,
        actualBondIssuance = s5.actualBondIssuance,
        corpBondAbsorption = s5.corpBondAbsorption,
        euMonthly = s7.euMonthly,
        remittanceOutflow = s6.remittanceOutflow,
        diasporaInflow = s6.diasporaInflow,
        tourismExport = s6.tourismExport,
        tourismImport = s6.tourismImport,
        equityReturn = w.financial.equity.monthlyReturn,
        investmentImports = s7.investmentImports,
        profitShifting = s5.sumProfitShifting,
        fdiRepatriation = s5.sumFdiRepatriation,
        foreignDividendOutflow = s7.foreignDividendOutflow,
        banks = banks,
        month = fiscal.month,
        commodityRng = rng,
      ),
    )
    val s8              = OpenEconEconomics.runStep(OpenEconEconomics.StepInput(w, s1, s2, s3, s4, s5, s6, s7, banks, rng))
    val banking         = BankingEconomics.compute(
      BankingEconomics.Input(
        w = w,
        month = fiscal.month,
        lendingBaseRate = fiscal.lendingBaseRate,
        resWage = fiscal.resWage,
        baseMinWage = fiscal.baseMinWage,
        minWagePriceLevel = fiscal.updatedMinWagePriceLevel,
        employed = s2.employed,
        newWage = s2.newWage,
        laborDemand = s2.laborDemand,
        wageGrowth = s2.wageGrowth,
        govPurchases = s4.govPurchases,
        sectorMults = s4.sectorMults,
        avgDemandMult = s4.avgDemandMult,
        sectorCap = s4.sectorCap,
        laggedInvestDemand = s4.laggedInvestDemand,
        fiscalRuleStatus = s4.fiscalRuleStatus,
        laborOutput = s2,
        hhOutput = s3,
        firmOutput = s5,
        hhFinancialOutput = s6,
        priceEquityOutput = s7,
        openEconOutput = s8,
        banks = banks,
        depositRng = rng,
      ),
    )
    val s9              = BankingEconomics.runStep(BankingEconomics.StepInput(w, s1, s2, s3, s4, s5, s6, s7, s8, banks, rng))
    val agg             = s3.hhAgg
    val eq              = w.financial.equity
    val h               = s9.housingAfterFlows
    val calc            = MonthlyCalculus(
      month = fiscal.month,
      resWage = fiscal.resWage,
      lendingBaseRate = fiscal.lendingBaseRate,
      baseMinWage = fiscal.baseMinWage,
      minWagePriceLevel = fiscal.updatedMinWagePriceLevel,
      wage = s2.newWage,
      employed = s2.employed,
      laborDemand = s2.laborDemand,
      retirees = labor.demographics.retirees,
      workingAgePop = labor.demographics.workingAgePop,
      nBankruptFirms = labor.nBankruptFirms,
      avgFirmWorkers = if s2.living.nonEmpty then s2.laborDemand / s2.living.length else 0,
      totalIncome = s3.totalIncome,
      consumption = agg.consumption,
      domesticConsumption = s3.domesticCons,
      importConsumption = s3.importCons,
      totalRent = agg.totalRent,
      totalPit = agg.totalPit,
      totalDebtService = agg.totalDebtService,
      totalDepositInterest = agg.totalDepositInterest,
      totalRemittances = agg.totalRemittances,
      totalUnempBenefits = agg.totalUnempBenefits,
      totalSocialTransfers = agg.totalSocialTransfers,
      totalCcOrigination = agg.totalConsumerOrigination,
      totalCcDebtService = agg.totalConsumerDebtService,
      totalCcDefault = agg.totalConsumerDefault,
      govPurchases = s4.govPurchases,
      firmTax = s5.sumTax,
      firmNewLoans = s5.sumNewLoans,
      firmPrincipal = s5.sumFirmPrincipal,
      firmInterestIncome = s5.intIncome,
      firmCapex = s5.sumCapex,
      firmEquityIssuance = s5.sumEquityIssuance,
      firmBondIssuance = s5.actualBondIssuance,
      firmIoPayments = s5.totalIoPaid,
      firmNplLoss = s5.nplLoss,
      firmProfitShifting = s5.sumProfitShifting,
      firmFdiRepatriation = s5.sumFdiRepatriation,
      firmGrossInvestment = s5.sumGrossInvestment,
      gdp = s7.gdp,
      inflation = s7.newInfl,
      equityDomDividends = eq.lastDomesticDividends,
      equityForDividends = eq.lastForeignDividends,
      equityDivTax = eq.lastDividendTax,
      equityIssuance = eq.lastIssuance,
      equityReturn = eq.monthlyReturn,
      exports = openEcon.exports,
      totalImports = openEcon.totalImports,
      tourismExport = s6.tourismExport,
      tourismImport = s6.tourismImport,
      fdi = openEcon.fdi,
      portfolioFlows = openEcon.portfolioFlows,
      primaryIncome = openEcon.primaryIncome,
      euFunds = openEcon.euFunds,
      diasporaInflow = s6.diasporaInflow,
      corpBondCoupon = openEcon.corpBondCoupon,
      corpBondDefaultLoss = openEcon.corpBondDefaultLoss,
      corpBondIssuance = openEcon.corpBondIssuance,
      corpBondAmortization = openEcon.corpBondAmortization,
      mortgageOrigination = h.lastOrigination,
      mortgageRepayment = h.lastRepayment,
      mortgageInterest = h.mortgageInterestIncome,
      mortgageDefault = h.lastDefault,
      bankGovBondIncome = banking.govBondIncome,
      bankReserveInterest = banking.reserveInterest,
      bankStandingFacility = banking.standingFacilityIncome,
      bankInterbankInterest = banking.interbankInterest,
      bankBfgLevy = banking.bfgLevy,
      bankUnrealizedLoss = banking.unrealizedBondLoss,
      bankBailIn = banking.bailInLoss,
      bankNbpRemittance = banking.nbpRemittance,
      govTaxRevenue = w.gov.taxRevenue,
      govDebtService = w.gov.debtServiceSpend,
      govEuCofinancing = w.gov.euCofinancing,
      govCapitalSpend = w.gov.govCapitalSpend,
      insurancePrevGovBonds = w.financial.insurance.govBondHoldings,
      insurancePrevCorpBonds = w.financial.insurance.corpBondHoldings,
      insurancePrevEquity = w.financial.insurance.equityHoldings,
      govBondYield = openEcon.newBondYield,
      corpBondYield = openEcon.corpBondYield,
    )
    FullComputation(calc, fiscal, s2, s3, s4, s5, s6, s7, s8, s9)

  /** Public API: compute calculus only (for tests that need MonthlyCalculus).
    */
  def computeCalculus(
      w: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      banks: Vector[Banking.BankState],
      rng: Random,
  )(using p: SimParams): MonthlyCalculus =
    computeAll(w, firms, households, banks, rng).calculus

  case class StepResult(
      calculus: MonthlyCalculus,
      flows: Vector[BatchedFlow],
      execution: ExecutionResult,
      sfcResult: Sfc.SfcResult,
      newWorld: World,
      newFirms: Vector[Firm.State],
      newHouseholds: Vector[Household.State],
      newBanks: Vector[Banking.BankState],
      householdAggregates: Household.Aggregates,
  )

  private def executeBatches(flows: Vector[BatchedFlow]): Either[String, ExecutionResult] =
    val state = AggregateBatchContract.emptyExecutionState()
    ImperativeInterpreter
      .planAndApplyAll(state, flows)
      .map: _ =>
        val snapshot = state.snapshot
        ExecutionResult(
          snapshot = snapshot,
          totalWealth = AggregateBatchContract.totalWealth(snapshot),
        )

  private def runtimeState(
      w: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      banks: Vector[Banking.BankState],
  ): Sfc.RuntimeState =
    Sfc.RuntimeState(w, firms, households, banks)

  private case class ExecutedBatchEvidence(
      totals: Map[MechanismId, Long],
      signedTotals: Map[MechanismId, Long],
  ):
    def amount(mechanism: MechanismId): PLN =
      PLN.fromRaw(totals.getOrElse(mechanism, 0L))

    def signedAmount(mechanism: MechanismId): PLN =
      PLN.fromRaw(signedTotals.getOrElse(mechanism, 0L))

    def sum(mechanisms: MechanismId*): PLN =
      PLN.fromRaw(mechanisms.iterator.map(m => totals.getOrElse(m, 0L)).sum)

  private object ExecutedBatchEvidence:
    def from(batches: Vector[BatchedFlow]): ExecutedBatchEvidence =
      val (totals, signedTotals) =
        batches.foldLeft(
          Map.empty[MechanismId, Long].withDefaultValue(0L),
          Map.empty[MechanismId, Long].withDefaultValue(0L),
        ):
          case ((totalsAcc, signedAcc), batch) =>
            val amount       = AggregateBatchContract.totalTransferred(batch)
            val signedAmount =
              if batch.mechanism == FlowMechanism.BankInterbankInterest || batch.mechanism == FlowMechanism.BankStandingFacility then
                (batch.from, batch.to) match
                  case (EntitySector.NBP, EntitySector.Banks) => amount
                  case (EntitySector.Banks, EntitySector.NBP) => -amount
                  case _                                      => amount
              else amount

            (
              totalsAcc.updated(batch.mechanism, totalsAcc(batch.mechanism) + amount),
              signedAcc.updated(batch.mechanism, signedAcc(batch.mechanism) + signedAmount),
            )

      ExecutedBatchEvidence(totals, signedTotals)

  private def buildSfcFlows(full: FullComputation, batches: Vector[BatchedFlow], fofResidual: PLN)(using p: SimParams): Sfc.SemanticFlows =
    val evidence = ExecutedBatchEvidence.from(batches)
    Sfc.SemanticFlows(
      govSpending =
        full.s9.newGovWithYield.domesticBudgetOutlays + full.s2.newZus.govSubvention + full.s2.newNfz.govSubvention + full.s2.newEarmarked.totalGovSubvention,
      govRevenue =
        full.s5.sumTax + full.s7.dividendTax + full.s9.pitAfterEvasion + full.s9.vatAfterEvasion + full.s8.banking.nbpRemittance + full.s9.exciseAfterEvasion + full.s9.customsDutyRevenue,
      nplLoss = full.s5.nplLoss,
      interestIncome = evidence.amount(FlowMechanism.FirmInterestPaid),
      hhDebtService = evidence.amount(FlowMechanism.HhDebtService),
      totalIncome = full.s3.totalIncome,
      totalConsumption = evidence.amount(FlowMechanism.HhConsumption),
      newLoans = evidence.amount(FlowMechanism.FirmNewLoan),
      nplRecovery = full.s5.nplNew * p.banking.loanRecovery,
      currentAccount = full.s8.external.newBop.currentAccount,
      valuationEffect = full.s8.external.oeValuationEffect,
      bankBondIncome = evidence.amount(FlowMechanism.BankGovBondIncome),
      qePurchase = full.s8.monetary.qePurchaseAmount,
      newBondIssuance = if p.flags.govBondMarket then full.s9.actualBondChange else PLN.Zero,
      depositInterestPaid = evidence.amount(FlowMechanism.HhDepositInterest),
      reserveInterest = evidence.amount(FlowMechanism.BankReserveInterest),
      standingFacilityIncome = evidence.signedAmount(FlowMechanism.BankStandingFacility),
      interbankInterest = evidence.signedAmount(FlowMechanism.BankInterbankInterest),
      jstDepositChange = full.s9.jstDepositChange,
      jstSpending = full.s9.newJst.spending,
      jstRevenue = full.s9.newJst.revenue,
      zusContributions = full.s2.newZus.contributions,
      zusPensionPayments = full.s2.newZus.pensionPayments,
      zusGovSubvention = full.s2.newZus.govSubvention,
      nfzContributions = full.s2.newNfz.contributions,
      nfzSpending = full.s2.newNfz.spending,
      nfzGovSubvention = full.s2.newNfz.govSubvention,
      dividendIncome = full.s7.netDomesticDividends,
      foreignDividendOutflow = full.s7.foreignDividendOutflow,
      dividendTax = full.s7.dividendTax,
      mortgageInterestIncome = evidence.amount(FlowMechanism.MortgageInterest),
      mortgageNplLoss = evidence.amount(FlowMechanism.MortgageDefault) * (Share.One - p.housing.mortgageRecovery),
      mortgageOrigination = evidence.amount(FlowMechanism.MortgageOrigination),
      mortgagePrincipalRepaid = evidence.amount(FlowMechanism.MortgageRepayment),
      mortgageDefaultAmount = evidence.amount(FlowMechanism.MortgageDefault),
      remittanceOutflow = evidence.amount(FlowMechanism.HhRemittance),
      fofResidual = fofResidual,
      consumerDebtService = evidence.amount(FlowMechanism.HhCcDebtService),
      consumerNplLoss = full.s6.consumerNplLoss,
      consumerOrigination = evidence.amount(FlowMechanism.HhCcOrigination),
      consumerPrincipalRepaid = full.s6.consumerPrincipal,
      consumerDefaultAmount = evidence.amount(FlowMechanism.HhCcDefault),
      corpBondCouponIncome = evidence.amount(FlowMechanism.CorpBondCoupon),
      corpBondDefaultLoss = evidence.amount(FlowMechanism.CorpBondDefault),
      corpBondIssuance = evidence.amount(FlowMechanism.CorpBondIssuance),
      corpBondAmortization = evidence.amount(FlowMechanism.CorpBondAmortization),
      corpBondDefaultAmount = evidence.amount(FlowMechanism.CorpBondDefault),
      insNetDepositChange = full.s8.nonBank.insNetDepositChange,
      nbfiDepositDrain = full.s8.nonBank.nbfiDepositDrain,
      nbfiOrigination = full.s9.finalNbfi.lastNbfiOrigination,
      nbfiRepayment = full.s9.finalNbfi.lastNbfiRepayment,
      nbfiDefaultAmount = full.s9.finalNbfi.lastNbfiDefaultAmount,
      fdiProfitShifting = evidence.amount(FlowMechanism.FirmProfitShifting),
      fdiRepatriation = evidence.amount(FlowMechanism.FirmFdiRepatriation),
      diasporaInflow = evidence.amount(FlowMechanism.DiasporaInflow),
      tourismExport = evidence.amount(FlowMechanism.TourismExport),
      tourismImport = evidence.amount(FlowMechanism.TourismImport),
      bfgLevy = evidence.amount(FlowMechanism.BankBfgLevy),
      bailInLoss = evidence.amount(FlowMechanism.BankBailIn),
      bankCapitalDestruction = full.s9.multiCapDestruction,
      investNetDepositFlow = full.s9.investNetDepositFlow,
      firmPrincipalRepaid = evidence.amount(FlowMechanism.FirmLoanRepayment),
      unrealizedBondLoss = evidence.amount(FlowMechanism.BankUnrealizedLoss),
      htmRealizedLoss = full.s9.htmRealizedLoss,
      eclProvisionChange = full.s9.eclProvisionChange,
    )

  /** Full step: compute calculus → emit flows → assemble new World.
    *
    * This is the pipeline entry point. Runs s1–s9 exactly once via
    * computeAll(), then feeds both MonthlyCalculus (for flows) and step outputs
    * (for WorldAssembly) from that single computation.
    */
  def step(w: World, firms: Vector[Firm.State], households: Vector[Household.State], banks: Vector[Banking.BankState], rng: Random)(using
      p: SimParams,
  ): StepResult =
    val full      = computeAll(w, firms, households, banks, rng)
    val flows     = emitAllBatches(full.calculus)
    val execution = executeBatches(flows).fold(
      err => throw IllegalStateException(s"Ledger batch execution failed: $err"),
      identity,
    )

    val assembled = WorldAssemblyEconomics.compute(
      WorldAssemblyEconomics.Input(
        w = w,
        firms = firms,
        households = households,
        month = full.fiscal.month,
        lendingBaseRate = full.fiscal.lendingBaseRate,
        resWage = full.fiscal.resWage,
        baseMinWage = full.fiscal.baseMinWage,
        minWagePriceLevel = full.fiscal.updatedMinWagePriceLevel,
        govPurchases = full.s4.govPurchases,
        sectorMults = full.s4.sectorMults,
        avgDemandMult = full.s4.avgDemandMult,
        sectorCap = full.s4.sectorCap,
        laggedInvestDemand = full.s4.laggedInvestDemand,
        fiscalRuleStatus = full.s4.fiscalRuleStatus,
        laborOutput = full.s2,
        hhOutput = full.s3,
        firmOutput = full.s5,
        hhFinancialOutput = full.s6,
        priceEquityOutput = full.s7,
        openEconOutput = full.s8,
        bankOutput = full.s9,
        banks = banks,
        rng = rng,
        migRng = rng,
      ),
    )
    val sfcFlows  = buildSfcFlows(full, flows, assembled.world.plumbing.fofResidual)
    val sfcResult = Sfc.validate(
      prev = runtimeState(w, firms, households, banks),
      curr = runtimeState(assembled.world, assembled.firms, assembled.households, assembled.banks),
      flows = sfcFlows,
      batches = flows,
      executionSnapshot = Sfc.ExecutionSnapshot.fromRaw(execution.snapshot),
      totalWealth = execution.totalWealth,
      tolerance = PLN.Zero,
      nfaTolerance = PLN(1000.0),
    )

    StepResult(
      full.calculus,
      flows,
      execution,
      sfcResult,
      assembled.world,
      assembled.firms,
      assembled.households,
      assembled.banks,
      assembled.householdAggregates,
    )
