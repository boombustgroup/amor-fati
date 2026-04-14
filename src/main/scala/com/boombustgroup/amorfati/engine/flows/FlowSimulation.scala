package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.accounting.Sfc
import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.*
import com.boombustgroup.amorfati.engine.SimulationMonth.{CompletedMonth, ExecutionMonth}
import com.boombustgroup.amorfati.engine.economics.*
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*

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

  /** Single typed input to one monthly step.
    *
    * The simulation loop should treat this as the month-`t` boundary state that
    * flows into one `step(state, randomness)` transition.
    */
  case class SimState(
      completedMonth: CompletedMonth,
      world: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      banks: Vector[Banking.BankState],
      householdAggregates: Household.Aggregates,
  )

  object SimState:
    def fromInit(init: com.boombustgroup.amorfati.init.WorldInit.InitResult): SimState =
      SimState(CompletedMonth.Zero, init.world, init.firms, init.households, init.banks, init.householdAggregates)

  case class ExecutionResult(
      snapshot: Map[(EntitySector, AssetType, Int), Long],
      totalWealth: Long,
  )

  /** All calculus results needed to feed flow mechanisms. */
  case class MonthlyCalculus(
      // Stage 1: Fiscal constraint
      month: ExecutionMonth,
      resWage: PLN,
      lendingBaseRate: Rate,
      baseMinWage: PLN,
      minWagePriceLevel: PriceIndex,
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
      equityGovDividends: PLN,
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
      EquityFlows.emitBatches(
        EquityFlows.Input(
          c.equityDomDividends,
          c.equityForDividends,
          c.equityDivTax,
          c.equityGovDividends,
          c.equityIssuance,
        ),
      ),
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

  /** Typed month-`t` boundary input used internally by [[step]]. */
  case class StepInput(
      stateIn: SimState,
      executionMonth: ExecutionMonth,
      seedIn: MonthSemantics.SeedIn,
      randomness: MonthRandomness.Contract,
      boundaryIn: MonthBoundarySnapshot,
  )

  /** Same-month signal surface reused by operational, timing, and seed
    * boundaries.
    */
  private[engine] case class SignalBoundaryInputs(
      labor: LaborEconomics.Output,
      demand: DemandEconomics.Output,
  )

  /** Same-month payload narrowed for executed-batch -> SFC semantic projection.
    */
  private[engine] case class SemanticFlowInputs(
      labor: LaborEconomics.Output,
      hhIncome: HouseholdIncomeEconomics.Output,
      firms: FirmEconomics.StepOutput,
      hhFinancial: HouseholdFinancialEconomics.Output,
      prices: PriceEquityEconomics.Output,
      openEcon: OpenEconEconomics.StepOutput,
      banking: BankingEconomics.StepOutput,
  )

  /** Same-month groups computed once, then immediately retyped into
    * downstream-specific month boundary views.
    */
  private case class StageOutputs(
      flowPlan: MonthlyCalculus,
      signals: SignalBoundaryInputs,
      postAssembly: WorldAssemblyEconomics.StepInput,
      semanticProjection: SemanticFlowInputs,
  )

  /** Post-assembly view of month `t`: assembled world plus the narrow payload
    * needed to build [[MonthTrace]].
    */
  case class PostMonth(
      assembled: WorldAssemblyEconomics.PostResult,
      boundaryOut: MonthBoundarySnapshot,
      timing: MonthTimingTrace,
  )

  /** Full typed boundary carried through one step: pre-step seed, narrow
    * same-month groups, post-month assembly, then the extracted seed for
    * `t + 1`.
    */
  case class MonthOutcome(
      operational: MonthSemantics.Operational,
      flowPlan: MonthSemantics.FlowPlan,
      semanticProjection: MonthSemantics.SemanticProjection,
      post: MonthSemantics.PostAssembly,
      seedOut: MonthSemantics.SeedOut,
      traceCore: MonthTraceCore,
  )

  /** Full month-step contract.
    *
    * `stateIn -> StepOutput(nextState, trace)` is the explicit monthly
    * boundary. The runtime may still drive this incrementally, but the
    * architectural shape is a single transition from one [[SimState]] to the
    * next.
    */
  case class StepOutput(
      stateIn: SimState,
      executionMonth: ExecutionMonth,
      calculus: MonthlyCalculus,
      operationalSignals: OperationalSignals,
      signalExtraction: SignalExtraction.Output,
      randomness: MonthRandomness.Contract,
      flows: Vector[BatchedFlow],
      execution: ExecutionResult,
      sfcResult: Sfc.SfcResult,
      trace: MonthTrace,
      nextState: SimState,
  ):
    def transition: (SimState, MonthTrace) = (nextState, trace)

  /** Single-month explicit public boundary.
    *
    * [[com.boombustgroup.amorfati.engine.MonthDriver.unfoldSteps]] is the
    * preferred runtime entrypoint, but `step` remains public as the narrow
    * one-month contract used by tests, replay, and diagnostics.
    */
  def step(stateIn: SimState, randomness: MonthRandomness.Contract)(using p: SimParams): StepOutput =
    val input      = stepInput(stateIn, randomness)
    val outcome    = computeMonthOutcome(input)
    val flows      = emitAllBatches(outcome.flowPlan.calculus)
    val execution  = executeBatches(flows).fold(
      err => throw new IllegalStateException(s"Ledger batch execution failed: $err"),
      identity,
    )
    val nextState  = advanceState(input, outcome)
    val sfcFlows   = buildSfcFlows(outcome.semanticProjection, flows, nextState.world.plumbing.fofResidual)
    val sfcResult  = Sfc.validate(
      prev = runtimeState(stateIn.world, stateIn.firms, stateIn.households, stateIn.banks),
      curr = runtimeState(nextState.world, nextState.firms, nextState.households, nextState.banks),
      flows = sfcFlows,
      batches = flows,
      executionSnapshot = Sfc.ExecutionSnapshot.fromRaw(execution.snapshot),
      totalWealth = execution.totalWealth,
    )
    val monthTrace = buildMonthTrace(
      input = input,
      outcome = outcome,
      executedFlows = sfcFlows,
      sfcResult = sfcResult,
    )

    StepOutput(
      stateIn = stateIn,
      executionMonth = input.executionMonth,
      calculus = outcome.flowPlan.calculus,
      operationalSignals = outcome.operational.operationalSignals,
      signalExtraction = outcome.seedOut.signalExtraction,
      randomness = randomness,
      flows,
      execution,
      sfcResult,
      trace = monthTrace,
      nextState = nextState,
    )

  /** Public API: compute calculus only (for tests that need MonthlyCalculus).
    */
  def computeCalculus(state: SimState, randomness: MonthRandomness.Contract)(using p: SimParams): MonthlyCalculus =
    computeStageOutputs(stepInput(state, randomness)).flowPlan

  private def stepInput(
      stateIn: SimState,
      randomness: MonthRandomness.Contract,
  ): StepInput =
    StepInput(
      stateIn = stateIn,
      executionMonth = stateIn.completedMonth.next,
      seedIn = MonthSemantics.seedIn(stateIn.world.seedIn),
      randomness = randomness,
      boundaryIn = MonthBoundarySnapshot.capture(stateIn.world, stateIn.firms, stateIn.households, stateIn.banks),
    )

  /** Compute same-month groups by chaining all Economics. Uses old pipeline
    * steps for HH/Demand/Firm/PriceEquity (pure calculus). Uses self-contained
    * OpenEconEconomics for monetary/external. Runs BankingEconomics exactly
    * once, then narrows the results into flow, signal, post-assembly, and SFC
    * projection views so later boundaries do not depend on one broad transport
    * bag.
    */
  private def computeStageOutputs(input: StepInput)(using p: SimParams): StageOutputs =
    val randomness        = input.randomness.stages
    val stateIn           = input.stateIn
    val w                 = stateIn.world
    val firms             = stateIn.firms
    val households        = stateIn.households
    val banks             = stateIn.banks
    val fiscal            = FiscalConstraintEconomics.compute(w, banks, input.executionMonth)
    val s1                = FiscalConstraintEconomics.toOutput(fiscal)
    val labor             = LaborEconomics.compute(w, firms, households, s1)
    val s2Pre             = LaborEconomics.Output(
      labor.wage,
      labor.employed,
      labor.laborDemand,
      labor.wageGrowth,
      labor.operationalHiringSlack,
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
    val s3                = HouseholdIncomeEconomics.compute(
      w,
      firms,
      households,
      banks,
      s1.lendingBaseRate,
      s1.resWage,
      s2Pre.newWage,
      randomness.householdIncomeEconomics.newStream(),
    )
    val s4                = DemandEconomics.compute(DemandEconomics.Input(w, s2Pre.employed, s2Pre.living, s3.domesticCons))
    val s5                = FirmEconomics.runStep(w, firms, households, banks, s1, s2Pre, s3, s4, randomness.firmEconomics.newStream())
    val postLivingFirms   = s5.ioFirms.filter(Firm.isAlive)
    val s2                = LaborEconomics.reconcilePostFirmStep(w, s1, s2Pre, postLivingFirms, s5.households)
    val s6                = HouseholdFinancialEconomics.compute(w, s1.m, s2.employed, s3.hhAgg, randomness.householdFinancialEconomics.newStream())
    val s7                = PriceEquityEconomics.compute(
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
      randomness.priceEquityEconomics.newStream(),
    )
    val openEcon          = OpenEconEconomics.compute(
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
        commodityRng = randomness.openEconEconomics.newStream(),
      ),
    )
    val s8                = OpenEconEconomics.runStep(
      OpenEconEconomics.StepInput(w, s1, s2, s3, s4, s5, s6, s7, banks, randomness.openEconEconomics.newStream()),
    )
    val operational       = operationalSignals(s2, s4)
    val bankingDepositRng = randomness.bankingEconomics.newStream()
    val bankingInput      = BankingEconomics.Input(
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
      avgDemandMult = s4.avgDemandMult,
      sectorCapReal = s4.sectorCapReal,
      laggedInvestDemand = s4.laggedInvestDemand,
      fiscalRuleStatus = s4.fiscalRuleStatus,
      laborOutput = s2,
      operationalSignals = operational,
      hhOutput = s3,
      firmOutput = s5,
      hhFinancialOutput = s6,
      priceEquityOutput = s7,
      openEconOutput = s8,
      banks = banks,
      depositRng = bankingDepositRng,
    )
    val s9                = BankingEconomics.runStep(
      BankingEconomics.StepInput(w, s1, s2, s3, s4, s5, s6, s7, s8, banks, bankingDepositRng),
    )
    val banking           = BankingEconomics.toResult(s9, bankingInput)
    val agg               = s3.hhAgg
    val eq                = w.financial.equity
    val h                 = s9.housingAfterFlows
    val calc              = MonthlyCalculus(
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
      equityGovDividends = w.gov.govDividendRevenue,
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
    StageOutputs(
      flowPlan = calc,
      signals = SignalBoundaryInputs(
        labor = s2,
        demand = s4,
      ),
      postAssembly = WorldAssemblyEconomics.StepInput(
        w = w,
        firms = firms,
        households = households,
        banks = banks,
        s1 = s1,
        s2 = s2,
        s3 = s3,
        s4 = s4,
        s5 = s5,
        s6 = s6,
        s7 = s7,
        s8 = s8,
        s9 = s9,
      ),
      semanticProjection = SemanticFlowInputs(
        labor = s2,
        hhIncome = s3,
        firms = s5,
        hhFinancial = s6,
        prices = s7,
        openEcon = s8,
        banking = s9,
      ),
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

  private def buildSfcFlows(
      semanticProjection: MonthSemantics.SemanticProjection,
      batches: Vector[BatchedFlow],
      fofResidual: PLN,
  )(using p: SimParams): Sfc.SemanticFlows =
    val labor       = semanticProjection.labor
    val hhIncome    = semanticProjection.hhIncome
    val firms       = semanticProjection.firms
    val hhFinancial = semanticProjection.hhFinancial
    val prices      = semanticProjection.prices
    val openEcon    = semanticProjection.openEcon
    val banking     = semanticProjection.banking
    val evidence    = ExecutedBatchEvidence.from(batches)
    Sfc.SemanticFlows(
      govSpending =
        banking.newGovWithYield.domesticBudgetOutlays + labor.newZus.govSubvention + labor.newNfz.govSubvention + labor.newEarmarked.totalGovSubvention,
      govRevenue = firms.sumTax + prices.dividendTax + evidence.amount(
        FlowMechanism.EquityGovDividend,
      ) + banking.pitAfterEvasion + banking.vatAfterEvasion + openEcon.banking.nbpRemittance + banking.exciseAfterEvasion + banking.customsDutyRevenue,
      nplLoss = firms.nplLoss,
      interestIncome = evidence.amount(FlowMechanism.FirmInterestPaid),
      hhDebtService = evidence.amount(FlowMechanism.HhDebtService),
      totalIncome = hhIncome.totalIncome,
      totalConsumption = evidence.amount(FlowMechanism.HhConsumption),
      newLoans = evidence.amount(FlowMechanism.FirmNewLoan),
      nplRecovery = firms.nplNew * p.banking.loanRecovery,
      currentAccount = openEcon.external.newBop.currentAccount,
      valuationEffect = openEcon.external.oeValuationEffect,
      bankBondIncome = evidence.amount(FlowMechanism.BankGovBondIncome),
      qePurchase = openEcon.monetary.qePurchaseAmount,
      newBondIssuance = banking.actualBondChange,
      depositInterestPaid = evidence.amount(FlowMechanism.HhDepositInterest),
      reserveInterest = evidence.amount(FlowMechanism.BankReserveInterest),
      standingFacilityIncome = evidence.signedAmount(FlowMechanism.BankStandingFacility),
      interbankInterest = evidence.signedAmount(FlowMechanism.BankInterbankInterest),
      jstDepositChange = banking.jstDepositChange,
      jstSpending = banking.newJst.spending,
      jstRevenue = banking.newJst.revenue,
      zusContributions = labor.newZus.contributions,
      zusPensionPayments = labor.newZus.pensionPayments,
      zusGovSubvention = labor.newZus.govSubvention,
      nfzContributions = labor.newNfz.contributions,
      nfzSpending = labor.newNfz.spending,
      nfzGovSubvention = labor.newNfz.govSubvention,
      dividendIncome = prices.netDomesticDividends,
      foreignDividendOutflow = prices.foreignDividendOutflow,
      dividendTax = prices.dividendTax,
      mortgageInterestIncome = evidence.amount(FlowMechanism.MortgageInterest),
      mortgageNplLoss = evidence.amount(FlowMechanism.MortgageDefault) * (Share.One - p.housing.mortgageRecovery),
      mortgageOrigination = evidence.amount(FlowMechanism.MortgageOrigination),
      mortgagePrincipalRepaid = evidence.amount(FlowMechanism.MortgageRepayment),
      mortgageDefaultAmount = evidence.amount(FlowMechanism.MortgageDefault),
      remittanceOutflow = evidence.amount(FlowMechanism.HhRemittance),
      fofResidual = fofResidual,
      consumerDebtService = evidence.amount(FlowMechanism.HhCcDebtService),
      consumerNplLoss = hhFinancial.consumerNplLoss,
      consumerOrigination = evidence.amount(FlowMechanism.HhCcOrigination),
      consumerPrincipalRepaid = hhFinancial.consumerPrincipal,
      consumerDefaultAmount = evidence.amount(FlowMechanism.HhCcDefault),
      corpBondCouponIncome = evidence.amount(FlowMechanism.CorpBondCoupon),
      corpBondDefaultLoss = evidence.amount(FlowMechanism.CorpBondDefault),
      corpBondIssuance = evidence.amount(FlowMechanism.CorpBondIssuance),
      corpBondAmortization = evidence.amount(FlowMechanism.CorpBondAmortization),
      corpBondDefaultAmount = evidence.amount(FlowMechanism.CorpBondDefault),
      insNetDepositChange = openEcon.nonBank.insNetDepositChange,
      nbfiDepositDrain = openEcon.nonBank.nbfiDepositDrain,
      nbfiOrigination = banking.finalNbfi.lastNbfiOrigination,
      nbfiRepayment = banking.finalNbfi.lastNbfiRepayment,
      nbfiDefaultAmount = banking.finalNbfi.lastNbfiDefaultAmount,
      fdiProfitShifting = evidence.amount(FlowMechanism.FirmProfitShifting),
      fdiRepatriation = evidence.amount(FlowMechanism.FirmFdiRepatriation),
      diasporaInflow = evidence.amount(FlowMechanism.DiasporaInflow),
      tourismExport = evidence.amount(FlowMechanism.TourismExport),
      tourismImport = evidence.amount(FlowMechanism.TourismImport),
      bfgLevy = evidence.amount(FlowMechanism.BankBfgLevy),
      bailInLoss = evidence.amount(FlowMechanism.BankBailIn),
      bankCapitalDestruction = banking.multiCapDestruction,
      investNetDepositFlow = banking.investNetDepositFlow,
      firmPrincipalRepaid = evidence.amount(FlowMechanism.FirmLoanRepayment),
      unrealizedBondLoss = evidence.amount(FlowMechanism.BankUnrealizedLoss),
      htmRealizedLoss = banking.htmRealizedLoss,
      eclProvisionChange = banking.eclProvisionChange,
    )

  private def operationalSignals(
      labor: LaborEconomics.Output,
      demand: DemandEconomics.Output,
  ): OperationalSignals =
    OperationalSignals(
      sectorDemandMult = demand.sectorMults,
      sectorDemandPressure = demand.sectorDemandPressure,
      sectorHiringSignal = demand.sectorHiringSignal,
      operationalHiringSlack = labor.operationalHiringSlack,
    )

  private def buildOperationalSignals(signalView: MonthSemantics.SignalView): MonthSemantics.Operational =
    MonthSemantics.operational(operationalSignals(signalView.labor, signalView.demand))

  private def buildTimingInputs(
      signalView: MonthSemantics.SignalView,
      assembled: WorldAssemblyEconomics.PostResult,
  ): MonthTimingInputs =
    MonthTimingInputs(
      labor = MonthTimingPayload.LaborSignals(
        operationalHiringSlack = signalView.labor.operationalHiringSlack,
      ),
      demand = MonthTimingPayload.DemandSignals(
        sectorDemandMult = signalView.demand.sectorMults,
        sectorDemandPressure = signalView.demand.sectorDemandPressure,
        sectorHiringSignal = signalView.demand.sectorHiringSignal,
      ),
      nominal = MonthTimingPayload.NominalSignals(
        realizedInflation = assembled.world.inflation,
        expectedInflation = assembled.world.mechanisms.expectations.expectedInflation,
      ),
      firmDynamics = MonthTimingPayload.FirmDynamics(
        startupAbsorptionRate = assembled.startupAbsorptionRate,
        firmBirths = assembled.world.flows.firmBirths,
        firmDeaths = assembled.world.flows.firmDeaths,
        netFirmBirths = assembled.world.flows.netFirmBirths,
      ),
    )

  private def assemblePostMonth(
      postInputs: MonthSemantics.PostInputs,
      signalView: MonthSemantics.SignalView,
      randomness: MonthRandomness.AssemblyStreams,
  )(using p: SimParams): MonthSemantics.PostAssembly =
    val assembled = WorldAssemblyEconomics.computePostMonth(postInputs.assemblyInput, randomness)
    // This stays at month `t`: the boundary seed is still `seedIn` here.
    MonthSemantics.postAssembly(
      PostMonth(
        assembled = assembled,
        boundaryOut = MonthBoundarySnapshot.capture(assembled.world, assembled.firms, assembled.households, assembled.banks),
        timing = MonthTimingTrace.fromInputs(buildTimingInputs(signalView, assembled)),
      ),
    )

  private def extractSeedOut(signalView: MonthSemantics.SignalView, post: MonthSemantics.PostAssembly): MonthSemantics.SeedOut =
    val assembled = post.assembled
    val employed  = Household.countEmployed(assembled.households)

    MonthSemantics.seedOut(
      SignalExtraction.compute(
        // Seed extraction is the only place that derives the next boundary
        // signal from realized month-`t` outcomes.
        SignalExtraction.inputFromRealizedOutcomes(
          unemploymentRate = assembled.world.unemploymentRate(employed),
          laggedHiringSlack = signalView.labor.operationalHiringSlack,
          startupAbsorptionRate = assembled.startupAbsorptionRate,
          inflation = assembled.world.inflation,
          expectedInflation = assembled.world.mechanisms.expectations.expectedInflation,
          sectorDemandMult = signalView.demand.sectorMults,
          sectorDemandPressure = signalView.demand.sectorDemandPressure,
          sectorHiringSignal = signalView.demand.sectorHiringSignal,
        ),
      ),
    )

  private def deriveTraceCore(
      input: StepInput,
      post: MonthSemantics.PostAssembly,
      seedOut: MonthSemantics.SeedOut,
  ): MonthTraceCore =
    MonthTraceCore(
      boundary = MonthBoundaryTrace.from(input.boundaryIn, post.boundaryOut),
      seedTransition = SeedTransitionTrace.from(input.seedIn, seedOut),
      timing = post.timing,
    )

  private def computeMonthOutcome(input: StepInput)(using p: SimParams): MonthOutcome =
    // Keep the month-step pipeline explicit:
    // `seedIn/pre -> same-month groups -> post-month assembly -> seedOut/next-pre`.
    val stageOutputs       = computeStageOutputs(input)
    val signalView         = MonthSemantics.signalView(stageOutputs.signals)
    val flowPlan           = MonthSemantics.flowPlan(stageOutputs.flowPlan)
    val postInputs         = MonthSemantics.postInputs(stageOutputs.postAssembly)
    val semanticProjection = MonthSemantics.semanticProjection(stageOutputs.semanticProjection)
    val operational        = buildOperationalSignals(signalView)
    val post               = assemblePostMonth(postInputs, signalView, input.randomness.assembly.newStreams())
    val seedOut            = extractSeedOut(signalView, post)
    val traceCore          = deriveTraceCore(input, post, seedOut)

    MonthOutcome(
      operational = operational,
      flowPlan = flowPlan,
      semanticProjection = semanticProjection,
      post = post,
      seedOut = seedOut,
      traceCore = traceCore,
    )

  private def advanceState(
      input: StepInput,
      outcome: MonthOutcome,
  ): SimState =
    val assembled   = outcome.post.assembled
    val nextSeed    = outcome.seedOut.nextSeed
    val nextWorld   = assembled.world.updatePipeline(_.withDecisionSignals(nextSeed))
    val currentSeed = input.seedIn.decisionSignals

    // `advanceState` is the only legal `post -> next-pre` transition:
    // post-month assembly still sees the old seed, next state applies `seedOut`.
    require(currentSeed == input.stateIn.world.seedIn, "StepInput seedIn must match stateIn.world.seedIn")
    require(assembled.world.seedIn == currentSeed, "PostMonth world must remain on the pre-step seed until advanceState runs")
    require(nextWorld.seedIn == nextSeed, "advanceState must be the transition that applies SeedOut to the next boundary")

    SimState(
      completedMonth = input.executionMonth.completed,
      world = nextWorld,
      firms = assembled.firms,
      households = assembled.households,
      banks = assembled.banks,
      householdAggregates = assembled.householdAggregates,
    )

  private def buildMonthTrace(
      input: StepInput,
      outcome: MonthOutcome,
      executedFlows: Sfc.SemanticFlows,
      sfcResult: Sfc.SfcResult,
  ): MonthTrace =
    MonthTrace.fromCore(
      executionMonth = input.executionMonth,
      randomness = input.randomness,
      core = outcome.traceCore,
      executedFlows = executedFlows,
      validations = Vector(MonthValidation.fromSfcResult(sfcResult)),
    )
