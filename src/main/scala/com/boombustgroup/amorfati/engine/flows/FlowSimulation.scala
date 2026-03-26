package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.engine.economics.*
import com.boombustgroup.amorfati.engine.steps.*
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*

import scala.util.Random

/** New flow-based simulation pipeline.
  *
  * Contract-First Design: shaped by the 14 flow mechanism Input contracts, not
  * by old step code.
  *
  * Three stages per month:
  *   1. CALCULUS — pure economics (CES, Phillips, Taylor, Meen, Calvo)
  *   2. TRANSLATION — map calculus results to flow mechanism inputs
  *   3. PLUMBING — emit flows, apply through verified interpreter
  *
  * SFC == 0L by construction (verified interpreter). No post-hoc validation
  * needed.
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
  def emitAllFlows(c: MonthlyCalculus)(using p: SimParams): Vector[Flow] =
    Vector.concat(
      // Tier 1: Social funds
      ZusFlows.emit(ZusFlows.ZusInput(c.employed, c.wage, c.retirees)),
      NfzFlows.emit(NfzFlows.NfzInput(c.employed, c.wage, c.workingAgePop, c.retirees)),
      PpkFlows.emit(PpkFlows.PpkInput(c.employed, c.wage)),
      EarmarkedFlows.emit(EarmarkedFlows.Input(c.employed, c.wage, c.totalUnempBenefits, c.nBankruptFirms, c.avgFirmWorkers)),
      JstFlows.emit(JstFlows.Input(c.govTaxRevenue, c.totalIncome, c.gdp, c.laborDemand, c.totalPit)),
      // Tier 2: Agents
      HouseholdFlows.emit(
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
      FirmFlows.emit(
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
      GovBudgetFlows.emit(
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
      InsuranceFlows.emit(
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
      EquityFlows.emit(EquityFlows.Input(c.equityDomDividends, c.equityForDividends, c.equityDivTax, c.equityIssuance)),
      CorpBondFlows.emit(CorpBondFlows.Input(c.corpBondCoupon, c.corpBondDefaultLoss, c.corpBondIssuance, c.corpBondAmortization)),
      MortgageFlows.emit(MortgageFlows.Input(c.mortgageOrigination, c.mortgageRepayment, c.mortgageInterest, c.mortgageDefault)),
      OpenEconFlows.emit(
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
      BankingFlows.emit(
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

  /** All intermediate step outputs needed for both MonthlyCalculus and
    * WorldAssembly. Computed once per step — eliminates the double-computation
    * bug where s1–s9 ran twice with diverging RNG state.
    */
  private case class FullComputation(
      calculus: MonthlyCalculus,
      fiscal: FiscalConstraintEconomics.Result,
      s2: LaborDemographicsStep.Output,
      s3: HouseholdIncomeStep.Output,
      s4: DemandStep.Output,
      s5: FirmProcessingStep.Output,
      s6: HouseholdFinancialStep.Output,
      s7: PriceEquityStep.Output,
      s8: OpenEconomyStep.Output,
      s9: BankUpdateStep.Output,
  )

  /** Compute MonthlyCalculus by chaining all Economics. Uses old pipeline steps
    * for HH/Demand/Firm/PriceEquity (pure calculus). Uses self-contained
    * OpenEconEconomics for monetary/external. Uses BankingEconomics (delegates
    * to BankUpdateStep) for banking.
    *
    * Returns FullComputation with both calculus AND step outputs for
    * WorldAssembly.
    */
  private def computeAll(
      w: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      rng: Random,
  )(using p: SimParams): FullComputation =
    val fiscal   = FiscalConstraintEconomics.compute(w)
    val s1       = FiscalConstraintStep.Output(fiscal.month, fiscal.baseMinWage, fiscal.updatedMinWagePriceLevel, fiscal.resWage, fiscal.lendingBaseRate)
    val labor    = LaborEconomics.compute(w, firms, households, s1)
    val s2       = LaborDemographicsStep.Output(
      labor.wage,
      labor.employed,
      labor.laborDemand,
      labor.wageGrowth,
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
    val s3       = HouseholdIncomeStep.run(HouseholdIncomeStep.Input(w, firms, households, s1, s2), rng)
    val s4       = DemandStep.run(DemandStep.Input(w, s2, s3))
    val s5       = FirmProcessingStep.run(FirmProcessingStep.Input(w, firms, households, s1, s2, s3, s4), rng)
    val s6       = HouseholdFinancialStep.run(HouseholdFinancialStep.Input(w, s1, s2, s3))
    val s7       = PriceEquityStep.run(PriceEquityStep.Input(w, s1, s2, s3, s4, s5), rng)
    val openEcon = OpenEconEconomics.compute(
      OpenEconEconomics.Input(
        w = w,
        employed = labor.employed,
        newWage = labor.wage,
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
        month = fiscal.month,
        commodityRng = rng,
      ),
    )
    val s8       = OpenEconomyStep.run(OpenEconomyStep.Input(w, s1, s2, s3, s4, s5, s6, s7, rng))
    val banking  = BankingEconomics.compute(
      BankingEconomics.Input(
        w = w,
        month = fiscal.month,
        lendingBaseRate = fiscal.lendingBaseRate,
        resWage = fiscal.resWage,
        baseMinWage = fiscal.baseMinWage,
        minWagePriceLevel = fiscal.updatedMinWagePriceLevel,
        employed = labor.employed,
        newWage = labor.wage,
        laborDemand = labor.laborDemand,
        wageGrowth = labor.wageGrowth,
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
        depositRng = rng,
      ),
    )
    val s9       = BankUpdateStep.run(BankUpdateStep.Input(w, s1, s2, s3, s4, s5, s6, s7, s8, rng))
    val agg      = s3.hhAgg
    val eq       = w.financial.equity
    val h        = w.real.housing
    val calc     = MonthlyCalculus(
      month = fiscal.month,
      resWage = fiscal.resWage,
      lendingBaseRate = fiscal.lendingBaseRate,
      baseMinWage = fiscal.baseMinWage,
      minWagePriceLevel = fiscal.updatedMinWagePriceLevel,
      wage = labor.wage,
      employed = labor.employed,
      laborDemand = labor.laborDemand,
      retirees = labor.demographics.retirees,
      workingAgePop = labor.demographics.workingAgePop,
      nBankruptFirms = labor.nBankruptFirms,
      avgFirmWorkers = labor.avgFirmWorkers,
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
      rng: Random,
  )(using p: SimParams): MonthlyCalculus =
    computeAll(w, firms, households, rng).calculus

  case class StepResult(
      calculus: MonthlyCalculus,
      flows: Vector[Flow],
      newWorld: World,
      newFirms: Vector[Firm.State],
      newHouseholds: Vector[Household.State],
  )

  /** Full step: compute calculus → emit flows → assemble new World.
    *
    * This is the pipeline entry point. Runs s1–s9 exactly once via
    * computeAll(), then feeds both MonthlyCalculus (for flows) and step outputs
    * (for WorldAssembly) from that single computation.
    */
  def step(w: World, firms: Vector[Firm.State], households: Vector[Household.State], rng: Random)(using
      p: SimParams,
  ): StepResult =
    val full  = computeAll(w, firms, households, rng)
    val flows = emitAllFlows(full.calculus)

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
        rng = rng,
        migRng = rng,
      ),
    )

    StepResult(full.calculus, flows, assembled.world, assembled.firms, assembled.households)
