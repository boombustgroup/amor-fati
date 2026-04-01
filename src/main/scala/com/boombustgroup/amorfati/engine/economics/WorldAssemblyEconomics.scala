package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.accounting.*
import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.agents.RegionalMigration
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.*
import com.boombustgroup.amorfati.engine.markets.{EquityMarket, LaborMarket}
import com.boombustgroup.amorfati.engine.mechanisms.{FirmEntry, SectoralMobility}
import com.boombustgroup.amorfati.types.*

import scala.util.Random

/** WorldAssembly economics: aggregation, informal economy, observables.
  *
  * Own Input takes raw values where possible, Step.Output types where
  * unavoidable. Returns assembled World + updated agents.
  *
  * Full decoupling happens when World is replaced by MutableWorldState (#131).
  */
object WorldAssemblyEconomics:

  // ---------------------------------------------------------------------------
  // StepInput / StepOutput — migrated from WorldAssemblyStep
  // ---------------------------------------------------------------------------

  case class StepInput(
      w: World,                               // current world state
      firms: Vector[Firm.State],              // pre-step firm population
      households: Vector[Household.State],    // pre-step household population
      banks: Vector[Banking.BankState],       // pre-step bank population
      s1: FiscalConstraintEconomics.Output,   // fiscal constraint (month, reservation wage, lending base rate)
      s2: LaborEconomics.Output,              // labor/demographics (wage, employment, ZUS, PPK)
      s3: HouseholdIncomeEconomics.Output,    // household income (consumption, PIT, import propensity)
      s4: DemandEconomics.Output,             // demand (sector multipliers, gov purchases)
      s5: FirmEconomics.StepOutput,           // firm processing (loans, NPL, tax, I-O, bond issuance)
      s6: HouseholdFinancialEconomics.Output, // household financial (debt service, remittances, tourism)
      s7: PriceEquityEconomics.Output,        // price/equity (inflation, GDP, equity, macropru)
      s8: OpenEconEconomics.StepOutput,       // open economy (monetary policy, forex, BOP, corp bonds)
      s9: BankingEconomics.StepOutput,        // bank update (balance sheets, tax revenue, housing flows)
  )

  case class StepOutput(
      newWorld: World,
      finalFirms: Vector[Firm.State],
      reassignedHouseholds: Vector[Household.State],
      banks: Vector[Banking.BankState],
      sfcResult: Sfc.SfcResult,
  )

  // ---------------------------------------------------------------------------
  // Private intermediate types
  // ---------------------------------------------------------------------------

  /** Intermediate result for informal economy computations. */
  private case class InformalResult(
      taxEvasionLoss: PLN,
      informalEmployed: Double,
      cyclicalAdj: Double,
      effectiveShadowShare: Double,
  )

  /** Intermediate result for observable values surfaced on World. */
  private case class Observables(
      depositFacilityUsage: PLN,
      etsPrice: Double,
      tourismSeasonalFactor: Double,
  )

  private case class EntryStepResult(
      firms: Vector[Firm.State],
      firmBirths: Int,
      netBirths: Int,
      entrantIds: Set[FirmId],
  )

  private case class StartupStaffingResult(
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      hhAgg: Household.Aggregates,
      crossSectorHires: Int,
      startupAbsorptionRate: Double,
  )

  // ---------------------------------------------------------------------------
  // Economics-level Input / Result (existing)
  // ---------------------------------------------------------------------------

  case class Input(
      w: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      banks: Vector[Banking.BankState],
      // Raw values
      month: Int,
      lendingBaseRate: Rate,
      resWage: PLN,
      baseMinWage: PLN,
      minWagePriceLevel: Double,
      govPurchases: PLN,
      sectorMults: Vector[Double],
      avgDemandMult: Double,
      sectorCap: Vector[Double],
      laggedInvestDemand: PLN,
      fiscalRuleStatus: com.boombustgroup.amorfati.engine.markets.FiscalRules.RuleStatus,
      // Step outputs (too complex to decompose)
      laborOutput: LaborEconomics.Output,
      hhOutput: HouseholdIncomeEconomics.Output,
      firmOutput: FirmEconomics.StepOutput,
      hhFinancialOutput: HouseholdFinancialEconomics.Output,
      priceEquityOutput: PriceEquityEconomics.Output,
      openEconOutput: OpenEconEconomics.StepOutput,
      bankOutput: BankingEconomics.StepOutput,
      rng: Random,
      migRng: Random,
  )

  case class Result(
      world: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      banks: Vector[Banking.BankState],
  )

  def compute(in: Input)(using SimParams): Result =
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

    val s10 = runStep(
      StepInput(
        in.w,
        in.firms,
        in.households,
        in.banks,
        s1,
        in.laborOutput,
        in.hhOutput,
        s4,
        in.firmOutput,
        in.hhFinancialOutput,
        in.priceEquityOutput,
        in.openEconOutput,
        in.bankOutput,
      ),
      in.rng,
      in.migRng,
    )

    Result(s10.newWorld, s10.finalFirms, s10.reassignedHouseholds, s10.banks)

  // ---------------------------------------------------------------------------
  // runStep — migrated from WorldAssemblyStep.run
  // ---------------------------------------------------------------------------

  def runStep(in: StepInput, rng: Random, migRng: Random)(using p: SimParams): StepOutput =
    val equityAfterStep = finalizeEquity(in)
    val fofResidual     = computeFofResidual(in)
    val informal        = computeInformalEconomy(in)
    val obs             = computeObservables(in)

    val newW      = assembleWorld(in, equityAfterStep, fofResidual, informal, obs)
    val sfcResult = validateSfc(in, newW, fofResidual)

    val postFdiFirms     = applyFdiMa(in.s9.reassignedFirms, rng)
    val updatedPop       = in.w.totalPopulation + in.s5.netMigration
    val postFirmEmployed = in.s9.reassignedHouseholds.count: hh =>
      hh.status match
        case HhStatus.Employed(_, _, _) => true
        case _                          => false
    val unemploymentRate = if updatedPop > 0 then 1.0 - postFirmEmployed.toDouble / updatedPop else 0.0
    val entryStep        =
      if p.flags.firmEntry then
        val r = FirmEntry.process(
          postFdiFirms,
          newW.real.automationRatio,
          newW.real.hybridRatio,
          unemploymentRate,
          in.s2.aggregateHiringSlack,
          newW.inflation,
          newW.mechanisms.expectations.expectedInflation,
          in.w.pipeline.startupAbsorptionRate,
          rng,
        )
        EntryStepResult(r.firms, r.births, r.netBirths, r.entrantIds)
      else EntryStepResult(postFdiFirms, 0, 0, Set.empty)

    val startupStaffing = applyStartupStaffing(in, entryStep.firms, in.s9.reassignedHouseholds, rng)

    // Regional migration: unemployed HH may relocate between NUTS-1 regions
    val postMigHh  =
      if p.flags.regionalLabor then RegionalMigration(startupStaffing.households, in.s2.regionalWages, migRng).households
      else startupStaffing.households
    val finalFirms = syncStartupStaffing(startupStaffing.firms, postMigHh)

    val finalW = newW
      .updateFlows(_.copy(firmBirths = entryStep.firmBirths, firmDeaths = in.s5.firmDeaths, netFirmBirths = entryStep.netBirths))
      .updatePipeline(_.copy(startupAbsorptionRate = startupStaffing.startupAbsorptionRate))
      .updateReal: r =>
        r.copy(
          sectoralMobility = r.sectoralMobility.copy(
            crossSectorHires = r.sectoralMobility.crossSectorHires + startupStaffing.crossSectorHires,
          ),
        )
      .copy(hhAgg = startupStaffing.hhAgg)
      .copy(regionalWages = in.s2.regionalWages)
    StepOutput(finalW, finalFirms, postMigHh, in.s9.banks, sfcResult)

  // ---------------------------------------------------------------------------
  // Private helpers — migrated from WorldAssemblyStep
  // ---------------------------------------------------------------------------

  /** Finalize GPW equity state with aggregate household equity wealth. */
  private def finalizeEquity(in: StepInput): EquityMarket.State =
    val totalHhEquityWealth = PLN.fromRaw(in.s9.reassignedHouseholds.map(_.equityWealth.toLong).sum)
    in.s7.equityAfterIssuance.copy(
      hhEquityWealth = totalHhEquityWealth,
      lastWealthEffect = PLN.Zero,
      lastDomesticDividends = in.s7.netDomesticDividends,
      lastForeignDividends = in.s7.foreignDividendOutflow,
      lastDividendTax = in.s7.dividendTax,
    )

  /** Flow-of-funds residual: total firm revenue minus adjusted demand. Both
    * sides computed via the same PLN path to avoid Long/Double rounding
    * mismatch.
    */
  private def computeFofResidual(in: StepInput)(using p: SimParams): PLN =
    val nSectors       = p.sectorDefs.length
    val sectorCapPln   = (0 until nSectors).map: s =>
      PLN.fromRaw(in.s2.living.filter(_.sector.toInt == s).map(f => Firm.computeCapacity(f).toLong).sum)
    val totalFirmRev   = PLN.fromRaw(
      (0 until nSectors)
        .map: s =>
          (sectorCapPln(s) * Multiplier(in.s4.sectorMults(s) * in.w.priceLevel)).toLong
        .sum,
    )
    val adjustedDemand = PLN.fromRaw(
      (0 until nSectors)
        .map: s =>
          (PLN(in.s4.sectorCap(s)) * Multiplier(in.s4.sectorMults(s) * in.w.priceLevel)).toLong
        .sum,
    )
    totalFirmRev - adjustedDemand

  /** Informal economy: four-channel tax evasion (CIT, VAT, PIT, excise),
    * estimated informal employment, and smoothed cyclical adjustment for the
    * counter-cyclical shadow economy share.
    */
  @boundaryEscape
  private def computeInformalEconomy(in: StepInput)(using p: SimParams): InformalResult =
    import ComputationBoundary.toDouble
    if !p.flags.informal then return InformalResult(PLN.Zero, 0.0, 0.0, 0.0)

    val taxEvasionLoss =
      in.s5.sumCitEvasion + (in.s9.vat - in.s9.vatAfterEvasion) +
        (in.s3.pitRevenue - in.s9.pitAfterEvasion) +
        (in.s9.exciseRevenue - in.s9.exciseAfterEvasion)

    val informalEmployed = in.s2.employed.toDouble * toDouble(in.s9.effectiveShadowShare)

    val unemp       = 1.0 - in.s2.employed.toDouble / in.w.totalPopulation
    val target      = Math.max(0.0, unemp - toDouble(p.informal.unempThreshold)) * toDouble(p.informal.cyclicalSens)
    val cyclicalAdj = in.w.mechanisms.informalCyclicalAdj * toDouble(p.informal.smoothing) +
      target * (1.0 - toDouble(p.informal.smoothing))

    val effectiveShadowShare =
      p.fiscal.fofConsWeights
        .map(toDouble(_))
        .zip(p.informal.sectorShares.map(toDouble(_)))
        .map((cw, ss) => cw * Math.min(1.0, ss + cyclicalAdj))
        .sum: Double

    InformalResult(taxEvasionLoss, informalEmployed, cyclicalAdj, effectiveShadowShare)

  /** Pre-compute observable values surfaced on World for SimOutput. */
  @boundaryEscape
  private def computeObservables(in: StepInput)(using p: SimParams): Observables =
    import ComputationBoundary.toDouble
    val aliveBanks           = in.s9.banks.filterNot(_.failed)
    val depositFacilityUsage = PLN.fromRaw(
      aliveBanks
        .filter(_.reservesAtNbp > PLN.Zero)
        .map(_.reservesAtNbp.toLong)
        .sum,
    )

    val monthsPerYear = 12.0
    val etsPrice      =
      if p.flags.energy then p.climate.etsBasePrice * Math.pow(1.0 + toDouble(p.climate.etsPriceDrift) / monthsPerYear, in.s1.m.toDouble)
      else 0.0

    val monthInYear           = ((in.s1.m - 1) % 12) + 1
    val tourismSeasonalFactor =
      1.0 + toDouble(p.tourism.seasonality) * Math.cos(2 * Math.PI * (monthInYear - p.tourism.peakMonth) / 12.0)

    Observables(depositFacilityUsage, etsPrice, tourismSeasonalFactor)

  private def applyStartupStaffing(
      in: StepInput,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      rng: Random,
  )(using p: SimParams): StartupStaffingResult =
    val startupIds = firms.filter(f => Firm.isAlive(f) && Firm.isInStartup(f)).map(_.id).toSet
    if startupIds.isEmpty then StartupStaffingResult(syncStartupStaffing(firms, households), households, in.s9.finalHhAgg, 0, 1.0)
    else
      val startupOpeningsBefore = firms
        .filter(f => Firm.isAlive(f) && Firm.isInStartup(f))
        .map(f => Math.max(0, f.startupTargetWorkers - f.startupFilledWorkers))
        .sum
      val startupFilledBefore   = firms
        .filter(f => Firm.isAlive(f) && Firm.isInStartup(f))
        .map(_.startupFilledWorkers)
        .sum
      val searchResult          = LaborMarket.jobSearch(households, firms, in.s2.newWage, rng, in.s2.regionalWages, startupIds)
      val postWages             = LaborMarket.updateWages(searchResult.households, firms, in.s2.newWage)
      val staffedFirms          = syncStartupStaffing(firms, postWages)
      val startupFilled         = staffedFirms.filter(f => Firm.isAlive(f) && Firm.isInStartup(f)).map(_.startupFilledWorkers).sum
      val startupHires          = Math.max(0, startupFilled - startupFilledBefore)
      val startupAbsorptionRate =
        if startupOpeningsBefore > 0 then startupHires.toDouble / startupOpeningsBefore
        else 1.0
      val hhAgg                 = Household.computeAggregates(
        postWages,
        in.s2.newWage,
        in.s1.resWage,
        in.s3.importAdj,
        in.s3.hhAgg.retrainingAttempts,
        in.s3.hhAgg.retrainingSuccesses,
      )
      StartupStaffingResult(staffedFirms, postWages, hhAgg, searchResult.crossSectorHires, startupAbsorptionRate)

  private def syncStartupStaffing(
      firms: Vector[Firm.State],
      households: Vector[Household.State],
  ): Vector[Firm.State] =
    val staffedCounts = households
      .flatMap: hh =>
        hh.status match
          case HhStatus.Employed(fid, _, _) => Some(fid)
          case _                            => None
      .groupMapReduce(identity)(_ => 1)(_ + _)
    firms.map: firm =>
      if Firm.isInStartup(firm) then
        val filled     = staffedCounts.getOrElse(firm.id, 0).min(firm.startupTargetWorkers)
        val syncedTech = firm.tech match
          case TechState.Traditional(_) => TechState.Traditional(Math.max(1, filled))
          case TechState.Hybrid(_, eff) => TechState.Hybrid(Math.max(1, filled), eff)
          case other                    => other
        firm.copy(startupFilledWorkers = filled, tech = syncedTech)
      else firm

  /** Construct the new World state from all step outputs. */
  @boundaryEscape
  private def assembleWorld(
      in: StepInput,
      equityAfterStep: EquityMarket.State,
      fofResidual: PLN,
      informal: InformalResult,
      obs: Observables,
  ): World =
    import ComputationBoundary.toDouble
    World(
      month = in.s1.m,
      inflation = in.s7.newInfl,
      priceLevel = in.s7.newPrice,
      gdpProxy = toDouble(in.s7.gdp),
      currentSigmas = in.s7.newSigmas,
      totalPopulation = in.w.totalPopulation + in.s5.netMigration,
      gov = in.s9.newGovWithYield.copy(
        minWageLevel = in.s1.baseMinWage,
        minWagePriceLevel = in.s1.updatedMinWagePriceLevel,
      ),
      nbp = in.s9.finalNbp,
      bankingSector = in.s9.bankingMarket,
      forex = in.s8.external.newForex,
      bop = in.s8.external.newBop,
      hhAgg = in.s9.finalHhAgg,
      social = SocialState(
        jst = in.s9.newJst,
        zus = in.s2.newZus,
        nfz = in.s2.newNfz,
        ppk = in.s9.finalPpk,
        demographics = in.s2.newDemographics,
        earmarked = in.s2.newEarmarked,
      ),
      financial = FinancialMarketsState(
        equity = equityAfterStep,
        corporateBonds = in.s8.corpBonds.newCorpBonds,
        insurance = in.s9.finalInsurance,
        nbfi = in.s9.finalNbfi,
        quasiFiscal = in.s9.newQuasiFiscal,
      ),
      external = ExternalState(
        gvc = in.s8.external.newGvc,
        immigration = in.s2.newImmig,
        tourismSeasonalFactor = obs.tourismSeasonalFactor,
      ),
      real = RealState(
        housing = in.s9.housingAfterFlows,
        sectoralMobility = SectoralMobility.State(
          crossSectorHires = in.s5.postFirmCrossSectorHires + in.s3.hhAgg.crossSectorHires,
          voluntaryQuits = in.s3.hhAgg.voluntaryQuits,
          sectorMobilityRate = in.s9.finalHhAgg.sectorMobilityRate,
        ),
        grossInvestment = in.s5.sumGrossInvestment,
        aggGreenInvestment = in.s5.sumGreenInvestment,
        aggGreenCapital = in.s7.aggGreenCapital,
        etsPrice = obs.etsPrice,
        automationRatio = in.s7.autoR,
        hybridRatio = in.s7.hybR,
      ),
      mechanisms = MechanismsState(
        macropru = in.s7.newMacropru,
        expectations = in.s8.monetary.newExp,
        bfgFundBalance = in.w.mechanisms.bfgFundBalance + in.s9.bfgLevy,
        informalCyclicalAdj = informal.cyclicalAdj,
        effectiveShadowShare = informal.effectiveShadowShare,
      ),
      plumbing = MonetaryPlumbingState(
        reserveInterestTotal = in.s8.banking.totalReserveInterest,
        standingFacilityNet = in.s8.banking.totalStandingFacilityIncome,
        interbankInterestNet = in.s8.banking.totalInterbankInterest,
        depositFacilityUsage = obs.depositFacilityUsage,
        fofResidual = fofResidual,
      ),
      pipeline = buildPipelineState(in),
      flows = buildFlowState(in, informal),
    )

  private def buildPipelineState(in: StepInput): PipelineState =
    PipelineState(
      sectorDemandMult = in.s4.sectorMults,
      sectorDemandPressure = in.s4.sectorDemandPressure,
      sectorHiringSignal = in.s4.sectorHiringSignal,
      fiscalRuleSeverity = in.s4.fiscalRuleStatus.bindingRule,
      govSpendingCutRatio = in.s4.fiscalRuleStatus.spendingCutRatio,
      aggregateHiringSlack = in.s2.aggregateHiringSlack,
    )

  /** Construct the FlowState for this step. */
  @boundaryEscape
  private def buildFlowState(in: StepInput, informal: InformalResult): FlowState =
    import ComputationBoundary.toDouble
    FlowState(
      ioFlows = in.s5.totalIoPaid,
      fdiProfitShifting = in.s5.sumProfitShifting,
      fdiRepatriation = in.s5.sumFdiRepatriation,
      fdiCitLoss = in.s8.external.fdiCitLoss,
      diasporaRemittanceInflow = in.s6.diasporaInflow,
      tourismExport = in.s6.tourismExport,
      tourismImport = in.s6.tourismImport,
      aggInventoryStock = in.s7.aggInventoryStock,
      aggInventoryChange = in.s7.aggInventoryChange,
      aggEnergyCost = in.s5.sumEnergyCost,
      firmBirths = 0,
      firmDeaths = 0,
      taxEvasionLoss = informal.taxEvasionLoss,
      informalEmployed = informal.informalEmployed,
      bailInLoss = in.s9.bailInLoss,
      bfgLevyTotal = toDouble(in.s9.bfgLevy),
    )

  /** Run SFC validation against previous and current snapshots. */
  private def validateSfc(in: StepInput, newW: World, fofResidual: PLN)(using p: SimParams): Sfc.SfcResult =
    val prevSnap = Sfc.snapshot(in.w, in.firms, in.households, in.banks)
    val currSnap = Sfc.snapshot(newW, in.s9.reassignedFirms, in.s9.reassignedHouseholds, in.s9.banks)
    val flows    = buildMonthlyFlows(in, fofResidual)
    Sfc.validate(prevSnap, currSnap, flows)

  /** Construct Sfc.MonthlyFlows from all step outputs. */
  @boundaryEscape
  private def buildMonthlyFlows(in: StepInput, fofResidual: PLN)(using p: SimParams): Sfc.MonthlyFlows =
    Sfc.MonthlyFlows(
      govSpending =
        in.s9.newGovWithYield.domesticBudgetOutlays + in.s2.newZus.govSubvention + in.s2.newNfz.govSubvention + in.s2.newEarmarked.totalGovSubvention,
      govRevenue =
        in.s5.sumTax + in.s7.dividendTax + in.s9.pitAfterEvasion + in.s9.vatAfterEvasion + in.s8.banking.nbpRemittance + in.s9.exciseAfterEvasion + in.s9.customsDutyRevenue,
      nplLoss = in.s5.nplLoss,
      interestIncome = in.s5.intIncome,
      hhDebtService = in.s6.hhDebtService,
      totalIncome = in.s3.totalIncome,
      totalConsumption = in.s3.consumption,
      newLoans = in.s5.sumNewLoans,
      nplRecovery = in.s5.nplNew * p.banking.loanRecovery,
      currentAccount = in.s8.external.newBop.currentAccount,
      valuationEffect = in.s8.external.oeValuationEffect,
      bankBondIncome = in.s8.banking.bankBondIncome,
      qePurchase = in.s8.monetary.qePurchaseAmount,
      newBondIssuance = if p.flags.govBondMarket then in.s9.actualBondChange else PLN.Zero,
      depositInterestPaid = in.s6.depositInterestPaid,
      reserveInterest = in.s8.banking.totalReserveInterest,
      standingFacilityIncome = in.s8.banking.totalStandingFacilityIncome,
      interbankInterest = in.s8.banking.totalInterbankInterest,
      jstDepositChange = in.s9.jstDepositChange,
      jstSpending = in.s9.newJst.spending,
      jstRevenue = in.s9.newJst.revenue,
      zusContributions = in.s2.newZus.contributions,
      zusPensionPayments = in.s2.newZus.pensionPayments,
      zusGovSubvention = in.s2.newZus.govSubvention,
      nfzContributions = in.s2.newNfz.contributions,
      nfzSpending = in.s2.newNfz.spending,
      nfzGovSubvention = in.s2.newNfz.govSubvention,
      dividendIncome = in.s7.netDomesticDividends,
      foreignDividendOutflow = in.s7.foreignDividendOutflow,
      dividendTax = in.s7.dividendTax,
      mortgageInterestIncome = in.s9.mortgageInterestIncome,
      mortgageNplLoss = in.s9.mortgageDefaultLoss,
      mortgageOrigination = in.s9.housingAfterFlows.lastOrigination,
      mortgagePrincipalRepaid = in.s9.mortgagePrincipal,
      mortgageDefaultAmount = in.s9.mortgageDefaultAmount,
      remittanceOutflow = in.s6.remittanceOutflow,
      fofResidual = fofResidual,
      consumerDebtService = in.s6.consumerDebtService,
      consumerNplLoss = in.s6.consumerNplLoss,
      consumerOrigination = in.s6.consumerOrigination,
      consumerPrincipalRepaid = in.s6.consumerPrincipal,
      consumerDefaultAmount = in.s6.consumerDefaultAmt,
      corpBondCouponIncome = in.s8.corpBonds.corpBondBankCoupon,
      corpBondDefaultLoss = in.s8.corpBonds.corpBondBankDefaultLoss,
      corpBondIssuance = in.s5.actualBondIssuance,
      corpBondAmortization = in.s8.corpBonds.corpBondAmort,
      corpBondDefaultAmount = in.s5.totalBondDefault,
      insNetDepositChange = in.s8.nonBank.insNetDepositChange,
      nbfiDepositDrain = in.s8.nonBank.nbfiDepositDrain,
      nbfiOrigination = in.s9.finalNbfi.lastNbfiOrigination,
      nbfiRepayment = in.s9.finalNbfi.lastNbfiRepayment,
      nbfiDefaultAmount = in.s9.finalNbfi.lastNbfiDefaultAmount,
      fdiProfitShifting = in.s5.sumProfitShifting,
      fdiRepatriation = in.s5.sumFdiRepatriation,
      diasporaInflow = in.s6.diasporaInflow,
      tourismExport = in.s6.tourismExport,
      tourismImport = in.s6.tourismImport,
      bfgLevy = in.s9.bfgLevy,
      bailInLoss = in.s9.bailInLoss,
      bankCapitalDestruction = in.s9.multiCapDestruction,
      investNetDepositFlow = in.s9.investNetDepositFlow,
      firmPrincipalRepaid = in.s5.sumFirmPrincipal,
      unrealizedBondLoss = in.s9.unrealizedBondLoss,
      htmRealizedLoss = in.s9.htmRealizedLoss,
      eclProvisionChange = in.s9.eclProvisionChange,
    )

  /** FDI M&A: monthly stochastic conversion of domestic firms to foreign
    * ownership, representing cross-border mergers and acquisitions.
    */
  private def applyFdiMa(firms: Vector[Firm.State], rng: Random)(using p: SimParams): Vector[Firm.State] =
    if p.flags.fdi && p.fdi.maProb > Share.Zero then
      firms.map: f =>
        if Firm.isAlive(f) && !f.foreignOwned &&
          f.initialSize >= p.fdi.maSizeMin &&
          p.fdi.maProb.sampleBelow(rng)
        then f.copy(foreignOwned = true)
        else f
    else firms
