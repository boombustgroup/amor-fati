package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.agents.RegionalMigration
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.*
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.ledger.{LedgerBoundaryProjection, LedgerFinancialState}
import com.boombustgroup.amorfati.engine.markets.{EquityMarket, LaborMarket}
import com.boombustgroup.amorfati.engine.mechanisms.{FirmEntry, InformalEconomy, SectoralMobility}
import com.boombustgroup.amorfati.types.*

import com.boombustgroup.amorfati.random.RandomStream

/** WorldAssembly economics: aggregation, informal economy, observables.
  *
  * Own Input takes raw values where possible, Step.Output types where
  * unavoidable. Returns assembled World + updated agents.
  */
object WorldAssemblyEconomics:

  // ---------------------------------------------------------------------------
  // StepInput / StepOutput — migrated from WorldAssemblyStep
  // ---------------------------------------------------------------------------

  case class StepInput(
      w: World,                                   // current world state
      firms: Vector[Firm.State],                  // pre-step firm population
      households: Vector[Household.State],        // pre-step household population
      banks: Vector[Banking.BankState],           // pre-step bank population
      ledgerFinancialState: LedgerFinancialState, // ledger-backed financial source of truth
      s1: FiscalConstraintEconomics.Output,       // fiscal constraint (month, reservation wage, lending base rate)
      s2: LaborEconomics.Output,                  // labor/demographics (wage, employment, ZUS, PPK)
      s3: HouseholdIncomeEconomics.Output,        // household income (consumption, PIT, import propensity)
      s4: DemandEconomics.Output,                 // demand (sector multipliers, gov purchases)
      s5: FirmEconomics.StepOutput,               // firm processing (loans, NPL, tax, I-O, bond issuance)
      s6: HouseholdFinancialEconomics.Output,     // household financial flows (debt service, remittances, tourism)
      s7: PriceEquityEconomics.Output,            // price/equity (inflation, GDP, equity, macropru)
      s8: OpenEconEconomics.StepOutput,           // open economy (monetary policy, forex, BOP, corp bonds)
      s9: BankingEconomics.StepOutput,            // bank update (balance sheets, tax revenue, housing flows)
  )

  case class StepOutput(
      newWorld: World,
      finalFirms: Vector[Firm.State],
      reassignedHouseholds: Vector[Household.State],
      banks: Vector[Banking.BankState],
      householdAggregates: Household.Aggregates,
      signalExtraction: SignalExtraction.Output,
  )

  // ---------------------------------------------------------------------------
  // Private intermediate types
  // ---------------------------------------------------------------------------

  /** Intermediate result for informal economy computations. */
  private case class InformalResult(
      taxEvasionLoss: PLN,
      realizedTaxShadowShare: Double,
      cyclicalAdj: Double,
      nextTaxShadowShare: Double,
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
      startupAbsorptionRate: Share,
  )

  // ---------------------------------------------------------------------------
  // Economics-level Input / Result (existing)
  // ---------------------------------------------------------------------------

  case class Input(
      w: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      banks: Vector[Banking.BankState],
      ledgerFinancialState: LedgerFinancialState,
      // Raw values
      month: ExecutionMonth,
      lendingBaseRate: Rate,
      resWage: PLN,
      baseMinWage: PLN,
      minWagePriceLevel: PriceIndex,
      govPurchases: PLN,
      sectorMults: Vector[Multiplier],
      sectorDemandPressure: Vector[Multiplier],
      sectorHiringSignal: Vector[Multiplier],
      avgDemandMult: Multiplier,
      sectorCapReal: Vector[PLN],
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
      randomness: MonthRandomness.AssemblyStreams,
  )

  case class Result(
      world: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      banks: Vector[Banking.BankState],
      householdAggregates: Household.Aggregates,
      ledgerFinancialState: LedgerFinancialState,
      signalExtraction: SignalExtraction.Output,
  )

  /** Internal post-month result kept distinct from the next-month boundary. */
  private[engine] case class PostResult(
      world: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      banks: Vector[Banking.BankState],
      householdAggregates: Household.Aggregates,
      ledgerFinancialState: LedgerFinancialState,
      startupAbsorptionRate: Share,
  )

  private case class SignalExtractionInput(
      operationalHiringSlack: Share,
      sectorDemandMult: Vector[Multiplier],
      sectorDemandPressure: Vector[Multiplier],
      sectorHiringSignal: Vector[Multiplier],
  )

  private def buildStepInput(in: Input): StepInput =
    val s1 = FiscalConstraintEconomics.Output(in.month, in.baseMinWage, in.minWagePriceLevel, in.resWage, in.lendingBaseRate)

    StepInput(
      in.w,
      in.firms,
      in.households,
      in.banks,
      in.ledgerFinancialState,
      s1,
      in.laborOutput,
      in.hhOutput,
      buildDemandOutput(in),
      in.firmOutput,
      in.hhFinancialOutput,
      in.priceEquityOutput,
      in.openEconOutput,
      in.bankOutput,
    )

  private def buildSignalExtractionInput(in: StepInput): SignalExtractionInput =
    SignalExtractionInput(
      operationalHiringSlack = in.s2.operationalHiringSlack,
      sectorDemandMult = in.s4.sectorMults,
      sectorDemandPressure = in.s4.sectorDemandPressure,
      sectorHiringSignal = in.s4.sectorHiringSignal,
    )

  private def extractSignalExtraction(
      signalInput: SignalExtractionInput,
      post: PostResult,
  ): SignalExtraction.Output =
    val employedHouseholds = Household.countEmployed(post.households)

    SignalExtraction.compute(
      SignalExtraction.inputFromRealizedOutcomes(
        unemploymentRate = post.world.unemploymentRate(employedHouseholds),
        laggedHiringSlack = signalInput.operationalHiringSlack,
        startupAbsorptionRate = post.startupAbsorptionRate,
        inflation = post.world.inflation,
        expectedInflation = post.world.mechanisms.expectations.expectedInflation,
        sectorDemandMult = signalInput.sectorDemandMult,
        sectorDemandPressure = signalInput.sectorDemandPressure,
        sectorHiringSignal = signalInput.sectorHiringSignal,
      ),
    )

  private def advanceToBoundaryWorld(
      postWorld: World,
      signals: DecisionSignals,
  ): World =
    postWorld.updatePipeline(_.withDecisionSignals(signals))

  private[engine] def computePostMonth(
      step: StepInput,
      randomness: MonthRandomness.AssemblyStreams,
  )(using SimParams): PostResult =
    runPostStep(
      step,
      randomness,
    )

  private[engine] def computePostMonth(in: Input)(using SimParams): PostResult =
    computePostMonth(buildStepInput(in), in.randomness)

  def compute(in: Input)(using SimParams): Result =
    val step        = buildStepInput(in)
    val signalInput = buildSignalExtractionInput(step)
    val post        = computePostMonth(step, in.randomness)
    val seed        = extractSignalExtraction(signalInput, post)
    Result(
      advanceToBoundaryWorld(post.world, seed.seedOut),
      post.firms,
      post.households,
      post.banks,
      post.householdAggregates,
      post.ledgerFinancialState,
      seed,
    )

  // ---------------------------------------------------------------------------
  // runStep — migrated from WorldAssemblyStep.run
  // ---------------------------------------------------------------------------

  def runStep(in: StepInput, randomness: MonthRandomness.AssemblyStreams)(using p: SimParams): StepOutput =
    val signalInput      = buildSignalExtractionInput(in)
    val post             = runPostStep(in, randomness)
    val signalExtraction = extractSignalExtraction(signalInput, post)
    StepOutput(
      newWorld = advanceToBoundaryWorld(post.world, signalExtraction.seedOut),
      finalFirms = post.firms,
      reassignedHouseholds = post.households,
      banks = post.banks,
      householdAggregates = post.householdAggregates,
      signalExtraction = signalExtraction,
    )

  private[engine] def runPostStep(in: StepInput, randomness: MonthRandomness.AssemblyStreams)(using p: SimParams): PostResult =
    val equityAfterStep = finalizeEquity(in)
    val fofResidual     = computeFofResidual(in)
    val informal        = computeInformalEconomy(in)
    val obs             = computeObservables(in)
    val seedIn          = in.w.seedIn

    val (newW, assembledLedgerFinancialState) = assembleWorld(in, equityAfterStep, fofResidual, informal, obs)

    val postFdiFirms = applyFdiMa(in.s9.reassignedFirms, randomness.fdiMa)
    val entryStep    =
      val r = FirmEntry.process(
        postFdiFirms,
        newW.real.automationRatio,
        newW.real.hybridRatio,
        FirmEntry.LaggedEntrySignals.fromDecisionSignals(seedIn),
        randomness.firmEntry,
      )
      EntryStepResult(r.firms, r.births, r.netBirths, r.entrantIds)

    val startupStaffing = applyStartupStaffing(in, entryStep.firms, in.s9.reassignedHouseholds, randomness.startupStaffing)

    // Regional migration: unemployed HH may relocate between NUTS-1 regions
    val postMigHh                 = RegionalMigration(startupStaffing.households, in.s2.regionalWages, randomness.regionalMigration).households
    val finalFirms                = syncStartupStaffing(startupStaffing.firms, postMigHh)
    val finalW                    = newW
      .updatePipeline(_ => buildPostMonthPipelineState(in))
      .updateFlows(_.copy(firmBirths = entryStep.firmBirths, firmDeaths = in.s5.firmDeaths, netFirmBirths = entryStep.netBirths))
      .updateReal: r =>
        r.copy(
          sectoralMobility = r.sectoralMobility.copy(
            crossSectorHires = r.sectoralMobility.crossSectorHires + startupStaffing.crossSectorHires,
          ),
        )
      .copy(regionalWages = in.s2.regionalWages)
    val finalLedgerFinancialState = assembledLedgerFinancialState.copy(
      households = postMigHh.map(LedgerFinancialState.householdBalances),
      firms = finalFirms.map(LedgerFinancialState.firmBalances),
      banks = in.s9.banks.map(LedgerFinancialState.bankBalances),
    )
    PostResult(
      finalW,
      finalFirms,
      postMigHh,
      in.s9.banks,
      startupStaffing.hhAgg,
      finalLedgerFinancialState,
      startupStaffing.startupAbsorptionRate,
    )

  // ---------------------------------------------------------------------------
  // Private helpers — migrated from WorldAssemblyStep
  // ---------------------------------------------------------------------------

  /** Finalize GPW market-memory fields for this month. */
  private def finalizeEquity(in: StepInput): EquityMarket.State =
    in.s7.equityAfterIssuance.copy(
      lastWealthEffect = PLN.Zero,
      lastDomesticDividends = in.s7.netDomesticDividends,
      lastForeignDividends = in.s7.foreignDividendOutflow,
      lastDividendTax = in.s7.dividendTax,
    )

  private def buildDemandOutput(in: Input): DemandEconomics.Output =
    DemandEconomics.Output(
      in.govPurchases,
      in.sectorMults,
      in.sectorDemandPressure,
      in.sectorHiringSignal,
      in.avgDemandMult,
      in.sectorCapReal,
      in.laggedInvestDemand,
      in.fiscalRuleStatus,
    )

  /** Flow-of-funds residual: total firm revenue minus adjusted demand. Both
    * sides computed via the same PLN path to avoid Long/Double rounding
    * mismatch.
    */
  private def computeFofResidual(in: StepInput)(using p: SimParams): PLN =
    val nSectors       = p.sectorDefs.length
    val sectorCapPln   = (0 until nSectors).map: s =>
      in.s2.living
        .filter(_.sector.toInt == s)
        .foldLeft(PLN.Zero): (acc, f) =>
          acc + Firm.computeCapacity(f)
    val priceMult      = in.w.priceLevel.toMultiplier
    val totalFirmRev   = (0 until nSectors).foldLeft(PLN.Zero): (acc, s) =>
      acc + (sectorCapPln(s) * in.s4.sectorMults(s) * priceMult)
    val adjustedDemand = (0 until nSectors).foldLeft(PLN.Zero): (acc, s) =>
      acc + (in.s4.sectorCapReal(s) * in.s4.sectorMults(s) * priceMult)
    totalFirmRev - adjustedDemand

  /** Informal economy: four-channel tax evasion (CIT, VAT, PIT, excise),
    * estimated informal employment, and smoothed cyclical adjustment for the
    * counter-cyclical shadow economy share.
    */
  @boundaryEscape
  private def computeInformalEconomy(in: StepInput)(using p: SimParams): InformalResult =
    import ComputationBoundary.toDouble

    val taxEvasionLoss =
      in.s5.sumCitEvasion + (in.s9.vat - in.s9.vatAfterEvasion) +
        (in.s3.pitRevenue - in.s9.pitAfterEvasion) +
        (in.s9.exciseRevenue - in.s9.exciseAfterEvasion)

    val realizedTaxShadowShare = toDouble(in.s9.realizedTaxShadowShare)

    val laborPopulation = in.w.social.demographics.workingAgePop.max(1)
    val unemp           = 1.0 - in.s2.employed.toDouble / laborPopulation
    val target          = Math.max(0.0, unemp - toDouble(p.informal.unempThreshold)) * toDouble(p.informal.cyclicalSens)
    val cyclicalAdj     = in.w.mechanisms.informalCyclicalAdj * toDouble(p.informal.smoothing) +
      target * (1.0 - toDouble(p.informal.smoothing))

    val nextTaxShadowShare = toDouble(InformalEconomy.aggregateTaxShadowShare(Share(cyclicalAdj)))

    InformalResult(taxEvasionLoss, realizedTaxShadowShare, cyclicalAdj, nextTaxShadowShare)

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
    val elapsedMonths = in.s1.m.previousCompleted.toInt.toDouble
    val etsPrice      = toDouble(p.climate.etsBasePrice) * Math.pow(1.0 + toDouble(p.climate.etsPriceDrift) / monthsPerYear, elapsedMonths)

    val monthInYear           = in.s1.m.monthInYear
    val tourismSeasonalFactor = 1.0 + toDouble(p.tourism.seasonality) * Math.cos(2 * Math.PI * (monthInYear - p.tourism.peakMonth) / 12.0)

    Observables(depositFacilityUsage, etsPrice, tourismSeasonalFactor)

  private def applyStartupStaffing(
      in: StepInput,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      rng: RandomStream,
  )(using p: SimParams): StartupStaffingResult =
    val startupIds = firms.filter(f => Firm.isAlive(f) && Firm.isInStartup(f)).map(_.id).toSet
    if startupIds.isEmpty then StartupStaffingResult(syncStartupStaffing(firms, households), households, in.s9.finalHhAgg, 0, Share.One)
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
        if startupOpeningsBefore > 0 then Share.fraction(startupHires, startupOpeningsBefore)
        else Share.One
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
  ): (World, LedgerFinancialState) =
    val ledgerFinancialState = buildLedgerFinancialState(in)
    val projectedSocial      = LedgerBoundaryProjection.socialState(
      SocialState(
        jst = in.s9.newJst,
        zus = in.s2.newZus,
        nfz = in.s2.newNfz,
        ppk = in.s9.finalPpk,
        demographics = in.s2.newDemographics,
        earmarked = in.s2.newEarmarked,
      ),
      ledgerFinancialState,
    )
    val world                = World(
      inflation = in.s7.newInfl,
      priceLevel = in.s7.newPrice,
      currentSigmas = in.s7.newSigmas,
      gov = LedgerBoundaryProjection.govState(
        in.s9.newGovWithYield.copy(
          policy = in.s9.newGovWithYield.policy.copy(
            minWageLevel = in.s1.baseMinWage,
            minWagePriceLevel = in.s1.updatedMinWagePriceLevel,
          ),
        ),
        ledgerFinancialState,
      ),
      nbp = LedgerBoundaryProjection.nbpState(in.s9.finalNbp, ledgerFinancialState),
      bankingSector = in.s9.bankingMarket,
      forex = in.s8.external.newForex,
      bop = in.s8.external.newBop,
      householdMarket = HouseholdMarketState.fromAggregates(in.s9.finalHhAgg),
      social = projectedSocial,
      financialMarkets = FinancialMarketsState(
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
        nextTaxShadowShare = informal.nextTaxShadowShare,
      ),
      plumbing = MonetaryPlumbingState(
        reserveInterestTotal = in.s8.banking.totalReserveInterest,
        standingFacilityNet = in.s8.banking.totalStandingFacilityIncome,
        interbankInterestNet = in.s8.banking.totalInterbankInterest,
        depositFacilityUsage = obs.depositFacilityUsage,
        fofResidual = fofResidual,
      ),
      pipeline = in.w.pipeline,
      flows = buildFlowState(in, informal),
    )
    (world, ledgerFinancialState)

  private def buildLedgerFinancialState(in: StepInput): LedgerFinancialState =
    in.s9.ledgerFinancialState

  private def buildPostMonthPipelineState(in: StepInput): PipelineState =
    in.w.pipeline
      .copy(
        operationalHiringSlack = in.s2.operationalHiringSlack,
        fiscalRuleSeverity = in.s4.fiscalRuleStatus.bindingRule,
        govSpendingCutRatio = in.s4.fiscalRuleStatus.spendingCutRatio,
      )

  /** Construct the FlowState for this step. */
  @boundaryEscape
  private def buildFlowState(in: StepInput, informal: InformalResult): FlowState =
    import ComputationBoundary.toDouble
    FlowState(
      monthlyGdpProxy = in.s7.gdp,
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
      realizedTaxShadowShare = informal.realizedTaxShadowShare,
      bailInLoss = in.s9.bailInLoss,
      bfgLevyTotal = toDouble(in.s9.bfgLevy),
    )

  /** FDI M&A: monthly stochastic conversion of domestic firms to foreign
    * ownership, representing cross-border mergers and acquisitions.
    */
  private def applyFdiMa(firms: Vector[Firm.State], rng: RandomStream)(using p: SimParams): Vector[Firm.State] =
    if p.fdi.maProb > Share.Zero then
      firms.map: f =>
        if Firm.isAlive(f) && !f.foreignOwned &&
          f.initialSize >= p.fdi.maSizeMin &&
          p.fdi.maProb.sampleBelow(rng)
        then f.copy(foreignOwned = true)
        else f
    else firms
