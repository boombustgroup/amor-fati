package com.boombustgroup.amorfati.engine.economics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.agents.RegionalMigration
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.*
import com.boombustgroup.amorfati.engine.ledger.LedgerStateAdapter
import com.boombustgroup.amorfati.engine.markets.{EquityMarket, LaborMarket}
import com.boombustgroup.amorfati.engine.mechanisms.{FirmEntry, InformalEconomy, SectoralMobility}
import com.boombustgroup.amorfati.types.*

import scala.util.Random

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
      householdAggregates: Household.Aggregates,
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
      rng: Random,
      migRng: Random,
  )

  case class Result(
      world: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      banks: Vector[Banking.BankState],
      householdAggregates: Household.Aggregates,
  )

  def compute(in: Input)(using SimParams): Result =
    val s1 = FiscalConstraintEconomics.Output(in.month, in.baseMinWage, in.minWagePriceLevel, in.resWage, in.lendingBaseRate)

    val s10 = runStep(
      StepInput(
        in.w,
        in.firms,
        in.households,
        in.banks,
        s1,
        in.laborOutput,
        in.hhOutput,
        buildDemandOutput(in),
        in.firmOutput,
        in.hhFinancialOutput,
        in.priceEquityOutput,
        in.openEconOutput,
        in.bankOutput,
      ),
      in.rng,
      in.migRng,
    )

    Result(s10.newWorld, s10.finalFirms, s10.reassignedHouseholds, s10.banks, s10.householdAggregates)

  // ---------------------------------------------------------------------------
  // runStep — migrated from WorldAssemblyStep.run
  // ---------------------------------------------------------------------------

  def runStep(in: StepInput, rng: Random, migRng: Random)(using p: SimParams): StepOutput =
    val equityAfterStep = finalizeEquity(in)
    val fofResidual     = computeFofResidual(in)
    val informal        = computeInformalEconomy(in)
    val obs             = computeObservables(in)

    val newW = assembleWorld(in, equityAfterStep, fofResidual, informal, obs)

    val postFdiFirms     = applyFdiMa(in.s9.reassignedFirms, rng)
    val updatedPop       = in.w.derivedTotalPopulation + in.s5.netMigration
    val postFirmEmployed = in.s9.reassignedHouseholds.count: hh =>
      hh.status match
        case HhStatus.Employed(_, _, _) => true
        case _                          => false
    val unemploymentRate = if updatedPop > 0 then Share.One - Share.fraction(postFirmEmployed, updatedPop) else Share.Zero
    val entryStep        =
      val r = FirmEntry.process(
        postFdiFirms,
        newW.real.automationRatio,
        newW.real.hybridRatio,
        unemploymentRate,
        Share(in.s2.aggregateHiringSlack),
        newW.inflation,
        newW.mechanisms.expectations.expectedInflation,
        Share(in.w.pipeline.startupAbsorptionRate),
        rng,
      )
      EntryStepResult(r.firms, r.births, r.netBirths, r.entrantIds)

    val startupStaffing = applyStartupStaffing(in, entryStep.firms, in.s9.reassignedHouseholds, rng)

    // Regional migration: unemployed HH may relocate between NUTS-1 regions
    val postMigHh  = RegionalMigration(startupStaffing.households, in.s2.regionalWages, migRng).households
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
      .copy(regionalWages = in.s2.regionalWages)
    StepOutput(finalW, finalFirms, postMigHh, in.s9.banks, startupStaffing.hhAgg)

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
    val etsPrice      = toDouble(p.climate.etsBasePrice) * Math.pow(1.0 + toDouble(p.climate.etsPriceDrift) / monthsPerYear, in.s1.m.toDouble)

    val monthInYear           = ((in.s1.m - 1) % 12) + 1
    val tourismSeasonalFactor = 1.0 + toDouble(p.tourism.seasonality) * Math.cos(2 * Math.PI * (monthInYear - p.tourism.peakMonth) / 12.0)

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
    val ledgerSupported  = buildLedgerSupportedSnapshot(in)
    val provisionalWorld = World(
      month = in.s1.m,
      inflation = in.s7.newInfl,
      priceLevel = in.s7.newPrice,
      currentSigmas = in.s7.newSigmas,
      gov = in.s9.newGovWithYield.copy(
        financial = in.s9.newGovWithYield.financial.copy(
          bondsOutstanding = in.w.gov.bondsOutstanding,
          foreignBondHoldings = in.w.gov.foreignBondHoldings,
        ),
        policy = in.s9.newGovWithYield.policy.copy(
          minWageLevel = in.s1.baseMinWage,
          minWagePriceLevel = in.s1.updatedMinWagePriceLevel,
        ),
      ),
      nbp = in.s9.finalNbp.copy(
        balance = in.s9.finalNbp.balance.copy(
          govBondHoldings = in.w.nbp.govBondHoldings,
          fxReserves = in.w.nbp.fxReserves,
        ),
      ),
      bankingSector = in.s9.bankingMarket,
      forex = in.s8.external.newForex,
      bop = in.s8.external.newBop,
      householdMarket = HouseholdMarketState.fromAggregates(in.s9.finalHhAgg),
      social = SocialState(
        jst = in.s9.newJst,
        zus = in.s2.newZus.copy(fusBalance = in.w.social.zus.fusBalance),
        nfz = in.s2.newNfz.copy(balance = in.w.social.nfz.balance),
        ppk = in.s9.finalPpk.copy(bondHoldings = in.w.social.ppk.bondHoldings),
        demographics = in.s2.newDemographics,
        earmarked = in.s2.newEarmarked.copy(
          fp = in.s2.newEarmarked.fp.copy(balance = in.w.social.earmarked.fpBalance),
          pfron = in.s2.newEarmarked.pfron.copy(balance = in.w.social.earmarked.pfronBalance),
          fgsp = in.s2.newEarmarked.fgsp.copy(balance = in.w.social.earmarked.fgspBalance),
        ),
      ),
      financial = FinancialMarketsState(
        equity = equityAfterStep,
        corporateBonds = in.s8.corpBonds.newCorpBonds.copy(
          bankHoldings = in.w.financial.corporateBonds.bankHoldings,
          ppkHoldings = in.w.financial.corporateBonds.ppkHoldings,
        ),
        insurance = in.s9.finalInsurance.copy(
          reserves = in.w.financial.insurance.reserves,
          portfolio = in.w.financial.insurance.portfolio,
        ),
        nbfi = in.s9.finalNbfi.copy(
          tfi = in.w.financial.nbfi.tfi,
          credit = in.w.financial.nbfi.credit,
        ),
        quasiFiscal = in.s9.newQuasiFiscal.copy(
          bondsOutstanding = in.w.financial.quasiFiscal.bondsOutstanding,
          loanPortfolio = in.w.financial.quasiFiscal.loanPortfolio,
        ),
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
      pipeline = buildPipelineState(in),
      flows = buildFlowState(in, informal),
    )
    withLedgerSupportedFinancialState(provisionalWorld, LedgerStateAdapter.roundTripSupported(ledgerSupported))

  private def buildLedgerSupportedSnapshot(in: StepInput): LedgerStateAdapter.SupportedFinancialSnapshot =
    LedgerStateAdapter.SupportedFinancialSnapshot(
      households = in.s9.reassignedHouseholds.map(LedgerStateAdapter.householdBalances),
      firms = in.s9.reassignedFirms.map(LedgerStateAdapter.firmBalances),
      banks = in.s9.banks.map(LedgerStateAdapter.bankBalances),
      government = LedgerStateAdapter.GovernmentBalances(
        govBondOutstanding = in.s9.newGovWithYield.bondsOutstanding,
      ),
      foreign = LedgerStateAdapter.ForeignBalances(
        govBondHoldings = in.s9.newGovWithYield.foreignBondHoldings,
      ),
      nbp = LedgerStateAdapter.NbpBalances(
        govBondHoldings = in.s9.finalNbp.govBondHoldings,
        foreignAssets = in.s9.finalNbp.fxReserves,
      ),
      insurance = LedgerStateAdapter.InsuranceBalances(
        lifeReserve = in.s9.finalInsurance.lifeReserves,
        nonLifeReserve = in.s9.finalInsurance.nonLifeReserves,
        govBondHoldings = in.s9.finalInsurance.govBondHoldings,
        corpBondHoldings = in.s9.finalInsurance.corpBondHoldings,
        equityHoldings = in.s9.finalInsurance.equityHoldings,
      ),
      funds = LedgerStateAdapter.FundBalances(
        zusCash = in.s2.newZus.fusBalance,
        nfzCash = in.s2.newNfz.balance,
        ppkGovBondHoldings = in.s9.finalPpk.bondHoldings,
        ppkCorpBondHoldings = in.s8.corpBonds.newCorpBonds.ppkHoldings,
        fpCash = in.s2.newEarmarked.fpBalance,
        pfronCash = in.s2.newEarmarked.pfronBalance,
        fgspCash = in.s2.newEarmarked.fgspBalance,
        jstCash = in.s9.newJst.deposits,
        corpBondOtherHoldings = in.s8.corpBonds.newCorpBonds.otherHoldings,
        nbfi = LedgerStateAdapter.NbfiFundBalances(
          tfiUnit = in.s9.finalNbfi.tfiAum,
          govBondHoldings = in.s9.finalNbfi.tfiGovBondHoldings,
          corpBondHoldings = in.s9.finalNbfi.tfiCorpBondHoldings,
          equityHoldings = in.s9.finalNbfi.tfiEquityHoldings,
          cashHoldings = in.s9.finalNbfi.tfiCashHoldings,
          nbfiLoanStock = in.s9.finalNbfi.nbfiLoanStock,
        ),
        quasiFiscal = LedgerStateAdapter.QuasiFiscalBalances(
          bondsOutstanding = in.s9.newQuasiFiscal.bondsOutstanding,
          loanPortfolio = in.s9.newQuasiFiscal.loanPortfolio,
        ),
      ),
    )

  private[economics] def withLedgerSupportedFinancialState(
      world: World,
      supported: LedgerStateAdapter.SupportedFinancialSnapshot,
  ): World =
    val corpBonds = LedgerStateAdapter.corporateBondCircuit(supported)
    world.copy(
      gov = world.gov.copy(
        financial = world.gov.financial.copy(
          bondsOutstanding = supported.government.govBondOutstanding,
          foreignBondHoldings = supported.foreign.govBondHoldings,
        ),
      ),
      nbp = world.nbp.copy(
        balance = world.nbp.balance.copy(
          govBondHoldings = supported.nbp.govBondHoldings,
          fxReserves = supported.nbp.foreignAssets,
        ),
      ),
      social = world.social.copy(
        jst = world.social.jst.copy(deposits = supported.funds.jstCash),
        zus = world.social.zus.copy(fusBalance = supported.funds.zusCash),
        nfz = world.social.nfz.copy(balance = supported.funds.nfzCash),
        ppk = world.social.ppk.copy(bondHoldings = supported.funds.ppkGovBondHoldings),
        earmarked = world.social.earmarked.copy(
          fp = world.social.earmarked.fp.copy(balance = supported.funds.fpCash),
          pfron = world.social.earmarked.pfron.copy(balance = supported.funds.pfronCash),
          fgsp = world.social.earmarked.fgsp.copy(balance = supported.funds.fgspCash),
        ),
      ),
      financial = world.financial.copy(
        corporateBonds = world.financial.corporateBonds.copy(
          bankHoldings = corpBonds.bankHoldings,
          ppkHoldings = corpBonds.ppkHoldings,
          otherHoldings = corpBonds.otherHoldings,
        ),
        insurance = world.financial.insurance.copy(
          reserves = world.financial.insurance.reserves.copy(
            lifeReserves = supported.insurance.lifeReserve,
            nonLifeReserves = supported.insurance.nonLifeReserve,
          ),
          portfolio = world.financial.insurance.portfolio.copy(
            govBondHoldings = supported.insurance.govBondHoldings,
            corpBondHoldings = supported.insurance.corpBondHoldings,
            equityHoldings = supported.insurance.equityHoldings,
          ),
        ),
        nbfi = world.financial.nbfi.copy(
          tfi = world.financial.nbfi.tfi.copy(
            tfiAum = supported.funds.nbfi.tfiUnit,
            tfiGovBondHoldings = supported.funds.nbfi.govBondHoldings,
            tfiCorpBondHoldings = supported.funds.nbfi.corpBondHoldings,
            tfiEquityHoldings = supported.funds.nbfi.equityHoldings,
            tfiCashHoldings = supported.funds.nbfi.cashHoldings,
          ),
          credit = world.financial.nbfi.credit.copy(
            nbfiLoanStock = supported.funds.nbfi.nbfiLoanStock,
          ),
        ),
        quasiFiscal = world.financial.quasiFiscal.copy(
          bondsOutstanding = supported.funds.quasiFiscal.bondsOutstanding,
          loanPortfolio = supported.funds.quasiFiscal.loanPortfolio,
        ),
      ),
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
  private def applyFdiMa(firms: Vector[Firm.State], rng: Random)(using p: SimParams): Vector[Firm.State] =
    if p.fdi.maProb > Share.Zero then
      firms.map: f =>
        if Firm.isAlive(f) && !f.foreignOwned &&
          f.initialSize >= p.fdi.maSizeMin &&
          p.fdi.maProb.sampleBelow(rng)
        then f.copy(foreignOwned = true)
        else f
    else firms
