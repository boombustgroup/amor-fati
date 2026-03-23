package com.boombustgroup.amorfati.engine.steps

import com.boombustgroup.amorfati.accounting.*
import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.*
import com.boombustgroup.amorfati.engine.markets.EquityMarket
import com.boombustgroup.amorfati.engine.mechanisms.{FirmEntry, SectoralMobility}
import com.boombustgroup.amorfati.agents.RegionalMigration
import com.boombustgroup.amorfati.types.*

import scala.util.Random

/** World assembly: constructs the new World state from all step outputs,
  * validates SFC accounting identities, applies FDI M&A conversions, and
  * triggers endogenous firm entry. Also computes informal economy effects
  * (four-channel tax evasion), observable values (ETS price, tourism seasonal
  * factor, deposit facility usage), and the flow-of-funds residual.
  */
object WorldAssemblyStep:

  case class Input(
      w: World,                            // current world state
      firms: Vector[Firm.State],           // pre-step firm population
      households: Vector[Household.State], // pre-step household population
      s1: FiscalConstraintStep.Output,     // fiscal constraint (month, reservation wage, lending base rate)
      s2: LaborDemographicsStep.Output,    // labor/demographics (wage, employment, ZUS, PPK)
      s3: HouseholdIncomeStep.Output,      // household income (consumption, PIT, import propensity)
      s4: DemandStep.Output,               // demand (sector multipliers, gov purchases)
      s5: FirmProcessingStep.Output,       // firm processing (loans, NPL, tax, I-O, bond issuance)
      s6: HouseholdFinancialStep.Output,   // household financial (debt service, remittances, tourism)
      s7: PriceEquityStep.Output,          // price/equity (inflation, GDP, equity, macropru)
      s8: OpenEconomyStep.Output,          // open economy (monetary policy, forex, BOP, corp bonds)
      s9: BankUpdateStep.Output,           // bank update (balance sheets, tax revenue, housing flows)
  )

  case class Output(
      newWorld: World,
      finalFirms: Vector[Firm.State],
      reassignedHouseholds: Vector[Household.State],
      sfcResult: Sfc.SfcResult,
  )

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

  def run(in: Input, rng: Random, migRng: Random)(using p: SimParams): Output =
    val equityAfterStep = finalizeEquity(in)
    val fofResidual     = computeFofResidual(in)
    val informal        = computeInformalEconomy(in)
    val obs             = computeObservables(in)

    val newW      = assembleWorld(in, equityAfterStep, fofResidual, informal, obs)
    val sfcResult = validateSfc(in, newW, fofResidual)

    val postFdiFirms             = applyFdiMa(in.s9.reassignedFirms, rng)
    val (finalFirms, firmBirths) =
      if p.flags.firmEntry then
        val r = FirmEntry.process(postFdiFirms, newW.real.automationRatio, newW.real.hybridRatio, rng)
        (r.firms, r.births)
      else (postFdiFirms, 0)

    // Regional migration: unemployed HH may relocate between NUTS-1 regions
    val postMigHh =
      if p.flags.regionalLabor then RegionalMigration(in.s9.reassignedHouseholds, in.s2.regionalWages, migRng).households
      else in.s9.reassignedHouseholds

    val finalW = newW
      .updateFlows(_.copy(firmBirths = firmBirths, firmDeaths = in.s5.firmDeaths))
      .copy(regionalWages = in.s2.regionalWages)
    Output(finalW, finalFirms, postMigHh, sfcResult)

  /** Finalize GPW equity state with aggregate household equity wealth. */
  private def finalizeEquity(in: Input): EquityMarket.State =
    val totalHhEquityWealth = PLN.fromRaw(in.s9.reassignedHouseholds.map(_.equityWealth.toLong).sum)
    in.s7.equityAfterIssuance.copy(
      hhEquityWealth = totalHhEquityWealth,
      lastWealthEffect = PLN.Zero,
      lastDomesticDividends = in.s7.netDomesticDividends,
      lastForeignDividends = in.s7.foreignDividendOutflow,
      lastDividendTax = in.s7.dividendTax,
    )

  /** Flow-of-funds residual: total firm revenue minus adjusted demand. Both
    * sides computed via the same PLN path to avoid Long↔Double rounding
    * mismatch.
    */
  private def computeFofResidual(in: Input)(using p: SimParams): PLN =
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
  private def computeInformalEconomy(in: Input)(using p: SimParams): InformalResult =
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
  private def computeObservables(in: Input)(using p: SimParams): Observables =
    import ComputationBoundary.toDouble
    val aliveBanks           = in.s9.finalBankingSector.banks.filterNot(_.failed)
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

  /** Construct the new World state from all step outputs. */
  @boundaryEscape
  private def assembleWorld(
      in: Input,
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
      bank = in.s9.resolvedBank,
      bankingSector = in.s9.finalBankingSector,
      forex = in.s8.external.newForex,
      bop = in.s8.external.newBop,
      hhAgg = in.s9.finalHhAgg,
      households = in.s9.reassignedHouseholds,
      monetaryAgg = in.s9.monAgg,
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
      flows = buildFlowState(in, informal),
    )

  /** Construct the FlowState for this step. */
  @boundaryEscape
  private def buildFlowState(in: Input, informal: InformalResult): FlowState =
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
      sectorDemandMult = in.s4.sectorMults,
      fiscalRuleSeverity = in.s4.fiscalRuleStatus.bindingRule,
      govSpendingCutRatio = in.s4.fiscalRuleStatus.spendingCutRatio,
    )

  /** Run SFC validation against previous and current snapshots. */
  private def validateSfc(in: Input, newW: World, fofResidual: PLN)(using p: SimParams): Sfc.SfcResult =
    val prevSnap = Sfc.snapshot(in.w, in.firms, in.w.households)
    val currSnap = Sfc.snapshot(newW, in.s9.reassignedFirms, in.s9.reassignedHouseholds)
    val flows    = buildMonthlyFlows(in, fofResidual)
    Sfc.validate(prevSnap, currSnap, flows)

  /** Construct Sfc.MonthlyFlows from all step outputs. */
  @boundaryEscape
  private def buildMonthlyFlows(in: Input, fofResidual: PLN)(using p: SimParams): Sfc.MonthlyFlows =
    Sfc.MonthlyFlows(
      govSpending = in.s9.newGovWithYield.unempBenefitSpend
        + in.s9.newGovWithYield.socialTransferSpend
        + in.s4.govPurchases + in.s8.banking.monthlyDebtService + in.s2.newZus.govSubvention
        + in.s2.newNfz.govSubvention + in.s2.newEarmarked.totalGovSubvention + in.s7.euCofin,
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
