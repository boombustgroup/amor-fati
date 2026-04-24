package com.boombustgroup.amorfati.diagnostics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.{MonthRandomness, SignalExtraction}
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.engine.economics.*
import com.boombustgroup.amorfati.engine.markets.{PriceLevel, RegionalClearing}
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import com.boombustgroup.amorfati.types.*

object InflationProbe:

  private def exchangeRateValue(er: ExchangeRate): Double =
    er.toLong.toDouble / com.boombustgroup.amorfati.fp.FixedPointBase.ScaleD

  private def laborSupplyCount(wage: PLN, resWage: PLN, totalPopulation: Int)(using p: SimParams): Int =
    import ComputationBoundary.toDouble
    val x     = toDouble(p.household.laborSupplySteepness) * (wage / resWage - 1.0)
    val ratio = 1.0 / (1.0 + Math.exp(-x))
    (totalPopulation * ratio).toInt

  private def topPressures(pressures: Vector[Multiplier])(using p: SimParams): String =
    p.sectorDefs
      .zip(pressures)
      .sortBy { case (_, v) => -(v / Multiplier.One) }
      .take(3)
      .map { case (sec, v) =>
        s"${sec.name}=${"%.2f".formatLocal(java.util.Locale.US, v / Multiplier.One)}"
      }
      .mkString(", ")

  private case class GovPurchasesBreakdown(
      zusNetSurplus: PLN,
      unempGap: Share,
      base: PLN,
      recycleCounterfactual: PLN,
      stimulus: PLN,
      rawTarget: PLN,
  )

  private def govPurchasesBreakdown(world: World, employed: Int)(using p: SimParams): GovPurchasesBreakdown =
    val zusNetSurplus = (world.social.zus.contributions - world.social.zus.pensionPayments).max(PLN.Zero)
    val unempRate     = Share.One - Share.fraction(employed, world.derivedTotalPopulation)
    val unempGap      = (unempRate - p.monetary.nairu).max(Share.Zero)
    val base          = p.fiscal.govBaseSpending * world.priceLevel.toMultiplier.max(Multiplier.One)
    val recycling     = (world.gov.taxRevenue + zusNetSurplus) * p.fiscal.govFiscalRecyclingRate
    val stimulus      = p.fiscal.govBaseSpending * unempGap * p.fiscal.govAutoStabMult
    GovPurchasesBreakdown(zusNetSurplus, unempGap, base, recycling, stimulus, base + stimulus)

  @main def runInflationProbe(seed: Long = 1L, months: Int = 12): Unit =
    given SimParams = SimParams.defaults
    import ComputationBoundary.toDouble

    val init                 = WorldInit.initialize(InitRandomness.Contract.fromSeed(seed))
    var world                = init.world
    var firms                = init.firms
    var hhs                  = init.households
    var banks                = init.banks
    var ledgerFinancialState = init.ledgerFinancialState

    println(s"seed=$seed months=$months")

    (1 to months).foreach: month =>
      val population        = world.derivedTotalPopulation.max(1)
      val contract          = MonthRandomness.Contract.fromSeed(seed * 1000 + month)
      val s1                = FiscalConstraintEconomics.compute(world, banks, ledgerFinancialState, ExecutionMonth(month))
      val s2Pre             = LaborEconomics.compute(world, firms, hhs, s1)
      val prevWage          = toDouble(world.householdMarket.marketWage)
      val rawLaborWage      = toDouble(RegionalClearing.clear(world.regionalWages, s1.resWage, s2Pre.laborDemand, population).nationalWage)
      val target            = toDouble(summon[SimParams].monetary.targetInfl)
      val expWagePressure   =
        toDouble(summon[SimParams].labor.expWagePassthrough) * Math.max(0.0, toDouble(world.mechanisms.expectations.expectedInflation) - target) / 12.0
      val wageAfterExp      = Math.max(toDouble(s1.resWage), rawLaborWage * (1.0 + expWagePressure))
      val aggUnionDensity   =
        summon[SimParams].sectorDefs.zipWithIndex
          .map((s, i) => toDouble(s.share) * toDouble(summon[SimParams].labor.unionDensity(i)))
          .sum
      val unionAdjustedWage =
        if wageAfterExp < prevWage then
          val decline = prevWage - wageAfterExp
          Math.max(toDouble(s1.resWage), wageAfterExp + decline * toDouble(summon[SimParams].labor.unionRigidity) * aggUnionDensity)
        else wageAfterExp
      val supplyAtPrev      = laborSupplyCount(world.householdMarket.marketWage, s1.resWage, population)
      val newSupply         = laborSupplyCount(PLN(unionAdjustedWage), s1.resWage, population)
      val excessDemand      = (s2Pre.laborDemand - supplyAtPrev).toDouble / population
      val phillipsGrowth    = if prevWage > 0.0 then rawLaborWage / prevWage - 1.0 else 0.0
      val expGrowth         = if rawLaborWage > 0.0 then wageAfterExp / rawLaborWage - 1.0 else 0.0
      val unionGrowth       = if wageAfterExp > 0.0 then unionAdjustedWage / wageAfterExp - 1.0 else 0.0
      val s3                =
        HouseholdIncomeEconomics.compute(
          world,
          firms,
          hhs,
          banks,
          ledgerFinancialState,
          s1.lendingBaseRate,
          s1.resWage,
          s2Pre.newWage,
          contract.stages.householdIncomeEconomics.newStream(),
        )
      val s4                = DemandEconomics.compute(world, s2Pre.employed, s2Pre.living, s3.domesticCons)
      val s5                = FirmEconomics.runStep(world, firms, hhs, banks, ledgerFinancialState, s1, s2Pre, s3, s4, contract.stages.firmEconomics.newStream())
      val living            = s5.ioFirms.filter(Firm.isAlive)
      val s2                = LaborEconomics.reconcilePostFirmStep(world, s1, s2Pre, living, s5.households)
      val s6                =
        HouseholdFinancialEconomics.compute(world, s1.m, s2.employed, s3.hhAgg, contract.stages.householdFinancialEconomics.newStream())
      val s7                = PriceEquityEconomics.compute(
        w = world,
        month = s1.m,
        wageGrowth = s2.wageGrowth,
        domesticCons = s3.domesticCons,
        govPurchases = s4.govPurchases,
        avgDemandMult = s4.avgDemandMult,
        totalSystemLoans = ledgerFinancialState.banks.map(_.firmLoan).sum,
        firmStep = s5,
      )
      val s8                =
        OpenEconEconomics.runStep(
          OpenEconEconomics.StepInput(
            world,
            ledgerFinancialState,
            s1,
            s2,
            s3,
            s4,
            s5,
            s6,
            s7,
            banks,
            contract.stages.openEconEconomics.newStream(),
          ),
        )
      val s9                =
        BankingEconomics.runStep(
          BankingEconomics.StepInput(
            world,
            ledgerFinancialState,
            s1,
            s2,
            s3,
            s4,
            s5,
            s6,
            s7,
            s8,
            banks,
            contract.stages.bankingEconomics.newStream(),
          ),
        )

      val exDev        = exchangeRateValue(world.forex.exchangeRate) / exchangeRateValue(summon[SimParams].forex.baseExRate) - 1.0
      val priceUpd     = PriceLevel.update(
        prevInflation = world.inflation,
        expectedInflation = world.mechanisms.expectations.expectedInflation,
        prevPrice = world.priceLevel,
        demandMult = s4.avgDemandMult,
        wageGrowth = s2.wageGrowth,
        exRateDeviation = world.forex.exchangeRate.deviationFrom(summon[SimParams].forex.baseExRate),
      )
      val demandPullM  = toDouble(priceUpd.demandPull)
      val costPushM    = toDouble(priceUpd.costPush)
      val importPushM  = toDouble(priceUpd.importPush)
      val rawMonthly   = toDouble(priceUpd.rawMonthly)
      val flooredM     = toDouble(priceUpd.flooredMonthly)
      val baseAnnual   = toDouble(priceUpd.inflation)
      val totalInfl    = toDouble(s7.newInfl)
      val markupAnnual = toDouble(s5.markupInflation)
      val unemp        = 1.0 - s2.employed.toDouble / population.toDouble
      val refRate      = toDouble(s8.monetary.newRefRate)
      val expInfl      = toDouble(s8.monetary.newExp.expectedInflation)
      val credibility  = toDouble(s8.monetary.newExp.credibility)
      val fwdGuidance  = toDouble(s8.monetary.newExp.forwardGuidanceRate)
      val realRate     = refRate - expInfl
      val govPurchases = toDouble(s4.govPurchases)
      val govBreakdown = govPurchasesBreakdown(world, s2Pre.employed)
      val govCurrent   = toDouble(s9.newGovWithYield.govCurrentSpend)
      val govCapital   = toDouble(s9.newGovWithYield.govCapitalSpend)
      val euProjectCap = toDouble(s9.newGovWithYield.euProjectCapital)
      val euCofin      = toDouble(s9.newGovWithYield.euCofinancing)
      val deficit      = toDouble(s9.newGovWithYield.deficit)
      val debtToGdp    =
        if s7.gdp > PLN.Zero then (toDouble(s9.newGovWithYield.cumulativeDebt) / toDouble(s7.gdp)) / 12.0 * 100.0
        else 0.0
      val deficitToGdp =
        if s7.gdp > PLN.Zero then (deficit / toDouble(s7.gdp)) * 100.0
        else 0.0

      println(
        f"m=$month%2d u=${unemp * 100.0}%.2f%% pi=${totalInfl * 100.0}%.2f%% wage=${toDouble(s2.newWage)}%.0f wg=${toDouble(s2.wageGrowth) * 100.0}%.2f%% demand=${s4.avgDemandMult / Multiplier.One}%.3f markup=${markupAnnual * 100.0}%.2f%%",
      )
      println(
        f"  channels monthly: demand=${demandPullM * 100.0}%.2fpp cost=${costPushM * 100.0}%.2fpp import=${importPushM * 100.0}%.2fpp raw=${rawMonthly * 100.0}%.2fpp floor=${flooredM * 100.0}%.2fpp",
      )
      println(
        f"  annualized: base=${baseAnnual * 100.0}%.2f%% markup=${markupAnnual * 100.0}%.2f%% total=${totalInfl * 100.0}%.2f%% exDev=${exDev * 100.0}%.2f%% importCost=${toDouble(world.external.gvc.importCostIndex)}%.3f commodity=${toDouble(world.external.gvc.commodityPriceIndex)}%.3f",
      )
      println(
        f"  policy: ref=${refRate * 100.0}%.2f%% expPi=${expInfl * 100.0}%.2f%% real=${realRate * 100.0}%.2f%% cred=${credibility * 100.0}%.1f%% fg=${fwdGuidance * 100.0}%.2f%%",
      )
      println(
        f"  wages: phillips=${phillipsGrowth * 100.0}%.2f%% exp=${expGrowth * 100.0}%.2f%% union=${unionGrowth * 100.0}%.2f%% raw=${rawLaborWage}%.0f afterExp=${wageAfterExp}%.0f final=${unionAdjustedWage}%.0f",
      )
      println(
        f"  labor: demand=${s2Pre.laborDemand}%d supplyPrev=${supplyAtPrev}%d supplyNew=${newSupply}%d excess=${excessDemand * 100.0}%.2f%% employedPre=${s2Pre.employed}%d employedPost=${s2.employed}%d",
      )
      println(
        f"  fiscal: govPurch=${govPurchases}%.0f govCur=${govCurrent}%.0f govCapDom=${govCapital}%.0f euProjCap=${euProjectCap}%.0f euCofinDom=${euCofin}%.0f def=${deficit}%.0f def/gdp=${deficitToGdp}%.2f%% debt/gdp=${debtToGdp}%.2f%% rule=${s4.fiscalRuleStatus.bindingRule} cut=${toDouble(s4.fiscalRuleStatus.spendingCutRatio) * 100.0}%.2f%%",
      )
      println(
        f"  gov raw target: base=${toDouble(govBreakdown.base)}%.0f recycleCf=${toDouble(govBreakdown.recycleCounterfactual)}%.0f stimulus=${toDouble(govBreakdown.stimulus)}%.0f raw=${toDouble(govBreakdown.rawTarget)}%.0f zusSurplus=${toDouble(govBreakdown.zusNetSurplus)}%.0f unempGap=${toDouble(govBreakdown.unempGap) * 100.0}%.2f%%",
      )
      println(s"  top pressure: ${topPressures(s4.sectorDemandPressure)}")

      val assemblyInput = WorldAssemblyEconomics.StepInput(
        world,
        s1,
        s2,
        s3,
        s4,
        s5,
        s6,
        s7,
        s8,
        s9,
      )
      val assembled     = WorldAssemblyEconomics.computePostMonth(assemblyInput, contract.assembly.newStreams())
      val seedOut       = SignalExtraction
        .fromPostMonth(
          world = assembled.world,
          households = assembled.households,
          operationalHiringSlack = assemblyInput.s2.operationalHiringSlack,
          startupAbsorptionRate = assembled.startupAbsorptionRate,
          demand = SignalExtraction.DemandOutcomes(
            sectorDemandMult = assemblyInput.s4.sectorMults,
            sectorDemandPressure = assemblyInput.s4.sectorDemandPressure,
            sectorHiringSignal = assemblyInput.s4.sectorHiringSignal,
          ),
        )
        .seedOut

      world = assembled.world.copy(pipeline = assembled.world.pipeline.withDecisionSignals(seedOut))
      firms = assembled.firms
      hhs = assembled.households
      banks = assembled.banks
      ledgerFinancialState = assembled.ledgerFinancialState
