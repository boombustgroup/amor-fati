package com.boombustgroup.amorfati.diagnostics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.engine.economics.*
import com.boombustgroup.amorfati.engine.markets.RegionalClearing
import com.boombustgroup.amorfati.init.WorldInit
import com.boombustgroup.amorfati.types.*

import scala.util.Random

object InflationProbe:

  private val DemandPullWeight                            = 0.15
  private val CostPushWeight                              = 0.25
  private val ImportPushWeight                            = 0.25
  private val DeflationFloor                              = -0.015
  private val FloorPassThrough                            = 0.3
  private val SmoothingLambda                             = 0.3
  private def exchangeRateValue(er: ExchangeRate): Double =
    er.toLong.toDouble / com.boombustgroup.amorfati.fp.FixedPointBase.ScaleD

  private def softFloor(raw: Double): Double =
    if raw >= DeflationFloor then raw
    else DeflationFloor + (raw - DeflationFloor) * FloorPassThrough

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

    val init  = WorldInit.initialize(seed)
    var world = init.world
    var firms = init.firms
    var hhs   = init.households
    var banks = init.banks

    println(s"seed=$seed months=$months")

    (1 to months).foreach: month =>
      val population        = world.derivedTotalPopulation.max(1)
      val rng               = new Random(seed * 1000 + month)
      val fiscal            = FiscalConstraintEconomics.compute(world, banks)
      val s1                = FiscalConstraintEconomics.toOutput(fiscal)
      val labor             = LaborEconomics.compute(world, firms, hhs, s1)
      val prevWage          = toDouble(world.householdMarket.marketWage)
      val rawLaborWage      = toDouble(RegionalClearing.clear(world.regionalWages, s1.resWage, labor.laborDemand, population).nationalWage)
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
      val excessDemand      = (labor.laborDemand - supplyAtPrev).toDouble / population
      val phillipsGrowth    = if prevWage > 0.0 then rawLaborWage / prevWage - 1.0 else 0.0
      val expGrowth         = if rawLaborWage > 0.0 then wageAfterExp / rawLaborWage - 1.0 else 0.0
      val unionGrowth       = if wageAfterExp > 0.0 then unionAdjustedWage / wageAfterExp - 1.0 else 0.0
      val s2Pre             = LaborEconomics.Output(
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
      val s3                = HouseholdIncomeEconomics.compute(world, firms, hhs, banks, s1.lendingBaseRate, s1.resWage, s2Pre.newWage, rng)
      val s4                = DemandEconomics.compute(DemandEconomics.Input(world, s2Pre.employed, s2Pre.living, s3.domesticCons))
      val s5                = FirmEconomics.runStep(world, firms, hhs, banks, s1, s2Pre, s3, s4, rng)
      val living            = s5.ioFirms.filter(Firm.isAlive)
      val s2                = LaborEconomics.reconcilePostFirmStep(world, s1, s2Pre, living, s5.households)
      val s6                = HouseholdFinancialEconomics.compute(world, s1.m, s2.employed, s3.hhAgg, rng)
      val s7                = PriceEquityEconomics.compute(
        PriceEquityEconomics.Input(
          world,
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
      val s8                = OpenEconEconomics.runStep(OpenEconEconomics.StepInput(world, s1, s2, s3, s4, s5, s6, s7, banks, rng))
      val s9                = BankingEconomics.runStep(BankingEconomics.StepInput(world, s1, s2, s3, s4, s5, s6, s7, s8, banks, rng))

      val exDev         = exchangeRateValue(world.forex.exchangeRate) / summon[SimParams].forex.baseExRate - 1.0
      val demandPullM   = toDouble((s4.avgDemandMult.deviationFromOne.toScalar * Scalar(DemandPullWeight)).toCoefficient)
      val costPushM     = toDouble(s2.wageGrowth) * CostPushWeight
      val rawImportPush = Math.max(0.0, exDev) * toDouble(summon[SimParams].forex.importPropensity) * ImportPushWeight
      val importPushM   = Math.min(rawImportPush, toDouble(summon[SimParams].openEcon.importPushCap))
      val rawMonthly    = demandPullM + costPushM + importPushM
      val flooredM      = softFloor(rawMonthly)
      val baseAnnual    = toDouble(world.inflation) * (1.0 - SmoothingLambda) + (flooredM * 12.0) * SmoothingLambda
      val totalInfl     = toDouble(s7.newInfl)
      val markupAnnual  = toDouble(s5.markupInflation)
      val unemp         = 1.0 - s2.employed.toDouble / population.toDouble
      val refRate       = toDouble(s8.monetary.newRefRate)
      val expInfl       = toDouble(s8.monetary.newExp.expectedInflation)
      val credibility   = toDouble(s8.monetary.newExp.credibility)
      val fwdGuidance   = toDouble(s8.monetary.newExp.forwardGuidanceRate)
      val realRate      = refRate - expInfl
      val govPurchases  = toDouble(s4.govPurchases)
      val govBreakdown  = govPurchasesBreakdown(world, s2Pre.employed)
      val govCurrent    = toDouble(s9.newGovWithYield.govCurrentSpend)
      val govCapital    = toDouble(s9.newGovWithYield.govCapitalSpend)
      val euProjectCap  = toDouble(s9.newGovWithYield.euProjectCapital)
      val euCofin       = toDouble(s9.newGovWithYield.euCofinancing)
      val deficit       = toDouble(s9.newGovWithYield.deficit)
      val debtToGdp     =
        if s7.gdp > PLN.Zero then (toDouble(s9.newGovWithYield.cumulativeDebt) / toDouble(s7.gdp)) / 12.0 * 100.0
        else 0.0
      val deficitToGdp  =
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
        f"  labor: demand=${labor.laborDemand}%d supplyPrev=${supplyAtPrev}%d supplyNew=${newSupply}%d excess=${excessDemand * 100.0}%.2f%% employedPre=${labor.employed}%d employedPost=${s2.employed}%d",
      )
      println(
        f"  fiscal: govPurch=${govPurchases}%.0f govCur=${govCurrent}%.0f govCapDom=${govCapital}%.0f euProjCap=${euProjectCap}%.0f euCofinDom=${euCofin}%.0f def=${deficit}%.0f def/gdp=${deficitToGdp}%.2f%% debt/gdp=${debtToGdp}%.2f%% rule=${s4.fiscalRuleStatus.bindingRule} cut=${toDouble(s4.fiscalRuleStatus.spendingCutRatio) * 100.0}%.2f%%",
      )
      println(
        f"  gov raw target: base=${toDouble(govBreakdown.base)}%.0f recycleCf=${toDouble(govBreakdown.recycleCounterfactual)}%.0f stimulus=${toDouble(govBreakdown.stimulus)}%.0f raw=${toDouble(govBreakdown.rawTarget)}%.0f zusSurplus=${toDouble(govBreakdown.zusNetSurplus)}%.0f unempGap=${toDouble(govBreakdown.unempGap) * 100.0}%.2f%%",
      )
      println(s"  top pressure: ${topPressures(s4.sectorDemandPressure)}")

      val assembled = WorldAssemblyEconomics.compute(
        WorldAssemblyEconomics.Input(
          w = world,
          firms = firms,
          households = hhs,
          banks = banks,
          month = fiscal.month,
          lendingBaseRate = fiscal.lendingBaseRate,
          resWage = fiscal.resWage,
          baseMinWage = fiscal.baseMinWage,
          minWagePriceLevel = fiscal.updatedMinWagePriceLevel,
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
          bankOutput = s9,
          rng = rng,
          migRng = rng,
        ),
      )

      world = assembled.world
      firms = assembled.firms
      hhs = assembled.households
      banks = assembled.banks
