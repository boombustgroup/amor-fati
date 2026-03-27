package com.boombustgroup.amorfati.diagnostics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.economics.*
import com.boombustgroup.amorfati.init.WorldInit
import com.boombustgroup.amorfati.types.*

import scala.util.Random

object InflationProbe:

  private val DemandPullWeight = 0.15
  private val CostPushWeight   = 0.25
  private val ImportPushWeight = 0.25
  private val AutoDeflation    = 0.060
  private val HybridDeflation  = 0.018
  private val DeflationFloor   = -0.015
  private val FloorPassThrough = 0.3
  private val SmoothingLambda  = 0.3

  private def softFloor(raw: Double): Double =
    if raw >= DeflationFloor then raw
    else DeflationFloor + (raw - DeflationFloor) * FloorPassThrough

  private def topPressures(pressures: Vector[Double])(using p: SimParams): String =
    p.sectorDefs.zip(pressures).sortBy(-_._2).take(3).map { case (sec, v) =>
      s"${sec.name}=${"%.2f".formatLocal(java.util.Locale.US, v)}"
    }.mkString(", ")

  @main def runInflationProbe(seed: Long = 1L, months: Int = 12): Unit =
    given SimParams = SimParams.defaults
    import ComputationBoundary.toDouble

    val init  = WorldInit.initialize(seed)
    var world = init.world
    var firms = init.firms
    var hhs   = init.households

    println(s"seed=$seed months=$months")

    (1 to months).foreach: month =>
      val rng    = new Random(seed * 1000 + month)
      val fiscal = FiscalConstraintEconomics.compute(world)
      val s1     = FiscalConstraintEconomics.toOutput(fiscal)
      val labor  = LaborEconomics.compute(world, firms, hhs, s1)
      val s2Pre  = LaborEconomics.Output(
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
      val s3     = HouseholdIncomeEconomics.compute(world, firms, hhs, s1.lendingBaseRate, s1.resWage, s2Pre.newWage, rng)
      val s4     = DemandEconomics.compute(DemandEconomics.Input(world, s2Pre.employed, s2Pre.living, s3.domesticCons))
      val s5     = FirmEconomics.runStep(world, firms, hhs, s1, s2Pre, s3, s4, rng)
      val living = s5.ioFirms.filter(Firm.isAlive)
      val s2     = s2Pre.copy(
        employed = s5.households.count(hh => hh.status match
          case HhStatus.Employed(_, _, _) => true
          case _                          => false
        ),
        laborDemand = living.map(Firm.workerCount).sum,
        living = living,
      )
      val s6     = HouseholdFinancialEconomics.compute(world, s1.m, s2.employed, s3.hhAgg, rng)
      val s7     = PriceEquityEconomics.compute(
        PriceEquityEconomics.Input(world, s1.m, s2.newWage, s2.employed, s2.wageGrowth, s3.domesticCons, s4.avgDemandMult, s4.sectorMults, s5),
        rng,
      )
      val s8     = OpenEconEconomics.runStep(OpenEconEconomics.StepInput(world, s1, s2, s3, s4, s5, s6, s7, rng))
      val s9     = BankingEconomics.runStep(BankingEconomics.StepInput(world, s1, s2, s3, s4, s5, s6, s7, s8, rng))

      val exDev         = (world.forex.exchangeRate / summon[SimParams].forex.baseExRate) - 1.0
      val demandPullM   = (s4.avgDemandMult - 1.0) * DemandPullWeight
      val costPushM     = toDouble(s2.wageGrowth) * CostPushWeight
      val rawImportPush = Math.max(0.0, exDev) * toDouble(summon[SimParams].forex.importPropensity) * ImportPushWeight
      val importPushM   = Math.min(rawImportPush, toDouble(summon[SimParams].openEcon.importPushCap))
      val techDeflM     = toDouble(s7.autoR) * AutoDeflation + toDouble(s7.hybR) * HybridDeflation
      val rawMonthly    = demandPullM + costPushM + importPushM - techDeflM
      val flooredM      = softFloor(rawMonthly)
      val baseAnnual    = toDouble(world.inflation) * (1.0 - SmoothingLambda) + (flooredM * 12.0) * SmoothingLambda
      val totalInfl     = toDouble(s7.newInfl)
      val markupAnnual  = toDouble(s5.markupInflation)
      val unemp         = 1.0 - s2.employed.toDouble / world.totalPopulation.toDouble

      println(
        f"m=$month%2d u=${unemp * 100.0}%.2f%% pi=${totalInfl * 100.0}%.2f%% wage=${toDouble(s2.newWage)}%.0f wg=${toDouble(s2.wageGrowth) * 100.0}%.2f%% demand=${s4.avgDemandMult}%.3f markup=${markupAnnual * 100.0}%.2f%%",
      )
      println(
        f"  channels monthly: demand=${demandPullM * 100.0}%.2fpp cost=${costPushM * 100.0}%.2fpp import=${importPushM * 100.0}%.2fpp tech=-${techDeflM * 100.0}%.2fpp raw=${rawMonthly * 100.0}%.2fpp floor=${flooredM * 100.0}%.2fpp",
      )
      println(
        f"  annualized: base=${baseAnnual * 100.0}%.2f%% markup=${markupAnnual * 100.0}%.2f%% total=${totalInfl * 100.0}%.2f%% exDev=${exDev * 100.0}%.2f%% importCost=${toDouble(world.external.gvc.importCostIndex)}%.3f commodity=${toDouble(world.external.gvc.commodityPriceIndex)}%.3f",
      )
      println(s"  top pressure: ${topPressures(s4.sectorDemandPressure)}")

      val assembled = WorldAssemblyEconomics.compute(
        WorldAssemblyEconomics.Input(
          w = world,
          firms = firms,
          households = hhs,
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
