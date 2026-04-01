package com.boombustgroup.amorfati.diagnostics

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.economics.*
import com.boombustgroup.amorfati.init.WorldInit
import com.boombustgroup.amorfati.types.PLN

import scala.util.Random

object LaborDemandProbe:

  private case class SectorSnapshot(
      workers: Int,
      firms: Int,
      startups: Int,
      startupWorkers: Int,
  )

  private case class SectorChangeSummary(
      upsizeFirms: Int,
      totalWorkersAdded: Int,
      medianAddedWorkers: Int,
      maxAddedWorkers: Int,
  )

  private case class HiringSummary(
      firms: Int,
      upsizeCandidates: Int,
      totalDesiredGap: Int,
      medianDesiredGap: Int,
      maxDesiredGap: Int,
      totalFeasibleGap: Int,
      medianFeasibleGap: Int,
      maxFeasibleGap: Int,
      medianDesiredTarget: Int,
      maxDesiredTarget: Int,
      medianFeasibleTarget: Int,
      maxFeasibleTarget: Int,
      medianProposedAdd: Int,
      maxProposedAdd: Int,
      medianSignalMonths: Int,
      maxSignalMonths: Int,
      requiredSignalMonths: Int,
  )

  private def sectorSnapshots(firms: Vector[Firm.State])(using p: SimParams): Vector[SectorSnapshot] =
    (0 until p.sectorDefs.length)
      .map: s =>
        val sectorFirms = firms.filter(f => Firm.isAlive(f) && f.sector.toInt == s)
        val startups    = sectorFirms.filter(Firm.isInStartup)
        SectorSnapshot(
          workers = sectorFirms.map(Firm.workerCount).sum,
          firms = sectorFirms.length,
          startups = startups.length,
          startupWorkers = startups.map(Firm.workerCount).sum,
        )
      .toVector

  private def printSectorTable(label: String, prev: Vector[SectorSnapshot], next: Vector[SectorSnapshot])(using p: SimParams): Unit =
    println(label)
    p.sectorDefs.zipWithIndex.foreach { case (sec, i) =>
      val a  = prev(i)
      val b  = next(i)
      val dw = b.workers - a.workers
      val df = b.firms - a.firms
      val ds = b.startups - a.startups
      println(
        f"  ${sec.name}%-16s workers=${a.workers}%6d -> ${b.workers}%6d  dW=${dw}%+6d  firms=${a.firms}%5d -> ${b.firms}%5d  dF=${df}%+4d  startups=${a.startups}%4d -> ${b.startups}%4d  dS=${ds}%+3d  startupWorkers=${b.startupWorkers}%5d",
      )
    }

  private def sectorChangeSummaries(prev: Vector[Firm.State], next: Vector[Firm.State])(using p: SimParams): Vector[SectorChangeSummary] =
    val prevById = prev.map(f => f.id -> f).toMap
    (0 until p.sectorDefs.length)
      .map: s =>
        val deltas = next
          .filter(f => Firm.isAlive(f) && f.sector.toInt == s)
          .flatMap: f =>
            prevById
              .get(f.id)
              .map: pf =>
                Firm.workerCount(f) - Firm.workerCount(pf)
          .filter(_ > 0)
          .sorted
        val median =
          if deltas.isEmpty then 0
          else deltas(deltas.length / 2)
        SectorChangeSummary(
          upsizeFirms = deltas.length,
          totalWorkersAdded = deltas.sum,
          medianAddedWorkers = median,
          maxAddedWorkers = deltas.lastOption.getOrElse(0),
        )
      .toVector

  private def printChangeSummaries(label: String, summaries: Vector[SectorChangeSummary])(using p: SimParams): Unit =
    println(label)
    p.sectorDefs.zipWithIndex.foreach { case (sec, i) =>
      val s = summaries(i)
      println(
        f"  ${sec.name}%-16s upsizeFirms=${s.upsizeFirms}%5d  totalAdded=${s.totalWorkersAdded}%6d  medianAdded=${s.medianAddedWorkers}%3d  maxAdded=${s.maxAddedWorkers}%3d",
      )
    }

  private def hiringSummaries(world: com.boombustgroup.amorfati.engine.World, firms: Vector[Firm.State])(using p: SimParams): Vector[HiringSummary] =
    (0 until p.sectorDefs.length)
      .map: s =>
        val ds                           = firms
          .filter(f => Firm.isAlive(f) && f.sector.toInt == s)
          .map(Firm.hiringDiagnostics(_, world))
        val positiveDesired              = ds.filter(_.desiredGap > 0)
        val desiredGaps                  = positiveDesired.map(_.desiredGap).sorted
        val feasibleGaps                 = positiveDesired.map(_.feasibleGap).sorted
        val desiredTargets               = positiveDesired.map(_.desiredWorkers).sorted
        val feasibleTargets              = positiveDesired.map(_.feasibleWorkers).sorted
        val candidateAdds                = positiveDesired.map(d => d.proposedWorkers - d.workers).sorted
        val signalMonths                 = positiveDesired.map(_.signalMonths).sorted
        def median(xs: Vector[Int]): Int = if xs.isEmpty then 0 else xs(xs.length / 2)
        HiringSummary(
          firms = ds.length,
          upsizeCandidates = ds.count(_.shouldAdjust),
          totalDesiredGap = desiredGaps.sum,
          medianDesiredGap = median(desiredGaps),
          maxDesiredGap = desiredGaps.lastOption.getOrElse(0),
          totalFeasibleGap = feasibleGaps.sum,
          medianFeasibleGap = median(feasibleGaps),
          maxFeasibleGap = feasibleGaps.lastOption.getOrElse(0),
          medianDesiredTarget = median(desiredTargets),
          maxDesiredTarget = desiredTargets.lastOption.getOrElse(0),
          medianFeasibleTarget = median(feasibleTargets),
          maxFeasibleTarget = feasibleTargets.lastOption.getOrElse(0),
          medianProposedAdd = median(candidateAdds),
          maxProposedAdd = candidateAdds.lastOption.getOrElse(0),
          medianSignalMonths = median(signalMonths),
          maxSignalMonths = signalMonths.lastOption.getOrElse(0),
          requiredSignalMonths = positiveDesired.headOption.map(_.requiredSignalMonths).getOrElse(0),
        )
      .toVector

  private def printHiringSummaries(label: String, summaries: Vector[HiringSummary])(using p: SimParams): Unit =
    println(label)
    p.sectorDefs.zipWithIndex.foreach { case (sec, i) =>
      val s = summaries(i)
      println(
        f"  ${sec.name}%-16s firms=${s.firms}%5d  upsizeCandidates=${s.upsizeCandidates}%5d  desiredGap=${s.totalDesiredGap}%7d  feasibleGap=${s.totalFeasibleGap}%6d  medDesiredGap=${s.medianDesiredGap}%3d  medFeasibleGap=${s.medianFeasibleGap}%3d  medDesiredT=${s.medianDesiredTarget}%3d  medFeasibleT=${s.medianFeasibleTarget}%3d  medAdd=${s.medianProposedAdd}%3d  medSignal=${s.medianSignalMonths}%2d/${s.requiredSignalMonths}%1d",
      )
    }

  @main def runLaborDemandProbe(seed: Long = 1L, months: Int = 2): Unit =
    given SimParams = SimParams.defaults

    val init  = WorldInit.initialize(seed)
    var world = init.world
    var firms = init.firms
    var hhs   = init.households
    var banks = init.banks

    println(s"seed=$seed months=$months")

    (1 to months).foreach: month =>
      val rng       = new Random(seed * 1000 + month)
      val beforeAll = sectorSnapshots(firms)
      val hiring    = hiringSummaries(world, firms)

      val fiscal = FiscalConstraintEconomics.compute(world, banks)
      val s1     = FiscalConstraintEconomics.toOutput(fiscal)
      val labor  = LaborEconomics.compute(world, firms, hhs, s1)
      val s2Pre  = LaborEconomics.Output(
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
      val s3     = HouseholdIncomeEconomics.compute(world, firms, hhs, banks, s1.lendingBaseRate, s1.resWage, s2Pre.newWage, rng)
      val s4     = DemandEconomics.compute(DemandEconomics.Input(world, s2Pre.employed, s2Pre.living, s3.domesticCons))
      val s5     = FirmEconomics.runStep(world, firms, hhs, banks, s1, s2Pre, s3, s4, rng)
      val s2Post = LaborEconomics.reconcilePostFirmStep(world, s1, s2Pre, s5.ioFirms.filter(Firm.isAlive), s5.households)
      val s6     = HouseholdFinancialEconomics.compute(world, s1.m, s2Post.employed, s3.hhAgg, rng)
      val s7     = PriceEquityEconomics.compute(
        PriceEquityEconomics.Input(
          world,
          s1.m,
          s2Post.newWage,
          s2Post.employed,
          s2Post.wageGrowth,
          s3.domesticCons,
          s4.govPurchases,
          s4.avgDemandMult,
          s4.sectorMults,
          banks,
          s5,
        ),
        rng,
      )
      val s8     = OpenEconEconomics.runStep(OpenEconEconomics.StepInput(world, s1, s2Post, s3, s4, s5, s6, s7, banks, rng))
      val s9     = BankingEconomics.runStep(BankingEconomics.StepInput(world, s1, s2Post, s3, s4, s5, s6, s7, s8, banks, rng))

      val afterFirm = sectorSnapshots(s5.ioFirms)
      val changes   = sectorChangeSummaries(firms, s5.ioFirms)
      println(
        s"m=$month preLaborDemand=${labor.laborDemand} postFirmDemand=${s5.ioFirms.filter(Firm.isAlive).map(Firm.workerCount).sum} employedPre=${labor.employed}",
      )
      printHiringSummaries("  pre-step hiring diagnostics:", hiring)
      printSectorTable("  sector deltas after FirmEconomics:", beforeAll, afterFirm)
      printChangeSummaries("  firm-level positive worker changes:", changes)

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
          laborOutput = s2Post,
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
