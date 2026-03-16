package com.boombustgroup.amorfati.engine.markets

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.mechanisms.SectoralMobility
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.amorfati.util.KahanSum.*

import scala.util.Random

/** Labor market: wage determination, separations, job search, wage updating.
  *
  * Wage adjustment via excess demand (Phillips curve); individual wages
  * heterogeneous by sector, skill, health, education, and immigrant status,
  * normalized so mean employed wage equals the macro market wage.
  *
  * Separations: workers displaced by automation (Traditional→Hybrid/Automated)
  * or firm bankruptcy, with education-based retention priority. Job search:
  * skill-ranked matching with same-sector priority and cross-sector friction
  * penalty (when sectoral mobility enabled).
  *
  * Calibration: GUS BAEL (Labor Force Survey) 2024, NBP wage statistics.
  */
object LaborMarket:

  /** Aggregate wage clearing result. */
  case class WageResult(wage: PLN, employed: Int)

  /** Job search matching result. */
  case class JobSearchResult(households: Vector[Household.State], crossSectorHires: Int)

  // --- Aggregate wage clearing ---

  /** Logistic labor supply curve: fraction of population willing to work at
    * given wage. Steepness controlled by p.household.laborSupplySteepness.
    */
  private def laborSupplyRatio(wage: PLN, resWage: PLN)(using p: SimParams): Ratio =
    val x = p.household.laborSupplySteepness * (wage / resWage - 1.0)
    Ratio(1.0 / (1.0 + Math.exp(-x)))

  /** Aggregate wage clearing: adjust market wage via excess demand, then
    * compute employment. New wage = max(reservationWage, prevWage × (1 +
    * excessDemand × adjSpeed)).
    */
  def updateLaborMarket(prevWage: PLN, resWage: PLN, laborDemand: Int, totalPopulation: Int)(using
      p: SimParams,
  ): WageResult =
    val supplyAtPrev = (totalPopulation * laborSupplyRatio(prevWage, resWage).toDouble).toInt
    val excessDemand = (laborDemand - supplyAtPrev).toDouble / totalPopulation
    val wageGrowth   = Ratio(excessDemand * p.household.wageAdjSpeed.toDouble)
    val newWage      = resWage.max(prevWage * (1.0 + wageGrowth.toDouble))
    val newSupply    = (totalPopulation * laborSupplyRatio(newWage, resWage).toDouble).toInt
    val employed     = Math.min(laborDemand, newSupply)
    WageResult(newWage, employed)

  // --- Separations ---

  /** Separate workers from firms that automated or went bankrupt this step.
    * Education-ranked retention: highest-educated workers kept first when firm
    * transitions to Hybrid or Automated (skeleton crew). Returns updated
    * households with newly unemployed workers.
    */
  def separations(
      households: Vector[Household.State],
      prevFirms: Vector[Firm.State],
      newFirms: Vector[Firm.State],
  )(using SimParams): Vector[Household.State] =
    val lostFirms = firmsThatLostWorkers(prevFirms, newFirms)
    if lostFirms.isEmpty then households
    else
      val counts   = retainCounts(newFirms, lostFirms)
      val retained = retainSets(households, lostFirms, counts)
      applySeparations(households, lostFirms, retained)

  // --- Job search ---

  /** Job search: unemployed households bid for open positions. Matching:
    * highest effective skill (skill × (1 - healthPenalty)) fills vacancies
    * first. Same-sector gets priority; cross-sector hires incur friction
    * penalty when sectoral mobility is enabled.
    */
  def jobSearch(
      households: Vector[Household.State],
      firms: Vector[Firm.State],
      marketWage: PLN,
      @scala.annotation.unused rng: Random,
  )(using p: SimParams): JobSearchResult =
    val vacancies = computeVacancies(households, firms)
    if vacancies.isEmpty then JobSearchResult(households, 0)
    else matchWorkers(households, firms, vacancies, marketWage)

  // --- Wage updating ---

  /** Update wages for all employed households based on current market wage.
    * Individual wages are heterogeneous (sector × skill × health × immigrant
    * discount × education premium) but normalized so mean employed wage =
    * marketWage (macro consistency).
    */
  def updateWages(households: Vector[Household.State], firms: Vector[Firm.State], marketWage: PLN)(using
      p: SimParams,
  ): Vector[Household.State] =
    val rawWages = households.map(rawRelativeWage(_, firms))
    val rawMean  = employedMeanRawWage(households, rawWages)
    val scale    = if rawMean > Ratio(0.0) then Ratio(1.0 / rawMean.toDouble) else Ratio(1.0)
    applyNormalizedWages(households, rawWages, marketWage, scale)

  // --- Shared helpers ---

  /** Effective skill: skill × (1 - healthPenalty). */
  private def effectiveSkill(hh: Household.State): Double =
    hh.skill.toDouble * (1.0 - hh.healthPenalty.toDouble)

  /** Infer sector for an unemployed household: last sector if known, else
    * fallback based on household id.
    */
  private def effectivePrevSector(hh: Household.State, firms: Vector[Firm.State]): SectorIdx =
    if hh.lastSectorIdx.toInt >= 0 then hh.lastSectorIdx
    else firms(hh.id.toInt % firms.length).sector

  // --- Separation helpers ---

  /** Identify firms that lost workers: bankruptcy, automation, or hybrid
    * reduction.
    */
  private def firmsThatLostWorkers(
      prevFirms: Vector[Firm.State],
      newFirms: Vector[Firm.State],
  ): Set[FirmId] =
    newFirms.indices.collect {
      case i if didLoseWorkers(prevFirms(i), newFirms(i)) => newFirms(i).id
    }.toSet

  private def didLoseWorkers(prev: Firm.State, curr: Firm.State): Boolean =
    val wentBankrupt   = Firm.isAlive(prev) && !Firm.isAlive(curr)
    val newlyAutomated = (prev.tech, curr.tech) match
      case (_: TechState.Automated, _) => false
      case (_, _: TechState.Automated) => true
      case _                           => false
    val hybridReduced  = (prev.tech, curr.tech) match
      case (TechState.Traditional(w1), TechState.Hybrid(w2, _)) => w2 < w1
      case _                                                    => false
    wentBankrupt || newlyAutomated || hybridReduced

  /** How many workers each affected firm retains (hybrid crew or skeleton
    * crew).
    */
  private def retainCounts(
      newFirms: Vector[Firm.State],
      lostFirms: Set[FirmId],
  )(using SimParams): Map[FirmId, Int] =
    newFirms.collect {
      case f if lostFirms.contains(f.id) =>
        f.tech match
          case TechState.Hybrid(w, _) => f.id -> w
          case _: TechState.Automated => f.id -> Firm.skeletonCrew(f)
          case _                      => f.id -> 0
    }.toMap

  /** Build retain sets: when SBTC enabled, low-routineness (cognitive) workers
    * retained first; otherwise education-ranked.
    */
  private def retainSets(
      households: Vector[Household.State],
      lostFirms: Set[FirmId],
      counts: Map[FirmId, Int],
  )(using p: SimParams): Map[FirmId, Set[Int]] =
    households.zipWithIndex
      .flatMap: (hh, idx) =>
        hh.status match
          case HhStatus.Employed(firmId, _, _) if lostFirms.contains(firmId) => Some(firmId -> idx)
          case _                                                             => None
      .groupMap(_._1)(_._2)
      .map: (firmId, indices) =>
        val sorted    =
          if p.flags.sbtc then
            // Low routineness = cognitive = retained; high routineness = routine = displaced
            indices.sortBy(i => (households(i).taskRoutineness.toDouble, -households(i).skill.toDouble))
          else indices.sortBy(i => (-households(i).education, -households(i).skill.toDouble))
        val maxRetain = counts.getOrElse(firmId, 0)
        firmId -> sorted.take(maxRetain).toSet

  /** Apply separations: retained workers stay, others become unemployed. */
  private def applySeparations(
      households: Vector[Household.State],
      lostFirms: Set[FirmId],
      retained: Map[FirmId, Set[Int]],
  ): Vector[Household.State] =
    households.zipWithIndex.map: (hh, idx) =>
      hh.status match
        case HhStatus.Employed(firmId, sectorIdx, _) if lostFirms.contains(firmId) =>
          if retained.getOrElse(firmId, Set.empty).contains(idx) then hh
          else hh.copy(status = HhStatus.Unemployed(0), lastSectorIdx = sectorIdx)
        case _                                                                     => hh

  // --- Vacancy & matching helpers ---

  /** Build vacancy map: firmId → number of open positions. */
  private def computeVacancies(
      households: Vector[Household.State],
      firms: Vector[Firm.State],
  )(using SimParams): Map[FirmId, Int] =
    val workerCounts: Map[FirmId, Int] = households
      .flatMap: hh =>
        hh.status match
          case HhStatus.Employed(fid, _, _) => Some(fid)
          case _                            => None
      .groupMapReduce(identity)(_ => 1)(_ + _)
    firms
      .filter(Firm.isAlive)
      .flatMap: f =>
        val needed = Firm.workerCount(f) - workerCounts.getOrElse(f.id, 0)
        if needed > 0 then Some(f.id -> needed) else None
      .toMap

  /** Accumulator for the pure-FP matching fold. */
  private case class MatchState(
      hired: Map[Int, Household.State],
      vacancies: Map[FirmId, Int],
      crossSectorHires: Int,
  )

  /** Core matching: rank unemployed by effective skill, assign to vacancies
    * with same-sector priority and cross-sector friction penalty.
    */
  private def matchWorkers(
      households: Vector[Household.State],
      firms: Vector[Firm.State],
      vacancies: Map[FirmId, Int],
      marketWage: PLN,
  )(using p: SimParams): JobSearchResult =
    val ranked          = rankUnemployed(households)
    val firmsBySector   = vacancies.keys.groupBy(fid => firms(fid.toInt).sector)
    val firmsByPriority = vacancies.keys.toVector.sortBy(fid => -p.sectorDefs(firms(fid.toInt).sector.toInt).sigma)

    val init   = MatchState(Map.empty, vacancies, 0)
    val result = ranked.foldLeft(init): (st, idx) =>
      if st.vacancies.isEmpty then st
      else tryHire(st, idx, households(idx), firms, firmsBySector, firmsByPriority, marketWage)

    val updated = households.zipWithIndex.map((hh, i) => result.hired.getOrElse(i, hh))
    JobSearchResult(updated, result.crossSectorHires)

  /** Rank unemployed households by effective skill descending. */
  private def rankUnemployed(households: Vector[Household.State]): Vector[Int] =
    households.zipWithIndex
      .flatMap: (hh, idx) =>
        hh.status match
          case _: HhStatus.Unemployed => Some(idx)
          case _                      => None
      .sortBy(i => -effectiveSkill(households(i)))

  /** Try hiring one unemployed household into a vacancy. */
  private def tryHire(
      st: MatchState,
      idx: Int,
      hh: Household.State,
      firms: Vector[Firm.State],
      firmsBySector: Map[SectorIdx, Iterable[FirmId]],
      firmsByPriority: Vector[FirmId],
      marketWage: PLN,
  )(using p: SimParams): MatchState =
    val prevSector = effectivePrevSector(hh, firms)
    val bestFirm   = firmsBySector
      .getOrElse(prevSector, Iterable.empty)
      .find(fid => st.vacancies.contains(fid))
      .orElse(firmsByPriority.find(fid => st.vacancies.contains(fid)))
    bestFirm match
      case None      => st
      case Some(fid) => hire(st, idx, hh, firms(fid.toInt), fid, prevSector, marketWage)

  /** Execute a hire: update household, decrement vacancy, count cross-sector.
    */
  private def hire(
      st: MatchState,
      idx: Int,
      hh: Household.State,
      firm: Firm.State,
      firmId: FirmId,
      prevSector: SectorIdx,
      marketWage: PLN,
  )(using p: SimParams): MatchState =
    val isCrossSector = firm.sector.toInt != prevSector.toInt
    val wage          = computeHireWage(hh, firm, marketWage, prevSector, isCrossSector)
    val newHh         = hh.copy(
      status = HhStatus.Employed(firmId, firm.sector, wage),
      lastSectorIdx = firm.sector,
    )
    val remaining     = st.vacancies(firmId) - 1
    val newVacancies  = if remaining <= 0 then st.vacancies - firmId else st.vacancies.updated(firmId, remaining)
    MatchState(st.hired.updated(idx, newHh), newVacancies, st.crossSectorHires + (if isCrossSector then 1 else 0))

  /** Compute individual wage for a new hire. */
  private def computeHireWage(
      hh: Household.State,
      firm: Firm.State,
      marketWage: PLN,
      prevSector: SectorIdx,
      isCrossSector: Boolean,
  )(using p: SimParams): PLN =
    val sectorMult = Firm.effectiveWageMult(firm.sector).toDouble
    val penalty    =
      if p.flags.sectoralMobility && isCrossSector
      then SectoralMobility.crossSectorWagePenalty(p.labor.frictionMatrix(prevSector.toInt)(firm.sector.toInt))
      else 1.0
    marketWage * (sectorMult * effectiveSkill(hh) * penalty * p.social.eduWagePremium(hh.education))

  // --- Wage helpers ---

  /** Raw relative wage weight for a household (unnormalized). */
  private def rawRelativeWage(hh: Household.State, firms: Vector[Firm.State])(using p: SimParams): Ratio =
    hh.status match
      case HhStatus.Employed(firmId, sectorIdx, _) =>
        val immigrantDiscount =
          if hh.isImmigrant && p.flags.immigration then 1.0 - p.immigration.wageDiscount.toDouble
          else 1.0
        val aiComplement      = aiComplementFactor(hh, firms(firmId.toInt))
        Ratio(
          Firm.effectiveWageMult(sectorIdx).toDouble * effectiveSkill(hh) * immigrantDiscount *
            p.social.eduWagePremium(hh.education) * aiComplement,
        )
      case _                                       => Ratio(0.0)

  /** AI-complement wage premium: cognitive workers in automated/hybrid firms
    * get a wage boost (1 - routineness) × complementPremium. Routine workers
    * get no boost. Acemoglu & Restrepo 2020.
    */
  private def aiComplementFactor(hh: Household.State, firm: Firm.State)(using p: SimParams): Double =
    if !p.flags.sbtc then 1.0
    else
      firm.tech match
        case _: TechState.Automated | _: TechState.Hybrid =>
          1.0 + p.labor.sbtcComplementPremium.toDouble * (1.0 - hh.taskRoutineness.toDouble)
        case _                                            => 1.0

  /** Mean raw wage across employed households (Kahan summation). */
  private def employedMeanRawWage(
      households: Vector[Household.State],
      rawWages: Vector[Ratio],
  ): Ratio =
    val employedIndices = households.indices.flatMap: i =>
      households(i).status match
        case _: HhStatus.Employed => Some(i)
        case _                    => None
    if employedIndices.nonEmpty
    then Ratio(employedIndices.kahanSumBy(i => rawWages(i).toDouble) / employedIndices.length)
    else Ratio(1.0)

  /** Apply normalized wages: each employed gets marketWage × (rawWeight ×
    * scale).
    */
  private def applyNormalizedWages(
      households: Vector[Household.State],
      rawWages: Vector[Ratio],
      marketWage: PLN,
      scale: Ratio,
  ): Vector[Household.State] =
    households.zipWithIndex.map: (hh, i) =>
      hh.status match
        case HhStatus.Employed(firmId, sectorIdx, _) =>
          hh.copy(status = HhStatus.Employed(firmId, sectorIdx, marketWage * (rawWages(i) * scale)))
        case _                                       => hh
