package sfc.engine

import sfc.config.{Config, SECTORS}
import sfc.agents.*
import sfc.types.*
import sfc.util.KahanSum.*

import scala.util.Random

object LaborMarket:

  /** Separate workers from firms that automated or went bankrupt this step.
    * Returns updated households with newly unemployed workers. */
  def separations(households: Vector[Household], prevFirms: Array[Firm],
                  newFirms: Array[Firm]): Vector[Household] =
    // Build set of firm IDs that lost workers this step
    val firmLostWorkers = (0 until newFirms.length).filter { i =>
      val prevAlive = FirmOps.isAlive(prevFirms(i))
      val newAlive = FirmOps.isAlive(newFirms(i))
      val automatedNow = newFirms(i).tech.isInstanceOf[TechState.Automated] &&
        !prevFirms(i).tech.isInstanceOf[TechState.Automated]
      val hybridReduced = (prevFirms(i).tech, newFirms(i).tech) match
        case (TechState.Traditional(w1), TechState.Hybrid(w2, _)) => w2 < w1
        case _ => false
      (!newAlive && prevAlive) || automatedNow || hybridReduced
    }.toSet

    // For firms that went to Hybrid, compute how many workers they retained
    val hybridRetained: Map[Int, Int] = newFirms.filter(f =>
      firmLostWorkers.contains(f.id.toInt) && f.tech.isInstanceOf[TechState.Hybrid]
    ).map { f =>
      val w = f.tech.asInstanceOf[TechState.Hybrid].workers
      f.id.toInt -> w
    }.toMap

    // For automated firms, only skeleton crew stays
    val automatedRetained: Map[Int, Int] = newFirms.filter(f =>
      firmLostWorkers.contains(f.id.toInt) && f.tech.isInstanceOf[TechState.Automated]
    ).map(f => f.id.toInt -> FirmOps.skeletonCrew(f)).toMap

    // Build retain sets: sort workers by (-education, -skill), take top maxRetain
    val firmToWorkers = scala.collection.mutable.Map[Int, scala.collection.mutable.ArrayBuffer[Int]]()
    for (hh, idx) <- households.zipWithIndex do
      hh.status match
        case HhStatus.Employed(firmId, _, _) if firmLostWorkers.contains(firmId.toInt) =>
          firmToWorkers.getOrElseUpdate(firmId.toInt, scala.collection.mutable.ArrayBuffer.empty) += idx
        case _ =>
    val eduRetainSets: Map[Int, Set[Int]] = firmToWorkers.map { (firmId, indices) =>
      val sorted = indices.sortBy(i => (-households(i).education, -households(i).skill))
      val maxRetain = hybridRetained.getOrElse(firmId, automatedRetained.getOrElse(firmId, 0))
      firmId -> sorted.take(maxRetain).toSet
    }.toMap

    households.zipWithIndex.map { (hh, idx) =>
      hh.status match
        case HhStatus.Employed(firmId, sectorIdx, wage) if firmLostWorkers.contains(firmId.toInt) =>
          if eduRetainSets.getOrElse(firmId.toInt, Set.empty).contains(idx) then hh
          else hh.copy(status = HhStatus.Unemployed(0), lastSectorIdx = sectorIdx)
        case _ => hh
    }

  /** Job search — unemployed households bid for open positions.
    * Matching: highest skill first fills vacancies.
    * Returns (updated households, cross-sector hire count). */
  def jobSearch(households: Vector[Household], firms: Array[Firm],
                marketWage: Double, rng: Random): (Vector[Household], Int) =
    // Compute vacancies per firm: living firms that need workers
    // O(N_hh) map build instead of O(N_firms × N_hh) nested scan
    val workerCounts = scala.collection.mutable.Map[Int, Int]().withDefaultValue(0)
    for hh <- households do
      hh.status match
        case HhStatus.Employed(fid, _, _) => workerCounts(fid.toInt) += 1
        case _ =>
    val vacancies = scala.collection.mutable.Map[Int, Int]()
    for f <- firms if FirmOps.isAlive(f) do
      val needed = FirmOps.workers(f) - workerCounts(f.id.toInt)
      if needed > 0 then vacancies(f.id.toInt) = needed

    if vacancies.isEmpty then return (households, 0)

    // Rank unemployed by effective skill (skill × (1 - healthPenalty))
    val unemployedIndices = households.indices.filter { i =>
      households(i).status.isInstanceOf[HhStatus.Unemployed]
    }.sortBy { i =>
      val hh = households(i)
      -(hh.skill * (1.0 - hh.healthPenalty))  // negative for descending sort
    }

    val result = households.toArray
    var crossSectorHires = 0

    // Pre-sort vacancy firms by sector sigma descending — O(V log V) once
    // For sector bonus, we group by sector: same-sector gets priority
    val vacancyFirmsBySector = vacancies.keys.toArray
      .groupBy(fid => firms(fid).sector.toInt)
    val vacancyFirmsByPriority = vacancies.keys.toArray
      .sortBy(fid => -SECTORS(firms(fid).sector.toInt).sigma)

    for idx <- unemployedIndices do
      if vacancies.nonEmpty then
        val hh = result(idx)
        // Fix: use lastSectorIdx (actual last employer sector) instead of hh.id % firms.length
        val prevSector = if hh.lastSectorIdx.toInt >= 0 then hh.lastSectorIdx.toInt
          else firms(hh.id % firms.length).sector.toInt

        // Try same-sector first (bonus), then fall back to global priority order
        val bestFirmId = vacancyFirmsBySector.getOrElse(prevSector, Array.empty[Int])
          .find(fid => vacancies.contains(fid))
          .orElse(vacancyFirmsByPriority.find(fid => vacancies.contains(fid)))

        bestFirmId.foreach { fid =>
          val f = firms(fid)
          val sectorMult = FirmOps.effectiveWageMult(f.sector)
          val isCrossSector = f.sector.toInt != prevSector
          // Cross-sector wage penalty when sectoral mobility is enabled
          val penalty = if Config.LmSectoralMobility && isCrossSector then
            SectoralMobility.crossSectorWagePenalty(Config.LmFrictionMatrix(prevSector)(f.sector.toInt))
          else 1.0
          val individualWage = marketWage * sectorMult * hh.skill * (1.0 - hh.healthPenalty) * penalty *
            Config.eduWagePremium(hh.education)
          if isCrossSector then crossSectorHires += 1
          result(idx) = hh.copy(
            status = HhStatus.Employed(FirmId(fid), f.sector, individualWage),
            lastSectorIdx = f.sector
          )
          val remaining = vacancies(fid) - 1
          if remaining <= 0 then vacancies.remove(fid)
          else vacancies(fid) = remaining
        }

    (result.toVector, crossSectorHires)

  /** Update wages for all employed households based on current market wage.
    * Individual wages are heterogeneous (sector × skill × health × immigrant discount)
    * but normalized so the mean employed wage = marketWage (macro consistency). */
  def updateWages(households: Vector[Household], marketWage: Double): Vector[Household] =
    // Compute raw relative wages for employed
    val rawWages = households.map { hh =>
      hh.status match
        case HhStatus.Employed(_, sectorIdx, _) =>
          val immigrantDiscount = if hh.isImmigrant && Config.ImmigEnabled then
            1.0 - Config.ImmigWageDiscount
          else 1.0
          FirmOps.effectiveWageMult(sectorIdx) * hh.skill * (1.0 - hh.healthPenalty) * immigrantDiscount *
            Config.eduWagePremium(hh.education)
        case _ => 0.0
    }
    val employed = households.indices.filter(i =>
      households(i).status.isInstanceOf[HhStatus.Employed])
    val rawMean = if employed.nonEmpty then
      employed.kahanSumBy(i => rawWages(i)) / employed.length
    else 1.0
    val scale = if rawMean > 0 then 1.0 / rawMean else 1.0

    households.zipWithIndex.map { (hh, i) =>
      hh.status match
        case HhStatus.Employed(firmId, sectorIdx, _) =>
          val newWage = marketWage * rawWages(i) * scale
          hh.copy(status = HhStatus.Employed(firmId, sectorIdx, newWage))
        case _ => hh
    }
