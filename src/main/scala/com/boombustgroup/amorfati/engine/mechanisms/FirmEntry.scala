package com.boombustgroup.amorfati.engine.mechanisms

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.agents.Region
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

import scala.util.Random

/** Endogenous firm entry: recycles bankrupt firm slots by spawning new entrants
  * whose sector choice is weighted by relative profitability signals across
  * sectors. Firms in more profitable sectors attract more entrants, subject to
  * sector-specific regulatory barriers (GUS CEIDG/KRS 2024 calibration).
  *
  * New entrants may be AI-native (hybrid technology with partial automation)
  * when the economy-wide technology adoption rate exceeds a maturity threshold,
  * reflecting the observed pattern of digitally-born startups in mature digital
  * ecosystems (OECD Digital Economy Outlook 2023).
  *
  * Entry probability per vacant slot: entryRate × sectorBarrier × profitSignal,
  * where profitSignal is normalized relative sector cash-to-global-average.
  */
object FirmEntry:

  // ---- Calibration constants ----
  private val ProfitClampMin      = -1.0 // floor for normalized sector profit signal
  private val ProfitClampMax      = 2.0  // ceiling for normalized sector profit signal
  private val MinSectorWeight     = 0.01 // floor so no sector has zero entry probability
  private val MaxEntrantSize      = 10   // upper bound on initial workforce draw
  private val MaxNeighbors        = 6    // network degree for new entrants
  private val AiNativeMinDr       = 0.50 // digital readiness floor for AI-native entrants
  private val AiNativeMaxDr       = 0.90 // digital readiness ceiling for AI-native entrants
  private val ConventionalDrNoise = 0.10 // std dev for conventional entrant DR draw
  private val ConventionalDrFloor = 0.02 // minimum DR for conventional entrant
  private val ConventionalDrCap   = 0.30 // maximum DR for conventional entrant
  private val MinAiProductivity   = 0.5  // AI productivity floor for hybrid entrants
  private val AiProductivityRange = 0.3  // AI productivity range above floor
  private val HybridMinWorkers    = 1    // minimum viable hybrid workforce

  case class Result(
      firms: Vector[Firm.State], // post-entry firm population (same length as input)
      births: Int,               // number of new entrants spawned this step
  )

  @computationBoundary
  def process(
      firms: Vector[Firm.State],
      automationRatio: Share,
      hybridRatio: Share,
      rng: Random,
  )(using p: SimParams): Result =
    import ComputationBoundary.toDouble
    val living        = firms.filter(Firm.isAlive)
    val profitSignals = computeProfitSignals(living)
    val sectorWeights = computeSectorWeights(profitSignals)
    val totalWeight   = sectorWeights.sum

    val totalAdoption = automationRatio + hybridRatio
    val livingIds     = living.map(_.id.toInt)
    var births        = 0

    val result = firms.map: f =>
      if !Firm.isAlive(f) then
        val slotSector = f.sector.toInt
        val entryProb  = toDouble(p.firm.entryRate) * toDouble(p.firm.entrySectorBarriers(slotSector)) *
          Math.max(0.0, 1.0 + profitSignals(slotSector) * toDouble(p.firm.entryProfitSens))
        if rng.nextDouble() < entryProb then
          births += 1
          createNewFirm(f.id, totalWeight, sectorWeights, totalAdoption, livingIds, rng)
        else f
      else f
    Result(result, births)

  @computationBoundary
  private def computeProfitSignals(living: Vector[Firm.State])(using p: SimParams): Vector[Double] =
    import ComputationBoundary.toDouble
    val bySector      = living.groupBy(_.sector.toInt)
    val sectorAvgCash = p.sectorDefs.indices.map: s =>
      bySector.get(s).fold(0.0)(fs => fs.map(f => toDouble(f.cash)).sum / fs.length)
    val globalAvgCash = if living.nonEmpty then living.map(f => toDouble(f.cash)).sum / living.length else 1.0
    sectorAvgCash
      .map: c =>
        Math.max(ProfitClampMin, Math.min(ProfitClampMax, (c - globalAvgCash) / Math.max(1.0, Math.abs(globalAvgCash))))
      .toVector

  @computationBoundary
  private def computeSectorWeights(profitSignals: Vector[Double])(using p: SimParams): Vector[Double] =
    import ComputationBoundary.toDouble
    p.sectorDefs.indices
      .map: s =>
        Math.max(MinSectorWeight, (1.0 + profitSignals(s) * toDouble(p.firm.entryProfitSens)) * toDouble(p.firm.entrySectorBarriers(s)))
      .toVector

  @computationBoundary
  private def createNewFirm(
      slotId: FirmId,
      totalWeight: Double,
      sectorWeights: Vector[Double],
      totalAdoption: Share,
      livingIds: Vector[Int],
      rng: Random,
  )(using p: SimParams): Firm.State =
    val newSector    = pickSector(totalWeight, sectorWeights, rng)
    val firmSize     = Math.max(1, rng.between(1, MaxEntrantSize))
    val sizeMult     = firmSize.toDouble / p.pop.workersPerFirm
    val isAiNative   = totalAdoption > p.firm.entryAiThreshold &&
      p.firm.entryAiProb.sampleBelow(rng)
    val tech         = chooseTechnology(isAiNative, firmSize, newSector, rng)
    val dr           = drawDigitalReadiness(isAiNative, newSector, rng)
    val newNeighbors = assignNeighbors(livingIds, rng)
    val newBankId    = Banking.assignBank(SectorIdx(newSector), Banking.DefaultConfigs, rng)

    Firm.State(
      id = slotId,
      cash = p.firm.entryStartupCash * Multiplier(sizeMult),
      debt = PLN.Zero,
      tech = tech,
      riskProfile = Share(rng.between(0.1, 0.9)),
      innovationCostFactor = rng.between(0.8, 1.5),
      digitalReadiness = dr,
      sector = SectorIdx(newSector),
      neighbors = newNeighbors,
      bankId = newBankId,
      equityRaised = PLN.Zero,
      initialSize = firmSize,
      capitalStock = initCapitalStock(firmSize, newSector),
      bondDebt = PLN.Zero,
      foreignOwned = p.flags.fdi && p.fdi.foreignShares(newSector).sampleBelow(rng),
      inventory = initInventory(firmSize, newSector),
      greenCapital = initGreenCapital(firmSize, newSector),
      accumulatedLoss = PLN.Zero,
      markup = p.pricing.baseMarkup,
      region = Region.cdfSample(rng),
    )

  /** Select technology regime: AI-native entrants start as Hybrid with partial
    * automation; conventional entrants use Traditional (workers hired via labor
    * market in subsequent steps).
    */
  @computationBoundary
  private def chooseTechnology(isAiNative: Boolean, firmSize: Int, sector: Int, rng: Random)(using
      p: SimParams,
  ): TechState =
    import ComputationBoundary.toDouble
    if isAiNative then
      val hybridWorkers = Math.max(HybridMinWorkers, (firmSize * toDouble(p.sectorDefs(sector).hybridRetainFrac)).toInt)
      TechState.Hybrid(hybridWorkers, MinAiProductivity + rng.nextDouble() * AiProductivityRange)
    else TechState.Traditional(firmSize)

  /** Draw digital readiness score: AI-native firms get high DR (0.50-0.90);
    * conventional entrants draw from sector baseline with Gaussian noise,
    * clamped to the feasible range for non-digital firms.
    */
  @computationBoundary
  private def drawDigitalReadiness(isAiNative: Boolean, sector: Int, rng: Random)(using p: SimParams): Share =
    import ComputationBoundary.toDouble
    if isAiNative then Share(rng.between(AiNativeMinDr, AiNativeMaxDr))
    else
      Share(toDouble(p.sectorDefs(sector).baseDigitalReadiness) + rng.nextGaussian() * ConventionalDrNoise)
        .clamp(Share(ConventionalDrFloor), Share(ConventionalDrCap))

  /** Assign network neighbors from the living firm population. */
  private def assignNeighbors(livingIds: Vector[Int], rng: Random): Vector[FirmId] =
    val nNeighbors = Math.min(MaxNeighbors, livingIds.length)
    if nNeighbors > 0 then rng.shuffle(livingIds.toList).take(nNeighbors).map(FirmId(_)).toVector
    else Vector.empty[FirmId]

  /** Initial physical capital stock from sector-specific capital-labor ratio.
    */
  @computationBoundary
  private def initCapitalStock(firmSize: Int, sector: Int)(using p: SimParams): PLN =
    import ComputationBoundary.toDouble
    if p.flags.physCap then PLN(toDouble(p.capital.klRatios(sector)) * firmSize)
    else PLN.Zero

  /** Initial inventory from sector target ratio, scaled to firm capacity. */
  private def initInventory(firmSize: Int, sector: Int)(using p: SimParams): PLN =
    if p.flags.inventory then
      val cap = p.firm.baseRevenue * Multiplier(firmSize.toDouble / p.pop.workersPerFirm) *
        p.sectorDefs(sector).revenueMultiplier
      cap * p.capital.inventoryTargetRatios(sector) * p.capital.inventoryInitRatio
    else PLN.Zero

  /** Initial green capital stock from sector-specific green K/L ratio. */
  @computationBoundary
  private def initGreenCapital(firmSize: Int, sector: Int)(using p: SimParams): PLN =
    import ComputationBoundary.toDouble
    if p.flags.energy then PLN(toDouble(p.climate.greenKLRatios(sector)) * firmSize) * p.climate.greenInitRatio
    else PLN.Zero

  private def pickSector(totalWeight: Double, sectorWeights: Vector[Double], rng: Random): Int =
    val roll   = rng.nextDouble() * totalWeight
    val cumuls = sectorWeights.scanLeft(0.0)(_ + _).drop(1)
    cumuls.indexWhere(_ > roll) match
      case -1 => sectorWeights.length - 1
      case i  => i
