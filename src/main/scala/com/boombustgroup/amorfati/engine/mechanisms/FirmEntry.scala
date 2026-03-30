package com.boombustgroup.amorfati.engine.mechanisms

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.agents.Region
import com.boombustgroup.amorfati.config.{FirmSizeDistribution, SimParams}
import com.boombustgroup.amorfati.types.*

import scala.util.Random

  /** Endogenous firm entry: replaces a share of bankrupt firm slots and may also
    * append net new firms when unemployment exceeds NAIRU. Sector choice is
    * weighted by relative profitability signals across sectors. Firms in more
    * profitable sectors attract more entrants, subject to sector-specific
    * regulatory barriers (GUS CEIDG/KRS 2024 calibration).
  *
  * New entrants may be AI-native (hybrid technology with partial automation)
  * when the economy-wide technology adoption rate exceeds a maturity threshold,
  * reflecting the observed pattern of digitally-born startups in mature digital
  * ecosystems (OECD Digital Economy Outlook 2023).
  *
  * Expansionary net entry activates only above NAIRU. Replacement entry is a
  * separate, faster channel intended to rebuild the firm base after exit waves.
    */
object FirmEntry:

  // ---- Calibration constants ----
  private val ProfitClampMin      = -1.0 // floor for normalized sector profit signal
  private val ProfitClampMax      = 2.0  // ceiling for normalized sector profit signal
  private val MinSectorWeight     = 0.01 // floor so no sector has zero entry probability
  private val MaxNeighbors        = 6    // network degree for new entrants
  private val AiNativeMinDr       = 0.50 // digital readiness floor for AI-native entrants
  private val AiNativeMaxDr       = 0.90 // digital readiness ceiling for AI-native entrants
  private val ConventionalDrNoise = 0.10 // std dev for conventional entrant DR draw
  private val ConventionalDrFloor = 0.02 // minimum DR for conventional entrant
  private val ConventionalDrCap   = 0.30 // maximum DR for conventional entrant
  private val MinAiProductivity   = 0.5  // AI productivity floor for hybrid entrants
  private val AiProductivityRange = 0.3  // AI productivity range above floor
  private val HybridMinWorkers    = 1    // minimum viable hybrid workforce
  private val StartupMonths       = 4    // short entrant startup ramp-up window
  private val StartupMinWorkers   = 1    // minimum planned startup team
  private val StartupMaxWorkers   = 4    // entrant startup team cap

  case class Result(
      firms: Vector[Firm.State], // post-entry firm population (may be longer than input if net creation occurred)
      births: Int,               // total new entrants (recycled + net new)
      netBirths: Int,            // net new firms appended to vector
      entrantIds: Set[FirmId],   // firms born this step (recycled + net new)
  )

  private case class EntryConditions(
      unemploymentRate: Double,
      aggregateHiringSlack: Double,
      inflation: Double,
      expectedInflation: Double,
  )

  def process(
      firms: Vector[Firm.State],
      automationRatio: Share,
      hybridRatio: Share,
      unemploymentRate: Double,
      rng: Random,
  )(using p: SimParams): Result =
    process(firms, automationRatio, hybridRatio, unemploymentRate, 1.0, Rate.Zero, Rate.Zero, rng)

  @boundaryEscape
  def process(
      firms: Vector[Firm.State],
      automationRatio: Share,
      hybridRatio: Share,
      unemploymentRate: Double,
      aggregateHiringSlack: Double = 1.0,
      inflation: Rate = Rate.Zero,
      expectedInflation: Rate = Rate.Zero,
      rng: Random,
  )(using p: SimParams): Result =
    val living        = firms.filter(Firm.isAlive)
    val profitSignals = computeProfitSignals(living)
    val sectorWeights = computeSectorWeights(profitSignals)
    val totalWeight   = sectorWeights.sum

    val totalAdoption              = automationRatio + hybridRatio
    val livingIds                  = living.map(_.id.toInt)
    val (recycledFirms, recycledIds) =
      recycleDeadSlots(firms, totalAdoption, livingIds, sectorWeights, totalWeight, rng)
    val recycledBirths             = recycledIds.size

    val conditions = EntryConditions(
      unemploymentRate = unemploymentRate,
      aggregateHiringSlack = aggregateHiringSlack,
      inflation = ComputationBoundary.toDouble(inflation),
      expectedInflation = ComputationBoundary.toDouble(expectedInflation),
    )
    val (finalFirms, netBirths, netIds) =
      netCreation(recycledFirms, living.length, conditions, totalAdoption, livingIds, sectorWeights, totalWeight, rng)
    Result(finalFirms, recycledBirths + netBirths, netBirths, recycledIds ++ netIds)

  @boundaryEscape
  private def recycleDeadSlots(
      firms: Vector[Firm.State],
      totalAdoption: Share,
      livingIds: Vector[Int],
      sectorWeights: Vector[Double],
      totalWeight: Double,
      rng: Random,
  )(using p: SimParams): (Vector[Firm.State], Set[FirmId]) =
    import ComputationBoundary.toDouble
    val deadSlots = firms.filterNot(Firm.isAlive)
    if deadSlots.isEmpty then return (firms, Set.empty)

    val rawCount = Math.ceil(deadSlots.length * toDouble(p.firm.replacementEntryRate)).toInt
    val boundedCount =
      Math.max(
        if deadSlots.nonEmpty then p.firm.replacementEntryMinMonthly else 0,
        Math.min(rawCount, p.firm.replacementEntryMaxMonthly),
      )
    val replacementCount = Math.min(deadSlots.length, boundedCount)
    if replacementCount <= 0 then return (firms, Set.empty)

    val replacementIds = rng.shuffle(deadSlots.map(_.id).toList).take(replacementCount).toSet
    (
      firms.map: f =>
        if !Firm.isAlive(f) && replacementIds.contains(f.id) then createNewFirm(f.id, totalWeight, sectorWeights, totalAdoption, livingIds, rng)
        else f,
      replacementIds,
    )

  /** Append net new firms when unemployment exceeds NAIRU. Birth count is
    * proportional to the gap, capped to prevent vector explosion. New firms get
    * sequential FirmIds starting at `firms.length` to maintain the FirmId ==
    * vector index invariant.
    */
  @boundaryEscape
  private def netCreation(
      firms: Vector[Firm.State],
      livingCount: Int,
      conditions: EntryConditions,
      totalAdoption: Share,
      livingIds: Vector[Int],
      sectorWeights: Vector[Double],
      totalWeight: Double,
      rng: Random,
  )(using p: SimParams): (Vector[Firm.State], Int, Set[FirmId]) =
    val signal   = expansionaryEntrySignal(conditions)
    if signal <= 0.0 then return (firms, 0, Set.empty)
    val rawCount = Math.ceil(signal * ComputationBoundary.toDouble(p.firm.netEntryRate) * livingCount).toInt
    val count    = Math.max(0, Math.min(rawCount, p.firm.netEntryMaxMonthly))
    if count <= 0 then return (firms, 0, Set.empty)

    val baseId   = firms.length
    val newFirms = (0 until count).map: i =>
      createNewFirm(FirmId(baseId + i), totalWeight, sectorWeights, totalAdoption, livingIds, rng)
    (firms ++ newFirms, count, newFirms.map(_.id).toSet)

  private def expansionaryEntrySignal(c: EntryConditions)(using p: SimParams): Double =
    val nairu           = ComputationBoundary.toDouble(p.monetary.nairu)
    val laborSlack      = Math.max(0.0, c.unemploymentRate - nairu)
    if laborSlack <= 0.0 then return 0.0
    val nominalSignal   =
      if c.inflation < 0.0 && c.expectedInflation < 0.0 then 0.0
      else if c.inflation < 0.0 || c.expectedInflation < 0.0 then 0.35
      else 1.0
    val hiringSignal    = c.aggregateHiringSlack.max(0.0).min(1.0)
    laborSlack * nominalSignal * hiringSignal

  @boundaryEscape
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

  @boundaryEscape
  private def computeSectorWeights(profitSignals: Vector[Double])(using p: SimParams): Vector[Double] =
    import ComputationBoundary.toDouble
    p.sectorDefs.indices
      .map: s =>
        Math.max(MinSectorWeight, (1.0 + profitSignals(s) * toDouble(p.firm.entryProfitSens)) * toDouble(p.firm.entrySectorBarriers(s)))
      .toVector

  @boundaryEscape
  private def createNewFirm(
      slotId: FirmId,
      totalWeight: Double,
      sectorWeights: Vector[Double],
      totalAdoption: Share,
      livingIds: Vector[Int],
      rng: Random,
  )(using p: SimParams): Firm.State =
    val newSector    = pickSector(totalWeight, sectorWeights, rng)
    val firmSize     = FirmSizeDistribution.draw(rng)
    val startupTeam  = drawStartupTargetWorkers(firmSize, rng)
    val sizeMult     = firmSize.toDouble / p.pop.workersPerFirm
    val isAiNative   = totalAdoption > p.firm.entryAiThreshold &&
      p.firm.entryAiProb.sampleBelow(rng)
    val tech         = chooseTechnology(isAiNative, startupTeam, rng)
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
      startupMonthsLeft = StartupMonths,
      startupTargetWorkers = startupTeam,
      startupFilledWorkers = 0,
    )

  /** Select technology regime: AI-native entrants start as Hybrid with partial
    * automation; conventional entrants use Traditional (workers hired via labor
    * market in subsequent steps).
    */
  @boundaryEscape
  private def chooseTechnology(isAiNative: Boolean, firmSize: Int, rng: Random): TechState =
    if isAiNative then
      val hybridWorkers = Math.max(HybridMinWorkers, firmSize)
      TechState.Hybrid(hybridWorkers, MinAiProductivity + rng.nextDouble() * AiProductivityRange)
    else TechState.Traditional(firmSize)

  /** Draw digital readiness score: AI-native firms get high DR (0.50-0.90);
    * conventional entrants draw from sector baseline with Gaussian noise,
    * clamped to the feasible range for non-digital firms.
    */
  @boundaryEscape
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
  @boundaryEscape
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
  @boundaryEscape
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

  private def drawStartupTargetWorkers(firmSize: Int, rng: Random): Int =
    val lower = Math.min(firmSize, StartupMinWorkers.max(Math.min(2, firmSize)))
    val upper = Math.min(firmSize, StartupMaxWorkers).max(lower)
    rng.between(lower, upper + 1)
