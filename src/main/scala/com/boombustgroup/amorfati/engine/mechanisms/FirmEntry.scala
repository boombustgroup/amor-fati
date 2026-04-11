package com.boombustgroup.amorfati.engine.mechanisms

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.agents.Region
import com.boombustgroup.amorfati.config.{FirmSizeDistribution, SimParams}
import com.boombustgroup.amorfati.engine.DecisionSignals
import com.boombustgroup.amorfati.types.*

import com.boombustgroup.amorfati.random.RandomStream

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
  private val ProfitClampMin      = Coefficient(-1.0) // floor for normalized sector profit signal
  private val ProfitClampMax      = Coefficient(2.0)  // ceiling for normalized sector profit signal
  private val MinSectorWeight     = Multiplier(0.01)  // floor so no sector has zero entry probability
  private val MaxNeighbors        = 6                 // network degree for new entrants
  private val AiNativeMinDr       = Share(0.50)       // digital readiness floor for AI-native entrants
  private val AiNativeMaxDr       = Share(0.90)       // digital readiness ceiling for AI-native entrants
  private val ConventionalDrNoise = Share(0.10)       // std dev for conventional entrant DR draw
  private val ConventionalDrFloor = Share(0.02)       // minimum DR for conventional entrant
  private val ConventionalDrCap   = Share(0.30)       // maximum DR for conventional entrant
  private val MinAiProductivity   = Multiplier(0.5)   // AI productivity floor for hybrid entrants
  private val MaxAiProductivity   = Multiplier(0.8)   // AI productivity ceiling for hybrid entrants
  private val HybridMinWorkers    = 1                 // minimum viable hybrid workforce
  private val StartupMonths       = 4                 // short entrant startup ramp-up window
  private val StartupMinWorkers   = 1                 // minimum planned startup team
  private val StartupMaxWorkers   = 4                 // entrant startup team cap

  case class Result(
      firms: Vector[Firm.State], // post-entry firm population (may be longer than input if net creation occurred)
      births: Int,               // total new entrants (recycled + net new)
      netBirths: Int,            // net new firms appended to vector
      entrantIds: Set[FirmId],   // firms born this step (recycled + net new)
  )

  case class LaggedEntrySignals(
      unemploymentRate: Share,
      inflation: Rate,
      expectedInflation: Rate,
      laggedHiringSlack: Share,
      startupAbsorptionRate: Share,
  )
  object LaggedEntrySignals:
    def fromDecisionSignals(signals: DecisionSignals): LaggedEntrySignals =
      LaggedEntrySignals(
        unemploymentRate = signals.unemploymentRate,
        inflation = signals.inflation,
        expectedInflation = signals.expectedInflation,
        laggedHiringSlack = signals.laggedHiringSlack,
        startupAbsorptionRate = signals.startupAbsorptionRate,
      )

  def process(
      firms: Vector[Firm.State],
      automationRatio: Share,
      hybridRatio: Share,
      laggedSignals: LaggedEntrySignals,
      rng: RandomStream,
  )(using p: SimParams): Result =
    val living        = firms.filter(Firm.isAlive)
    val profitSignals = computeProfitSignals(living)
    val sectorWeights = computeSectorWeights(profitSignals)

    val totalAdoption                = automationRatio + hybridRatio
    val livingIds                    = living.map(_.id.toInt)
    val (recycledFirms, recycledIds) =
      recycleDeadSlots(firms, totalAdoption, livingIds, sectorWeights, rng)
    val recycledBirths               = recycledIds.size

    val (finalFirms, netBirths, netIds) =
      netCreation(recycledFirms, living.length, laggedSignals, totalAdoption, livingIds, sectorWeights, rng)
    Result(finalFirms, recycledBirths + netBirths, netBirths, recycledIds ++ netIds)

  private def recycleDeadSlots(
      firms: Vector[Firm.State],
      totalAdoption: Share,
      livingIds: Vector[Int],
      sectorWeights: Vector[Multiplier],
      rng: RandomStream,
  )(using p: SimParams): (Vector[Firm.State], Set[FirmId]) =
    val deadSlots = firms.filterNot(Firm.isAlive)
    if deadSlots.isEmpty then return (firms, Set.empty)

    val rawCount         = p.firm.replacementEntryRate.ceilApplyTo(deadSlots.length)
    val boundedCount     =
      Math.max(
        if deadSlots.nonEmpty then p.firm.replacementEntryMinMonthly else 0,
        Math.min(rawCount, p.firm.replacementEntryMaxMonthly),
      )
    val replacementCount = Math.min(deadSlots.length, boundedCount)
    if replacementCount <= 0 then return (firms, Set.empty)

    val replacementIds = rng.shuffle(deadSlots.map(_.id).toList).take(replacementCount).toSet
    (
      firms.map: f =>
        if !Firm.isAlive(f) && replacementIds.contains(f.id) then createNewFirm(f.id, sectorWeights, totalAdoption, livingIds, rng)
        else f,
      replacementIds,
    )

  /** Append net new firms when unemployment exceeds NAIRU. Birth count is
    * proportional to the gap, capped to prevent vector explosion. New firms get
    * sequential FirmIds starting at `firms.length` to maintain the FirmId ==
    * vector index invariant.
    */
  private def netCreation(
      firms: Vector[Firm.State],
      livingCount: Int,
      laggedSignals: LaggedEntrySignals,
      totalAdoption: Share,
      livingIds: Vector[Int],
      sectorWeights: Vector[Multiplier],
      rng: RandomStream,
  )(using p: SimParams): (Vector[Firm.State], Int, Set[FirmId]) =
    val signal   = expansionaryEntrySignal(laggedSignals)
    if signal <= Share.Zero then return (firms, 0, Set.empty)
    val rawCount = (signal * p.firm.netEntryRate).ceilApplyTo(livingCount)
    val count    = Math.max(0, Math.min(rawCount, p.firm.netEntryMaxMonthly))
    if count <= 0 then return (firms, 0, Set.empty)

    val baseId   = firms.length
    val newFirms = (0 until count).map: i =>
      createNewFirm(FirmId(baseId + i), sectorWeights, totalAdoption, livingIds, rng)
    (firms ++ newFirms, count, newFirms.map(_.id).toSet)

  private def expansionaryEntrySignal(c: LaggedEntrySignals)(using p: SimParams): Share =
    val laborSlack    = (c.unemploymentRate - p.monetary.nairu).max(Share.Zero)
    if laborSlack <= Share.Zero then return Share.Zero
    val nominalSignal =
      if c.inflation < Rate.Zero && c.expectedInflation < Rate.Zero then 0.0
      else if c.inflation < Rate.Zero || c.expectedInflation < Rate.Zero then 0.35
      else 1.0
    laborSlack * Share(nominalSignal) * c.laggedHiringSlack.clamp(Share.Zero, Share.One) * c.startupAbsorptionRate.clamp(Share.Zero, Share.One)

  private def computeProfitSignals(living: Vector[Firm.State])(using p: SimParams): Vector[Coefficient] =
    val bySector      = living.groupBy(_.sector.toInt)
    val sectorAvgCash = p.sectorDefs.indices.map: s =>
      bySector.get(s).fold(PLN.Zero)(fs => fs.map(_.cash).foldLeft(PLN.Zero)(_ + _).divideBy(fs.length))
    val globalAvgCash = if living.nonEmpty then living.map(_.cash).foldLeft(PLN.Zero)(_ + _).divideBy(living.length) else PLN(1.0)
    sectorAvgCash
      .map: c =>
        (c - globalAvgCash)
          .ratioTo(globalAvgCash.abs.max(PLN(1.0)))
          .toCoefficient
          .clamp(ProfitClampMin, ProfitClampMax)
      .toVector

  private def computeSectorWeights(profitSignals: Vector[Coefficient])(using p: SimParams): Vector[Multiplier] =
    p.sectorDefs.indices
      .map: s =>
        (
          (profitSignals(s) * p.firm.entryProfitSens).growthMultiplier *
            p.firm.entrySectorBarriers(s).toMultiplier
        ).max(
          MinSectorWeight,
        )
      .toVector

  private def createNewFirm(
      slotId: FirmId,
      sectorWeights: Vector[Multiplier],
      totalAdoption: Share,
      livingIds: Vector[Int],
      rng: RandomStream,
  )(using p: SimParams): Firm.State =
    val newSector    = pickSector(sectorWeights, rng)
    val firmSize     = FirmSizeDistribution.draw(rng)
    val startupTeam  = drawStartupTargetWorkers(firmSize, rng)
    val sizeMult     = Scalar.fraction(firmSize, p.pop.workersPerFirm).toMultiplier
    val isAiNative   = totalAdoption > p.firm.entryAiThreshold &&
      p.firm.entryAiProb.sampleBelow(rng)
    val tech         = chooseTechnology(isAiNative, startupTeam, rng)
    val dr           = drawDigitalReadiness(isAiNative, newSector, rng)
    val newNeighbors = assignNeighbors(livingIds, rng)
    val newBankId    = Banking.assignBank(SectorIdx(newSector), Banking.DefaultConfigs, rng)

    Firm.State(
      id = slotId,
      cash = p.firm.entryStartupCash * sizeMult,
      debt = PLN.Zero,
      tech = tech,
      riskProfile = TypedRandom.randomBetween(Share(0.1), Share(0.9), rng),
      innovationCostFactor = TypedRandom.randomBetween(Multiplier(0.8), Multiplier(1.5), rng),
      digitalReadiness = dr,
      sector = SectorIdx(newSector),
      neighbors = newNeighbors,
      bankId = newBankId,
      equityRaised = PLN.Zero,
      initialSize = firmSize,
      capitalStock = initCapitalStock(firmSize, newSector),
      bondDebt = PLN.Zero,
      foreignOwned = p.fdi.foreignShares(newSector).sampleBelow(rng),
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
  private def chooseTechnology(isAiNative: Boolean, firmSize: Int, rng: RandomStream): TechState =
    if isAiNative then
      val hybridWorkers = Math.max(HybridMinWorkers, firmSize)
      TechState.Hybrid(hybridWorkers, TypedRandom.randomBetween(MinAiProductivity, MaxAiProductivity, rng))
    else TechState.Traditional(firmSize)

  /** Draw digital readiness score: AI-native firms get high DR (0.50-0.90);
    * conventional entrants draw from sector baseline with Gaussian noise,
    * clamped to the feasible range for non-digital firms.
    */
  private def drawDigitalReadiness(isAiNative: Boolean, sector: Int, rng: RandomStream)(using p: SimParams): Share =
    if isAiNative then TypedRandom.randomBetween(AiNativeMinDr, AiNativeMaxDr, rng)
    else
      TypedRandom
        .withGaussianNoise(p.sectorDefs(sector).baseDigitalReadiness, ConventionalDrNoise, rng)
        .clamp(ConventionalDrFloor, ConventionalDrCap)

  /** Assign network neighbors from the living firm population. */
  private def assignNeighbors(livingIds: Vector[Int], rng: RandomStream): Vector[FirmId] =
    val nNeighbors = Math.min(MaxNeighbors, livingIds.length)
    if nNeighbors > 0 then rng.shuffle(livingIds.toList).take(nNeighbors).map(FirmId(_)).toVector
    else Vector.empty[FirmId]

  /** Initial physical capital stock from sector-specific capital-labor ratio.
    */
  private def initCapitalStock(firmSize: Int, sector: Int)(using p: SimParams): PLN =
    firmSize * p.capital.klRatios(sector)

  /** Initial inventory from sector target ratio, scaled to firm capacity. */
  private def initInventory(firmSize: Int, sector: Int)(using p: SimParams): PLN =
    val cap = p.firm.baseRevenue * Scalar.fraction(firmSize, p.pop.workersPerFirm).toMultiplier *
      p.sectorDefs(sector).revenueMultiplier
    cap * p.capital.inventoryTargetRatios(sector) * p.capital.inventoryInitRatio

  /** Initial green capital stock from sector-specific green K/L ratio. */
  private def initGreenCapital(firmSize: Int, sector: Int)(using p: SimParams): PLN =
    (firmSize * p.climate.greenKLRatios(sector)) * p.climate.greenInitRatio

  private def pickSector(sectorWeights: Vector[Multiplier], rng: RandomStream): Int =
    WeightedSelection.choose(sectorWeights, rng)

  private def drawStartupTargetWorkers(firmSize: Int, rng: RandomStream): Int =
    val lower = Math.min(firmSize, StartupMinWorkers.max(Math.min(2, firmSize)))
    val upper = Math.min(firmSize, StartupMaxWorkers).max(lower)
    rng.between(lower, upper + 1)
