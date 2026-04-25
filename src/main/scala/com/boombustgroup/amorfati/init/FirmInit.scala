package com.boombustgroup.amorfati.init

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.*
import com.boombustgroup.amorfati.networks.Network
import com.boombustgroup.amorfati.random.RandomStream
import com.boombustgroup.amorfati.types.*

/** Initial firm population factory.
  *
  * Creates a heterogeneous firm population with network topology, sector
  * assignments (GUS structural shares), size distribution, digital readiness,
  * physical capital, inventories, green capital, FDI ownership, and bank
  * relationships. Cash and debt are distributed proportionally to workforce
  * share (NBP M3 2024 corporate deposit split).
  *
  * RNG ordering matters: passes are separated so that adding a new stochastic
  * draw in one pass (e.g. FDI) does not shift the random sequence of another
  * (e.g. bank assignment).
  */
object FirmInit:

  // ---- Calibration constants ----
  private val FirmDepositShare   = Share("0.35")     // NBP M3 2024: ~35% of deposits are corporate
  private val CashMin            = PLN("10_000.0")   // PLN floor for initial cash draw
  private val CashMax            = PLN("80_000.0")   // PLN ceiling for initial cash draw
  private val LargeCashBonus     = PLN("200_000.0")  // PLN bonus for top-decile firms (lottery draw)
  private val LargeCashProb      = Share("0.10")     // probability of receiving large cash bonus
  private val RiskProfileMin     = Share("0.1")      // minimum firm risk appetite
  private val RiskProfileMax     = Share("0.9")      // maximum firm risk appetite
  private val InnovCostMin       = Multiplier("0.8") // minimum innovation cost factor
  private val InnovCostMax       = Multiplier("1.5") // maximum innovation cost factor
  private val DrNoise            = Share("0.20")     // std dev for digital readiness draw
  private val DrFloor            = Share("0.02")     // minimum digital readiness
  private val DrCap              = Share("0.98")     // maximum digital readiness
  private val InitHybridMinSigma = Sigma("5.0")      // minimum sector sigma for init Hybrid (BPO=50, Mfg=10, Retail=5)
  private val InitHybridProb     = Share("0.08")     // ~8% of eligible firms start as Hybrid (OECD 2024: 5-10%)

  case class Population(
      firms: Vector[Firm.State],
      financialStocks: Vector[Firm.FinancialStocks],
  )

  /** Create firm population with all post-creation enhancements. */
  def create(randomness: InitRandomness.FirmStreams)(using p: SimParams): Population =
    val adjList           = buildNetwork(randomness.network)
    val sectorAssignments = assignSectors(randomness.sectorAssignments)
    val skeleton          = buildSkeleton(adjList, sectorAssignments, randomness.skeleton, randomness.regions)
    val withCapAndBank    = assignCapitalAndBank(skeleton, randomness.capitalAndBank)
    val withFdi           = assignForeignOwnership(withCapAndBank, randomness.foreignOwnership)
    val withSoe           = assignStateOwnership(withFdi, randomness.stateOwnership)
    finalize(withSoe)

  /** Build network adjacency list from configured topology. */
  private def buildNetwork(rng: RandomStream)(using p: SimParams): Array[Array[Int]] =
    p.topology match
      case Topology.Ws      => Network.wattsStrogatz(p.pop.firmsCount, p.firm.networkK, p.firm.networkRewireP, rng)
      case Topology.Er      => Network.erdosRenyi(p.pop.firmsCount, p.firm.networkK, rng)
      case Topology.Ba      => Network.barabasiAlbert(p.pop.firmsCount, p.firm.networkK / 2, rng)
      case Topology.Lattice => Network.lattice(p.pop.firmsCount, p.firm.networkK)

  /** Assign sectors to firm slots based on GUS structural shares, shuffled. */
  private def assignSectors(rng: RandomStream)(using p: SimParams): Vector[Int] =
    val perSector  = p.sectorDefs.map(s => s.share.applyTo(p.pop.firmsCount))
    val lastSector = p.sectorDefs.length - 1
    val assigned   = perSector.zipWithIndex.flatMap: (count, sector) =>
      Vector.fill(count)(sector)
    val padded     = assigned ++ Vector.fill(p.pop.firmsCount - assigned.length)(lastSector)
    rng.shuffle(padded)

  /** Create initial firm states with stochastic attributes (cash, size, DR). */
  private def buildSkeleton(
      adjList: Array[Array[Int]],
      sectorAssignments: Vector[Int],
      rng: RandomStream,
      regionRng: RandomStream,
  )(using p: SimParams): Vector[Firm.State] =
    (0 until p.pop.firmsCount)
      .map: i =>
        val sec          = p.sectorDefs(sectorAssignments(i))
        val firmSize     = FirmSizeDistribution.draw(rng)
        // Preserve the historical RNG contract; final cash is assigned in the
        // deterministic ledger-stock pass below.
        rng.between(CashMin.toLong, CashMax.toLong) + (if LargeCashProb.sampleBelow(rng) then LargeCashBonus.toLong else 0L)
        val dr           = TypedRandom.withGaussianNoise(sec.baseDigitalReadiness, DrNoise, rng).clamp(DrFloor, DrCap)
        // Init tech mix: high-σ sectors with high DR may start as Hybrid (OECD 2024: ~5-10% AI adoption)
        val isHybridInit = sec.sigma >= InitHybridMinSigma &&
          dr > p.firm.hybridReadinessMin &&
          InitHybridProb.sampleBelow(rng)
        val tech         =
          if isHybridInit then
            val hybW = Math.max(1, sec.hybridRetainFrac.applyTo(firmSize))
            val eff  = Multiplier.One + TypedRandom.randomBetween(Multiplier.Zero, Multiplier("0.3"), rng)
            TechState.Hybrid(hybW, eff)
          else TechState.Traditional(firmSize)
        Firm.State(
          id = FirmId(i),
          tech = tech,
          riskProfile = TypedRandom.randomBetween(RiskProfileMin, RiskProfileMax, rng),
          innovationCostFactor = TypedRandom.randomBetween(InnovCostMin, InnovCostMax, rng),
          digitalReadiness = dr,
          sector = SectorIdx(sectorAssignments(i)),
          neighbors = adjList(i).iterator.map(FirmId(_)).toVector,
          bankId = BankId(0),
          initialSize = firmSize,
          capitalStock = PLN.Zero,
          foreignOwned = false,
          inventory = PLN.Zero,
          greenCapital = PLN.Zero,
          accumulatedLoss = PLN.Zero,
          markup = p.pricing.baseMarkup,
          region = Region.cdfSample(regionRng),
        )
      .toVector

  /** Assign physical capital stock and bank relationship (rng: bank
    * assignment).
    */
  private def assignCapitalAndBank(firms: Vector[Firm.State], rng: RandomStream)(using p: SimParams): Vector[Firm.State] =
    firms.map: f =>
      val withCap = f.copy(capitalStock = p.capital.klRatios(f.sector.toInt) * Firm.workerCount(f))
      withCap.copy(bankId = Banking.assignBank(f.sector, Banking.DefaultConfigs, rng))

  /** Mark firms as foreign-owned based on per-sector FDI shares (rng: ownership
    * draw).
    */
  private def assignForeignOwnership(firms: Vector[Firm.State], rng: RandomStream)(using p: SimParams): Vector[Firm.State] =
    firms.map: f =>
      if p.fdi.foreignShares(f.sector.toInt).sampleBelow(rng) then f.copy(foreignOwned = true)
      else f

  /** Mark firms as state-owned based on sector-specific SOE shares. */
  private def assignStateOwnership(firms: Vector[Firm.State], rng: RandomStream): Vector[Firm.State] =
    firms.map: f =>
      if StateOwned.sectorSoeShare(f.sector.toInt).sampleBelow(rng) then f.copy(stateOwned = true)
      else f

  /** Deterministic final pass: inventory, green capital, cash/debt
    * distribution. No RNG calls — safe to combine into one pass without
    * affecting random sequence.
    */
  private def finalize(firms: Vector[Firm.State])(using p: SimParams): Population =
    val totalWorkers  = firms.map(Firm.workerCount).sum
    val totalFirmCash = p.banking.initDeposits * FirmDepositShare
    val rows          = firms.map: f =>
      val wshare     = Share.fraction(Firm.workerCount(f), totalWorkers)
      val withInv    = initInventory(f)
      val withEnergy = initGreenCapital(withInv)
      (
        withEnergy,
        Firm.FinancialStocks(
          cash = totalFirmCash * wshare,
          firmLoan = p.banking.initLoans * wshare,
          equity = PLN.Zero,
        ),
      )
    Population(rows.map(_._1), rows.map(_._2))

  /** Set initial inventory stock from sector target ratio scaled to firm
    * capacity.
    */
  private def initInventory(f: Firm.State)(using p: SimParams): Firm.State =
    val capacity  = Firm.computeCapacity(f)
    val targetInv = capacity * p.capital.inventoryTargetRatios(f.sector.toInt)
    f.copy(inventory = targetInv * p.capital.inventoryInitRatio)

  /** Set initial green capital stock from sector-specific green K/L ratio. */
  private def initGreenCapital(f: Firm.State)(using p: SimParams): Firm.State =
    val targetGK = p.climate.greenKLRatios(f.sector.toInt) * Firm.workerCount(f)
    f.copy(greenCapital = targetGK * p.climate.greenInitRatio)
