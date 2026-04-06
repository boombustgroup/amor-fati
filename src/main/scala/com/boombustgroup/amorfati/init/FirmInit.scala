package com.boombustgroup.amorfati.init

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.*
import com.boombustgroup.amorfati.networks.Network
import com.boombustgroup.amorfati.types.*

import scala.util.Random

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
  private val FirmDepositShare   = 0.35      // NBP M3 2024: ~35% of deposits are corporate
  private val CashMin            = 10_000.0  // PLN floor for initial cash draw
  private val CashMax            = 80_000.0  // PLN ceiling for initial cash draw
  private val LargeCashBonus     = 200_000.0 // PLN bonus for top-decile firms (lottery draw)
  private val LargeCashProb      = 0.10      // probability of receiving large cash bonus
  private val RiskProfileMin     = 0.1       // minimum firm risk appetite
  private val RiskProfileMax     = 0.9       // maximum firm risk appetite
  private val InnovCostMin       = 0.8       // minimum innovation cost factor
  private val InnovCostMax       = 1.5       // maximum innovation cost factor
  private val DrNoise            = 0.20      // std dev for digital readiness draw
  private val DrFloor            = 0.02      // minimum digital readiness
  private val DrCap              = 0.98      // maximum digital readiness
  private val InitHybridMinSigma = 5.0       // minimum sector σ for init Hybrid (BPO=50, Mfg=10, Retail=5)
  private val InitHybridProb     = 0.08      // ~8% of eligible firms start as Hybrid (OECD 2024: 5-10%)

  /** Create firm array with all post-creation enhancements. */
  def create(rng: Random)(using p: SimParams): Vector[Firm.State] =
    val adjList           = buildNetwork(rng)
    val sectorAssignments = assignSectors(rng)
    val skeleton          = buildSkeleton(adjList, sectorAssignments, rng)
    val withCapAndBank    = assignCapitalAndBank(skeleton, rng)
    val withFdi           = assignForeignOwnership(withCapAndBank, rng)
    val withSoe           = assignStateOwnership(withFdi, rng)
    finalize(withSoe)

  /** Build network adjacency list from configured topology. */
  @boundaryEscape
  private def buildNetwork(rng: Random)(using p: SimParams): Array[Array[Int]] =
    import ComputationBoundary.toDouble
    p.topology match
      case Topology.Ws      => Network.wattsStrogatz(p.pop.firmsCount, p.firm.networkK, toDouble(p.firm.networkRewireP), rng)
      case Topology.Er      => Network.erdosRenyi(p.pop.firmsCount, p.firm.networkK, rng)
      case Topology.Ba      => Network.barabasiAlbert(p.pop.firmsCount, p.firm.networkK / 2, rng)
      case Topology.Lattice => Network.lattice(p.pop.firmsCount, p.firm.networkK)

  /** Assign sectors to firm slots based on GUS structural shares, shuffled. */
  @boundaryEscape
  private def assignSectors(rng: Random)(using p: SimParams): Vector[Int] =
    import ComputationBoundary.toDouble
    val perSector  = p.sectorDefs.map(s => (toDouble(s.share) * p.pop.firmsCount).toInt)
    val lastSector = p.sectorDefs.length - 1
    val assigned   = perSector.zipWithIndex.flatMap: (count, sector) =>
      Vector.fill(count)(sector)
    val padded     = assigned ++ Vector.fill(p.pop.firmsCount - assigned.length)(lastSector)
    rng.shuffle(padded)

  /** Create initial firm states with stochastic attributes (cash, size, DR). */
  @boundaryEscape
  private def buildSkeleton(
      adjList: Array[Array[Int]],
      sectorAssignments: Vector[Int],
      rng: Random,
  )(using p: SimParams): Vector[Firm.State] =
    import ComputationBoundary.toDouble
    val regionRng = new Random(rng.nextLong()) // isolated sub-RNG: one draw from main, then independent
    (0 until p.pop.firmsCount)
      .map: i =>
        val sec          = p.sectorDefs(sectorAssignments(i))
        val firmSize     = FirmSizeDistribution.draw(rng)
        val sizeMult     = firmSize.toDouble / p.pop.workersPerFirm
        val baseCash     = rng.between(CashMin, CashMax) + (if rng.nextDouble() < LargeCashProb then LargeCashBonus else 0.0)
        val dr           = Share(toDouble(sec.baseDigitalReadiness) + rng.nextGaussian() * DrNoise).clamp(Share(DrFloor), Share(DrCap))
        // Init tech mix: high-σ sectors with high DR may start as Hybrid (OECD 2024: ~5-10% AI adoption)
        val isHybridInit = sec.sigma >= Sigma(InitHybridMinSigma) &&
          dr > p.firm.hybridReadinessMin &&
          rng.nextDouble() < InitHybridProb
        val tech         =
          if isHybridInit then
            val hybW = Math.max(1, (firmSize * toDouble(sec.hybridRetainFrac)).toInt)
            val eff  = 1.0 + rng.nextDouble() * 0.3
            TechState.Hybrid(hybW, eff)
          else TechState.Traditional(firmSize)
        Firm.State(
          id = FirmId(i),
          cash = PLN(baseCash * sizeMult),
          debt = PLN.Zero,
          tech = tech,
          riskProfile = Share(rng.between(RiskProfileMin, RiskProfileMax)),
          innovationCostFactor = rng.between(InnovCostMin, InnovCostMax),
          digitalReadiness = dr,
          sector = SectorIdx(sectorAssignments(i)),
          neighbors = adjList(i).iterator.map(FirmId(_)).toVector,
          bankId = BankId(0),
          equityRaised = PLN.Zero,
          initialSize = firmSize,
          capitalStock = PLN.Zero,
          bondDebt = PLN.Zero,
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
  @boundaryEscape
  private def assignCapitalAndBank(firms: Vector[Firm.State], rng: Random)(using p: SimParams): Vector[Firm.State] =
    import ComputationBoundary.toDouble
    firms.map: f =>
      val withCap =
        if true then f.copy(capitalStock = PLN(toDouble(p.capital.klRatios(f.sector.toInt)) * Firm.workerCount(f)))
        else f
      withCap.copy(bankId = Banking.assignBank(f.sector, Banking.DefaultConfigs, rng))

  /** Mark firms as foreign-owned based on per-sector FDI shares (rng: ownership
    * draw).
    */
  private def assignForeignOwnership(firms: Vector[Firm.State], rng: Random)(using p: SimParams): Vector[Firm.State] =
    if true then
      firms.map: f =>
        if p.fdi.foreignShares(f.sector.toInt).sampleBelow(rng) then f.copy(foreignOwned = true)
        else f
    else firms

  /** Mark firms as state-owned based on sector-specific SOE shares. */
  private def assignStateOwnership(firms: Vector[Firm.State], rng: Random): Vector[Firm.State] =
    firms.map: f =>
      if StateOwned.sectorSoeShare(f.sector.toInt).sampleBelow(rng) then f.copy(stateOwned = true)
      else f

  /** Deterministic final pass: inventory, green capital, cash/debt
    * distribution. No RNG calls — safe to combine into one pass without
    * affecting random sequence.
    */
  private def finalize(firms: Vector[Firm.State])(using p: SimParams): Vector[Firm.State] =
    val totalWorkers  = firms.map(Firm.workerCount).sum
    val totalFirmCash = p.banking.initDeposits * Share(FirmDepositShare)
    firms.map: f =>
      val wshare     = Firm.workerCount(f).toDouble / totalWorkers
      val withInv    = initInventory(f)
      val withEnergy = initGreenCapital(withInv)
      withEnergy.copy(cash = totalFirmCash * Share(wshare), debt = p.banking.initLoans * Share(wshare))

  /** Set initial inventory stock from sector target ratio scaled to firm
    * capacity.
    */
  private def initInventory(f: Firm.State)(using p: SimParams): Firm.State =
    if true then
      val capacity  = Firm.computeCapacity(f)
      val targetInv = capacity * p.capital.inventoryTargetRatios(f.sector.toInt)
      f.copy(inventory = targetInv * p.capital.inventoryInitRatio)
    else f

  /** Set initial green capital stock from sector-specific green K/L ratio. */
  @boundaryEscape
  private def initGreenCapital(f: Firm.State)(using p: SimParams): Firm.State =
    import ComputationBoundary.toDouble
    if true then
      val targetGK = PLN(toDouble(p.climate.greenKLRatios(f.sector.toInt)) * Firm.workerCount(f))
      f.copy(greenCapital = targetGK * p.climate.greenInitRatio)
    else f
