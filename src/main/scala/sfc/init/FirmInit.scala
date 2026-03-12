package sfc.init

import sfc.agents.*
import sfc.config.*
import sfc.networks.Network
import sfc.types.*

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
  private val FirmDepositShare = 0.35      // NBP M3 2024: ~35% of deposits are corporate
  private val CashMin          = 10_000.0  // PLN floor for initial cash draw
  private val CashMax          = 80_000.0  // PLN ceiling for initial cash draw
  private val LargeCashBonus   = 200_000.0 // PLN bonus for top-decile firms (lottery draw)
  private val LargeCashProb    = 0.10      // probability of receiving large cash bonus
  private val RiskProfileMin   = 0.1       // minimum firm risk appetite
  private val RiskProfileMax   = 0.9       // maximum firm risk appetite
  private val InnovCostMin     = 0.8       // minimum innovation cost factor
  private val InnovCostMax     = 1.5       // maximum innovation cost factor
  private val DrNoise          = 0.20      // std dev for digital readiness draw
  private val DrFloor          = 0.02      // minimum digital readiness
  private val DrCap            = 0.98      // maximum digital readiness

  /** Create firm array with all post-creation enhancements. */
  def create(rng: Random)(using p: SimParams): Vector[Firm.State] =
    val adjList           = buildNetwork(rng)
    val sectorAssignments = assignSectors(rng)
    val skeleton          = buildSkeleton(adjList, sectorAssignments, rng)
    val withCapAndBank    = assignCapitalAndBank(skeleton, rng)
    val withFdi           = assignForeignOwnership(withCapAndBank, rng)
    finalize(withFdi)

  /** Build network adjacency list from configured topology. */
  private def buildNetwork(rng: Random)(using p: SimParams): Array[Array[Int]] =
    p.topology match
      case Topology.Ws      => Network.wattsStrogatz(p.pop.firmsCount, p.firm.networkK, p.firm.networkRewireP.toDouble, rng)
      case Topology.Er      => Network.erdosRenyi(p.pop.firmsCount, p.firm.networkK, rng)
      case Topology.Ba      => Network.barabasiAlbert(p.pop.firmsCount, p.firm.networkK / 2, rng)
      case Topology.Lattice => Network.lattice(p.pop.firmsCount, p.firm.networkK)

  /** Assign sectors to firm slots based on GUS structural shares, shuffled. */
  private def assignSectors(rng: Random)(using p: SimParams): Vector[Int] =
    val perSector  = p.sectorDefs.map(s => (s.share.toDouble * p.pop.firmsCount).toInt)
    val lastSector = p.sectorDefs.length - 1
    val assigned   = perSector.zipWithIndex.flatMap: (count, sector) =>
      Vector.fill(count)(sector)
    val padded     = assigned ++ Vector.fill(p.pop.firmsCount - assigned.length)(lastSector)
    rng.shuffle(padded)

  /** Create initial firm states with stochastic attributes (cash, size, DR). */
  private def buildSkeleton(
      adjList: Array[Array[Int]],
      sectorAssignments: Vector[Int],
      rng: Random,
  )(using p: SimParams): Vector[Firm.State] =
    (0 until p.pop.firmsCount)
      .map: i =>
        val sec      = p.sectorDefs(sectorAssignments(i))
        val firmSize = FirmSizeDistribution.draw(rng)
        val sizeMult = firmSize.toDouble / p.pop.workersPerFirm
        val baseCash = rng.between(CashMin, CashMax) + (if rng.nextDouble() < LargeCashProb then LargeCashBonus else 0.0)
        val dr       = Ratio(sec.baseDigitalReadiness.toDouble + rng.nextGaussian() * DrNoise).clamp(Ratio(DrFloor), Ratio(DrCap))
        Firm.State(
          id = FirmId(i),
          cash = PLN(baseCash * sizeMult),
          debt = PLN.Zero,
          tech = TechState.Traditional(firmSize),
          riskProfile = Ratio(rng.between(RiskProfileMin, RiskProfileMax)),
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
        )
      .toVector

  /** Assign physical capital stock and bank relationship (rng: bank
    * assignment).
    */
  private def assignCapitalAndBank(firms: Vector[Firm.State], rng: Random)(using p: SimParams): Vector[Firm.State] =
    firms.map: f =>
      val withCap =
        if p.flags.physCap then f.copy(capitalStock = p.capital.klRatios(f.sector.toInt) * Firm.workerCount(f).toDouble)
        else f
      withCap.copy(bankId = Banking.assignBank(f.sector, Banking.DefaultConfigs, rng))

  /** Mark firms as foreign-owned based on per-sector FDI shares (rng: ownership
    * draw).
    */
  private def assignForeignOwnership(firms: Vector[Firm.State], rng: Random)(using p: SimParams): Vector[Firm.State] =
    if p.flags.fdi then
      firms.map: f =>
        if rng.nextDouble() < p.fdi.foreignShares.map(_.toDouble)(f.sector.toInt) then f.copy(foreignOwned = true)
        else f
    else firms

  /** Deterministic final pass: inventory, green capital, cash/debt
    * distribution. No RNG calls — safe to combine into one pass without
    * affecting random sequence.
    */
  private def finalize(firms: Vector[Firm.State])(using p: SimParams): Vector[Firm.State] =
    val totalWorkers  = firms.map(Firm.workerCount).sum
    val totalFirmCash = p.banking.initDeposits * FirmDepositShare
    firms.map: f =>
      val wshare     = Firm.workerCount(f).toDouble / totalWorkers
      val withInv    = initInventory(f)
      val withEnergy = initGreenCapital(withInv)
      withEnergy.copy(cash = totalFirmCash * wshare, debt = p.banking.initLoans * wshare)

  /** Set initial inventory stock from sector target ratio scaled to firm
    * capacity.
    */
  private def initInventory(f: Firm.State)(using p: SimParams): Firm.State =
    if p.flags.inventory then
      val capacity  = Firm.computeCapacity(f)
      val targetInv = capacity * p.capital.inventoryTargetRatios(f.sector.toInt)
      f.copy(inventory = targetInv * p.capital.inventoryInitRatio)
    else f

  /** Set initial green capital stock from sector-specific green K/L ratio. */
  private def initGreenCapital(f: Firm.State)(using p: SimParams): Firm.State =
    if p.flags.energy then
      val targetGK = p.climate.greenKLRatios(f.sector.toInt) * Firm.workerCount(f).toDouble
      f.copy(greenCapital = targetGK * p.climate.greenInitRatio)
    else f
