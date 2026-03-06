package sfc.init

import scala.util.Random
import sfc.agents.*
import sfc.config.*
import sfc.networks.Network
import sfc.types.*

/** Factory for firm array initialization. */
object FirmInit:

  /** Create firm array with all post-creation enhancements.
    * Returns (firms, actualTotalPopulation) — caller handles Config.setTotalPopulation.
    */
  def create(rng: Random): (Array[Firm], Int) =
    // Generate network based on TOPOLOGY env var
    val adjList = TOPOLOGY match
      case Topology.Ws      => Network.wattsStrogatz(Config.FirmsCount, Config.NetworkK, Config.NetworkRewireP)
      case Topology.Er      => Network.erdosRenyi(Config.FirmsCount, Config.NetworkK, rng)
      case Topology.Ba      => Network.barabasiAlbert(Config.FirmsCount, Config.NetworkK / 2, rng)
      case Topology.Lattice => Network.lattice(Config.FirmsCount, Config.NetworkK)

    // Assign sectors
    val sectorCounts = SECTORS.map(s => (s.share.toDouble * Config.FirmsCount).toInt)
    val sectorAssignments =
      val arr = new Array[Int](Config.FirmsCount)
      var idx = 0
      for
        s <- SECTORS.indices
        _ <- 0 until sectorCounts(s)
      do
        if idx < Config.FirmsCount then { arr(idx) = s; idx += 1 }
      while idx < Config.FirmsCount do { arr(idx) = SECTORS.length - 1; idx += 1 }
      rng.shuffle(arr.toList).toArray

    // Initialize firms
    var firms = (0 until Config.FirmsCount).map { i =>
      val sec = SECTORS(sectorAssignments(i))
      val firmSize = FirmSizeDistribution.draw(rng)
      val sizeMult = firmSize.toDouble / Config.WorkersPerFirm
      Firm(
        id = FirmId(i),
        cash = PLN((rng.between(10000.0, 80000.0) + (if rng.nextDouble() < 0.1 then 200000.0 else 0.0)) * sizeMult),
        debt = PLN.Zero,
        tech = TechState.Traditional(firmSize),
        riskProfile = Ratio(rng.between(0.1, 0.9)),
        innovationCostFactor = rng.between(0.8, 1.5),
        digitalReadiness = Ratio(Math.max(0.02, Math.min(0.98,
          sec.baseDigitalReadiness.toDouble + (rng.nextGaussian() * 0.20)))),
        sector = SectorIdx(sectorAssignments(i)),
        neighbors = adjList(i),
        initialSize = firmSize
      )
    }.toArray

    val actualTotalPop = firms.map(f => FirmOps.workers(f)).sum

    // Physical capital stock
    if Config.PhysCapEnabled then
      firms = firms.map(f =>
        f.copy(capitalStock = PLN(FirmOps.workers(f).toDouble * Config.PhysCapKLRatios(f.sector.toInt))))

    // Multi-bank: assign firms to banks
    if Config.BankMulti then
      firms = firms.map(f =>
        f.copy(bankId = Banking.assignBank(f.sector, Banking.DefaultConfigs, rng)))

    // FDI: assign foreign ownership by sector (#33)
    if Config.FdiEnabled then
      firms = firms.map { f =>
        if rng.nextDouble() < Config.FdiForeignShares(f.sector.toInt) then
          f.copy(foreignOwned = true)
        else f
      }

    // Initialize inventory stock (#43)
    if Config.InventoryEnabled then
      firms = firms.map { f =>
        val capacity = FirmOps.capacity(f).toDouble
        val targetInv = capacity * Config.InventoryTargetRatios(f.sector.toInt)
        f.copy(inventory = PLN(targetInv * Config.InventoryInitRatio))
      }

    // Initialize green capital stock (#36)
    if Config.EnergyEnabled then
      firms = firms.map { f =>
        val targetGK = FirmOps.workers(f).toDouble * Config.GreenKLRatios(f.sector.toInt)
        f.copy(greenCapital = PLN(targetGK * Config.GreenInitRatio))
      }

    // Distribute firm cash/debt proportionally to firm size (realistic initialization)
    val totalWorkers = firms.map(f => FirmOps.workers(f)).sum
    val firmDepositShare = 0.35  // ~35% of deposits are corporate (NBP M3 2024)
    val totalFirmCash = Config.InitBankDeposits * firmDepositShare
    firms = firms.map { f =>
      val workerShare = FirmOps.workers(f).toDouble / totalWorkers
      f.copy(
        cash = PLN(totalFirmCash * workerShare),
        debt = PLN(Config.InitBankLoans * workerShare)
      )
    }

    (firms, actualTotalPop)
