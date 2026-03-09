package sfc.init

import sfc.agents.*
import sfc.config.*
import sfc.networks.Network
import sfc.types.*

import scala.util.Random

/** Factory for firm array initialization. */
object FirmInit:

  /** Create firm array with all post-creation enhancements. Returns (firms,
    * actualTotalPopulation) — caller handles totalPopulation.
    */
  def create(rng: Random)(using p: SimParams): (Vector[Firm.State], Int) =
    // Generate network based on TOPOLOGY env var
    val adjList = TOPOLOGY match
      case Topology.Ws      => Network.wattsStrogatz(p.pop.firmsCount, p.firm.networkK, p.firm.networkRewireP.toDouble)
      case Topology.Er      => Network.erdosRenyi(p.pop.firmsCount, p.firm.networkK, rng)
      case Topology.Ba      => Network.barabasiAlbert(p.pop.firmsCount, p.firm.networkK / 2, rng)
      case Topology.Lattice => Network.lattice(p.pop.firmsCount, p.firm.networkK)

    // Assign sectors
    val sectorCounts      = SectorDefs.map(s => (s.share.toDouble * p.pop.firmsCount).toInt)
    val sectorAssignments =
      val arr = new Array[Int](p.pop.firmsCount)
      var idx = 0
      for
        s <- SectorDefs.indices
        _ <- 0 until sectorCounts(s)
      do
        if idx < p.pop.firmsCount then {
          arr(idx) = s; idx += 1
        }
      while idx < p.pop.firmsCount do {
        arr(idx) = SectorDefs.length - 1; idx += 1
      }
      rng.shuffle(arr.toList).toArray

    // Initialize firms
    var firms = (0 until p.pop.firmsCount).map { i =>
      val sec      = SectorDefs(sectorAssignments(i))
      val firmSize = FirmSizeDistribution.draw(rng)
      val sizeMult = firmSize.toDouble / p.pop.workersPerFirm
      Firm.State(
        id = FirmId(i),
        cash = PLN((rng.between(10000.0, 80000.0) + (if rng.nextDouble() < 0.1 then 200000.0 else 0.0)) * sizeMult),
        debt = PLN.Zero,
        tech = TechState.Traditional(firmSize),
        riskProfile = Ratio(rng.between(0.1, 0.9)),
        innovationCostFactor = rng.between(0.8, 1.5),
        digitalReadiness = Ratio(Math.max(0.02, Math.min(0.98, sec.baseDigitalReadiness.toDouble + (rng.nextGaussian() * 0.20)))),
        sector = SectorIdx(sectorAssignments(i)),
        neighbors = adjList(i).map(FirmId(_)),
        initialSize = firmSize,
      )
    }.toVector

    val actualTotalPop = firms.map(f => Firm.workers(f)).sum

    // Physical capital stock
    if p.flags.physCap then firms = firms.map(f => f.copy(capitalStock = PLN(Firm.workers(f).toDouble * p.capital.klRatios.map(_.toDouble)(f.sector.toInt))))

    // Assign firms to banks (always 7 banks via DefaultConfigs)
    firms = firms.map(f => f.copy(bankId = Banking.assignBank(f.sector, Banking.DefaultConfigs, rng)))

    // FDI: assign foreign ownership by sector (#33)
    if p.flags.fdi then
      firms = firms.map { f =>
        if rng.nextDouble() < p.fdi.foreignShares.map(_.toDouble)(f.sector.toInt) then f.copy(foreignOwned = true)
        else f
      }

    // Initialize inventory stock (#43)
    if p.flags.inventory then
      firms = firms.map { f =>
        val capacity  = Firm.capacity(f).toDouble
        val targetInv = capacity * p.capital.inventoryTargetRatios.map(_.toDouble)(f.sector.toInt)
        f.copy(inventory = PLN(targetInv * p.capital.inventoryInitRatio.toDouble))
      }

    // Initialize green capital stock (#36)
    if p.flags.energy then
      firms = firms.map { f =>
        val targetGK = Firm.workers(f).toDouble * p.climate.greenKLRatios.map(_.toDouble)(f.sector.toInt)
        f.copy(greenCapital = PLN(targetGK * p.climate.greenInitRatio.toDouble))
      }

    // Distribute firm cash/debt proportionally to firm size (realistic initialization)
    val totalWorkers     = firms.map(f => Firm.workers(f)).sum
    val firmDepositShare = 0.35 // ~35% of deposits are corporate (NBP M3 2024)
    val totalFirmCash    = p.banking.initDeposits.toDouble * firmDepositShare
    firms = firms.map { f =>
      val workerShare = Firm.workers(f).toDouble / totalWorkers
      f.copy(
        cash = PLN(totalFirmCash * workerShare),
        debt = PLN(p.banking.initLoans.toDouble * workerShare),
      )
    }

    (firms, actualTotalPop)
