package sfc.init

import sfc.agents.*
import sfc.config.*
import sfc.networks.Network
import sfc.types.*

import scala.util.Random

/** Factory for firm array initialization. */
object FirmInit:

  private val FirmDepositShare = 0.35 // NBP M3 2024: ~35% of deposits are corporate
  private val CashMin          = 10_000.0
  private val CashMax          = 80_000.0
  private val LargeCashBonus   = 200_000.0
  private val LargeCashProb    = 0.10

  /** Create firm array with all post-creation enhancements. */
  def create(rng: Random)(using p: SimParams): Vector[Firm.State] =
    // Generate network based on TOPOLOGY env var
    val adjList = TOPOLOGY match
      case Topology.Ws      => Network.wattsStrogatz(p.pop.firmsCount, p.firm.networkK, p.firm.networkRewireP.toDouble)
      case Topology.Er      => Network.erdosRenyi(p.pop.firmsCount, p.firm.networkK, rng)
      case Topology.Ba      => Network.barabasiAlbert(p.pop.firmsCount, p.firm.networkK / 2, rng)
      case Topology.Lattice => Network.lattice(p.pop.firmsCount, p.firm.networkK)

    // Assign sectors
    val sectorCounts      = SectorDefs.map(s => (s.share.toDouble * p.pop.firmsCount).toInt)
    assert(sectorCounts.sum <= p.pop.firmsCount, s"Sector counts overflow: ${sectorCounts.sum} > ${p.pop.firmsCount}")
    val sectorAssignments =
      val arr = new Array[Int](p.pop.firmsCount)
      var idx = 0
      for
        s <- SectorDefs.indices
        _ <- 0 until sectorCounts(s)
      do if idx < p.pop.firmsCount then { arr(idx) = s; idx += 1 }
      while idx < p.pop.firmsCount do { arr(idx) = SectorDefs.length - 1; idx += 1 }
      rng.shuffle(arr.toIndexedSeq).toArray

    // Initialize firms
    val firms0 = (0 until p.pop.firmsCount).map { i =>
      val sec      = SectorDefs(sectorAssignments(i))
      val firmSize = FirmSizeDistribution.draw(rng)
      val sizeMult = firmSize.toDouble / p.pop.workersPerFirm
      Firm.State(
        id = FirmId(i),
        cash = PLN((rng.between(CashMin, CashMax) + (if rng.nextDouble() < LargeCashProb then LargeCashBonus else 0.0)) * sizeMult),
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

    // Pass 1: physCap (no rng) + bankId (rng) — separate to preserve rng sequence
    val firms1 = firms0.map { f =>
      val withCap =
        if p.flags.physCap then f.copy(capitalStock = PLN(Firm.workers(f).toDouble * p.capital.klRatios.map(_.toDouble)(f.sector.toInt)))
        else f
      withCap.copy(bankId = Banking.assignBank(f.sector, Banking.DefaultConfigs, rng))
    }

    // Pass 2: FDI (rng) — separate to preserve rng sequence after bankId pass
    val firms2 =
      if p.flags.fdi then
        firms1.map { f =>
          if rng.nextDouble() < p.fdi.foreignShares.map(_.toDouble)(f.sector.toInt) then f.copy(foreignOwned = true)
          else f
        }
      else firms1

    // Pass 3: inventory + energy + cash/debt (no rng) — single combined pass
    val totalWorkers  = firms2.map(f => Firm.workers(f)).sum
    val totalFirmCash = p.banking.initDeposits.toDouble * FirmDepositShare

    firms2.map(f => enhanceNoRng(f, totalWorkers, totalFirmCash))

  private def enhanceNoRng(f: Firm.State, totalWorkers: Int, totalFirmCash: Double)(using p: SimParams): Firm.State =
    val withInv    =
      if p.flags.inventory then
        val capacity  = Firm.capacity(f).toDouble
        val targetInv = capacity * p.capital.inventoryTargetRatios.map(_.toDouble)(f.sector.toInt)
        f.copy(inventory = PLN(targetInv * p.capital.inventoryInitRatio.toDouble))
      else f
    val withEnergy =
      if p.flags.energy then
        val targetGK = Firm.workers(f).toDouble * p.climate.greenKLRatios.map(_.toDouble)(f.sector.toInt)
        withInv.copy(greenCapital = PLN(targetGK * p.climate.greenInitRatio.toDouble))
      else withInv
    val wshare     = Firm.workers(f).toDouble / totalWorkers
    withEnergy.copy(cash = PLN(totalFirmCash * wshare), debt = PLN(p.banking.initLoans.toDouble * wshare))
