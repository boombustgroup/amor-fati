package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.engine.ledger.FundRuntimeIndex
import com.boombustgroup.ledger.*

/** First-class runtime ledger topology derived from the current simulation
  * state.
  *
  * The execution layer uses one unified contract:
  *   - real runtime populations occupy the leading indices of dynamic sectors
  *   - non-persisted execution / settlement shells are appended explicitly
  *   - fixed sectors reserve index `0` for persisted stock owners where the
  *     engine already has them, then place runtime shells after that
  *
  * This is the primary runtime execution topology.
  */
final case class RuntimeLedgerTopology(
    households: RuntimeLedgerTopology.Households,
    firms: RuntimeLedgerTopology.Firms,
    banks: RuntimeLedgerTopology.Banks,
    government: RuntimeLedgerTopology.Government,
    nbp: RuntimeLedgerTopology.Nbp,
    insurance: RuntimeLedgerTopology.Insurance,
    funds: RuntimeLedgerTopology.Funds,
    foreign: RuntimeLedgerTopology.Foreign,
):

  val sectorSizes: Map[EntitySector, Int] = Map(
    EntitySector.Households -> households.sectorSize,
    EntitySector.Firms      -> firms.sectorSize,
    EntitySector.Banks      -> banks.sectorSize,
    EntitySector.Government -> government.sectorSize,
    EntitySector.NBP        -> nbp.sectorSize,
    EntitySector.Insurance  -> insurance.sectorSize,
    EntitySector.Funds      -> funds.sectorSize,
    EntitySector.Foreign    -> foreign.sectorSize,
  )

  private val sectorOrder: Vector[EntitySector] = Vector(
    EntitySector.Households,
    EntitySector.Firms,
    EntitySector.Banks,
    EntitySector.Government,
    EntitySector.NBP,
    EntitySector.Insurance,
    EntitySector.Funds,
    EntitySector.Foreign,
  )

  val offsets: Map[EntitySector, Int] =
    sectorOrder
      .foldLeft((Map.empty[EntitySector, Int], 0)):
        case ((acc, next), sector) => (acc.updated(sector, next), next + sectorSizes(sector))
      ._1

  def emptyExecutionState(): MutableWorldState =
    new MutableWorldState(sectorSizes)

  def toFlatFlows(batches: Vector[BatchedFlow]): Vector[Flow] =
    batches.flatMap(toFlatFlows)

  def netDelta(snapshot: Map[(EntitySector, AssetType, Int), Long]): Long =
    snapshot.valuesIterator.sum

  def netDelta(state: MutableWorldState): Long =
    netDelta(state.snapshot)

  private def requireBoundedIndex(
      batchKind: String,
      position: String,
      sector: EntitySector,
      sectorSize: Int,
      index: Int,
  ): Unit =
    require(
      index >= 0 && index < sectorSize,
      s"$batchKind $position index $index is out of bounds for $sector (size=$sectorSize).",
    )

  private def validateScatter(scatter: BatchedFlow.Scatter): Unit =
    val sourceSize = sectorSizes(scatter.from)
    val targetSize = sectorSizes(scatter.to)
    require(
      scatter.amounts.length == scatter.targetIndices.length,
      s"Scatter ${scatter.mechanism} has amounts.length=${scatter.amounts.length} but targetIndices.length=${scatter.targetIndices.length}.",
    )
    require(
      scatter.amounts.length == sourceSize,
      s"Scatter ${scatter.mechanism} has amounts.length=${scatter.amounts.length} but source sector ${scatter.from} has size=$sourceSize.",
    )
    scatter.targetIndices.zipWithIndex.foreach:
      case (targetIndex, senderIndex) =>
        requireBoundedIndex(
          batchKind = s"Scatter ${scatter.mechanism}",
          position = s"target[$senderIndex]",
          sector = scatter.to,
          sectorSize = targetSize,
          index = targetIndex,
        )

  private def validateBroadcast(broadcast: BatchedFlow.Broadcast): Unit =
    val sourceSize = sectorSizes(broadcast.from)
    val targetSize = sectorSizes(broadcast.to)
    require(
      broadcast.amounts.length == broadcast.targetIndices.length,
      s"Broadcast ${broadcast.mechanism} has amounts.length=${broadcast.amounts.length} but targetIndices.length=${broadcast.targetIndices.length}.",
    )
    requireBoundedIndex(
      batchKind = s"Broadcast ${broadcast.mechanism}",
      position = "from",
      sector = broadcast.from,
      sectorSize = sourceSize,
      index = broadcast.fromIndex,
    )
    broadcast.targetIndices.zipWithIndex.foreach:
      case (targetIndex, receiverIndex) =>
        requireBoundedIndex(
          batchKind = s"Broadcast ${broadcast.mechanism}",
          position = s"target[$receiverIndex]",
          sector = broadcast.to,
          sectorSize = targetSize,
          index = targetIndex,
        )

  private def toFlatFlows(batch: BatchedFlow): Vector[Flow] =
    batch match
      case scatter: BatchedFlow.Scatter     =>
        validateScatter(scatter)
        val fromOffset = offsets(scatter.from)
        val toOffset   = offsets(scatter.to)
        scatter.amounts.indices
          .flatMap: i =>
            val amount = scatter.amounts(i)
            val fromId = fromOffset + i
            val toId   = toOffset + scatter.targetIndices(i)
            if amount != 0L && fromId != toId then Some(Flow(fromId, toId, amount, scatter.mechanism.toInt)) else None
          .toVector
      case broadcast: BatchedFlow.Broadcast =>
        validateBroadcast(broadcast)
        val fromOffset = offsets(broadcast.from)
        val toOffset   = offsets(broadcast.to)
        broadcast.amounts.indices
          .flatMap: i =>
            val amount = broadcast.amounts(i)
            val fromId = fromOffset + broadcast.fromIndex
            val toId   = toOffset + broadcast.targetIndices(i)
            if amount != 0L && fromId != toId then Some(Flow(fromId, toId, amount, broadcast.mechanism.toInt)) else None
          .toVector

object RuntimeLedgerTopology:

  def totalTransferred(batch: BatchedFlow): Long =
    batch match
      case scatter: BatchedFlow.Scatter     => scatter.amounts.iterator.sum
      case broadcast: BatchedFlow.Broadcast => broadcast.amounts.iterator.sum

  final case class Households(persistedCount: Int):
    val aggregate: Int  = persistedCount
    val landlords: Int  = persistedCount + 1
    val depositors: Int = persistedCount + 2
    val investors: Int  = persistedCount + 3
    val sectorSize: Int = persistedCount + 4

  final case class Firms(persistedCount: Int):
    val aggregate: Int      = persistedCount
    val services: Int       = persistedCount + 1
    val capitalGoods: Int   = persistedCount + 2
    val ioCounterparty: Int = persistedCount + 3
    val domesticDemand: Int = persistedCount + 4
    val sectorSize: Int     = persistedCount + 5

  final case class Banks(persistedCount: Int):
    val aggregate: Int  = persistedCount
    val sectorSize: Int = persistedCount + 1

  final case class Government():
    val sovereignIssuer: Int          = 0
    val treasuryBudgetSettlement: Int = 1
    val taxpayerCollection: Int       = 2
    val sectorSize: Int               = 3

  final case class Nbp():
    val persistedOwner: Int    = 0
    val reserveSettlement: Int = 1
    val sectorSize: Int        = 2

  final case class Insurance():
    val persistedOwner: Int = 0
    val aggregate: Int      = 1
    val sectorSize: Int     = 2

  final case class Funds():
    val zus: Int            = FundRuntimeIndex.Zus
    val nfz: Int            = FundRuntimeIndex.Nfz
    val ppk: Int            = FundRuntimeIndex.Ppk
    val fp: Int             = FundRuntimeIndex.Fp
    val pfron: Int          = FundRuntimeIndex.Pfron
    val fgsp: Int           = FundRuntimeIndex.Fgsp
    val jst: Int            = FundRuntimeIndex.Jst
    val corpBondOther: Int  = FundRuntimeIndex.CorpBondOther
    val nbfi: Int           = FundRuntimeIndex.Nbfi
    val quasiFiscal: Int    = FundRuntimeIndex.QuasiFiscal
    val supportedCount: Int = FundRuntimeIndex.Count
    val bondholders: Int    = supportedCount
    val bondMarket: Int     = supportedCount + 1
    val markets: Int        = supportedCount + 2
    val healthcare: Int     = supportedCount + 3
    val sectorSize: Int     = supportedCount + 4

  final case class Foreign():
    val govBondHolder: Int      = 0
    val tradeSettlement: Int    = 1
    val incomeSettlement: Int   = 2
    val capitalSettlement: Int  = 3
    val transferSettlement: Int = 4
    val sectorSize: Int         = 5

  def fromState(state: FlowSimulation.SimState): RuntimeLedgerTopology =
    RuntimeLedgerTopology(
      households = Households(state.households.size),
      firms = Firms(state.firms.size),
      banks = Banks(state.banks.size),
      government = Government(),
      nbp = Nbp(),
      insurance = Insurance(),
      funds = Funds(),
      foreign = Foreign(),
    )

  /** Zero-population topology for isolated emitter tests that do not care about
    * real runtime populations.
    */
  val zeroPopulation: RuntimeLedgerTopology =
    RuntimeLedgerTopology(
      households = Households(0),
      firms = Firms(0),
      banks = Banks(0),
      government = Government(),
      nbp = Nbp(),
      insurance = Insurance(),
      funds = Funds(),
      foreign = Foreign(),
    )

  val nonZeroPopulation: RuntimeLedgerTopology =
    RuntimeLedgerTopology(
      households = Households(3),
      firms = Firms(4),
      banks = Banks(2),
      government = Government(),
      nbp = Nbp(),
      insurance = Insurance(),
      funds = Funds(),
      foreign = Foreign(),
    )
