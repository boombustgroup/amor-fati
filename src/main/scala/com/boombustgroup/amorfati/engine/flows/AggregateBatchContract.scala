package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.ledger.*

/** Aggregate batched-flow contract for the current pre-agent runtime path.
  *
  * The main simulation still emits aggregate mechanism totals, not per-agent
  * flows. To use the public BatchedFlow API without pretending we already have
  * per-agent sender/receiver vectors, each mechanism writes against a small set
  * of explicit aggregate nodes inside each ledger sector.
  *
  * This contract is intentionally separate from the real runtime
  * LedgerStateAdapter sector sizing. It is an emission-layer bridge only.
  */
object AggregateBatchContract:

  object HouseholdIndex:
    val Aggregate  = 0
    val Landlords  = 1
    val Depositors = 2
    val Investors  = 3

  object FirmIndex:
    val Aggregate      = 0
    val Services       = 1
    val CapitalGoods   = 2
    val IoCounterparty = 3
    val DomesticDemand = 4

  object BankIndex:
    val Aggregate = 0

  object GovernmentIndex:
    val Budget       = 0
    val TaxpayerPool = 1

  object NbpIndex:
    val Aggregate = 0

  object InsuranceIndex:
    val Aggregate = 0

  object FundIndex:
    val Zus         = 0
    val Nfz         = 1
    val Ppk         = 2
    val Fp          = 3
    val Pfron       = 4
    val Fgsp        = 5
    val Jst         = 6
    val Bondholders = 7
    val BondMarket  = 8
    val Markets     = 9
    val Healthcare  = 10

  object ForeignIndex:
    val GovBondHolder      = 0
    val TradeSettlement    = 1
    val IncomeSettlement   = 2
    val CapitalSettlement  = 3
    val TransferSettlement = 4

  val sectorSizes: Map[EntitySector, Int] = Map(
    EntitySector.Households -> 4,
    EntitySector.Firms      -> 5,
    EntitySector.Banks      -> 1,
    EntitySector.Government -> 2,
    EntitySector.NBP        -> 1,
    EntitySector.Insurance  -> 1,
    EntitySector.Funds      -> 11,
    EntitySector.Foreign    -> 5,
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

  /** Net delta across the executed aggregate runtime ledger. */
  def netDelta(snapshot: Map[(EntitySector, AssetType, Int), Long]): Long =
    snapshot.valuesIterator.sum

  def netDelta(state: MutableWorldState): Long =
    netDelta(state.snapshot)

  def toLegacyFlows(batches: Vector[BatchedFlow]): Vector[Flow] =
    batches.flatMap(toLegacyFlows)

  def totalTransferred(batches: Vector[BatchedFlow]): Long =
    batches.iterator.map(totalTransferred).sum

  def totalTransferred(batch: BatchedFlow): Long =
    batch match
      case scatter: BatchedFlow.Scatter     => scatter.amounts.iterator.sum
      case broadcast: BatchedFlow.Broadcast => broadcast.amounts.iterator.sum

  private def toLegacyFlows(batch: BatchedFlow): Vector[Flow] =
    batch match
      case scatter: BatchedFlow.Scatter     =>
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
        val fromOffset = offsets(broadcast.from)
        val toOffset   = offsets(broadcast.to)
        broadcast.amounts.indices
          .flatMap: i =>
            val amount = broadcast.amounts(i)
            val fromId = fromOffset + broadcast.fromIndex
            val toId   = toOffset + broadcast.targetIndices(i)
            if amount != 0L && fromId != toId then Some(Flow(fromId, toId, amount, broadcast.mechanism.toInt)) else None
          .toVector

end AggregateBatchContract
