package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.MonthRandomness
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import com.boombustgroup.amorfati.types.PLN
import com.boombustgroup.ledger.{AssetType, BatchedFlow, EntitySector, Flow, MechanismId}
import org.scalatest.matchers.should.Matchers

object RuntimeFlowsTestSupport extends Matchers:

  private val DefaultSeed = 42L

  def initFromSeed(seed: Long = DefaultSeed)(using SimParams): WorldInit.InitResult =
    WorldInit.initialize(InitRandomness.Contract.fromSeed(seed))

  def stateFromInit(init: WorldInit.InitResult): FlowSimulation.SimState =
    FlowSimulation.SimState.fromInit(init)

  def stateFromSeed(seed: Long = DefaultSeed)(using SimParams): FlowSimulation.SimState =
    stateFromInit(initFromSeed(seed))

  def monthRandomness(seed: Long = DefaultSeed): MonthRandomness.Contract =
    MonthRandomness.Contract.fromSeed(seed)

  def stepWithSeed(
      state: FlowSimulation.SimState,
      seed: Long = DefaultSeed,
  )(using SimParams): FlowSimulation.StepOutput =
    FlowSimulation.step(state, monthRandomness(seed))

  def stepFromSeed(initSeed: Long = DefaultSeed, monthSeed: Long = DefaultSeed)(using SimParams): FlowSimulation.StepOutput =
    stepWithSeed(stateFromSeed(initSeed), monthSeed)

  def mechanismBatches(batches: Vector[BatchedFlow], mechanism: MechanismId): Vector[BatchedFlow] =
    batches.filter(_.mechanism == mechanism)

  def assertAllCash(selected: Vector[BatchedFlow], mechanism: MechanismId): Unit =
    withClue(s"Mechanism ${mechanism.toInt} should emit only cash batches: ") {
      selected.map(_.asset).toSet shouldBe Set(AssetType.Cash)
    }

  def totalTransferred(selected: Vector[BatchedFlow]): PLN =
    PLN.fromRaw(
      selected.iterator
        .map(RuntimeLedgerTopology.totalTransferred)
        .sum,
    )

  def cashMechanismTotal(selected: Vector[BatchedFlow]): PLN =
    totalTransferred(selected)

  def mechanismTotal(batches: Vector[BatchedFlow], mechanism: MechanismId): PLN =
    cashMechanismTotal(mechanismBatches(batches, mechanism))

  def mechanismsTotal(batches: Vector[BatchedFlow], mechanisms: Iterable[MechanismId]): PLN =
    mechanisms.iterator
      .map(mechanismTotal(batches, _))
      .foldLeft(PLN.Zero)(_ + _)

  def signedMechanismTotal(
      batches: Vector[BatchedFlow],
      mechanism: MechanismId,
      positiveFrom: EntitySector,
      positiveTo: EntitySector,
  ): PLN =
    PLN.fromRaw(
      mechanismBatches(batches, mechanism).iterator
        .map: batch =>
          val amount = RuntimeLedgerTopology.totalTransferred(batch)
          (batch.from, batch.to) match
            case (`positiveFrom`, `positiveTo`) => amount
            case (`positiveTo`, `positiveFrom`) => -amount
            case _                              =>
              throw new IllegalArgumentException(
                s"Mechanism ${mechanism.toInt} has unsupported signed direction ${batch.from}->${batch.to}; expected $positiveFrom->$positiveTo or $positiveTo->$positiveFrom.",
              )
        .sum,
    )

  def flowTotal(flows: Vector[Flow], mechanism: MechanismId): PLN =
    PLN.fromRaw(
      flows.iterator
        .filter(_.mechanism == mechanism.toInt)
        .map(_.amount)
        .sum,
    )

  def canonicalBatches(batches: Vector[BatchedFlow]): Vector[Any] =
    batches.map:
      case scatter: BatchedFlow.Scatter     =>
        (
          "scatter",
          scatter.from,
          scatter.to,
          scatter.amounts.toVector,
          scatter.targetIndices.toVector,
          scatter.asset,
          scatter.mechanism,
        )
      case broadcast: BatchedFlow.Broadcast =>
        (
          "broadcast",
          broadcast.from,
          broadcast.fromIndex,
          broadcast.to,
          broadcast.amounts.toVector,
          broadcast.targetIndices.toVector,
          broadcast.asset,
          broadcast.mechanism,
        )
