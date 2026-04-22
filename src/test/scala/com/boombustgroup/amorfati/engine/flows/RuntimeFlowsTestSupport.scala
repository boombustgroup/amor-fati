package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.types.PLN
import com.boombustgroup.ledger.{AssetType, BatchedFlow, EntitySector, Flow, MechanismId}
import org.scalatest.matchers.should.Matchers

object RuntimeFlowsTestSupport extends Matchers:

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
            case _                              => amount
        .sum,
    )

  def flowTotal(flows: Vector[Flow], mechanism: MechanismId): PLN =
    PLN.fromRaw(
      flows.iterator
        .filter(_.mechanism == mechanism.toInt)
        .map(_.amount)
        .sum,
    )
