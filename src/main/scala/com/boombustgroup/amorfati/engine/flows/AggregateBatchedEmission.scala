package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.types.PLN
import com.boombustgroup.ledger.*

/** Helper for aggregate one-sender / one-receiver batching. */
object AggregateBatchedEmission:

  def transfer(
      from: EntitySector,
      fromIndex: Int,
      to: EntitySector,
      toIndex: Int,
      amount: PLN,
      asset: AssetType,
      mechanism: MechanismId,
  ): Vector[BatchedFlow] =
    if amount > PLN.Zero then Vector(BatchedFlow.Broadcast(from, fromIndex, to, Array(amount.toLong), Array(toIndex), asset, mechanism))
    else Vector.empty

  def signedTransfer(
      positiveFrom: EntitySector,
      positiveFromIndex: Int,
      positiveTo: EntitySector,
      positiveToIndex: Int,
      signedAmount: PLN,
      asset: AssetType,
      mechanism: MechanismId,
  ): Vector[BatchedFlow] =
    if signedAmount > PLN.Zero then transfer(positiveFrom, positiveFromIndex, positiveTo, positiveToIndex, signedAmount, asset, mechanism)
    else if signedAmount < PLN.Zero then transfer(positiveTo, positiveToIndex, positiveFrom, positiveFromIndex, signedAmount.abs, asset, mechanism)
    else Vector.empty
