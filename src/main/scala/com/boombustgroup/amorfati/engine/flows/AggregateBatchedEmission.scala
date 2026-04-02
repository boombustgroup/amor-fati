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
