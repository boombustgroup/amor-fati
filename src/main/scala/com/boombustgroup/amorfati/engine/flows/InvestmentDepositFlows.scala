package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*

/** Investment-timing deposit reconciliation.
  *
  * Positive `investNetDepositFlow` means lagged investment receipts exceed
  * current domestic investment spending, so firm deposits increase. Negative
  * values reverse the channel when current domestic investment draws down the
  * deposit stock. This remains an aggregate execution channel until firm
  * deposits are holder-resolved in persisted ledger state.
  */
object InvestmentDepositFlows:

  val BANK_ACCOUNT: Int = 0
  val FIRM_ACCOUNT: Int = 1

  case class Input(investNetDepositFlow: PLN)

  def emitBatches(input: Input)(using topology: RuntimeLedgerTopology): Vector[BatchedFlow] =
    AggregateBatchedEmission.signedTransfer(
      EntitySector.Banks,
      topology.banks.aggregate,
      EntitySector.Firms,
      topology.firms.aggregate,
      input.investNetDepositFlow,
      AssetType.DemandDeposit,
      FlowMechanism.InvestNetDepositFlow,
    )

  def emit(input: Input): Vector[Flow] =
    if input.investNetDepositFlow > PLN.Zero then
      Vector(Flow(BANK_ACCOUNT, FIRM_ACCOUNT, input.investNetDepositFlow.toLong, FlowMechanism.InvestNetDepositFlow.toInt))
    else if input.investNetDepositFlow < PLN.Zero then
      Vector(Flow(FIRM_ACCOUNT, BANK_ACCOUNT, input.investNetDepositFlow.abs.toLong, FlowMechanism.InvestNetDepositFlow.toInt))
    else Vector.empty

end InvestmentDepositFlows
