package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*

/** Investment-timing deposit settlement.
  *
  * This is not gross investment spending; [[FirmFlows]] emits that separately.
  * This channel records the private-sector deposit settlement created by the
  * timing gap between lagged domestic investment demand and current domestic
  * investment spending.
  *
  * Positive `investNetDepositFlow` means lagged investment receipts exceed the
  * current domestic investment spend, so aggregate firm/private deposits
  * increase. Negative values reverse the channel when current domestic
  * investment draws down that deposit stock. This remains an aggregate
  * execution channel until firm deposits are holder-resolved in persisted
  * ledger state.
  */
object InvestmentDepositSettlementFlows:

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
      FlowMechanism.InvestmentDepositSettlement,
    )

  def emit(input: Input): Vector[Flow] =
    if input.investNetDepositFlow > PLN.Zero then
      Vector(Flow(BANK_ACCOUNT, FIRM_ACCOUNT, input.investNetDepositFlow.toLong, FlowMechanism.InvestmentDepositSettlement.toInt))
    else if input.investNetDepositFlow < PLN.Zero then
      Vector(Flow(FIRM_ACCOUNT, BANK_ACCOUNT, input.investNetDepositFlow.abs.toLong, FlowMechanism.InvestmentDepositSettlement.toInt))
    else Vector.empty

end InvestmentDepositSettlementFlows
