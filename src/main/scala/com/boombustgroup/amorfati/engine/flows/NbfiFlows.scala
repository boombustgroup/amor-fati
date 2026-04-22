package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*

/** Runtime evidence for monetary TFI/NBFI channels.
  *
  * This emitter covers channels that already affect SFC monetary identities:
  * TFI deposit drain and NBFI credit stock movements. Portfolio rebalancing,
  * investment return, and target asset allocation remain stock projections in
  * `Nbfi.step` until those holder books become first-class runtime contracts.
  */
object NbfiFlows:

  val HH_ACCOUNT: Int   = 0
  val BANK_ACCOUNT: Int = 1
  val NBFI_ACCOUNT: Int = 2

  case class Input(
      depositDrain: PLN,
      origination: PLN,
      repayment: PLN,
      defaultAmount: PLN,
  )

  def emitBatches(input: Input)(using topology: RuntimeLedgerTopology): Vector[BatchedFlow] =
    Vector.concat(
      AggregateBatchedEmission.signedTransfer(
        EntitySector.Banks,
        topology.banks.aggregate,
        EntitySector.Households,
        topology.households.investors,
        input.depositDrain,
        AssetType.DemandDeposit,
        FlowMechanism.TfiDepositDrain,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Funds,
        topology.funds.nbfi,
        EntitySector.Households,
        topology.households.aggregate,
        input.origination,
        AssetType.NbfiLoan,
        FlowMechanism.NbfiOrigination,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Households,
        topology.households.aggregate,
        EntitySector.Funds,
        topology.funds.nbfi,
        input.repayment,
        AssetType.NbfiLoan,
        FlowMechanism.NbfiRepayment,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Households,
        topology.households.aggregate,
        EntitySector.Funds,
        topology.funds.nbfi,
        input.defaultAmount,
        AssetType.NbfiLoan,
        FlowMechanism.NbfiDefault,
      ),
    )

  def emit(input: Input): Vector[Flow] =
    val flows = Vector.newBuilder[Flow]

    if input.depositDrain > PLN.Zero then flows += Flow(BANK_ACCOUNT, HH_ACCOUNT, input.depositDrain.toLong, FlowMechanism.TfiDepositDrain.toInt)
    else if input.depositDrain < PLN.Zero then flows += Flow(HH_ACCOUNT, BANK_ACCOUNT, input.depositDrain.abs.toLong, FlowMechanism.TfiDepositDrain.toInt)
    if input.origination > PLN.Zero then flows += Flow(NBFI_ACCOUNT, HH_ACCOUNT, input.origination.toLong, FlowMechanism.NbfiOrigination.toInt)
    if input.repayment > PLN.Zero then flows += Flow(HH_ACCOUNT, NBFI_ACCOUNT, input.repayment.toLong, FlowMechanism.NbfiRepayment.toInt)
    if input.defaultAmount > PLN.Zero then flows += Flow(HH_ACCOUNT, NBFI_ACCOUNT, input.defaultAmount.toLong, FlowMechanism.NbfiDefault.toInt)

    flows.result()

end NbfiFlows
