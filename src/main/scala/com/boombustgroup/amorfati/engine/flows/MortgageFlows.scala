package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.engine.ledger.MortgageRuntimeContract
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*

/** Housing/mortgage market emitting flows.
  *
  * Batched runtime principal flows stay inside the household sector because
  * `MortgageLoan` has no persisted bank-side stock owner. Cash interest remains
  * an aggregate household-to-bank payment.
  *
  * Legacy flat Account IDs: 0=HH, 1=Bank
  */
object MortgageFlows:

  val HH_ACCOUNT: Int   = 0
  val BANK_ACCOUNT: Int = 1

  case class Input(
      origination: PLN,
      principalRepayment: PLN,
      interest: PLN,
      defaultAmount: PLN,
  )

  def emitBatches(input: Input)(using topology: RuntimeLedgerTopology): Vector[BatchedFlow] =
    val principalShell = MortgageRuntimeContract.principalSettlement(topology)
    Vector.concat(
      AggregateBatchedEmission.transfer(
        principalShell.sector,
        principalShell.index,
        EntitySector.Households,
        topology.households.aggregate,
        input.origination,
        AssetType.MortgageLoan,
        FlowMechanism.MortgageOrigination,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Households,
        topology.households.aggregate,
        principalShell.sector,
        principalShell.index,
        input.principalRepayment,
        AssetType.MortgageLoan,
        FlowMechanism.MortgageRepayment,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Households,
        topology.households.aggregate,
        EntitySector.Banks,
        topology.banks.aggregate,
        input.interest,
        AssetType.Cash,
        FlowMechanism.MortgageInterest,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Households,
        topology.households.aggregate,
        principalShell.sector,
        principalShell.index,
        input.defaultAmount,
        AssetType.MortgageLoan,
        FlowMechanism.MortgageDefault,
      ),
    )

  /** Legacy flat interpreter helper used by old unit tests only.
    *
    * Production ledger execution uses `emitBatches` through `FlowSimulation`.
    * This method preserves the legacy flat Account IDs contract above and does
    * not model the mortgage principal settlement shell.
    */
  def emit(input: Input): Vector[Flow] =
    val flows = Vector.newBuilder[Flow]
    if input.origination > PLN.Zero then flows += Flow(BANK_ACCOUNT, HH_ACCOUNT, input.origination.toLong, FlowMechanism.MortgageOrigination.toInt)
    if input.principalRepayment > PLN.Zero then flows += Flow(HH_ACCOUNT, BANK_ACCOUNT, input.principalRepayment.toLong, FlowMechanism.MortgageRepayment.toInt)
    if input.interest > PLN.Zero then flows += Flow(HH_ACCOUNT, BANK_ACCOUNT, input.interest.toLong, FlowMechanism.MortgageInterest.toInt)
    if input.defaultAmount > PLN.Zero then flows += Flow(HH_ACCOUNT, BANK_ACCOUNT, input.defaultAmount.toLong, FlowMechanism.MortgageDefault.toInt)
    flows.result()
