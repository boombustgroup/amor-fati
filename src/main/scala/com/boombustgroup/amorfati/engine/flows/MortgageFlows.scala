package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*

/** Housing/mortgage market emitting flows.
  *
  * Origination (Bank→HH), repayment (HH→Bank principal), interest (HH→Bank),
  * default (HH→Bank loss).
  *
  * Account IDs: 0=HH, 1=Bank
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
    Vector.concat(
      AggregateBatchedEmission.transfer(
        EntitySector.Banks,
        topology.banks.aggregate,
        EntitySector.Households,
        topology.households.aggregate,
        input.origination,
        AssetType.MortgageLoan,
        FlowMechanism.MortgageOrigination,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Households,
        topology.households.aggregate,
        EntitySector.Banks,
        topology.banks.aggregate,
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
        EntitySector.Banks,
        topology.banks.aggregate,
        input.defaultAmount,
        AssetType.MortgageLoan,
        FlowMechanism.MortgageDefault,
      ),
    )

  def emit(input: Input): Vector[Flow] =
    val flows = Vector.newBuilder[Flow]
    if input.origination > PLN.Zero then flows += Flow(BANK_ACCOUNT, HH_ACCOUNT, input.origination.toLong, FlowMechanism.MortgageOrigination.toInt)
    if input.principalRepayment > PLN.Zero then flows += Flow(HH_ACCOUNT, BANK_ACCOUNT, input.principalRepayment.toLong, FlowMechanism.MortgageRepayment.toInt)
    if input.interest > PLN.Zero then flows += Flow(HH_ACCOUNT, BANK_ACCOUNT, input.interest.toLong, FlowMechanism.MortgageInterest.toInt)
    if input.defaultAmount > PLN.Zero then flows += Flow(HH_ACCOUNT, BANK_ACCOUNT, input.defaultAmount.toLong, FlowMechanism.MortgageDefault.toInt)
    flows.result()
