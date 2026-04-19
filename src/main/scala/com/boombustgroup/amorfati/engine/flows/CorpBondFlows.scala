package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*

/** Corporate bond market emitting flows.
  *
  * Coupon (Firm→Holders), default (gross principal write-off), issuance
  * (Holders→Firm), amortization (Firm→Holders principal return).
  *
  * Account IDs: 0=Firm, 1=BondHolders (aggregate runtime settlement shell)
  */
object CorpBondFlows:

  val FIRM_ACCOUNT: Int   = 0
  val HOLDER_ACCOUNT: Int = 1

  case class Input(
      coupon: PLN,
      defaultAmount: PLN,
      issuance: PLN,
      amortization: PLN,
  )

  def emitBatches(input: Input)(using topology: RuntimeLedgerTopology): Vector[BatchedFlow] =
    Vector.concat(
      AggregateBatchedEmission.transfer(
        EntitySector.Firms,
        topology.firms.aggregate,
        EntitySector.Funds,
        topology.funds.bondholders,
        input.coupon,
        AssetType.Cash,
        FlowMechanism.CorpBondCoupon,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Firms,
        topology.firms.aggregate,
        EntitySector.Funds,
        topology.funds.bondholders,
        input.defaultAmount,
        AssetType.CorpBond,
        FlowMechanism.CorpBondDefault,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Funds,
        topology.funds.bondMarket,
        EntitySector.Firms,
        topology.firms.aggregate,
        input.issuance,
        AssetType.CorpBond,
        FlowMechanism.CorpBondIssuance,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Firms,
        topology.firms.aggregate,
        EntitySector.Funds,
        topology.funds.bondholders,
        input.amortization,
        AssetType.CorpBond,
        FlowMechanism.CorpBondAmortization,
      ),
    )

  def emit(input: Input): Vector[Flow] =
    val flows = Vector.newBuilder[Flow]
    if input.coupon > PLN.Zero then flows += Flow(FIRM_ACCOUNT, HOLDER_ACCOUNT, input.coupon.toLong, FlowMechanism.CorpBondCoupon.toInt)
    if input.defaultAmount > PLN.Zero then flows += Flow(FIRM_ACCOUNT, HOLDER_ACCOUNT, input.defaultAmount.toLong, FlowMechanism.CorpBondDefault.toInt)
    if input.issuance > PLN.Zero then flows += Flow(HOLDER_ACCOUNT, FIRM_ACCOUNT, input.issuance.toLong, FlowMechanism.CorpBondIssuance.toInt)
    if input.amortization > PLN.Zero then flows += Flow(FIRM_ACCOUNT, HOLDER_ACCOUNT, input.amortization.toLong, FlowMechanism.CorpBondAmortization.toInt)
    flows.result()
