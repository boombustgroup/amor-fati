package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*

/** Corporate bond market emitting flows.
  *
  * Coupon (Firm→Holders), default (Firm→Holders loss), issuance (Holders→Firm),
  * amortization (Firm→Holders principal return).
  *
  * Account IDs: 0=Firm, 1=BondHolders (banks+PPK+other)
  */
object CorpBondFlows:

  val FIRM_ACCOUNT: Int   = 0
  val HOLDER_ACCOUNT: Int = 1

  case class Input(
      coupon: PLN,
      defaultLoss: PLN,
      issuance: PLN,
      amortization: PLN,
  )

  def emit(input: Input): Vector[Flow] =
    val flows = Vector.newBuilder[Flow]
    if input.coupon.toLong > 0L then flows += Flow(FIRM_ACCOUNT, HOLDER_ACCOUNT, input.coupon.toLong, FlowMechanism.CorpBondCoupon.toInt)
    if input.defaultLoss.toLong > 0L then flows += Flow(FIRM_ACCOUNT, HOLDER_ACCOUNT, input.defaultLoss.toLong, FlowMechanism.CorpBondDefault.toInt)
    if input.issuance.toLong > 0L then flows += Flow(HOLDER_ACCOUNT, FIRM_ACCOUNT, input.issuance.toLong, FlowMechanism.CorpBondIssuance.toInt)
    if input.amortization.toLong > 0L then flows += Flow(FIRM_ACCOUNT, HOLDER_ACCOUNT, input.amortization.toLong, FlowMechanism.CorpBondAmortization.toInt)
    flows.result()
