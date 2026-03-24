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

  def emit(input: Input): Vector[Flow] =
    val flows = Vector.newBuilder[Flow]
    if input.origination.toLong > 0L then flows += Flow(BANK_ACCOUNT, HH_ACCOUNT, input.origination.toLong, FlowMechanism.MortgageOrigination.toInt)
    if input.principalRepayment.toLong > 0L then flows += Flow(HH_ACCOUNT, BANK_ACCOUNT, input.principalRepayment.toLong, FlowMechanism.MortgageRepayment.toInt)
    if input.interest.toLong > 0L then flows += Flow(HH_ACCOUNT, BANK_ACCOUNT, input.interest.toLong, FlowMechanism.MortgageInterest.toInt)
    if input.defaultAmount.toLong > 0L then flows += Flow(HH_ACCOUNT, BANK_ACCOUNT, input.defaultAmount.toLong, FlowMechanism.MortgageDefault.toInt)
    flows.result()
