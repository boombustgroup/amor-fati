package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*

/** GPW equity market emitting flows.
  *
  * Dividends: domestic (Firm→HH net of Belka tax), foreign (Firm→Foreign).
  * Issuance: HH→Firm (equity capital).
  *
  * Account IDs: 0=Firm, 1=HH, 2=Foreign, 3=Gov (Belka tax)
  */
object EquityFlows:

  val FIRM_ACCOUNT: Int    = 0
  val HH_ACCOUNT: Int      = 1
  val FOREIGN_ACCOUNT: Int = 2
  val GOV_ACCOUNT: Int     = 3

  case class Input(
      netDomesticDividends: PLN,
      foreignDividends: PLN,
      dividendTax: PLN,
      issuance: PLN,
  )

  def emit(input: Input): Vector[Flow] =
    val flows = Vector.newBuilder[Flow]
    if input.netDomesticDividends.toLong > 0L then
      flows += Flow(FIRM_ACCOUNT, HH_ACCOUNT, input.netDomesticDividends.toLong, FlowMechanism.EquityDomDividend.toInt)
    if input.foreignDividends.toLong > 0L then
      flows += Flow(FIRM_ACCOUNT, FOREIGN_ACCOUNT, input.foreignDividends.toLong, FlowMechanism.EquityForDividend.toInt)
    if input.dividendTax.toLong > 0L then flows += Flow(HH_ACCOUNT, GOV_ACCOUNT, input.dividendTax.toLong, FlowMechanism.EquityDividendTax.toInt)
    if input.issuance.toLong > 0L then flows += Flow(HH_ACCOUNT, FIRM_ACCOUNT, input.issuance.toLong, FlowMechanism.EquityIssuance.toInt)
    flows.result()
