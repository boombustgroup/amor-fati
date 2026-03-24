package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*

/** Household sector emitting flows from aggregate data.
  *
  * Aggregate level (Phase 1): total consumption, rent, PIT, debt service, etc.
  * Per-agent BatchedFlow.Scatter will replace this when the new pipeline is
  * built.
  *
  * Wages (Firm→HH) are NOT here — emitted by Firm mechanism (#122).
  * Unemployment benefits and social transfers are NOT here — emitted by
  * GovBudgetFlows (#124).
  *
  * Account IDs: 0=HH, 1=Firms (consumption), 2=Landlord (rent), 3=Gov (PIT),
  * 4=Bank (debt service/deposits), 5=Foreign (remittances)
  */
object HouseholdFlows:

  val HH_ACCOUNT: Int       = 0
  val FIRM_ACCOUNT: Int     = 1
  val LANDLORD_ACCOUNT: Int = 2
  val GOV_ACCOUNT: Int      = 3
  val BANK_ACCOUNT: Int     = 4
  val FOREIGN_ACCOUNT: Int  = 5

  case class Input(
      consumption: PLN,
      rent: PLN,
      pit: PLN,
      debtService: PLN,
      depositInterest: PLN,
      remittances: PLN,
      ccOrigination: PLN,
      ccDebtService: PLN,
      ccDefault: PLN,
  )

  def emit(input: Input): Vector[Flow] =
    val flows = Vector.newBuilder[Flow]

    if input.consumption > PLN.Zero then flows += Flow(HH_ACCOUNT, FIRM_ACCOUNT, input.consumption.toLong, FlowMechanism.HhConsumption.toInt)
    if input.rent > PLN.Zero then flows += Flow(HH_ACCOUNT, LANDLORD_ACCOUNT, input.rent.toLong, FlowMechanism.HhRent.toInt)
    if input.pit > PLN.Zero then flows += Flow(HH_ACCOUNT, GOV_ACCOUNT, input.pit.toLong, FlowMechanism.HhPit.toInt)
    if input.debtService > PLN.Zero then flows += Flow(HH_ACCOUNT, BANK_ACCOUNT, input.debtService.toLong, FlowMechanism.HhDebtService.toInt)
    if input.depositInterest > PLN.Zero then flows += Flow(BANK_ACCOUNT, HH_ACCOUNT, input.depositInterest.toLong, FlowMechanism.HhDepositInterest.toInt)
    if input.remittances > PLN.Zero then flows += Flow(HH_ACCOUNT, FOREIGN_ACCOUNT, input.remittances.toLong, FlowMechanism.HhRemittance.toInt)
    if input.ccOrigination > PLN.Zero then flows += Flow(BANK_ACCOUNT, HH_ACCOUNT, input.ccOrigination.toLong, FlowMechanism.HhCcOrigination.toInt)
    if input.ccDebtService > PLN.Zero then flows += Flow(HH_ACCOUNT, BANK_ACCOUNT, input.ccDebtService.toLong, FlowMechanism.HhCcDebtService.toInt)
    if input.ccDefault > PLN.Zero then flows += Flow(HH_ACCOUNT, BANK_ACCOUNT, input.ccDefault.toLong, FlowMechanism.HhCcDefault.toInt)

    flows.result()
