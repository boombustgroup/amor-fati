package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*

/** Government budget mechanism emitting flows.
  *
  * Same logic as FiscalBudget.update. Revenue from taxes, spending on
  * purchases/benefits/transfers/debt service/investment.
  *
  * ZUS/NFZ/Earmarked subventions are NOT emitted here — those are already
  * emitted by their respective flow mechanisms. This avoids double-counting.
  *
  * Per-HH unemployment benefits and 800+ transfers will become
  * BatchedFlow.Broadcast when Household is migrated (#121). For now: aggregate
  * flows.
  *
  * Account IDs: 0=Taxpayers, 1=GOV, 2=Firms, 3=Bondholders, 4=HH,
  * 5=Infrastructure
  */
object GovBudgetFlows:

  val TAXPAYER_ACCOUNT: Int       = 0
  val GOV_ACCOUNT: Int            = 1
  val FIRM_ACCOUNT: Int           = 2
  val BONDHOLDER_ACCOUNT: Int     = 3
  val HH_ACCOUNT: Int             = 4
  val INFRASTRUCTURE_ACCOUNT: Int = 5

  case class Input(
      taxRevenue: PLN,
      govPurchases: PLN,
      debtService: PLN,
      unempBenefitSpend: PLN,
      socialTransferSpend: PLN,
      euCofinancing: PLN,
      govCapitalSpend: PLN,
  )

  def emit(input: Input): Vector[Flow] =
    val flows = Vector.newBuilder[Flow]

    // Revenue: taxes → GOV
    if input.taxRevenue.toLong > 0L then flows += Flow(TAXPAYER_ACCOUNT, GOV_ACCOUNT, input.taxRevenue.toLong, FlowMechanism.GovTaxRevenue.toInt)

    // Spending: GOV → various recipients
    if input.govPurchases.toLong > 0L then flows += Flow(GOV_ACCOUNT, FIRM_ACCOUNT, input.govPurchases.toLong, FlowMechanism.GovPurchases.toInt)

    if input.debtService.toLong > 0L then flows += Flow(GOV_ACCOUNT, BONDHOLDER_ACCOUNT, input.debtService.toLong, FlowMechanism.GovDebtService.toInt)

    if input.unempBenefitSpend.toLong > 0L then flows += Flow(GOV_ACCOUNT, HH_ACCOUNT, input.unempBenefitSpend.toLong, FlowMechanism.GovUnempBenefit.toInt)

    if input.socialTransferSpend.toLong > 0L then
      flows += Flow(GOV_ACCOUNT, HH_ACCOUNT, input.socialTransferSpend.toLong, FlowMechanism.GovSocialTransfer.toInt)

    if input.euCofinancing.toLong > 0L then flows += Flow(GOV_ACCOUNT, INFRASTRUCTURE_ACCOUNT, input.euCofinancing.toLong, FlowMechanism.GovEuCofin.toInt)

    if input.govCapitalSpend.toLong > 0L then
      flows += Flow(GOV_ACCOUNT, INFRASTRUCTURE_ACCOUNT, input.govCapitalSpend.toLong, FlowMechanism.GovCapitalInvestment.toInt)

    flows.result()
