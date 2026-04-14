package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.engine.ledger.TreasuryRuntimeContract
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*

/** Government budget mechanism emitting flows.
  *
  * Same logic as FiscalBudget.update. This emitter only handles the treasury
  * revenue legs that do not already have their own explicit runtime mechanism
  * elsewhere: VAT, excise, and customs. Spending covers
  * purchases/benefits/transfers/debt service/investment.
  *
  * ZUS/NFZ/Earmarked subventions are NOT emitted here — those are already
  * emitted by their respective flow mechanisms. This avoids double-counting.
  * Firm CIT, household PIT, dividend tax, NBP remittance, and SOE dividends are
  * also emitted elsewhere with their own mechanism IDs.
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
      vatRevenue: PLN,
      exciseRevenue: PLN,
      customsDutyRevenue: PLN,
      govPurchases: PLN,
      debtService: PLN,
      unempBenefitSpend: PLN,
      socialTransferSpend: PLN,
      euCofinancing: PLN,
      govCapitalSpend: PLN,
  )

  def emitBatches(input: Input): Vector[BatchedFlow] =
    import AggregateBatchContract.*
    Vector.concat(
      AggregateBatchedEmission.transfer(
        EntitySector.Government,
        TreasuryRuntimeContract.TaxpayerCollection.index,
        EntitySector.Government,
        TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
        input.vatRevenue,
        AssetType.Cash,
        FlowMechanism.GovVatRevenue,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Government,
        TreasuryRuntimeContract.TaxpayerCollection.index,
        EntitySector.Government,
        TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
        input.exciseRevenue,
        AssetType.Cash,
        FlowMechanism.GovExciseRevenue,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Government,
        TreasuryRuntimeContract.TaxpayerCollection.index,
        EntitySector.Government,
        TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
        input.customsDutyRevenue,
        AssetType.Cash,
        FlowMechanism.GovCustomsDutyRevenue,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Government,
        TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
        EntitySector.Firms,
        FirmIndex.Aggregate,
        input.govPurchases,
        AssetType.Cash,
        FlowMechanism.GovPurchases,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Government,
        TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
        EntitySector.Funds,
        FundIndex.Bondholders,
        input.debtService,
        AssetType.Cash,
        FlowMechanism.GovDebtService,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Government,
        TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
        EntitySector.Households,
        HouseholdIndex.Aggregate,
        input.unempBenefitSpend,
        AssetType.Cash,
        FlowMechanism.GovUnempBenefit,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Government,
        TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
        EntitySector.Households,
        HouseholdIndex.Aggregate,
        input.socialTransferSpend,
        AssetType.Cash,
        FlowMechanism.GovSocialTransfer,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Government,
        TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
        EntitySector.Firms,
        FirmIndex.CapitalGoods,
        input.euCofinancing,
        AssetType.Cash,
        FlowMechanism.GovEuCofin,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Government,
        TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
        EntitySector.Firms,
        FirmIndex.CapitalGoods,
        input.govCapitalSpend,
        AssetType.Cash,
        FlowMechanism.GovCapitalInvestment,
      ),
    )

  def emit(input: Input): Vector[Flow] =
    val flows = Vector.newBuilder[Flow]

    // Revenue: taxpayer collection shell -> GOV
    if input.vatRevenue > PLN.Zero then flows += Flow(TAXPAYER_ACCOUNT, GOV_ACCOUNT, input.vatRevenue.toLong, FlowMechanism.GovVatRevenue.toInt)
    if input.exciseRevenue > PLN.Zero then flows += Flow(TAXPAYER_ACCOUNT, GOV_ACCOUNT, input.exciseRevenue.toLong, FlowMechanism.GovExciseRevenue.toInt)
    if input.customsDutyRevenue > PLN.Zero then
      flows += Flow(TAXPAYER_ACCOUNT, GOV_ACCOUNT, input.customsDutyRevenue.toLong, FlowMechanism.GovCustomsDutyRevenue.toInt)

    // Spending: GOV → various recipients
    if input.govPurchases > PLN.Zero then flows += Flow(GOV_ACCOUNT, FIRM_ACCOUNT, input.govPurchases.toLong, FlowMechanism.GovPurchases.toInt)

    if input.debtService > PLN.Zero then flows += Flow(GOV_ACCOUNT, BONDHOLDER_ACCOUNT, input.debtService.toLong, FlowMechanism.GovDebtService.toInt)

    if input.unempBenefitSpend > PLN.Zero then flows += Flow(GOV_ACCOUNT, HH_ACCOUNT, input.unempBenefitSpend.toLong, FlowMechanism.GovUnempBenefit.toInt)

    if input.socialTransferSpend > PLN.Zero then flows += Flow(GOV_ACCOUNT, HH_ACCOUNT, input.socialTransferSpend.toLong, FlowMechanism.GovSocialTransfer.toInt)

    if input.euCofinancing > PLN.Zero then flows += Flow(GOV_ACCOUNT, INFRASTRUCTURE_ACCOUNT, input.euCofinancing.toLong, FlowMechanism.GovEuCofin.toInt)

    if input.govCapitalSpend > PLN.Zero then
      flows += Flow(GOV_ACCOUNT, INFRASTRUCTURE_ACCOUNT, input.govCapitalSpend.toLong, FlowMechanism.GovCapitalInvestment.toInt)

    flows.result()
