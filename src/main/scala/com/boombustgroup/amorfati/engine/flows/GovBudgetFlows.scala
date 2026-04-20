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
  * Flat-flow account IDs: 0=Taxpayers, 1=GOV, 2=Firms, 3=Bondholders, 4=HH,
  * 5=Infrastructure
  */
object GovBudgetFlows:

  val TAXPAYER_ACCOUNT: Int       = 0
  val GOV_ACCOUNT: Int            = 1
  val FIRM_ACCOUNT: Int           = 2
  val BONDHOLDER_ACCOUNT: Int     = 3
  val HH_ACCOUNT: Int             = 4
  val INFRASTRUCTURE_ACCOUNT: Int = 5

  val DirectTreasuryRevenueMechanisms: Vector[MechanismId] = Vector(
    FlowMechanism.GovVatRevenue,
    FlowMechanism.GovExciseRevenue,
    FlowMechanism.GovCustomsDutyRevenue,
  )

  val CentralGovernmentRevenueMechanisms: Vector[MechanismId] =
    Vector(
      FlowMechanism.FirmCit,
      FlowMechanism.HhPit,
      FlowMechanism.EquityDividendTax,
      FlowMechanism.EquityGovDividend,
    ) ++ DirectTreasuryRevenueMechanisms ++ Vector(
      FlowMechanism.BankNbpRemittance,
    )

  case class Input(
      vatRevenue: PLN,
      exciseRevenue: PLN,
      customsDutyRevenue: PLN,
      govCurrentSpend: PLN,
      debtService: PLN,
      unempBenefitSpend: PLN,
      socialTransferSpend: PLN,
      euCofinancing: PLN,
      govCapitalSpend: PLN,
  )

  private case class FlowLeg(
      fromSector: EntitySector,
      fromIndex: Int,
      toSector: EntitySector,
      toIndex: Int,
      flatFrom: Int,
      flatTo: Int,
      amount: PLN,
      asset: AssetType,
      mechanism: MechanismId,
  )

  private def flowLegs(input: Input)(using topology: RuntimeLedgerTopology): Vector[FlowLeg] =
    Vector(
      FlowLeg(
        fromSector = EntitySector.Government,
        fromIndex = TreasuryRuntimeContract.TaxpayerCollection.index,
        toSector = EntitySector.Government,
        toIndex = TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
        flatFrom = TAXPAYER_ACCOUNT,
        flatTo = GOV_ACCOUNT,
        amount = input.vatRevenue,
        asset = AssetType.Cash,
        mechanism = FlowMechanism.GovVatRevenue,
      ),
      FlowLeg(
        fromSector = EntitySector.Government,
        fromIndex = TreasuryRuntimeContract.TaxpayerCollection.index,
        toSector = EntitySector.Government,
        toIndex = TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
        flatFrom = TAXPAYER_ACCOUNT,
        flatTo = GOV_ACCOUNT,
        amount = input.exciseRevenue,
        asset = AssetType.Cash,
        mechanism = FlowMechanism.GovExciseRevenue,
      ),
      FlowLeg(
        fromSector = EntitySector.Government,
        fromIndex = TreasuryRuntimeContract.TaxpayerCollection.index,
        toSector = EntitySector.Government,
        toIndex = TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
        flatFrom = TAXPAYER_ACCOUNT,
        flatTo = GOV_ACCOUNT,
        amount = input.customsDutyRevenue,
        asset = AssetType.Cash,
        mechanism = FlowMechanism.GovCustomsDutyRevenue,
      ),
      FlowLeg(
        fromSector = EntitySector.Government,
        fromIndex = TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
        toSector = EntitySector.Firms,
        toIndex = topology.firms.aggregate,
        flatFrom = GOV_ACCOUNT,
        flatTo = FIRM_ACCOUNT,
        amount = input.govCurrentSpend,
        asset = AssetType.Cash,
        mechanism = FlowMechanism.GovPurchases,
      ),
      FlowLeg(
        fromSector = EntitySector.Government,
        fromIndex = TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
        toSector = EntitySector.Funds,
        toIndex = topology.funds.bondholders,
        flatFrom = GOV_ACCOUNT,
        flatTo = BONDHOLDER_ACCOUNT,
        amount = input.debtService,
        asset = AssetType.Cash,
        mechanism = FlowMechanism.GovDebtService,
      ),
      FlowLeg(
        fromSector = EntitySector.Government,
        fromIndex = TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
        toSector = EntitySector.Households,
        toIndex = topology.households.aggregate,
        flatFrom = GOV_ACCOUNT,
        flatTo = HH_ACCOUNT,
        amount = input.unempBenefitSpend,
        asset = AssetType.Cash,
        mechanism = FlowMechanism.GovUnempBenefit,
      ),
      FlowLeg(
        fromSector = EntitySector.Government,
        fromIndex = TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
        toSector = EntitySector.Households,
        toIndex = topology.households.aggregate,
        flatFrom = GOV_ACCOUNT,
        flatTo = HH_ACCOUNT,
        amount = input.socialTransferSpend,
        asset = AssetType.Cash,
        mechanism = FlowMechanism.GovSocialTransfer,
      ),
      FlowLeg(
        fromSector = EntitySector.Government,
        fromIndex = TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
        toSector = EntitySector.Firms,
        toIndex = topology.firms.capitalGoods,
        flatFrom = GOV_ACCOUNT,
        flatTo = INFRASTRUCTURE_ACCOUNT,
        amount = input.euCofinancing,
        asset = AssetType.Cash,
        mechanism = FlowMechanism.GovEuCofin,
      ),
      FlowLeg(
        fromSector = EntitySector.Government,
        fromIndex = TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
        toSector = EntitySector.Firms,
        toIndex = topology.firms.capitalGoods,
        flatFrom = GOV_ACCOUNT,
        flatTo = INFRASTRUCTURE_ACCOUNT,
        amount = input.govCapitalSpend,
        asset = AssetType.Cash,
        mechanism = FlowMechanism.GovCapitalInvestment,
      ),
    )

  def emitBatches(input: Input)(using topology: RuntimeLedgerTopology): Vector[BatchedFlow] =
    flowLegs(input).flatMap: leg =>
      AggregateBatchedEmission.transfer(
        leg.fromSector,
        leg.fromIndex,
        leg.toSector,
        leg.toIndex,
        leg.amount,
        leg.asset,
        leg.mechanism,
      )

  def emit(input: Input): Vector[Flow] =
    flowLegs(input)(using RuntimeLedgerTopology.zeroPopulation).collect:
      case leg if leg.amount > PLN.Zero =>
        Flow(
          leg.flatFrom,
          leg.flatTo,
          leg.amount.toLong,
          leg.mechanism.toInt,
        )
