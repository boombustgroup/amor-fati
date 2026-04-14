package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.engine.ledger.TreasuryRuntimeContract
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
      govDividends: PLN,
      issuance: PLN,
  )

  def emitBatches(input: Input): Vector[BatchedFlow] =
    import AggregateBatchContract.*
    Vector.concat(
      AggregateBatchedEmission.transfer(
        EntitySector.Firms,
        FirmIndex.Aggregate,
        EntitySector.Households,
        HouseholdIndex.Investors,
        input.netDomesticDividends,
        AssetType.Cash,
        FlowMechanism.EquityDomDividend,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Firms,
        FirmIndex.Aggregate,
        EntitySector.Foreign,
        ForeignIndex.Aggregate,
        input.foreignDividends,
        AssetType.Cash,
        FlowMechanism.EquityForDividend,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Households,
        HouseholdIndex.Investors,
        EntitySector.Government,
        TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
        input.dividendTax,
        AssetType.Cash,
        FlowMechanism.EquityDividendTax,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Firms,
        FirmIndex.Aggregate,
        EntitySector.Government,
        TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
        input.govDividends,
        AssetType.Cash,
        FlowMechanism.EquityGovDividend,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Households,
        HouseholdIndex.Investors,
        EntitySector.Firms,
        FirmIndex.Aggregate,
        input.issuance,
        AssetType.Equity,
        FlowMechanism.EquityIssuance,
      ),
    )

  def emit(input: Input): Vector[Flow] =
    val flows = Vector.newBuilder[Flow]
    if input.netDomesticDividends > PLN.Zero then
      flows += Flow(FIRM_ACCOUNT, HH_ACCOUNT, input.netDomesticDividends.toLong, FlowMechanism.EquityDomDividend.toInt)
    if input.foreignDividends > PLN.Zero then flows += Flow(FIRM_ACCOUNT, FOREIGN_ACCOUNT, input.foreignDividends.toLong, FlowMechanism.EquityForDividend.toInt)
    if input.dividendTax > PLN.Zero then flows += Flow(HH_ACCOUNT, GOV_ACCOUNT, input.dividendTax.toLong, FlowMechanism.EquityDividendTax.toInt)
    if input.govDividends > PLN.Zero then flows += Flow(FIRM_ACCOUNT, GOV_ACCOUNT, input.govDividends.toLong, FlowMechanism.EquityGovDividend.toInt)
    if input.issuance > PLN.Zero then flows += Flow(HH_ACCOUNT, FIRM_ACCOUNT, input.issuance.toLong, FlowMechanism.EquityIssuance.toInt)
    flows.result()
