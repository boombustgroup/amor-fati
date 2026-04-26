package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.ledger.TreasuryRuntimeContract
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*

/** JST (local government / samorzady) emitting flows.
  *
  * Same logic as Jst.step. Tax revenue from PIT/CIT shares and property tax is
  * emitted separately from central-government subventions/dotacje so SFC
  * diagnostics can distinguish local revenue from central budget spending.
  *
  * Account IDs: 0=Taxpayers, 1=JST, 2=LocalServices (spending sink), 3=GOV
  */
object JstFlows:

  val TAXPAYER_ACCOUNT: Int          = 0
  val JST_ACCOUNT: Int               = 1
  val SERVICES_ACCOUNT: Int          = 2
  val GOV_ACCOUNT: Int               = 3
  private val FallbackPitRate: Share = Share.decimal(12, 2)

  case class Input(
      centralCitRevenue: PLN,
      totalWageIncome: PLN,
      gdp: PLN,
      nFirms: Int,
      pitRevenue: PLN,
  )

  private case class Components(
      taxRevenue: PLN,
      govSubvention: PLN,
      totalRevenue: PLN,
      totalSpending: PLN,
  )

  private def components(input: Input)(using p: SimParams): Components =
    val jstPitIncome =
      if input.pitRevenue > PLN.Zero then input.pitRevenue * p.fiscal.jstPitShare
      else input.totalWageIncome * (FallbackPitRate * p.fiscal.jstPitShare)
    val citRevenue   = input.centralCitRevenue * p.fiscal.jstCitShare
    val propertyTax  = input.nFirms * p.fiscal.jstPropertyTax / 12L
    val subvention   = input.gdp * p.fiscal.jstSubventionShare / 12L
    val dotacje      = input.gdp * p.fiscal.jstDotacjeShare / 12L

    val taxRevenue    = jstPitIncome + citRevenue + propertyTax
    val govSubvention = subvention + dotacje
    val totalRevenue  = taxRevenue + govSubvention
    Components(
      taxRevenue = taxRevenue,
      govSubvention = govSubvention,
      totalRevenue = totalRevenue,
      totalSpending = totalRevenue * p.fiscal.jstSpendingMult,
    )

  def emitBatches(input: Input)(using p: SimParams, topology: RuntimeLedgerTopology): Vector[BatchedFlow] =
    val c = components(input)
    Vector.concat(
      AggregateBatchedEmission.transfer(
        EntitySector.Government,
        TreasuryRuntimeContract.TaxpayerCollection.index,
        EntitySector.Funds,
        topology.funds.jst,
        c.taxRevenue,
        AssetType.Cash,
        FlowMechanism.JstRevenue,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Government,
        TreasuryRuntimeContract.TreasuryBudgetSettlement.index,
        EntitySector.Funds,
        topology.funds.jst,
        c.govSubvention,
        AssetType.Cash,
        FlowMechanism.JstGovSubvention,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Funds,
        topology.funds.jst,
        EntitySector.Firms,
        topology.firms.services,
        c.totalSpending,
        AssetType.Cash,
        FlowMechanism.JstSpending,
      ),
    )

  def emit(input: Input)(using p: SimParams): Vector[Flow] =
    val c = components(input)

    val flows = Vector.newBuilder[Flow]
    if c.taxRevenue > PLN.Zero then flows += Flow(TAXPAYER_ACCOUNT, JST_ACCOUNT, c.taxRevenue.toLong, FlowMechanism.JstRevenue.toInt)
    if c.govSubvention > PLN.Zero then flows += Flow(GOV_ACCOUNT, JST_ACCOUNT, c.govSubvention.toLong, FlowMechanism.JstGovSubvention.toInt)
    if c.totalSpending > PLN.Zero then flows += Flow(JST_ACCOUNT, SERVICES_ACCOUNT, c.totalSpending.toLong, FlowMechanism.JstSpending.toInt)
    flows.result()
