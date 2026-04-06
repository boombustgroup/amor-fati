package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*

/** JST (local government / samorzady) emitting flows.
  *
  * Same logic as Jst.step. Revenue from PIT/CIT shares, property tax,
  * subventions, dotacje. Spending = revenue x multiplier (deficit bias).
  *
  * Account IDs: 0=Taxpayers, 1=JST, 2=LocalServices (spending sink)
  */
object JstFlows:

  val TAXPAYER_ACCOUNT: Int = 0
  val JST_ACCOUNT: Int      = 1
  val SERVICES_ACCOUNT: Int = 2

  case class Input(
      govTaxRevenue: PLN,
      totalWageIncome: PLN,
      gdp: PLN,
      nFirms: Int,
      pitRevenue: PLN,
  )

  def emitBatches(input: Input)(using p: SimParams): Vector[BatchedFlow] =
    import AggregateBatchContract.*
    val jstPitIncome  =
      if input.pitRevenue > PLN.Zero then input.pitRevenue * p.fiscal.jstPitShare
      else input.totalWageIncome * (Share(FallbackPitRate) * p.fiscal.jstPitShare)
    val citRevenue    = input.govTaxRevenue * p.fiscal.jstCitShare
    val propertyTax   = input.nFirms * p.fiscal.jstPropertyTax / 12L
    val subvention    = input.gdp * p.fiscal.jstSubventionShare / 12L
    val dotacje       = input.gdp * p.fiscal.jstDotacjeShare / 12L
    val totalRevenue  = jstPitIncome + citRevenue + propertyTax + subvention + dotacje
    val totalSpending = totalRevenue * p.fiscal.jstSpendingMult
    Vector.concat(
      AggregateBatchedEmission.transfer(
        EntitySector.Government,
        GovernmentIndex.TaxpayerPool,
        EntitySector.Funds,
        FundIndex.Jst,
        totalRevenue,
        AssetType.Cash,
        FlowMechanism.JstRevenue,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Funds,
        FundIndex.Jst,
        EntitySector.Firms,
        FirmIndex.Services,
        totalSpending,
        AssetType.Cash,
        FlowMechanism.JstSpending,
      ),
    )

  def emit(input: Input)(using p: SimParams): Vector[Flow] =
    val jstPitIncome =
      if input.pitRevenue > PLN.Zero then input.pitRevenue * p.fiscal.jstPitShare
      else input.totalWageIncome * (Share(FallbackPitRate) * p.fiscal.jstPitShare)
    val citRevenue   = input.govTaxRevenue * p.fiscal.jstCitShare
    val propertyTax  = input.nFirms * p.fiscal.jstPropertyTax / 12L
    val subvention   = input.gdp * p.fiscal.jstSubventionShare / 12L
    val dotacje      = input.gdp * p.fiscal.jstDotacjeShare / 12L

    val totalRevenue  = jstPitIncome + citRevenue + propertyTax + subvention + dotacje
    val totalSpending = totalRevenue * p.fiscal.jstSpendingMult

    val flows = Vector.newBuilder[Flow]
    if totalRevenue > PLN.Zero then flows += Flow(TAXPAYER_ACCOUNT, JST_ACCOUNT, totalRevenue.toLong, FlowMechanism.JstRevenue.toInt)
    if totalSpending > PLN.Zero then flows += Flow(JST_ACCOUNT, SERVICES_ACCOUNT, totalSpending.toLong, FlowMechanism.JstSpending.toInt)
    flows.result()

  private val FallbackPitRate = 0.12
