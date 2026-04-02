package com.boombustgroup.amorfati.engine.flows

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.*

/** Insurance sector emitting flows.
  *
  * Same logic as Insurance.step. Life + non-life premiums from HH, claims back
  * to HH, investment income from asset holdings.
  *
  * Account IDs: 0=HH, 1=Insurance, 2=Markets (investment income source)
  */
object InsuranceFlows:

  val HH_ACCOUNT: Int      = 0
  val INS_ACCOUNT: Int     = 1
  val MARKETS_ACCOUNT: Int = 2

  private val NonLifeUnempThreshold = 0.05

  case class Input(
      employed: Int,
      wage: PLN,
      unempRate: Share,
      prevGovBondHoldings: PLN,
      prevCorpBondHoldings: PLN,
      prevEquityHoldings: PLN,
      govBondYield: Rate,
      corpBondYield: Rate,
      equityReturn: Rate,
  )

  def emitBatches(input: Input)(using p: SimParams): Vector[BatchedFlow] =
    import AggregateBatchContract.*
    val lifePrem    = input.employed * (input.wage * p.ins.lifePremiumRate)
    val nonLifePrem = input.employed * (input.wage * p.ins.nonLifePremiumRate)

    val lifeCl      = lifePrem * p.ins.lifeLossRatio
    val nonLifeBase = nonLifePrem * p.ins.nonLifeLossRatio
    val stressGap   = (input.unempRate - Share(NonLifeUnempThreshold)).max(Share.Zero)
    val stressAdj   = (stressGap * p.ins.nonLifeUnempSens).toMultiplier
    val nonLifeCl   = nonLifeBase * (Multiplier.One + stressAdj)

    val invIncome = input.prevGovBondHoldings * input.govBondYield.monthly +
      input.prevCorpBondHoldings * input.corpBondYield.monthly +
      input.prevEquityHoldings * input.equityReturn

    Vector.concat(
      AggregateBatchedEmission.transfer(
        EntitySector.Households,
        HouseholdIndex.Aggregate,
        EntitySector.Insurance,
        InsuranceIndex.Aggregate,
        lifePrem,
        AssetType.LifeReserve,
        FlowMechanism.InsLifePremium,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Households,
        HouseholdIndex.Aggregate,
        EntitySector.Insurance,
        InsuranceIndex.Aggregate,
        nonLifePrem,
        AssetType.NonLifeReserve,
        FlowMechanism.InsNonLifePremium,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Insurance,
        InsuranceIndex.Aggregate,
        EntitySector.Households,
        HouseholdIndex.Aggregate,
        lifeCl,
        AssetType.LifeReserve,
        FlowMechanism.InsLifeClaim,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Insurance,
        InsuranceIndex.Aggregate,
        EntitySector.Households,
        HouseholdIndex.Aggregate,
        nonLifeCl,
        AssetType.NonLifeReserve,
        FlowMechanism.InsNonLifeClaim,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Funds,
        FundIndex.Markets,
        EntitySector.Insurance,
        InsuranceIndex.Aggregate,
        invIncome,
        AssetType.Cash,
        FlowMechanism.InsInvestmentIncome,
      ),
    )

  def emit(input: Input)(using p: SimParams): Vector[Flow] =
    val lifePrem    = input.employed * (input.wage * p.ins.lifePremiumRate)
    val nonLifePrem = input.employed * (input.wage * p.ins.nonLifePremiumRate)

    val lifeCl      = lifePrem * p.ins.lifeLossRatio
    val nonLifeBase = nonLifePrem * p.ins.nonLifeLossRatio
    val stressGap   = (input.unempRate - Share(NonLifeUnempThreshold)).max(Share.Zero)
    val stressAdj   = (stressGap * p.ins.nonLifeUnempSens).toMultiplier
    val nonLifeCl   = nonLifeBase * (Multiplier.One + stressAdj)

    val invIncome = input.prevGovBondHoldings * input.govBondYield.monthly +
      input.prevCorpBondHoldings * input.corpBondYield.monthly +
      input.prevEquityHoldings * input.equityReturn

    val flows = Vector.newBuilder[Flow]

    if lifePrem > PLN.Zero then flows += Flow(HH_ACCOUNT, INS_ACCOUNT, lifePrem.toLong, FlowMechanism.InsLifePremium.toInt)
    if nonLifePrem > PLN.Zero then flows += Flow(HH_ACCOUNT, INS_ACCOUNT, nonLifePrem.toLong, FlowMechanism.InsNonLifePremium.toInt)
    if lifeCl > PLN.Zero then flows += Flow(INS_ACCOUNT, HH_ACCOUNT, lifeCl.toLong, FlowMechanism.InsLifeClaim.toInt)
    if nonLifeCl > PLN.Zero then flows += Flow(INS_ACCOUNT, HH_ACCOUNT, nonLifeCl.toLong, FlowMechanism.InsNonLifeClaim.toInt)
    if invIncome > PLN.Zero then flows += Flow(MARKETS_ACCOUNT, INS_ACCOUNT, invIncome.toLong, FlowMechanism.InsInvestmentIncome.toInt)

    flows.result()
