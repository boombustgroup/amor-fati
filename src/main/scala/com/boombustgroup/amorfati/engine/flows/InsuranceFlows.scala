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
      currentLifeReserves: PLN,
      currentNonLifeReserves: PLN,
      prevGovBondHoldings: PLN,
      prevCorpBondHoldings: PLN,
      corpBondDefaultLoss: PLN,
      prevEquityHoldings: PLN,
      govBondYield: Rate,
      corpBondYield: Rate,
      equityReturn: Rate,
  )

  def emitBatches(input: Input)(using p: SimParams, topology: RuntimeLedgerTopology): Vector[BatchedFlow] =
    val lifePrem    = input.employed * (input.wage * p.ins.lifePremiumRate)
    val nonLifePrem = input.employed * (input.wage * p.ins.nonLifePremiumRate)

    val lifeCl      = lifePrem * p.ins.lifeLossRatio
    val nonLifeBase = nonLifePrem * p.ins.nonLifeLossRatio
    val stressGap   = (input.unempRate - Share(NonLifeUnempThreshold)).max(Share.Zero)
    val stressAdj   = (stressGap * p.ins.nonLifeUnempSens).toMultiplier
    val nonLifeCl   = nonLifeBase * (Multiplier.One + stressAdj)

    val grossInvestmentIncome = input.prevGovBondHoldings * input.govBondYield.monthly +
      input.prevCorpBondHoldings * input.corpBondYield.monthly +
      input.prevEquityHoldings * input.equityReturn
    val invIncome             = grossInvestmentIncome - input.corpBondDefaultLoss
    val totalReserves         = input.currentLifeReserves + input.currentNonLifeReserves
    val lifeShare             =
      if totalReserves > PLN.Zero then Share(input.currentLifeReserves / totalReserves) else Share(0.5)
    val lifeInvIncome         = invIncome * lifeShare
    val nonLifeInvIncome      = invIncome - lifeInvIncome

    Vector.concat(
      AggregateBatchedEmission.transfer(
        EntitySector.Households,
        topology.households.aggregate,
        EntitySector.Insurance,
        topology.insurance.aggregate,
        lifePrem,
        AssetType.LifeReserve,
        FlowMechanism.InsLifePremium,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Households,
        topology.households.aggregate,
        EntitySector.Insurance,
        topology.insurance.aggregate,
        nonLifePrem,
        AssetType.NonLifeReserve,
        FlowMechanism.InsNonLifePremium,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Insurance,
        topology.insurance.aggregate,
        EntitySector.Households,
        topology.households.aggregate,
        lifeCl,
        AssetType.LifeReserve,
        FlowMechanism.InsLifeClaim,
      ),
      AggregateBatchedEmission.transfer(
        EntitySector.Insurance,
        topology.insurance.aggregate,
        EntitySector.Households,
        topology.households.aggregate,
        nonLifeCl,
        AssetType.NonLifeReserve,
        FlowMechanism.InsNonLifeClaim,
      ),
      AggregateBatchedEmission.signedTransfer(
        EntitySector.Funds,
        topology.funds.markets,
        EntitySector.Insurance,
        topology.insurance.aggregate,
        lifeInvIncome,
        AssetType.LifeReserve,
        FlowMechanism.InsInvestmentIncome,
      ),
      AggregateBatchedEmission.signedTransfer(
        EntitySector.Funds,
        topology.funds.markets,
        EntitySector.Insurance,
        topology.insurance.aggregate,
        nonLifeInvIncome,
        AssetType.NonLifeReserve,
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

    val grossInvestmentIncome = input.prevGovBondHoldings * input.govBondYield.monthly +
      input.prevCorpBondHoldings * input.corpBondYield.monthly +
      input.prevEquityHoldings * input.equityReturn
    val invIncome             = grossInvestmentIncome - input.corpBondDefaultLoss

    val flows = Vector.newBuilder[Flow]

    if lifePrem > PLN.Zero then flows += Flow(HH_ACCOUNT, INS_ACCOUNT, lifePrem.toLong, FlowMechanism.InsLifePremium.toInt)
    if nonLifePrem > PLN.Zero then flows += Flow(HH_ACCOUNT, INS_ACCOUNT, nonLifePrem.toLong, FlowMechanism.InsNonLifePremium.toInt)
    if lifeCl > PLN.Zero then flows += Flow(INS_ACCOUNT, HH_ACCOUNT, lifeCl.toLong, FlowMechanism.InsLifeClaim.toInt)
    if nonLifeCl > PLN.Zero then flows += Flow(INS_ACCOUNT, HH_ACCOUNT, nonLifeCl.toLong, FlowMechanism.InsNonLifeClaim.toInt)
    if invIncome > PLN.Zero then flows += Flow(MARKETS_ACCOUNT, INS_ACCOUNT, invIncome.toLong, FlowMechanism.InsInvestmentIncome.toInt)
    else if invIncome < PLN.Zero then flows += Flow(INS_ACCOUNT, MARKETS_ACCOUNT, invIncome.abs.toLong, FlowMechanism.InsInvestmentIncome.toInt)

    flows.result()
