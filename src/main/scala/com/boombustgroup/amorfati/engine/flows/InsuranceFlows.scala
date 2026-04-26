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

  private val NonLifeUnempThreshold: Share = Share.decimal(5, 2)

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
    val stressGap   = (input.unempRate - NonLifeUnempThreshold).max(Share.Zero)
    val stressAdj   = (stressGap * p.ins.nonLifeUnempSens).toMultiplier
    val nonLifeCl   = nonLifeBase * (Multiplier.One + stressAdj)

    val grossInvestmentIncome = input.prevGovBondHoldings * input.govBondYield.monthly +
      input.prevCorpBondHoldings * input.corpBondYield.monthly +
      input.prevEquityHoldings * input.equityReturn
    val invIncome             = grossInvestmentIncome - input.corpBondDefaultLoss
    val totalReserves         = input.currentLifeReserves + input.currentNonLifeReserves
    val lifeShare             =
      if totalReserves > PLN.Zero then Share(input.currentLifeReserves / totalReserves) else Share.decimal(5, 1)
    val lifeInvIncome         = invIncome * lifeShare
    val nonLifeInvIncome      = invIncome - lifeInvIncome

    Vector.concat(
      reserveIncrease(lifePrem, AssetType.LifeReserve, FlowMechanism.InsLifePremium),
      reserveIncrease(nonLifePrem, AssetType.NonLifeReserve, FlowMechanism.InsNonLifePremium),
      reserveDecrease(lifeCl, AssetType.LifeReserve, FlowMechanism.InsLifeClaim),
      reserveDecrease(nonLifeCl, AssetType.NonLifeReserve, FlowMechanism.InsNonLifeClaim),
      reserveSignedChange(lifeInvIncome, AssetType.LifeReserve, FlowMechanism.InsInvestmentIncome),
      reserveSignedChange(nonLifeInvIncome, AssetType.NonLifeReserve, FlowMechanism.InsInvestmentIncome),
    )

  private def reserveIncrease(
      amount: PLN,
      asset: AssetType,
      mechanism: MechanismId,
  )(using topology: RuntimeLedgerTopology): Vector[BatchedFlow] =
    AggregateBatchedEmission.transfer(
      EntitySector.Insurance,
      topology.insurance.aggregate,
      EntitySector.Insurance,
      topology.insurance.persistedOwner,
      amount,
      asset,
      mechanism,
    )

  private def reserveDecrease(
      amount: PLN,
      asset: AssetType,
      mechanism: MechanismId,
  )(using topology: RuntimeLedgerTopology): Vector[BatchedFlow] =
    AggregateBatchedEmission.transfer(
      EntitySector.Insurance,
      topology.insurance.persistedOwner,
      EntitySector.Insurance,
      topology.insurance.aggregate,
      amount,
      asset,
      mechanism,
    )

  private def reserveSignedChange(
      amount: PLN,
      asset: AssetType,
      mechanism: MechanismId,
  )(using topology: RuntimeLedgerTopology): Vector[BatchedFlow] =
    if amount >= PLN.Zero then reserveIncrease(amount, asset, mechanism)
    else reserveDecrease(amount.abs, asset, mechanism)

  def emit(input: Input)(using p: SimParams): Vector[Flow] =
    val lifePrem    = input.employed * (input.wage * p.ins.lifePremiumRate)
    val nonLifePrem = input.employed * (input.wage * p.ins.nonLifePremiumRate)

    val lifeCl      = lifePrem * p.ins.lifeLossRatio
    val nonLifeBase = nonLifePrem * p.ins.nonLifeLossRatio
    val stressGap   = (input.unempRate - NonLifeUnempThreshold).max(Share.Zero)
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
