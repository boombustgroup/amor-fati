package sfc.agents

import sfc.config.SimParams
import sfc.types.*

/** Insurance sector: life + non-life reserves, asset allocation. */
object Insurance:
  case class State(
      lifeReserves: PLN,
      nonLifeReserves: PLN,
      govBondHoldings: PLN,
      corpBondHoldings: PLN,
      equityHoldings: PLN,
      lastLifePremium: PLN = PLN.Zero,
      lastNonLifePremium: PLN = PLN.Zero,
      lastLifeClaims: PLN = PLN.Zero,
      lastNonLifeClaims: PLN = PLN.Zero,
      lastInvestmentIncome: PLN = PLN.Zero,
      lastNetDepositChange: PLN = PLN.Zero,
  )

  def zero: State = State(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)

  def initial(using p: SimParams): State =
    val totalAssets = p.ins.lifeReserves.toDouble + p.ins.nonLifeReserves.toDouble
    State(
      lifeReserves = PLN(p.ins.lifeReserves.toDouble),
      nonLifeReserves = PLN(p.ins.nonLifeReserves.toDouble),
      govBondHoldings = PLN(totalAssets * p.ins.govBondShare.toDouble),
      corpBondHoldings = PLN(totalAssets * p.ins.corpBondShare.toDouble),
      equityHoldings = PLN(totalAssets * p.ins.equityShare.toDouble),
    )

  /** Full monthly step: premiums, claims, investment income, rebalancing. */
  def step(
      prev: State,
      employed: Int,
      wage: Double,
      priceLevel: Double,
      unempRate: Double,
      govBondYield: Double,
      corpBondYield: Double,
      equityReturn: Double,
  )(using p: SimParams): State =
    // Premiums: proportional to wage bill (Double arithmetic)
    val lifePrem    = employed * wage * p.ins.lifePremiumRate.toDouble
    val nonLifePrem = employed * wage * p.ins.nonLifePremiumRate.toDouble * priceLevel

    // Claims: life steady, non-life widens with unemployment stress
    val lifeCl      = lifePrem * p.ins.lifeLossRatio.toDouble
    val nonLifeBase = nonLifePrem * p.ins.nonLifeLossRatio.toDouble
    val nonLifeCl   = nonLifeBase * (1.0 + p.ins.nonLifeUnempSens * Math.max(0.0, unempRate - 0.05))

    // Investment income from all three asset classes
    val invIncome = prev.govBondHoldings * govBondYield / 12.0 +
      prev.corpBondHoldings * corpBondYield / 12.0 +
      prev.equityHoldings * equityReturn

    // Net deposit change: premium outflow from HH minus claims inflow to HH
    val netDepositChange = PLN(-(lifePrem + nonLifePrem - lifeCl - nonLifeCl))

    // Update reserves: split investment income proportionally
    val totalReserves = prev.lifeReserves + prev.nonLifeReserves
    val lifeShare     = if totalReserves > PLN.Zero then prev.lifeReserves / totalReserves else 0.5
    val newLifeRes    = prev.lifeReserves + PLN(lifePrem - lifeCl) + invIncome * lifeShare
    val newNonLifeRes = prev.nonLifeReserves + PLN(nonLifePrem - nonLifeCl) + invIncome * (1.0 - lifeShare)

    // Rebalance towards target allocation
    val totalAssets = newLifeRes + newNonLifeRes
    val s           = p.ins.rebalanceSpeed.toDouble
    val targetGov   = totalAssets * p.ins.govBondShare.toDouble
    val targetCorp  = totalAssets * p.ins.corpBondShare.toDouble
    val targetEq    = totalAssets * p.ins.equityShare.toDouble
    val newGov      = prev.govBondHoldings + (targetGov - prev.govBondHoldings) * s
    val newCorp     = prev.corpBondHoldings + (targetCorp - prev.corpBondHoldings) * s
    val newEq       = prev.equityHoldings + (targetEq - prev.equityHoldings) * s

    State(
      newLifeRes,
      newNonLifeRes,
      newGov,
      newCorp,
      newEq,
      PLN(lifePrem),
      PLN(nonLifePrem),
      PLN(lifeCl),
      PLN(nonLifeCl),
      invIncome,
      netDepositChange,
    )
