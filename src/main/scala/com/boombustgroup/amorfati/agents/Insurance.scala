package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

/** Insurance sector: life + non-life reserves, three-asset allocation (gov
  * bonds, corp bonds, equities). KNF 2024 calibration.
  */
object Insurance:

  // Unemployment threshold below which non-life claims have no cyclical add-on
  private val NonLifeUnempThreshold = 0.05

  case class State(
      lifeReserves: PLN,         // life insurance technical reserves
      nonLifeReserves: PLN,      // non-life insurance technical reserves
      govBondHoldings: PLN,      // government bond allocation
      corpBondHoldings: PLN,     // corporate bond allocation
      equityHoldings: PLN,       // equity allocation (GPW)
      lastLifePremium: PLN,      // life premium collected this month
      lastNonLifePremium: PLN,   // non-life premium collected this month
      lastLifeClaims: PLN,       // life claims paid this month
      lastNonLifeClaims: PLN,    // non-life claims paid this month
      lastInvestmentIncome: PLN, // total investment income this month
      lastNetDepositChange: PLN, // net deposit effect: −(premiums − claims)
  )

  object State:
    val zero: State = State(
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
    )

  /** Initialize from SimParams calibration (KNF 2024 reserves + target
    * allocation).
    */
  def initial(using p: SimParams): State =
    val totalAssets = p.ins.lifeReserves + p.ins.nonLifeReserves
    State(
      lifeReserves = p.ins.lifeReserves,
      nonLifeReserves = p.ins.nonLifeReserves,
      govBondHoldings = totalAssets * p.ins.govBondShare.toDouble,
      corpBondHoldings = totalAssets * p.ins.corpBondShare.toDouble,
      equityHoldings = totalAssets * p.ins.equityShare.toDouble,
      lastLifePremium = PLN.Zero,
      lastNonLifePremium = PLN.Zero,
      lastLifeClaims = PLN.Zero,
      lastNonLifeClaims = PLN.Zero,
      lastInvestmentIncome = PLN.Zero,
      lastNetDepositChange = PLN.Zero,
    )

  /** Full monthly step: premiums, claims, investment income, rebalancing. */
  def step(
      prev: State,
      employed: Int,       // employed workers (premium base)
      wage: PLN,           // average monthly wage
      priceLevel: Double,  // CPI price level (non-life premium scaling)
      unempRate: Share,    // unemployment rate (non-life claim cyclicality)
      govBondYield: Rate,  // government bond yield (annualised)
      corpBondYield: Rate, // corporate bond yield (annualised)
      equityReturn: Rate,  // equity monthly return
  )(using p: SimParams): State =
    // Premiums: proportional to wage bill
    val lifePrem    = wage * (employed.toDouble * p.ins.lifePremiumRate.toDouble)
    val nonLifePrem = wage * (employed.toDouble * p.ins.nonLifePremiumRate.toDouble * priceLevel)

    // Claims: life steady, non-life widens with unemployment stress
    val lifeCl      = lifePrem * p.ins.lifeLossRatio.toDouble
    val nonLifeBase = nonLifePrem * p.ins.nonLifeLossRatio.toDouble
    val nonLifeCl   = nonLifeBase * (1.0 + p.ins.nonLifeUnempSens * Math.max(0.0, unempRate.toDouble - NonLifeUnempThreshold))

    // Investment income from all three asset classes
    val invIncome = prev.govBondHoldings * govBondYield.toDouble / 12.0 +
      prev.corpBondHoldings * corpBondYield.toDouble / 12.0 +
      prev.equityHoldings * equityReturn.toDouble

    // Net deposit change: premium outflow from HH minus claims inflow to HH
    val netDepositChange = -(lifePrem + nonLifePrem - lifeCl - nonLifeCl)

    // Update reserves: split investment income proportionally
    val totalReserves = prev.lifeReserves + prev.nonLifeReserves
    val lifeShare     = if totalReserves > PLN.Zero then prev.lifeReserves / totalReserves else 0.5
    val newLifeRes    = prev.lifeReserves + (lifePrem - lifeCl) + invIncome * lifeShare
    val newNonLifeRes = prev.nonLifeReserves + (nonLifePrem - nonLifeCl) + invIncome * (1.0 - lifeShare)

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
      lifePrem,
      nonLifePrem,
      lifeCl,
      nonLifeCl,
      invIncome,
      netDepositChange,
    )
