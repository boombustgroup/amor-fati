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
      govBondHoldings = totalAssets * p.ins.govBondShare,
      corpBondHoldings = totalAssets * p.ins.corpBondShare,
      equityHoldings = totalAssets * p.ins.equityShare,
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
    val lifePrem    = employed * (wage * p.ins.lifePremiumRate)
    val nonLifePrem = employed * (wage * p.ins.nonLifePremiumRate) // TODO: × priceLevel (needs PriceIndex param)

    // Claims: life steady, non-life widens with unemployment stress
    val lifeCl      = lifePrem * p.ins.lifeLossRatio
    val nonLifeBase = nonLifePrem * p.ins.nonLifeLossRatio
    val stressGap   = (unempRate - Share(NonLifeUnempThreshold)).max(Share.Zero)
    val stressAdj   = (stressGap * p.ins.nonLifeUnempSens).toMultiplier // Share * Coefficient → Coefficient → Multiplier
    val nonLifeCl   = nonLifeBase * (Multiplier.One + stressAdj)

    // Investment income from all three asset classes
    val invIncome = prev.govBondHoldings * govBondYield.monthly +
      prev.corpBondHoldings * corpBondYield.monthly +
      prev.equityHoldings * equityReturn

    // Net deposit change: premium outflow from HH minus claims inflow to HH
    val netDepositChange = -(lifePrem + nonLifePrem - lifeCl - nonLifeCl)

    // Update reserves: split investment income proportionally
    val totalReserves = prev.lifeReserves + prev.nonLifeReserves
    val lifeShare     = if totalReserves > PLN.Zero then Share(prev.lifeReserves / totalReserves) else Share(0.5)
    val newLifeRes    = prev.lifeReserves + (lifePrem - lifeCl) + invIncome * lifeShare
    val newNonLifeRes = prev.nonLifeReserves + (nonLifePrem - nonLifeCl) + invIncome * (Share.One - lifeShare)

    // Rebalance towards target allocation
    val totalAssets = newLifeRes + newNonLifeRes
    val speed       = p.ins.rebalanceSpeed // Coefficient used as adjustment speed
    val targetGov   = totalAssets * p.ins.govBondShare
    val targetCorp  = totalAssets * p.ins.corpBondShare
    val targetEq    = totalAssets * p.ins.equityShare
    val newGov      = prev.govBondHoldings + (targetGov - prev.govBondHoldings) * speed
    val newCorp     = prev.corpBondHoldings + (targetCorp - prev.corpBondHoldings) * speed
    val newEq       = prev.equityHoldings + (targetEq - prev.equityHoldings) * speed

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
