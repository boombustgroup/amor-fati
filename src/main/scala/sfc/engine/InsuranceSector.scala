package sfc.engine

import sfc.config.Config

/** Insurance sector state: life + non-life reserves, asset allocation (#41). */
case class InsuranceSectorState(
  lifeReserves: Double,
  nonLifeReserves: Double,
  govBondHoldings: Double,
  corpBondHoldings: Double,
  equityHoldings: Double,
  lastLifePremium: Double = 0.0,
  lastNonLifePremium: Double = 0.0,
  lastLifeClaims: Double = 0.0,
  lastNonLifeClaims: Double = 0.0,
  lastInvestmentIncome: Double = 0.0,
  lastNetDepositChange: Double = 0.0
)

object InsuranceSector:
  def zero: InsuranceSectorState = InsuranceSectorState(0, 0, 0, 0, 0)

  def initial: InsuranceSectorState =
    val totalAssets = Config.InsLifeReserves + Config.InsNonLifeReserves
    InsuranceSectorState(
      lifeReserves = Config.InsLifeReserves,
      nonLifeReserves = Config.InsNonLifeReserves,
      govBondHoldings = totalAssets * Config.InsGovBondShare,
      corpBondHoldings = totalAssets * Config.InsCorpBondShare,
      equityHoldings = totalAssets * Config.InsEquityShare
    )

  /** Full monthly step: premiums, claims, investment income, rebalancing. */
  def step(prev: InsuranceSectorState, employed: Int, wage: Double,
           priceLevel: Double, unempRate: Double,
           govBondYield: Double, corpBondYield: Double,
           equityReturn: Double): InsuranceSectorState =
    // Premiums: proportional to wage bill
    val lifePrem = employed * wage * Config.InsLifePremiumRate
    val nonLifePrem = employed * wage * Config.InsNonLifePremiumRate * priceLevel

    // Claims: life steady, non-life widens with unemployment stress
    val lifeCl = lifePrem * Config.InsLifeLossRatio
    val nonLifeBase = nonLifePrem * Config.InsNonLifeLossRatio
    val nonLifeCl = nonLifeBase * (1.0 + Config.InsNonLifeUnempSens * Math.max(0.0, unempRate - 0.05))

    // Investment income from all three asset classes
    val invIncome = prev.govBondHoldings * govBondYield / 12.0 +
      prev.corpBondHoldings * corpBondYield / 12.0 +
      prev.equityHoldings * equityReturn

    // Net deposit change: premium outflow from HH minus claims inflow to HH
    val netDepositChange = -(lifePrem + nonLifePrem - lifeCl - nonLifeCl)

    // Update reserves: split investment income proportionally
    val totalReserves = prev.lifeReserves + prev.nonLifeReserves
    val lifeShare = if totalReserves > 0 then prev.lifeReserves / totalReserves else 0.5
    val newLifeRes = prev.lifeReserves + (lifePrem - lifeCl) + invIncome * lifeShare
    val newNonLifeRes = prev.nonLifeReserves + (nonLifePrem - nonLifeCl) + invIncome * (1.0 - lifeShare)

    // Rebalance towards target allocation
    val totalAssets = newLifeRes + newNonLifeRes
    val s = Config.InsRebalanceSpeed
    val targetGov = totalAssets * Config.InsGovBondShare
    val targetCorp = totalAssets * Config.InsCorpBondShare
    val targetEq = totalAssets * Config.InsEquityShare
    val newGov = prev.govBondHoldings + (targetGov - prev.govBondHoldings) * s
    val newCorp = prev.corpBondHoldings + (targetCorp - prev.corpBondHoldings) * s
    val newEq = prev.equityHoldings + (targetEq - prev.equityHoldings) * s

    InsuranceSectorState(newLifeRes, newNonLifeRes, newGov, newCorp, newEq,
      lifePrem, nonLifePrem, lifeCl, nonLifeCl, invIncome, netDepositChange)
