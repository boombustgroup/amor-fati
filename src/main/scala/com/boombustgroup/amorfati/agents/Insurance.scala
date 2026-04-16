package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

/** Insurance sector: life + non-life reserves, three-asset allocation (gov
  * bonds, corp bonds, equities). KNF 2024 calibration.
  */
object Insurance:

  // Unemployment threshold below which non-life claims have no cyclical add-on
  private val NonLifeUnempThreshold = 0.05

  case class StockState(
      lifeReserves: PLN,     // life insurance technical reserves
      nonLifeReserves: PLN,  // non-life insurance technical reserves
      govBondHoldings: PLN,  // government bond allocation
      corpBondHoldings: PLN, // corporate bond allocation
      equityHoldings: PLN,   // equity allocation (GPW)
  ):
    def totalReserves: PLN = lifeReserves + nonLifeReserves

  object StockState:
    val zero: StockState = StockState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)

  case class MonthlyFlowState(
      lastLifePremium: PLN,      // life premium collected this month
      lastNonLifePremium: PLN,   // non-life premium collected this month
      lastLifeClaims: PLN,       // life claims paid this month
      lastNonLifeClaims: PLN,    // non-life claims paid this month
      lastInvestmentIncome: PLN, // total investment income this month
      lastNetDepositChange: PLN, // net deposit effect: −(premiums − claims)
  )

  case class State(
      monthly: MonthlyFlowState,
  ):
    def lastLifePremium: PLN      = monthly.lastLifePremium
    def lastNonLifePremium: PLN   = monthly.lastNonLifePremium
    def lastLifeClaims: PLN       = monthly.lastLifeClaims
    def lastNonLifeClaims: PLN    = monthly.lastNonLifeClaims
    def lastInvestmentIncome: PLN = monthly.lastInvestmentIncome
    def lastNetDepositChange: PLN = monthly.lastNetDepositChange

  object State:
    def apply(
        lastLifePremium: PLN,
        lastNonLifePremium: PLN,
        lastLifeClaims: PLN,
        lastNonLifeClaims: PLN,
        lastInvestmentIncome: PLN,
        lastNetDepositChange: PLN,
    ): State =
      State(
        monthly = MonthlyFlowState(
          lastLifePremium,
          lastNonLifePremium,
          lastLifeClaims,
          lastNonLifeClaims,
          lastInvestmentIncome,
          lastNetDepositChange,
        ),
      )

    val zero: State = State(
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
  def initial: State = State.zero

  def initialStock(using p: SimParams): StockState =
    val totalAssets = p.ins.lifeReserves + p.ins.nonLifeReserves
    StockState(
      lifeReserves = p.ins.lifeReserves,
      nonLifeReserves = p.ins.nonLifeReserves,
      govBondHoldings = totalAssets * p.ins.govBondShare,
      corpBondHoldings = totalAssets * p.ins.corpBondShare,
      equityHoldings = totalAssets * p.ins.equityShare,
    )

  case class StepResult(state: State, stock: StockState)

  /** Full monthly step: premiums, claims, investment income, rebalancing.
    *
    * Corporate bond holdings are settled by CorporateBondMarket, not by the
    * insurance portfolio target. Insurance only computes income from the
    * opening stock and receives the closing stock from market settlement.
    */
  def step(
      prevStock: StockState,
      employed: Int,       // employed workers (premium base)
      wage: PLN,           // average monthly wage
      unempRate: Share,    // unemployment rate (non-life claim cyclicality)
      govBondYield: Rate,  // government bond yield (annualised)
      corpBondYield: Rate, // corporate bond yield (annualised)
      equityReturn: Rate,  // equity monthly return
      settledCorpBondHoldings: PLN,
      corpBondDefaultLoss: PLN,
  )(using p: SimParams): StepResult =
    // Premiums: proportional to wage bill
    val lifePrem    = employed * (wage * p.ins.lifePremiumRate)
    val nonLifePrem = employed * (wage * p.ins.nonLifePremiumRate)

    // Claims: life steady, non-life widens with unemployment stress
    val lifeCl      = lifePrem * p.ins.lifeLossRatio
    val nonLifeBase = nonLifePrem * p.ins.nonLifeLossRatio
    val stressGap   = (unempRate - Share(NonLifeUnempThreshold)).max(Share.Zero)
    val stressAdj   = (stressGap * p.ins.nonLifeUnempSens).toMultiplier // Share * Coefficient → Coefficient → Multiplier
    val nonLifeCl   = nonLifeBase * (Multiplier.One + stressAdj)

    // Investment income from all three asset classes
    val grossInvestmentIncome = prevStock.govBondHoldings * govBondYield.monthly +
      prevStock.corpBondHoldings * corpBondYield.monthly +
      prevStock.equityHoldings * equityReturn
    val invIncome             = grossInvestmentIncome - corpBondDefaultLoss

    // Net deposit change: premium outflow from HH minus claims inflow to HH
    val netDepositChange = -(lifePrem + nonLifePrem - lifeCl - nonLifeCl)

    // Update reserves: split investment income proportionally
    val totalReserves = prevStock.totalReserves
    val lifeShare     = if totalReserves > PLN.Zero then Share(prevStock.lifeReserves / totalReserves) else Share(0.5)
    val newLifeRes    = prevStock.lifeReserves + (lifePrem - lifeCl) + invIncome * lifeShare
    val newNonLifeRes = prevStock.nonLifeReserves + (nonLifePrem - nonLifeCl) + invIncome * (Share.One - lifeShare)

    // Rebalance towards target allocation
    val totalAssets = newLifeRes + newNonLifeRes
    val speed       = p.ins.rebalanceSpeed // Coefficient used as adjustment speed
    val targetGov   = totalAssets * p.ins.govBondShare
    val targetEq    = totalAssets * p.ins.equityShare
    val newGov      = prevStock.govBondHoldings + (targetGov - prevStock.govBondHoldings) * speed
    val newEq       = prevStock.equityHoldings + (targetEq - prevStock.equityHoldings) * speed

    StepResult(
      state = State(
        lifePrem,
        nonLifePrem,
        lifeCl,
        nonLifeCl,
        invIncome,
        netDepositChange,
      ),
      stock = StockState(
        newLifeRes,
        newNonLifeRes,
        newGov,
        settledCorpBondHoldings,
        newEq,
      ),
    )
