package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

/** Insurance sector: monthly premiums, claims, investment income and
  * rebalancing. Financial balances are ledger-owned; the agent only receives an
  * opening view and returns the non-corporate-bond closing update.
  */
object Insurance:

  // Unemployment threshold below which non-life claims have no cyclical add-on
  private val NonLifeUnempThreshold = 0.05

  case class OpeningBalances(
      lifeReserves: PLN,     // life insurance technical reserves
      nonLifeReserves: PLN,  // non-life insurance technical reserves
      govBondHoldings: PLN,  // government bond allocation
      corpBondHoldings: PLN, // ledger-owned corporate bond opening stock
      equityHoldings: PLN,   // equity allocation (GPW)
  ):
    def totalReserves: PLN = lifeReserves + nonLifeReserves

  case class ClosingBalances(
      lifeReserves: PLN,    // life insurance technical reserves
      nonLifeReserves: PLN, // non-life insurance technical reserves
      govBondHoldings: PLN, // government bond allocation
      equityHoldings: PLN,  // equity allocation (GPW)
  )

  object ClosingBalances:
    val zero: ClosingBalances = ClosingBalances(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)

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

  def initialBalances(using p: SimParams): ClosingBalances =
    val totalAssets = p.ins.lifeReserves + p.ins.nonLifeReserves
    ClosingBalances(
      lifeReserves = p.ins.lifeReserves,
      nonLifeReserves = p.ins.nonLifeReserves,
      govBondHoldings = totalAssets * p.ins.govBondShare,
      equityHoldings = totalAssets * p.ins.equityShare,
    )

  case class StepInput(
      opening: OpeningBalances,
      employed: Int,       // employed workers (premium base)
      wage: PLN,           // average monthly wage
      unempRate: Share,    // unemployment rate (non-life claim cyclicality)
      govBondYield: Rate,  // government bond yield (annualised)
      corpBondYield: Rate, // corporate bond yield (annualised)
      equityReturn: Rate,  // equity monthly return
      corpBondDefaultLoss: PLN,
  )

  case class StepResult(state: State, closing: ClosingBalances)

  /** Full monthly step: premiums, claims, investment income, rebalancing.
    *
    * Corporate bond holdings are settled by CorporateBondMarket and owned by
    * LedgerFinancialState. Insurance only receives the opening corporate-bond
    * stock to compute monthly investment income.
    */
  def step(input: StepInput)(using p: SimParams): StepResult =
    val opening = input.opening

    // Premiums: proportional to wage bill
    val lifePrem    = input.employed * (input.wage * p.ins.lifePremiumRate)
    val nonLifePrem = input.employed * (input.wage * p.ins.nonLifePremiumRate)

    // Claims: life steady, non-life widens with unemployment stress
    val lifeCl      = lifePrem * p.ins.lifeLossRatio
    val nonLifeBase = nonLifePrem * p.ins.nonLifeLossRatio
    val stressGap   = (input.unempRate - Share(NonLifeUnempThreshold)).max(Share.Zero)
    val stressAdj   = (stressGap * p.ins.nonLifeUnempSens).toMultiplier // Share * Coefficient → Coefficient → Multiplier
    val nonLifeCl   = nonLifeBase * (Multiplier.One + stressAdj)

    // Investment income from all three asset classes
    val grossInvestmentIncome = opening.govBondHoldings * input.govBondYield.monthly +
      opening.corpBondHoldings * input.corpBondYield.monthly +
      opening.equityHoldings * input.equityReturn
    val invIncome             = grossInvestmentIncome - input.corpBondDefaultLoss

    // Net deposit change: premium outflow from HH minus claims inflow to HH
    val netDepositChange = -(lifePrem + nonLifePrem - lifeCl - nonLifeCl)

    // Update reserves: split investment income proportionally
    val totalReserves = opening.totalReserves
    val lifeShare     = if totalReserves > PLN.Zero then Share(opening.lifeReserves / totalReserves) else Share(0.5)
    val newLifeRes    = opening.lifeReserves + (lifePrem - lifeCl) + invIncome * lifeShare
    val newNonLifeRes = opening.nonLifeReserves + (nonLifePrem - nonLifeCl) + invIncome * (Share.One - lifeShare)

    // Rebalance towards target allocation
    val totalAssets = newLifeRes + newNonLifeRes
    val speed       = p.ins.rebalanceSpeed // Coefficient used as adjustment speed
    val targetGov   = totalAssets * p.ins.govBondShare
    val targetEq    = totalAssets * p.ins.equityShare
    val newGov      = opening.govBondHoldings + (targetGov - opening.govBondHoldings) * speed
    val newEq       = opening.equityHoldings + (targetEq - opening.equityHoldings) * speed

    StepResult(
      state = State(
        lifePrem,
        nonLifePrem,
        lifeCl,
        nonLifeCl,
        invIncome,
        netDepositChange,
      ),
      closing = ClosingBalances(
        newLifeRes,
        newNonLifeRes,
        newGov,
        newEq,
      ),
    )
