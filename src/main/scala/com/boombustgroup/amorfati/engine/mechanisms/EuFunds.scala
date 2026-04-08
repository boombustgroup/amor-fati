package com.boombustgroup.amorfati.engine.mechanisms

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.math.EuFundsMath
import com.boombustgroup.amorfati.types.*

/** EU structural funds absorption with Beta-curve timing.
  *
  * Models the disbursement profile of EU cohesion/structural funds over a
  * multi-year programming period. Absorption follows a Beta(α,β) density
  * (front-loaded when α < β, matching empirical EU fund draw-down patterns).
  *
  * Calibration: MFiPR KPO/FENIKS 2021-2027 programming period. Total envelope:
  * 76 bln EUR, period: 84 months, α=2, β=5 (peak ~20% into the period),
  * co-financing rate: 15%, capital share of spending: 60%.
  *
  * The total is scaled by (firmsCount / ReferenceEconomy) to keep per-firm
  * flows proportional regardless of simulation population size.
  */
object EuFunds:

  private val ReferenceEconomy = 10000 // baseline firm count for calibration scaling

  /** Monthly EU transfer in PLN, following a Beta(α,β) absorption curve. */
  def monthlyTransfer(month: Int)(using p: SimParams): PLN =
    val totalPln = EuFundsMath.totalEnvelopePln(
      p.fiscal.euFundsTotalEur,
      p.forex.baseExRate,
      p.pop.firmsCount,
      ReferenceEconomy,
    )
    totalPln * EuFundsMath.monthlyWeight(
      month,
      p.fiscal.euFundsStartMonth,
      p.fiscal.euFundsPeriodMonths,
      p.fiscal.euFundsAlpha,
      p.fiscal.euFundsBeta,
    )

  /** Domestic co-financing from gov budget: cofin = eu × rate / (1 − rate). */
  def cofinancing(euMonthly: PLN)(using p: SimParams): PLN =
    val cofinanceRate = p.fiscal.euCofinanceRate
    if cofinanceRate >= Share.One then PLN.Zero
    else euMonthly * cofinanceRate / cofinanceRate.complement

  /** Capital portion of total EU project spending (EU + cofin). */
  def capitalInvestment(euMonthly: PLN, cofin: PLN)(using p: SimParams): PLN =
    (euMonthly + cofin) * p.fiscal.euCapitalShare
