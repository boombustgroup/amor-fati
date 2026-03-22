package com.boombustgroup.amorfati.engine.markets

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

/** Fiscal rule constraints: Art. 216 Konstytucja RP (60% debt/GDP ceiling),
  * Art. 86 uFP (55% cautionary threshold), SRW Art. 112aa uFP (expenditure
  * growth rule), and SGP Pakt Stabilności i Wzrostu (3% deficit/GDP limit).
  *
  * Stateless — pure constraint logic applied to government spending decisions
  * before they flow into FiscalBudget.update.
  */
object FiscalRules:

  /** Inputs for fiscal rule evaluation. */
  case class Input(
      rawGovPurchases: PLN,  // unconstrained gov purchases from DemandStep formula
      prevGovSpend: PLN,     // previous month total gov spending (current + capital)
      cumulativeDebt: PLN,   // current government debt stock
      monthlyGdp: PLN,       // monthly GDP proxy (gdpProxy)
      prevRevenue: PLN,      // previous month total tax revenue
      prevDeficit: PLN,      // previous month budget deficit (single-month flow)
      inflation: Rate,       // current CPI YoY inflation
      outputGap: Coefficient, // (unemp − NAIRU) / NAIRU (positive = slack)
  )

  /** Result of fiscal rule application. */
  case class Output(
      constrainedGovPurchases: PLN, // final gov purchases after all rule constraints
      status: RuleStatus,           // which rules are binding + diagnostics
  )

  /** Rule compliance status for observability (surfaced in SimOutput). */
  case class RuleStatus(
      debtToGdp: Share,       // current debt/GDP ratio
      deficitToGdp: Share,    // current deficit/GDP ratio (annualized)
      srwCeiling: PLN,        // SRW expenditure ceiling this month
      bindingRule: Int,       // 0=none, 1=SRW, 2=SGP, 3=Art86_55, 4=Art216_60
      spendingCutRatio: Share, // fraction of raw spending that was cut (0 = no cut)
  )

  /** Apply fiscal rules in order of severity, taking the most restrictive. */
  def constrain(in: Input)(using p: SimParams): Output =
    val annualGdp    = in.monthlyGdp * 12.0
    val debtToGdp    = if annualGdp > PLN.Zero then Share(in.cumulativeDebt / annualGdp) else Share.Zero
    val deficitToGdp = if annualGdp > PLN.Zero then Share(in.prevDeficit / in.monthlyGdp) else Share.Zero // monthly deficit / monthly GDP = annualized ratio

    // 1. SRW: expenditure growth ceiling with convergence blending
    val srwCeiling = computeSrwCeiling(in)
    val afterSrw   = blendSrw(in.rawGovPurchases, srwCeiling)

    // 2. SGP: cap spending if deficit/GDP > 3%
    val afterSgp = applySgp(afterSrw, in, deficitToGdp)

    // 3. Art. 86 (55%): consolidation cut
    val afterArt86 = applyConsolidation55(afterSgp, debtToGdp)

    // 4. Art. 216 (60%): budget must balance
    val afterArt216 = applyArt216(afterArt86, in, debtToGdp)

    // Determine which rule is binding (most restrictive wins)
    val constrained = afterArt216
    val bindingRule = determineBindingRule(in.rawGovPurchases, afterSrw, afterSgp, afterArt86, afterArt216)
    val cutRatio    =
      if in.rawGovPurchases > PLN.Zero then Share(((in.rawGovPurchases - constrained) / in.rawGovPurchases).max(0.0))
      else Share.Zero

    Output(
      constrainedGovPurchases = constrained,
      status = RuleStatus(debtToGdp, deficitToGdp, srwCeiling, bindingRule, cutRatio),
    )

  /** SRW ceiling: previous spending × (1 + monthly inflation + monthly real cap
    * − output gap correction).
    */
  private def computeSrwCeiling(in: Input)(using p: SimParams): PLN =
    val monthlyInflation = Multiplier(in.inflation.monthly.toDouble)              // Rate → Multiplier: used as growth increment
    val monthlyRealCap   = Multiplier(p.fiscal.srwRealGrowthCap.monthly.toDouble) // Rate → Multiplier: growth allowance
    val gapCorrection    = (in.outputGap * p.fiscal.srwOutputGapSensitivity).monthly
    in.prevGovSpend * (Multiplier.One + monthlyInflation + monthlyRealCap - gapCorrection)

  /** Blend raw spending toward SRW ceiling at convergence speed. */
  private def blendSrw(raw: PLN, ceiling: PLN)(using p: SimParams): PLN =
    val s = p.fiscal.srwCorrectionSpeed.monthly
    raw * (Share.One - s) + ceiling * s

  /** SGP: if annualized deficit/GDP > limit, cap spending at revenue +
    * allowable deficit.
    */
  private def applySgp(spending: PLN, in: Input, deficitToGdp: Share)(using p: SimParams): PLN =
    if deficitToGdp > p.fiscal.sgpDeficitLimit then
      val maxSpend = in.prevRevenue + in.monthlyGdp * p.fiscal.sgpDeficitLimit
      spending.min(maxSpend)
    else spending

  /** Art. 86 uFP (55%): apply consolidation spending cut. */
  private def applyConsolidation55(spending: PLN, debtToGdp: Share)(using p: SimParams): PLN =
    if debtToGdp > p.fiscal.fiscalRuleCautionThreshold then spending * (Share.One - p.fiscal.fiscalConsolidationSpeed55.monthly)
    else spending

  /** Art. 216 (60%): budget must balance — spending capped at revenue. */
  private def applyArt216(spending: PLN, in: Input, debtToGdp: Share)(using p: SimParams): PLN =
    if debtToGdp > p.fiscal.fiscalRuleDebtCeiling then
      val consolidated = spending * (Share.One - p.fiscal.fiscalConsolidationSpeed60.monthly)
      consolidated.min(in.prevRevenue) // hard ceiling: cannot exceed revenue
    else spending

  /** Identify which rule is most restrictive (highest severity binding). */
  private def determineBindingRule(
      raw: PLN,
      afterSrw: PLN,
      afterSgp: PLN,
      afterArt86: PLN,
      afterArt216: PLN,
  ): Int =
    if afterArt216 < afterArt86 then 4   // Art. 216 (60%) binding
    else if afterArt86 < afterSgp then 3 // Art. 86 (55%) binding
    else if afterSgp < afterSrw then 2 // SGP binding
    else if afterSrw < raw then 1 // SRW binding
    else 0 // no rule binding
