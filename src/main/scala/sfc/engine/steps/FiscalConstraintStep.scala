package sfc.engine.steps

import sfc.accounting.GovState
import sfc.agents.Banking
import sfc.config.Config
import sfc.engine.YieldCurve
import sfc.types.*

object FiscalConstraintStep:

  case class Input(
    month: Int,
    gdpProxy: Double,
    gov: GovState,
    priceLevel: Double,
    minWageLevel: PLN,
    minWagePriceLevel: Double,
    marketWage: PLN,
    bankingSector: Banking.State,
    nbpReferenceRate: Double,
    expectedRate: Double,
    bdpAmount: Double,
    isEurozone: Boolean,
  )

  case class Output(
    m: Int,
    bdpActive: Boolean,
    bdp: Double,
    baseMinWage: Double,
    updatedMinWagePriceLevel: Double,
    resWage: Double,
    lendingBaseRate: Double,
  )

  def run(in: Input): Output =
    val m = in.month + 1
    val bdpActive = m >= Config.ShockMonth

    val bdpUnconstrained = if bdpActive then in.bdpAmount else 0.0
    val bdp = if in.isEurozone && bdpActive && bdpUnconstrained > 0 then
      val annualGdp = in.gdpProxy * 12.0
      val maxMonthlyDeficit = Config.SgpDeficitLimit * annualGdp / 12.0
      val baseGovSpend = Config.GovBaseSpending * in.priceLevel
      val estRevenue = in.gov.taxRevenue
      val maxBdpSpend = Math.max(0.0, maxMonthlyDeficit + estRevenue.toDouble - baseGovSpend)
      val maxBdpPerCapita = maxBdpSpend / Config.TotalPopulation.toDouble
      val debtRatio = if annualGdp > 0 then in.gov.cumulativeDebt.toDouble / annualGdp else 0.0
      val austerityMult =
        if debtRatio > Config.SgpDebtLimit then
          Math.max(0.0, 1.0 - (debtRatio - Config.SgpDebtLimit) * Config.SgpAusterityRate)
        else 1.0
      Math.max(0.0, Math.min(bdpUnconstrained, maxBdpPerCapita * austerityMult))
    else bdpUnconstrained

    val (baseMinWage, updatedMinWagePriceLevel) = if Config.MinWageEnabled then
      val isAdjustMonth = m > 0 && m % Config.MinWageAdjustMonths == 0
      if isAdjustMonth then
        val cumInfl =
          if Config.MinWageInflationIndex && in.minWagePriceLevel > 0 then in.priceLevel / in.minWagePriceLevel - 1.0
          else 0.0
        val inflIndexed = in.minWageLevel.toDouble * (1.0 + Math.max(0.0, cumInfl))
        val target = in.marketWage.toDouble * Config.MinWageTargetRatio
        val gap = target - inflIndexed
        val adjusted =
          if gap > 0 then inflIndexed + gap * Config.MinWageConvergenceSpeed
          else inflIndexed
        (Math.max(in.minWageLevel.toDouble, adjusted), in.priceLevel)
      else (in.minWageLevel.toDouble, in.minWagePriceLevel)
    else (Config.BaseReservationWage, in.minWagePriceLevel)

    val resWage = baseMinWage + bdp * Config.ReservationBdpMult

    val rawLendingBaseRate: Double =
      if Config.InterbankTermStructure then YieldCurve.compute(in.bankingSector.interbankRate.toDouble).wibor3m.toDouble
      else in.nbpReferenceRate

    val lendingBaseRate =
      if Config.ExpEnabled then 0.5 * rawLendingBaseRate + 0.5 * in.expectedRate
      else rawLendingBaseRate

    Output(m, bdpActive, bdp, baseMinWage, updatedMinWagePriceLevel, resWage, lendingBaseRate)
