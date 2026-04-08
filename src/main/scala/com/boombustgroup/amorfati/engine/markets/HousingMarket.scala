package com.boombustgroup.amorfati.engine.markets

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.Distribute

/** Residential real estate market: prices, mortgages, wealth effects, and
  * regional disaggregation.
  *
  * Meen-type housing price model (Meen 2002): ΔP/P = α×(ΔY/Y) + β×Δr +
  * γ×(P*−P)/P*, where P* = annualRent / (mortgageRate − incomeGrowth). Mortgage
  * origination subject to LTV limits (KNF Recommendation S), defaults with
  * unemployment sensitivity, and housing wealth effects on consumption (Case,
  * Quigley & Shiller 2005).
  *
  * Optional 7-region disaggregation: Warsaw, Kraków, Wrocław, Gdańsk, Łódź,
  * Poznań, rest-of-Poland.
  *
  * Calibration: NBP residential price survey 2024, KNF Recommendation S, GUS
  * wage surveys 2024.
  */
object HousingMarket:

  private val MinHousingValueForIncomeRatio = PLN.fromLong(1000)

  val NRegions = 7

  case class RegionalState(
      priceIndex: PriceIndex,
      totalValue: PLN,
      mortgageStock: PLN,
      lastOrigination: PLN,
      lastRepayment: PLN,
      lastDefault: PLN,
      monthlyReturn: Rate,
  )

  case class State(
      priceIndex: PriceIndex,
      totalValue: PLN,
      mortgageStock: PLN,
      avgMortgageRate: Rate,
      hhHousingWealth: PLN,
      lastOrigination: PLN,
      lastRepayment: PLN,
      lastDefault: PLN,
      lastWealthEffect: PLN,
      monthlyReturn: Rate,
      mortgageInterestIncome: PLN,
      regions: Option[Vector[RegionalState]] = None,
  )

  case class MortgageFlows(
      interest: PLN,
      principal: PLN,
      defaultAmount: PLN,
      defaultLoss: PLN,
  )

  case class StepInput(
      prev: State,
      mortgageRate: Rate,
      inflation: Rate,
      incomeGrowth: Rate,
      employed: Int,
      prevMortgageRate: Rate,
  )

  private case class PriceUpdate(value: PLN, hpi: PriceIndex, monthlyReturn: Rate)

  def zero: State = State(
    priceIndex = PriceIndex.Zero,
    totalValue = PLN.Zero,
    mortgageStock = PLN.Zero,
    avgMortgageRate = Rate.Zero,
    hhHousingWealth = PLN.Zero,
    lastOrigination = PLN.Zero,
    lastRepayment = PLN.Zero,
    lastDefault = PLN.Zero,
    lastWealthEffect = PLN.Zero,
    monthlyReturn = Rate.Zero,
    mortgageInterestIncome = PLN.Zero,
    regions = None,
  )

  def initial(using p: SimParams): State =
    State(
      priceIndex = p.housing.initHpi,
      totalValue = p.housing.initValue,
      mortgageStock = p.housing.initMortgage,
      avgMortgageRate = p.monetary.initialRate + p.housing.mortgageSpread,
      hhHousingWealth = p.housing.initValue - p.housing.initMortgage,
      lastOrigination = PLN.Zero,
      lastRepayment = PLN.Zero,
      lastDefault = PLN.Zero,
      lastWealthEffect = PLN.Zero,
      monthlyReturn = Rate.Zero,
      mortgageInterestIncome = PLN.Zero,
      regions = Some(initRegions),
    )

  private def initRegions(using p: SimParams): Vector[RegionalState] =
    (0 until NRegions)
      .map: r =>
        RegionalState(
          priceIndex = p.housing.regionalHpi(r),
          totalValue = p.housing.initValue * p.housing.regionalValueShares(r),
          mortgageStock = p.housing.initMortgage * p.housing.regionalMortgageShares(r),
          lastOrigination = PLN.Zero,
          lastRepayment = PLN.Zero,
          lastDefault = PLN.Zero,
          monthlyReturn = Rate.Zero,
        )
      .toVector

  def step(in: StepInput)(using p: SimParams): State =
    val rateChange = in.mortgageRate - in.prevMortgageRate
    in.prev.regions match
      case Some(regs) => stepRegional(in, regs, rateChange)
      case None       => stepAggregate(in, rateChange)

  private def stepRegional(
      in: StepInput,
      regs: Vector[RegionalState],
      rateChange: Rate,
  )(using p: SimParams): State =
    val updatedRegions = regs.zipWithIndex.map: (reg, r) =>
      val gamma          = p.housing.regionalGammas(r)
      val regionalGrowth = in.incomeGrowth * p.housing.regionalIncomeMult(r)
      val update         = meenPriceUpdate(reg.totalValue, reg.priceIndex, gamma, regionalGrowth, rateChange, in.mortgageRate)
      reg.copy(priceIndex = update.hpi, totalValue = update.value, monthlyReturn = update.monthlyReturn)
    aggregateFromRegions(in.prev, updatedRegions, in.mortgageRate)

  private def stepAggregate(in: StepInput, rateChange: Rate)(using p: SimParams): State =
    val update = meenPriceUpdate(
      in.prev.totalValue,
      in.prev.priceIndex,
      p.housing.priceReversion,
      in.incomeGrowth,
      rateChange,
      in.mortgageRate,
    )
    in.prev.copy(
      priceIndex = update.hpi,
      totalValue = update.value,
      monthlyReturn = update.monthlyReturn,
      avgMortgageRate = in.mortgageRate,
    )

  private def aggregateFromRegions(
      prev: State,
      regions: Vector[RegionalState],
      mortgageRate: Rate,
  )(using p: SimParams): State =
    val aggValue  = sumPln(regions.map(_.totalValue))
    val aggHpi    =
      if aggValue > PLN.Zero then
        regions.zipWithIndex
          .foldLeft(Multiplier.Zero): (acc, regR) =>
            val (reg, r) = regR
            acc + (p.housing.regionalValueShares(r) * reg.priceIndex.toMultiplier)
          .toPriceIndex
      else prev.priceIndex
    val aggReturn =
      if prev.priceIndex > PriceIndex.Zero then aggHpi.ratioTo(prev.priceIndex).toMultiplier.deviationFromOne.toRate
      else Rate.Zero
    prev.copy(
      priceIndex = aggHpi,
      totalValue = aggValue,
      monthlyReturn = aggReturn,
      avgMortgageRate = mortgageRate,
      regions = Some(regions),
    )

  private def meenPriceUpdate(
      prevValue: PLN,
      prevHpi: PriceIndex,
      gamma: Coefficient,
      incomeGrowth: Rate,
      rateChange: Rate,
      mortgageRate: Rate,
  )(using p: SimParams): PriceUpdate =
    val annualRent       = prevValue * p.housing.rentalYield
    val effectiveRate    = mortgageRate.max(Rate(0.01))
    val expectedGrowth   = incomeGrowth.annualize.max(Rate(-0.05)).min(effectiveRate - Rate(0.005))
    val denominator      = effectiveRate - expectedGrowth
    val fundamentalValue =
      if denominator > Rate(0.005) then annualRent / denominator.toMultiplier
      else prevValue
    val monthlyGamma     = gamma / 12
    val fundamentalGap   =
      if fundamentalValue > PLN.fromLong(1) then (fundamentalValue - prevValue).ratioTo(fundamentalValue).toCoefficient
      else Coefficient.Zero
    val pricePressure    =
      (p.housing.priceIncomeElast * incomeGrowth.toCoefficient)
        .max(Coefficient(-10.0))
        .min(Coefficient(10.0)) +
        (p.housing.priceRateElast * rateChange.toCoefficient).max(Coefficient(-10.0)).min(Coefficient(10.0)) +
        (monthlyGamma * fundamentalGap)
    val clampedChange    = pricePressure.max(Coefficient(-0.03)).min(Coefficient(0.03))
    val newValue         = (prevValue * clampedChange.growthMultiplier).max(prevValue * Multiplier(0.30))
    val newHpi           =
      if prevValue > PLN.Zero then prevHpi * newValue.ratioTo(prevValue).toMultiplier
      else prevHpi
    val mReturn          =
      if prevHpi > PriceIndex.Zero then newHpi.ratioTo(prevHpi).toMultiplier.deviationFromOne.toRate
      else Rate.Zero
    PriceUpdate(newValue, newHpi, mReturn)

  def processOrigination(prev: State, totalIncome: PLN, mortgageRate: Rate, bankCapacity: Boolean)(using p: SimParams): State =
    if !bankCapacity then
      prev.copy(
        lastOrigination = PLN.Zero,
        regions = prev.regions.map(_.map(_.copy(lastOrigination = PLN.Zero))),
      )
    else
      val rawOrigination = computeRawOrigination(prev, totalIncome, mortgageRate)
      val headroom       = (prev.totalValue * p.housing.ltvMax - prev.mortgageStock).max(PLN.Zero)
      val origination    = rawOrigination.min(headroom)
      prev.regions match
        case Some(regs) => distributeOrigination(prev, regs, origination)
        case None       =>
          prev.copy(
            mortgageStock = prev.mortgageStock + origination,
            lastOrigination = origination,
          )

  private def computeRawOrigination(prev: State, totalIncome: PLN, mortgageRate: Rate)(using p: SimParams): PLN =
    val baseOrigination = prev.totalValue * p.housing.originationRate
    val rateAdj         =
      (-(mortgageRate - Rate(0.06)).toCoefficient * Coefficient(0.5)).growthMultiplier
        .clamp(Multiplier(0.3), Multiplier(2.0))
    val incomeBase      =
      if prev.totalValue >= MinHousingValueForIncomeRatio then prev.totalValue
      else MinHousingValueForIncomeRatio
    val incomeAdj       =
      (totalIncome.ratioTo(incomeBase).toCoefficient * Coefficient(10.0)).growthMultiplier
        .clamp(Multiplier(0.5), Multiplier(1.5))
    (baseOrigination * rateAdj) * incomeAdj

  private def distributeOrigination(
      prev: State,
      regs: Vector[RegionalState],
      origination: PLN,
  )(using p: SimParams): State =
    val distributed         = Distribute.distribute(origination.distributeRaw, p.housing.regionalValueShares.map(_.distributeRaw).toArray)
    val updatedRegions      = regs
      .zip(distributed.iterator)
      .map: (reg, allocatedRaw) =>
        val regionalRaw      = PLN.fromRaw(allocatedRaw)
        val regionalHeadroom = (reg.totalValue * p.housing.ltvMax - reg.mortgageStock).max(PLN.Zero)
        val regionalOrig     = regionalRaw.min(regionalHeadroom)
        reg.copy(
          mortgageStock = reg.mortgageStock + regionalOrig,
          lastOrigination = regionalOrig,
        )
    val realizedOrigination = updatedRegions.foldLeft(PLN.Zero)(_ + _.lastOrigination)
    prev.copy(
      mortgageStock = prev.mortgageStock + realizedOrigination,
      lastOrigination = realizedOrigination,
      regions = Some(updatedRegions),
    )

  def processMortgageFlows(prev: State, mortgageRate: Rate, unemploymentRate: Share)(using p: SimParams): MortgageFlows =
    if prev.mortgageStock <= PLN.Zero
    then MortgageFlows(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
    else
      val stock         = prev.mortgageStock
      val interest      = stock * mortgageRate.max(Rate.Zero).monthly
      val principal     = stock / p.housing.mortgageMaturity
      val unempExcess   = (unemploymentRate - Share(0.05)).max(Share.Zero)
      val stressAdj     = (p.housing.defaultUnempSens * unempExcess).toMultiplier
      val defaultRate   = p.housing.defaultBase + stressAdj.toShare
      val defaultAmount = stock * defaultRate
      val defaultLoss   = defaultAmount * (Share.One - p.housing.mortgageRecovery)
      MortgageFlows(interest, principal, defaultAmount, defaultLoss)

  def applyFlows(prev: State, flows: MortgageFlows)(using p: SimParams): State =
    val newStock     = PLN.Zero.max(prev.mortgageStock - flows.principal - flows.defaultAmount)
    val newHhWealth  = prev.totalValue - newStock
    val wealthChange = newHhWealth - prev.hhHousingWealth
    val wealthEffect =
      if wealthChange > PLN.Zero then wealthChange * p.housing.wealthMpc
      else PLN.Zero
    prev.copy(
      mortgageStock = newStock,
      hhHousingWealth = newHhWealth,
      lastRepayment = flows.principal,
      lastDefault = flows.defaultAmount,
      lastWealthEffect = wealthEffect,
      mortgageInterestIncome = flows.interest,
      regions = prev.regions.map(distributeFlows(_, flows, prev.mortgageStock)),
    )

  private def distributeFlows(
      regs: Vector[RegionalState],
      flows: MortgageFlows,
      totalPrevStock: PLN,
  ): Vector[RegionalState] =
    if totalPrevStock <= PLN.Zero then regs
    else
      val weights         = regs.map(_.mortgageStock.distributeRaw).toArray
      val distributedPrin = Distribute.distribute(flows.principal.distributeRaw, weights)
      val distributedDef  = Distribute.distribute(flows.defaultAmount.distributeRaw, weights)
      regs.zip(distributedPrin.iterator).zip(distributedDef.iterator).map { case ((reg, principalRaw), defaultRaw) =>
        val rPrincipal = PLN.fromRaw(principalRaw)
        val rDefault   = PLN.fromRaw(defaultRaw)
        val rStock     = (reg.mortgageStock - rPrincipal - rDefault).max(PLN.Zero)
        reg.copy(
          mortgageStock = rStock,
          lastRepayment = rPrincipal,
          lastDefault = rDefault,
        )
      }

  private def sumPln(values: Vector[PLN]): PLN =
    values.foldLeft(PLN.Zero)(_ + _)
