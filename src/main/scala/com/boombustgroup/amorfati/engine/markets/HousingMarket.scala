package com.boombustgroup.amorfati.engine.markets

import com.boombustgroup.ledger.Distribute
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

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

  val NRegions = 7

  // --- Data types ---

  /** Per-region housing state: only fields that vary regionally. */
  case class RegionalState(
      priceIndex: Double,
      totalValue: PLN,
      mortgageStock: PLN,
      lastOrigination: PLN,
      lastRepayment: PLN,
      lastDefault: PLN,
      monthlyReturn: Rate,
  )

  /** Housing market state: aggregate HPI, property value, mortgage stock,
    * flows.
    */
  case class State(
      priceIndex: Double,
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

  /** Mortgage flow result: interest, principal, gross default, and net loss. */
  case class MortgageFlows(
      interest: PLN,
      principal: PLN,
      defaultAmount: PLN,
      defaultLoss: PLN,
  )

  /** Price step input. */
  case class StepInput(
      prev: State,
      mortgageRate: Rate,
      inflation: Rate,
      incomeGrowth: Rate,
      employed: Int,
      prevMortgageRate: Rate,
  )

  /** Meen model output for a single price update. */
  private case class PriceUpdate(value: PLN, hpi: Double, monthlyReturn: Rate)

  // --- Constructors ---

  def zero: State = State(
    priceIndex = 0.0,
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

  // --- Price step (Meen model) ---

  /** Update house prices via Meen-type fundamentals model. Returns zero state
    * when housing module is disabled.
    */
  def step(in: StepInput)(using p: SimParams): State =
    val rateChange = in.mortgageRate - in.prevMortgageRate
    in.prev.regions match
      case Some(regs) => stepRegional(in, regs, rateChange)
      case None       => stepAggregate(in, rateChange)

  /** Regional mode: apply Meen model per region, then aggregate. */
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

  /** Aggregate mode: single Meen model for the whole market. */
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

  /** Value-weighted aggregation from regional states. */
  @boundaryEscape
  private def aggregateFromRegions(
      prev: State,
      regions: Vector[RegionalState],
      mortgageRate: Rate,
  )(using p: SimParams): State =
    import ComputationBoundary.toDouble
    val aggValue  = PLN.fromRaw(regions.map(_.totalValue.toLong).sum)
    val aggHpi    =
      if aggValue > PLN.Zero then
        regions.zipWithIndex.foldLeft(0.0): (acc, regR) =>
          val (reg, r) = regR
          acc + reg.priceIndex * toDouble(p.housing.regionalValueShares(r))
      else prev.priceIndex
    val aggReturn = if prev.priceIndex > 0 then aggHpi / prev.priceIndex - 1.0 else 0.0
    prev.copy(
      priceIndex = aggHpi,
      totalValue = aggValue,
      monthlyReturn = Rate(aggReturn),
      avgMortgageRate = mortgageRate,
      regions = Some(regions),
    )

  /** Meen model: compute new value, HPI, and monthly return from fundamentals.
    * P* = annualRent / (effectiveRate − expectedGrowth).
    */
  @boundaryEscape
  private def meenPriceUpdate(
      prevValue: PLN,
      prevHpi: Double,
      gamma: Coefficient,
      incomeGrowth: Rate,
      rateChange: Rate,
      mortgageRate: Rate,
  )(using p: SimParams): PriceUpdate =
    import ComputationBoundary.toDouble
    val prevVal          = toDouble(prevValue)
    val annualRent       = prevVal * toDouble(p.housing.rentalYield)
    val effectiveRate    = Math.max(0.01, toDouble(mortgageRate))
    val incGrowthD       = toDouble(incomeGrowth)
    val expectedGrowth   = Math.max(-0.05, Math.min(effectiveRate - 0.005, incGrowthD * 12.0))
    val denominator      = effectiveRate - expectedGrowth
    val fundamentalValue =
      if denominator > 0.005 then annualRent / denominator
      else prevVal
    val monthlyGamma     = toDouble(gamma) / 12.0
    val pricePressure    = Math.fma(
      toDouble(p.housing.priceIncomeElast),
      incGrowthD,
      Math.fma(
        toDouble(p.housing.priceRateElast),
        toDouble(rateChange),
        monthlyGamma * (fundamentalValue - prevVal) / Math.max(1.0, fundamentalValue),
      ),
    )
    val clampedChange    = Math.max(-0.03, Math.min(0.03, pricePressure))
    val newValue         = PLN(Math.max(prevVal * 0.30, prevVal * (1.0 + clampedChange)))
    val newHpi           = if prevVal > 0 then prevHpi * (newValue / prevValue) else prevHpi
    val mReturn          = if prevHpi > 0 then newHpi / prevHpi - 1.0 else 0.0
    PriceUpdate(newValue, newHpi, Rate(mReturn))

  // --- Mortgage origination ---

  /** New mortgages issued monthly. Scaled by origination rate, income growth,
    * and rate sensitivity. LTV constraint caps mortgage at ltvMax × property
    * value (KNF Recommendation S).
    */
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

  /** Base origination adjusted for rate and income sensitivity. */
  @boundaryEscape
  private def computeRawOrigination(prev: State, totalIncome: PLN, mortgageRate: Rate)(using p: SimParams): PLN =
    import ComputationBoundary.toDouble
    val baseOrigination = toDouble(prev.totalValue) * toDouble(p.housing.originationRate)
    val rateAdj         = Math.max(0.3, Math.min(2.0, 1.0 - 0.5 * (toDouble(mortgageRate) - 0.06)))
    val incomeAdj       = Math.max(0.5, Math.min(1.5, 1.0 + toDouble(totalIncome) / Math.max(1.0, toDouble(prev.totalValue)) * 10.0))
    PLN(baseOrigination * rateAdj * Math.min(1.5, incomeAdj))

  /** Distribute origination proportionally to regional value share, per-region
    * LTV cap.
    */
  private def distributeOrigination(
      prev: State,
      regs: Vector[RegionalState],
      origination: PLN,
  )(using p: SimParams): State =
    val distributed    = Distribute.distribute(origination.toLong, p.housing.regionalValueShares.map(_.toLong).toArray)
    val updatedRegions = regs
      .zip(distributed.iterator)
      .map: (reg, allocatedRaw) =>
        val regionalRaw      = PLN.fromRaw(allocatedRaw)
        val regionalHeadroom = (reg.totalValue * p.housing.ltvMax - reg.mortgageStock).max(PLN.Zero)
        val regionalOrig     = regionalRaw.min(regionalHeadroom)
        reg.copy(
          mortgageStock = reg.mortgageStock + regionalOrig,
          lastOrigination = regionalOrig,
        )
    // Aggregate uses exact origination (not sum of regional rounded shares)
    prev.copy(mortgageStock = prev.mortgageStock + origination, lastOrigination = origination, regions = Some(updatedRegions))

  // --- Mortgage flows ---

  /** Compute interest, principal repayment, and defaults from mortgage stock.
    * Default rate rises with unemployment (sensitivity above 5% threshold).
    * Returns both gross defaultAmount (for stock reduction) and net defaultLoss
    * (after recovery, for bank P&L).
    */
  def processMortgageFlows(prev: State, mortgageRate: Rate, unemploymentRate: Share)(using p: SimParams): MortgageFlows =
    if prev.mortgageStock <= PLN.Zero
    then MortgageFlows(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
    else
      val stock         = prev.mortgageStock
      val interest      = stock * mortgageRate.max(Rate.Zero).monthly
      val principal     = stock / p.housing.mortgageMaturity.toLong
      val unempExcess   = (unemploymentRate - Share(0.05)).max(Share.Zero)
      val stressAdj     = (p.housing.defaultUnempSens * unempExcess).toMultiplier // Coefficient × Share → Coefficient → Multiplier
      val defaultRate   = p.housing.defaultBase + stressAdj.toShare
      val defaultAmount = stock * defaultRate
      val defaultLoss   = defaultAmount * (Share.One - p.housing.mortgageRecovery)
      MortgageFlows(interest, principal, defaultAmount, defaultLoss)

  // --- Apply flows ---

  /** Update mortgage stock after flows (principal repayment + defaults).
    * Computes housing wealth effect on consumption.
    */
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

  /** Distribute principal/default flows to regions proportional to mortgage
    * stock share.
    */
  private def distributeFlows(
      regs: Vector[RegionalState],
      flows: MortgageFlows,
      totalPrevStock: PLN,
  ): Vector[RegionalState] =
    if totalPrevStock <= PLN.Zero then regs
    else
      val weights         = regs.map(_.mortgageStock.toLong).toArray
      val distributedPrin = Distribute.distribute(flows.principal.toLong, weights)
      val distributedDef  = Distribute.distribute(flows.defaultAmount.toLong, weights)
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
