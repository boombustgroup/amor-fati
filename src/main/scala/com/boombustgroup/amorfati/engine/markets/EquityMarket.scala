package com.boombustgroup.amorfati.engine.markets

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

/** GPW equity market: WIG index, market cap, dividends, foreign ownership.
  *
  * Index tracks fundamental value via Gordon growth model (Gordon 1959): P = D
  * / (r - g), where D = dividend yield × index, r = refRate + equity risk
  * premium, g = expected earnings growth (GDP proxy). Monthly partial
  * adjustment smooths convergence.
  *
  * Issuance (IPO/SPO) increases market cap with proportional index dilution.
  * Dividends split between domestic (subject to Belka 19% PIT) and foreign
  * holders. Foreign ownership mean-reverts to calibrated share.
  *
  * Calibration: GPW market data 2024, NBP BoP statistics.
  */
object EquityMarket:

  // --- Named constants ---
  private val EquityRiskPremium      = Rate(0.05)        // GPW historical average
  private val MinDiscountRate        = Rate(0.02)        // floor to avoid near-zero discount
  private val GrowthFloor            = Rate(-0.10)       // max annualized contraction
  private val GordonSingularityGuard = Rate(0.005)       // min denominator (r - g) to avoid explosion
  private val AdjustmentSpeed        = Share(0.15)       // monthly partial adjustment to fundamental
  private val MinIndex               = PriceIndex(100.0) // index floor
  private val EarningsYieldFloor     = Rate(0.01)        // E/P floor (P/E = 100)
  private val EarningsYieldCap       = Rate(0.50)        // E/P cap (P/E = 2)
  private val PayoutRatio            = Share(0.57)       // GPW average payout ratio
  private val DivYieldSmoothing      = Share(0.10)       // weight on implied div yield (1-α on prev)
  private val ForeignReversionSpeed  = Share(0.01)       // monthly mean-reversion speed

  /** GPW equity market state: aggregate index, market cap, yields, foreign
    * ownership.
    */
  case class State(
      index: PriceIndex,
      marketCap: PLN,
      earningsYield: Rate,
      dividendYield: Rate,
      foreignOwnership: Share,
      lastIssuance: PLN = PLN.Zero,
      lastDomesticDividends: PLN = PLN.Zero,
      lastForeignDividends: PLN = PLN.Zero,
      lastDividendTax: PLN = PLN.Zero,
      hhEquityWealth: PLN = PLN.Zero,
      lastWealthEffect: PLN = PLN.Zero,
      monthlyReturn: Rate = Rate.Zero,
  )

  def zero: State = State(
    index = PriceIndex.Zero,
    marketCap = PLN.Zero,
    earningsYield = Rate.Zero,
    dividendYield = Rate.Zero,
    foreignOwnership = Share.Zero,
  )

  def initial(using p: SimParams): State = State(
    index = p.equity.initIndex,
    marketCap = p.equity.initMcap,
    earningsYield = p.equity.peMean.reciprocal.toRate,
    dividendYield = p.equity.divYield,
    foreignOwnership = p.equity.foreignShare,
  )

  /** Monthly equity market step using Gordon growth model for equilibrium
    * price. P = D / (r - g), where: D = dividend yield x current index, r =
    * discount rate = refRate + equity risk premium, g = expected earnings
    * growth (proxy: gdpGrowth).
    */
  case class StepInput(
      prev: State,
      refRate: Rate,
      inflation: Rate,
      gdpGrowth: Coefficient,
      firmProfits: PLN,
  )

  def step(in: StepInput)(using p: SimParams): State =
    val discountRate   = (in.refRate + EquityRiskPremium).max(MinDiscountRate)
    val growthCap      = discountRate - GordonSingularityGuard
    val expectedGrowth = in.gdpGrowth.toRate.annualize.clamp(GrowthFloor, growthCap)

    // Gordon growth fundamental value
    val dividend    = in.prev.index * in.prev.dividendYield
    val denominator = discountRate - expectedGrowth
    val gordonIndex =
      if denominator > GordonSingularityGuard then dividend / denominator
      else in.prev.index

    val retainedWeight = Share.One - AdjustmentSpeed
    val newIndex       =
      ((in.prev.index * retainedWeight.toMultiplier) + (gordonIndex * AdjustmentSpeed.toMultiplier)).max(MinIndex)

    // Market cap scales with index
    val indexReturn  = if in.prev.index > PriceIndex.Zero then newIndex.ratioTo(in.prev.index).toMultiplier else Multiplier.One
    val newMarketCap = (in.prev.marketCap * indexReturn).max(PLN.Zero)

    // Earnings yield from firm profits and market cap
    val annualProfits    = in.firmProfits * 12
    val newEarningsYield =
      if newMarketCap > PLN.Zero then annualProfits.ratioTo(newMarketCap).toRate.clamp(EarningsYieldFloor, EarningsYieldCap)
      else in.prev.earningsYield

    // Dividend yield: payout ratio x earnings yield (mean-reverting to calibrated)
    val impliedDivYield = newEarningsYield * PayoutRatio
    val newDivYield     =
      (in.prev.dividendYield * (Share.One - DivYieldSmoothing).toMultiplier) + (impliedDivYield * DivYieldSmoothing)

    // Foreign ownership: slow-moving, mean-reverting to calibrated share
    val newForeignOwnership =
      (in.prev.foreignOwnership * (Share.One - ForeignReversionSpeed)) + (p.equity.foreignShare * ForeignReversionSpeed)

    val mReturn = if in.prev.index > PriceIndex.Zero then newIndex.ratioTo(in.prev.index).toMultiplier.deviationFromOne.toRate else Rate.Zero

    State(newIndex, newMarketCap, newEarningsYield, newDivYield, newForeignOwnership, monthlyReturn = mReturn)

  /** Process equity issuance: firm raises CAPEX via equity, increasing market
    * cap. Index diluted by supply effect.
    */
  def processIssuance(amount: PLN, prev: State): State =
    if amount <= PLN.Zero then prev.copy(lastIssuance = PLN.Zero)
    else
      val dilutionFactor = prev.marketCap.ratioTo(prev.marketCap + amount).toMultiplier
      prev.copy(
        marketCap = prev.marketCap + amount,
        index = prev.index * dilutionFactor,
        lastIssuance = amount,
      )

  /** @param netDomestic
    *   net domestic dividends (after Belka tax)
    * @param foreign
    *   foreign dividend outflow
    * @param tax
    *   Belka tax on domestic dividends (19% PIT)
    */
  case class DividendResult(netDomestic: PLN, foreign: PLN, tax: PLN)

  val DividendResultZero: DividendResult = DividendResult(PLN.Zero, PLN.Zero, PLN.Zero)

  /** Compute cash dividends from realized profits, not directly from market
    * valuation.
    */
  def computeDividends(
      realizedProfits: PLN,
      foreignShare: Share,
  )(using p: SimParams): DividendResult =
    if realizedProfits <= PLN.Zero then DividendResultZero
    else
      val totalDividends   = realizedProfits * PayoutRatio
      val foreignDividends = totalDividends * foreignShare
      val domesticGross    = totalDividends - foreignDividends
      val dividendTax      = domesticGross * p.equity.divTax
      DividendResult(
        netDomestic = domesticGross - dividendTax,
        foreign = foreignDividends,
        tax = dividendTax,
      )
