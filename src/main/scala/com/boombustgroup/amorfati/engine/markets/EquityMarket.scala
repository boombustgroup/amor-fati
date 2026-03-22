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
  private val EquityRiskPremium      = 0.05  // GPW historical average
  private val MinDiscountRate        = 0.02  // floor to avoid near-zero discount
  private val GrowthFloor            = -0.10 // max annualized contraction
  private val GordonSingularityGuard = 0.005 // min denominator (r - g) to avoid explosion
  private val AdjustmentSpeed        = 0.15  // monthly partial adjustment to fundamental
  private val MinIndex               = 100.0 // index floor
  private val MonthsPerYear          = 12.0  // annual-to-monthly conversion
  private val EarningsYieldFloor     = 0.01  // E/P floor (P/E = 100)
  private val EarningsYieldCap       = 0.50  // E/P cap (P/E = 2)
  private val PayoutRatio            = 0.57  // GPW average payout ratio
  private val DivYieldSmoothing      = 0.10  // weight on implied div yield (1-α on prev)
  private val ForeignReversionSpeed  = 0.01  // monthly mean-reversion speed

  /** GPW equity market state: aggregate index, market cap, yields, foreign
    * ownership.
    */
  case class State(
      index: Double,
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
    index = 0.0,
    marketCap = PLN.Zero,
    earningsYield = Rate.Zero,
    dividendYield = Rate.Zero,
    foreignOwnership = Share.Zero,
  )

  def initial(using p: SimParams): State = State(
    index = p.equity.initIndex,
    marketCap = p.equity.initMcap,
    earningsYield = Rate(1.0 / p.equity.peMean),
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
      gdpGrowth: Double,
      firmProfits: PLN,
  )

  @computationBoundary
  def step(in: StepInput)(using p: SimParams): State =
    import ComputationBoundary.toDouble
    if !p.flags.gpw then zero
    else
      val discountRate   = Math.max(MinDiscountRate, toDouble(in.refRate) + EquityRiskPremium)
      val growthCap      = discountRate - GordonSingularityGuard
      val expectedGrowth = Math.max(GrowthFloor, Math.min(growthCap, in.gdpGrowth * MonthsPerYear))

      // Gordon growth fundamental value
      val dividend    = toDouble(in.prev.dividendYield) * in.prev.index
      val denominator = discountRate - expectedGrowth
      val gordonIndex =
        if denominator > GordonSingularityGuard then dividend / denominator
        else in.prev.index

      val newIndex = Math.max(MinIndex, in.prev.index + AdjustmentSpeed * (gordonIndex - in.prev.index))

      // Market cap scales with index
      val indexReturn  = if in.prev.index > 0 then newIndex / in.prev.index else 1.0
      val newMarketCap = (in.prev.marketCap * Multiplier(indexReturn)).max(PLN.Zero)

      // Earnings yield from firm profits and market cap
      val annualProfits    = in.firmProfits * Multiplier(MonthsPerYear)
      val newEarningsYield = Rate(
        if newMarketCap > PLN.Zero then Math.max(EarningsYieldFloor, Math.min(EarningsYieldCap, annualProfits / newMarketCap))
        else toDouble(in.prev.earningsYield),
      )

      // Dividend yield: payout ratio x earnings yield (mean-reverting to calibrated)
      val impliedDivYield = toDouble(newEarningsYield) * PayoutRatio
      val newDivYield     = in.prev.dividendYield * Multiplier(1.0 - DivYieldSmoothing) + Rate(impliedDivYield * DivYieldSmoothing)

      // Foreign ownership: slow-moving, mean-reverting to calibrated share
      val newForeignOwnership =
        in.prev.foreignOwnership * Share(1.0 - ForeignReversionSpeed) + p.equity.foreignShare * Share(ForeignReversionSpeed)

      val mReturn = if in.prev.index > 0 then newIndex / in.prev.index - 1.0 else 0.0

      State(newIndex, newMarketCap, newEarningsYield, newDivYield, newForeignOwnership, monthlyReturn = Rate(mReturn))

  /** Process equity issuance: firm raises CAPEX via equity, increasing market
    * cap. Index diluted by supply effect.
    */
  def processIssuance(amount: PLN, prev: State): State =
    if amount <= PLN.Zero then prev.copy(lastIssuance = PLN.Zero)
    else
      val dilutionFactor = prev.marketCap / (prev.marketCap + amount) // PLN/PLN → Double
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

  /** Compute dividends from market cap and yields. */
  def computeDividends(
      divYield: Rate,
      marketCap: PLN,
      foreignShare: Share,
  )(using p: SimParams): DividendResult =
    if marketCap <= PLN.Zero then DividendResultZero
    else
      val totalDividends   = marketCap * divYield.monthly
      val foreignDividends = totalDividends * foreignShare
      val domesticGross    = totalDividends - foreignDividends
      val dividendTax      = domesticGross * p.equity.divTax
      DividendResult(
        netDomestic = domesticGross - dividendTax,
        foreign = foreignDividends,
        tax = dividendTax,
      )
