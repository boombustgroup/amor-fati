package sfc.engine.markets

import sfc.config.SimParams
import sfc.types.*

object EquityMarket:

  /** GPW equity market state: aggregate index, market cap, yields, foreign
    * ownership.
    */
  case class State(
      index: Double,                         // WIG-like composite index level
      marketCap: PLN,                        // total market capitalization
      earningsYield: Rate,                   // E/P = inverse of P/E
      dividendYield: Rate,                   // dividend / price
      foreignOwnership: Ratio,               // fraction held by foreign investors
      lastIssuance: PLN = PLN.Zero,          // equity issuance this month
      lastDomesticDividends: PLN = PLN.Zero, // net domestic dividends this month
      lastForeignDividends: PLN = PLN.Zero,  // foreign dividend outflow this month
      lastDividendTax: PLN = PLN.Zero,       // dividend tax this month
      hhEquityWealth: PLN = PLN.Zero,        // total HH equity holdings value
      lastWealthEffect: PLN = PLN.Zero,      // wealth effect consumption boost this month
      monthlyReturn: Rate = Rate.Zero,       // index return this month (for HH revaluation next month)
  )
  def zero: State = State(
    0.0,
    PLN.Zero,
    Rate.Zero,
    Rate.Zero,
    Ratio.Zero,
    PLN.Zero,
    PLN.Zero,
    PLN.Zero,
    PLN.Zero,
    PLN.Zero,
    PLN.Zero,
    Rate.Zero,
  )

  def initial(using p: SimParams): State = State(
    index = p.equity.initIndex,
    marketCap = PLN(p.equity.initMcap.toDouble),
    earningsYield = Rate(1.0 / p.equity.peMean),
    dividendYield = Rate(p.equity.divYield.toDouble),
    foreignOwnership = Ratio(p.equity.foreignShare.toDouble),
  )

  /** Monthly equity market step using Gordon growth model for equilibrium
    * price. P = D / (r - g), where: D = dividend yield × current index r =
    * discount rate = refRate + equity risk premium g = expected earnings growth
    * (proxy: gdpGrowth)
    *
    * @param prev
    *   previous equity market state
    * @param refRate
    *   central bank reference rate (annual)
    * @param inflation
    *   current annualized inflation
    * @param gdpGrowth
    *   GDP growth rate (monthly, relative to last period)
    * @param firmProfits
    *   total firm profits this month
    * @return
    *   updated equity market state
    */
  def step(prev: State, refRate: Double, inflation: Double, gdpGrowth: Double, firmProfits: Double)(using
      p: SimParams,
  ): State =
    if !p.flags.gpw then return zero

    // Equity risk premium: mean-reverting around 5% (GPW historical)
    val equityRiskPremium = 0.05
    // Discount rate: risk-free + ERP (annual)
    val discountRate      = Math.max(0.02, refRate + equityRiskPremium)
    // Expected growth: proxy from GDP growth (annualized) capped to avoid singularity
    val expectedGrowth    = Math.max(-0.10, Math.min(discountRate - 0.01, gdpGrowth * 12.0))

    // Gordon growth fundamental value
    val dividend    = prev.dividendYield.toDouble * prev.index
    val denominator = discountRate - expectedGrowth
    val gordonIndex =
      if denominator > 0.005 then dividend / denominator
      else prev.index // fallback: hold steady if r ≈ g

    // Partial adjustment: index converges to fundamental (monthly smoothing)
    val adjustmentSpeed = 0.15
    val newIndex        = Math.max(100.0, prev.index + adjustmentSpeed * (gordonIndex - prev.index))

    // Market cap scales with index
    val indexReturn  = if prev.index > 0 then newIndex / prev.index else 1.0
    val newMarketCap = (prev.marketCap * indexReturn).max(PLN.Zero)

    // Earnings yield from firm profits and market cap
    val annualProfits    = firmProfits * 12.0
    val newEarningsYield = Rate(
      if newMarketCap > PLN.Zero then Math.max(0.01, Math.min(0.50, annualProfits / newMarketCap.toDouble))
      else prev.earningsYield.toDouble,
    )

    // Dividend yield: payout ratio × earnings yield (mean-reverting to calibrated)
    val payoutRatio     = 0.57 // GPW average
    val impliedDivYield = newEarningsYield.toDouble * payoutRatio
    val newDivYield     = Rate(prev.dividendYield.toDouble * 0.9 + impliedDivYield * 0.1)

    // Foreign ownership: slow-moving, mean-reverting to calibrated share
    val newForeignOwnership = Ratio(prev.foreignOwnership.toDouble * 0.99 + p.equity.foreignShare.toDouble * 0.01)

    // Monthly return for HH revaluation (used next month)
    val mReturn = if prev.index > 0 then newIndex / prev.index - 1.0 else 0.0

    State(newIndex, newMarketCap, newEarningsYield, newDivYield, newForeignOwnership, monthlyReturn = Rate(mReturn))

  /** Process equity issuance: firm raises CAPEX via equity, increasing market
    * cap. Returns updated state with diluted index (supply effect).
    */
  def processIssuance(amount: Double, prev: State): State =
    if amount <= 0 then return prev.copy(lastIssuance = PLN.Zero)
    val amountPLN      = PLN(amount)
    // New shares increase market cap; index diluted by supply effect
    val dilutionFactor = prev.marketCap.toDouble / (prev.marketCap.toDouble + amount)
    prev.copy(
      marketCap = prev.marketCap + amountPLN,
      index = prev.index * dilutionFactor,
      lastIssuance = amountPLN,
    )

  /** Compute dividends from firm profits.
    * @return
    *   (domesticDividends, foreignDividends, dividendTax)
    */
  /** Compute dividends from market cap and yields.
    * @return
    *   (netDomesticDividends, foreignDividends, dividendTax)
    */
  def computeDividends(
      firmProfits: Double,
      divYield: Double,
      marketCap: Double,
      foreignShare: Double,
  )(using p: SimParams): (Double, Double, Double) =
    if marketCap <= 0 then return (0.0, 0.0, 0.0)
    // Total monthly dividends: divYield is annual, so /12
    val totalDividends       = divYield * marketCap / 12.0
    val foreignDividends     = totalDividends * foreignShare
    val domesticDividends    = totalDividends - foreignDividends
    // Belka tax on domestic dividends (19%)
    val dividendTax          = domesticDividends * p.equity.divTax.toDouble
    val netDomesticDividends = domesticDividends - dividendTax
    (netDomesticDividends, foreignDividends, dividendTax)
