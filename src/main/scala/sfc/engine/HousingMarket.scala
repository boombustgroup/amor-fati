package sfc.engine

import sfc.config.Config
import KahanSum.*

/** Per-region housing state: only fields that vary regionally. */
case class RegionalHousingState(
  priceIndex: Double,      // regional HPI (absolute — Warszawa starts at 230, Rest at 100)
  totalValue: Double,      // regional property value
  mortgageStock: Double,   // regional mortgage debt
  lastOrigination: Double,
  lastRepayment: Double,
  lastDefault: Double,
  monthlyReturn: Double
)

/** Housing market state: aggregate HPI, property value, mortgage stock, flows. */
case class HousingMarketState(
  priceIndex: Double,              // HPI (base 100)
  totalValue: Double,              // aggregate residential property value
  mortgageStock: Double,           // total outstanding mortgage debt
  avgMortgageRate: Double,         // bank-weighted average mortgage rate
  hhHousingWealth: Double,         // HH property equity (value - mortgage)
  lastOrigination: Double,         // monthly new mortgages issued
  lastRepayment: Double,           // monthly principal repaid
  lastDefault: Double,             // monthly mortgage defaults
  lastWealthEffect: Double,        // consumption boost from housing wealth
  monthlyReturn: Double,           // HPI monthly return (for HH revaluation)
  mortgageInterestIncome: Double,  // monthly interest income (-> bank capital)
  regions: Option[Vector[RegionalHousingState]] = None  // 7 entries when RE_REGIONAL=true
)

object HousingMarket:
  val NRegions = 7
  // Region names (for documentation): Warszawa, Kraków, Wrocław, Gdańsk, Łódź, Poznań, Rest

  def zero: HousingMarketState = HousingMarketState(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, None)

  def initial: HousingMarketState =
    val initMortgageRate = Config.NbpInitialRate + Config.ReMortgageSpread
    val regions = if Config.ReRegional then
      Some((0 until NRegions).map { r =>
        val rValue = Config.ReInitValue * Config.ReRegionalValueShares(r)
        val rMortgage = Config.ReInitMortgage * Config.ReRegionalMortgageShares(r)
        RegionalHousingState(
          priceIndex = Config.ReRegionalHpi(r),
          totalValue = rValue,
          mortgageStock = rMortgage,
          lastOrigination = 0.0,
          lastRepayment = 0.0,
          lastDefault = 0.0,
          monthlyReturn = 0.0
        )
      }.toVector)
    else None
    HousingMarketState(
      priceIndex = Config.ReInitHpi,
      totalValue = Config.ReInitValue,
      mortgageStock = Config.ReInitMortgage,
      avgMortgageRate = initMortgageRate,
      hhHousingWealth = Config.ReInitValue - Config.ReInitMortgage,
      lastOrigination = 0.0,
      lastRepayment = 0.0,
      lastDefault = 0.0,
      lastWealthEffect = 0.0,
      monthlyReturn = 0.0,
      mortgageInterestIncome = 0.0,
      regions = regions
    )

  /** Meen-type housing price model (Meen 2002).
    *
    * ΔP/P = α × (ΔY/Y) + β × Δr + γ × (P* - P)/P*
    *
    * P* = annualRent / (mortgageRate - incomeGrowth)  — fundamental value
    *
    * @param prev previous housing market state
    * @param mortgageRate current mortgage rate (annual)
    * @param inflation current annualized inflation
    * @param incomeGrowth income growth rate (monthly)
    * @param employed number employed
    * @param prevMortgageRate previous period mortgage rate (for Δr)
    * @return updated housing market state (price only — origination/flows computed separately)
    */
  def step(prev: HousingMarketState, mortgageRate: Double, inflation: Double,
           incomeGrowth: Double, employed: Int, prevMortgageRate: Double): HousingMarketState =
    if !Config.ReEnabled then return zero

    val alpha = Config.RePriceIncomeElast
    val beta = Config.RePriceRateElast
    val rateChange = mortgageRate - prevMortgageRate

    prev.regions match
      case Some(regs) =>
        // Regional mode: apply Meen model per region with regional gamma and income mult
        val updatedRegions = regs.zipWithIndex.map { (reg, r) =>
          val gamma = Config.ReRegionalGammas(r)
          val incomeMult = Config.ReRegionalIncomeMult(r)
          val regionalIncomeGrowth = incomeGrowth * incomeMult

          val annualRent = reg.totalValue * Config.ReRentalYield
          val effectiveRate = Math.max(0.01, mortgageRate)
          val expectedGrowth = Math.max(-0.05, Math.min(effectiveRate - 0.005, regionalIncomeGrowth * 12.0))
          val denominator = effectiveRate - expectedGrowth
          val fundamentalValue = if denominator > 0.005 then annualRent / denominator
                                 else reg.totalValue

          val monthlyGamma = gamma / 12.0
          val pricePressure = Math.fma(alpha, regionalIncomeGrowth,
            Math.fma(beta, rateChange,
              monthlyGamma * (fundamentalValue - reg.totalValue) / Math.max(1.0, fundamentalValue)))

          val clampedChange = Math.max(-0.03, Math.min(0.03, pricePressure))

          val newValue = Math.max(reg.totalValue * 0.30, reg.totalValue * (1.0 + clampedChange))
          val newHpi = if reg.totalValue > 0 then reg.priceIndex * (newValue / reg.totalValue)
                       else reg.priceIndex
          val mReturn = if reg.priceIndex > 0 then newHpi / reg.priceIndex - 1.0 else 0.0

          reg.copy(priceIndex = newHpi, totalValue = newValue, monthlyReturn = mReturn)
        }

        // Aggregate: value-weighted HPI average
        val aggTotalValue = updatedRegions.kahanSumBy(_.totalValue)
        val aggHpi = if aggTotalValue > 0 then
          updatedRegions.zip(Config.ReRegionalValueShares).kahanSumBy { (reg, share) =>
            reg.priceIndex * share
          }
        else prev.priceIndex
        val aggReturn = if prev.priceIndex > 0 then aggHpi / prev.priceIndex - 1.0 else 0.0

        prev.copy(
          priceIndex = aggHpi,
          totalValue = aggTotalValue,
          monthlyReturn = aggReturn,
          avgMortgageRate = mortgageRate,
          regions = Some(updatedRegions)
        )

      case None =>
        // Aggregate mode (original logic)
        val gamma = Config.RePriceReversion
        val annualRent = prev.totalValue * Config.ReRentalYield
        val effectiveRate = Math.max(0.01, mortgageRate)
        val expectedGrowth = Math.max(-0.05, Math.min(effectiveRate - 0.005, incomeGrowth * 12.0))
        val denominator = effectiveRate - expectedGrowth
        val fundamentalValue = if denominator > 0.005 then annualRent / denominator
                               else prev.totalValue

        val monthlyGamma = gamma / 12.0
        val pricePressure = Math.fma(alpha, incomeGrowth,
          Math.fma(beta, rateChange,
            monthlyGamma * (fundamentalValue - prev.totalValue) / Math.max(1.0, fundamentalValue)))

        val clampedChange = Math.max(-0.03, Math.min(0.03, pricePressure))

        val newTotalValue = Math.max(prev.totalValue * 0.30, prev.totalValue * (1.0 + clampedChange))
        val newHpi = if prev.totalValue > 0 then prev.priceIndex * (newTotalValue / prev.totalValue)
                     else prev.priceIndex
        val mReturn = if prev.priceIndex > 0 then newHpi / prev.priceIndex - 1.0 else 0.0

        prev.copy(
          priceIndex = newHpi,
          totalValue = newTotalValue,
          monthlyReturn = mReturn,
          avgMortgageRate = mortgageRate
        )

  /** Mortgage origination: new mortgages issued monthly.
    * Scaled by origination rate, income growth, and bank capacity.
    * LTV constraint caps mortgage at RE_LTV_MAX × property value. */
  def processOrigination(prev: HousingMarketState, totalIncome: Double,
                         mortgageRate: Double, bankCapacity: Boolean): HousingMarketState =
    if !Config.ReEnabled || !Config.ReMortgage || !bankCapacity then
      return prev.copy(lastOrigination = 0.0,
        regions = prev.regions.map(_.map(_.copy(lastOrigination = 0.0))))

    // Base origination: fraction of total market value per month
    val baseOrigination = prev.totalValue * Config.ReOriginationRate

    // Rate sensitivity: lower rates -> more origination (elasticity -0.5)
    val rateAdj = Math.max(0.3, Math.min(2.0, 1.0 - 0.5 * (mortgageRate - 0.06)))

    // Income sensitivity: higher income growth -> more origination
    val incomeAdj = Math.max(0.5, Math.min(1.5, 1.0 + totalIncome / Math.max(1.0, prev.totalValue) * 10.0))

    val rawOrigination = baseOrigination * rateAdj * Math.min(1.5, incomeAdj)

    // LTV constraint: total mortgages <= LTV_MAX x total value
    val maxMortgage = prev.totalValue * Config.ReLtvMax
    val headroom = Math.max(0.0, maxMortgage - prev.mortgageStock)
    val origination = Math.min(rawOrigination, headroom)

    prev.regions match
      case Some(regs) =>
        // Distribute origination proportional to regional value share, per-region LTV
        val updatedRegions = regs.zipWithIndex.map { (reg, r) =>
          val regionalRaw = origination * Config.ReRegionalValueShares(r)
          val regionalMax = reg.totalValue * Config.ReLtvMax
          val regionalHeadroom = Math.max(0.0, regionalMax - reg.mortgageStock)
          val regionalOrig = Math.min(regionalRaw, regionalHeadroom)
          reg.copy(
            mortgageStock = reg.mortgageStock + regionalOrig,
            lastOrigination = regionalOrig
          )
        }
        val aggOrig = updatedRegions.kahanSumBy(_.lastOrigination)
        val aggStock = updatedRegions.kahanSumBy(_.mortgageStock)
        prev.copy(
          mortgageStock = aggStock,
          lastOrigination = aggOrig,
          regions = Some(updatedRegions)
        )

      case None =>
        prev.copy(
          mortgageStock = prev.mortgageStock + origination,
          lastOrigination = origination
        )

  /** Mortgage servicing: compute interest, principal repayment, and defaults.
    * @return (interestIncome, principalRepaid, defaultLoss) */
  def processMortgageFlows(prev: HousingMarketState,
                           mortgageRate: Double,
                           unemploymentRate: Double
  ): (Double, Double, Double) =
    if !Config.ReEnabled || prev.mortgageStock <= 0 then return (0.0, 0.0, 0.0)

    // Interest: mortgage stock x annual rate / 12
    val interestIncome = prev.mortgageStock * Math.max(0.0, mortgageRate) / 12.0

    // Principal repayment: amortization over maturity
    val principalRepaid = prev.mortgageStock / Config.ReMortgageMaturity.toDouble

    // Default rate: base + unemployment sensitivity x max(0, unempRate - 5%)
    val defaultRate = Config.ReDefaultBase +
      Config.ReDefaultUnempSens * Math.max(0.0, unemploymentRate - 0.05)
    val defaultAmount = prev.mortgageStock * defaultRate

    // Default loss = defaultAmount x (1 - recovery)
    val defaultLoss = defaultAmount * (1.0 - Config.ReMortgageRecovery)

    (interestIncome, principalRepaid, defaultLoss)

  /** Update mortgage stock after flows (principal repayment + defaults). */
  def applyFlows(prev: HousingMarketState, principalRepaid: Double,
                 defaultAmount: Double, interestIncome: Double): HousingMarketState =
    val newStock = Math.max(0.0, prev.mortgageStock - principalRepaid - defaultAmount)
    val newHhWealth = prev.totalValue - newStock
    // Housing wealth effect: MPC x housing wealth change
    val wealthChange = newHhWealth - prev.hhHousingWealth
    val wealthEffect = if Config.ReHhHousing && wealthChange > 0 then
      wealthChange * Config.ReWealthMpc
    else 0.0

    val updatedRegions = prev.regions.map { regs =>
      val totalPrevStock = prev.mortgageStock
      if totalPrevStock <= 0 then regs
      else regs.map { reg =>
        val share = reg.mortgageStock / totalPrevStock
        val rPrincipal = principalRepaid * share
        val rDefault = defaultAmount * share
        val rStock = Math.max(0.0, reg.mortgageStock - rPrincipal - rDefault)
        reg.copy(
          mortgageStock = rStock,
          lastRepayment = rPrincipal,
          lastDefault = rDefault
        )
      }
    }

    prev.copy(
      mortgageStock = newStock,
      hhHousingWealth = newHhWealth,
      lastRepayment = principalRepaid,
      lastDefault = defaultAmount,
      lastWealthEffect = wealthEffect,
      mortgageInterestIncome = interestIncome,
      regions = updatedRegions
    )
