package sfc.engine.markets

import sfc.config.SimParams
import sfc.types.*
import sfc.util.KahanSum.*

object HousingMarket:

  /** Per-region housing state: only fields that vary regionally. */
  case class RegionalState(
      priceIndex: Double, // regional HPI (absolute — Warszawa starts at 230, Rest at 100)
      totalValue: PLN,    // regional property value
      mortgageStock: PLN, // regional mortgage debt
      lastOrigination: PLN,
      lastRepayment: PLN,
      lastDefault: PLN,
      monthlyReturn: Rate,
  )

  /** Housing market state: aggregate HPI, property value, mortgage stock,
    * flows.
    */
  case class State(
      priceIndex: Double,                           // HPI (base 100)
      totalValue: PLN,                              // aggregate residential property value
      mortgageStock: PLN,                           // total outstanding mortgage debt
      avgMortgageRate: Rate,                        // bank-weighted average mortgage rate
      hhHousingWealth: PLN,                         // HH property equity (value - mortgage)
      lastOrigination: PLN,                         // monthly new mortgages issued
      lastRepayment: PLN,                           // monthly principal repaid
      lastDefault: PLN,                             // monthly mortgage defaults
      lastWealthEffect: PLN,                        // consumption boost from housing wealth
      monthlyReturn: Rate,                          // HPI monthly return (for HH revaluation)
      mortgageInterestIncome: PLN,                  // monthly interest income (-> bank capital)
      regions: Option[Vector[RegionalState]] = None, // 7 entries when RE_REGIONAL=true
  )
  val NRegions = 7
  // Region names (for documentation): Warszawa, Kraków, Wrocław, Gdańsk, Łódź, Poznań, Rest

  def zero: State = State(
    0.0,
    PLN.Zero,
    PLN.Zero,
    Rate.Zero,
    PLN.Zero,
    PLN.Zero,
    PLN.Zero,
    PLN.Zero,
    PLN.Zero,
    Rate.Zero,
    PLN.Zero,
    None,
  )

  def initial(using p: SimParams): State =
    val initMortgageRate = p.monetary.initialRate.toDouble + p.housing.mortgageSpread.toDouble
    val regions          =
      if p.flags.reRegional then
        Some((0 until NRegions).map { r =>
          val rValue    = PLN(p.housing.initValue.toDouble * p.housing.regionalValueShares.map(_.toDouble)(r))
          val rMortgage = PLN(p.housing.initMortgage.toDouble * p.housing.regionalMortgageShares.map(_.toDouble)(r))
          RegionalState(
            priceIndex = p.housing.regionalHpi(r),
            totalValue = rValue,
            mortgageStock = rMortgage,
            lastOrigination = PLN.Zero,
            lastRepayment = PLN.Zero,
            lastDefault = PLN.Zero,
            monthlyReturn = Rate.Zero,
          )
        }.toVector)
      else None
    State(
      priceIndex = p.housing.initHpi,
      totalValue = PLN(p.housing.initValue.toDouble),
      mortgageStock = PLN(p.housing.initMortgage.toDouble),
      avgMortgageRate = Rate(initMortgageRate),
      hhHousingWealth = PLN(p.housing.initValue.toDouble - p.housing.initMortgage.toDouble),
      lastOrigination = PLN.Zero,
      lastRepayment = PLN.Zero,
      lastDefault = PLN.Zero,
      lastWealthEffect = PLN.Zero,
      monthlyReturn = Rate.Zero,
      mortgageInterestIncome = PLN.Zero,
      regions = regions,
    )

  /** Meen-type housing price model (Meen 2002).
    *
    * ΔP/P = α × (ΔY/Y) + β × Δr + γ × (P* - P)/P*
    *
    * P* = annualRent / (mortgageRate - incomeGrowth) — fundamental value
    *
    * @param prev
    *   previous housing market state
    * @param mortgageRate
    *   current mortgage rate (annual)
    * @param inflation
    *   current annualized inflation
    * @param incomeGrowth
    *   income growth rate (monthly)
    * @param employed
    *   number employed
    * @param prevMortgageRate
    *   previous period mortgage rate (for Δr)
    * @return
    *   updated housing market state (price only — origination/flows computed
    *   separately)
    */
  def step(
      prev: State,
      mortgageRate: Double,
      inflation: Double,
      incomeGrowth: Double,
      employed: Int,
      prevMortgageRate: Double,
  )(using p: SimParams): State =
    if !p.flags.re then return zero

    val alpha      = p.housing.priceIncomeElast
    val beta       = p.housing.priceRateElast
    val rateChange = mortgageRate - prevMortgageRate

    prev.regions match
      case Some(regs) =>
        // Regional mode: apply Meen model per region with regional gamma and income mult
        val updatedRegions = regs.zipWithIndex.map { (reg, r) =>
          val gamma                = p.housing.regionalGammas.map(_.toDouble)(r)
          val incomeMult           = p.housing.regionalIncomeMult(r)
          val regionalIncomeGrowth = incomeGrowth * incomeMult

          val regTotalVal      = reg.totalValue.toDouble
          val annualRent       = regTotalVal * p.housing.rentalYield.toDouble
          val effectiveRate    = Math.max(0.01, mortgageRate)
          val expectedGrowth   = Math.max(-0.05, Math.min(effectiveRate - 0.005, regionalIncomeGrowth * 12.0))
          val denominator      = effectiveRate - expectedGrowth
          val fundamentalValue =
            if denominator > 0.005 then annualRent / denominator
            else regTotalVal

          val monthlyGamma  = gamma / 12.0
          val pricePressure = Math.fma(
            alpha,
            regionalIncomeGrowth,
            Math.fma(
              beta,
              rateChange,
              monthlyGamma * (fundamentalValue - regTotalVal) / Math.max(1.0, fundamentalValue),
            ),
          )

          val clampedChange = Math.max(-0.03, Math.min(0.03, pricePressure))

          val newValue = PLN(Math.max(regTotalVal * 0.30, regTotalVal * (1.0 + clampedChange)))
          val newHpi   =
            if regTotalVal > 0 then reg.priceIndex * (newValue / reg.totalValue)
            else reg.priceIndex
          val mReturn  = if reg.priceIndex > 0 then newHpi / reg.priceIndex - 1.0 else 0.0

          reg.copy(priceIndex = newHpi, totalValue = newValue, monthlyReturn = Rate(mReturn))
        }

        // Aggregate: value-weighted HPI average
        val aggTotalValue = PLN(updatedRegions.kahanSumBy(_.totalValue.toDouble))
        val aggHpi        =
          if aggTotalValue > PLN.Zero then
            updatedRegions.zip(p.housing.regionalValueShares.map(_.toDouble)).kahanSumBy { (reg, share) =>
              reg.priceIndex * share
            }
          else prev.priceIndex
        val aggReturn     = if prev.priceIndex > 0 then aggHpi / prev.priceIndex - 1.0 else 0.0

        prev.copy(
          priceIndex = aggHpi,
          totalValue = aggTotalValue,
          monthlyReturn = Rate(aggReturn),
          avgMortgageRate = Rate(mortgageRate),
          regions = Some(updatedRegions),
        )

      case None =>
        // Aggregate mode (original logic)
        val gamma            = p.housing.priceReversion.toDouble
        val prevTotalVal     = prev.totalValue.toDouble
        val annualRent       = prevTotalVal * p.housing.rentalYield.toDouble
        val effectiveRate    = Math.max(0.01, mortgageRate)
        val expectedGrowth   = Math.max(-0.05, Math.min(effectiveRate - 0.005, incomeGrowth * 12.0))
        val denominator      = effectiveRate - expectedGrowth
        val fundamentalValue =
          if denominator > 0.005 then annualRent / denominator
          else prevTotalVal

        val monthlyGamma  = gamma / 12.0
        val pricePressure = Math.fma(
          alpha,
          incomeGrowth,
          Math.fma(beta, rateChange, monthlyGamma * (fundamentalValue - prevTotalVal) / Math.max(1.0, fundamentalValue)),
        )

        val clampedChange = Math.max(-0.03, Math.min(0.03, pricePressure))

        val newTotalValue = PLN(Math.max(prevTotalVal * 0.30, prevTotalVal * (1.0 + clampedChange)))
        val newHpi        =
          if prevTotalVal > 0 then prev.priceIndex * (newTotalValue / prev.totalValue)
          else prev.priceIndex
        val mReturn       = if prev.priceIndex > 0 then newHpi / prev.priceIndex - 1.0 else 0.0

        prev.copy(
          priceIndex = newHpi,
          totalValue = newTotalValue,
          monthlyReturn = Rate(mReturn),
          avgMortgageRate = Rate(mortgageRate),
        )

  /** Mortgage origination: new mortgages issued monthly. Scaled by origination
    * rate, income growth, and bank capacity. LTV constraint caps mortgage at
    * RE_LTV_MAX × property value.
    */
  def processOrigination(prev: State, totalIncome: Double, mortgageRate: Double, bankCapacity: Boolean)(using
      p: SimParams,
  ): State =
    if !p.flags.re || !p.flags.reMortgage || !bankCapacity then
      return prev.copy(
        lastOrigination = PLN.Zero,
        regions = prev.regions.map(_.map(_.copy(lastOrigination = PLN.Zero))),
      )

    // Base origination: fraction of total market value per month
    val baseOrigination = prev.totalValue.toDouble * p.housing.originationRate.toDouble

    // Rate sensitivity: lower rates -> more origination (elasticity -0.5)
    val rateAdj = Math.max(0.3, Math.min(2.0, 1.0 - 0.5 * (mortgageRate - 0.06)))

    // Income sensitivity: higher income growth -> more origination
    val incomeAdj = Math.max(0.5, Math.min(1.5, 1.0 + totalIncome / Math.max(1.0, prev.totalValue.toDouble) * 10.0))

    val rawOrigination = baseOrigination * rateAdj * Math.min(1.5, incomeAdj)

    // LTV constraint: total mortgages <= LTV_MAX x total value
    val maxMortgage = prev.totalValue.toDouble * p.housing.ltvMax.toDouble
    val headroom    = Math.max(0.0, maxMortgage - prev.mortgageStock.toDouble)
    val origination = Math.min(rawOrigination, headroom)

    prev.regions match
      case Some(regs) =>
        // Distribute origination proportional to regional value share, per-region LTV
        val updatedRegions = regs.zipWithIndex.map { (reg, r) =>
          val regionalRaw      = origination * p.housing.regionalValueShares.map(_.toDouble)(r)
          val regionalMax      = reg.totalValue.toDouble * p.housing.ltvMax.toDouble
          val regionalHeadroom = Math.max(0.0, regionalMax - reg.mortgageStock.toDouble)
          val regionalOrig     = Math.min(regionalRaw, regionalHeadroom)
          reg.copy(
            mortgageStock = reg.mortgageStock + PLN(regionalOrig),
            lastOrigination = PLN(regionalOrig),
          )
        }
        val aggOrig        = PLN(updatedRegions.kahanSumBy(_.lastOrigination.toDouble))
        val aggStock       = PLN(updatedRegions.kahanSumBy(_.mortgageStock.toDouble))
        prev.copy(
          mortgageStock = aggStock,
          lastOrigination = aggOrig,
          regions = Some(updatedRegions),
        )

      case None =>
        prev.copy(
          mortgageStock = prev.mortgageStock + PLN(origination),
          lastOrigination = PLN(origination),
        )

  /** Mortgage servicing: compute interest, principal repayment, and defaults.
    * @return
    *   (interestIncome, principalRepaid, defaultLoss)
    */
  def processMortgageFlows(prev: State, mortgageRate: Double, unemploymentRate: Double)(using
      p: SimParams,
  ): (Double, Double, Double) =
    if !p.flags.re || prev.mortgageStock <= PLN.Zero then return (0.0, 0.0, 0.0)

    val stock          = prev.mortgageStock.toDouble
    // Interest: mortgage stock x annual rate / 12
    val interestIncome = stock * Math.max(0.0, mortgageRate) / 12.0

    // Principal repayment: amortization over maturity
    val principalRepaid = stock / p.housing.mortgageMaturity.toDouble

    // Default rate: base + unemployment sensitivity x max(0, unempRate - 5%)
    val defaultRate   = p.housing.defaultBase.toDouble +
      p.housing.defaultUnempSens * Math.max(0.0, unemploymentRate - 0.05)
    val defaultAmount = stock * defaultRate

    // Default loss = defaultAmount x (1 - recovery)
    val defaultLoss = defaultAmount * (1.0 - p.housing.mortgageRecovery.toDouble)

    (interestIncome, principalRepaid, defaultLoss)

  /** Update mortgage stock after flows (principal repayment + defaults). */
  def applyFlows(prev: State, principalRepaid: Double, defaultAmount: Double, interestIncome: Double)(using
      p: SimParams,
  ): State =
    val newStock     = PLN(Math.max(0.0, prev.mortgageStock.toDouble - principalRepaid - defaultAmount))
    val newHhWealth  = prev.totalValue - newStock
    // Housing wealth effect: MPC x housing wealth change
    val wealthChange = newHhWealth - prev.hhHousingWealth
    val wealthEffect =
      if p.flags.reHhHousing && wealthChange > PLN.Zero then wealthChange * p.housing.wealthMpc.toDouble
      else PLN.Zero

    val updatedRegions = prev.regions.map { regs =>
      val totalPrevStock = prev.mortgageStock.toDouble
      if totalPrevStock <= 0 then regs
      else
        regs.map { reg =>
          val share      = reg.mortgageStock.toDouble / totalPrevStock
          val rPrincipal = principalRepaid * share
          val rDefault   = defaultAmount * share
          val rStock     = PLN(Math.max(0.0, reg.mortgageStock.toDouble - rPrincipal - rDefault))
          reg.copy(
            mortgageStock = rStock,
            lastRepayment = PLN(rPrincipal),
            lastDefault = PLN(rDefault),
          )
        }
    }

    prev.copy(
      mortgageStock = newStock,
      hhHousingWealth = newHhWealth,
      lastRepayment = PLN(principalRepaid),
      lastDefault = PLN(defaultAmount),
      lastWealthEffect = wealthEffect,
      mortgageInterestIncome = PLN(interestIncome),
      regions = updatedRegions,
    )
