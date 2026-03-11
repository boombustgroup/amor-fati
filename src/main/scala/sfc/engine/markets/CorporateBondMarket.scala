package sfc.engine.markets

import sfc.config.SimParams
import sfc.types.*

/** Corporate bond market: Catalyst + non-public issuance (GPW Catalyst 2024).
  *
  * Three holder classes (banks, PPK, other) absorb issuance proportionally.
  * Monthly cycle: yield repricing → coupon → amortization → default → issuance.
  *
  * Demand-side absorption constraint (two gates):
  *   - Gate 1: spread-based investor appetite (cyclical, widens with NPL)
  *   - Gate 2: bank CAR headroom (corp bonds at 50% risk weight, KNF 2024)
  *
  * Yield = gov bond yield + credit spread; spread widens with system NPL ratio
  * (credit risk channel). Calibration: NBP Financial Stability Report 2024, GPW
  * Catalyst market data.
  */
object CorporateBondMarket:

  // --- Named constants ---
  private val NplSensitivity      = 5.0  // spread multiplier per unit NPL ratio
  private val MaxSpread           = 0.10 // spread cap (1000 bps)
  private val MinYield            = 0.01 // yield floor (100 bps)
  private val MinAbsorption       = 0.3  // absorption floor
  private val CarBufferZone       = 0.02 // 200 bps CAR ramp zone above minCar
  private val MonthsPerYear       = 12.0
  private val SpreadAbsorptionCap = 0.10 // excess spread at which absorption hits floor

  /** Corporate bond market state: Catalyst + non-public issuance. */
  case class State(
      outstanding: PLN,
      bankHoldings: PLN,
      ppkHoldings: PLN,
      otherHoldings: PLN,
      corpBondYield: Rate,
      lastIssuance: PLN = PLN.Zero,
      lastAmortization: PLN = PLN.Zero,
      lastCouponIncome: PLN = PLN.Zero,
      lastDefaultLoss: PLN = PLN.Zero,
      lastDefaultAmount: PLN = PLN.Zero,
      creditSpread: Rate = Rate(0.025),
      lastAbsorptionRate: Ratio = Ratio.One,
  )
  def zero: State = State(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, Rate.Zero)

  def initial(using p: SimParams): State =
    val stock     = p.corpBond.initStock
    val bankShare = p.corpBond.bankShare.toDouble
    val ppkShare  = p.corpBond.ppkShare.toDouble
    State(
      outstanding = stock,
      bankHoldings = stock * bankShare,
      ppkHoldings = stock * ppkShare,
      otherHoldings = stock * (1.0 - bankShare - ppkShare),
      corpBondYield = Rate(0.06 + p.corpBond.spread.toDouble),
      creditSpread = Rate(p.corpBond.spread.toDouble),
    )

  /** Compute current corporate bond yield = gov bond yield + credit spread.
    * Spread widens with system NPL (credit risk channel).
    */
  def computeYield(govBondYield: Rate, nplRatio: Ratio)(using p: SimParams): Rate =
    val cyclicalSpread = p.corpBond.spread.toDouble * (1.0 + nplRatio.toDouble * NplSensitivity)
    val spread         = Math.min(MaxSpread, cyclicalSpread)
    Rate(Math.max(MinYield, govBondYield.toDouble + spread))

  /** @param total
    *   total monthly coupon across all holders
    * @param bank
    *   bank share of monthly coupon
    * @param ppk
    *   PPK share of monthly coupon
    */
  case class CouponResult(total: PLN, bank: PLN, ppk: PLN)

  /** Monthly coupon income from corporate bond holdings. */
  def computeCoupon(state: State): CouponResult =
    val yieldMonthly = state.corpBondYield.toDouble / MonthsPerYear
    CouponResult(
      total = state.outstanding * yieldMonthly,
      bank = state.bankHoldings * yieldMonthly,
      ppk = state.ppkHoldings * yieldMonthly,
    )

  /** @param grossDefault
    *   gross default amount (face value)
    * @param lossAfterRecovery
    *   net loss after recovery rate applied
    * @param bankLoss
    *   bank share of net loss
    * @param ppkLoss
    *   PPK share of net loss
    */
  case class DefaultResult(grossDefault: PLN, lossAfterRecovery: PLN, bankLoss: PLN, ppkLoss: PLN)

  val DefaultResultZero: DefaultResult = DefaultResult(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)

  /** Process defaults from bankrupt firms' bond debt. */
  def processDefaults(state: State, totalBondDefault: PLN)(using p: SimParams): DefaultResult =
    if totalBondDefault <= PLN.Zero || state.outstanding <= PLN.Zero then DefaultResultZero
    else
      val defaultFrac = Math.min(1.0, totalBondDefault.toDouble / state.outstanding.toDouble)
      val lossRate    = 1.0 - p.corpBond.recovery.toDouble
      DefaultResult(
        grossDefault = totalBondDefault,
        lossAfterRecovery = totalBondDefault * lossRate,
        bankLoss = state.bankHoldings * defaultFrac * lossRate,
        ppkLoss = state.ppkHoldings * defaultFrac * lossRate,
      )

  /** Monthly amortization: outstanding / maturity. */
  def amortization(state: State)(using p: SimParams): PLN =
    state.outstanding * (1.0 / Math.max(1.0, p.corpBond.maturity))

  /** Compute market absorption rate for new bond issuance.
    *
    * Gate 1: spread-based investor appetite (cyclical). Gate 2: bank CAR
    * headroom (banks hold CorpBondBankShare at 50% RW).
    *
    * @return
    *   absorption rate in [0.3, 1.0]
    */
  def computeAbsorption(state: State, tentativeIssuance: PLN, aggBankCar: Ratio, minCar: Ratio)(using
      p: SimParams,
  ): Ratio =
    if tentativeIssuance <= PLN.Zero then Ratio.One
    else
      val excessSpread     = Math.max(0.0, state.creditSpread.toDouble - p.corpBond.spread.toDouble)
      val spreadAbsorption = Math.max(MinAbsorption, 1.0 - excessSpread / SpreadAbsorptionCap)
      val carAbsorption    =
        if aggBankCar <= minCar then MinAbsorption
        else if aggBankCar.toDouble >= minCar.toDouble + CarBufferZone then 1.0
        else MinAbsorption + (1.0 - MinAbsorption) * (aggBankCar.toDouble - minCar.toDouble) / CarBufferZone
      Ratio(Math.max(MinAbsorption, Math.min(1.0, spreadAbsorption * carAbsorption)))

  /** Process new issuance: allocate to holders proportionally. */
  def processIssuance(state: State, issuance: PLN)(using p: SimParams): State =
    if issuance <= PLN.Zero then state.copy(lastIssuance = PLN.Zero)
    else
      val bankShare = p.corpBond.bankShare.toDouble
      val ppkShare  = p.corpBond.ppkShare.toDouble
      state.copy(
        outstanding = state.outstanding + issuance,
        bankHoldings = state.bankHoldings + issuance * bankShare,
        ppkHoldings = state.ppkHoldings + issuance * ppkShare,
        otherHoldings = state.otherHoldings + issuance * (1.0 - bankShare - ppkShare),
        lastIssuance = issuance,
      )

  /** Full monthly step: yield → coupon → amortization → default → issuance. */
  case class StepInput(
      prev: State,
      govBondYield: Rate,
      nplRatio: Ratio,
      totalBondDefault: PLN,
      totalBondIssuance: PLN,
  )

  def step(in: StepInput)(using p: SimParams): State =
    val newYield       = computeYield(in.govBondYield, in.nplRatio)
    val coupon         = computeCoupon(in.prev)
    val amort          = amortization(in.prev)
    val defaults       = processDefaults(in.prev, in.totalBondDefault)
    val reduction      = amort + defaults.grossDefault
    val reductionFrac  =
      if in.prev.outstanding > PLN.Zero then Math.min(1.0, reduction.toDouble / in.prev.outstanding.toDouble) else 0.0
    val afterReduction = in.prev.copy(
      outstanding = (in.prev.outstanding - reduction).max(PLN.Zero),
      bankHoldings = (in.prev.bankHoldings - in.prev.bankHoldings * reductionFrac).max(PLN.Zero),
      ppkHoldings = (in.prev.ppkHoldings - in.prev.ppkHoldings * reductionFrac).max(PLN.Zero),
      otherHoldings = (in.prev.otherHoldings - in.prev.otherHoldings * reductionFrac).max(PLN.Zero),
      corpBondYield = newYield,
      creditSpread = Rate(Math.max(0.0, newYield.toDouble - in.govBondYield.toDouble)),
      lastAmortization = amort,
      lastDefaultAmount = defaults.grossDefault,
      lastDefaultLoss = defaults.lossAfterRecovery,
      lastCouponIncome = coupon.bank + coupon.ppk,
    )
    processIssuance(afterReduction, in.totalBondIssuance)
