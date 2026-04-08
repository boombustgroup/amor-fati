package com.boombustgroup.amorfati.engine.markets

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.Distribute

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
  private val NplSensitivity      = Multiplier(5.0)  // spread multiplier per unit NPL ratio
  private val MaxSpread           = Rate(0.10)       // spread cap (1000 bps)
  private val MinYield            = Rate(0.01)       // yield floor (100 bps)
  private val MinAbsorption       = Share(0.3)       // absorption floor
  private val CarBufferZone       = Multiplier(0.02) // 200 bps CAR ramp zone above minCar
  private val SpreadAbsorptionCap = Rate(0.10)       // excess spread at which absorption hits floor

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
      lastAbsorptionRate: Share = Share.One,
  )
  def zero: State = State(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, Rate.Zero)

  def initial(using p: SimParams): State =
    val stock   = p.corpBond.initStock
    val otherSh = Share.One - p.corpBond.bankShare - p.corpBond.ppkShare
    State(
      outstanding = stock,
      bankHoldings = stock * p.corpBond.bankShare,
      ppkHoldings = stock * p.corpBond.ppkShare,
      otherHoldings = stock * otherSh,
      corpBondYield = Rate(0.06) + p.corpBond.spread,
      creditSpread = p.corpBond.spread,
    )

  /** Compute current corporate bond yield = gov bond yield + credit spread.
    * Spread widens with system NPL (credit risk channel).
    */
  def computeYield(govBondYield: Rate, nplRatio: Share)(using p: SimParams): Rate =
    val nplMult        = Multiplier.One + (nplRatio * NplSensitivity) // Share × Multiplier → Multiplier
    val cyclicalSpread = p.corpBond.spread * nplMult                  // Rate × Multiplier → Rate
    val spread         = cyclicalSpread.min(MaxSpread)
    (govBondYield + spread).max(MinYield)

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
    val yieldMonthly = state.corpBondYield.monthly
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
      val defaultFrac = totalBondDefault.ratioTo(state.outstanding).toShare.clamp(Share.Zero, Share.One)
      val lossRate    = Share.One - p.corpBond.recovery
      DefaultResult(
        grossDefault = totalBondDefault,
        lossAfterRecovery = totalBondDefault * lossRate,
        bankLoss = state.bankHoldings * defaultFrac * lossRate,
        ppkLoss = state.ppkHoldings * defaultFrac * lossRate,
      )

  /** Monthly amortization: outstanding / maturity. */
  def amortization(state: State)(using p: SimParams): PLN =
    state.outstanding / math.max(1, p.corpBond.maturity)

  /** Compute market absorption rate for new bond issuance.
    *
    * Gate 1: spread-based investor appetite (cyclical). Gate 2: bank CAR
    * headroom (banks hold CorpBondBankShare at 50% RW).
    *
    * @return
    *   absorption rate in [0.3, 1.0]
    */
  def computeAbsorption(state: State, tentativeIssuance: PLN, aggBankCar: Multiplier, minCar: Multiplier)(using p: SimParams): Share =
    if tentativeIssuance <= PLN.Zero then Share.One
    else
      val spreadHeadroom   = (state.creditSpread - p.corpBond.spread).max(Rate.Zero)
      val spreadPenalty    = spreadHeadroom.ratioTo(SpreadAbsorptionCap).clamp(Scalar.Zero, Scalar.One)
      val spreadAbsorption = Share.One - (spreadPenalty * (Share.One - MinAbsorption))
      val carAbsorption    =
        if aggBankCar <= minCar then MinAbsorption
        else
          val headroom = (aggBankCar - minCar).max(Multiplier.Zero)
          val ramp     = headroom.ratioTo(CarBufferZone).clamp(Scalar.Zero, Scalar.One)
          MinAbsorption + (ramp * (Share.One - MinAbsorption))
      spreadAbsorption.min(carAbsorption).clamp(MinAbsorption, Share.One)

  /** Process new issuance: allocate to holders proportionally. */
  def processIssuance(state: State, issuance: PLN)(using p: SimParams): State =
    if issuance <= PLN.Zero then state.copy(lastIssuance = PLN.Zero)
    else
      val holderWeights = Array(
        p.corpBond.bankShare.distributeRaw,
        p.corpBond.ppkShare.distributeRaw,
        (Share.One - p.corpBond.bankShare - p.corpBond.ppkShare).distributeRaw,
      )
      val allocations   = Distribute.distribute(issuance.distributeRaw, holderWeights)
      state.copy(
        outstanding = state.outstanding + issuance,
        bankHoldings = state.bankHoldings + PLN.fromRaw(allocations(0)),
        ppkHoldings = state.ppkHoldings + PLN.fromRaw(allocations(1)),
        otherHoldings = state.otherHoldings + PLN.fromRaw(allocations(2)),
        lastIssuance = issuance,
      )

  /** Full monthly step: yield → coupon → amortization → default → issuance. */
  case class StepInput(
      prev: State,
      govBondYield: Rate,
      nplRatio: Share,
      totalBondDefault: PLN,
      totalBondIssuance: PLN,
  )

  def step(in: StepInput)(using p: SimParams): State =
    val newYield        = computeYield(in.govBondYield, in.nplRatio)
    val coupon          = computeCoupon(in.prev)
    val amort           = amortization(in.prev)
    val defaults        = processDefaults(in.prev, in.totalBondDefault)
    val reduction       = amort + defaults.grossDefault
    val actualReduction = reduction.min(in.prev.outstanding).max(PLN.Zero)
    val reductions      =
      if in.prev.outstanding > PLN.Zero then
        Distribute.distribute(
          actualReduction.distributeRaw,
          Array(in.prev.bankHoldings.distributeRaw, in.prev.ppkHoldings.distributeRaw, in.prev.otherHoldings.distributeRaw),
        )
      else Array(0L, 0L, 0L)
    val afterReduction  = in.prev.copy(
      outstanding = in.prev.outstanding - actualReduction,
      bankHoldings = (in.prev.bankHoldings - PLN.fromRaw(reductions(0))).max(PLN.Zero),
      ppkHoldings = (in.prev.ppkHoldings - PLN.fromRaw(reductions(1))).max(PLN.Zero),
      otherHoldings = (in.prev.otherHoldings - PLN.fromRaw(reductions(2))).max(PLN.Zero),
      corpBondYield = newYield,
      creditSpread = (newYield - in.govBondYield).max(Rate.Zero),
      lastAmortization = amort,
      lastDefaultAmount = defaults.grossDefault,
      lastDefaultLoss = defaults.lossAfterRecovery,
      lastCouponIncome = coupon.bank + coupon.ppk,
    )
    processIssuance(afterReduction, in.totalBondIssuance)
