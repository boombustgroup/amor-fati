package com.boombustgroup.amorfati.engine.markets

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.ledger.Distribute

/** Corporate bond market: Catalyst + non-public issuance (GPW Catalyst 2024).
  *
  * Holder buckets (banks, PPK, insurance, NBFI, other) absorb issuance. Monthly
  * cycle: yield repricing → coupon → amortization → default → issuance.
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

  /** Projection of ledger-owned corporate bond balances used by market
    * calculus.
    *
    * This market DTO is not persisted as ownership state. Month boundaries keep
    * issuer and holder balances in LedgerFinancialState.
    */
  case class StockState(
      outstanding: PLN,
      bankHoldings: PLN,
      ppkHoldings: PLN,
      otherHoldings: PLN,
      insuranceHoldings: PLN = PLN.Zero,
      nbfiHoldings: PLN = PLN.Zero,
  ):
    def holderTotal: PLN =
      bankHoldings + ppkHoldings + otherHoldings + insuranceHoldings + nbfiHoldings

  object StockState:
    val zero: StockState = StockState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)

  /** Corporate bond market memory: pricing and last-month diagnostics only. */
  case class State(
      corpBondYield: Rate,
      lastIssuance: PLN = PLN.Zero,
      lastAmortization: PLN = PLN.Zero,
      lastCouponIncome: PLN = PLN.Zero,
      lastDefaultLoss: PLN = PLN.Zero,
      lastDefaultAmount: PLN = PLN.Zero,
      creditSpread: Rate = Rate(0.025),
      lastAbsorptionRate: Share = Share.One,
  )

  case class StepResult(state: State, stock: StockState)

  def zero: State = State(Rate.Zero, creditSpread = Rate.Zero)

  def initial(using p: SimParams): State =
    State(
      corpBondYield = Rate(0.06) + p.corpBond.spread,
      creditSpread = p.corpBond.spread,
    )

  def initialStock(using p: SimParams): StockState =
    val stock       = p.corpBond.initStock
    val allocations = allocateToHolders(stock)
    StockState(
      outstanding = stock,
      bankHoldings = PLN.fromRaw(allocations.bank),
      ppkHoldings = PLN.fromRaw(allocations.ppk),
      otherHoldings = PLN.fromRaw(allocations.other),
      insuranceHoldings = PLN.fromRaw(allocations.insurance),
      nbfiHoldings = PLN.fromRaw(allocations.nbfi),
    )

  /** Compute current corporate bond yield = gov bond yield + credit spread.
    * Spread widens with system NPL (credit risk channel).
    */
  def computeYield(govBondYield: Rate, nplRatio: Share)(using p: SimParams): Rate =
    val nplMult        = Multiplier.One + (nplRatio * NplSensitivity) // Share × Multiplier → Multiplier
    val cyclicalSpread = p.corpBond.spread * nplMult                  // Rate × Multiplier → Rate
    val spread         = cyclicalSpread.min(MaxSpread)
    (govBondYield + spread).max(MinYield)

  /** Monthly coupon income split across concrete holder buckets. */
  case class CouponResult(
      total: PLN,
      bank: PLN,
      ppk: PLN,
      other: PLN,
      insurance: PLN,
      nbfi: PLN,
  ):
    def holderTotal: PLN =
      bank + ppk + other + insurance + nbfi

  /** Monthly coupon income from corporate bond holdings. */
  def computeCoupon(state: State, stock: StockState): CouponResult =
    val yieldMonthly = state.corpBondYield.monthly
    CouponResult(
      total = stock.outstanding * yieldMonthly,
      bank = stock.bankHoldings * yieldMonthly,
      ppk = stock.ppkHoldings * yieldMonthly,
      other = stock.otherHoldings * yieldMonthly,
      insurance = stock.insuranceHoldings * yieldMonthly,
      nbfi = stock.nbfiHoldings * yieldMonthly,
    )

  /** Gross face-value default plus net loss split across holder buckets. */
  case class DefaultResult(
      grossDefault: PLN,
      lossAfterRecovery: PLN,
      bankLoss: PLN,
      ppkLoss: PLN,
      otherLoss: PLN,
      insuranceLoss: PLN,
      nbfiLoss: PLN,
  ):
    def holderLossTotal: PLN =
      bankLoss + ppkLoss + otherLoss + insuranceLoss + nbfiLoss

  val DefaultResultZero: DefaultResult = DefaultResult(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)

  /** Process defaults from bankrupt firms' bond debt. */
  def processDefaults(stock: StockState, totalBondDefault: PLN)(using p: SimParams): DefaultResult =
    if totalBondDefault <= PLN.Zero || stock.outstanding <= PLN.Zero then DefaultResultZero
    else
      val lossRate     = Share.One - p.corpBond.recovery
      val loss         = totalBondDefault * lossRate
      val holderLosses = Distribute.distribute(
        loss.distributeRaw,
        Array(
          stock.bankHoldings.distributeRaw,
          stock.ppkHoldings.distributeRaw,
          stock.otherHoldings.distributeRaw,
          stock.insuranceHoldings.distributeRaw,
          stock.nbfiHoldings.distributeRaw,
        ),
      )
      DefaultResult(
        grossDefault = totalBondDefault,
        lossAfterRecovery = loss,
        bankLoss = PLN.fromRaw(holderLosses(0)),
        ppkLoss = PLN.fromRaw(holderLosses(1)),
        otherLoss = PLN.fromRaw(holderLosses(2)),
        insuranceLoss = PLN.fromRaw(holderLosses(3)),
        nbfiLoss = PLN.fromRaw(holderLosses(4)),
      )

  /** Monthly amortization: outstanding / maturity. */
  def amortization(stock: StockState)(using p: SimParams): PLN =
    stock.outstanding / math.max(1, p.corpBond.maturity)

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
  def processIssuance(stock: StockState, issuance: PLN)(using p: SimParams): StockState =
    if issuance <= PLN.Zero then stock
    else
      val allocations = allocateToHolders(issuance)
      stock.copy(
        outstanding = stock.outstanding + issuance,
        bankHoldings = stock.bankHoldings + PLN.fromRaw(allocations.bank),
        ppkHoldings = stock.ppkHoldings + PLN.fromRaw(allocations.ppk),
        otherHoldings = stock.otherHoldings + PLN.fromRaw(allocations.other),
        insuranceHoldings = stock.insuranceHoldings + PLN.fromRaw(allocations.insurance),
        nbfiHoldings = stock.nbfiHoldings + PLN.fromRaw(allocations.nbfi),
      )

  /** Full monthly step: yield → coupon → amortization → default → issuance. */
  case class StepInput(
      prevState: State,
      prevStock: StockState,
      govBondYield: Rate,
      nplRatio: Share,
      totalBondDefault: PLN,
      totalBondIssuance: PLN,
  )

  def step(in: StepInput)(using p: SimParams): StepResult =
    val newYield        = computeYield(in.govBondYield, in.nplRatio)
    val coupon          = computeCoupon(in.prevState, in.prevStock)
    val amort           = amortization(in.prevStock)
    val defaults        = processDefaults(in.prevStock, in.totalBondDefault)
    val reduction       = amort + defaults.grossDefault
    val actualReduction = reduction.min(in.prevStock.outstanding).max(PLN.Zero)
    val reductions      =
      if in.prevStock.outstanding > PLN.Zero then
        Distribute.distribute(
          actualReduction.distributeRaw,
          Array(
            in.prevStock.bankHoldings.distributeRaw,
            in.prevStock.ppkHoldings.distributeRaw,
            in.prevStock.otherHoldings.distributeRaw,
            in.prevStock.insuranceHoldings.distributeRaw,
            in.prevStock.nbfiHoldings.distributeRaw,
          ),
        )
      else Array(0L, 0L, 0L, 0L, 0L)
    val afterReduction  = in.prevStock.copy(
      outstanding = in.prevStock.outstanding - actualReduction,
      bankHoldings = (in.prevStock.bankHoldings - PLN.fromRaw(reductions(0))).max(PLN.Zero),
      ppkHoldings = (in.prevStock.ppkHoldings - PLN.fromRaw(reductions(1))).max(PLN.Zero),
      otherHoldings = (in.prevStock.otherHoldings - PLN.fromRaw(reductions(2))).max(PLN.Zero),
      insuranceHoldings = (in.prevStock.insuranceHoldings - PLN.fromRaw(reductions(3))).max(PLN.Zero),
      nbfiHoldings = (in.prevStock.nbfiHoldings - PLN.fromRaw(reductions(4))).max(PLN.Zero),
    )
    val newState        = in.prevState.copy(
      corpBondYield = newYield,
      creditSpread = (newYield - in.govBondYield).max(Rate.Zero),
      lastIssuance = in.totalBondIssuance.max(PLN.Zero),
      lastAmortization = amort,
      lastDefaultAmount = defaults.grossDefault,
      lastDefaultLoss = defaults.lossAfterRecovery,
      lastCouponIncome = coupon.total,
    )
    StepResult(newState, processIssuance(afterReduction, in.totalBondIssuance))

  private case class HolderAllocation(bank: Long, ppk: Long, other: Long, insurance: Long, nbfi: Long)

  private def allocateToHolders(amount: PLN)(using p: SimParams): HolderAllocation =
    if amount <= PLN.Zero then HolderAllocation(0L, 0L, 0L, 0L, 0L)
    else
      val fixedWeights = Array(p.corpBond.bankShare.distributeRaw, p.corpBond.ppkShare.distributeRaw)
      val fixedShare   = p.corpBond.bankShare + p.corpBond.ppkShare
      if fixedShare >= Share.One then
        val fixed = Distribute.distribute(amount.distributeRaw, fixedWeights)
        HolderAllocation(fixed(0), fixed(1), 0L, 0L, 0L)
      else
        val bank      = amount * p.corpBond.bankShare
        val ppk       = amount * p.corpBond.ppkShare
        val residual  = (amount - bank - ppk).max(PLN.Zero)
        val remaining = Distribute.distribute(residual.distributeRaw, positiveResidualHolderWeights)
        HolderAllocation(bank.distributeRaw, ppk.distributeRaw, remaining(0), remaining(1), remaining(2))

  private def positiveResidualHolderWeights(using p: SimParams): Array[Long] =
    val weights = residualHolderWeights
    if weights.exists(_ > 0L) then weights else Array(1L, 0L, 0L)

  private def residualHolderWeights(using p: SimParams): Array[Long] =
    val stock           = p.corpBond.initStock
    val bankTarget      = stock * p.corpBond.bankShare
    val ppkTarget       = stock * p.corpBond.ppkShare
    val insuranceTarget = (p.ins.lifeReserves + p.ins.nonLifeReserves) * p.ins.corpBondShare
    val nbfiTarget      = p.nbfi.tfiInitAum * p.nbfi.tfiCorpBondShare
    val otherTarget     = (stock - bankTarget - ppkTarget - insuranceTarget - nbfiTarget).max(PLN.Zero)
    Array(otherTarget.distributeRaw, insuranceTarget.distributeRaw, nbfiTarget.distributeRaw)
