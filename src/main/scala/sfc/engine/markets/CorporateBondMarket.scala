package sfc.engine.markets

import sfc.config.SimParams
import sfc.types.*

object CorporateBondMarket:

  /** Corporate bond market state: Catalyst + non-public issuance. */
  case class State(
      outstanding: PLN,                     // total corp bonds outstanding
      bankHoldings: PLN,                    // banks' share of outstanding
      ppkHoldings: PLN,                     // PPK's share of outstanding
      otherHoldings: PLN,                   // other investors (off-model sink)
      corpBondYield: Rate,                  // current yield = govBondYield + spread
      lastIssuance: PLN = PLN.Zero,
      lastAmortization: PLN = PLN.Zero,
      lastCouponIncome: PLN = PLN.Zero,     // bank + ppk coupon income
      lastDefaultLoss: PLN = PLN.Zero,      // net loss after recovery
      lastDefaultAmount: PLN = PLN.Zero,    // gross default amount
      creditSpread: Rate = Rate(0.025),     // current credit spread
      lastAbsorptionRate: Ratio = Ratio.One, // demand-side absorption rate [0.3, 1.0]
  )
  def zero: State = State(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, Rate.Zero)

  def initial(using p: SimParams): State =
    val stock = PLN(p.corpBond.initStock.toDouble)
    State(
      outstanding = stock,
      bankHoldings = stock * p.corpBond.bankShare.toDouble,
      ppkHoldings = stock * p.corpBond.ppkShare.toDouble,
      otherHoldings = stock * (1.0 - p.corpBond.bankShare.toDouble - p.corpBond.ppkShare.toDouble),
      corpBondYield = Rate(0.06 + p.corpBond.spread.toDouble),
      creditSpread = Rate(p.corpBond.spread.toDouble),
    )

  /** Compute current corporate bond yield = gov bond yield + credit spread.
    * Spread widens with system NPL (credit risk channel).
    */
  def computeYield(govBondYield: Double, nplRatio: Double)(using p: SimParams): Double =
    val cyclicalSpread = p.corpBond.spread.toDouble * (1.0 + nplRatio * 5.0)
    val spread         = Math.min(0.10, cyclicalSpread)
    Math.max(0.01, govBondYield + spread)

  /** Monthly coupon income from corporate bond holdings. Returns (totalCoupon,
    * bankCoupon, ppkCoupon).
    */
  def computeCoupon(state: State): (Double, Double, Double) =
    val totalCoupon = state.outstanding.toDouble * state.corpBondYield.toDouble / 12.0
    val bankCoupon  = state.bankHoldings.toDouble * state.corpBondYield.toDouble / 12.0
    val ppkCoupon   = state.ppkHoldings.toDouble * state.corpBondYield.toDouble / 12.0
    (totalCoupon, bankCoupon, ppkCoupon)

  /** Process defaults from bankrupt firms' bond debt. Returns (grossDefault,
    * lossAfterRecovery, bankLoss, ppkLoss).
    */
  def processDefaults(state: State, totalBondDefault: Double)(using p: SimParams): (Double, Double, Double, Double) =
    if totalBondDefault <= 0 || state.outstanding <= PLN.Zero then return (0.0, 0.0, 0.0, 0.0)
    val defaultFrac       = Math.min(1.0, totalBondDefault / state.outstanding.toDouble)
    val bankDefault       = state.bankHoldings.toDouble * defaultFrac
    val ppkDefault        = state.ppkHoldings.toDouble * defaultFrac
    val lossAfterRecovery = totalBondDefault * (1.0 - p.corpBond.recovery.toDouble)
    val bankLoss          = bankDefault * (1.0 - p.corpBond.recovery.toDouble)
    val ppkLoss           = ppkDefault * (1.0 - p.corpBond.recovery.toDouble)
    (totalBondDefault, lossAfterRecovery, bankLoss, ppkLoss)

  /** Monthly amortization: outstanding / maturity. */
  def amortization(state: State)(using p: SimParams): Double =
    state.outstanding.toDouble / Math.max(1.0, p.corpBond.maturity)

  /** Compute market absorption rate for new bond issuance. Gate 1: spread-based
    * investor appetite (cyclical). Gate 2: bank CAR headroom (banks hold
    * CorpBondBankShare at 50% RW). Returns absorption rate in [0.3, 1.0].
    */
  def computeAbsorption(state: State, tentativeIssuance: Double, aggBankCar: Double, minCar: Double)(using
      p: SimParams,
  ): Double =
    if tentativeIssuance <= 0 then return 1.0
    // Gate 1: spread-based appetite
    val excessSpread     = Math.max(0.0, state.creditSpread.toDouble - p.corpBond.spread.toDouble)
    val spreadAbsorption = Math.max(0.3, 1.0 - excessSpread / 0.10)
    // Gate 2: bank CAR headroom (linear ramp in 200 bps buffer zone)
    val carAbsorption    =
      if aggBankCar <= minCar then 0.3
      else if aggBankCar >= minCar + 0.02 then 1.0
      else 0.3 + 0.7 * (aggBankCar - minCar) / 0.02
    Math.max(0.3, Math.min(1.0, spreadAbsorption * carAbsorption))

  /** Process new issuance: allocate to holders proportionally. */
  def processIssuance(state: State, issuance: Double)(using p: SimParams): State =
    if issuance <= 0 then return state.copy(lastIssuance = PLN.Zero)
    state.copy(
      outstanding = state.outstanding + PLN(issuance),
      bankHoldings = state.bankHoldings + PLN(issuance * p.corpBond.bankShare.toDouble),
      ppkHoldings = state.ppkHoldings + PLN(issuance * p.corpBond.ppkShare.toDouble),
      otherHoldings = state.otherHoldings + PLN(
        issuance *
          (1.0 - p.corpBond.bankShare.toDouble - p.corpBond.ppkShare.toDouble),
      ),
      lastIssuance = PLN(issuance),
    )

  /** Full monthly step: yield -> coupon -> amortization -> default -> issuance.
    */
  def step(
      prev: State,
      govBondYield: Double,
      nplRatio: Double,
      totalBondDefault: Double,
      totalBondIssuance: Double,
  )(using p: SimParams): State =
    val newYield                   = computeYield(govBondYield, nplRatio)
    val (_, bankCoupon, ppkCoupon) = computeCoupon(prev)
    val amort                      = amortization(prev)
    val (grossDefault, _, _, _)    = processDefaults(prev, totalBondDefault)
    // Subtract amortization + defaults from outstanding and holdings (proportional)
    val reduction                  = amort + grossDefault
    val reductionFrac              =
      if prev.outstanding > PLN.Zero then Math.min(1.0, reduction / prev.outstanding.toDouble) else 0.0
    val afterReduction             = prev.copy(
      outstanding = (prev.outstanding - PLN(reduction)).max(PLN.Zero),
      bankHoldings = (prev.bankHoldings - prev.bankHoldings * reductionFrac).max(PLN.Zero),
      ppkHoldings = (prev.ppkHoldings - prev.ppkHoldings * reductionFrac).max(PLN.Zero),
      otherHoldings = (prev.otherHoldings - prev.otherHoldings * reductionFrac).max(PLN.Zero),
      corpBondYield = Rate(newYield),
      creditSpread = Rate(Math.max(0.0, newYield - govBondYield)),
      lastAmortization = PLN(amort),
      lastDefaultAmount = PLN(grossDefault),
      lastDefaultLoss = PLN(grossDefault * (1.0 - p.corpBond.recovery.toDouble)),
      lastCouponIncome = PLN(bankCoupon + ppkCoupon),
    )
    // Add new issuance
    processIssuance(afterReduction, totalBondIssuance)
