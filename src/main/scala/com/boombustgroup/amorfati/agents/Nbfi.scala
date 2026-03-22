package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

/** Shadow Banking / NBFI: TFI investment funds + NBFI credit (leasing +
  * fintech).
  */
object Nbfi:

  // ---------------------------------------------------------------------------
  // Named constants
  // ---------------------------------------------------------------------------

  private val NplTightnessFloor     = 0.03 // NPL ratio below which bank tightness = 0
  private val NplTightnessRange     = 0.03 // NPL range over which tightness rises from 0 → 1
  private val UnempDefaultThreshold = 0.05 // unemployment rate below which no cyclical default add-on
  private val ExcessReturnSens      = 5.0  // TFI inflow sensitivity to excess fund vs deposit return
  private val ExcessReturnCap       = 0.05 // cap on absolute excess return signal
  private val MonthsPerYear         = 12.0

  // ---------------------------------------------------------------------------
  // State
  // ---------------------------------------------------------------------------

  case class State(
      // TFI component
      tfiAum: PLN,                 // total assets under management
      tfiGovBondHoldings: PLN,     // gov bonds (target share)
      tfiCorpBondHoldings: PLN,    // corp bonds (target share)
      tfiEquityHoldings: PLN,      // equities (target share)
      tfiCashHoldings: PLN,        // cash/money market (residual)
      // NBFI credit component (leasing + fintech)
      nbfiLoanStock: PLN,          // outstanding NBFI loans
      // Flow tracking
      lastTfiNetInflow: PLN,       // HH net fund purchases this month
      lastNbfiOrigination: PLN,    // monthly new NBFI credit
      lastNbfiRepayment: PLN,      // monthly principal repaid
      lastNbfiDefaultAmount: PLN,  // monthly gross defaults
      lastNbfiInterestIncome: PLN, // NBFI interest earned this month
      lastBankTightness: Share,    // counter-cyclical signal [0,1]
      lastDepositDrain: PLN,       // net deposit outflow (TFI inflow)
  )

  object State:
    val zero: State = State(
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      Share.Zero,
      PLN.Zero,
    )

  /** Initialize from SimParams calibration. */
  def initial(using p: SimParams): State =
    val aum = p.nbfi.tfiInitAum
    State(
      tfiAum = aum,
      tfiGovBondHoldings = aum * p.nbfi.tfiGovBondShare,
      tfiCorpBondHoldings = aum * p.nbfi.tfiCorpBondShare,
      tfiEquityHoldings = aum * p.nbfi.tfiEquityShare,
      tfiCashHoldings = aum * (Share.One - p.nbfi.tfiGovBondShare - p.nbfi.tfiCorpBondShare - p.nbfi.tfiEquityShare),
      nbfiLoanStock = p.nbfi.creditInitStock,
      lastTfiNetInflow = PLN.Zero,
      lastNbfiOrigination = PLN.Zero,
      lastNbfiRepayment = PLN.Zero,
      lastNbfiDefaultAmount = PLN.Zero,
      lastNbfiInterestIncome = PLN.Zero,
      lastBankTightness = Share.Zero,
      lastDepositDrain = PLN.Zero,
    )

  // ---------------------------------------------------------------------------
  // Pure helper functions
  // ---------------------------------------------------------------------------

  /** Bank credit tightness signal: 0 at NPL ≤ 3%, rises linearly, 1.0 at 6%. */
  @computationBoundary
  def bankTightness(bankNplRatio: Share): Share =
    Share((ComputationBoundary.toDouble(bankNplRatio) - NplTightnessFloor) / NplTightnessRange).clamp(Share.Zero, Share.One)

  /** TFI net inflow: proportional to wage bill, modulated by excess returns. */
  @computationBoundary
  def tfiInflow(employed: Int, wage: PLN, equityReturn: Rate, govBondYield: Rate, depositRate: Rate)(using
      p: SimParams,
  ): PLN =
    val wageBill   = employed * wage
    val base       = wageBill * p.nbfi.tfiInflowRate
    // Excess return: weighted avg of fund returns vs deposit rate
    val fundReturn = ComputationBoundary.toDouble(govBondYield) * ComputationBoundary.toDouble(p.nbfi.tfiGovBondShare) +
      ComputationBoundary.toDouble(equityReturn) * MonthsPerYear * ComputationBoundary.toDouble(p.nbfi.tfiEquityShare) +
      ComputationBoundary.toDouble(govBondYield) * ComputationBoundary.toDouble(p.nbfi.tfiCorpBondShare) // proxy: corp ~ gov yield
    val excessReturn = Math.max(-ExcessReturnCap, Math.min(ExcessReturnCap, fundReturn - ComputationBoundary.toDouble(depositRate)))
    base * Multiplier(1.0 + excessReturn * ExcessReturnSens)

  /** NBFI credit origination: counter-cyclical to bank tightness. */
  def nbfiOrigination(domesticCons: PLN, bankNplRatio: Share)(using p: SimParams): PLN =
    val tight       = bankTightness(bankNplRatio)
    val cyclicalAdj = Multiplier.One + (p.nbfi.countercyclical * tight).toMultiplier
    domesticCons * p.nbfi.creditBaseRate * cyclicalAdj

  /** NBFI loan repayment: stock / maturity. */
  def nbfiRepayment(loanStock: PLN)(using p: SimParams): PLN =
    loanStock / p.nbfi.creditMaturity.toLong.max(1L)

  /** NBFI defaults: base rate widening with unemployment. */
  def nbfiDefaults(loanStock: PLN, unempRate: Share)(using p: SimParams): PLN =
    val unempExcess = (unempRate - Share(UnempDefaultThreshold)).max(Share.Zero)
    val cyclicalAdj = Multiplier.One + (p.nbfi.defaultUnempSens * unempExcess).toMultiplier
    loanStock * p.nbfi.defaultBase * cyclicalAdj

  // ---------------------------------------------------------------------------
  // Monthly step
  // ---------------------------------------------------------------------------

  /** Full monthly step: TFI inflow → investment income → rebalance; NBFI credit
    * flows.
    */
  def step(
      prev: State,
      employed: Int,                               // employed workers
      wage: PLN,                                   // average monthly wage
      @scala.annotation.unused priceLevel: Double, // CPI price level (unused in current spec, kept for interface stability)
      unempRate: Share,                            // unemployment rate
      bankNplRatio: Share,                         // aggregate bank NPL ratio (tightness signal)
      govBondYield: Rate,                          // government bond yield (annualised)
      corpBondYield: Rate,                         // corporate bond yield (annualised)
      equityReturn: Rate,                          // equity monthly return
      depositRate: Rate,                           // bank deposit rate (TFI opportunity cost)
      domesticCons: PLN,                           // domestic consumption (NBFI credit base)
  )(using p: SimParams): State =
    // TFI: inflow + investment income + rebalance
    val netInflow = tfiInflow(employed, wage, equityReturn, govBondYield, depositRate)
    val invIncome = prev.tfiGovBondHoldings * govBondYield.monthly +
      prev.tfiCorpBondHoldings * corpBondYield.monthly +
      prev.tfiEquityHoldings * equityReturn
    val newAum    = (prev.tfiAum + netInflow + invIncome).max(PLN.Zero)

    // Rebalance towards target allocation
    val s          = p.nbfi.tfiRebalanceSpeed
    val targetGov  = newAum * p.nbfi.tfiGovBondShare
    val targetCorp = newAum * p.nbfi.tfiCorpBondShare
    val targetEq   = newAum * p.nbfi.tfiEquityShare
    val newGov     = prev.tfiGovBondHoldings + (targetGov - prev.tfiGovBondHoldings) * s
    val newCorp    = prev.tfiCorpBondHoldings + (targetCorp - prev.tfiCorpBondHoldings) * s
    val newEq      = prev.tfiEquityHoldings + (targetEq - prev.tfiEquityHoldings) * s
    val newCash    = newAum - newGov - newCorp - newEq

    // Deposit drain: HH buys fund units → deposits decrease
    val depositDrain = -netInflow

    // NBFI credit: counter-cyclical origination → repayment → defaults
    val tight          = bankTightness(bankNplRatio)
    val origination    = nbfiOrigination(domesticCons, bankNplRatio)
    val repayment      = nbfiRepayment(prev.nbfiLoanStock)
    val defaults       = nbfiDefaults(prev.nbfiLoanStock, unempRate)
    val newLoanStock   = (prev.nbfiLoanStock + origination - repayment - defaults).max(PLN.Zero)
    val interestIncome = prev.nbfiLoanStock * p.nbfi.creditRate.monthly

    State(
      tfiAum = newAum,
      tfiGovBondHoldings = newGov,
      tfiCorpBondHoldings = newCorp,
      tfiEquityHoldings = newEq,
      tfiCashHoldings = newCash,
      nbfiLoanStock = newLoanStock,
      lastTfiNetInflow = netInflow,
      lastNbfiOrigination = origination,
      lastNbfiRepayment = repayment,
      lastNbfiDefaultAmount = defaults,
      lastNbfiInterestIncome = interestIncome,
      lastBankTightness = tight,
      lastDepositDrain = depositDrain,
    )
