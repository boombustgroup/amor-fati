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

  private val NplTightnessFloor: Share      = Share.decimal(3, 2) // NPL ratio below which bank tightness = 0
  private val NplTightnessRange: Share      = Share.decimal(3, 2) // NPL range over which tightness rises from 0 → 1
  private val UnempDefaultThreshold: Share  = Share.decimal(5, 2) // unemployment rate below which no cyclical default add-on
  private val ExcessReturnSens: Coefficient = Coefficient(5)      // TFI inflow sensitivity to excess fund vs deposit return
  private val ExcessReturnCap: Rate         = Rate.decimal(5, 2)  // cap on absolute excess return signal

  // ---------------------------------------------------------------------------
  // State
  // ---------------------------------------------------------------------------

  case class OpeningBalances(
      tfiAum: PLN,             // total assets under management
      tfiGovBondHoldings: PLN, // gov bonds (target share)
      corpBondHoldings: PLN,   // ledger-owned corporate bond opening stock
      tfiEquityHoldings: PLN,  // equities (target share)
      nbfiLoanStock: PLN,      // outstanding NBFI loans
  )

  case class ClosingBalances(
      tfiAum: PLN,             // total assets under management
      tfiGovBondHoldings: PLN, // gov bonds (target share)
      tfiEquityHoldings: PLN,  // equities (target share)
      nbfiLoanStock: PLN,      // outstanding NBFI loans
  )

  object ClosingBalances:
    val zero: ClosingBalances = ClosingBalances(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)

  case class FlowState(
      lastTfiNetInflow: PLN,       // HH net fund purchases this month
      lastNbfiOrigination: PLN,    // monthly new NBFI credit
      lastNbfiRepayment: PLN,      // monthly principal repaid
      lastNbfiDefaultAmount: PLN,  // monthly gross defaults
      lastNbfiInterestIncome: PLN, // NBFI interest earned this month
      lastBankTightness: Share,    // counter-cyclical signal [0,1]
      lastDepositDrain: PLN,       // net deposit outflow (TFI inflow)
  )

  case class State(
      monthly: FlowState,
  ):
    def lastTfiNetInflow: PLN       = monthly.lastTfiNetInflow
    def lastNbfiOrigination: PLN    = monthly.lastNbfiOrigination
    def lastNbfiRepayment: PLN      = monthly.lastNbfiRepayment
    def lastNbfiDefaultAmount: PLN  = monthly.lastNbfiDefaultAmount
    def lastNbfiInterestIncome: PLN = monthly.lastNbfiInterestIncome
    def lastBankTightness: Share    = monthly.lastBankTightness
    def lastDepositDrain: PLN       = monthly.lastDepositDrain

  object State:
    def apply(
        lastTfiNetInflow: PLN,
        lastNbfiOrigination: PLN,
        lastNbfiRepayment: PLN,
        lastNbfiDefaultAmount: PLN,
        lastNbfiInterestIncome: PLN,
        lastBankTightness: Share,
        lastDepositDrain: PLN,
    ): State =
      State(
        monthly = FlowState(
          lastTfiNetInflow,
          lastNbfiOrigination,
          lastNbfiRepayment,
          lastNbfiDefaultAmount,
          lastNbfiInterestIncome,
          lastBankTightness,
          lastDepositDrain,
        ),
      )

    val zero: State = State(
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      PLN.Zero,
      Share.Zero,
      PLN.Zero,
    )

  /** Initialize from SimParams calibration. */
  def initial: State = State.zero

  def initialBalances(using p: SimParams): ClosingBalances =
    val aum = p.nbfi.tfiInitAum
    ClosingBalances(
      tfiAum = aum,
      tfiGovBondHoldings = aum * p.nbfi.tfiGovBondShare,
      tfiEquityHoldings = aum * p.nbfi.tfiEquityShare,
      nbfiLoanStock = p.nbfi.creditInitStock,
    )

  // ---------------------------------------------------------------------------
  // Pure helper functions
  // ---------------------------------------------------------------------------

  /** Bank credit tightness signal: 0 at NPL ≤ 3%, rises linearly, 1.0 at 6%. */
  def bankTightness(bankNplRatio: Share): Share =
    val nplExcess = (bankNplRatio - NplTightnessFloor).max(Share.Zero)
    nplExcess.ratioTo(NplTightnessRange).toShare.clamp(Share.Zero, Share.One)

  /** TFI net inflow: proportional to wage bill, modulated by excess returns. */
  def tfiInflow(employed: Int, wage: PLN, equityReturn: Rate, govBondYield: Rate, depositRate: Rate)(using
      p: SimParams,
  ): PLN =
    val wageBill   = employed * wage
    val base       = wageBill * p.nbfi.tfiInflowRate
    // Excess return: weighted avg of fund returns vs deposit rate
    val fundReturn = govBondYield * p.nbfi.tfiGovBondShare +
      equityReturn.annualize * p.nbfi.tfiEquityShare +
      govBondYield * p.nbfi.tfiCorpBondShare // proxy: corp ~ gov yield
    val excessReturn = (fundReturn - depositRate).clamp(-ExcessReturnCap, ExcessReturnCap)
    base * (Multiplier.One + (excessReturn * ExcessReturnSens).toMultiplier)

  /** NBFI credit origination: counter-cyclical to bank tightness. */
  def nbfiOrigination(domesticCons: PLN, bankNplRatio: Share)(using p: SimParams): PLN =
    val tight       = bankTightness(bankNplRatio)
    val cyclicalAdj = Multiplier.One + (p.nbfi.countercyclical * tight).toMultiplier
    domesticCons * p.nbfi.creditBaseRate * cyclicalAdj

  /** NBFI loan repayment: stock / maturity. */
  def nbfiRepayment(loanStock: PLN)(using p: SimParams): PLN =
    loanStock / math.max(1, p.nbfi.creditMaturity.toLong.toInt / 10000)

  /** NBFI defaults: base rate widening with unemployment. */
  def nbfiDefaults(loanStock: PLN, unempRate: Share)(using p: SimParams): PLN =
    val unempExcess = (unempRate - UnempDefaultThreshold).max(Share.Zero)
    val cyclicalAdj = Multiplier.One + (p.nbfi.defaultUnempSens * unempExcess).toMultiplier
    loanStock * p.nbfi.defaultBase * cyclicalAdj

  // ---------------------------------------------------------------------------
  // Monthly step
  // ---------------------------------------------------------------------------

  case class StepInput(
      opening: OpeningBalances,
      employed: Int,                                   // employed workers
      wage: PLN,                                       // average monthly wage
      @scala.annotation.unused priceLevel: PriceIndex, // CPI price level (unused in current spec, kept for interface stability)
      unempRate: Share,                                // unemployment rate
      bankNplRatio: Share,                             // aggregate bank NPL ratio (tightness signal)
      govBondYield: Rate,                              // government bond yield (annualised)
      corpBondYield: Rate,                             // corporate bond yield (annualised)
      equityReturn: Rate,                              // equity monthly return
      depositRate: Rate,                               // bank deposit rate (TFI opportunity cost)
      domesticCons: PLN,                               // domestic consumption (NBFI credit base)
      corpBondDefaultLoss: PLN,
  )

  case class StepResult(state: State, closing: ClosingBalances)

  /** Full monthly step: TFI inflow → investment income → rebalance; NBFI credit
    * flows.
    *
    * Corporate bond holdings are settled by CorporateBondMarket and owned by
    * LedgerFinancialState. TFI receives the opening stock for income; residual
    * cash is derived later by the ledger from the settled closing stock.
    */
  def step(input: StepInput)(using p: SimParams): StepResult =
    val opening = input.opening

    // TFI: inflow + investment income + rebalance
    val netInflow             = tfiInflow(input.employed, input.wage, input.equityReturn, input.govBondYield, input.depositRate)
    val grossInvestmentIncome = opening.tfiGovBondHoldings * input.govBondYield.monthly +
      opening.corpBondHoldings * input.corpBondYield.monthly +
      opening.tfiEquityHoldings * input.equityReturn
    val invIncome             = grossInvestmentIncome - input.corpBondDefaultLoss
    val newAum                = (opening.tfiAum + netInflow + invIncome).max(PLN.Zero)

    // Rebalance towards target allocation
    val s         = p.nbfi.tfiRebalanceSpeed
    val targetGov = newAum * p.nbfi.tfiGovBondShare
    val targetEq  = newAum * p.nbfi.tfiEquityShare
    val newGov    = opening.tfiGovBondHoldings + (targetGov - opening.tfiGovBondHoldings) * s
    val newEq     = opening.tfiEquityHoldings + (targetEq - opening.tfiEquityHoldings) * s

    // Deposit drain: HH buys fund units → deposits decrease
    val depositDrain = -netInflow

    // NBFI credit: counter-cyclical origination → repayment → defaults
    val tight          = bankTightness(input.bankNplRatio)
    val origination    = nbfiOrigination(input.domesticCons, input.bankNplRatio)
    val repayment      = nbfiRepayment(opening.nbfiLoanStock)
    val defaults       = nbfiDefaults(opening.nbfiLoanStock, input.unempRate)
    val newLoanStock   = (opening.nbfiLoanStock + origination - repayment - defaults).max(PLN.Zero)
    val interestIncome = opening.nbfiLoanStock * p.nbfi.creditRate.monthly

    StepResult(
      state = State(
        lastTfiNetInflow = netInflow,
        lastNbfiOrigination = origination,
        lastNbfiRepayment = repayment,
        lastNbfiDefaultAmount = defaults,
        lastNbfiInterestIncome = interestIncome,
        lastBankTightness = tight,
        lastDepositDrain = depositDrain,
      ),
      closing = ClosingBalances(
        tfiAum = newAum,
        tfiGovBondHoldings = newGov,
        tfiEquityHoldings = newEq,
        nbfiLoanStock = newLoanStock,
      ),
    )
