package sfc.engine

import sfc.config.Config

/** Shadow Banking / NBFI state: TFI investment funds + NBFI credit (leasing + fintech) (#42). */
case class NbfiState(
  // TFI component
  tfiAum: Double,                     // Total AUM
  tfiGovBondHoldings: Double,         // Gov bonds (target share)
  tfiCorpBondHoldings: Double,        // Corp bonds (target share)
  tfiEquityHoldings: Double,          // Equities (target share)
  tfiCashHoldings: Double,            // Cash/money market (residual)
  // NBFI credit component (leasing + fintech)
  nbfiLoanStock: Double,              // Outstanding NBFI loans
  // Flow tracking
  lastTfiNetInflow: Double = 0.0,     // HH net fund purchases
  lastNbfiOrigination: Double = 0.0,  // Monthly new NBFI credit
  lastNbfiRepayment: Double = 0.0,    // Monthly principal repaid
  lastNbfiDefaultAmount: Double = 0.0,// Monthly gross defaults
  lastNbfiInterestIncome: Double = 0.0,// NBFI interest earned
  lastBankTightness: Double = 0.0,    // Counter-cyclical signal
  lastDepositDrain: Double = 0.0      // Net deposit outflow (TFI inflow)
)

object ShadowBanking:
  def zero: NbfiState = NbfiState(0, 0, 0, 0, 0, 0)

  def initial: NbfiState =
    val aum = Config.NbfiTfiInitAum
    NbfiState(
      tfiAum = aum,
      tfiGovBondHoldings = aum * Config.NbfiTfiGovBondShare,
      tfiCorpBondHoldings = aum * Config.NbfiTfiCorpBondShare,
      tfiEquityHoldings = aum * Config.NbfiTfiEquityShare,
      tfiCashHoldings = aum * (1.0 - Config.NbfiTfiGovBondShare - Config.NbfiTfiCorpBondShare - Config.NbfiTfiEquityShare),
      nbfiLoanStock = Config.NbfiCreditInitStock
    )

  /** Bank credit tightness signal: 0 at NPL <= 3%, rises linearly, 1.0 at 6%. */
  def bankTightness(bankNplRatio: Double): Double =
    Math.max(0.0, Math.min(1.0, (bankNplRatio - 0.03) / 0.03))

  /** TFI net inflow: proportional to wage bill, modulated by excess returns. */
  def tfiInflow(employed: Int, wage: Double, equityReturn: Double,
                govBondYield: Double, depositRate: Double): Double =
    val wageBill = employed.toDouble * wage
    val base = wageBill * Config.NbfiTfiInflowRate
    // Excess return: weighted avg of fund returns vs deposit rate
    val fundReturn = govBondYield * Config.NbfiTfiGovBondShare +
      equityReturn * 12.0 * Config.NbfiTfiEquityShare +
      govBondYield * Config.NbfiTfiCorpBondShare  // proxy: corp ~ gov yield
    val excessReturn = Math.max(-0.05, Math.min(0.05, fundReturn - depositRate))
    base * (1.0 + excessReturn * 5.0)

  /** NBFI credit origination: counter-cyclical to bank tightness. */
  def nbfiOrigination(domesticCons: Double, bankNplRatio: Double): Double =
    val tight = bankTightness(bankNplRatio)
    domesticCons * Config.NbfiCreditBaseRate * (1.0 + Config.NbfiCountercyclical * tight)

  /** NBFI loan repayment: stock / maturity. */
  def nbfiRepayment(loanStock: Double): Double =
    loanStock / Config.NbfiCreditMaturity

  /** NBFI defaults: base rate widening with unemployment (sensitivity 3.0). */
  def nbfiDefaults(loanStock: Double, unempRate: Double): Double =
    loanStock * Config.NbfiDefaultBase * (1.0 + Config.NbfiDefaultUnempSens * Math.max(0.0, unempRate - 0.05))

  /** Full monthly step: TFI inflow -> investment income -> rebalance; NBFI credit flows. */
  def step(prev: NbfiState, employed: Int, wage: Double, priceLevel: Double,
           unempRate: Double, bankNplRatio: Double, govBondYield: Double,
           corpBondYield: Double, equityReturn: Double,
           depositRate: Double, domesticCons: Double): NbfiState =
    // TFI: inflow + investment income + rebalance
    val netInflow = tfiInflow(employed, wage, equityReturn, govBondYield, depositRate)
    val invIncome = prev.tfiGovBondHoldings * govBondYield / 12.0 +
      prev.tfiCorpBondHoldings * corpBondYield / 12.0 +
      prev.tfiEquityHoldings * equityReturn
    val newAum = Math.max(0.0, prev.tfiAum + netInflow + invIncome)

    // Rebalance towards target allocation
    val s = Config.NbfiTfiRebalanceSpeed
    val targetGov = newAum * Config.NbfiTfiGovBondShare
    val targetCorp = newAum * Config.NbfiTfiCorpBondShare
    val targetEq = newAum * Config.NbfiTfiEquityShare
    val newGov = prev.tfiGovBondHoldings + (targetGov - prev.tfiGovBondHoldings) * s
    val newCorp = prev.tfiCorpBondHoldings + (targetCorp - prev.tfiCorpBondHoldings) * s
    val newEq = prev.tfiEquityHoldings + (targetEq - prev.tfiEquityHoldings) * s
    val newCash = newAum - newGov - newCorp - newEq

    // Deposit drain: HH buys fund units -> deposits decrease
    val depositDrain = -netInflow

    // NBFI credit: counter-cyclical origination -> repayment -> defaults
    val tight = bankTightness(bankNplRatio)
    val origination = nbfiOrigination(domesticCons, bankNplRatio)
    val repayment = nbfiRepayment(prev.nbfiLoanStock)
    val defaults = nbfiDefaults(prev.nbfiLoanStock, unempRate)
    val newLoanStock = Math.max(0.0, prev.nbfiLoanStock + origination - repayment - defaults)
    val interestIncome = prev.nbfiLoanStock * Config.NbfiCreditRate / 12.0

    NbfiState(
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
      lastDepositDrain = depositDrain
    )
