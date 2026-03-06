package sfc.agents

import sfc.config.Config
import sfc.types.*

/** Shadow Banking / NBFI: TFI investment funds + NBFI credit (leasing + fintech). */
object Nbfi:
  case class State(
                    // TFI component
                    tfiAum: PLN, // Total AUM
                    tfiGovBondHoldings: PLN, // Gov bonds (target share)
                    tfiCorpBondHoldings: PLN, // Corp bonds (target share)
                    tfiEquityHoldings: PLN, // Equities (target share)
                    tfiCashHoldings: PLN, // Cash/money market (residual)
                    // NBFI credit component (leasing + fintech)
                    nbfiLoanStock: PLN, // Outstanding NBFI loans
                    // Flow tracking
                    lastTfiNetInflow: PLN = PLN.Zero, // HH net fund purchases
                    lastNbfiOrigination: PLN = PLN.Zero, // Monthly new NBFI credit
                    lastNbfiRepayment: PLN = PLN.Zero, // Monthly principal repaid
                    lastNbfiDefaultAmount: PLN = PLN.Zero, // Monthly gross defaults
                    lastNbfiInterestIncome: PLN = PLN.Zero, // NBFI interest earned
                    lastBankTightness: Ratio = Ratio.Zero, // Counter-cyclical signal
                    lastDepositDrain: PLN = PLN.Zero, // Net deposit outflow (TFI inflow)
                  )

  def zero: State = State(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)

  def initial: State =
    val aum = PLN(Config.NbfiTfiInitAum)
    State(
      tfiAum = aum,
      tfiGovBondHoldings = aum * Config.NbfiTfiGovBondShare,
      tfiCorpBondHoldings = aum * Config.NbfiTfiCorpBondShare,
      tfiEquityHoldings = aum * Config.NbfiTfiEquityShare,
      tfiCashHoldings =
        aum * (1.0 - Config.NbfiTfiGovBondShare - Config.NbfiTfiCorpBondShare - Config.NbfiTfiEquityShare),
      nbfiLoanStock = PLN(Config.NbfiCreditInitStock),
    )

  /** Bank credit tightness signal: 0 at NPL <= 3%, rises linearly, 1.0 at 6%. */
  def bankTightness(bankNplRatio: Double): Double =
    Math.max(0.0, Math.min(1.0, (bankNplRatio - 0.03) / 0.03))

  /** TFI net inflow: proportional to wage bill, modulated by excess returns. */
  def tfiInflow(employed: Int, wage: Double, equityReturn: Double, govBondYield: Double, depositRate: Double): Double =
    val wageBill = employed.toDouble * wage
    val base = wageBill * Config.NbfiTfiInflowRate
    // Excess return: weighted avg of fund returns vs deposit rate
    val fundReturn = govBondYield * Config.NbfiTfiGovBondShare +
      equityReturn * 12.0 * Config.NbfiTfiEquityShare +
      govBondYield * Config.NbfiTfiCorpBondShare // proxy: corp ~ gov yield
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
  def step(
            prev: State,
            employed: Int,
            wage: Double,
            priceLevel: Double,
            unempRate: Double,
            bankNplRatio: Double,
            govBondYield: Double,
            corpBondYield: Double,
            equityReturn: Double,
            depositRate: Double,
            domesticCons: Double,
          ): State =
    // TFI: inflow + investment income + rebalance
    val netInflow = tfiInflow(employed, wage, equityReturn, govBondYield, depositRate)
    val invIncome = prev.tfiGovBondHoldings * govBondYield / 12.0 +
      prev.tfiCorpBondHoldings * corpBondYield / 12.0 +
      prev.tfiEquityHoldings * equityReturn
    val newAum = (prev.tfiAum + PLN(netInflow) + invIncome).max(PLN.Zero)

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
    val depositDrain = PLN(-netInflow)

    // NBFI credit: counter-cyclical origination -> repayment -> defaults
    val tight = bankTightness(bankNplRatio)
    val origination = nbfiOrigination(domesticCons, bankNplRatio)
    val repayment = nbfiRepayment(prev.nbfiLoanStock.toDouble)
    val defaults = nbfiDefaults(prev.nbfiLoanStock.toDouble, unempRate)
    val newLoanStock = (prev.nbfiLoanStock + PLN(origination) - PLN(repayment) - PLN(defaults)).max(PLN.Zero)
    val interestIncome = prev.nbfiLoanStock * Config.NbfiCreditRate / 12.0

    State(
      tfiAum = newAum,
      tfiGovBondHoldings = newGov,
      tfiCorpBondHoldings = newCorp,
      tfiEquityHoldings = newEq,
      tfiCashHoldings = newCash,
      nbfiLoanStock = newLoanStock,
      lastTfiNetInflow = PLN(netInflow),
      lastNbfiOrigination = PLN(origination),
      lastNbfiRepayment = PLN(repayment),
      lastNbfiDefaultAmount = PLN(defaults),
      lastNbfiInterestIncome = interestIncome,
      lastBankTightness = Ratio(tight),
      lastDepositDrain = depositDrain,
    )
