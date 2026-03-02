package sfc.sfc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.agents.*
import sfc.config.{Config, SECTORS, RunConfig}
import sfc.engine.World

class SfcCheckSpec extends AnyFlatSpec with Matchers:

  private def makeWorld(bankCapital: Double = 500000.0,
                        bankDeposits: Double = 1000000.0,
                        bankLoans: Double = 0.0,
                        bankNpl: Double = 0.0,
                        govDebt: Double = 0.0): World =
    World(
      month = 1, inflation = 0.02, priceLevel = 1.0,
      gov = GovState(false, 0, 0, 0, govDebt, 0),
      nbp = NbpState(0.0575),
      bank = BankState(bankLoans, bankNpl, bankCapital, bankDeposits),
      forex = ForexState(4.33, 0, 0, 0, 0),
      hh = HhState(100, 8266.0, 4666.0, 0, 0, 0, 0),
      automationRatio = 0.0, hybridRatio = 0.0,
      gdpProxy = 1e9,
      currentSigmas = SECTORS.map(_.sigma).toVector
    )

  private def makeFirms(n: Int, cash: Double = 50000.0, debt: Double = 0.0): Array[Firm] =
    (0 until n).map { i =>
      Firm(i, cash, debt, TechState.Traditional(10), 0.5, 1.0, 0.3, 0, Array.empty)
    }.toArray

  private def makeHouseholds(n: Int, savings: Double = 15000.0,
                              debt: Double = 0.0): Vector[Household] =
    (0 until n).map { i =>
      Household(i, savings, debt, 1800.0, 0.8, 0.0, 0.82,
        HhStatus.Employed(0, 0, 8266.0), Array.empty)
    }.toVector

  private val zeroFlows = SfcCheck.MonthlyFlows(0, 0, 0, 0, 0, 0, 0, 0, 0)

  // ---- Snapshot tests ----

  "SfcCheck.snapshot" should "correctly sum firm cash and debt" in {
    val w = makeWorld()
    val firms = makeFirms(5, cash = 10000.0, debt = 5000.0)
    val snap = SfcCheck.snapshot(w, firms, None)
    snap.firmCash shouldBe 50000.0 +- 0.01
    snap.firmDebt shouldBe 25000.0 +- 0.01
  }

  it should "correctly sum household savings and debt" in {
    val w = makeWorld()
    val firms = makeFirms(1)
    val hhs = makeHouseholds(10, savings = 20000.0, debt = 5000.0)
    val snap = SfcCheck.snapshot(w, firms, Some(hhs))
    snap.hhSavings shouldBe 200000.0 +- 0.01
    snap.hhDebt shouldBe 50000.0 +- 0.01
  }

  it should "return zero HH values in aggregate mode" in {
    val w = makeWorld()
    val firms = makeFirms(1)
    val snap = SfcCheck.snapshot(w, firms, None)
    snap.hhSavings shouldBe 0.0
    snap.hhDebt shouldBe 0.0
  }

  it should "capture bank state from World" in {
    val w = makeWorld(bankCapital = 123456.0, bankDeposits = 789012.0,
                      bankLoans = 50000.0, govDebt = 100000.0)
    val firms = makeFirms(1)
    val snap = SfcCheck.snapshot(w, firms, None)
    snap.bankCapital shouldBe 123456.0
    snap.bankDeposits shouldBe 789012.0
    snap.bankLoans shouldBe 50000.0
    snap.govDebt shouldBe 100000.0
  }

  // ---- Identity 1: Bank capital ----

  "SfcCheck.validate (bank capital)" should "pass when change matches formula exactly" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0)
    // nplLoss=7000, intIncome=10000, hhDebtService=2000
    // expected change = -7000 + 10000*0.3 + 2000*0.3 = -7000 + 3000 + 600 = -3400
    val curr = prev.copy(bankCapital = prev.bankCapital - 3400)
    val flows = zeroFlows.copy(nplLoss = 7000, interestIncome = 10000, hhDebtService = 2000)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.bankCapitalError shouldBe 0.0 +- 0.01
    result.passed shouldBe true
  }

  it should "detect error when NPL loss is not applied" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0)
    // Bug: bank capital didn't decrease by nplLoss
    val curr = prev.copy(bankCapital = prev.bankCapital)
    val flows = zeroFlows.copy(nplLoss = 5000)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.bankCapitalError shouldBe 5000.0 +- 0.01  // actual=0, expected=-5000
    result.passed shouldBe false
  }

  it should "detect error when interest income routing is wrong" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0)
    // Bug: 50% of interest goes to bank instead of 30%
    val curr = prev.copy(bankCapital = prev.bankCapital + 10000 * 0.5)
    val flows = zeroFlows.copy(interestIncome = 10000)
    val result = SfcCheck.validate(1, prev, curr, flows)
    // actual change = +5000, expected = +3000, error = 2000
    result.bankCapitalError shouldBe 2000.0 +- 0.01
    result.passed shouldBe false
  }

  it should "detect error when debt service is not routed to bank" in {
    val prev = SfcCheck.Snapshot(100000, 5000, 500000, 0, 200000, 1000000, 0, 0)
    // Bug: hhDebtService=3000 should add 900 to bank capital, but bank unchanged
    val curr = prev.copy(bankCapital = prev.bankCapital)
    val flows = zeroFlows.copy(hhDebtService = 3000)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.bankCapitalError shouldBe -900.0 +- 0.01  // actual=0, expected=+900
    result.passed shouldBe false
  }

  // ---- Identity 2: Bank deposits ----

  "SfcCheck.validate (bank deposits)" should "pass when change matches income - consumption" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0)
    // totalIncome=100000, consumption=82000 → deposit increase = 18000
    val curr = prev.copy(bankDeposits = prev.bankDeposits + 18000)
    val flows = zeroFlows.copy(totalIncome = 100000, totalConsumption = 82000)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.bankDepositsError shouldBe 0.0 +- 0.01
    result.passed shouldBe true
  }

  it should "detect error when deposits don't match income-consumption" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0)
    // Bug: deposits unchanged despite positive savings
    val curr = prev.copy(bankDeposits = prev.bankDeposits)
    val flows = zeroFlows.copy(totalIncome = 100000, totalConsumption = 82000)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.bankDepositsError shouldBe -18000.0 +- 0.01
    result.passed shouldBe false
  }

  // ---- Identity 3: Government debt ----

  "SfcCheck.validate (gov debt)" should "pass when change matches deficit" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 50000)
    // govSpending=30000, govRevenue=20000 → deficit=10000
    val curr = prev.copy(govDebt = prev.govDebt + 10000)
    val flows = zeroFlows.copy(govSpending = 30000, govRevenue = 20000)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.govDebtError shouldBe 0.0 +- 0.01
    result.passed shouldBe true
  }

  it should "pass with government surplus (negative deficit)" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 50000)
    // govSpending=15000, govRevenue=25000 → deficit=-10000 (surplus)
    val curr = prev.copy(govDebt = prev.govDebt - 10000)
    val flows = zeroFlows.copy(govSpending = 15000, govRevenue = 25000)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.govDebtError shouldBe 0.0 +- 0.01
    result.passed shouldBe true
  }

  it should "detect error when debt doesn't match deficit" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 50000)
    // Bug: debt doesn't change despite deficit
    val curr = prev.copy(govDebt = prev.govDebt)
    val flows = zeroFlows.copy(govSpending = 30000, govRevenue = 20000)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.govDebtError shouldBe -10000.0 +- 0.01
    result.passed shouldBe false
  }

  // ---- Zero-flow identity ----

  "SfcCheck.validate" should "pass with zero flows and no changes" in {
    val snap = SfcCheck.Snapshot(100000, 5000, 500000, 10000, 200000, 800000, 10000, 0)
    val result = SfcCheck.validate(1, snap, snap, zeroFlows)
    result.bankCapitalError shouldBe 0.0
    result.bankDepositsError shouldBe 0.0
    result.govDebtError shouldBe 0.0
    result.passed shouldBe true
  }

  // ---- Combined flows ----

  it should "pass when all three identities hold simultaneously" in {
    val prev = SfcCheck.Snapshot(100000, 5000, 500000, 10000, 200000, 800000, 10000, 50000)
    // Bank capital: -2000 nplLoss + 6000*0.3 intIncome + 1000*0.3 hhDebtSvc = -2000 + 1800 + 300 = 100
    // Deposits: totalIncome(50000) - consumption(41000) = 9000
    // Gov debt: spending(30000) - revenue(25000) = 5000
    val curr = prev.copy(
      bankCapital = prev.bankCapital + 100,
      bankDeposits = prev.bankDeposits + 9000,
      govDebt = prev.govDebt + 5000
    )
    val flows = SfcCheck.MonthlyFlows(
      govSpending = 30000, govRevenue = 25000,
      nplLoss = 2000, interestIncome = 6000,
      hhDebtService = 1000, totalIncome = 50000,
      totalConsumption = 41000, newLoans = 0, nplRecovery = 0
    )
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.bankCapitalError shouldBe 0.0 +- 0.01
    result.bankDepositsError shouldBe 0.0 +- 0.01
    result.govDebtError shouldBe 0.0 +- 0.01
    result.passed shouldBe true
  }

  // ---- Tolerance ----

  it should "pass when error is below tolerance" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0)
    // Bank capital off by 0.005 (below default tolerance of 0.01)
    val curr = prev.copy(bankCapital = prev.bankCapital + 0.005)
    val result = SfcCheck.validate(1, prev, curr, zeroFlows)
    result.passed shouldBe true
  }

  it should "fail when error exceeds tolerance" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0)
    // Bank capital off by 0.02 (above default tolerance of 0.01)
    val curr = prev.copy(bankCapital = prev.bankCapital + 0.02)
    val result = SfcCheck.validate(1, prev, curr, zeroFlows)
    result.passed shouldBe false
  }

  it should "respect custom tolerance parameter" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0)
    val curr = prev.copy(bankCapital = prev.bankCapital + 5.0)
    // Default tolerance (0.01): fails
    SfcCheck.validate(1, prev, curr, zeroFlows).passed shouldBe false
    // Loose tolerance (10.0): passes
    SfcCheck.validate(1, prev, curr, zeroFlows, tolerance = 10.0).passed shouldBe true
  }

  // ---- Identity 5: Bond clearing ----

  "SfcCheck.validate (bond clearing)" should "pass when holdings sum to outstanding" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0, 0.0,
      bankBondHoldings = 5000.0, nbpBondHoldings = 3000.0, bondsOutstanding = 8000.0)
    val result = SfcCheck.validate(1, prev, prev, zeroFlows)
    result.bondClearingError shouldBe 0.0 +- 0.01
    result.passed shouldBe true
  }

  it should "detect error when holdings don't sum to outstanding" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0, 0.0,
      bankBondHoldings = 5000.0, nbpBondHoldings = 3000.0, bondsOutstanding = 10000.0)
    val result = SfcCheck.validate(1, prev, prev, zeroFlows)
    result.bondClearingError shouldBe -2000.0 +- 0.01
    result.passed shouldBe false
  }

  it should "pass trivially when all bond fields are zero" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0)
    val result = SfcCheck.validate(1, prev, prev, zeroFlows)
    result.bondClearingError shouldBe 0.0
    result.passed shouldBe true
  }

  it should "include insurance gov bond holdings in bond clearing" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0, 0.0,
      bankBondHoldings = 5000.0, nbpBondHoldings = 3000.0, bondsOutstanding = 10000.0,
      insuranceGovBondHoldings = 2000.0)
    val result = SfcCheck.validate(1, prev, prev, zeroFlows)
    // 5000 + 3000 + 0 (ppk) + 2000 (insurance) = 10000 = outstanding
    result.bondClearingError shouldBe 0.0 +- 0.01
    result.passed shouldBe true
  }

  // ---- Identity 1 with bond income ----

  "SfcCheck.validate (bank capital with bond income)" should "include bankBondIncome in Identity 1" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0)
    // bankBondIncome=6000 → 6000*0.3 = 1800 added to bank capital
    val curr = prev.copy(bankCapital = prev.bankCapital + 1800)
    val flows = zeroFlows.copy(bankBondIncome = 6000)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.bankCapitalError shouldBe 0.0 +- 0.01
    result.passed shouldBe true
  }

  // ---- Unemployment benefit SFC flow ----

  // ---- Identity 6: Interbank netting ----

  "SfcCheck.validate (interbank netting)" should "pass when interbankNetSum is zero" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0,
      interbankNetSum = 0.0)
    val result = SfcCheck.validate(1, prev, prev, zeroFlows)
    result.interbankNettingError shouldBe 0.0
    result.passed shouldBe true
  }

  it should "detect error when interbankNetSum is non-zero" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0)
    val curr = prev.copy(interbankNetSum = 5000.0)
    val result = SfcCheck.validate(1, prev, curr, zeroFlows)
    result.interbankNettingError shouldBe 5000.0 +- 0.01
    result.passed shouldBe false
  }

  it should "pass trivially in single-bank mode (interbankNetSum=0)" in {
    val snap = SfcCheck.Snapshot(100000, 5000, 500000, 10000, 200000, 800000, 10000, 0)
    val result = SfcCheck.validate(1, snap, snap, zeroFlows)
    result.interbankNettingError shouldBe 0.0
    result.passed shouldBe true
  }

  // ---- Identity 2 with insurance deposit change (#41) ----

  "SfcCheck.validate (insurance deposits)" should "include insNetDepositChange in Identity 2" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0)
    // Insurance premium > claims → negative deposit change (drain)
    val insDepChange = -500.0
    val curr = prev.copy(bankDeposits = prev.bankDeposits + insDepChange)
    val flows = zeroFlows.copy(insNetDepositChange = insDepChange)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.bankDepositsError shouldBe 0.0 +- 0.01
    result.passed shouldBe true
  }

  // ---- Unemployment benefit SFC flow ----

  "SfcCheck.validate (gov debt with benefits)" should "pass when benefits included in govSpending" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 50000)
    val benefitSpend = 15000.0
    val baseGovSpend = 30000.0
    val totalGovSpend = baseGovSpend + benefitSpend  // 45000
    val govRevenue = 25000.0
    val expectedDeficit = totalGovSpend - govRevenue  // 20000
    val curr = prev.copy(govDebt = prev.govDebt + expectedDeficit)
    val flows = zeroFlows.copy(govSpending = totalGovSpend, govRevenue = govRevenue)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.govDebtError shouldBe 0.0 +- 0.01
    result.passed shouldBe true
  }

  // ---- Identity 1 with deposit interest ----

  "SfcCheck.validate (bank capital with deposit interest)" should "subtract deposit interest from bank capital" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0)
    // depositInterestPaid=3000 -> (0 + 0 + 0 - 3000) * 0.3 = -900
    val curr = prev.copy(bankCapital = prev.bankCapital - 900)
    val flows = zeroFlows.copy(depositInterestPaid = 3000)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.bankCapitalError shouldBe 0.0 +- 0.01
    result.passed shouldBe true
  }

  it should "pass with combined interest income and deposit interest" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0)
    // intIncome=10000, hhDebtService=2000, bankBondIncome=1000, depositInterestPaid=3000
    // expected = -0 + (10000 + 2000 + 1000 - 3000) * 0.3 = 10000 * 0.3 = 3000
    val curr = prev.copy(bankCapital = prev.bankCapital + 3000)
    val flows = zeroFlows.copy(interestIncome = 10000, hhDebtService = 2000,
      bankBondIncome = 1000, depositInterestPaid = 3000)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.bankCapitalError shouldBe 0.0 +- 0.01
    result.passed shouldBe true
  }

  it should "detect error when deposit interest not deducted from bank capital" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0)
    // Bug: bank capital unchanged despite deposit interest obligation
    val curr = prev.copy(bankCapital = prev.bankCapital)
    val flows = zeroFlows.copy(depositInterestPaid = 5000)
    val result = SfcCheck.validate(1, prev, curr, flows)
    // actual=0, expected=-5000*0.3=-1500, error=0-(-1500)=1500
    result.bankCapitalError shouldBe 1500.0 +- 0.01
    result.passed shouldBe false
  }

  // ---- Identity 2 with dividend flows ----

  "SfcCheck.validate (deposits with dividends)" should "pass when dividendIncome added to deposits" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0)
    // dividendIncome=5000 → deposits increase by 5000
    val curr = prev.copy(bankDeposits = prev.bankDeposits + 5000)
    val flows = zeroFlows.copy(dividendIncome = 5000)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.bankDepositsError shouldBe 0.0 +- 0.01
    result.passed shouldBe true
  }

  it should "pass when foreignDividendOutflow deducted from deposits" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0)
    // foreignDividendOutflow=8000 → deposits decrease by 8000
    val curr = prev.copy(bankDeposits = prev.bankDeposits - 8000)
    val flows = zeroFlows.copy(foreignDividendOutflow = 8000)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.bankDepositsError shouldBe 0.0 +- 0.01
    result.passed shouldBe true
  }

  it should "pass with combined dividend flows (income - outflow)" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0)
    // totalIncome=50000, totalCons=40000, divIncome=3000, foreignDiv=7000
    // expected deposit Δ = 50000 - 40000 + 3000 - 7000 = 6000
    val curr = prev.copy(bankDeposits = prev.bankDeposits + 6000)
    val flows = zeroFlows.copy(totalIncome = 50000, totalConsumption = 40000,
      dividendIncome = 3000, foreignDividendOutflow = 7000)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.bankDepositsError shouldBe 0.0 +- 0.01
    result.passed shouldBe true
  }

  it should "detect error when dividend income not added to deposits" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0)
    // Bug: deposits unchanged despite dividend income
    val curr = prev.copy(bankDeposits = prev.bankDeposits)
    val flows = zeroFlows.copy(dividendIncome = 10000)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.bankDepositsError shouldBe -10000.0 +- 0.01
    result.passed shouldBe false
  }

  // ---- Identity 1 with mortgage flows ----

  "SfcCheck.validate (bank capital with mortgage)" should "include mortgage interest in Identity 1" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0)
    // mortgageInterestIncome=9000 → 9000*0.3 = 2700 added to bank capital
    val curr = prev.copy(bankCapital = prev.bankCapital + 2700)
    val flows = zeroFlows.copy(mortgageInterestIncome = 9000)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.bankCapitalError shouldBe 0.0 +- 0.01
    result.passed shouldBe true
  }

  it should "include mortgage NPL loss in Identity 1" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0)
    // mortgageNplLoss=5000 → bank capital decreases by 5000
    val curr = prev.copy(bankCapital = prev.bankCapital - 5000)
    val flows = zeroFlows.copy(mortgageNplLoss = 5000)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.bankCapitalError shouldBe 0.0 +- 0.01
    result.passed shouldBe true
  }

  it should "pass with combined mortgage interest and NPL" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0)
    // mortgageInterest=10000 → +3000, mortgageNplLoss=2000 → -2000, net = +1000
    val curr = prev.copy(bankCapital = prev.bankCapital + 1000)
    val flows = zeroFlows.copy(mortgageInterestIncome = 10000, mortgageNplLoss = 2000)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.bankCapitalError shouldBe 0.0 +- 0.01
    result.passed shouldBe true
  }

  it should "detect error when mortgage interest not routed to bank capital" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0)
    // Bug: bank capital unchanged despite mortgage interest
    val curr = prev.copy(bankCapital = prev.bankCapital)
    val flows = zeroFlows.copy(mortgageInterestIncome = 6000)
    val result = SfcCheck.validate(1, prev, curr, flows)
    // actual=0, expected=+1800, error=-1800
    result.bankCapitalError shouldBe -1800.0 +- 0.01
    result.passed shouldBe false
  }

  // ---- Identity 9: Mortgage stock ----

  "SfcCheck.validate (mortgage stock)" should "pass when stock change matches flows" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0,
      mortgageStock = 100000.0)
    // origination=20000, principal=3000, default=1000 → Δ = 20000-3000-1000 = 16000
    val curr = prev.copy(mortgageStock = 116000.0)
    val flows = zeroFlows.copy(mortgageOrigination = 20000, mortgagePrincipalRepaid = 3000,
      mortgageDefaultAmount = 1000)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.mortgageStockError shouldBe 0.0 +- 0.01
    result.passed shouldBe true
  }

  it should "pass trivially when RE disabled (all zeros)" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0)
    val result = SfcCheck.validate(1, prev, prev, zeroFlows)
    result.mortgageStockError shouldBe 0.0
    result.passed shouldBe true
  }

  it should "detect error when stock doesn't match flows" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0,
      mortgageStock = 100000.0)
    // Bug: stock unchanged despite origination
    val curr = prev.copy(mortgageStock = 100000.0)
    val flows = zeroFlows.copy(mortgageOrigination = 15000)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.mortgageStockError shouldBe -15000.0 +- 0.01
    result.passed shouldBe false
  }

  it should "handle net reduction in stock (repayment > origination)" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0,
      mortgageStock = 100000.0)
    // origination=2000, principal=5000, default=500 → Δ = 2000-5000-500 = -3500
    val curr = prev.copy(mortgageStock = 96500.0)
    val flows = zeroFlows.copy(mortgageOrigination = 2000, mortgagePrincipalRepaid = 5000,
      mortgageDefaultAmount = 500)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.mortgageStockError shouldBe 0.0 +- 0.01
    result.passed shouldBe true
  }

  // ---- Identity 2 with remittance outflow ----

  "SfcCheck.validate (deposits with remittances)" should "pass when remittance deducted from deposits" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0)
    // remittanceOutflow=12000 → deposits decrease by 12000
    val curr = prev.copy(bankDeposits = prev.bankDeposits - 12000)
    val flows = zeroFlows.copy(remittanceOutflow = 12000)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.bankDepositsError shouldBe 0.0 +- 0.01
    result.passed shouldBe true
  }

  it should "pass with combined income, consumption, and remittance" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0)
    // totalIncome=80000, totalCons=60000, remittance=5000
    // expected deposit Δ = 80000 - 60000 - 5000 = 15000
    val curr = prev.copy(bankDeposits = prev.bankDeposits + 15000)
    val flows = zeroFlows.copy(totalIncome = 80000, totalConsumption = 60000,
      remittanceOutflow = 5000)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.bankDepositsError shouldBe 0.0 +- 0.01
    result.passed shouldBe true
  }

  it should "detect error when remittance not deducted" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0)
    // Bug: deposits unchanged despite remittance outflow
    val curr = prev.copy(bankDeposits = prev.bankDeposits)
    val flows = zeroFlows.copy(remittanceOutflow = 8000)
    val result = SfcCheck.validate(1, prev, curr, flows)
    // actual=0, expected=-8000, error=0-(-8000)=8000
    result.bankDepositsError shouldBe 8000.0 +- 0.01
    result.passed shouldBe false
  }

  it should "pass trivially when remittance is zero (immigration disabled)" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0)
    val result = SfcCheck.validate(1, prev, prev, zeroFlows)
    result.bankDepositsError shouldBe 0.0
    result.passed shouldBe true
  }
