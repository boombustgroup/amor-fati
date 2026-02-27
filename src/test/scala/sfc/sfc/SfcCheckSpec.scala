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
      month = 1, inflation = 0.02, priceLevel = 1.0, demandMultiplier = 1.0,
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
    // Bank capital off by 0.5 (below default tolerance of 1.0)
    val curr = prev.copy(bankCapital = prev.bankCapital + 0.5)
    val result = SfcCheck.validate(1, prev, curr, zeroFlows)
    result.passed shouldBe true
  }

  it should "fail when error exceeds tolerance" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0)
    // Bank capital off by 2.0 (above default tolerance of 1.0)
    val curr = prev.copy(bankCapital = prev.bankCapital + 2.0)
    val result = SfcCheck.validate(1, prev, curr, zeroFlows)
    result.passed shouldBe false
  }

  it should "respect custom tolerance parameter" in {
    val prev = SfcCheck.Snapshot(0, 0, 500000, 0, 200000, 1000000, 0, 0)
    val curr = prev.copy(bankCapital = prev.bankCapital + 5.0)
    // Default tolerance (1.0): fails
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
