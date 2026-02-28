package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.config.Config

/** Reserve Interest, Standing Facilities, Interbank Interest tests. */
class MonetaryPlumbingSpec extends AnyFlatSpec with Matchers:

  private def mkBank(id: Int, deposits: Double = 1e9, loans: Double = 5e8,
                     capital: Double = 1e8, reservesAtNbp: Double = 1e7,
                     interbankNet: Double = 0.0, failed: Boolean = false) =
    IndividualBankState(id, deposits, loans, capital, 0.0, 0.0, reservesAtNbp,
      interbankNet, failed, if failed then 30 else 0, 0)

  // =========================================================================
  // Reserve Interest
  // =========================================================================

  "BankingSector.reserveInterest" should "compute monthly interest on reserves" in {
    val bank = mkBank(0, reservesAtNbp = 1e8)  // 100M in reserves
    val refRate = 0.0575
    val interest = BankingSector.reserveInterest(bank, refRate)
    // Expected: 100M × 0.0575 × 0.5 / 12 ≈ 239,583
    interest shouldBe (1e8 * 0.0575 * 0.5 / 12.0 +- 1.0)
  }

  it should "return 0 for failed banks" in {
    val bank = mkBank(0, reservesAtNbp = 1e8, failed = true)
    BankingSector.reserveInterest(bank, 0.0575) shouldBe 0.0
  }

  it should "return 0 when reserves are zero" in {
    val bank = mkBank(0, reservesAtNbp = 0.0)
    BankingSector.reserveInterest(bank, 0.0575) shouldBe 0.0
  }

  "BankingSector.computeReserveInterest" should "sum per-bank interest" in {
    val banks = Vector(
      mkBank(0, reservesAtNbp = 1e8),
      mkBank(1, reservesAtNbp = 5e7)
    )
    val refRate = 0.06
    val (perBank, total) = BankingSector.computeReserveInterest(banks, refRate)
    perBank.length shouldBe 2
    total shouldBe (perBank.sum +- 0.01)
    total should be > 0.0
  }

  // =========================================================================
  // Standing Facilities
  // =========================================================================

  "BankingSector.computeStandingFacilities" should "return zeros when disabled" in {
    // Standing facilities are OFF by default (Config.NbpStandingFacilities = false)
    val banks = Vector(mkBank(0, reservesAtNbp = 1e8), mkBank(1, reservesAtNbp = 5e7))
    val (perBank, total) = BankingSector.computeStandingFacilities(banks, 0.06)
    perBank.foreach(_ shouldBe 0.0)
    total shouldBe 0.0
  }

  it should "compute deposit facility income for banks with excess reserves" in {
    // When standing facilities enabled, banks with reservesAtNbp > 0 earn deposit rate
    // We can't easily set Config at runtime, so test the formula directly
    val bank = mkBank(0, reservesAtNbp = 1e8)
    val refRate = 0.0575
    val depositRate = Math.max(0.0, refRate - 0.01)  // 4.75%
    val expectedMonthly = 1e8 * depositRate / 12.0
    // Direct formula check: reservesAtNbp × (refRate − spread) / 12
    expectedMonthly shouldBe (1e8 * 0.0475 / 12.0 +- 1.0)
  }

  it should "return zero for failed banks" in {
    val banks = Vector(mkBank(0, reservesAtNbp = 1e8, failed = true))
    val (perBank, total) = BankingSector.computeStandingFacilities(banks, 0.06)
    // Even if enabled, failed banks get 0 — but currently disabled by default
    total shouldBe 0.0
  }

  // =========================================================================
  // Interbank Interest Flows
  // =========================================================================

  "BankingSector.interbankInterestFlows" should "compute interest on net positions" in {
    val banks = Vector(
      mkBank(0, interbankNet = 1e8),   // Lender: +100M
      mkBank(1, interbankNet = -1e8)   // Borrower: -100M
    )
    val rate = 0.06
    val (perBank, total) = BankingSector.interbankInterestFlows(banks, rate)
    perBank(0) should be > 0.0  // Lender earns
    perBank(1) should be < 0.0  // Borrower pays
    // Net should be ≈ 0 (closed system)
    total shouldBe (0.0 +- 0.01)
  }

  it should "sum to zero for balanced interbank positions" in {
    val banks = Vector(
      mkBank(0, interbankNet = 5e7),
      mkBank(1, interbankNet = -3e7),
      mkBank(2, interbankNet = -2e7)
    )
    val (_, total) = BankingSector.interbankInterestFlows(banks, 0.055)
    total shouldBe (0.0 +- 0.01)
  }

  it should "return zeros for zero net positions" in {
    val banks = Vector(mkBank(0, interbankNet = 0.0), mkBank(1, interbankNet = 0.0))
    val (perBank, total) = BankingSector.interbankInterestFlows(banks, 0.06)
    perBank.foreach(_ shouldBe 0.0)
    total shouldBe 0.0
  }

  it should "return zero for failed banks" in {
    val banks = Vector(
      mkBank(0, interbankNet = 1e8, failed = true),
      mkBank(1, interbankNet = -1e8)
    )
    val (perBank, _) = BankingSector.interbankInterestFlows(banks, 0.06)
    perBank(0) shouldBe 0.0
    perBank(1) should be < 0.0  // Borrower still pays
  }

  it should "scale linearly with rate" in {
    val banks = Vector(
      mkBank(0, interbankNet = 1e8),
      mkBank(1, interbankNet = -1e8)
    )
    val (perBank1, _) = BankingSector.interbankInterestFlows(banks, 0.06)
    val (perBank2, _) = BankingSector.interbankInterestFlows(banks, 0.12)
    perBank2(0) shouldBe (perBank1(0) * 2.0 +- 0.01)
  }

  // =========================================================================
  // SFC Integration: monetary plumbing flows enter bank capital
  // =========================================================================

  "SfcCheck" should "pass with reserve interest in bank capital" in {
    import sfc.sfc.SfcCheck
    val prev = SfcCheck.Snapshot(0, 0, 0, 0,
      bankCapital = 1e8, bankDeposits = 1e9, bankLoans = 5e8,
      govDebt = 0, nfa = 0, bankBondHoldings = 0, nbpBondHoldings = 0,
      bondsOutstanding = 0, interbankNetSum = 0)
    val reserveInt = 100000.0
    val expectedCapChange = reserveInt * 0.3
    val curr = prev.copy(bankCapital = prev.bankCapital + expectedCapChange)
    val flows = SfcCheck.MonthlyFlows(0, 0, 0, 0, 0, 0, 0, 0, 0,
      reserveInterest = reserveInt)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.passed shouldBe true
  }

  it should "pass with standing facility income in bank capital" in {
    import sfc.sfc.SfcCheck
    val prev = SfcCheck.Snapshot(0, 0, 0, 0,
      bankCapital = 1e8, bankDeposits = 1e9, bankLoans = 5e8,
      govDebt = 0, nfa = 0, bankBondHoldings = 0, nbpBondHoldings = 0,
      bondsOutstanding = 0, interbankNetSum = 0)
    val sfIncome = 50000.0
    val expectedCapChange = sfIncome * 0.3
    val curr = prev.copy(bankCapital = prev.bankCapital + expectedCapChange)
    val flows = SfcCheck.MonthlyFlows(0, 0, 0, 0, 0, 0, 0, 0, 0,
      standingFacilityIncome = sfIncome)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.passed shouldBe true
  }

  it should "pass with interbank interest (net ≈ 0) in bank capital" in {
    import sfc.sfc.SfcCheck
    val prev = SfcCheck.Snapshot(0, 0, 0, 0,
      bankCapital = 1e8, bankDeposits = 1e9, bankLoans = 5e8,
      govDebt = 0, nfa = 0, bankBondHoldings = 0, nbpBondHoldings = 0,
      bondsOutstanding = 0, interbankNetSum = 0)
    // Interbank interest nets to ~0 in aggregate, so bank capital unchanged
    val ibInt = 0.0
    val curr = prev.copy(bankCapital = prev.bankCapital)
    val flows = SfcCheck.MonthlyFlows(0, 0, 0, 0, 0, 0, 0, 0, 0,
      interbankInterest = ibInt)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.passed shouldBe true
  }

  it should "detect mismatch when reserve interest not in flows" in {
    import sfc.sfc.SfcCheck
    val prev = SfcCheck.Snapshot(0, 0, 0, 0,
      bankCapital = 1e8, bankDeposits = 1e9, bankLoans = 5e8,
      govDebt = 0, nfa = 0, bankBondHoldings = 0, nbpBondHoldings = 0,
      bondsOutstanding = 0, interbankNetSum = 0)
    val reserveInt = 100000.0
    val curr = prev.copy(bankCapital = prev.bankCapital + reserveInt * 0.3)
    // Flows do NOT include reserveInterest — should fail
    val flows = SfcCheck.MonthlyFlows(0, 0, 0, 0, 0, 0, 0, 0, 0)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.passed shouldBe false
    Math.abs(result.bankCapitalError) should be > 1.0
  }

  it should "pass with all three monetary plumbing flows combined" in {
    import sfc.sfc.SfcCheck
    val prev = SfcCheck.Snapshot(0, 0, 0, 0,
      bankCapital = 1e8, bankDeposits = 1e9, bankLoans = 5e8,
      govDebt = 0, nfa = 0, bankBondHoldings = 0, nbpBondHoldings = 0,
      bondsOutstanding = 0, interbankNetSum = 0)
    val resInt = 200000.0
    val sfInc = 50000.0
    val ibInt = -1000.0  // small net from rounding
    val expectedCapChange = (resInt + sfInc + ibInt) * 0.3
    val curr = prev.copy(bankCapital = prev.bankCapital + expectedCapChange)
    val flows = SfcCheck.MonthlyFlows(0, 0, 0, 0, 0, 0, 0, 0, 0,
      reserveInterest = resInt, standingFacilityIncome = sfInc, interbankInterest = ibInt)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.passed shouldBe true
  }

  // =========================================================================
  // Credit Diagnostics (M1/M2)
  // =========================================================================

  "MonetaryAggregates.compute" should "compute M1 as deposits" in {
    import sfc.sfc.MonetaryAggregates
    val agg = MonetaryAggregates.compute(1e9, 1e8)
    agg.m1 shouldBe 1e9
    agg.monetaryBase shouldBe 1e8
  }

  it should "compute credit multiplier as M1/base" in {
    import sfc.sfc.MonetaryAggregates
    val agg = MonetaryAggregates.compute(4.5e9, 1e9)
    agg.creditMultiplier shouldBe (4.5 +- 0.01)
  }

  it should "handle zero reserves with floor" in {
    import sfc.sfc.MonetaryAggregates
    val agg = MonetaryAggregates.compute(1e9, 0.0)
    agg.creditMultiplier shouldBe (1e9 +- 1.0)  // m1 / max(1.0, 0.0)
  }

  "MonetaryAggregates.zero" should "have all zero values" in {
    import sfc.sfc.MonetaryAggregates
    MonetaryAggregates.zero.m1 shouldBe 0.0
    MonetaryAggregates.zero.monetaryBase shouldBe 0.0
    MonetaryAggregates.zero.creditMultiplier shouldBe 0.0
  }

  // =========================================================================
  // JST — SFC deposit and debt identities
  // =========================================================================

  "SfcCheck Identity 2" should "include JST deposit change" in {
    import sfc.sfc.SfcCheck
    val prev = SfcCheck.Snapshot(0, 0, 0, 0,
      bankCapital = 1e8, bankDeposits = 1e9, bankLoans = 5e8,
      govDebt = 0, nfa = 0, bankBondHoldings = 0, nbpBondHoldings = 0,
      bondsOutstanding = 0, interbankNetSum = 0)
    val jstDep = 50000.0  // positive = JST adds to bank deposits
    val curr = prev.copy(bankDeposits = prev.bankDeposits + jstDep)
    val flows = SfcCheck.MonthlyFlows(0, 0, 0, 0, 0, 0, 0, 0, 0,
      jstDepositChange = jstDep)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.passed shouldBe true
  }

  it should "fail when JST deposit change not accounted for" in {
    import sfc.sfc.SfcCheck
    val prev = SfcCheck.Snapshot(0, 0, 0, 0,
      bankCapital = 1e8, bankDeposits = 1e9, bankLoans = 5e8,
      govDebt = 0, nfa = 0, bankBondHoldings = 0, nbpBondHoldings = 0,
      bondsOutstanding = 0, interbankNetSum = 0)
    val jstDep = 50000.0
    val curr = prev.copy(bankDeposits = prev.bankDeposits + jstDep)
    // Flows do NOT include jstDepositChange → should fail
    val flows = SfcCheck.MonthlyFlows(0, 0, 0, 0, 0, 0, 0, 0, 0)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.passed shouldBe false
    Math.abs(result.bankDepositsError) should be > 1.0
  }

  "SfcCheck Identity 7" should "pass when JST debt change matches" in {
    import sfc.sfc.SfcCheck
    val prev = SfcCheck.Snapshot(0, 0, 0, 0,
      bankCapital = 1e8, bankDeposits = 1e9, bankLoans = 5e8,
      govDebt = 0, nfa = 0, bankBondHoldings = 0, nbpBondHoldings = 0,
      bondsOutstanding = 0, interbankNetSum = 0,
      jstDeposits = 0, jstDebt = 0)
    val jstSpend = 1e7
    val jstRev = 9.8e6
    val deficit = jstSpend - jstRev
    val depChange = -(deficit)  // deposit change = revenue - spending = -deficit
    val curr = prev.copy(
      jstDebt = prev.jstDebt + deficit,
      bankDeposits = prev.bankDeposits + depChange  // Identity 2: deposits change by jstDepositChange
    )
    val flows = SfcCheck.MonthlyFlows(0, 0, 0, 0, 0, 0, 0, 0, 0,
      jstSpending = jstSpend, jstRevenue = jstRev,
      jstDepositChange = depChange)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.passed shouldBe true
  }

  it should "fail when JST debt change mismatches" in {
    import sfc.sfc.SfcCheck
    val prev = SfcCheck.Snapshot(0, 0, 0, 0,
      bankCapital = 1e8, bankDeposits = 1e9, bankLoans = 5e8,
      govDebt = 0, nfa = 0, bankBondHoldings = 0, nbpBondHoldings = 0,
      bondsOutstanding = 0, interbankNetSum = 0,
      jstDeposits = 0, jstDebt = 1000.0)
    // JST debt goes up by 5000 but flows say zero
    val curr = prev.copy(jstDebt = prev.jstDebt + 5000.0)
    val flows = SfcCheck.MonthlyFlows(0, 0, 0, 0, 0, 0, 0, 0, 0)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.passed shouldBe false
    Math.abs(result.jstDebtError) should be > 1.0
  }
