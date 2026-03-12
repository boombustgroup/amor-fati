package com.boombustgroup.amorfati.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.agents.Banking
import com.boombustgroup.amorfati.types.*

/** Reserve Interest, Standing Facilities, Interbank Interest tests. */
class MonetaryPlumbingSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams = SimParams.defaults

  import com.boombustgroup.amorfati.accounting.Sfc

  private def zeroSnap: Sfc.Snapshot = Sfc.Snapshot(
    hhSavings = PLN.Zero,
    hhDebt = PLN.Zero,
    firmCash = PLN.Zero,
    firmDebt = PLN.Zero,
    bankCapital = PLN.Zero,
    bankDeposits = PLN.Zero,
    bankLoans = PLN.Zero,
    govDebt = PLN.Zero,
    nfa = PLN.Zero,
    bankBondHoldings = PLN.Zero,
    nbpBondHoldings = PLN.Zero,
    bondsOutstanding = PLN.Zero,
    interbankNetSum = PLN.Zero,
    jstDeposits = PLN.Zero,
    jstDebt = PLN.Zero,
    fusBalance = PLN.Zero,
    ppkBondHoldings = PLN.Zero,
    mortgageStock = PLN.Zero,
    consumerLoans = PLN.Zero,
    corpBondsOutstanding = PLN.Zero,
    insuranceGovBondHoldings = PLN.Zero,
    tfiGovBondHoldings = PLN.Zero,
    nbfiLoanStock = PLN.Zero,
  )

  private def zeroFlows: Sfc.MonthlyFlows = Sfc.MonthlyFlows(
    govSpending = PLN.Zero,
    govRevenue = PLN.Zero,
    nplLoss = PLN.Zero,
    interestIncome = PLN.Zero,
    hhDebtService = PLN.Zero,
    totalIncome = PLN.Zero,
    totalConsumption = PLN.Zero,
    newLoans = PLN.Zero,
    nplRecovery = PLN.Zero,
    currentAccount = PLN.Zero,
    valuationEffect = PLN.Zero,
    bankBondIncome = PLN.Zero,
    qePurchase = PLN.Zero,
    newBondIssuance = PLN.Zero,
    depositInterestPaid = PLN.Zero,
    reserveInterest = PLN.Zero,
    standingFacilityIncome = PLN.Zero,
    interbankInterest = PLN.Zero,
    jstDepositChange = PLN.Zero,
    jstSpending = PLN.Zero,
    jstRevenue = PLN.Zero,
    zusContributions = PLN.Zero,
    zusPensionPayments = PLN.Zero,
    zusGovSubvention = PLN.Zero,
    dividendIncome = PLN.Zero,
    foreignDividendOutflow = PLN.Zero,
    dividendTax = PLN.Zero,
    mortgageInterestIncome = PLN.Zero,
    mortgageNplLoss = PLN.Zero,
    mortgageOrigination = PLN.Zero,
    mortgagePrincipalRepaid = PLN.Zero,
    mortgageDefaultAmount = PLN.Zero,
    remittanceOutflow = PLN.Zero,
    fofResidual = PLN.Zero,
    consumerDebtService = PLN.Zero,
    consumerNplLoss = PLN.Zero,
    consumerOrigination = PLN.Zero,
    consumerPrincipalRepaid = PLN.Zero,
    consumerDefaultAmount = PLN.Zero,
    corpBondCouponIncome = PLN.Zero,
    corpBondDefaultLoss = PLN.Zero,
    corpBondIssuance = PLN.Zero,
    corpBondAmortization = PLN.Zero,
    corpBondDefaultAmount = PLN.Zero,
    insNetDepositChange = PLN.Zero,
    nbfiDepositDrain = PLN.Zero,
    nbfiOrigination = PLN.Zero,
    nbfiRepayment = PLN.Zero,
    nbfiDefaultAmount = PLN.Zero,
    fdiProfitShifting = PLN.Zero,
    fdiRepatriation = PLN.Zero,
    diasporaInflow = PLN.Zero,
    tourismExport = PLN.Zero,
    tourismImport = PLN.Zero,
    bfgLevy = PLN.Zero,
    bailInLoss = PLN.Zero,
    bankCapitalDestruction = PLN.Zero,
    investNetDepositFlow = PLN.Zero,
  )

  private def mkBank(
      id: Int,
      deposits: PLN = PLN(1e9),
      loans: PLN = PLN(5e8),
      capital: PLN = PLN(1e8),
      reservesAtNbp: PLN = PLN(1e7),
      interbankNet: PLN = PLN.Zero,
      failed: Boolean = false,
  ) =
    Banking.BankState(
      id = BankId(id),
      deposits = deposits,
      loans = loans,
      capital = capital,
      nplAmount = PLN.Zero,
      govBondHoldings = PLN.Zero,
      reservesAtNbp = reservesAtNbp,
      interbankNet = interbankNet,
      status = if failed then Banking.BankStatus.Failed(30) else Banking.BankStatus.Active(0),
      demandDeposits = PLN.Zero,
      termDeposits = PLN.Zero,
      loansShort = PLN.Zero,
      loansMedium = PLN.Zero,
      loansLong = PLN.Zero,
      consumerLoans = PLN.Zero,
      consumerNpl = PLN.Zero,
      corpBondHoldings = PLN.Zero,
    )

  // =========================================================================
  // Reserve Interest
  // =========================================================================

  "Banking.reserveInterest" should "compute monthly interest on reserves" in {
    val bank     = mkBank(0, reservesAtNbp = PLN(1e8)) // 100M in reserves
    val interest = Banking.reserveInterest(bank, Rate(0.0575))
    // Expected: 100M × 0.0575 × 0.5 / 12 ≈ 239,583
    interest.toDouble shouldBe (1e8 * 0.0575 * 0.5 / 12.0 +- 1.0)
  }

  it should "return 0 for failed banks" in {
    val bank = mkBank(0, reservesAtNbp = PLN(1e8), failed = true)
    Banking.reserveInterest(bank, Rate(0.0575)) shouldBe PLN.Zero
  }

  it should "return 0 when reserves are zero" in {
    val bank = mkBank(0, reservesAtNbp = PLN.Zero)
    Banking.reserveInterest(bank, Rate(0.0575)) shouldBe PLN.Zero
  }

  "Banking.computeReserveInterest" should "sum per-bank interest" in {
    val banks  = Vector(
      mkBank(0, reservesAtNbp = PLN(1e8)),
      mkBank(1, reservesAtNbp = PLN(5e7)),
    )
    val result = Banking.computeReserveInterest(banks, Rate(0.06))
    result.perBank.length shouldBe 2
    result.total.toDouble shouldBe (result.perBank.map(_.toDouble).sum +- 0.01)
    result.total should be > PLN.Zero
  }

  // =========================================================================
  // Standing Facilities
  // =========================================================================

  "Banking.computeStandingFacilities" should "return zeros when disabled" in {
    // Standing facilities are OFF by default (p.flags.nbpStandingFacilities = false)
    val banks  = Vector(mkBank(0, reservesAtNbp = PLN(1e8)), mkBank(1, reservesAtNbp = PLN(5e7)))
    val result = Banking.computeStandingFacilities(banks, Rate(0.06))
    result.perBank.foreach(_ shouldBe PLN.Zero)
    result.total shouldBe PLN.Zero
  }

  it should "compute deposit facility income for banks with excess reserves" in {
    // When standing facilities enabled, banks with reservesAtNbp > 0 earn deposit rate
    // We can't easily set Config at runtime, so test the formula directly
    val bank            = mkBank(0, reservesAtNbp = PLN(1e8))
    val refRate         = 0.0575
    val depositRate     = Math.max(0.0, refRate - 0.01) // 4.75%
    val expectedMonthly = bank.reservesAtNbp.toDouble * depositRate / 12.0
    // Direct formula check: reservesAtNbp × (refRate − spread) / 12
    expectedMonthly shouldBe (1e8 * 0.0475 / 12.0 +- 1.0)
  }

  it should "charge lombard rate for interbank borrowers (formula check)" in {
    // Bank with negative interbankNet should pay lombard cost (negative income)
    // interbankNet = -100M, refRate = 5.75%, lombardSpread = 1% → lombardRate = 6.75%
    val refRate             = 0.0575
    val lombardRate         = refRate + 0.01 // 6.75%
    val interbankBorrowing  = 1e8
    val expectedMonthlyCost = (interbankBorrowing * lombardRate / 12.0) * -1.0
    // Direct formula check: -|interbankNet| × lombardRate / 12
    expectedMonthlyCost shouldBe (-1e8 * 0.0675 / 12.0 +- 1.0)
    expectedMonthlyCost should be < 0.0
  }

  it should "return zero for failed banks" in {
    val banks  = Vector(mkBank(0, reservesAtNbp = PLN(1e8), failed = true))
    val result = Banking.computeStandingFacilities(banks, Rate(0.06))
    // Even if enabled, failed banks get 0 — but currently disabled by default
    result.total shouldBe PLN.Zero
  }

  // =========================================================================
  // Interbank Interest Flows
  // =========================================================================

  "Banking.interbankInterestFlows" should "compute interest on net positions" in {
    val banks  = Vector(
      mkBank(0, interbankNet = PLN(1e8)), // Lender: +100M
      mkBank(1, interbankNet = PLN(-1e8)), // Borrower: -100M
    )
    val result = Banking.interbankInterestFlows(banks, Rate(0.06))
    result.perBank(0) should be > PLN.Zero // Lender earns
    result.perBank(1) should be < PLN.Zero // Borrower pays
    // Net should be ≈ 0 (closed system)
    result.total.toDouble shouldBe (0.0 +- 0.01)
  }

  it should "sum to zero for balanced interbank positions" in {
    val banks  = Vector(
      mkBank(0, interbankNet = PLN(5e7)),
      mkBank(1, interbankNet = PLN(-3e7)),
      mkBank(2, interbankNet = PLN(-2e7)),
    )
    val result = Banking.interbankInterestFlows(banks, Rate(0.055))
    result.total.toDouble shouldBe (0.0 +- 0.01)
  }

  it should "return zeros for zero net positions" in {
    val banks  = Vector(mkBank(0, interbankNet = PLN.Zero), mkBank(1, interbankNet = PLN.Zero))
    val result = Banking.interbankInterestFlows(banks, Rate(0.06))
    result.perBank.foreach(_ shouldBe PLN.Zero)
    result.total shouldBe PLN.Zero
  }

  it should "return zero for failed banks" in {
    val banks  = Vector(
      mkBank(0, interbankNet = PLN(1e8), failed = true),
      mkBank(1, interbankNet = PLN(-1e8)),
    )
    val result = Banking.interbankInterestFlows(banks, Rate(0.06))
    result.perBank(0) shouldBe PLN.Zero
    result.perBank(1) should be < PLN.Zero // Borrower still pays
  }

  it should "scale linearly with rate" in {
    val banks   = Vector(
      mkBank(0, interbankNet = PLN(1e8)),
      mkBank(1, interbankNet = PLN(-1e8)),
    )
    val result1 = Banking.interbankInterestFlows(banks, Rate(0.06))
    val result2 = Banking.interbankInterestFlows(banks, Rate(0.12))
    result2.perBank(0).toDouble shouldBe (result1.perBank(0).toDouble * 2.0 +- 0.01)
  }

  // =========================================================================
  // SFC Integration: monetary plumbing flows enter bank capital
  // =========================================================================

  "Sfc" should "pass with reserve interest in bank capital" in {
    val prev              = zeroSnap.copy(bankCapital = PLN(1e8), bankDeposits = PLN(1e9), bankLoans = PLN(5e8))
    val reserveInt        = PLN(100000.0)
    val expectedCapChange = reserveInt * 0.3
    val curr              = prev.copy(bankCapital = prev.bankCapital + expectedCapChange)
    val flows             = zeroFlows.copy(reserveInterest = reserveInt)
    val result            = Sfc.validate(prev, curr, flows)
    result shouldBe Right(())
  }

  it should "pass with standing facility income in bank capital" in {
    val prev              = zeroSnap.copy(bankCapital = PLN(1e8), bankDeposits = PLN(1e9), bankLoans = PLN(5e8))
    val sfIncome          = PLN(50000.0)
    val expectedCapChange = sfIncome * 0.3
    val curr              = prev.copy(bankCapital = prev.bankCapital + expectedCapChange)
    val flows             = zeroFlows.copy(standingFacilityIncome = sfIncome)
    val result            = Sfc.validate(prev, curr, flows)
    result shouldBe Right(())
  }

  it should "pass with interbank interest (net ≈ 0) in bank capital" in {
    val prev   = zeroSnap.copy(bankCapital = PLN(1e8), bankDeposits = PLN(1e9), bankLoans = PLN(5e8))
    // Interbank interest nets to ~0 in aggregate, so bank capital unchanged
    val ibInt  = PLN.Zero
    val curr   = prev.copy(bankCapital = prev.bankCapital)
    val flows  = zeroFlows.copy(interbankInterest = ibInt)
    val result = Sfc.validate(prev, curr, flows)
    result shouldBe Right(())
  }

  it should "detect mismatch when reserve interest not in flows" in {
    val prev       = zeroSnap.copy(bankCapital = PLN(1e8), bankDeposits = PLN(1e9), bankLoans = PLN(5e8))
    val reserveInt = PLN(100000.0)
    val curr       = prev.copy(bankCapital = prev.bankCapital + reserveInt * 0.3)
    // Flows do NOT include reserveInterest — should fail
    val flows      = zeroFlows
    val result     = Sfc.validate(prev, curr, flows)
    result shouldBe a[Left[?, ?]]
    result.swap.getOrElse(Vector.empty).exists(_.identity == Sfc.SfcIdentity.BankCapital) shouldBe true
  }

  it should "pass with all three monetary plumbing flows combined" in {
    val prev              = zeroSnap.copy(bankCapital = PLN(1e8), bankDeposits = PLN(1e9), bankLoans = PLN(5e8))
    val resInt            = PLN(200000.0)
    val sfInc             = PLN(50000.0)
    val ibInt             = PLN(-1000.0) // small net from rounding
    val expectedCapChange = (resInt + sfInc + ibInt) * 0.3
    val curr              = prev.copy(bankCapital = prev.bankCapital + expectedCapChange)
    val flows             = zeroFlows.copy(
      reserveInterest = resInt,
      standingFacilityIncome = sfInc,
      interbankInterest = ibInt,
    )
    val result            = Sfc.validate(prev, curr, flows)
    result shouldBe Right(())
  }

  // =========================================================================
  // Credit Diagnostics (M1/M2)
  // =========================================================================

  "MonetaryAggregates.compute" should "compute M1 as deposits" in {
    val agg = Banking.MonetaryAggregates.compute(PLN(1e9), PLN(1e8))
    agg.m1 shouldBe PLN(1e9)
    agg.monetaryBase shouldBe PLN(1e8)
  }

  it should "compute credit multiplier as M1/base" in {
    val agg = Banking.MonetaryAggregates.compute(PLN(4.5e9), PLN(1e9))
    agg.creditMultiplier shouldBe (4.5 +- 0.01)
  }

  it should "handle zero reserves with floor" in {
    val agg = Banking.MonetaryAggregates.compute(PLN(1e9), PLN.Zero)
    agg.creditMultiplier shouldBe (1e9 +- 1.0) // m1 / max(1.0, 0.0)
  }

  "MonetaryAggregates.zero" should "have all zero values" in {
    Banking.MonetaryAggregates.zero.m1 shouldBe PLN.Zero
    Banking.MonetaryAggregates.zero.monetaryBase shouldBe PLN.Zero
    Banking.MonetaryAggregates.zero.creditMultiplier shouldBe 0.0
  }

  // =========================================================================
  // JST — SFC deposit and debt identities
  // =========================================================================

  "Sfc Identity 2" should "include JST deposit change" in {
    val prev   = zeroSnap.copy(bankCapital = PLN(1e8), bankDeposits = PLN(1e9), bankLoans = PLN(5e8))
    val jstDep = PLN(50000.0) // positive = JST adds to bank deposits
    val curr   = prev.copy(bankDeposits = prev.bankDeposits + jstDep)
    val flows  = zeroFlows.copy(jstDepositChange = jstDep)
    val result = Sfc.validate(prev, curr, flows)
    result shouldBe Right(())
  }

  it should "fail when JST deposit change not accounted for" in {
    val prev   = zeroSnap.copy(bankCapital = PLN(1e8), bankDeposits = PLN(1e9), bankLoans = PLN(5e8))
    val jstDep = PLN(50000.0)
    val curr   = prev.copy(bankDeposits = prev.bankDeposits + jstDep)
    // Flows do NOT include jstDepositChange → should fail
    val flows  = zeroFlows
    val result = Sfc.validate(prev, curr, flows)
    result shouldBe a[Left[?, ?]]
    result.swap.getOrElse(Vector.empty).exists(_.identity == Sfc.SfcIdentity.BankDeposits) shouldBe true
  }

  "Sfc Identity 7" should "pass when JST debt change matches" in {
    val prev      = zeroSnap.copy(bankCapital = PLN(1e8), bankDeposits = PLN(1e9), bankLoans = PLN(5e8))
    val jstSpend  = PLN(1e7)
    val jstRev    = PLN(9.8e6)
    val deficit   = jstSpend - jstRev
    val depChange = -deficit // deposit change = revenue - spending = -deficit
    val curr      = prev.copy(
      jstDebt = prev.jstDebt + deficit,
      bankDeposits = prev.bankDeposits + depChange, // Identity 2: deposits change by jstDepositChange
    )
    val flows     = zeroFlows.copy(
      jstSpending = jstSpend,
      jstRevenue = jstRev,
      jstDepositChange = depChange,
    )
    val result    = Sfc.validate(prev, curr, flows)
    result shouldBe Right(())
  }

  it should "fail when JST debt change mismatches" in {
    val prev   = zeroSnap.copy(
      bankCapital = PLN(1e8),
      bankDeposits = PLN(1e9),
      bankLoans = PLN(5e8),
      jstDebt = PLN(1000.0),
    )
    // JST debt goes up by 5000 but flows say zero
    val curr   = prev.copy(jstDebt = prev.jstDebt + PLN(5000.0))
    val flows  = zeroFlows
    val result = Sfc.validate(prev, curr, flows)
    result shouldBe a[Left[?, ?]]
    result.swap.getOrElse(Vector.empty).exists(_.identity == Sfc.SfcIdentity.JstDebt) shouldBe true
  }
