package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.Generators
import com.boombustgroup.amorfati.TestHouseholdState
import com.boombustgroup.amorfati.FixedPointSpecSupport.*

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import com.boombustgroup.amorfati.random.RandomStream

class DepositMobilitySpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  private def mkBankRow(id: Int, car: BigDecimal, failed: Boolean = false): (Banking.BankState, Banking.BankFinancialStocks) =
    // Set capital and loans to achieve desired CAR
    val loans = BigDecimal("10e9")
    val cap   = car * loans // CAR = capital / RWA ≈ capital / loans
    (
      Banking.BankState(
        id = BankId(id),
        capital = PLN(cap),
        nplAmount = PLN.Zero,
        htmBookYield = Rate("0.05"),
        status = if failed then Banking.BankStatus.Failed(ExecutionMonth.First) else Banking.BankStatus.Active(0),
        loansShort = PLN("3e9"),
        loansMedium = PLN("4e9"),
        loansLong = PLN("3e9"),
        consumerNpl = PLN.Zero,
      ),
      Banking.BankFinancialStocks(
        totalDeposits = PLN("10e9"),
        firmLoan = PLN(loans),
        govBondAfs = PLN("1e9"),
        govBondHtm = PLN("1e9"),
        reserve = PLN.Zero,
        interbankLoan = PLN.Zero,
        demandDeposit = PLN("6e9"),
        termDeposit = PLN("4e9"),
        consumerLoan = PLN.Zero,
      ),
    )

  private def mkTinyRwaBankRow(id: Int): (Banking.BankState, Banking.BankFinancialStocks) =
    val row = mkBankRow(id, BigDecimal("0.01"))
    (
      row._1.copy(
        capital = PLN("1e6"),
        loansShort = PLN.Zero,
        loansMedium = PLN.Zero,
        loansLong = PLN.Zero,
      ),
      row._2.copy(
        totalDeposits = PLN("1e6"),
        firmLoan = PLN.Zero,
        demandDeposit = PLN("6e5"),
        termDeposit = PLN("4e5"),
        consumerLoan = PLN.Zero,
      ),
    )

  private def banks(rows: Vector[(Banking.BankState, Banking.BankFinancialStocks)]): Vector[Banking.BankState] =
    rows.map(_._1)

  private def stocks(rows: Vector[(Banking.BankState, Banking.BankFinancialStocks)]): Vector[Banking.BankFinancialStocks] =
    rows.map(_._2)

  private def mkHh(bankId: Int, savings: BigDecimal = BigDecimal("50000.0")): Household.State =
    TestHouseholdState(
      id = HhId(0),
      savings = PLN(savings),
      debt = PLN.Zero,
      monthlyRent = PLN.Zero,
      skill = Share("0.5"),
      healthPenalty = Share.Zero,
      mpc = Share("0.7"),
      status = HhStatus.Employed(FirmId(0), SectorIdx(0), PLN("8000.0")),
      socialNeighbors = Array.empty[HhId],
      bankId = BankId(bankId),
      equityWealth = PLN.Zero,
      lastSectorIdx = SectorIdx(0),
      isImmigrant = false,
      numDependentChildren = 0,
      consumerDebt = PLN.Zero,
      education = 2,
      taskRoutineness = Share("0.5"),
      wageScar = Share.Zero,
    )

  private def mkWorld() =
    Generators.testWorld(totalPopulation = 1, employed = 1, marketWage = PLN("8000.0"), reservationWage = PLN("4666.0"))

  "DepositMobility" should "not move deposits when all banks are healthy" in {
    val rows   = Vector(mkBankRow(0, BigDecimal("0.15")), mkBankRow(1, BigDecimal("0.14")))
    val hhs    = Vector(mkHh(0), mkHh(1))
    val result = DepositMobility(hhs, banks(rows), stocks(rows), anyBankFailed = false, RandomStream.seeded(42))
    result.households.map(_.bankId) shouldBe hhs.map(_.bankId)
  }

  it should "trigger flight from weak bank (low CAR)" in {
    val rows     = Vector(mkBankRow(0, BigDecimal("0.04")), mkBankRow(1, BigDecimal("0.15"))) // bank 0 below threshold
    val hhs      = (0 until 100).map(_ => mkHh(0)).toVector
    val result   = DepositMobility(hhs, banks(rows), stocks(rows), anyBankFailed = false, RandomStream.seeded(42))
    val switched = result.households.count(_.bankId == BankId(1))
    switched should be > 0
  }

  it should "trigger panic switching when a bank fails" in {
    // HH at bank 2, bank 1 fails, healthiest is bank 0 → some HH panic-switch to bank 0
    val rows     = Vector(mkBankRow(0, BigDecimal("0.15")), mkBankRow(1, BigDecimal("0.15"), failed = true), mkBankRow(2, BigDecimal("0.12")))
    val hhs      = (0 until 100).map(_ => mkHh(2)).toVector
    val result   = DepositMobility(hhs, banks(rows), stocks(rows), anyBankFailed = true, RandomStream.seeded(42))
    val switched = result.households.count(_.bankId == BankId(0))
    // With panicRate = 3%, expect ~3 of 100 to switch
    switched should be > 0
  }

  it should "preserve total number of households" in {
    val rows   = Vector(mkBankRow(0, BigDecimal("0.04")), mkBankRow(1, BigDecimal("0.15")))
    val hhs    = (0 until 50).map(i => mkHh(i % 2)).toVector
    val result = DepositMobility(hhs, banks(rows), stocks(rows), anyBankFailed = true, RandomStream.seeded(42))
    result.households.length shouldBe hhs.length
  }

  it should "move deposits toward healthiest bank" in {
    val rows    = Vector(mkBankRow(0, BigDecimal("0.04")), mkBankRow(1, BigDecimal("0.20")), mkBankRow(2, BigDecimal("0.12")))
    val hhs     = (0 until 100).map(_ => mkHh(0)).toVector
    val result  = DepositMobility(hhs, banks(rows), stocks(rows), anyBankFailed = false, RandomStream.seeded(42))
    // Bank 1 is healthiest (CAR 20%) — switchers should go there
    val atBank1 = result.households.count(_.bankId == BankId(1))
    val atBank2 = result.households.count(_.bankId == BankId(2))
    atBank1 should be >= atBank2
  }

  it should "not select a tiny-RWA CAR-floor bank as the flight-to-safety target" in {
    val rows     = Vector(mkBankRow(0, BigDecimal("0.04")), mkTinyRwaBankRow(1), mkBankRow(2, BigDecimal("0.15")))
    val hhs      = (0 until 100).map(id => mkHh(0).copy(id = HhId(id))).toVector
    val result   = DepositMobility(hhs, banks(rows), stocks(rows), anyBankFailed = false, RandomStream.seeded(42))
    val switched = result.households.filter(_.bankId != BankId(0))

    Banking.healthiestBankId(banks(rows), stocks(rows)) shouldBe BankId(2)
    switched.map(_.bankId).distinct shouldBe Vector(BankId(2))
    switched should not be empty
  }

  it should "fail fast for an unknown household bankId" in {
    val rows = Vector(mkBankRow(0, BigDecimal("0.15")), mkBankRow(1, BigDecimal("0.14")))
    val ex   = intercept[IllegalArgumentException]:
      DepositMobility(Vector(mkHh(99)), banks(rows), stocks(rows), anyBankFailed = false, RandomStream.seeded(42))

    ex.getMessage should include("known bankId")
    ex.getMessage should include("99")
  }

  it should "fail fast when a household still points at a failed bank" in {
    val rows = Vector(mkBankRow(0, BigDecimal("0.15")), mkBankRow(1, BigDecimal("0.04"), failed = true))
    val ex   = intercept[IllegalArgumentException]:
      DepositMobility(Vector(mkHh(1)), banks(rows), stocks(rows), anyBankFailed = true, RandomStream.seeded(42))

    ex.getMessage should include("failed bank 1")
    ex.getMessage should include("before mobility runs")
  }

  it should "be deterministic with same seed" in {
    val rows = Vector(mkBankRow(0, BigDecimal("0.04")), mkBankRow(1, BigDecimal("0.15")))
    val hhs  = (0 until 100).map(_ => mkHh(0)).toVector
    val r1   = DepositMobility(hhs, banks(rows), stocks(rows), anyBankFailed = true, RandomStream.seeded(42))
    val r2   = DepositMobility(hhs, banks(rows), stocks(rows), anyBankFailed = true, RandomStream.seeded(42))
    r1.households.map(_.bankId) shouldBe r2.households.map(_.bankId)
  }

  it should "return only delayed boundary bankId updates, not a same-month transfer plan" in {
    val rows     = Vector(mkBankRow(0, BigDecimal("0.04")), mkBankRow(1, BigDecimal("0.20")))
    val original = (0 until 100).map(id => mkHh(0).copy(id = HhId(id))).toVector
    val result   = DepositMobility(original, banks(rows), stocks(rows), anyBankFailed = false, RandomStream.seeded(42))

    result.productArity shouldBe 1
    result.productElementName(0) shouldBe "households"
    result.households.count(_.bankId == BankId(1)) should be > 0
    original
      .zip(result.households)
      .foreach: (before, after) =>
        after shouldBe before.copy(bankId = after.bankId)
  }

  it should "route the next household step through the reassigned bankId" in {
    val rows       = Vector(mkBankRow(0, BigDecimal("0.04")), mkBankRow(1, BigDecimal("0.20")))
    val original   = (0 until 100).map(id => mkHh(0, savings = BigDecimal("120000.0")).copy(id = HhId(id))).toVector
    val reassigned =
      DepositMobility(original, banks(rows), stocks(rows), anyBankFailed = false, RandomStream.seeded(42)).households
        .find(_.bankId == BankId(1))
        .getOrElse(fail("expected at least one household to switch to the healthiest bank"))

    reassigned.bankId shouldBe BankId(1)

    val bankRates = BankRates(
      lendingRates = Vector(Rate("0.07"), Rate("0.07")),
      depositRates = Vector(Rate.Zero, Rate("0.12")),
    )
    val step      = Household.step(
      households = Vector(reassigned),
      financialStocks = Vector(TestHouseholdState.financial(savings = PLN("120000.0"))),
      world = mkWorld(),
      marketWage = PLN("8000.0"),
      reservationWage = PLN("4666.0"),
      importAdj = Share("0.4"),
      rng = RandomStream.seeded(7),
      nBanks = 2,
      bankRates = Some(bankRates),
    )
    val flows     = step.perBankFlows.get

    flows(0).depositInterest shouldBe PLN.Zero
    decimal(flows(1).depositInterest) shouldBe (decimal(PLN("1200.0")) +- decimal(PLN("1.0")))
  }
