package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class DepositMobilitySpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  private def mkBank(id: Int, car: Double, failed: Boolean = false): Banking.BankState =
    // Set capital and loans to achieve desired CAR
    val loans = 10e9
    val cap   = car * loans // CAR = capital / RWA ≈ capital / loans
    Banking.BankState(
      id = BankId(id),
      deposits = PLN(10e9),
      loans = PLN(loans),
      capital = PLN(cap),
      nplAmount = PLN.Zero,
      afsBonds = PLN(1e9),
      htmBonds = PLN(1e9),
      htmBookYield = Rate(0.05),
      reservesAtNbp = PLN.Zero,
      interbankNet = PLN.Zero,
      status = if failed then Banking.BankStatus.Failed(0) else Banking.BankStatus.Active(0),
      demandDeposits = PLN(6e9),
      termDeposits = PLN(4e9),
      loansShort = PLN(3e9),
      loansMedium = PLN(4e9),
      loansLong = PLN(3e9),
      consumerLoans = PLN.Zero,
      consumerNpl = PLN.Zero,
      corpBondHoldings = PLN.Zero,
    )

  private def mkHh(bankId: Int, savings: Double = 50000.0): Household.State =
    Household.State(
      id = HhId(0),
      savings = PLN(savings),
      debt = PLN.Zero,
      monthlyRent = PLN.Zero,
      skill = Share(0.5),
      healthPenalty = Share.Zero,
      mpc = Share(0.7),
      status = HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(8000.0)),
      socialNeighbors = Array.empty[HhId],
      bankId = BankId(bankId),
      equityWealth = PLN.Zero,
      lastSectorIdx = SectorIdx(0),
      isImmigrant = false,
      numDependentChildren = 0,
      consumerDebt = PLN.Zero,
      education = 2,
      taskRoutineness = Share(0.5),
      wageScar = Share.Zero,
    )

  "DepositMobility" should "not move deposits when all banks are healthy" in {
    val banks  = Vector(mkBank(0, 0.15), mkBank(1, 0.14))
    val hhs    = Vector(mkHh(0), mkHh(1))
    val result = DepositMobility(hhs, banks, anyBankFailed = false, new Random(42))
    result.households.map(_.bankId) shouldBe hhs.map(_.bankId)
  }

  it should "trigger flight from weak bank (low CAR)" in {
    val banks    = Vector(mkBank(0, 0.04), mkBank(1, 0.15)) // bank 0 below threshold
    val hhs      = (0 until 100).map(_ => mkHh(0)).toVector
    val result   = DepositMobility(hhs, banks, anyBankFailed = false, new Random(42))
    val switched = result.households.count(_.bankId == BankId(1))
    switched should be > 0
  }

  it should "trigger panic switching when a bank fails" in {
    // HH at bank 2, bank 1 fails, healthiest is bank 0 → some HH panic-switch to bank 0
    val banks    = Vector(mkBank(0, 0.15), mkBank(1, 0.15, failed = true), mkBank(2, 0.12))
    val hhs      = (0 until 100).map(_ => mkHh(2)).toVector
    val result   = DepositMobility(hhs, banks, anyBankFailed = true, new Random(42))
    val switched = result.households.count(_.bankId == BankId(0))
    // With panicRate = 3%, expect ~3 of 100 to switch
    switched should be > 0
  }

  it should "preserve total number of households" in {
    val banks  = Vector(mkBank(0, 0.04), mkBank(1, 0.15))
    val hhs    = (0 until 50).map(i => mkHh(i % 2)).toVector
    val result = DepositMobility(hhs, banks, anyBankFailed = true, new Random(42))
    result.households.length shouldBe hhs.length
  }

  it should "move deposits toward healthiest bank" in {
    val banks   = Vector(mkBank(0, 0.04), mkBank(1, 0.20), mkBank(2, 0.12))
    val hhs     = (0 until 100).map(_ => mkHh(0)).toVector
    val result  = DepositMobility(hhs, banks, anyBankFailed = false, new Random(42))
    // Bank 1 is healthiest (CAR 20%) — switchers should go there
    val atBank1 = result.households.count(_.bankId == BankId(1))
    val atBank2 = result.households.count(_.bankId == BankId(2))
    atBank1 should be >= atBank2
  }

  it should "be deterministic with same seed" in {
    val banks = Vector(mkBank(0, 0.04), mkBank(1, 0.15))
    val hhs   = (0 until 100).map(_ => mkHh(0)).toVector
    val r1    = DepositMobility(hhs, banks, anyBankFailed = true, new Random(42))
    val r2    = DepositMobility(hhs, banks, anyBankFailed = true, new Random(42))
    r1.households.map(_.bankId) shouldBe r2.households.map(_.bankId)
  }
