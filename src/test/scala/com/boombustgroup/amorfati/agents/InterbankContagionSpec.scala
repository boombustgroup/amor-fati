package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InterbankContagionSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  private val p = summon[SimParams]

  // Helper: create a minimal bank state with given interbank net
  private def mkBank(id: Int, interbankNet: Double, capital: Double = 1e9, failed: Boolean = false): Banking.BankState =
    Banking.BankState(
      id = BankId(id),
      deposits = PLN(10e9),
      loans = PLN(5e9),
      capital = PLN(capital),
      nplAmount = PLN.Zero,
      afsBonds = PLN(2e9),
      htmBonds = PLN(1e9),
      htmBookYield = Rate(0.05),
      reservesAtNbp = PLN.Zero,
      interbankNet = PLN(interbankNet),
      status = if failed then Banking.BankStatus.Failed(0) else Banking.BankStatus.Active(0),
      demandDeposits = PLN(6e9),
      termDeposits = PLN(4e9),
      loansShort = PLN(1.5e9),
      loansMedium = PLN(2e9),
      loansLong = PLN(1.5e9),
      consumerLoans = PLN.Zero,
      consumerNpl = PLN.Zero,
      corpBondHoldings = PLN.Zero,
    )

  "buildExposureMatrix" should "have zero diagonal" in {
    val banks  = Vector(mkBank(0, 100.0), mkBank(1, -50.0), mkBank(2, -50.0))
    val matrix = InterbankContagion.buildExposureMatrix(banks)
    for i <- 0 until 3 do matrix(i)(i) shouldBe PLN.Zero
  }

  it should "have lender → borrower exposures proportional to deficit share" in {
    val banks  = Vector(mkBank(0, 100.0), mkBank(1, -60.0), mkBank(2, -40.0))
    val matrix = InterbankContagion.buildExposureMatrix(banks)
    // Bank 0 lends 100, borrower 1 has 60% of deficit, borrower 2 has 40%
    matrix(0)(1).toDouble shouldBe 60.0 +- 0.01
    matrix(0)(2).toDouble shouldBe 40.0 +- 0.01
    // Borrowers don't lend
    matrix(1)(0) shouldBe PLN.Zero
    matrix(2)(0) shouldBe PLN.Zero
  }

  it should "return zero matrix when no borrowing" in {
    val banks  = Vector(mkBank(0, 100.0), mkBank(1, 50.0))
    val matrix = InterbankContagion.buildExposureMatrix(banks)
    matrix.flatten.forall(_ == PLN.Zero) shouldBe true
  }

  "applyContagionLosses" should "reduce capital of exposed lenders when counterparty fails" in {
    val lender       = mkBank(0, 100.0, capital = 1e9)
    val borrower     = mkBank(1, -100.0, capital = -1.0, failed = true)
    val banks        = Vector(lender, borrower)
    val matrix       = InterbankContagion.buildExposureMatrix(banks)
    val after        = InterbankContagion.applyContagionLosses(banks, matrix)
    // Lender loses exposure × (1 - recovery)
    val expectedLoss = 100.0 * (1.0 - p.banking.interbankRecoveryRate.toDouble)
    after(0).capital.toDouble shouldBe (1e9 - expectedLoss) +- 0.01
  }

  it should "not affect banks with no exposure to failed bank" in {
    val safe     = mkBank(0, 0.0, capital = 1e9)
    val borrower = mkBank(1, -100.0, capital = -1.0, failed = true)
    val lender   = mkBank(2, 100.0, capital = 1e9)
    val banks    = Vector(safe, borrower, lender)
    val matrix   = InterbankContagion.buildExposureMatrix(banks)
    val after    = InterbankContagion.applyContagionLosses(banks, matrix)
    after(0).capital.toDouble shouldBe 1e9 +- 0.01 // safe bank untouched
  }

  "hoardingFactor" should "be 1.0 when NPL below threshold" in {
    val factor = InterbankContagion.hoardingFactor(Ratio(0.02))
    factor.toDouble shouldBe 1.0 +- 0.01
  }

  it should "decrease when NPL exceeds threshold" in {
    val factor = InterbankContagion.hoardingFactor(Ratio(0.10))
    factor.toDouble should be < 1.0
    factor.toDouble should be >= 0.0
  }

  it should "be zero (full freeze) at very high NPL" in {
    val factor = InterbankContagion.hoardingFactor(Ratio(0.50))
    factor.toDouble shouldBe 0.0 +- 0.01
  }
