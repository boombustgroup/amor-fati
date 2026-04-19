package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InterbankContagionSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  private val p  = summon[SimParams]
  private val td = ComputationBoundary

  // Helper: create a minimal bank state with given interbank net
  private def mkBankRow(id: Int, interbankNet: Double, capital: Double = 1e9, failed: Boolean = false): (Banking.BankState, Banking.BankFinancialStocks) =
    (
      Banking.BankState(
        id = BankId(id),
        capital = PLN(capital),
        nplAmount = PLN.Zero,
        htmBookYield = Rate(0.05),
        status = if failed then Banking.BankStatus.Failed(ExecutionMonth.First) else Banking.BankStatus.Active(0),
        loansShort = PLN(1.5e9),
        loansMedium = PLN(2e9),
        loansLong = PLN(1.5e9),
        consumerNpl = PLN.Zero,
      ),
      Banking.BankFinancialStocks(
        totalDeposits = PLN(10e9),
        firmLoan = PLN(5e9),
        govBondAfs = PLN(2e9),
        govBondHtm = PLN(1e9),
        reserve = PLN.Zero,
        interbankLoan = PLN(interbankNet),
        demandDeposit = PLN(6e9),
        termDeposit = PLN(4e9),
        consumerLoan = PLN.Zero,
      ),
    )

  private def banks(rows: Vector[(Banking.BankState, Banking.BankFinancialStocks)]): Vector[Banking.BankState] =
    rows.map(_._1)

  private def stocks(rows: Vector[(Banking.BankState, Banking.BankFinancialStocks)]): Vector[Banking.BankFinancialStocks] =
    rows.map(_._2)

  "buildExposureMatrix" should "have zero diagonal" in {
    val rows   = Vector(mkBankRow(0, 100.0), mkBankRow(1, -50.0), mkBankRow(2, -50.0))
    val matrix = InterbankContagion.buildExposureMatrix(banks(rows), stocks(rows))
    for i <- 0 until 3 do matrix(i)(i) shouldBe PLN.Zero
  }

  it should "have lender → borrower exposures proportional to deficit share" in {
    val rows   = Vector(mkBankRow(0, 100.0), mkBankRow(1, -60.0), mkBankRow(2, -40.0))
    val matrix = InterbankContagion.buildExposureMatrix(banks(rows), stocks(rows))
    // Bank 0 lends 100, borrower 1 has 60% of deficit, borrower 2 has 40%
    td.toDouble(matrix(0)(1)) shouldBe 60.0 +- 0.01
    td.toDouble(matrix(0)(2)) shouldBe 40.0 +- 0.01
    // Borrowers don't lend
    matrix(1)(0) shouldBe PLN.Zero
    matrix(2)(0) shouldBe PLN.Zero
  }

  it should "preserve each lender row sum exactly" in {
    val rows   = Vector(mkBankRow(0, 101.0), mkBankRow(1, -60.0), mkBankRow(2, -41.0))
    val matrix = InterbankContagion.buildExposureMatrix(banks(rows), stocks(rows))
    matrix(0).map(_.toLong).sum shouldBe PLN(101.0).toLong
  }

  it should "match borrower deficits exactly when interbank market clears exactly" in {
    val rows   = Vector(mkBankRow(0, 101.0), mkBankRow(1, 99.0), mkBankRow(2, -60.0), mkBankRow(3, -140.0))
    val matrix = InterbankContagion.buildExposureMatrix(banks(rows), stocks(rows))
    val col2   = matrix.map(_(2).toLong).sum
    val col3   = matrix.map(_(3).toLong).sum
    col2 shouldBe PLN(60.0).toLong
    col3 shouldBe PLN(140.0).toLong
  }

  it should "return zero matrix when no borrowing" in {
    val rows   = Vector(mkBankRow(0, 100.0), mkBankRow(1, 50.0))
    val matrix = InterbankContagion.buildExposureMatrix(banks(rows), stocks(rows))
    matrix.flatten.forall(_ == PLN.Zero) shouldBe true
  }

  "applyContagionLosses" should "reduce capital of exposed lenders when counterparty fails" in {
    val rows         = Vector(mkBankRow(0, 100.0, capital = 1e9), mkBankRow(1, -100.0, capital = -1.0, failed = true))
    val matrix       = InterbankContagion.buildExposureMatrix(banks(rows), stocks(rows))
    val after        = InterbankContagion.applyContagionLosses(banks(rows), matrix)
    // Lender loses exposure × (1 - recovery)
    val expectedLoss = 100.0 * (1.0 - td.toDouble(p.banking.interbankRecoveryRate))
    td.toDouble(after(0).capital) shouldBe (1e9 - expectedLoss) +- 0.01
  }

  it should "not affect banks with no exposure to failed bank" in {
    val rows   = Vector(mkBankRow(0, 0.0, capital = 1e9), mkBankRow(1, -100.0, capital = -1.0, failed = true), mkBankRow(2, 100.0, capital = 1e9))
    val matrix = InterbankContagion.buildExposureMatrix(banks(rows), stocks(rows))
    val after  = InterbankContagion.applyContagionLosses(banks(rows), matrix)
    td.toDouble(after(0).capital) shouldBe 1e9 +- 0.01 // safe bank untouched
  }

  "hoardingFactor" should "be 1.0 when NPL below threshold" in {
    val factor = InterbankContagion.hoardingFactor(Share(0.02))
    td.toDouble(factor) shouldBe 1.0 +- 0.01
  }

  it should "decrease when NPL exceeds threshold" in {
    val factor = InterbankContagion.hoardingFactor(Share(0.10))
    td.toDouble(factor) should be < 1.0
    td.toDouble(factor) should be >= 0.0
  }

  it should "be zero (full freeze) at very high NPL" in {
    val factor = InterbankContagion.hoardingFactor(Share(0.50))
    td.toDouble(factor) shouldBe 0.0 +- 0.01
  }
