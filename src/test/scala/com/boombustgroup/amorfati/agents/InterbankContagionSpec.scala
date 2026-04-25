package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InterbankContagionSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  private val p = summon[SimParams]

  // Helper: create a minimal bank state with given interbank net
  private def mkBankRow(
      id: Int,
      interbankNet: BigDecimal,
      capital: BigDecimal = BigDecimal("1e9"),
      failed: Boolean = false,
  ): (Banking.BankState, Banking.BankFinancialStocks) =
    (
      Banking.BankState(
        id = BankId(id),
        capital = plnBD(capital),
        nplAmount = PLN.Zero,
        htmBookYield = Rate.decimal(5, 2),
        status = if failed then Banking.BankStatus.Failed(ExecutionMonth.First) else Banking.BankStatus.Active(0),
        loansShort = PLN(1500000000),
        loansMedium = PLN(2000000000),
        loansLong = PLN(1500000000),
        consumerNpl = PLN.Zero,
      ),
      Banking.BankFinancialStocks(
        totalDeposits = PLN(10000000000L),
        firmLoan = PLN(5000000000L),
        govBondAfs = PLN(2000000000),
        govBondHtm = PLN(1000000000),
        reserve = PLN.Zero,
        interbankLoan = plnBD(interbankNet),
        demandDeposit = PLN(6000000000L),
        termDeposit = PLN(4000000000L),
        consumerLoan = PLN.Zero,
      ),
    )

  private def banks(rows: Vector[(Banking.BankState, Banking.BankFinancialStocks)]): Vector[Banking.BankState] =
    rows.map(_._1)

  private def stocks(rows: Vector[(Banking.BankState, Banking.BankFinancialStocks)]): Vector[Banking.BankFinancialStocks] =
    rows.map(_._2)

  "buildExposureMatrix" should "have zero diagonal" in {
    val rows   = Vector(mkBankRow(0, BigDecimal("100.0")), mkBankRow(1, -BigDecimal("50.0")), mkBankRow(2, -BigDecimal("50.0")))
    val matrix = InterbankContagion.buildExposureMatrix(banks(rows), stocks(rows))
    for i <- 0 until 3 do matrix(i)(i) shouldBe PLN.Zero
  }

  it should "have lender → borrower exposures proportional to deficit share" in {
    val rows   = Vector(mkBankRow(0, BigDecimal("100.0")), mkBankRow(1, -BigDecimal("60.0")), mkBankRow(2, -BigDecimal("40.0")))
    val matrix = InterbankContagion.buildExposureMatrix(banks(rows), stocks(rows))
    // Bank 0 lends 100, borrower 1 has 60% of deficit, borrower 2 has 40%
    decimal(matrix(0)(1)) shouldBe BigDecimal("60.0") +- BigDecimal("0.01")
    decimal(matrix(0)(2)) shouldBe BigDecimal("40.0") +- BigDecimal("0.01")
    // Borrowers don't lend
    matrix(1)(0) shouldBe PLN.Zero
    matrix(2)(0) shouldBe PLN.Zero
  }

  it should "preserve each lender row sum exactly" in {
    val rows   = Vector(mkBankRow(0, BigDecimal("101.0")), mkBankRow(1, -BigDecimal("60.0")), mkBankRow(2, -BigDecimal("41.0")))
    val matrix = InterbankContagion.buildExposureMatrix(banks(rows), stocks(rows))
    matrix(0).map(_.toLong).sum shouldBe PLN(101).toLong
  }

  it should "match borrower deficits exactly when interbank market clears exactly" in {
    val rows   =
      Vector(mkBankRow(0, BigDecimal("101.0")), mkBankRow(1, BigDecimal("99.0")), mkBankRow(2, -BigDecimal("60.0")), mkBankRow(3, -BigDecimal("140.0")))
    val matrix = InterbankContagion.buildExposureMatrix(banks(rows), stocks(rows))
    val col2   = matrix.map(_(2).toLong).sum
    val col3   = matrix.map(_(3).toLong).sum
    col2 shouldBe PLN(60).toLong
    col3 shouldBe PLN(140).toLong
  }

  it should "return zero matrix when no borrowing" in {
    val rows   = Vector(mkBankRow(0, BigDecimal("100.0")), mkBankRow(1, BigDecimal("50.0")))
    val matrix = InterbankContagion.buildExposureMatrix(banks(rows), stocks(rows))
    matrix.flatten.forall(_ == PLN.Zero) shouldBe true
  }

  it should "exclude failed zero-position banks from exposures" in {
    val rows   = Vector(mkBankRow(0, BigDecimal("100.0")), mkBankRow(1, -BigDecimal("100.0")), mkBankRow(2, BigDecimal("0.0"), failed = true))
    val matrix = InterbankContagion.buildExposureMatrix(banks(rows), stocks(rows))
    matrix(2).forall(_ == PLN.Zero) shouldBe true
    matrix.map(_(2)).forall(_ == PLN.Zero) shouldBe true
  }

  it should "reject failed banks with non-zero interbank positions" in {
    val rows = Vector(mkBankRow(0, BigDecimal("100.0")), mkBankRow(1, -BigDecimal("100.0"), failed = true))
    val ex   = intercept[IllegalArgumentException]:
      InterbankContagion.buildExposureMatrix(banks(rows), stocks(rows))
    ex.getMessage should include("failed bank 1")
    ex.getMessage should include("interbankLoan")
  }

  "applyContagionLosses" should "reduce capital of exposed lenders when counterparty fails" in {
    val exposureRows = Vector(mkBankRow(0, BigDecimal("100.0"), capital = BigDecimal("1e9")), mkBankRow(1, -BigDecimal("100.0"), capital = BigDecimal("1e9")))
    val failedRows   =
      Vector(mkBankRow(0, BigDecimal("100.0"), capital = BigDecimal("1e9")), mkBankRow(1, -BigDecimal("100.0"), capital = -BigDecimal("1.0"), failed = true))
    val matrix       = InterbankContagion.buildExposureMatrix(banks(exposureRows), stocks(exposureRows))
    val after        = InterbankContagion.applyContagionLosses(banks(failedRows), matrix)
    // Lender loses exposure × (1 - recovery)
    val expectedLoss = BigDecimal("100.0") * (BigDecimal("1.0") - decimal(p.banking.interbankRecoveryRate))
    decimal(after(0).capital) shouldBe (BigDecimal("1e9") - expectedLoss) +- BigDecimal("0.01")
  }

  it should "not affect banks with no exposure to failed bank" in {
    val exposureRows = Vector(
      mkBankRow(0, BigDecimal("0.0"), capital = BigDecimal("1e9")),
      mkBankRow(1, -BigDecimal("100.0"), capital = BigDecimal("1e9")),
      mkBankRow(2, BigDecimal("100.0"), capital = BigDecimal("1e9")),
    )
    val failedRows   =
      Vector(
        mkBankRow(0, BigDecimal("0.0"), capital = BigDecimal("1e9")),
        mkBankRow(1, -BigDecimal("100.0"), capital = -BigDecimal("1.0"), failed = true),
        mkBankRow(2, BigDecimal("100.0"), capital = BigDecimal("1e9")),
      )
    val matrix       = InterbankContagion.buildExposureMatrix(banks(exposureRows), stocks(exposureRows))
    val after        = InterbankContagion.applyContagionLosses(banks(failedRows), matrix)
    decimal(after(0).capital) shouldBe BigDecimal("1e9") +- BigDecimal("0.01") // safe bank untouched
  }

  it should "make contagion insolvency visible to the same-month failure check" in {
    val exposureRows = Vector(mkBankRow(0, BigDecimal("100.0"), capital = BigDecimal("1.0")), mkBankRow(1, -BigDecimal("100.0"), capital = BigDecimal("1e9")))
    val failedRows   =
      Vector(mkBankRow(0, BigDecimal("100.0"), capital = BigDecimal("1.0")), mkBankRow(1, -BigDecimal("100.0"), capital = -BigDecimal("1.0"), failed = true))
    val matrix       = InterbankContagion.buildExposureMatrix(banks(exposureRows), stocks(exposureRows))
    val afterLosses  = InterbankContagion.applyContagionLosses(banks(failedRows), matrix)
    val cleanStocks  = stocks(exposureRows).map(_.copy(demandDeposit = PLN.Zero, termDeposit = PLN.Zero))
    val checked      = Banking.checkFailures(afterLosses, cleanStocks, ExecutionMonth(30), enabled = true, Multiplier.Zero)

    afterLosses(0).capital should be < PLN.Zero
    checked.banks(0).failed shouldBe true
    checked.anyFailed shouldBe true
  }

  "hoardingFactor" should "be 1.0 when NPL below threshold" in {
    val factor = InterbankContagion.hoardingFactor(Share.decimal(2, 2))
    decimal(factor) shouldBe BigDecimal("1.0") +- BigDecimal("0.01")
  }

  it should "decrease when NPL exceeds threshold" in {
    val factor = InterbankContagion.hoardingFactor(Share.decimal(10, 2))
    decimal(factor) should be < BigDecimal("1.0")
    decimal(factor) should be >= BigDecimal("0.0")
  }

  it should "be zero (full freeze) at very high NPL" in {
    val factor = InterbankContagion.hoardingFactor(Share.decimal(50, 2))
    decimal(factor) shouldBe BigDecimal("0.0") +- BigDecimal("0.01")
  }
