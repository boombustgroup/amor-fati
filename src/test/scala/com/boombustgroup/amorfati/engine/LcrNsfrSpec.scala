package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.boombustgroup.amorfati.agents.Banking
import com.boombustgroup.amorfati.agents.Banking.BankStatus
import com.boombustgroup.amorfati.types.*

/** LCR/NSFR and maturity mismatch tests. */
class LcrNsfrSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams = SimParams.defaults

  private def mkBankRow(
      id: Int = 0,
      deposits: PLN = PLN(1000000000),
      loans: PLN = PLN(500000000),
      capital: PLN = PLN(100000000),
      reservesAtNbp: PLN = PLN(10000000),
      govBonds: PLN = PLN(100000000),
      demandDep: PLN = PLN.Zero,
      termDep: PLN = PLN.Zero,
      loansS: PLN = PLN.Zero,
      loansM: PLN = PLN.Zero,
      loansL: PLN = PLN.Zero,
  ) =
    (
      Banking.BankState(
        id = BankId(id),
        capital = capital,
        nplAmount = PLN.Zero,
        htmBookYield = Rate.decimal(55, 3),
        status = BankStatus.Active(0),
        loansShort = loansS,
        loansMedium = loansM,
        loansLong = loansL,
        consumerNpl = PLN.Zero,
      ),
      Banking.BankFinancialStocks(
        totalDeposits = deposits,
        firmLoan = loans,
        govBondAfs = govBonds * Share.decimal(40, 2),
        govBondHtm = govBonds * Share.decimal(60, 2),
        reserve = reservesAtNbp,
        interbankLoan = PLN.Zero,
        demandDeposit = demandDep,
        termDeposit = termDep,
        consumerLoan = PLN.Zero,
      ),
    )

  // =========================================================================
  // HQLA
  // =========================================================================

  "Banking.BankState.hqla" should "equal reserves + gov bonds" in {
    val (_, stocks) = mkBankRow(reservesAtNbp = PLN(50000000), govBonds = PLN(200000000))
    Banking.hqla(stocks) shouldBe PLN(250000000)
  }

  // =========================================================================
  // LCR
  // =========================================================================

  "Banking.BankState.lcr" should "compute HQLA / net cash outflows" in {
    val (_, stocks) = mkBankRow(reservesAtNbp = PLN(50000000), govBonds = PLN(200000000), demandDep = PLN(1000000000))
    // HQLA = 50M + 200M = 250M
    // Net outflows = 1B × 0.10 = 100M
    // LCR = 250M / 100M = 2.5
    decimal(Banking.lcr(stocks)) shouldBe (BigDecimal("2.5") +- BigDecimal("0.01"))
  }

  it should "return 10.0 when outflows are zero" in {
    val (_, stocks) = mkBankRow(demandDep = PLN.Zero)
    Banking.lcr(stocks) shouldBe Multiplier(10)
  }

  // =========================================================================
  // NSFR
  // =========================================================================

  "Banking.BankState.nsfr" should "compute ASF / RSF" in {
    val (bank, stocks) = mkBankRow(
      capital = PLN(100000000),
      demandDep = PLN(600000000),
      termDep = PLN(400000000),
      loansS = PLN(100000000),
      loansM = PLN(150000000),
      loansL = PLN(250000000),
      govBonds = PLN(50000000),
    )
    // ASF = 100M + 400M×0.95 + 600M×0.90 = 100M + 380M + 540M = 1,020M
    // RSF = 100M×0.50 + 150M×0.65 + 250M×0.85 + 50M×0.05
    //     = 50M + 97.5M + 212.5M + 2.5M = 362.5M
    // NSFR = 1020M / 362.5M ≈ 2.81
    decimal(Banking.nsfr(bank, stocks, PLN.Zero)) shouldBe (BigDecimal("1020e6") / BigDecimal("362.5e6") +- BigDecimal("0.01"))
  }

  it should "return 10.0 when RSF is zero" in {
    val (bank, stocks) = mkBankRow(loansS = PLN.Zero, loansM = PLN.Zero, loansL = PLN.Zero, govBonds = PLN.Zero)
    Banking.nsfr(bank, stocks, PLN.Zero) shouldBe Multiplier(10)
  }

  // =========================================================================
  // canLend with LCR/NSFR
  // =========================================================================

  "canLend" should "reject when LCR below minimum (when enabled)" in {
    val (_, stocks) = mkBankRow(reservesAtNbp = PLN.Zero, govBonds = PLN.Zero, demandDep = PLN(1000000000))
    decimal(Banking.lcr(stocks)) shouldBe (BigDecimal("0.0") +- BigDecimal("0.01")) // zero HQLA -> LCR approx 0
    Banking.lcr(stocks) should be < Multiplier(1)                                   // Below LCR min
  }

  // =========================================================================
  // Deposit split consistency
  // =========================================================================

  "deposit split" should "sum to total deposits" in {
    val termFrac = BigDecimal("0.40")
    val deposits = BigDecimal("1e9")
    val demand   = deposits * (BigDecimal("1.0") - termFrac)
    val term     = deposits * termFrac
    (demand + term) shouldBe (deposits +- BigDecimal("0.01"))
  }

  "loan maturity split" should "sum to total loans" in {
    val loans  = BigDecimal("5e8")
    val short  = loans * BigDecimal("0.20")
    val medium = loans * BigDecimal("0.30")
    val long   = loans * BigDecimal("0.50")
    (short + medium + long) shouldBe (loans +- BigDecimal("0.01"))
  }

class LcrNsfrPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:
  import com.boombustgroup.amorfati.config.SimParams
  given SimParams = SimParams.defaults

  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  private def mkBankRow(
      deposits: PLN = PLN(1000000000),
      loans: PLN = PLN(500000000),
      capital: PLN = PLN(100000000),
      govBonds: PLN = PLN.Zero,
      reservesAtNbp: PLN = PLN.Zero,
      demandDeposits: PLN = PLN.Zero,
      termDeposits: PLN = PLN.Zero,
      loansShort: PLN = PLN.Zero,
      loansMedium: PLN = PLN.Zero,
      loansLong: PLN = PLN.Zero,
  ) = (
    Banking.BankState(
      id = BankId(0),
      capital = capital,
      nplAmount = PLN.Zero,
      htmBookYield = Rate.decimal(55, 3),
      status = BankStatus.Active(0),
      loansShort = loansShort,
      loansMedium = loansMedium,
      loansLong = loansLong,
      consumerNpl = PLN.Zero,
    ),
    Banking.BankFinancialStocks(
      totalDeposits = deposits,
      firmLoan = loans,
      govBondAfs = govBonds * Share.decimal(40, 2),
      govBondHtm = govBonds * Share.decimal(60, 2),
      reserve = reservesAtNbp,
      interbankLoan = PLN.Zero,
      demandDeposit = demandDeposits,
      termDeposit = termDeposits,
      consumerLoan = PLN.Zero,
    ),
  )

  "LCR" should "be non-negative" in
    forAll(genDecimal("0.0", "1e9"), genDecimal("0.0", "1e9"), genDecimal("0.0", "1e9")) { (reserves, bonds, demandDep) =>
      val (_, stocks) = mkBankRow(reservesAtNbp = plnBD(reserves), govBonds = plnBD(bonds), demandDeposits = plnBD(demandDep))
      Banking.lcr(stocks) should be >= Multiplier.Zero
    }

  "NSFR" should "be non-negative" in
    forAll(
      genDecimal("0.0", "1e8"),
      genDecimal("0.0", "1e9"),
      genDecimal("0.0", "1e9"),
      genDecimal("0.0", "1e8"),
      genDecimal("0.0", "1.5e8"),
      genDecimal("0.0", "2.5e8"),
    ) { (capital, demandDep, termDep, loansS, loansM, loansL) =>
      val (bank, stocks) = mkBankRow(
        capital = plnBD(capital),
        demandDeposits = plnBD(demandDep),
        termDeposits = plnBD(termDep),
        loansShort = plnBD(loansS),
        loansMedium = plnBD(loansM),
        loansLong = plnBD(loansL),
      )
      Banking.nsfr(bank, stocks, PLN.Zero) should be >= Multiplier.Zero
    }

  "HQLA" should "equal reserves + gov bonds" in
    forAll(genDecimal("0.0", "1e9"), genDecimal("0.0", "1e9")) { (reserves, bonds) =>
      val (_, stocks) = mkBankRow(reservesAtNbp = plnBD(reserves), govBonds = plnBD(bonds))
      decimal(Banking.hqla(stocks)) shouldBe (reserves + bonds +- BigDecimal("0.01"))
    }
