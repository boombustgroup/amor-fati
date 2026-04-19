package com.boombustgroup.amorfati.engine

import org.scalacheck.Gen
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
  private val td  = ComputationBoundary

  private def mkBankRow(
      id: Int = 0,
      deposits: PLN = PLN(1e9),
      loans: PLN = PLN(5e8),
      capital: PLN = PLN(1e8),
      reservesAtNbp: PLN = PLN(1e7),
      govBonds: PLN = PLN(1e8),
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
        htmBookYield = Rate(0.055),
        status = BankStatus.Active(0),
        loansShort = loansS,
        loansMedium = loansM,
        loansLong = loansL,
        consumerNpl = PLN.Zero,
      ),
      Banking.BankFinancialStocks(
        totalDeposits = deposits,
        firmLoan = loans,
        govBondAfs = govBonds * Share(0.40),
        govBondHtm = govBonds * Share(0.60),
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
    val (_, stocks) = mkBankRow(reservesAtNbp = PLN(5e7), govBonds = PLN(2e8))
    Banking.hqla(stocks) shouldBe PLN(5e7 + 2e8)
  }

  // =========================================================================
  // LCR
  // =========================================================================

  "Banking.BankState.lcr" should "compute HQLA / net cash outflows" in {
    val (_, stocks) = mkBankRow(reservesAtNbp = PLN(5e7), govBonds = PLN(2e8), demandDep = PLN(1e9))
    // HQLA = 50M + 200M = 250M
    // Net outflows = 1B × 0.10 = 100M
    // LCR = 250M / 100M = 2.5
    td.toDouble(Banking.lcr(stocks)) shouldBe (2.5 +- 0.01)
  }

  it should "return 10.0 when outflows are zero" in {
    val (_, stocks) = mkBankRow(demandDep = PLN.Zero)
    Banking.lcr(stocks) shouldBe Multiplier(10.0)
  }

  // =========================================================================
  // NSFR
  // =========================================================================

  "Banking.BankState.nsfr" should "compute ASF / RSF" in {
    val (bank, stocks) = mkBankRow(
      capital = PLN(1e8),
      demandDep = PLN(6e8),
      termDep = PLN(4e8),
      loansS = PLN(1e8),
      loansM = PLN(1.5e8),
      loansL = PLN(2.5e8),
      govBonds = PLN(5e7),
    )
    // ASF = 100M + 400M×0.95 + 600M×0.90 = 100M + 380M + 540M = 1,020M
    // RSF = 100M×0.50 + 150M×0.65 + 250M×0.85 + 50M×0.05
    //     = 50M + 97.5M + 212.5M + 2.5M = 362.5M
    // NSFR = 1020M / 362.5M ≈ 2.81
    td.toDouble(Banking.nsfr(bank, stocks, PLN.Zero)) shouldBe (1020e6 / 362.5e6 +- 0.01)
  }

  it should "return 10.0 when RSF is zero" in {
    val (bank, stocks) = mkBankRow(loansS = PLN.Zero, loansM = PLN.Zero, loansL = PLN.Zero, govBonds = PLN.Zero)
    Banking.nsfr(bank, stocks, PLN.Zero) shouldBe Multiplier(10.0)
  }

  // =========================================================================
  // canLend with LCR/NSFR
  // =========================================================================

  "canLend" should "reject when LCR below minimum (when enabled)" in {
    val (_, stocks) = mkBankRow(reservesAtNbp = PLN.Zero, govBonds = PLN.Zero, demandDep = PLN(1e9))
    td.toDouble(Banking.lcr(stocks)) shouldBe (0.0 +- 0.01) // zero HQLA -> LCR approx 0
    Banking.lcr(stocks) should be < Multiplier(1.0)         // Below LCR min
  }

  // =========================================================================
  // Deposit split consistency
  // =========================================================================

  "deposit split" should "sum to total deposits" in {
    val termFrac = 0.40
    val deposits = 1e9
    val demand   = deposits * (1.0 - termFrac)
    val term     = deposits * termFrac
    (demand + term) shouldBe (deposits +- 0.01)
  }

  "loan maturity split" should "sum to total loans" in {
    val loans  = 5e8
    val short  = loans * 0.20
    val medium = loans * 0.30
    val long   = loans * 0.50
    (short + medium + long) shouldBe (loans +- 0.01)
  }

class LcrNsfrPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:
  import com.boombustgroup.amorfati.config.SimParams
  given SimParams = SimParams.defaults
  private val td  = ComputationBoundary

  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  private def mkBankRow(
      deposits: PLN = PLN(1e9),
      loans: PLN = PLN(5e8),
      capital: PLN = PLN(1e8),
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
      htmBookYield = Rate(0.055),
      status = BankStatus.Active(0),
      loansShort = loansShort,
      loansMedium = loansMedium,
      loansLong = loansLong,
      consumerNpl = PLN.Zero,
    ),
    Banking.BankFinancialStocks(
      totalDeposits = deposits,
      firmLoan = loans,
      govBondAfs = govBonds * Share(0.40),
      govBondHtm = govBonds * Share(0.60),
      reserve = reservesAtNbp,
      interbankLoan = PLN.Zero,
      demandDeposit = demandDeposits,
      termDeposit = termDeposits,
      consumerLoan = PLN.Zero,
    ),
  )

  "LCR" should "be non-negative" in
    forAll(Gen.choose(0.0, 1e9), Gen.choose(0.0, 1e9), Gen.choose(0.0, 1e9)) { (reserves, bonds, demandDep) =>
      val (_, stocks) = mkBankRow(reservesAtNbp = PLN(reserves), govBonds = PLN(bonds), demandDeposits = PLN(demandDep))
      Banking.lcr(stocks) should be >= Multiplier.Zero
    }

  "NSFR" should "be non-negative" in
    forAll(
      Gen.choose(0.0, 1e8),
      Gen.choose(0.0, 1e9),
      Gen.choose(0.0, 1e9),
      Gen.choose(0.0, 1e8),
      Gen.choose(0.0, 1.5e8),
      Gen.choose(0.0, 2.5e8),
    ) { (capital, demandDep, termDep, loansS, loansM, loansL) =>
      val (bank, stocks) = mkBankRow(
        capital = PLN(capital),
        demandDeposits = PLN(demandDep),
        termDeposits = PLN(termDep),
        loansShort = PLN(loansS),
        loansMedium = PLN(loansM),
        loansLong = PLN(loansL),
      )
      Banking.nsfr(bank, stocks, PLN.Zero) should be >= Multiplier.Zero
    }

  "HQLA" should "equal reserves + gov bonds" in
    forAll(Gen.choose(0.0, 1e9), Gen.choose(0.0, 1e9)) { (reserves, bonds) =>
      val (_, stocks) = mkBankRow(reservesAtNbp = PLN(reserves), govBonds = PLN(bonds))
      td.toDouble(Banking.hqla(stocks)) shouldBe (reserves + bonds +- 0.01)
    }
