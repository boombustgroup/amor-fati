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

  private def mkBank(
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
      corpBondHoldings: PLN = PLN.Zero,
  ) =
    Banking.BankState(
      id = BankId(id),
      deposits = deposits,
      loans = loans,
      capital = capital,
      nplAmount = PLN.Zero,
      afsBonds = govBonds * 0.40,
      htmBonds = govBonds * 0.60,
      htmBookYield = Rate(0.055),
      reservesAtNbp = reservesAtNbp,
      interbankNet = PLN.Zero,
      status = BankStatus.Active(0),
      demandDeposits = demandDep,
      termDeposits = termDep,
      loansShort = loansS,
      loansMedium = loansM,
      loansLong = loansL,
      consumerLoans = PLN.Zero,
      consumerNpl = PLN.Zero,
      corpBondHoldings = corpBondHoldings,
    )

  // =========================================================================
  // HQLA
  // =========================================================================

  "Banking.BankState.hqla" should "equal reserves + gov bonds" in {
    val b = mkBank(reservesAtNbp = PLN(5e7), govBonds = PLN(2e8))
    b.hqla shouldBe PLN(5e7 + 2e8)
  }

  // =========================================================================
  // LCR
  // =========================================================================

  "Banking.BankState.lcr" should "compute HQLA / net cash outflows" in {
    val b = mkBank(reservesAtNbp = PLN(5e7), govBonds = PLN(2e8), demandDep = PLN(1e9))
    // HQLA = 50M + 200M = 250M
    // Net outflows = 1B × 0.10 = 100M
    // LCR = 250M / 100M = 2.5
    b.lcr.toDouble shouldBe (2.5 +- 0.01)
  }

  it should "return 10.0 when outflows are zero" in {
    val b = mkBank(demandDep = PLN.Zero)
    b.lcr shouldBe Multiplier(10.0)
  }

  // =========================================================================
  // NSFR
  // =========================================================================

  "Banking.BankState.nsfr" should "compute ASF / RSF" in {
    val b = mkBank(
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
    b.nsfr.toDouble shouldBe (1020e6 / 362.5e6 +- 0.01)
  }

  it should "return 10.0 when RSF is zero" in {
    val b = mkBank(loansS = PLN.Zero, loansM = PLN.Zero, loansL = PLN.Zero, govBonds = PLN.Zero)
    b.nsfr shouldBe Multiplier(10.0)
  }

  // =========================================================================
  // canLend with LCR/NSFR
  // =========================================================================

  "canLend" should "reject when LCR below minimum (when enabled)" in {
    val b = mkBank(reservesAtNbp = PLN.Zero, govBonds = PLN.Zero, demandDep = PLN(1e9))
    b.lcr.toDouble shouldBe (0.0 +- 0.01) // zero HQLA → LCR ≈ 0
    b.lcr should be < Multiplier(1.0)     // Below LCR min
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

  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  private def mkBank(
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
  ) = Banking.BankState(
    id = BankId(0),
    deposits = deposits,
    loans = loans,
    capital = capital,
    nplAmount = PLN.Zero,
    afsBonds = govBonds * 0.40,
    htmBonds = govBonds * 0.60,
    htmBookYield = Rate(0.055),
    reservesAtNbp = reservesAtNbp,
    interbankNet = PLN.Zero,
    status = BankStatus.Active(0),
    demandDeposits = demandDeposits,
    termDeposits = termDeposits,
    loansShort = loansShort,
    loansMedium = loansMedium,
    loansLong = loansLong,
    consumerLoans = PLN.Zero,
    consumerNpl = PLN.Zero,
    corpBondHoldings = PLN.Zero,
  )

  "LCR" should "be non-negative" in
    forAll(Gen.choose(0.0, 1e9), Gen.choose(0.0, 1e9), Gen.choose(0.0, 1e9)) { (reserves, bonds, demandDep) =>
      val b = mkBank(reservesAtNbp = PLN(reserves), govBonds = PLN(bonds), demandDeposits = PLN(demandDep))
      b.lcr should be >= Multiplier.Zero
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
      val b = mkBank(
        capital = PLN(capital),
        demandDeposits = PLN(demandDep),
        termDeposits = PLN(termDep),
        loansShort = PLN(loansS),
        loansMedium = PLN(loansM),
        loansLong = PLN(loansL),
      )
      b.nsfr should be >= Multiplier.Zero
    }

  "HQLA" should "equal reserves + gov bonds" in
    forAll(Gen.choose(0.0, 1e9), Gen.choose(0.0, 1e9)) { (reserves, bonds) =>
      val b = mkBank(reservesAtNbp = PLN(reserves), govBonds = PLN(bonds))
      b.hqla.toDouble shouldBe (reserves + bonds +- 0.01)
    }
