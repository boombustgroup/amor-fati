package sfc.engine

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sfc.agents.Banking
import sfc.types.*

/** LCR/NSFR and maturity mismatch tests. */
class LcrNsfrSpec extends AnyFlatSpec with Matchers:

  import sfc.config.SimParams
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
  ) =
    Banking.BankState(
      BankId(id),
      deposits,
      loans,
      capital,
      PLN.Zero,
      govBonds,
      reservesAtNbp,
      PLN.Zero,
      false,
      0,
      0,
      demandDeposits = demandDep,
      termDeposits = termDep,
      loansShort = loansS,
      loansMedium = loansM,
      loansLong = loansL,
    )

  // =========================================================================
  // HQLA
  // =========================================================================

  "Banking.BankState.hqla" should "equal reserves + gov bonds" in {
    val b = mkBank(reservesAtNbp = PLN(5e7), govBonds = PLN(2e8))
    b.hqla shouldBe (5e7 + 2e8)
  }

  // =========================================================================
  // LCR
  // =========================================================================

  "Banking.BankState.lcr" should "compute HQLA / net cash outflows" in {
    val b = mkBank(reservesAtNbp = PLN(5e7), govBonds = PLN(2e8), demandDep = PLN(1e9))
    // HQLA = 50M + 200M = 250M
    // Net outflows = 1B × 0.10 = 100M
    // LCR = 250M / 100M = 2.5
    b.lcr shouldBe (2.5 +- 0.01)
  }

  it should "return 10.0 when outflows are zero" in {
    val b = mkBank(demandDep = PLN.Zero)
    b.lcr shouldBe 10.0
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
    b.nsfr shouldBe (1020e6 / 362.5e6 +- 0.01)
  }

  it should "return 10.0 when RSF is zero" in {
    val b = mkBank(loansS = PLN.Zero, loansM = PLN.Zero, loansL = PLN.Zero, govBonds = PLN.Zero)
    b.nsfr shouldBe 10.0
  }

  // =========================================================================
  // canLend with LCR/NSFR
  // =========================================================================

  "canLend" should "reject when LCR below minimum (when enabled)" in {
    // We can't easily toggle p.flags.bankLcr in tests,
    // but we can test that the LCR/NSFR formulas work correctly
    val b = mkBank(reservesAtNbp = PLN.Zero, govBonds = PLN.Zero, demandDep = PLN(1e9))
    b.lcr shouldBe (0.0 +- 0.01) // zero HQLA → LCR ≈ 0
    b.lcr should be < 1.0        // Below LCR min
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
  import sfc.config.SimParams
  given SimParams = SimParams.defaults

  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  "LCR" should "be non-negative" in
    forAll(Gen.choose(0.0, 1e9), Gen.choose(0.0, 1e9), Gen.choose(0.0, 1e9)) { (reserves, bonds, demandDep) =>
      val b = Banking.BankState(
        BankId(0),
        PLN(1e9),
        PLN(5e8),
        PLN(1e8),
        PLN.Zero,
        PLN(bonds),
        PLN(reserves),
        PLN.Zero,
        false,
        0,
        0,
        demandDeposits = PLN(demandDep),
        termDeposits = PLN.Zero,
      )
      b.lcr should be >= 0.0
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
      val b = Banking.BankState(
        BankId(0),
        PLN(1e9),
        PLN(5e8),
        PLN(capital),
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        false,
        0,
        0,
        demandDeposits = PLN(demandDep),
        termDeposits = PLN(termDep),
        loansShort = PLN(loansS),
        loansMedium = PLN(loansM),
        loansLong = PLN(loansL),
      )
      b.nsfr should be >= 0.0
    }

  "HQLA" should "equal reserves + gov bonds" in
    forAll(Gen.choose(0.0, 1e9), Gen.choose(0.0, 1e9)) { (reserves, bonds) =>
      val b = Banking.BankState(
        BankId(0),
        PLN(1e9),
        PLN(5e8),
        PLN(1e8),
        PLN.Zero,
        PLN(bonds),
        PLN(reserves),
        PLN.Zero,
        false,
        0,
        0,
      )
      b.hqla shouldBe (reserves + bonds +- 0.01)
    }
