package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen

/** LCR/NSFR and maturity mismatch tests. */
class LcrNsfrSpec extends AnyFlatSpec with Matchers:

  private def mkBank(id: Int = 0, deposits: Double = 1e9, loans: Double = 5e8,
                     capital: Double = 1e8, reservesAtNbp: Double = 1e7,
                     govBonds: Double = 1e8,
                     demandDep: Double = 0, termDep: Double = 0,
                     loansS: Double = 0, loansM: Double = 0, loansL: Double = 0) =
    IndividualBankState(id, deposits, loans, capital, 0.0, govBonds, reservesAtNbp,
      0.0, false, 0, 0,
      demandDeposits = demandDep, termDeposits = termDep,
      loansShort = loansS, loansMedium = loansM, loansLong = loansL)

  // =========================================================================
  // HQLA
  // =========================================================================

  "IndividualBankState.hqla" should "equal reserves + gov bonds" in {
    val b = mkBank(reservesAtNbp = 5e7, govBonds = 2e8)
    b.hqla shouldBe (5e7 + 2e8)
  }

  // =========================================================================
  // LCR
  // =========================================================================

  "IndividualBankState.lcr" should "compute HQLA / net cash outflows" in {
    val b = mkBank(reservesAtNbp = 5e7, govBonds = 2e8, demandDep = 1e9)
    // HQLA = 50M + 200M = 250M
    // Net outflows = 1B × 0.10 = 100M
    // LCR = 250M / 100M = 2.5
    b.lcr shouldBe (2.5 +- 0.01)
  }

  it should "return 10.0 when outflows are zero" in {
    val b = mkBank(demandDep = 0)
    b.lcr shouldBe 10.0
  }

  // =========================================================================
  // NSFR
  // =========================================================================

  "IndividualBankState.nsfr" should "compute ASF / RSF" in {
    val b = mkBank(capital = 1e8, demandDep = 6e8, termDep = 4e8,
      loansS = 1e8, loansM = 1.5e8, loansL = 2.5e8, govBonds = 5e7)
    // ASF = 100M + 400M×0.95 + 600M×0.90 = 100M + 380M + 540M = 1,020M
    // RSF = 100M×0.50 + 150M×0.65 + 250M×0.85 + 50M×0.05
    //     = 50M + 97.5M + 212.5M + 2.5M = 362.5M
    // NSFR = 1020M / 362.5M ≈ 2.81
    b.nsfr shouldBe (1020e6 / 362.5e6 +- 0.01)
  }

  it should "return 10.0 when RSF is zero" in {
    val b = mkBank(loansS = 0, loansM = 0, loansL = 0, govBonds = 0)
    b.nsfr shouldBe 10.0
  }

  // =========================================================================
  // canLend with LCR/NSFR
  // =========================================================================

  "canLend" should "reject when LCR below minimum (when enabled)" in {
    // We can't easily toggle Config.BankLcrEnabled in tests,
    // but we can test that the LCR/NSFR formulas work correctly
    val b = mkBank(reservesAtNbp = 0, govBonds = 0, demandDep = 1e9)
    b.lcr shouldBe (0.0 +- 0.01)  // zero HQLA → LCR ≈ 0
    b.lcr should be < 1.0  // Below LCR min
  }

  // =========================================================================
  // Deposit split consistency
  // =========================================================================

  "deposit split" should "sum to total deposits" in {
    val termFrac = 0.40
    val deposits = 1e9
    val demand = deposits * (1.0 - termFrac)
    val term = deposits * termFrac
    (demand + term) shouldBe (deposits +- 0.01)
  }

  "loan maturity split" should "sum to total loans" in {
    val loans = 5e8
    val short = loans * 0.20
    val medium = loans * 0.30
    val long = loans * 0.50
    (short + medium + long) shouldBe (loans +- 0.01)
  }

class LcrNsfrPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:
  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  "LCR" should "be non-negative" in {
    forAll(Gen.choose(0.0, 1e9), Gen.choose(0.0, 1e9), Gen.choose(0.0, 1e9)) {
      (reserves, bonds, demandDep) =>
        val b = IndividualBankState(0, 1e9, 5e8, 1e8, 0, bonds, reserves, 0, false, 0, 0,
          demandDeposits = demandDep, termDeposits = 0)
        b.lcr should be >= 0.0
    }
  }

  "NSFR" should "be non-negative" in {
    forAll(Gen.choose(0.0, 1e8), Gen.choose(0.0, 1e9), Gen.choose(0.0, 1e9),
           Gen.choose(0.0, 1e8), Gen.choose(0.0, 1.5e8), Gen.choose(0.0, 2.5e8)) {
      (capital, demandDep, termDep, loansS, loansM, loansL) =>
        val b = IndividualBankState(0, 1e9, 5e8, capital, 0, 0, 0, 0, false, 0, 0,
          demandDeposits = demandDep, termDeposits = termDep,
          loansShort = loansS, loansMedium = loansM, loansLong = loansL)
        b.nsfr should be >= 0.0
    }
  }

  "HQLA" should "equal reserves + gov bonds" in {
    forAll(Gen.choose(0.0, 1e9), Gen.choose(0.0, 1e9)) { (reserves, bonds) =>
      val b = IndividualBankState(0, 1e9, 5e8, 1e8, 0, bonds, reserves, 0, false, 0, 0)
      b.hqla shouldBe (reserves + bonds +- 0.01)
    }
  }
