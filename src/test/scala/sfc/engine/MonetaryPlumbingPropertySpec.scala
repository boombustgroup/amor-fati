package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen
import sfc.testutil.Generators.*
import sfc.sfc.SfcCheck

/** Monetary plumbing property-based tests. */
class MonetaryPlumbingPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:
  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  // =========================================================================
  // Reserve Interest Properties
  // =========================================================================

  "reserveInterest" should "be non-negative for alive banks with non-negative reserves" in {
    forAll(genIndividualBankState, genRate) { (bank, rate) =>
      whenever(!bank.failed && bank.reservesAtNbp >= 0 && rate >= 0) {
        BankingSector.reserveInterest(bank, rate) should be >= 0.0
      }
    }
  }

  it should "scale linearly with reserves" in {
    forAll(genRate, Gen.choose(1e4, 1e9), Gen.choose(1.1, 5.0)) { (rate, reserves, mult) =>
      whenever(rate > 0.001) {
        val b1 = IndividualBankState(0, 1e9, 5e8, 1e8, 0, 0, reserves, 0, false, 0, 0)
        val b2 = b1.copy(reservesAtNbp = reserves * mult)
        val r1 = BankingSector.reserveInterest(b1, rate)
        val r2 = BankingSector.reserveInterest(b2, rate)
        r2 shouldBe (r1 * mult +- 1.0)
      }
    }
  }

  "computeReserveInterest total" should "equal sum of per-bank interest" in {
    forAll(genBankingSectorState, genRate) { (bs, rate) =>
      val (perBank, total) = BankingSector.computeReserveInterest(bs.banks, rate)
      total shouldBe (perBank.sum +- 0.01)
    }
  }

  // =========================================================================
  // Interbank Interest Properties
  // =========================================================================

  "interbankInterestFlows" should "net to zero for balanced positions" in {
    forAll(Gen.choose(-1e8, 1e8), genRate) { (net1, rate) =>
      whenever(rate > 0.001) {
        val banks = Vector(
          IndividualBankState(0, 1e9, 5e8, 1e8, 0, 0, 0, net1, false, 0, 0),
          IndividualBankState(1, 1e9, 5e8, 1e8, 0, 0, 0, -net1, false, 0, 0)
        )
        val (_, total) = BankingSector.interbankInterestFlows(banks, rate)
        total shouldBe (0.0 +- 1.0)
      }
    }
  }

  it should "return zero for all-zero positions" in {
    forAll(genRate) { rate =>
      val banks = Vector(
        IndividualBankState(0, 1e9, 5e8, 1e8, 0, 0, 0, 0, false, 0, 0),
        IndividualBankState(1, 1e9, 5e8, 1e8, 0, 0, 0, 0, false, 0, 0)
      )
      val (perBank, total) = BankingSector.interbankInterestFlows(banks, rate)
      perBank.foreach(_ shouldBe 0.0)
      total shouldBe 0.0
    }
  }

  // =========================================================================
  // SFC: consistent flows with monetary plumbing still pass
  // =========================================================================

  "SfcCheck with monetary plumbing flows" should "pass for consistent snapshots" in {
    forAll(genConsistentFlowsAndSnapshots) { case (prev, curr, flows) =>
      val result = SfcCheck.validate(1, prev, curr, flows)
      withClue(s"bankCapErr=${result.bankCapitalError}, bankDepErr=${result.bankDepositsError}, " +
        s"govDebtErr=${result.govDebtError}: ") {
        result.passed shouldBe true
      }
    }
  }

  it should "detect reserve interest perturbation" in {
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(1000.0, 1e6)) { case ((prev, curr, flows), delta) =>
      // Add reserve interest to flows but NOT to bank capital → should fail
      val perturbedFlows = flows.copy(reserveInterest = flows.reserveInterest + delta)
      val result = SfcCheck.validate(1, prev, curr, perturbedFlows)
      result.passed shouldBe false
    }
  }
