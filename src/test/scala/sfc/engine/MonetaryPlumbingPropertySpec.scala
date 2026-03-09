package sfc.engine

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sfc.Generators.*
import sfc.accounting.Sfc
import sfc.agents.Banking
import sfc.types.*

/** Monetary plumbing property-based tests. */
class MonetaryPlumbingPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  import sfc.config.SimParams
  given SimParams                                                         = SimParams.defaults
  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  // =========================================================================
  // Reserve Interest Properties
  // =========================================================================

  "reserveInterest" should "be non-negative for alive banks with non-negative reserves" in
    forAll(genBanking.BankState, genRate) { (bank, rate) =>
      whenever(!bank.failed && bank.reservesAtNbp >= PLN.Zero && rate >= 0) {
        Banking.reserveInterest(bank, rate) should be >= 0.0
      }
    }

  it should "scale linearly with reserves" in
    forAll(genRate, Gen.choose(1e4, 1e9), Gen.choose(1.1, 5.0)) { (rate, reserves, mult) =>
      whenever(rate > 0.001) {
        val b1 = Banking.BankState(
          BankId(0),
          PLN(1e9),
          PLN(5e8),
          PLN(1e8),
          PLN.Zero,
          PLN.Zero,
          PLN(reserves),
          PLN.Zero,
          false,
          0,
          0,
        )
        val b2 = b1.copy(reservesAtNbp = PLN(reserves * mult))
        val r1 = Banking.reserveInterest(b1, rate)
        val r2 = Banking.reserveInterest(b2, rate)
        r2 shouldBe (r1 * mult +- 1.0)
      }
    }

  "computeReserveInterest total" should "equal sum of per-bank interest" in
    forAll(genBanking.State, genRate) { (bs, rate) =>
      val (perBank, total) = Banking.computeReserveInterest(bs.banks, rate)
      total shouldBe (perBank.sum +- 0.01)
    }

  // =========================================================================
  // Interbank Interest Properties
  // =========================================================================

  "interbankInterestFlows" should "net to zero for balanced positions" in
    forAll(Gen.choose(-1e8, 1e8), genRate) { (net1, rate) =>
      whenever(rate > 0.001) {
        val banks      = Vector(
          Banking
            .BankState(BankId(0), PLN(1e9), PLN(5e8), PLN(1e8), PLN.Zero, PLN.Zero, PLN.Zero, PLN(net1), false, 0, 0),
          Banking.BankState(
            BankId(1),
            PLN(1e9),
            PLN(5e8),
            PLN(1e8),
            PLN.Zero,
            PLN.Zero,
            PLN.Zero,
            PLN(-net1),
            false,
            0,
            0,
          ),
        )
        val (_, total) = Banking.interbankInterestFlows(banks, rate)
        total shouldBe (0.0 +- 1.0)
      }
    }

  it should "return zero for all-zero positions" in
    forAll(genRate) { rate =>
      val banks            = Vector(
        Banking.BankState(BankId(0), PLN(1e9), PLN(5e8), PLN(1e8), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, false, 0, 0),
        Banking.BankState(BankId(1), PLN(1e9), PLN(5e8), PLN(1e8), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, false, 0, 0),
      )
      val (perBank, total) = Banking.interbankInterestFlows(banks, rate)
      perBank.foreach(_ shouldBe 0.0)
      total shouldBe 0.0
    }

  // =========================================================================
  // SFC: consistent flows with monetary plumbing still pass
  // =========================================================================

  "Sfc with monetary plumbing flows" should "pass for consistent snapshots" in
    forAll(genConsistentFlowsAndSnapshots) { case (prev, curr, flows) =>
      val result = Sfc.validate(prev, curr, flows)
      result shouldBe Right(())
    }

  it should "detect reserve interest perturbation" in
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(1000.0, 1e6)) { case ((prev, curr, flows), delta) =>
      // Add reserve interest to flows but NOT to bank capital → should fail
      val perturbedFlows = flows.copy(reserveInterest = flows.reserveInterest + PLN(delta))
      val result         = Sfc.validate(prev, curr, perturbedFlows)
      result shouldBe a[Left[?, ?]]
    }
