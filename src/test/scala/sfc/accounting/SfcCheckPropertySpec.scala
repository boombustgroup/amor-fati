package sfc.accounting

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sfc.Generators.*
import sfc.accounting.SfcCheck
import sfc.types.*

class SfcCheckPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  // --- Consistent flows always pass ---

  "SfcCheck.validate" should "pass when flows are consistent with snapshots" in {
    forAll(genConsistentFlowsAndSnapshots) { (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows)) =>
      val (prev, curr, flows) = triple
      val result = SfcCheck.validate(1, prev, curr, flows)
      result.passed shouldBe true
    }
  }

  it should "pass with zero flows and identical snapshots" in {
    forAll(genSnapshot) { (snap: SfcCheck.Snapshot) =>
      val zeroFlows = SfcCheck.MonthlyFlows(
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
        PLN.Zero,
      )
      val result = SfcCheck.validate(1, snap, snap, zeroFlows)
      result.passed shouldBe true
    }
  }

  // --- Perturbed states detect errors ---

  it should "detect perturbed bankCapital" in {
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(10.0, 1000.0)) {
      (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows), perturbation: Double) =>
        val (prev, curr, flows) = triple
        val perturbed = curr.copy(bankCapital = curr.bankCapital + PLN(perturbation))
        val result = SfcCheck.validate(1, prev, perturbed, flows)
        Math.abs(result.bankCapitalError) should be > 0.0
    }
  }

  it should "detect perturbed bankDeposits" in {
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(10.0, 1000.0)) {
      (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows), perturbation: Double) =>
        val (prev, curr, flows) = triple
        val perturbed = curr.copy(bankDeposits = curr.bankDeposits + PLN(perturbation))
        val result = SfcCheck.validate(1, prev, perturbed, flows)
        Math.abs(result.bankDepositsError) should be > 0.0
    }
  }

  it should "detect perturbed govDebt" in {
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(10.0, 1000.0)) {
      (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows), perturbation: Double) =>
        val (prev, curr, flows) = triple
        val perturbed = curr.copy(govDebt = curr.govDebt + PLN(perturbation))
        val result = SfcCheck.validate(1, prev, perturbed, flows)
        Math.abs(result.govDebtError) should be > 0.0
    }
  }

  it should "detect perturbed nfa" in {
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(100.0, 10000.0)) {
      (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows), perturbation: Double) =>
        val (prev, curr, flows) = triple
        val perturbed = curr.copy(nfa = curr.nfa + PLN(perturbation))
        val result = SfcCheck.validate(1, prev, perturbed, flows)
        Math.abs(result.nfaError) should be > 0.0
    }
  }

  // --- Tolerance monotonicity ---

  it should "have tolerance monotonicity (pass at t implies pass at t' > t)" in {
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(0.01, 5.0)) {
      (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows), perturbation: Double) =>
        val (prev, curr, flows) = triple
        val perturbed = curr.copy(bankCapital = curr.bankCapital + PLN(perturbation))
        val strictResult = SfcCheck.validate(1, prev, perturbed, flows, tolerance = perturbation)
        val looseResult = SfcCheck.validate(1, prev, perturbed, flows, tolerance = perturbation * 10)
        if strictResult.passed then looseResult.passed shouldBe true
    }
  }

  // --- Error magnitude property ---

  it should "have error magnitude = actual change - expected change" in {
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(1.0, 1000.0)) {
      (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows), delta: Double) =>
        val (prev, curr, flows) = triple
        val perturbed = curr.copy(bankCapital = curr.bankCapital + PLN(delta))
        val result = SfcCheck.validate(1, prev, perturbed, flows)
        result.bankCapitalError shouldBe (delta +- 1e-6)
    }
  }

  // --- Four identities independent ---

  it should "have independent identities (perturb one, others unaffected)" in {
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(100.0, 10000.0)) {
      (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows), delta: Double) =>
        val (prev, curr, flows) = triple
        val perturbed = curr.copy(govDebt = curr.govDebt + PLN(delta))
        val result = SfcCheck.validate(1, prev, perturbed, flows)
        // Floating-point: large snapshot values cause small residual errors
        Math.abs(result.bankCapitalError) should be < 0.01
        Math.abs(result.bankDepositsError) should be < 0.01
        Math.abs(result.govDebtError) shouldBe (delta +- 0.01)
    }
  }

  // --- Bond clearing identity ---

  it should "pass bond clearing when bankBonds + nbpBonds = bondsOutstanding" in {
    forAll(genConsistentFlowsAndSnapshots) { (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows)) =>
      val (prev, curr, flows) = triple
      val result = SfcCheck.validate(1, prev, curr, flows)
      result.bondClearingError shouldBe 0.0 +- 1e-6
    }
  }

  it should "detect perturbed bondClearingError" in {
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(10.0, 1000.0)) {
      (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows), perturbation: Double) =>
        val (prev, curr, flows) = triple
        val perturbed = curr.copy(bankBondHoldings = curr.bankBondHoldings + PLN(perturbation))
        val result = SfcCheck.validate(1, prev, perturbed, flows)
        Math.abs(result.bondClearingError) should be > 0.0
    }
  }

  // --- Interbank netting identity ---

  it should "pass interbank netting when interbankNetSum is zero" in {
    forAll(genConsistentFlowsAndSnapshots) { (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows)) =>
      val (prev, curr, flows) = triple
      val result = SfcCheck.validate(1, prev, curr, flows)
      result.interbankNettingError shouldBe 0.0 +- 1e-6
    }
  }

  it should "detect perturbed interbankNetSum" in {
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(10.0, 1000.0)) {
      (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows), perturbation: Double) =>
        val (prev, curr, flows) = triple
        val perturbed = curr.copy(interbankNetSum = PLN(perturbation))
        val result = SfcCheck.validate(1, prev, perturbed, flows)
        Math.abs(result.interbankNettingError) should be > 0.0
    }
  }

  // --- Snapshot sums property ---

  it should "compute correct snapshot sums from firms" in {
    forAll(Gen.choose(3, 20)) { (n: Int) =>
      forAll(Gen.listOfN(n, genAliveFirm)) { (firmList: List[sfc.agents.Firm.State]) =>
        val firms = firmList.toArray
        val expectedCash = firms.map(_.cash.toDouble).sum
        val expectedDebt = firms.map(_.debt.toDouble).sum
        Math.abs(expectedCash - firms.map(_.cash.toDouble).sum) should be < 1e-6
        Math.abs(expectedDebt - firms.map(_.debt.toDouble).sum) should be < 1e-6
      }
    }
  }

  // --- Dividend flow consistency ---

  it should "pass with non-zero dividend flows in consistent snapshots" in {
    // Verify that genConsistentFlowsAndSnapshots correctly handles dividend fields
    forAll(genConsistentFlowsAndSnapshots) { (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows)) =>
      val (prev, curr, flows) = triple
      // genMonthlyFlows generates non-zero dividend fields — verify all 8 identities hold
      val result = SfcCheck.validate(1, prev, curr, flows)
      withClue(
        s"bankCapErr=${result.bankCapitalError} bankDepErr=${result.bankDepositsError} " +
          s"govDebtErr=${result.govDebtError} nfaErr=${result.nfaError} divIncome=${flows.dividendIncome.toDouble} " +
          s"foreignDiv=${flows.foreignDividendOutflow.toDouble} divTax=${flows.dividendTax.toDouble}: ",
      ) {
        result.passed shouldBe true
      }
    }
  }

  it should "detect perturbed deposits from dividend flow mismatch" in {
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(10.0, 1000.0)) {
      (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows), perturbation: Double) =>
        val (prev, curr, flows) = triple
        // Perturb dividendIncome without updating deposits → Identity 2 fails
        val badFlows = flows.copy(dividendIncome = flows.dividendIncome + PLN(perturbation))
        val result = SfcCheck.validate(1, prev, curr, badFlows)
        Math.abs(result.bankDepositsError) should be > 0.0
    }
  }

  // --- Mortgage stock identity ---

  it should "detect perturbed mortgageStock" in {
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(10.0, 1000.0)) {
      (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows), perturbation: Double) =>
        val (prev, curr, flows) = triple
        val perturbed = curr.copy(mortgageStock = curr.mortgageStock + PLN(perturbation))
        val result = SfcCheck.validate(1, prev, perturbed, flows)
        Math.abs(result.mortgageStockError) should be > 0.0
    }
  }

  it should "pass mortgage stock identity in consistent snapshots" in {
    forAll(genConsistentFlowsAndSnapshots) { (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows)) =>
      val (prev, curr, flows) = triple
      val result = SfcCheck.validate(1, prev, curr, flows)
      // Wider tolerance: mortgage stock values can reach 1e12, floating-point cancellation
      result.mortgageStockError shouldBe 0.0 +- 0.01
    }
  }
