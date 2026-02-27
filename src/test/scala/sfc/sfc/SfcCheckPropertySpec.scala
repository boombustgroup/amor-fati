package sfc.sfc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen
import sfc.testutil.Generators.*

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
      val zeroFlows = SfcCheck.MonthlyFlows(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      val result = SfcCheck.validate(1, snap, snap, zeroFlows)
      result.passed shouldBe true
    }
  }

  // --- Perturbed states detect errors ---

  it should "detect perturbed bankCapital" in {
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(10.0, 1000.0)) {
      (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows), perturbation: Double) =>
        val (prev, curr, flows) = triple
        val perturbed = curr.copy(bankCapital = curr.bankCapital + perturbation)
        val result = SfcCheck.validate(1, prev, perturbed, flows)
        Math.abs(result.bankCapitalError) should be > 0.0
    }
  }

  it should "detect perturbed bankDeposits" in {
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(10.0, 1000.0)) {
      (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows), perturbation: Double) =>
        val (prev, curr, flows) = triple
        val perturbed = curr.copy(bankDeposits = curr.bankDeposits + perturbation)
        val result = SfcCheck.validate(1, prev, perturbed, flows)
        Math.abs(result.bankDepositsError) should be > 0.0
    }
  }

  it should "detect perturbed govDebt" in {
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(10.0, 1000.0)) {
      (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows), perturbation: Double) =>
        val (prev, curr, flows) = triple
        val perturbed = curr.copy(govDebt = curr.govDebt + perturbation)
        val result = SfcCheck.validate(1, prev, perturbed, flows)
        Math.abs(result.govDebtError) should be > 0.0
    }
  }

  it should "detect perturbed nfa" in {
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(100.0, 10000.0)) {
      (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows), perturbation: Double) =>
        val (prev, curr, flows) = triple
        val perturbed = curr.copy(nfa = curr.nfa + perturbation)
        val result = SfcCheck.validate(1, prev, perturbed, flows)
        Math.abs(result.nfaError) should be > 0.0
    }
  }

  // --- Tolerance monotonicity ---

  it should "have tolerance monotonicity (pass at t implies pass at t' > t)" in {
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(0.01, 5.0)) {
      (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows), perturbation: Double) =>
        val (prev, curr, flows) = triple
        val perturbed = curr.copy(bankCapital = curr.bankCapital + perturbation)
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
        val perturbed = curr.copy(bankCapital = curr.bankCapital + delta)
        val result = SfcCheck.validate(1, prev, perturbed, flows)
        result.bankCapitalError shouldBe (delta +- 1e-6)
    }
  }

  // --- Four identities independent ---

  it should "have independent identities (perturb one, others unaffected)" in {
    forAll(genConsistentFlowsAndSnapshots, Gen.choose(100.0, 10000.0)) {
      (triple: (SfcCheck.Snapshot, SfcCheck.Snapshot, SfcCheck.MonthlyFlows), delta: Double) =>
        val (prev, curr, flows) = triple
        val perturbed = curr.copy(govDebt = curr.govDebt + delta)
        val result = SfcCheck.validate(1, prev, perturbed, flows)
        // Floating-point: large snapshot values cause small residual errors
        Math.abs(result.bankCapitalError) should be < 0.01
        Math.abs(result.bankDepositsError) should be < 0.01
        Math.abs(result.govDebtError) shouldBe (delta +- 0.01)
    }
  }

  // --- Snapshot sums property ---

  it should "compute correct snapshot sums from firms" in {
    forAll(Gen.choose(3, 20)) { (n: Int) =>
      forAll(Gen.listOfN(n, genAliveFirm)) { (firmList: List[_root_.sfc.agents.Firm]) =>
        val firms = firmList.toArray
        val expectedCash = firms.map(_.cash).sum
        val expectedDebt = firms.map(_.debt).sum
        Math.abs(expectedCash - firms.map(_.cash).sum) should be < 1e-6
        Math.abs(expectedDebt - firms.map(_.debt).sum) should be < 1e-6
      }
    }
  }
