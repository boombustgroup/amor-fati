package sfc.accounting

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.accounting.SfcCheck
import sfc.types.*

class BopSfcCheckSpec extends AnyFlatSpec with Matchers:

  private val zeroFlows = SfcCheck.MonthlyFlows(PLN(0), PLN(0), PLN(0), PLN(0), PLN(0), PLN(0), PLN(0), PLN(0), PLN(0))
  private val baseSnap =
    SfcCheck.Snapshot(PLN(0), PLN(0), PLN(500000), PLN(0), PLN(200000), PLN(1000000), PLN(0), PLN(0), nfa = PLN(0.0))

  // ---- Identity 4: NFA ----

  "SfcCheck.validate (NFA)" should "pass trivially when OPEN_ECON is off (all zeros)" in {
    val result = SfcCheck.validate(1, baseSnap, baseSnap, zeroFlows)
    result.nfaError shouldBe 0.0
    result.passed shouldBe true
  }

  it should "pass when NFA change matches CA + valuation" in {
    val ca = PLN(50000.0)
    val valuation = PLN(3000.0)
    val prev = baseSnap.copy(nfa = PLN(100000.0))
    val curr = baseSnap.copy(nfa = PLN(100000.0) + ca + valuation)
    val flows = zeroFlows.copy(currentAccount = ca, valuationEffect = valuation)
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.nfaError shouldBe 0.0 +- 0.01
    result.passed shouldBe true
  }

  it should "detect error when NFA doesn't match CA + valuation" in {
    val prev = baseSnap.copy(nfa = PLN(100000.0))
    // Bug: NFA unchanged despite positive CA
    val curr = baseSnap.copy(nfa = PLN(100000.0))
    val flows = zeroFlows.copy(currentAccount = PLN(25000.0), valuationEffect = PLN(0.0))
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.nfaError shouldBe -25000.0 +- 0.01
    result.passed shouldBe false
  }

  it should "handle negative NFA changes (capital outflow)" in {
    val prev = baseSnap.copy(nfa = PLN(100000.0))
    val curr = baseSnap.copy(nfa = PLN(70000.0))
    val flows = zeroFlows.copy(currentAccount = PLN(-30000.0), valuationEffect = PLN(0.0))
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.nfaError shouldBe 0.0 +- 0.01
    result.passed shouldBe true
  }

  it should "include valuation effects in NFA check" in {
    val prev = baseSnap.copy(nfa = PLN(100000.0))
    // CA=-10000, valuation=+15000 → expected ΔNFA = +5000
    val curr = baseSnap.copy(nfa = PLN(105000.0))
    val flows = zeroFlows.copy(currentAccount = PLN(-10000.0), valuationEffect = PLN(15000.0))
    val result = SfcCheck.validate(1, prev, curr, flows)
    result.nfaError shouldBe 0.0 +- 0.01
    result.passed shouldBe true
  }

  it should "fail 4th identity without breaking other 3 identities" in {
    val prev = baseSnap.copy(nfa = PLN(100000.0))
    // NFA jumps by 50000 but CA+valuation = 0 → error on identity 4 only
    val curr = baseSnap.copy(nfa = PLN(150000.0))
    val result = SfcCheck.validate(1, prev, curr, zeroFlows)
    result.bankCapitalError shouldBe 0.0
    result.bankDepositsError shouldBe 0.0
    result.govDebtError shouldBe 0.0
    result.nfaError shouldBe 50000.0 +- 0.01
    result.passed shouldBe false
  }
