package com.boombustgroup.amorfati.agents

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.boombustgroup.amorfati.Generators.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*

class CentralBankPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  // --- bondYield properties ---

  "Nbp.bondYield" should "be >= 0 for all inputs" in
    forAll(genRate, Gen.choose(0.0, 2.0), Gen.choose(0.0, 0.50), Gen.choose(-1e10, 1e10)) {
      (refRate: Double, debtToGdp: Double, nbpBondGdpShare: Double, nfa: Double) =>
        val y = Nbp.bondYield(Rate(refRate), Ratio(debtToGdp), Ratio(nbpBondGdpShare), PLN(nfa), Rate.Zero)
        y.toDouble should be >= 0.0
    }

  it should "cap fiscal risk premium at 10% even at extreme debtToGdp" in
    forAll(genRate, Gen.choose(1.0, 100.0), Gen.choose(0.0, 0.50), Gen.choose(-1e10, 1e10)) {
      (refRate: Double, debtToGdp: Double, nbpBondGdpShare: Double, nfa: Double) =>
        val y = Nbp.bondYield(Rate(refRate), Ratio(debtToGdp), Ratio(nbpBondGdpShare), PLN(nfa), Rate.Zero)
        // Fiscal risk ≤ 0.10, so yield ≤ refRate + termPremium + 0.10
        y.toDouble should be <= (refRate + p.fiscal.govTermPremium.toDouble + 0.10 + 0.001)
    }

  it should "be monotonic in debtToGdp (higher debt → higher yield)" in
    forAll(genRate, Gen.choose(0.0, 1.0), Gen.choose(0.0, 0.50), Gen.choose(-1e10, 1e10)) {
      (refRate: Double, baseDebt: Double, nbpBondGdpShare: Double, nfa: Double) =>
        val low  = Nbp.bondYield(Rate(refRate), Ratio(baseDebt), Ratio(nbpBondGdpShare), PLN(nfa), Rate.Zero)
        val high = Nbp.bondYield(Rate(refRate), Ratio(baseDebt + 0.10), Ratio(nbpBondGdpShare), PLN(nfa), Rate.Zero)
        high.toDouble should be >= (low.toDouble - 1e-10)
    }

  it should "be monotonically decreasing in nbpBondGdpShare (QE effect)" in
    forAll(genRate, Gen.choose(0.0, 1.0), Gen.choose(0.0, 0.30), Gen.choose(-1e10, 1e10)) { (refRate: Double, debtToGdp: Double, baseQe: Double, nfa: Double) =>
      val low  = Nbp.bondYield(Rate(refRate), Ratio(debtToGdp), Ratio(baseQe + 0.10), PLN(nfa), Rate.Zero)
      val high = Nbp.bondYield(Rate(refRate), Ratio(debtToGdp), Ratio(baseQe), PLN(nfa), Rate.Zero)
      high.toDouble should be >= (low.toDouble - 1e-10)
    }

  // --- executeQe properties ---

  "Nbp.executeQe" should "always return purchase >= 0" in
    forAll(genNbpState, Gen.choose(0.0, 1e10), Gen.choose(1e6, 1e12)) { (nbp: Nbp.State, bankBonds: Double, gdp: Double) =>
      val qeResult = Nbp.executeQe(nbp, PLN(bankBonds), PLN(gdp))
      qeResult.requestedPurchase.toDouble should be >= 0.0
    }

  it should "not exceed bankBondHoldings" in
    forAll(genNbpState, Gen.choose(0.0, 1e10), Gen.choose(1e6, 1e12)) { (nbp: Nbp.State, bankBonds: Double, gdp: Double) =>
      val qeResult = Nbp.executeQe(nbp, PLN(bankBonds), PLN(gdp))
      qeResult.requestedPurchase.toDouble should be <= (bankBonds + 1e-6)
    }

  it should "not exceed max GDP share limit when active" in
    forAll(Gen.choose(0.0, 0.25), Gen.choose(0.0, 1e10), Gen.choose(1e6, 1e12)) { (rate: Double, bankBonds: Double, gdp: Double) =>
      val nbp      = Nbp.State(Rate(rate), PLN.Zero, true, PLN.Zero, PLN.Zero, PLN.Zero)
      val qeResult = Nbp.executeQe(nbp, PLN(bankBonds), PLN(gdp))
      qeResult.nbpState.govBondHoldings.toDouble should be <= (p.monetary.qeMaxGdpShare.toDouble * gdp + 1e-6)
    }

  it should "preserve bond clearing through QE (bank - purchase + nbp + purchase = total)" in
    forAll(genNbpState, Gen.choose(0.0, 1e10), Gen.choose(1e6, 1e12)) { (nbp: Nbp.State, bankBonds: Double, gdp: Double) =>
      val totalBefore = bankBonds + nbp.govBondHoldings.toDouble
      val qeResult    = Nbp.executeQe(nbp, PLN(bankBonds), PLN(gdp))
      val totalAfter  = (bankBonds - qeResult.requestedPurchase.toDouble) + qeResult.nbpState.govBondHoldings.toDouble
      totalAfter shouldBe (totalBefore +- 1.0) // FP tolerance at 1e10 scale
    }

  // --- shouldActivateQe properties ---

  "Nbp.shouldActivateQe" should "imply rate near floor when true" in
    forAll(genRate, genInflation) { (refRate: Double, inflation: Double) =>
      if Nbp.shouldActivateQe(Rate(refRate), Rate(inflation)) then refRate should be <= (p.monetary.rateFloor.toDouble + 0.0025)
    }

  // --- shouldTaperQe properties ---

  "Nbp.shouldTaperQe" should "be consistent with inflation vs target" in
    forAll(genInflation) { (inflation: Double) =>
      val shouldTaper = Nbp.shouldTaperQe(Rate(inflation))
      if shouldTaper then inflation should be > p.monetary.targetInfl.toDouble
    }
