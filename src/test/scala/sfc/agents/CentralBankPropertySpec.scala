package sfc.agents

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sfc.Generators.*
import sfc.config.Config
import sfc.types.*

class CentralBankPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  // --- bondYield properties ---

  "Nbp.bondYield" should "be >= 0 for all inputs" in {
    forAll(genRate, Gen.choose(0.0, 2.0), Gen.choose(0.0, 0.50), Gen.choose(-1e10, 1e10)) {
      (refRate: Double, debtToGdp: Double, nbpBondGdpShare: Double, nfa: Double) =>
        val y = Nbp.bondYield(refRate, debtToGdp, nbpBondGdpShare, nfa)
        y should be >= 0.0
    }
  }

  it should "cap fiscal risk premium at 10% even at extreme debtToGdp" in {
    forAll(genRate, Gen.choose(1.0, 100.0), Gen.choose(0.0, 0.50), Gen.choose(-1e10, 1e10)) {
      (refRate: Double, debtToGdp: Double, nbpBondGdpShare: Double, nfa: Double) =>
        val y = Nbp.bondYield(refRate, debtToGdp, nbpBondGdpShare, nfa)
        // Fiscal risk ≤ 0.10, so yield ≤ refRate + termPremium + 0.10
        y should be <= (refRate + Config.GovTermPremium + 0.10 + 0.001)
    }
  }

  it should "be monotonic in debtToGdp (higher debt → higher yield)" in {
    forAll(genRate, Gen.choose(0.0, 1.0), Gen.choose(0.0, 0.50), Gen.choose(-1e10, 1e10)) {
      (refRate: Double, baseDebt: Double, nbpBondGdpShare: Double, nfa: Double) =>
        val low = Nbp.bondYield(refRate, baseDebt, nbpBondGdpShare, nfa)
        val high = Nbp.bondYield(refRate, baseDebt + 0.10, nbpBondGdpShare, nfa)
        high should be >= (low - 1e-10)
    }
  }

  it should "be monotonically decreasing in nbpBondGdpShare (QE effect)" in {
    forAll(genRate, Gen.choose(0.0, 1.0), Gen.choose(0.0, 0.30), Gen.choose(-1e10, 1e10)) {
      (refRate: Double, debtToGdp: Double, baseQe: Double, nfa: Double) =>
        val low = Nbp.bondYield(refRate, debtToGdp, baseQe + 0.10, nfa)
        val high = Nbp.bondYield(refRate, debtToGdp, baseQe, nfa)
        high should be >= (low - 1e-10)
    }
  }

  // --- executeQe properties ---

  "Nbp.executeQe" should "always return purchase >= 0" in {
    forAll(genNbpState, Gen.choose(0.0, 1e10), Gen.choose(1e6, 1e12)) {
      (nbp: Nbp.State, bankBonds: Double, gdp: Double) =>
        val (_, purchase) = Nbp.executeQe(nbp, bankBonds, gdp)
        purchase should be >= 0.0
    }
  }

  it should "not exceed bankBondHoldings" in {
    forAll(genNbpState, Gen.choose(0.0, 1e10), Gen.choose(1e6, 1e12)) {
      (nbp: Nbp.State, bankBonds: Double, gdp: Double) =>
        val (_, purchase) = Nbp.executeQe(nbp, bankBonds, gdp)
        purchase should be <= (bankBonds + 1e-6)
    }
  }

  it should "not exceed max GDP share limit when active" in {
    forAll(Gen.choose(0.0, 0.25), Gen.choose(0.0, 1e10), Gen.choose(1e6, 1e12)) {
      (rate: Double, bankBonds: Double, gdp: Double) =>
        val nbp = Nbp.State(Rate(rate), PLN.Zero, qeActive = true, PLN.Zero)
        val (newNbp, _) = Nbp.executeQe(nbp, bankBonds, gdp)
        newNbp.govBondHoldings.toDouble should be <= (Config.NbpQeMaxGdpShare * gdp + 1e-6)
    }
  }

  it should "preserve bond clearing through QE (bank - purchase + nbp + purchase = total)" in {
    forAll(genNbpState, Gen.choose(0.0, 1e10), Gen.choose(1e6, 1e12)) {
      (nbp: Nbp.State, bankBonds: Double, gdp: Double) =>
        val totalBefore = bankBonds + nbp.govBondHoldings.toDouble
        val (newNbp, purchase) = Nbp.executeQe(nbp, bankBonds, gdp)
        val totalAfter = (bankBonds - purchase) + newNbp.govBondHoldings.toDouble
        totalAfter shouldBe (totalBefore +- 1.0) // FP tolerance at 1e10 scale
    }
  }

  // --- shouldActivateQe properties ---

  "Nbp.shouldActivateQe" should "imply rate near floor when true" in {
    forAll(genRate, genInflation) { (refRate: Double, inflation: Double) =>
      if Nbp.shouldActivateQe(refRate, inflation) then refRate should be <= (Config.RateFloor + 0.0025)
    }
  }

  // --- shouldTaperQe properties ---

  "Nbp.shouldTaperQe" should "be consistent with inflation vs target" in {
    forAll(genInflation) { (inflation: Double) =>
      val shouldTaper = Nbp.shouldTaperQe(inflation)
      if shouldTaper then inflation should be > Config.NbpTargetInfl
    }
  }
