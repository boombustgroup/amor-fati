package com.boombustgroup.amorfati.agents

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import com.boombustgroup.amorfati.Generators.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.fp.ComputationBoundary
import com.boombustgroup.amorfati.types.*

class CentralBankPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]
  private val td           = ComputationBoundary

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  // --- bondYield properties ---

  "Nbp.bondYield" should "be >= 0 for all inputs" in
    forAll(genRate, Gen.choose(0.0, 2.0), Gen.choose(0.0, 0.50), Gen.choose(-1e10, 1e10)) {
      (refRate: Double, debtToGdp: Double, nbpBondGdpShare: Double, nfa: Double) =>
        val y = Nbp.bondYield(Rate(refRate), Share(debtToGdp), Share(nbpBondGdpShare), PLN(nfa), Rate.Zero)
        td.toDouble(y) should be >= 0.0
    }

  it should "cap fiscal risk premium at 10% even at extreme debtToGdp" in
    forAll(genRate, Gen.choose(1.0, 100.0), Gen.choose(0.0, 0.50), Gen.choose(-1e10, 1e10)) {
      (refRate: Double, debtToGdp: Double, nbpBondGdpShare: Double, nfa: Double) =>
        val y = Nbp.bondYield(Rate(refRate), Share(debtToGdp), Share(nbpBondGdpShare), PLN(nfa), Rate.Zero)
        // Fiscal risk <= 0.10, so yield <= refRate + termPremium + 0.10
        td.toDouble(y) should be <= (refRate + td.toDouble(p.fiscal.govTermPremium) + 0.10 + 0.001)
    }

  it should "be monotonic in debtToGdp (higher debt -> higher yield)" in
    forAll(genRate, Gen.choose(0.0, 1.0), Gen.choose(0.0, 0.50), Gen.choose(-1e10, 1e10)) {
      (refRate: Double, baseDebt: Double, nbpBondGdpShare: Double, nfa: Double) =>
        val low  = Nbp.bondYield(Rate(refRate), Share(baseDebt), Share(nbpBondGdpShare), PLN(nfa), Rate.Zero)
        val high = Nbp.bondYield(Rate(refRate), Share(baseDebt + 0.10), Share(nbpBondGdpShare), PLN(nfa), Rate.Zero)
        td.toDouble(high) should be >= (td.toDouble(low) - 1e-10)
    }

  it should "be monotonically decreasing in nbpBondGdpShare (QE effect)" in
    forAll(genRate, Gen.choose(0.0, 1.0), Gen.choose(0.0, 0.30), Gen.choose(-1e10, 1e10)) { (refRate: Double, debtToGdp: Double, baseQe: Double, nfa: Double) =>
      val low  = Nbp.bondYield(Rate(refRate), Share(debtToGdp), Share(baseQe + 0.10), PLN(nfa), Rate.Zero)
      val high = Nbp.bondYield(Rate(refRate), Share(debtToGdp), Share(baseQe), PLN(nfa), Rate.Zero)
      td.toDouble(high) should be >= (td.toDouble(low) - 1e-10)
    }

  // --- executeQe properties ---

  "Nbp.executeQe" should "always return purchase >= 0" in
    forAll(genNbpState, Gen.choose(0.0, 1e10), Gen.choose(1e6, 1e12)) { (nbp: Nbp.State, bankBonds: Double, gdp: Double) =>
      val qeResult = Nbp.executeQe(nbp, PLN(bankBonds), PLN(gdp), Rate(-0.02), Rate(-0.01))
      qeResult.requestedPurchase should be >= PLN.Zero
    }

  it should "not exceed bankBondHoldings" in
    forAll(genNbpState, Gen.choose(0.0, 1e10), Gen.choose(1e6, 1e12)) { (nbp: Nbp.State, bankBonds: Double, gdp: Double) =>
      val qeResult = Nbp.executeQe(nbp, PLN(bankBonds), PLN(gdp), Rate(-0.02), Rate(-0.01))
      td.toDouble(qeResult.requestedPurchase) should be <= (bankBonds + 1.0)
    }

  it should "not exceed max GDP share limit when active" in
    forAll(Gen.choose(0.0, 0.25), Gen.choose(0.0, 1e10), Gen.choose(1e6, 1e12)) { (rate: Double, bankBonds: Double, gdp: Double) =>
      val nbp      = Nbp.State(Rate(rate), PLN.Zero, true, PLN.Zero, PLN.Zero, PLN.Zero)
      val qeResult = Nbp.executeQe(nbp, PLN(bankBonds), PLN(gdp), Rate(-0.02), Rate(-0.01))
      // executeQe returns a request; bond update happens in BankingEconomics
      td.toDouble(qeResult.requestedPurchase + nbp.govBondHoldings) should be <= (td.toDouble(p.monetary.qeMaxGdpShare) * gdp + 1e-6)
    }

  it should "not modify nbpState.govBondHoldings (deferred to BankingEconomics)" in
    forAll(genNbpState, Gen.choose(0.0, 1e10), Gen.choose(1e6, 1e12)) { (nbp: Nbp.State, bankBonds: Double, gdp: Double) =>
      val qeResult = Nbp.executeQe(nbp, PLN(bankBonds), PLN(gdp), Rate(-0.02), Rate(-0.01))
      // executeQe returns a request; bond update happens in BankingEconomics waterfall
      qeResult.nbpState.govBondHoldings shouldBe nbp.govBondHoldings
      qeResult.requestedPurchase should be >= PLN.Zero
      td.toDouble(qeResult.requestedPurchase) should be <= (bankBonds + 1.0)
    }

  // --- shouldActivateQe properties ---

  "Nbp.shouldActivateQe" should "imply rate near floor when true" in
    forAll(genRate, genInflation, genInflation) { (refRate: Double, inflation: Double, expInflation: Double) =>
      if Nbp.shouldActivateQe(Rate(refRate), Rate(inflation), Rate(expInflation)) then
        refRate should be <= (td.toDouble(p.monetary.rateFloor) + 0.0026)
    }

  // --- shouldTaperQe properties ---

  "Nbp.shouldTaperQe" should "be consistent with inflation vs target" in
    forAll(genInflation, genInflation) { (inflation: Double, expInflation: Double) =>
      val shouldTaper = Nbp.shouldTaperQe(Rate(inflation), Rate(expInflation))
      if shouldTaper then
        inflation should be > td.toDouble(p.monetary.targetInfl)
        expInflation should be > td.toDouble(p.monetary.targetInfl)
    }
