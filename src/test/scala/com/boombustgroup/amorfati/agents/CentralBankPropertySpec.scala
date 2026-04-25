package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.Generators.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class CentralBankPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]
  private val zeroStocks   = Nbp.FinancialStocks(PLN.Zero, PLN.Zero)

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  // --- bondYield properties ---

  "Nbp.bondYield" should "be >= 0 for all inputs" in
    forAll(genRate, genDecimal("0.0", "2.0"), genDecimal("0.0", "0.50"), genDecimal("-1e10", "1e10")) {
      (refRate: BigDecimal, debtToGdp: BigDecimal, nbpBondGdpShare: BigDecimal, nfa: BigDecimal) =>
        val y = Nbp.bondYield(Rate(refRate), Share(debtToGdp), Share(nbpBondGdpShare), PLN(nfa), Rate.Zero)
        decimal(y) should be >= BigDecimal("0.0")
    }

  it should "cap fiscal risk premium at 10% even at extreme debtToGdp" in
    forAll(genRate, genDecimal("1.0", "100.0"), genDecimal("0.0", "0.50"), genDecimal("-1e10", "1e10")) {
      (refRate: BigDecimal, debtToGdp: BigDecimal, nbpBondGdpShare: BigDecimal, nfa: BigDecimal) =>
        val y = Nbp.bondYield(Rate(refRate), Share(debtToGdp), Share(nbpBondGdpShare), PLN(nfa), Rate.Zero)
        // Fiscal risk <= 0.10, so yield <= refRate + termPremium + 0.10
        decimal(y) should be <= (refRate + decimal(p.fiscal.govTermPremium) + BigDecimal("0.10") + BigDecimal("0.001"))
    }

  it should "be monotonic in debtToGdp (higher debt -> higher yield)" in
    forAll(genRate, genDecimal("0.0", "1.0"), genDecimal("0.0", "0.50"), genDecimal("-1e10", "1e10")) {
      (refRate: BigDecimal, baseDebt: BigDecimal, nbpBondGdpShare: BigDecimal, nfa: BigDecimal) =>
        val low  = Nbp.bondYield(Rate(refRate), Share(baseDebt), Share(nbpBondGdpShare), PLN(nfa), Rate.Zero)
        val high = Nbp.bondYield(Rate(refRate), Share(baseDebt + BigDecimal("0.10")), Share(nbpBondGdpShare), PLN(nfa), Rate.Zero)
        decimal(high) should be >= (decimal(low) - BigDecimal("1e-10"))
    }

  it should "be monotonically decreasing in nbpBondGdpShare (QE effect)" in
    forAll(genRate, genDecimal("0.0", "1.0"), genDecimal("0.0", "0.30"), genDecimal("-1e10", "1e10")) {
      (refRate: BigDecimal, debtToGdp: BigDecimal, baseQe: BigDecimal, nfa: BigDecimal) =>
        val low  = Nbp.bondYield(Rate(refRate), Share(debtToGdp), Share(baseQe + BigDecimal("0.10")), PLN(nfa), Rate.Zero)
        val high = Nbp.bondYield(Rate(refRate), Share(debtToGdp), Share(baseQe), PLN(nfa), Rate.Zero)
        decimal(high) should be >= (decimal(low) - BigDecimal("1e-10"))
    }

  // --- executeQe properties ---

  "Nbp.executeQe" should "always return purchase >= 0" in
    forAll(genNbpState, genDecimal("0.0", "1e10"), genDecimal("1e6", "1e12")) { (nbp: Nbp.State, bankBonds: BigDecimal, gdp: BigDecimal) =>
      val qeResult = Nbp.executeQe(nbp, zeroStocks, PLN(bankBonds), PLN(gdp), Rate("-0.02"), Rate("-0.01"))
      qeResult.requestedPurchase should be >= PLN.Zero
    }

  it should "not exceed bankBondHoldings" in
    forAll(genNbpState, genDecimal("0.0", "1e10"), genDecimal("1e6", "1e12")) { (nbp: Nbp.State, bankBonds: BigDecimal, gdp: BigDecimal) =>
      val qeResult = Nbp.executeQe(nbp, zeroStocks, PLN(bankBonds), PLN(gdp), Rate("-0.02"), Rate("-0.01"))
      decimal(qeResult.requestedPurchase) should be <= (bankBonds + BigDecimal("1.0"))
    }

  it should "not exceed max GDP share limit when active" in
    forAll(genDecimal("0.0", "0.25"), genDecimal("0.0", "1e10"), genDecimal("1e6", "1e12")) { (rate: BigDecimal, bankBonds: BigDecimal, gdp: BigDecimal) =>
      val nbp         = Nbp.State(Rate(rate), true, PLN.Zero, PLN.Zero)
      val qeResult    = Nbp.executeQe(nbp, zeroStocks, PLN(bankBonds), PLN(gdp), Rate("-0.02"), Rate("-0.01"))
      // executeQe returns a request; bond update happens in BankingEconomics
      val maxHoldings = (PLN(gdp) * p.monetary.qeMaxGdpShare).max(PLN.Zero)
      qeResult.requestedPurchase should be <= maxHoldings
    }

  it should "not modify NBP policy/QE state" in
    forAll(genNbpState, genDecimal("0.0", "1e10"), genDecimal("1e6", "1e12")) { (nbp: Nbp.State, bankBonds: BigDecimal, gdp: BigDecimal) =>
      val qeResult = Nbp.executeQe(nbp, zeroStocks, PLN(bankBonds), PLN(gdp), Rate("-0.02"), Rate("-0.01"))
      // executeQe returns a request; bond update happens in BankingEconomics waterfall
      qeResult.nbpState shouldBe nbp
      qeResult.requestedPurchase should be >= PLN.Zero
      decimal(qeResult.requestedPurchase) should be <= (bankBonds + BigDecimal("1.0"))
    }

  // --- shouldActivateQe properties ---

  "Nbp.shouldActivateQe" should "imply rate near floor when true" in
    forAll(genRate, genInflation, genInflation) { (refRate: BigDecimal, inflation: BigDecimal, expInflation: BigDecimal) =>
      if Nbp.shouldActivateQe(Rate(refRate), Rate(inflation), Rate(expInflation)) then
        refRate should be <= (decimal(p.monetary.rateFloor) + BigDecimal("0.0026"))
    }

  // --- shouldTaperQe properties ---

  "Nbp.shouldTaperQe" should "be consistent with inflation vs target" in
    forAll(genInflation, genInflation) { (inflation: BigDecimal, expInflation: BigDecimal) =>
      val shouldTaper = Nbp.shouldTaperQe(Rate(inflation), Rate(expInflation))
      if shouldTaper then
        inflation should be > decimal(p.monetary.targetInfl)
        expInflation should be > decimal(p.monetary.targetInfl)
    }
