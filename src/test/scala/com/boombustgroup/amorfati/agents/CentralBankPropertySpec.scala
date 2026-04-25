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
        val y = Nbp.bondYield(rateBD(refRate), shareBD(debtToGdp), shareBD(nbpBondGdpShare), plnBD(nfa), Rate.Zero)
        decimal(y) should be >= BigDecimal("0.0")
    }

  it should "cap fiscal risk premium at 10% even at extreme debtToGdp" in
    forAll(genRate, genDecimal("1.0", "100.0"), genDecimal("0.0", "0.50"), genDecimal("-1e10", "1e10")) {
      (refRate: BigDecimal, debtToGdp: BigDecimal, nbpBondGdpShare: BigDecimal, nfa: BigDecimal) =>
        val y = Nbp.bondYield(rateBD(refRate), shareBD(debtToGdp), shareBD(nbpBondGdpShare), plnBD(nfa), Rate.Zero)
        // Fiscal risk <= 0.10, so yield <= refRate + termPremium + 0.10
        decimal(y) should be <= (refRate + decimal(p.fiscal.govTermPremium) + BigDecimal("0.10") + BigDecimal("0.001"))
    }

  it should "be monotonic in debtToGdp (higher debt -> higher yield)" in
    forAll(genRate, genDecimal("0.0", "1.0"), genDecimal("0.0", "0.50"), genDecimal("-1e10", "1e10")) {
      (refRate: BigDecimal, baseDebt: BigDecimal, nbpBondGdpShare: BigDecimal, nfa: BigDecimal) =>
        val low  = Nbp.bondYield(rateBD(refRate), shareBD(baseDebt), shareBD(nbpBondGdpShare), plnBD(nfa), Rate.Zero)
        val high = Nbp.bondYield(rateBD(refRate), shareBD(baseDebt + BigDecimal("0.10")), shareBD(nbpBondGdpShare), plnBD(nfa), Rate.Zero)
        decimal(high) should be >= (decimal(low) - BigDecimal("1e-10"))
    }

  it should "be monotonically decreasing in nbpBondGdpShare (QE effect)" in
    forAll(genRate, genDecimal("0.0", "1.0"), genDecimal("0.0", "0.30"), genDecimal("-1e10", "1e10")) {
      (refRate: BigDecimal, debtToGdp: BigDecimal, baseQe: BigDecimal, nfa: BigDecimal) =>
        val low  = Nbp.bondYield(rateBD(refRate), shareBD(debtToGdp), shareBD(baseQe + BigDecimal("0.10")), plnBD(nfa), Rate.Zero)
        val high = Nbp.bondYield(rateBD(refRate), shareBD(debtToGdp), shareBD(baseQe), plnBD(nfa), Rate.Zero)
        decimal(high) should be >= (decimal(low) - BigDecimal("1e-10"))
    }

  // --- executeQe properties ---

  "Nbp.executeQe" should "always return purchase >= 0" in
    forAll(genNbpState, genDecimal("0.0", "1e10"), genDecimal("1e6", "1e12")) { (nbp: Nbp.State, bankBonds: BigDecimal, gdp: BigDecimal) =>
      val qeResult = Nbp.executeQe(nbp, zeroStocks, plnBD(bankBonds), plnBD(gdp), Rate.decimal(-2, 2), Rate.decimal(-1, 2))
      qeResult.requestedPurchase should be >= PLN.Zero
    }

  it should "not exceed bankBondHoldings" in
    forAll(genNbpState, genDecimal("0.0", "1e10"), genDecimal("1e6", "1e12")) { (nbp: Nbp.State, bankBonds: BigDecimal, gdp: BigDecimal) =>
      val qeResult = Nbp.executeQe(nbp, zeroStocks, plnBD(bankBonds), plnBD(gdp), Rate.decimal(-2, 2), Rate.decimal(-1, 2))
      decimal(qeResult.requestedPurchase) should be <= (bankBonds + BigDecimal("1.0"))
    }

  it should "not exceed max GDP share limit when active" in
    forAll(genDecimal("0.0", "0.25"), genDecimal("0.0", "1e10"), genDecimal("1e6", "1e12")) { (rate: BigDecimal, bankBonds: BigDecimal, gdp: BigDecimal) =>
      val nbp         = Nbp.State(rateBD(rate), true, PLN.Zero, PLN.Zero)
      val qeResult    = Nbp.executeQe(nbp, zeroStocks, plnBD(bankBonds), plnBD(gdp), Rate.decimal(-2, 2), Rate.decimal(-1, 2))
      // executeQe returns a request; bond update happens in BankingEconomics
      val maxHoldings = (plnBD(gdp) * p.monetary.qeMaxGdpShare).max(PLN.Zero)
      qeResult.requestedPurchase should be <= maxHoldings
    }

  it should "not modify NBP policy/QE state" in
    forAll(genNbpState, genDecimal("0.0", "1e10"), genDecimal("1e6", "1e12")) { (nbp: Nbp.State, bankBonds: BigDecimal, gdp: BigDecimal) =>
      val qeResult = Nbp.executeQe(nbp, zeroStocks, plnBD(bankBonds), plnBD(gdp), Rate.decimal(-2, 2), Rate.decimal(-1, 2))
      // executeQe returns a request; bond update happens in BankingEconomics waterfall
      qeResult.nbpState shouldBe nbp
      qeResult.requestedPurchase should be >= PLN.Zero
      decimal(qeResult.requestedPurchase) should be <= (bankBonds + BigDecimal("1.0"))
    }

  // --- shouldActivateQe properties ---

  "Nbp.shouldActivateQe" should "imply rate near floor when true" in
    forAll(genRate, genInflation, genInflation) { (refRate: BigDecimal, inflation: BigDecimal, expInflation: BigDecimal) =>
      if Nbp.shouldActivateQe(rateBD(refRate), rateBD(inflation), rateBD(expInflation)) then
        refRate should be <= (decimal(p.monetary.rateFloor) + BigDecimal("0.0026"))
    }

  // --- shouldTaperQe properties ---

  "Nbp.shouldTaperQe" should "be consistent with inflation vs target" in
    forAll(genInflation, genInflation) { (inflation: BigDecimal, expInflation: BigDecimal) =>
      val shouldTaper = Nbp.shouldTaperQe(rateBD(inflation), rateBD(expInflation))
      if shouldTaper then
        inflation should be > decimal(p.monetary.targetInfl)
        expInflation should be > decimal(p.monetary.targetInfl)
    }
