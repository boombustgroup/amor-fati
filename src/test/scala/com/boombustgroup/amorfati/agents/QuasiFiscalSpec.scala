package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class QuasiFiscalSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults
  private val td  = ComputationBoundary

  private val govCapital = PLN(5e9)
  private val euCapital  = PLN(2e9)

  "QuasiFiscal.step" should "issue bonds proportional to gov capital spending" in {
    val result = QuasiFiscal.step(QuasiFiscal.State.zero, govCapital, euCapital, nbpQeActive = false)
    td.toDouble(result.monthlyIssuance) should be > 0.0
    td.toDouble(result.bondsOutstanding) should be > 0.0
  }

  it should "route NBP purchases when QE active" in {
    val withQe    = QuasiFiscal.step(QuasiFiscal.State.zero, govCapital, euCapital, nbpQeActive = true)
    val withoutQe = QuasiFiscal.step(QuasiFiscal.State.zero, govCapital, euCapital, nbpQeActive = false)
    td.toDouble(withQe.nbpHoldings) should be > td.toDouble(withoutQe.nbpHoldings)
  }

  it should "have zero NBP holdings when QE inactive" in {
    val result = QuasiFiscal.step(QuasiFiscal.State.zero, govCapital, euCapital, nbpQeActive = false)
    result.nbpHoldings shouldBe PLN.Zero
  }

  it should "grow loan portfolio with lending" in {
    val result = QuasiFiscal.step(QuasiFiscal.State.zero, govCapital, euCapital, nbpQeActive = false)
    td.toDouble(result.loanPortfolio) should be > 0.0
  }

  it should "maintain bond clearing (outstanding = bank + nbp)" in {
    val result  = QuasiFiscal.step(QuasiFiscal.State.zero, govCapital, euCapital, nbpQeActive = true)
    val holders = td.toDouble(result.bankHoldings) + td.toDouble(result.nbpHoldings)
    holders shouldBe td.toDouble(result.bondsOutstanding) +- 1.0
  }

  it should "amortize bonds over time" in {
    val m1 = QuasiFiscal.step(QuasiFiscal.State.zero, govCapital, euCapital, nbpQeActive = false)
    // Run with zero new issuance — bonds should decline
    val m2 = QuasiFiscal.step(m1, PLN.Zero, PLN.Zero, nbpQeActive = false)
    td.toDouble(m2.bondsOutstanding) should be < td.toDouble(m1.bondsOutstanding)
  }

  it should "compute ESA 2010 debt as MF + quasi-fiscal" in {
    val govDebt = PLN(100e9)
    val qfDebt  = PLN(20e9)
    val esa     = QuasiFiscal.esa2010Debt(govDebt, qfDebt)
    td.toDouble(esa) shouldBe 120e9 +- 1.0
  }
