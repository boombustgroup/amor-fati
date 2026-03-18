package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class QuasiFiscalSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  private val govCapital = PLN(5e9)
  private val euCapital  = PLN(2e9)

  "QuasiFiscal.step" should "issue bonds proportional to gov capital spending" in {
    val result = QuasiFiscal.step(QuasiFiscal.State.zero, govCapital, euCapital, nbpQeActive = false)
    result.monthlyIssuance.toDouble should be > 0.0
    result.bondsOutstanding.toDouble should be > 0.0
  }

  it should "route NBP purchases when QE active" in {
    val withQe    = QuasiFiscal.step(QuasiFiscal.State.zero, govCapital, euCapital, nbpQeActive = true)
    val withoutQe = QuasiFiscal.step(QuasiFiscal.State.zero, govCapital, euCapital, nbpQeActive = false)
    withQe.nbpHoldings.toDouble should be > withoutQe.nbpHoldings.toDouble
  }

  it should "have zero NBP holdings when QE inactive" in {
    val result = QuasiFiscal.step(QuasiFiscal.State.zero, govCapital, euCapital, nbpQeActive = false)
    result.nbpHoldings shouldBe PLN.Zero
  }

  it should "grow loan portfolio with lending" in {
    val result = QuasiFiscal.step(QuasiFiscal.State.zero, govCapital, euCapital, nbpQeActive = false)
    result.loanPortfolio.toDouble should be > 0.0
  }

  it should "maintain bond clearing (outstanding = bank + nbp)" in {
    val result  = QuasiFiscal.step(QuasiFiscal.State.zero, govCapital, euCapital, nbpQeActive = true)
    val holders = result.bankHoldings.toDouble + result.nbpHoldings.toDouble
    holders shouldBe result.bondsOutstanding.toDouble +- 1.0
  }

  it should "amortize bonds over time" in {
    val m1 = QuasiFiscal.step(QuasiFiscal.State.zero, govCapital, euCapital, nbpQeActive = false)
    // Run with zero new issuance — bonds should decline
    val m2 = QuasiFiscal.step(m1, PLN.Zero, PLN.Zero, nbpQeActive = false)
    m2.bondsOutstanding.toDouble should be < m1.bondsOutstanding.toDouble
  }

  it should "compute ESA 2010 debt as MF + quasi-fiscal" in {
    val govDebt = PLN(100e9)
    val qfDebt  = PLN(20e9)
    val esa     = QuasiFiscal.esa2010Debt(govDebt, qfDebt)
    esa.toDouble shouldBe 120e9 +- 1.0
  }
