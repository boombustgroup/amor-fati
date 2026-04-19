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

  private def step(
      stock: QuasiFiscal.StockState = QuasiFiscal.StockState.zero,
      govCapitalSpend: PLN = govCapital,
      euProjectCapital: PLN = euCapital,
      nbpQeActive: Boolean = false,
  ): QuasiFiscal.StepResult =
    QuasiFiscal.step(stock, govCapitalSpend, euProjectCapital, nbpQeActive)

  "QuasiFiscal.step" should "issue bonds proportional to gov capital spending" in {
    val result = step()
    td.toDouble(result.state.monthlyIssuance) should be > 0.0
    td.toDouble(result.stock.bondsOutstanding) should be > 0.0
  }

  it should "route NBP purchases when QE active" in {
    val withQe    = step(nbpQeActive = true)
    val withoutQe = step(nbpQeActive = false)
    td.toDouble(withQe.stock.nbpHoldings) should be > td.toDouble(withoutQe.stock.nbpHoldings)
  }

  it should "have zero NBP holdings when QE inactive" in {
    val result = step(nbpQeActive = false)
    result.stock.nbpHoldings shouldBe PLN.Zero
  }

  it should "grow loan portfolio with lending" in {
    val result = step(nbpQeActive = false)
    td.toDouble(result.stock.loanPortfolio) should be > 0.0
  }

  it should "maintain bond clearing (outstanding = bank + nbp)" in {
    val result  = step(nbpQeActive = true)
    val holders = td.toDouble(result.stock.bankHoldings) + td.toDouble(result.stock.nbpHoldings)
    holders shouldBe td.toDouble(result.stock.bondsOutstanding) +- 1.0
  }

  it should "amortize bonds over time" in {
    val m1 = step(nbpQeActive = false)
    // Run with zero new issuance — bonds should decline
    val m2 = step(
      stock = m1.stock,
      govCapitalSpend = PLN.Zero,
      euProjectCapital = PLN.Zero,
      nbpQeActive = false,
    )
    td.toDouble(m2.stock.bondsOutstanding) should be < td.toDouble(m1.stock.bondsOutstanding)
  }

  it should "compute ESA 2010 debt as MF + quasi-fiscal" in {
    val govDebt = PLN(100e9)
    val qfDebt  = PLN(20e9)
    val esa     = QuasiFiscal.esa2010Debt(govDebt, qfDebt)
    td.toDouble(esa) shouldBe 120e9 +- 1.0
  }
