package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class QuasiFiscalSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  private val govCapital = PLN(5000000000L)
  private val euCapital  = PLN(2000000000)

  private def step(
      stock: QuasiFiscal.StockState = QuasiFiscal.StockState.zero,
      govCapitalSpend: PLN = govCapital,
      euProjectCapital: PLN = euCapital,
      nbpQeActive: Boolean = false,
  ): QuasiFiscal.StepResult =
    QuasiFiscal.step(stock, govCapitalSpend, euProjectCapital, nbpQeActive)

  "QuasiFiscal.step" should "issue bonds proportional to gov capital spending" in {
    val result = step()
    decimal(result.state.monthlyIssuance) should be > BigDecimal("0.0")
    decimal(result.stock.bondsOutstanding) should be > BigDecimal("0.0")
  }

  it should "route NBP purchases when QE active" in {
    val withQe    = step(nbpQeActive = true)
    val withoutQe = step(nbpQeActive = false)
    decimal(withQe.stock.nbpHoldings) should be > decimal(withoutQe.stock.nbpHoldings)
    withQe.state.monthlyBankBondIssuance + withQe.state.monthlyNbpBondAbsorption shouldBe withQe.state.monthlyIssuance
  }

  it should "have zero NBP holdings when QE inactive" in {
    val result = step(nbpQeActive = false)
    result.stock.nbpHoldings shouldBe PLN.Zero
  }

  it should "grow loan portfolio with lending" in {
    val result = step(nbpQeActive = false)
    decimal(result.stock.loanPortfolio) should be > BigDecimal("0.0")
  }

  it should "maintain bond clearing (outstanding = bank + nbp)" in {
    val result  = step(nbpQeActive = true)
    val holders = decimal(result.stock.bankHoldings) + decimal(result.stock.nbpHoldings)
    holders shouldBe decimal(result.stock.bondsOutstanding) +- BigDecimal("1.0")
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
    decimal(m2.stock.bondsOutstanding) should be < decimal(m1.stock.bondsOutstanding)
    m2.state.monthlyBankBondAmortization + m2.state.monthlyNbpBondAmortization shouldBe m2.state.monthlyBondAmortization
  }

  it should "compute ESA 2010 debt as MF + quasi-fiscal" in {
    val govDebt = PLN(100000000000L)
    val qfDebt  = PLN(20000000000L)
    val esa     = QuasiFiscal.esa2010Debt(govDebt, qfDebt)
    decimal(esa) shouldBe BigDecimal("120e9") +- BigDecimal("1.0")
  }
