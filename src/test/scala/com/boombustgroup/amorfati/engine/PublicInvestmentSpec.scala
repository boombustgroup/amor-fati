package com.boombustgroup.amorfati.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.engine.markets.FiscalBudget
import com.boombustgroup.amorfati.types.*

class PublicInvestmentSpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams = SimParams.defaults

  "GovState" should "have new fields default to 0" in {
    val g = FiscalBudget.GovState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
    g.publicCapitalStock shouldBe PLN.Zero
    g.govCurrentSpend shouldBe PLN.Zero
    g.govCapitalSpend shouldBe PLN.Zero
    g.euProjectCapital shouldBe PLN.Zero
  }

  // --- Formula verification ---

  "spending split formula" should "sum to total when share=0.20" in {
    val base    = BigDecimal("100000000.0")
    val share   = BigDecimal("0.20")
    val current = base * (BigDecimal("1.0") - share)
    val capital = base * share
    (current + capital) shouldBe base +- BigDecimal("0.01")
    current shouldBe BigDecimal("80000000.0") +- BigDecimal("0.01")
    capital shouldBe BigDecimal("20000000.0") +- BigDecimal("0.01")
  }

  it should "sum to total for any valid share" in {
    val base = BigDecimal("100000000.0")
    for share <- Seq(BigDecimal("0.0"), BigDecimal("0.10"), BigDecimal("0.20"), BigDecimal("0.50"), BigDecimal("1.0")) do
      val current = base * (BigDecimal("1.0") - share)
      val capital = base * share
      (current + capital) shouldBe base +- BigDecimal("0.01")
  }

  "capital stock formula" should "accumulate investment net of depreciation" in {
    val depRate    = BigDecimal("0.06") // annual
    val monthlyDep = depRate / BigDecimal("12.0")
    val prevStock  = BigDecimal("1000000.0")
    val investment = BigDecimal("20000.0")
    val newStock   = prevStock * (BigDecimal("1.0") - monthlyDep) + investment
    newStock shouldBe (BigDecimal("1000000.0") * (BigDecimal("1.0") - BigDecimal("0.005")) + BigDecimal("20000.0")) +- BigDecimal("0.01")
    newStock shouldBe BigDecimal("1015000.0") +- BigDecimal("0.01")
  }

  it should "depreciate monotonically with no investment" in {
    val depRate    = BigDecimal("0.06")
    val monthlyDep = depRate / BigDecimal("12.0")
    var stock      = BigDecimal("1000000.0")
    var prevStock  = stock
    for _ <- 1 to 120 do
      stock = stock * (BigDecimal("1.0") - monthlyDep)
      stock should be < prevStock
      prevStock = stock
    // After 10 years at 6%/year: ~548K (about half)
    stock shouldBe BigDecimal("547986.0") +- BigDecimal("500.0")
  }

  "GDP proxy formula" should "produce weighted multiplier 0.94 with defaults" in {
    val base          = BigDecimal("100000000.0")
    val share         = BigDecimal("0.20")
    val currentMult   = BigDecimal("0.8")
    val capitalMult   = BigDecimal("1.5")
    val govGdp        = base * (BigDecimal("1.0") - share) * currentMult + base * share * capitalMult
    val effectiveMult = govGdp / base
    effectiveMult shouldBe BigDecimal("0.94") +- BigDecimal("0.001")
  }
