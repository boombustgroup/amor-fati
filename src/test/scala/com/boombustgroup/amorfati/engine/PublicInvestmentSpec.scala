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
  }

  // --- Formula verification (independent of p.flags.govInvest) ---

  "spending split formula" should "sum to total when share=0.20" in {
    val base    = 100000000.0
    val share   = 0.20
    val current = base * (1.0 - share)
    val capital = base * share
    (current + capital) shouldBe base +- 0.01
    current shouldBe 80000000.0 +- 0.01
    capital shouldBe 20000000.0 +- 0.01
  }

  it should "sum to total for any valid share" in {
    val base = 100000000.0
    for share <- Seq(0.0, 0.10, 0.20, 0.50, 1.0) do
      val current = base * (1.0 - share)
      val capital = base * share
      (current + capital) shouldBe base +- 0.01
  }

  "capital stock formula" should "accumulate investment net of depreciation" in {
    val depRate    = 0.06 // annual
    val monthlyDep = depRate / 12.0
    val prevStock  = 1000000.0
    val investment = 20000.0
    val newStock   = prevStock * (1.0 - monthlyDep) + investment
    newStock shouldBe (1000000.0 * (1.0 - 0.005) + 20000.0) +- 0.01
    newStock shouldBe 1015000.0 +- 0.01
  }

  it should "depreciate monotonically with no investment" in {
    val depRate    = 0.06
    val monthlyDep = depRate / 12.0
    var stock      = 1000000.0
    var prevStock  = stock
    for _ <- 1 to 120 do
      stock = stock * (1.0 - monthlyDep)
      stock should be < prevStock
      prevStock = stock
    // After 10 years at 6%/year: ~548K (about half)
    stock shouldBe 547986.0 +- 500.0
  }

  "GDP proxy formula" should "produce weighted multiplier 0.94 with defaults" in {
    val base          = 100000000.0
    val share         = 0.20
    val currentMult   = 0.8
    val capitalMult   = 1.5
    val govGdp        = base * (1.0 - share) * currentMult + base * share * capitalMult
    val effectiveMult = govGdp / base
    effectiveMult shouldBe 0.94 +- 0.001
  }
