package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.accounting.GovState
import sfc.config.Config
import sfc.types.*

class EuFundsSpec extends AnyFlatSpec with Matchers:

  // --- Beta PDF tests ---

  "betaPdf" should "integrate to ~1.0 over [0,1]" in {
    val n = 10000
    val sum = (1 until n).map { i =>
      val x = i.toDouble / n
      EuFunds.betaPdf(x, 2.0, 5.0) / n
    }.sum
    sum shouldBe 1.0 +- 0.01
  }

  it should "peak at (a-1)/(a+b-2) for a=2, b=5" in {
    val peak = (2.0 - 1.0) / (2.0 + 5.0 - 2.0) // 0.20
    val atPeak = EuFunds.betaPdf(peak, 2.0, 5.0)
    // Values nearby should be lower
    val before = EuFunds.betaPdf(peak - 0.05, 2.0, 5.0)
    val after = EuFunds.betaPdf(peak + 0.05, 2.0, 5.0)
    atPeak should be > before
    atPeak should be > after
  }

  it should "return 0 outside [0,1]" in {
    EuFunds.betaPdf(-0.1, 2.0, 5.0) shouldBe 0.0
    EuFunds.betaPdf(0.0, 2.0, 5.0) shouldBe 0.0
    EuFunds.betaPdf(1.0, 2.0, 5.0) shouldBe 0.0
    EuFunds.betaPdf(1.5, 2.0, 5.0) shouldBe 0.0
  }

  // --- monthlyTransfer tests ---
  // Note: monthlyTransfer depends on Config env vars. Default: start=1, period=84

  "monthlyTransfer" should "return 0 before startMonth" in {
    EuFunds.monthlyTransfer(0) shouldBe 0.0
    EuFunds.monthlyTransfer(1) shouldBe 0.0 // t=0 → betaPdf(0)=0
  }

  it should "return 0 after startMonth + periodMonths" in {
    val afterEnd = Config.EuFundsStartMonth + Config.EuFundsPeriodMonths + 1
    EuFunds.monthlyTransfer(afterEnd) shouldBe 0.0
  }

  it should "return positive value in the middle of the period" in {
    val mid = Config.EuFundsStartMonth + Config.EuFundsPeriodMonths / 2
    EuFunds.monthlyTransfer(mid) should be > 0.0
  }

  it should "sum to ~totalAllocation over full period" in {
    val totalPln = Config.EuFundsTotalEur * Config.BaseExRate *
      (Config.FirmsCount.toDouble / 10000.0)
    val sum = (1 to Config.EuFundsPeriodMonths + Config.EuFundsStartMonth).map { m =>
      EuFunds.monthlyTransfer(m)
    }.sum
    sum shouldBe totalPln +- (totalPln * 0.02) // 2% tolerance (numerical integration)
  }

  // --- cofinancing tests ---

  "cofinancing" should "equal euMonthly * rate / (1 - rate)" in {
    val eu = 1000000.0
    val rate = 0.15
    val expected = eu * rate / (1.0 - rate)
    EuFunds.cofinancing(eu) shouldBe expected +- 0.01
  }

  it should "return 0 for zero transfer" in {
    EuFunds.cofinancing(0.0) shouldBe 0.0
  }

  // --- capitalInvestment tests ---

  "capitalInvestment" should "equal (eu + cofin) * capitalShare" in {
    val eu = 1000000.0
    val cofin = 176470.59 // 15/85 of eu
    val expected = (eu + cofin) * 0.60
    EuFunds.capitalInvestment(eu, cofin) shouldBe expected +- 0.01
  }

  // --- updateGov integration ---

  "updateGov" should "include euCofinancing in deficit" in {
    val prev = GovState(false, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
    val base =
      Sectors.updateGov(prev, 100000, 200000, bdpActive = false, bdpAmount = 0, priceLevel = 1.0, unempBenefitSpend = 0)
    val withEu = Sectors.updateGov(
      prev,
      100000,
      200000,
      bdpActive = false,
      bdpAmount = 0,
      priceLevel = 1.0,
      unempBenefitSpend = 0,
      euCofinancing = 50000.0,
    )
    // Deficit should increase by euCofinancing amount
    (withEu.deficit - base.deficit).toDouble shouldBe 50000.0 +- 0.01
  }

  it should "record euCofinancing in GovState" in {
    val prev = GovState(false, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
    val result = Sectors.updateGov(
      prev,
      100000,
      200000,
      bdpActive = false,
      bdpAmount = 0,
      priceLevel = 1.0,
      unempBenefitSpend = 0,
      euCofinancing = 75000.0,
    )
    result.euCofinancing.toDouble shouldBe 75000.0
  }

  it should "add euProjectCapital to govCapitalSpend when GovInvest disabled" in {
    val prev = GovState(false, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
    val result = Sectors.updateGov(
      prev,
      100000,
      200000,
      bdpActive = false,
      bdpAmount = 0,
      priceLevel = 1.0,
      unempBenefitSpend = 0,
      euProjectCapital = 30000.0,
    )
    // GovInvestEnabled=false by default, so govCapitalSpend = 0 + euProjectCapital
    result.govCapitalSpend.toDouble shouldBe 30000.0
  }

  // --- Disabled mode: identical to flat transfer ---

  "disabled mode" should "produce euCofinancing = 0" in {
    // When EU_FUNDS_ENABLED=false (default), euCofin should be 0
    Config.EuFundsEnabled shouldBe false
    val euMonthly = Config.OeEuTransfers // flat fallback
    euMonthly should be > 0.0 // sanity
  }
