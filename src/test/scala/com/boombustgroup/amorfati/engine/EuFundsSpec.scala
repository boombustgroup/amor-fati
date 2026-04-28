package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.markets.FiscalBudget
import com.boombustgroup.amorfati.engine.mechanisms.EuFunds
import com.boombustgroup.amorfati.math.EuFundsMath
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EuFundsSpec extends AnyFlatSpec with Matchers:

  given SimParams                          = SimParams.defaults
  private val p: SimParams                 = summon[SimParams]
  private def referenceEnvelopePln: PLN    =
    EuFundsMath.totalEnvelopePln(p.fiscal.euFundsTotalEur, p.forex.baseExRate, p.pop.firmsCount, 10000)
  private def runtimeEnvelopePln: PLN      = referenceEnvelopePln * p.gdpRatio
  private def polandScale(value: PLN): PLN = value / p.gdpRatio.toMultiplier

  // --- monthlyTransfer tests ---
  // Note: monthlyTransfer depends on Config env vars. Default: start=1, period=84

  "monthlyTransfer" should "return 0 before startMonth when the configured start month is after month 1" in {
    if p.fiscal.euFundsStartMonth > 1 then EuFunds.monthlyTransfer(ExecutionMonth(p.fiscal.euFundsStartMonth - 1)).shouldBe(PLN.Zero)
    else succeed
  }

  it should "return positive value at startMonth" in
    EuFunds.monthlyTransfer(ExecutionMonth(p.fiscal.euFundsStartMonth)).should(be > PLN.Zero)

  it should "scale the default total envelope without overflowing" in {
    val totalPln = referenceEnvelopePln

    totalPln should be > PLN.Zero
    decimal(totalPln) shouldBe BigDecimal("329080000000.0") +- BigDecimal("1.0")
  }

  it should "never produce negative transfers during the configured absorption period" in {
    val transfers = (p.fiscal.euFundsStartMonth until p.fiscal.euFundsStartMonth + p.fiscal.euFundsPeriodMonths)
      .map(month => EuFunds.monthlyTransfer(ExecutionMonth(month)))

    transfers.foreach(_ should be >= PLN.Zero)
    transfers.exists(_ > PLN.Zero) shouldBe true
  }

  it should "return 0 after startMonth + periodMonths" in {
    val afterEnd = p.fiscal.euFundsStartMonth + p.fiscal.euFundsPeriodMonths + 1
    EuFunds.monthlyTransfer(ExecutionMonth(afterEnd)).shouldBe(PLN.Zero)
  }

  it should "return positive value in the middle of the period" in {
    val mid = p.fiscal.euFundsStartMonth + p.fiscal.euFundsPeriodMonths / 2
    decimal(EuFunds.monthlyTransfer(ExecutionMonth(mid))).should(be > BigDecimal("0.0"))
  }

  it should "sum to the gdpRatio-scaled runtime allocation over the full period" in {
    val expected = runtimeEnvelopePln
    val sum      = (1 to p.fiscal.euFundsPeriodMonths + p.fiscal.euFundsStartMonth).map { m =>
      EuFunds.monthlyTransfer(ExecutionMonth(m))
    }.sumPln
    decimal(sum).shouldBe(decimal(expected) +- (decimal(expected) * BigDecimal("0.02")))
  }

  it should "recover the Poland-scale envelope when runtime transfers are exported" in {
    val sum = (1 to p.fiscal.euFundsPeriodMonths + p.fiscal.euFundsStartMonth).map { m =>
      polandScale(EuFunds.monthlyTransfer(ExecutionMonth(m)))
    }.sumPln

    decimal(sum).shouldBe(decimal(referenceEnvelopePln) +- (decimal(referenceEnvelopePln) * BigDecimal("0.02")))
  }

  // --- cofinancing tests ---

  "cofinancing" should "equal euMonthly * rate / (1 - rate)" in {
    val eu       = PLN(1000000)
    val rate     = BigDecimal("0.15")
    val expected = BigDecimal("1000000.0") * rate / (BigDecimal("1.0") - rate)
    decimal(EuFunds.cofinancing(eu)).shouldBe(expected +- BigDecimal("0.01"))
  }

  it should "return 0 for zero transfer" in
    EuFunds.cofinancing(PLN.Zero).shouldBe(PLN.Zero)

  // --- capitalInvestment tests ---

  "capitalInvestment" should "equal (eu + cofin) * capitalShare" in {
    val eu       = PLN(1000000)
    val cofin    = PLN.decimal(17647059, 2)
    val expected = (BigDecimal("1000000.0") + BigDecimal("176470.59")) * BigDecimal("0.60")
    decimal(EuFunds.capitalInvestment(eu, cofin)).shouldBe(expected +- BigDecimal("0.01"))
  }

  // --- updateGov integration ---

  "updateGov" should "include euCofinancing in deficit" in {
    val prev      = FiscalBudget.GovState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
    val baseInput = FiscalBudget.Input(prev, priceLevel = PriceIndex.Base, citPaid = PLN(100000), govDividendRevenue = PLN.Zero, vat = PLN(200000))
    val base      = FiscalBudget.update(baseInput)
    val withEu    = FiscalBudget.update(baseInput.copy(euCofinancing = PLN(50000)))
    // Deficit should increase by euCofinancing amount
    decimal(withEu.deficit - base.deficit) shouldBe BigDecimal("50000.0") +- BigDecimal("0.01")
  }

  it should "record euCofinancing in GovState" in {
    val prev   = FiscalBudget.GovState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
    val result = FiscalBudget.update(
      FiscalBudget.Input(
        prev,
        priceLevel = PriceIndex.Base,
        citPaid = PLN(100000),
        govDividendRevenue = PLN.Zero,
        vat = PLN(200000),
        euCofinancing = PLN(75000),
      ),
    )
    decimal(result.euCofinancing) shouldBe BigDecimal("75000.0")
  }

  it should "track euProjectCapital separately from budget-financed govCapitalSpend" in {
    val prev   = FiscalBudget.GovState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
    val base   = FiscalBudget.update(
      FiscalBudget.Input(
        prev,
        priceLevel = PriceIndex.Base,
        citPaid = PLN(100000),
        govDividendRevenue = PLN.Zero,
        vat = PLN(200000),
      ),
    )
    val result = FiscalBudget.update(
      FiscalBudget.Input(
        prev,
        priceLevel = PriceIndex.Base,
        citPaid = PLN(100000),
        govDividendRevenue = PLN.Zero,
        vat = PLN(200000),
        euProjectCapital = PLN(30000),
      ),
    )
    decimal(result.govCapitalSpend) shouldBe decimal(base.govCapitalSpend) +- BigDecimal("0.01")
    decimal(result.euProjectCapital) shouldBe BigDecimal("30000.0") +- BigDecimal("0.01")
  }
