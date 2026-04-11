package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.markets.FiscalBudget
import com.boombustgroup.amorfati.engine.mechanisms.EuFunds
import com.boombustgroup.amorfati.math.EuFundsMath
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class EuFundsSpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]
  private val td           = ComputationBoundary

  // --- monthlyTransfer tests ---
  // Note: monthlyTransfer depends on Config env vars. Default: start=1, period=84

  "monthlyTransfer" should "return 0 before startMonth when the configured start month is after month 1" in {
    if p.fiscal.euFundsStartMonth > 1 then EuFunds.monthlyTransfer(ExecutionMonth(p.fiscal.euFundsStartMonth - 1)).shouldBe(PLN.Zero)
    else succeed
  }

  it should "return positive value at startMonth" in
    EuFunds.monthlyTransfer(ExecutionMonth(p.fiscal.euFundsStartMonth)).should(be > PLN.Zero)

  it should "return 0 after startMonth + periodMonths" in {
    val afterEnd = p.fiscal.euFundsStartMonth + p.fiscal.euFundsPeriodMonths + 1
    EuFunds.monthlyTransfer(ExecutionMonth(afterEnd)).shouldBe(PLN.Zero)
  }

  it should "return positive value in the middle of the period" in {
    val mid = p.fiscal.euFundsStartMonth + p.fiscal.euFundsPeriodMonths / 2
    td.toDouble(EuFunds.monthlyTransfer(ExecutionMonth(mid))).should(be > 0.0)
  }

  it should "sum to ~totalAllocation over full period" in {
    val totalPln = EuFundsMath.totalEnvelopePln(p.fiscal.euFundsTotalEur, p.forex.baseExRate, p.pop.firmsCount, 10000)
    val sum      = (1 to p.fiscal.euFundsPeriodMonths + p.fiscal.euFundsStartMonth).map { m =>
      EuFunds.monthlyTransfer(ExecutionMonth(m))
    }.sum
    td.toDouble(sum).shouldBe(td.toDouble(totalPln) +- (td.toDouble(totalPln) * 0.02))
  }

  // --- cofinancing tests ---

  "cofinancing" should "equal euMonthly * rate / (1 - rate)" in {
    val eu       = PLN(1000000.0)
    val rate     = 0.15
    val expected = 1000000.0 * rate / (1.0 - rate)
    td.toDouble(EuFunds.cofinancing(eu)).shouldBe(expected +- 0.01)
  }

  it should "return 0 for zero transfer" in
    EuFunds.cofinancing(PLN.Zero).shouldBe(PLN.Zero)

  // --- capitalInvestment tests ---

  "capitalInvestment" should "equal (eu + cofin) * capitalShare" in {
    val eu       = PLN(1000000.0)
    val cofin    = PLN(176470.59)
    val expected = (1000000.0 + 176470.59) * 0.60
    td.toDouble(EuFunds.capitalInvestment(eu, cofin)).shouldBe(expected +- 0.01)
  }

  // --- updateGov integration ---

  "updateGov" should "include euCofinancing in deficit" in {
    val prev      = FiscalBudget.GovState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
    val baseInput = FiscalBudget.Input(prev, priceLevel = PriceIndex.Base, citPaid = PLN(100000), vat = PLN(200000))
    val base      = FiscalBudget.update(baseInput)
    val withEu    = FiscalBudget.update(baseInput.copy(euCofinancing = PLN(50000.0)))
    // Deficit should increase by euCofinancing amount
    td.toDouble(withEu.deficit - base.deficit) shouldBe 50000.0 +- 0.01
  }

  it should "record euCofinancing in GovState" in {
    val prev   = FiscalBudget.GovState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
    val result = FiscalBudget.update(
      FiscalBudget.Input(
        prev,
        priceLevel = PriceIndex.Base,
        citPaid = PLN(100000),
        vat = PLN(200000),
        euCofinancing = PLN(75000.0),
      ),
    )
    td.toDouble(result.euCofinancing) shouldBe 75000.0
  }

  it should "track euProjectCapital separately from budget-financed govCapitalSpend" in {
    val prev   = FiscalBudget.GovState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
    val base   = FiscalBudget.update(
      FiscalBudget.Input(
        prev,
        priceLevel = PriceIndex.Base,
        citPaid = PLN(100000),
        vat = PLN(200000),
      ),
    )
    val result = FiscalBudget.update(
      FiscalBudget.Input(
        prev,
        priceLevel = PriceIndex.Base,
        citPaid = PLN(100000),
        vat = PLN(200000),
        euProjectCapital = PLN(30000.0),
      ),
    )
    td.toDouble(result.govCapitalSpend) shouldBe td.toDouble(base.govCapitalSpend) +- 0.01
    td.toDouble(result.euProjectCapital) shouldBe 30000.0 +- 0.01
  }
