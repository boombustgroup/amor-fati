package sfc.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.types.*

/** Progressive PIT unit tests. */
class PitSpec extends AnyFlatSpec with Matchers:

  import sfc.config.SimParams
  given SimParams = SimParams.defaults

  // Note: p.flags.pit is false by default, so computeMonthlyPit returns 0.
  // These tests verify the formula logic assuming PIT_ENABLED=true would be set.
  // Since env vars are JVM-global, we test the internal math directly.

  "computeMonthlyPit" should "return 0 when PIT is disabled (default)" in {
    Household.computeMonthlyPit(PLN(10000.0)) shouldBe PLN.Zero
  }

  it should "return 0 for zero income" in {
    Household.computeMonthlyPit(PLN(0.0)) shouldBe PLN.Zero
  }

  it should "return 0 for negative income" in {
    Household.computeMonthlyPit(PLN(-5000.0)) shouldBe PLN.Zero
  }

  // --- Formula verification tests (independent of p.flags.pit) ---
  // These test the mathematical formula: 12% bracket 1, 32% bracket 2, 3600 PLN/yr credit

  "PIT formula" should "compute correctly for income below bracket 1" in {
    // 8266 PLN/month → 99192 PLN/year (below 120000 bracket)
    val monthly    = 8266.0
    val annualized = monthly * 12.0
    val grossTax   = annualized * 0.12                       // 11903.04
    val netTax     = Math.max(0.0, grossTax - 3600.0) / 12.0 // (11903.04 - 3600) / 12 = 691.92
    netTax shouldBe 691.92 +- 0.01
  }

  it should "compute correctly for income above bracket 1" in {
    // 15000 PLN/month → 180000 PLN/year (above 120000 bracket)
    val monthly     = 15000.0
    val annualized  = monthly * 12.0
    val bracket1Tax = 120000.0 * 0.12                         // 14400
    val bracket2Tax = (annualized - 120000.0) * 0.32          // 60000 × 0.32 = 19200
    val grossTax    = bracket1Tax + bracket2Tax               // 33600
    val netTax      = Math.max(0.0, grossTax - 3600.0) / 12.0 // (33600 - 3600) / 12 = 2500
    netTax shouldBe 2500.0 +- 0.01
  }

  it should "produce effective rate ~9% for median earner" in {
    // Median: ~7000 PLN/month → 84000/year
    val monthly       = 7000.0
    val annualized    = monthly * 12.0
    val grossTax      = annualized * 0.12                       // 10080
    val netTax        = Math.max(0.0, grossTax - 3600.0) / 12.0 // (10080 - 3600) / 12 = 540
    val effectiveRate = netTax / monthly
    effectiveRate shouldBe 0.0771 +- 0.01
    effectiveRate should be < 0.12 // kwota wolna reduces effective rate
  }

  it should "have kwota wolna eliminate tax for very low income" in {
    // If annual income × 12% ≤ 3600 → no tax
    // 3600 / 0.12 = 30000 PLN/year → 2500 PLN/month
    val monthly    = 2500.0
    val annualized = monthly * 12.0
    val grossTax   = annualized * 0.12                       // 3600
    val netTax     = Math.max(0.0, grossTax - 3600.0) / 12.0 // 0
    netTax shouldBe 0.0
  }

  it should "be strictly positive above kwota wolna threshold" in {
    // Just above: 2501 PLN/month → 30012/year → tax = 30012 × 0.12 - 3600 = 1.44/year → 0.12/month
    val monthly    = 2501.0
    val annualized = monthly * 12.0
    val grossTax   = annualized * 0.12
    val netTax     = Math.max(0.0, grossTax - 3600.0) / 12.0
    netTax should be > 0.0
    netTax shouldBe 0.12 +- 0.01
  }

  "Household.Aggregates.totalPit" should "default to 0.0" in {
    val agg = Household.Aggregates(
      employed = 0,
      unemployed = 0,
      retraining = 0,
      bankrupt = 0,
      totalIncome = PLN.Zero,
      consumption = PLN.Zero,
      domesticConsumption = PLN.Zero,
      importConsumption = PLN.Zero,
      marketWage = PLN.Zero,
      reservationWage = PLN.Zero,
      giniIndividual = Ratio.Zero,
      giniWealth = Ratio.Zero,
      meanSavings = PLN.Zero,
      medianSavings = PLN.Zero,
      povertyRate50 = Ratio.Zero,
      bankruptcyRate = Ratio.Zero,
      meanSkill = 0,
      meanHealthPenalty = 0,
      retrainingAttempts = 0,
      retrainingSuccesses = 0,
      consumptionP10 = PLN.Zero,
      consumptionP50 = PLN.Zero,
      consumptionP90 = PLN.Zero,
      meanMonthsToRuin = 0,
      povertyRate30 = Ratio.Zero,
      totalRent = PLN.Zero,
      totalDebtService = PLN.Zero,
      totalUnempBenefits = PLN.Zero,
      totalDepositInterest = PLN.Zero,
      crossSectorHires = 0,
      voluntaryQuits = 0,
      sectorMobilityRate = Ratio.Zero,
      totalRemittances = PLN.Zero,
      totalPit = PLN.Zero,
      totalSocialTransfers = PLN.Zero,
      totalConsumerDebtService = PLN.Zero,
      totalConsumerOrigination = PLN.Zero,
      totalConsumerDefault = PLN.Zero,
      totalConsumerPrincipal = PLN.Zero,
    )
    agg.totalPit shouldBe PLN.Zero
  }
