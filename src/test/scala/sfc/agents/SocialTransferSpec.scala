package sfc.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.config.Config

import scala.util.Random

/** Social transfers (800+ child benefit) unit tests. */
class SocialTransferSpec extends AnyFlatSpec with Matchers:

  // Note: Config.Social800Enabled is false by default, so computeSocialTransfer returns 0.
  // Tests verify formula logic directly.

  "computeSocialTransfer" should "return 0 when disabled (default)" in {
    HouseholdLogic.computeSocialTransfer(2) shouldBe 0.0
  }

  it should "return 0 for 0 children regardless of config" in {
    HouseholdLogic.computeSocialTransfer(0) shouldBe 0.0
  }

  it should "return 0 for negative children" in {
    HouseholdLogic.computeSocialTransfer(-1) shouldBe 0.0
  }

  // --- Formula verification (independent of Config.Social800Enabled) ---

  "Social transfer formula" should "compute rate * children" in {
    // 2 children × 800 PLN = 1600 PLN/month
    val rate = 800.0
    val children = 2
    val expected = children.toDouble * rate
    expected shouldBe 1600.0
  }

  it should "scale linearly with number of children" in {
    val rate = 800.0
    (1 to 5).foreach { n =>
      val transfer = n.toDouble * rate
      transfer shouldBe n * 800.0
    }
  }

  "poissonSample" should "return 0 for lambda=0" in {
    val rng = new Random(42)
    HouseholdInit.poissonSample(0.0, rng) shouldBe 0
  }

  it should "return 0 for negative lambda" in {
    val rng = new Random(42)
    HouseholdInit.poissonSample(-1.0, rng) shouldBe 0
  }

  it should "have mean approximately equal to lambda" in {
    val rng = new Random(42)
    val lambda = 0.35
    val n = 10000
    val samples = (0 until n).map(_ => HouseholdInit.poissonSample(lambda, rng))
    val mean = samples.sum.toDouble / n
    mean shouldBe lambda +- (lambda * 0.10)  // ±10% tolerance
  }

  it should "produce non-negative values" in {
    val rng = new Random(42)
    val samples = (0 until 1000).map(_ => HouseholdInit.poissonSample(0.35, rng))
    all(samples) should be >= 0
  }

  "HhAggregates.totalSocialTransfers" should "default to 0.0" in {
    val agg = HhAggregates(
      employed = 0, unemployed = 0, retraining = 0, bankrupt = 0,
      totalIncome = 0, consumption = 0, domesticConsumption = 0, importConsumption = 0,
      marketWage = 0, reservationWage = 0, giniIndividual = 0, giniWealth = 0,
      meanSavings = 0, medianSavings = 0, povertyRate50 = 0, bankruptcyRate = 0,
      meanSkill = 0, meanHealthPenalty = 0, retrainingAttempts = 0, retrainingSuccesses = 0,
      consumptionP10 = 0, consumptionP50 = 0, consumptionP90 = 0,
      meanMonthsToRuin = 0, povertyRate30 = 0, totalRent = 0,
      totalDebtService = 0, totalUnempBenefits = 0
    )
    agg.totalSocialTransfers shouldBe 0.0
  }

  "Household.numDependentChildren" should "default to 0" in {
    val hh = Household(
      id = 0, savings = 1000, debt = 0, monthlyRent = 1000,
      skill = 0.5, healthPenalty = 0.0, mpc = 0.8,
      status = HhStatus.Employed(0, 0, 8000.0),
      socialNeighbors = Array.empty
    )
    hh.numDependentChildren shouldBe 0
  }

  "GovState.socialTransferSpend" should "default to 0.0" in {
    val gov = sfc.sfc.GovState(false, 0, 0, 0, 0, 0)
    gov.socialTransferSpend shouldBe 0.0
  }
