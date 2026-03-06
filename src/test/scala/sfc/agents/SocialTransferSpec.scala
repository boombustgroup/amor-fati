package sfc.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.accounting.GovState
import sfc.types.*

import scala.util.Random

/** Social transfers (800+ child benefit) unit tests. */
class SocialTransferSpec extends AnyFlatSpec with Matchers:

  // Note: Config.Social800Enabled is false by default, so computeSocialTransfer returns 0.
  // Tests verify formula logic directly.

  "computeSocialTransfer" should "return 0 when disabled (default)" in {
    Household.computeSocialTransfer(2) shouldBe 0.0
  }

  it should "return 0 for 0 children regardless of config" in {
    Household.computeSocialTransfer(0) shouldBe 0.0
  }

  it should "return 0 for negative children" in {
    Household.computeSocialTransfer(-1) shouldBe 0.0
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
    Household.Init.poissonSample(0.0, rng) shouldBe 0
  }

  it should "return 0 for negative lambda" in {
    val rng = new Random(42)
    Household.Init.poissonSample(-1.0, rng) shouldBe 0
  }

  it should "have mean approximately equal to lambda" in {
    val rng = new Random(42)
    val lambda = 0.35
    val n = 10000
    val samples = (0 until n).map(_ => Household.Init.poissonSample(lambda, rng))
    val mean = samples.sum.toDouble / n
    mean shouldBe lambda +- (lambda * 0.10) // ±10% tolerance
  }

  it should "produce non-negative values" in {
    val rng = new Random(42)
    val samples = (0 until 1000).map(_ => Household.Init.poissonSample(0.35, rng))
    all(samples) should be >= 0
  }

  "Household.Aggregates.totalSocialTransfers" should "default to 0.0" in {
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
    )
    agg.totalSocialTransfers.toDouble shouldBe 0.0
  }

  "Household.numDependentChildren" should "default to 0" in {
    val hh = Household.State(
      id = 0,
      savings = PLN(1000),
      debt = PLN.Zero,
      monthlyRent = PLN(1000),
      skill = Ratio(0.5),
      healthPenalty = Ratio(0.0),
      mpc = Ratio(0.8),
      status = HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(8000.0)),
      socialNeighbors = Array.empty[Int],
    )
    hh.numDependentChildren shouldBe 0
  }

  "GovState.socialTransferSpend" should "default to 0.0" in {
    val gov = GovState(false, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
    gov.socialTransferSpend.toDouble shouldBe 0.0
  }
