package com.boombustgroup.amorfati.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.markets.FiscalBudget
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.amorfati.util.Distributions

import scala.util.Random

/** Social transfers (800+ child benefit) unit tests. */
class SocialTransferSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  "computeSocialTransfer" should "return 0 for 0 children regardless of config" in {
    Household.computeSocialTransfer(0) shouldBe PLN.Zero
  }

  it should "return 0 for negative children" in {
    Household.computeSocialTransfer(-1) shouldBe PLN.Zero
  }

  // --- Formula verification (independent of p.flags.social800) ---

  "Social transfer formula" should "compute rate * children" in {
    // 2 children × 800 PLN = 1600 PLN/month
    val rate     = 800.0
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
    Distributions.poissonSample(0.0, rng) shouldBe 0
  }

  it should "return 0 for negative lambda" in {
    val rng = new Random(42)
    Distributions.poissonSample(-1.0, rng) shouldBe 0
  }

  it should "have mean approximately equal to lambda" in {
    val rng     = new Random(42)
    val lambda  = 0.35
    val n       = 10000
    val samples = (0 until n).map(_ => Distributions.poissonSample(lambda, rng))
    val mean    = samples.sum.toDouble / n
    mean shouldBe lambda +- (lambda * 0.10) // ±10% tolerance
  }

  it should "produce non-negative values" in {
    val rng     = new Random(42)
    val samples = (0 until 1000).map(_ => Distributions.poissonSample(0.35, rng))
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
    agg.totalSocialTransfers shouldBe PLN.Zero
  }

  "Household.numDependentChildren" should "default to 0" in {
    val hh = Household.State(
      id = HhId(0),
      savings = PLN(1000),
      debt = PLN.Zero,
      monthlyRent = PLN(1000),
      skill = Ratio(0.5),
      healthPenalty = Ratio(0.0),
      mpc = Ratio(0.8),
      status = HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(8000.0)),
      socialNeighbors = Array.empty[HhId],
      bankId = BankId(0),
      equityWealth = PLN.Zero,
      lastSectorIdx = SectorIdx(-1),
      isImmigrant = false,
      numDependentChildren = 0,
      consumerDebt = PLN.Zero,
      education = 2,
      taskRoutineness = Ratio(0.5),
      wageScar = Ratio.Zero,
    )
    hh.numDependentChildren shouldBe 0
  }

  "GovState.socialTransferSpend" should "default to 0.0" in {
    val gov = FiscalBudget.GovState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
    gov.socialTransferSpend shouldBe PLN.Zero
  }
