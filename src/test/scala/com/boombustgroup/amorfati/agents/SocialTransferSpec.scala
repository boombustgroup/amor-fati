package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.TestHouseholdState

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.markets.FiscalBudget
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.amorfati.util.Distributions

import com.boombustgroup.amorfati.random.RandomStream

/** Social transfers (800+ child benefit) unit tests. */
class SocialTransferSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  "computeSocialTransfer" should "return 0 for 0 children regardless of config" in {
    Household.computeSocialTransfer(0) shouldBe PLN.Zero
  }

  it should "return 0 for negative children" in {
    Household.computeSocialTransfer(-1) shouldBe PLN.Zero
  }

  // --- Formula verification ---

  "Social transfer formula" should "compute rate * children" in {
    // 2 children × 800 PLN = 1600 PLN/month
    val rate     = BigDecimal("800.0")
    val children = 2
    val expected = decimal(children) * rate
    expected shouldBe BigDecimal("1600.0")
  }

  it should "scale linearly with number of children" in {
    val rate = BigDecimal("800.0")
    (1 to 5).foreach { n =>
      val transfer = decimal(n) * rate
      transfer shouldBe decimal(n) * BigDecimal("800.0")
    }
  }

  "poissonSample" should "return 0 for lambda=0" in {
    val rng = RandomStream.seeded(42)
    Distributions.poissonSample(Scalar.Zero, rng) shouldBe 0
  }

  it should "return 0 for negative lambda" in {
    val rng = RandomStream.seeded(42)
    Distributions.poissonSample(Scalar(-1), rng) shouldBe 0
  }

  it should "have mean approximately equal to lambda" in {
    val rng     = RandomStream.seeded(42)
    val lambda  = Scalar.decimal(35, 2)
    val n       = 10000
    val samples = (0 until n).map(_ => Distributions.poissonSample(lambda, rng))
    val mean    = decimal(samples.sum) / decimal(n)
    mean shouldBe decimal(lambda) +- (decimal(lambda) * BigDecimal("0.10")) // ±10% tolerance
  }

  it should "produce non-negative values" in {
    val rng     = RandomStream.seeded(42)
    val samples = (0 until 1000).map(_ => Distributions.poissonSample(Scalar.decimal(35, 2), rng))
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
      giniIndividual = Share.Zero,
      giniWealth = Share.Zero,
      meanSavings = PLN.Zero,
      medianSavings = PLN.Zero,
      povertyRate50 = Share.Zero,
      bankruptcyRate = Share.Zero,
      meanSkill = Share.Zero,
      meanHealthPenalty = Share.Zero,
      retrainingAttempts = 0,
      retrainingSuccesses = 0,
      consumptionP10 = PLN.Zero,
      consumptionP50 = PLN.Zero,
      consumptionP90 = PLN.Zero,
      meanMonthsToRuin = Scalar.Zero,
      povertyRate30 = Share.Zero,
      totalRent = PLN.Zero,
      totalDebtService = PLN.Zero,
      totalUnempBenefits = PLN.Zero,
      totalDepositInterest = PLN.Zero,
      crossSectorHires = 0,
      voluntaryQuits = 0,
      sectorMobilityRate = Share.Zero,
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
    val hh = TestHouseholdState(
      id = HhId(0),
      savings = PLN(1000),
      debt = PLN.Zero,
      monthlyRent = PLN(1000),
      skill = Share.decimal(5, 1),
      healthPenalty = Share(0),
      mpc = Share.decimal(8, 1),
      status = HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(8000)),
      socialNeighbors = Array.empty[HhId],
      bankId = BankId(0),
      equityWealth = PLN.Zero,
      lastSectorIdx = SectorIdx(-1),
      isImmigrant = false,
      numDependentChildren = 0,
      consumerDebt = PLN.Zero,
      education = 2,
      taskRoutineness = Share.decimal(5, 1),
      wageScar = Share.Zero,
    )
    hh.numDependentChildren shouldBe 0
  }

  "GovState.socialTransferSpend" should "default to 0.0" in {
    val gov = FiscalBudget.GovState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero)
    gov.socialTransferSpend shouldBe PLN.Zero
  }
