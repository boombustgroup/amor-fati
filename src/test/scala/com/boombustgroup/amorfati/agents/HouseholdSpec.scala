package com.boombustgroup.amorfati.agents

import org.scalatest.flatspec.AnyFlatSpec
import com.boombustgroup.amorfati.Generators
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.{
  ExternalState,
  FinancialMarketsState,
  FlowState,
  MechanismsState,
  MonetaryPlumbingState,
  RealState,
  SocialState,
  World,
}
import com.boombustgroup.amorfati.engine.markets.{FiscalBudget, OpenEconomy}
import com.boombustgroup.amorfati.fp.ComputationBoundary
import com.boombustgroup.amorfati.types.*

import com.boombustgroup.amorfati.random.RandomStream

class HouseholdSpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]
  private val td           = ComputationBoundary

  private inline def shareValue(s: Share): Double =
    s.toLong.toDouble / com.boombustgroup.amorfati.fp.FixedPointBase.ScaleD

  // --- HouseholdInit ---

  "Household.Init.initialize" should "create correct number of households" in {
    val rng     = RandomStream.seeded(42)
    val firms   = mkFirms(100)
    val network = Array.fill(1000)(Array.empty[Int])
    val hhs     = Household.Init.initialize(1000, firms, network, rng)
    hhs.length shouldBe 1000
  }

  it should "start all households as Employed" in {
    val rng     = RandomStream.seeded(42)
    val firms   = mkFirms(100)
    val network = Array.fill(500)(Array.empty[Int])
    val hhs     = Household.Init.initialize(500, firms, network, rng)
    hhs.foreach { hh =>
      hh.status shouldBe a[HhStatus.Employed]
    }
  }

  it should "assign positive savings to all households" in {
    val rng     = RandomStream.seeded(42)
    val firms   = mkFirms(50)
    val network = Array.fill(200)(Array.empty[Int])
    val hhs     = Household.Init.initialize(200, firms, network, rng)
    hhs.foreach(_.savings should be > PLN.Zero)
  }

  it should "have MPC in [0.5, 0.98]" in {
    val rng     = RandomStream.seeded(42)
    val firms   = mkFirms(50)
    val network = Array.fill(500)(Array.empty[Int])
    val hhs     = Household.Init.initialize(500, firms, network, rng)
    hhs.foreach { hh =>
      hh.mpc should be >= Share(0.5)
      hh.mpc should be <= Share(0.98)
    }
  }

  it should "have skill in [0.3, 1.0]" in {
    val rng     = RandomStream.seeded(42)
    val firms   = mkFirms(50)
    val network = Array.fill(500)(Array.empty[Int])
    val hhs     = Household.Init.initialize(500, firms, network, rng)
    hhs.foreach { hh =>
      hh.skill should be >= Share(0.3)
      hh.skill should be <= Share.One
    }
  }

  it should "have rent >= floor" in {
    val rng     = RandomStream.seeded(42)
    val firms   = mkFirms(50)
    val network = Array.fill(500)(Array.empty[Int])
    val hhs     = Household.Init.initialize(500, firms, network, rng)
    hhs.foreach(_.monthlyRent should be >= p.household.rentFloor)
  }

  // --- Household.step ---

  "Household.step" should "not change bankrupt households" in {
    val rng             = RandomStream.seeded(42)
    val hhs             = Vector(
      mkHousehold(0, HhStatus.Bankrupt, savings = PLN(0.0)),
      mkHousehold(1, HhStatus.Employed(FirmId(0), SectorIdx(2), PLN(8000.0)), savings = PLN(50000.0)),
    )
    val (updated, _, _) = Household.step(hhs, mkWorld(), PLN(8000.0), PLN(4666.0), Share(0.4), rng)
    updated(0).status shouldBe HhStatus.Bankrupt
  }

  it should "increase unemployment months for unemployed" in {
    val rng             = RandomStream.seeded(42)
    val hhs             = Vector(
      mkHousehold(0, HhStatus.Unemployed(3), savings = PLN(50000.0)),
    )
    val (updated, _, _) = Household.step(hhs, mkWorld(), PLN(8000.0), PLN(4666.0), Share(0.4), rng)
    updated(0).status match
      case HhStatus.Unemployed(m)       => m should be >= 4
      case HhStatus.Retraining(_, _, _) => succeed // may enter retraining
      case HhStatus.Bankrupt            => succeed // may go bankrupt
      case other                        => fail(s"Unexpected status: $other")
  }

  it should "apply skill decay after scarring onset" in {
    val rng             = RandomStream.seeded(42)
    val hh              = mkHousehold(0, HhStatus.Unemployed(5), savings = PLN(100000.0), skill = 0.8)
    val (updated, _, _) = Household.step(Vector(hh), mkWorld(), PLN(8000.0), PLN(4666.0), Share(0.4), rng)
    updated(0).skill should be < Share(0.8)
  }

  it should "not decay skill before scarring onset" in {
    val rng             = RandomStream.seeded(42)
    val hh              = mkHousehold(0, HhStatus.Unemployed(1), savings = PLN(100000.0), skill = 0.8)
    val (updated, _, _) = Household.step(Vector(hh), mkWorld(), PLN(8000.0), PLN(4666.0), Share(0.4), rng)
    updated(0).skill shouldBe Share(0.8)
  }

  it should "apply health scarring after onset" in {
    val rng             = RandomStream.seeded(42)
    val hh              = mkHousehold(0, HhStatus.Unemployed(5), savings = PLN(100000.0), healthPenalty = 0.0)
    val (updated, _, _) = Household.step(Vector(hh), mkWorld(), PLN(8000.0), PLN(4666.0), Share(0.4), rng)
    updated(0).healthPenalty should be > Share.Zero
  }

  it should "not bankrupt household after a single month of deep distress" in {
    val rng             = RandomStream.seeded(42)
    val hh              = mkHousehold(0, HhStatus.Unemployed(1), savings = PLN(-10000.0), rent = PLN(1800.0))
    val (updated, _, _) = Household.step(Vector(hh), mkWorld(), PLN(8000.0), PLN(4666.0), Share(0.4), rng)
    updated(0).status should not be HhStatus.Bankrupt
    updated(0).financialDistressMonths shouldBe 1
  }

  it should "bankrupt household after persistent deep distress" in {
    val rng             = RandomStream.seeded(42)
    val hh              = mkHousehold(
      0,
      HhStatus.Unemployed(1),
      savings = PLN(-10000.0),
      rent = PLN(1800.0),
    ).copy(financialDistressMonths = p.household.bankruptcyDistressMonths - 1)
    val (updated, _, _) = Household.step(Vector(hh), mkWorld(), PLN(8000.0), PLN(4666.0), Share(0.4), rng)
    updated(0).status shouldBe HhStatus.Bankrupt
  }

  it should "reset financial distress months after recovery" in {
    val rng             = RandomStream.seeded(42)
    val hh              = mkHousehold(
      0,
      HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(8000.0)),
      savings = PLN(20000.0),
      rent = PLN(1800.0),
    ).copy(financialDistressMonths = 2)
    val (updated, _, _) = Household.step(Vector(hh), mkWorld(), PLN(8000.0), PLN(4666.0), Share(0.4), rng)
    updated(0).status shouldBe a[HhStatus.Employed]
    updated(0).financialDistressMonths shouldBe 0
  }

  it should "return None for perBankHhFlows when bankRates not provided" in {
    val rng         = RandomStream.seeded(42)
    val hhs         = Vector(mkHousehold(0, HhStatus.Employed(FirmId(0), SectorIdx(2), PLN(8000.0)), savings = PLN(50000.0)))
    val (_, _, pbf) = Household.step(hhs, mkWorld(), PLN(8000.0), PLN(4666.0), Share(0.4), rng)
    pbf shouldBe None
  }

  it should "let unemployed households smooth consumption by drawing down savings" in {
    val rng               = RandomStream.seeded(42)
    val hh                = mkHousehold(0, HhStatus.Unemployed(1), savings = PLN(30000.0), rent = PLN(1800.0))
    val (updated, agg, _) = Household.step(Vector(hh), mkWorld(), PLN(8000.0), PLN(4666.0), Share(0.4), rng)

    agg.consumption should be > PLN.Zero
    updated(0).savings should be < hh.savings
  }

  // --- Variable-rate debt service + deposit interest ---

  "Household.step with bankRates" should "use variable lending rate for debt service" in {
    val rng              = RandomStream.seeded(42)
    val debt             = PLN(100000.0)
    val hhs              = Vector(
      mkHousehold(
        0,
        HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(8000.0)),
        savings = PLN(50000.0),
        debt = debt,
        bankId = 0,
      ),
      mkHousehold(
        1,
        HhStatus.Employed(FirmId(1), SectorIdx(0), PLN(8000.0)),
        savings = PLN(50000.0),
        debt = debt,
        bankId = 1,
      ),
    )
    // Bank 0: 6% annual lending rate, Bank 1: 10% annual
    val br               = BankRates(
      lendingRates = Vector(Rate(0.06), Rate(0.10)),
      depositRates = Vector(Rate(0.04), Rate(0.04)),
    )
    val (_, _, maybePbf) =
      Household.step(hhs, mkWorld(), PLN(8000.0), PLN(4666.0), Share(0.4), rng, nBanks = 2, bankRates = Some(br))
    val pbf              = maybePbf.get
    // Expected debt service: debt * (HhBaseAmortRate + lendingRate/12)
    val expectedDs0      = td.toDouble(debt) * (td.toDouble(p.household.baseAmortRate) + 0.06 / 12.0)
    val expectedDs1      = td.toDouble(debt) * (td.toDouble(p.household.baseAmortRate) + 0.10 / 12.0)
    pbf(0).debtService shouldBe PLN(expectedDs0) +- PLN(10.0)
    pbf(1).debtService shouldBe PLN(expectedDs1) +- PLN(10.0)
    // Bank 1's higher rate should mean higher debt service
    pbf(1).debtService should be > pbf(0).debtService
  }

  it should "pay deposit interest to HH with positive savings" in {
    val rng                = RandomStream.seeded(42)
    val savings            = PLN(100000.0)
    val hhs                = Vector(
      mkHousehold(0, HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(8000.0)), savings = savings, bankId = 0),
    )
    val depRate            = 0.04 // 4% annual
    val br                 = BankRates(
      lendingRates = Vector(Rate(0.07)),
      depositRates = Vector(Rate(depRate)),
    )
    val (_, agg, maybePbf) =
      Household.step(hhs, mkWorld(), PLN(8000.0), PLN(4666.0), Share(0.4), rng, nBanks = 1, bankRates = Some(br))
    val pbf                = maybePbf.get
    val expectedDepInt     = depRate / 12.0 * td.toDouble(savings)
    pbf(0).depositInterest shouldBe PLN(expectedDepInt) +- PLN(10.0)
    agg.totalDepositInterest shouldBe PLN(expectedDepInt) +- PLN(10.0)
  }

  it should "include deposit interest in totalIncome" in {
    val rng            = RandomStream.seeded(42)
    val savings        = PLN(200000.0)
    val wage           = 8000.0
    val depRate        = 0.04
    val hhs            = Vector(
      mkHousehold(0, HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(wage)), savings = savings, bankId = 0),
    )
    val br             = BankRates(
      lendingRates = Vector(Rate(0.07)),
      depositRates = Vector(Rate(depRate)),
    )
    val (_, agg, _)    = Household.step(hhs, mkWorld(), PLN(wage), PLN(4666.0), Share(0.4), rng, nBanks = 1, bankRates = Some(br))
    val expectedDepInt = depRate / 12.0 * td.toDouble(savings)
    val grossIncome    = wage + expectedDepInt
    val pitTax         = Household.computeMonthlyPit(PLN(grossIncome))
    // totalIncome = grossIncome - PIT + socialTransfer (0 children → no transfer)
    agg.totalIncome shouldBe PLN(grossIncome - td.toDouble(pitTax)) +- PLN(20.0)
  }

  it should "accumulate per-bank flows correctly for 2 banks" in {
    val rng              = RandomStream.seeded(42)
    val hhs              = Vector(
      mkHousehold(
        0,
        HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(8000.0)),
        savings = PLN(50000.0),
        debt = PLN(0.0),
        bankId = 0,
      ),
      mkHousehold(
        1,
        HhStatus.Employed(FirmId(1), SectorIdx(0), PLN(7000.0)),
        savings = PLN(30000.0),
        debt = PLN(0.0),
        bankId = 0,
      ),
      mkHousehold(
        2,
        HhStatus.Employed(FirmId(2), SectorIdx(0), PLN(9000.0)),
        savings = PLN(80000.0),
        debt = PLN(0.0),
        bankId = 1,
      ),
    )
    val br               = BankRates(
      lendingRates = Vector(Rate(0.07), Rate(0.08)),
      depositRates = Vector(Rate(0.035), Rate(0.035)),
    )
    val (_, _, maybePbf) =
      Household.step(hhs, mkWorld(), PLN(8000.0), PLN(4666.0), Share(0.4), rng, nBanks = 2, bankRates = Some(br))
    val pbf              = maybePbf.get
    // Bank 0 has HH 0 and 1: income should include both
    pbf(0).income should be > PLN.Zero
    pbf(1).income should be > PLN.Zero
    // Bank 0 deposit interest: (50000 + 30000) * 0.035/12
    val expDepInt0       = (50000.0 + 30000.0) * 0.035 / 12.0
    pbf(0).depositInterest shouldBe PLN(expDepInt0) +- PLN(10.0)
    // Bank 1 deposit interest: 80000 * 0.035/12
    val expDepInt1       = 80000.0 * 0.035 / 12.0
    pbf(1).depositInterest shouldBe PLN(expDepInt1) +- PLN(10.0)
  }

  it should "not pay deposit interest on negative savings" in {
    val rng                = RandomStream.seeded(42)
    val hhs                = Vector(
      mkHousehold(0, HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(8000.0)), savings = PLN(-5000.0), bankId = 0),
    )
    val br                 = BankRates(
      lendingRates = Vector(Rate(0.07)),
      depositRates = Vector(Rate(0.04)),
    )
    val (_, agg, maybePbf) =
      Household.step(hhs, mkWorld(), PLN(8000.0), PLN(4666.0), Share(0.4), rng, nBanks = 1, bankRates = Some(br))
    val pbf                = maybePbf.get
    // Deposit interest on negative savings is floored at 0
    pbf(0).depositInterest shouldBe PLN.Zero
    agg.totalDepositInterest shouldBe PLN.Zero
  }

  // --- Immigration: remittance deduction ---

  "Household.step" should "not deduct remittances from non-immigrant HH" in {
    val rng         = RandomStream.seeded(42)
    val wage        = 8000.0
    val hhs         = Vector(
      mkHousehold(0, HhStatus.Employed(FirmId(0), SectorIdx(2), PLN(wage)), savings = PLN(50000.0))
        .copy(isImmigrant = false),
    )
    val (_, agg, _) = Household.step(hhs, mkWorld(), PLN(wage), PLN(4666.0), Share(0.4), rng)
    agg.totalRemittances shouldBe PLN.Zero
  }

  // --- Household.giniSorted ---

  "Household.giniSorted" should "return 0 for equal values" in {
    shareValue(Household.giniSorted(Array(100L, 100L, 100L, 100L))) shouldBe (0.0 +- 0.001)
  }

  it should "return 0 for single element" in {
    Household.giniSorted(Array(42L)) shouldBe Share.Zero
  }

  it should "return value in [0, 1] for typical distribution" in {
    val values = Array(1000L, 2000L, 3000L, 5000L, 10000L, 50000L)
    val g      = Household.giniSorted(values)
    g should be >= Share.Zero
    g should be <= Share.One
  }

  it should "increase with more inequality" in {
    val equal   = Array(1000L, 1000L, 1000L, 1000L)
    val unequal = Array(0L, 0L, 0L, 4000L)
    Household.giniSorted(unequal) should be > Household.giniSorted(equal)
  }

  // --- Household.computeAggregates ---

  "Household.computeAggregates" should "count statuses correctly" in {
    val hhs = Vector(
      mkHousehold(0, HhStatus.Employed(FirmId(0), SectorIdx(2), PLN(8000.0))),
      mkHousehold(1, HhStatus.Employed(FirmId(1), SectorIdx(2), PLN(7000.0))),
      mkHousehold(2, HhStatus.Unemployed(3)),
      mkHousehold(3, HhStatus.Retraining(4, SectorIdx(1), PLN(5000.0))),
      mkHousehold(4, HhStatus.Bankrupt),
    )
    val agg = Household.computeAggregates(hhs, PLN(8000.0), PLN(4666.0), Share(0.4), 0, 0)
    agg.employed shouldBe 2
    agg.unemployed shouldBe 1
    agg.retraining shouldBe 1
    agg.bankrupt shouldBe 1
    agg.bankruptcyRate shouldBe Share(0.2) +- Share(0.001)
  }

  // --- helpers ---

  private def mkFirms(n: Int): Vector[Firm.State] =
    (0 until n).map { i =>
      Firm.State(
        FirmId(i),
        PLN(50000.0),
        PLN(0.0),
        TechState.Traditional(10),
        Share(0.5),
        Multiplier.One,
        Share(0.5),
        SectorIdx(i % p.sectorDefs.length),
        Vector.empty[FirmId],
        bankId = BankId(0),
        equityRaised = PLN.Zero,
        initialSize = 10,
        capitalStock = PLN.Zero,
        bondDebt = PLN.Zero,
        foreignOwned = false,
        inventory = PLN.Zero,
        greenCapital = PLN.Zero,
        accumulatedLoss = PLN.Zero,
      )
    }.toVector

  private def mkHousehold(
      id: Int,
      status: HhStatus,
      savings: PLN = PLN(20000.0),
      debt: PLN = PLN(0.0),
      rent: PLN = PLN(1800.0),
      skill: Double = 0.7,
      healthPenalty: Double = 0.0,
      mpc: Double = 0.82,
      bankId: Int = 0,
  ): Household.State =
    Household.State(
      HhId(id),
      savings,
      debt,
      rent,
      Share(skill),
      Share(healthPenalty),
      Share(mpc),
      status,
      Array.empty[HhId],
      BankId(bankId),
      equityWealth = PLN.Zero,
      lastSectorIdx = SectorIdx(-1),
      isImmigrant = false,
      numDependentChildren = 0,
      consumerDebt = PLN.Zero,
      education = 2,
      taskRoutineness = Share(0.5),
      wageScar = Share.Zero,
    )

  private def mkWorld(): World =
    World(
      inflation = Rate(0.02),
      priceLevel = 1.0,
      gdpProxy = 1e9,
      currentSigmas = p.sectorDefs.map(_.sigma).toVector,
      totalPopulation = 100000,
      gov = FiscalBudget.GovState(PLN(0.0), PLN(0.0), PLN(0.0), PLN(0.0)),
      nbp = Nbp.State(Rate(0.0575), PLN.Zero, false, PLN.Zero, PLN.Zero, PLN.Zero),
      bankingSector = Generators.testBankingSector().marketState,
      forex = OpenEconomy.ForexState(ExchangeRate(4.33), PLN(0.0), PLN(190000000), PLN(0.0), PLN(0.0)),
      hhAgg = Household.Aggregates(
        employed = 100000,
        unemployed = 0,
        retraining = 0,
        bankrupt = 0,
        totalIncome = PLN.Zero,
        consumption = PLN.Zero,
        domesticConsumption = PLN.Zero,
        importConsumption = PLN.Zero,
        marketWage = p.household.baseWage,
        reservationWage = p.household.baseReservationWage,
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
      ),
      social = SocialState.zero,
      financial = FinancialMarketsState.zero,
      external = ExternalState.zero,
      real = RealState.zero,
      mechanisms = MechanismsState.zero,
      plumbing = MonetaryPlumbingState.zero,
      flows = FlowState.zero,
    )
