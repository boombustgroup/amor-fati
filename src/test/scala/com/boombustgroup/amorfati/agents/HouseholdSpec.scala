package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.TestHouseholdState

import com.boombustgroup.amorfati.TestFirmState

import org.scalatest.flatspec.AnyFlatSpec
import com.boombustgroup.amorfati.Generators
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.engine.markets.OpenEconomy
import com.boombustgroup.amorfati.fp.ComputationBoundary
import com.boombustgroup.amorfati.types.*

import com.boombustgroup.amorfati.random.RandomStream

class HouseholdSpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]
  private val td           = ComputationBoundary

  private inline def shareValue(s: Share): Double =
    s.toLong.toDouble / com.boombustgroup.amorfati.fp.FixedPointBase.ScaleD

  private val financialById = scala.collection.mutable.Map.empty[Int, Household.FinancialStocks]

  extension (hh: Household.State)
    private def savings: PLN      = stockOf(hh).demandDeposit
    private def debt: PLN         = stockOf(hh).mortgageLoan
    private def consumerDebt: PLN = stockOf(hh).consumerLoan
    private def equityWealth: PLN = stockOf(hh).equity

  private def stockOf(hh: Household.State): Household.FinancialStocks =
    financialById.getOrElse(hh.id.toInt, Household.FinancialStocks(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero))

  private def step(
      households: Vector[Household.State],
      world: World,
      marketWage: PLN,
      reservationWage: PLN,
      importAdj: Share,
      rng: RandomStream,
      nBanks: Int = 1,
      bankRates: Option[BankRates] = None,
      equityIndexReturn: Rate = Rate.Zero,
      sectorWages: Option[Vector[PLN]] = None,
      sectorVacancies: Option[Vector[Int]] = None,
  ): Household.StepResult =
    val result = Household.step(
      households,
      households.map(stockOf),
      world,
      marketWage,
      reservationWage,
      importAdj,
      rng,
      nBanks,
      bankRates,
      equityIndexReturn,
      sectorWages,
      sectorVacancies,
    )
    result.households.zip(result.financialStocks).foreach((hh, stocks) => financialById.update(hh.id.toInt, stocks))
    result

  private def computeAggregates(
      households: Vector[Household.State],
      marketWage: PLN,
      reservationWage: PLN,
      importAdj: Share,
      retrainingAttempts: Int,
      retrainingSuccesses: Int,
  ): Household.Aggregates =
    Household.computeAggregates(
      households,
      households.map(stockOf),
      marketWage,
      reservationWage,
      importAdj,
      retrainingAttempts,
      retrainingSuccesses,
    )

  // --- HouseholdInit ---

  "Household.Init.initialize" should "create correct number of households" in {
    val rng     = RandomStream.seeded(42)
    val firms   = mkFirms(100)
    val network = Array.fill(1000)(Array.empty[Int])
    val hhs     = Household.Init.initialize(1000, firms, network, rng).households
    hhs.length shouldBe 1000
  }

  it should "start all households as Employed" in {
    val rng     = RandomStream.seeded(42)
    val firms   = mkFirms(100)
    val network = Array.fill(500)(Array.empty[Int])
    val hhs     = Household.Init.initialize(500, firms, network, rng).households
    hhs.foreach { hh =>
      hh.status shouldBe a[HhStatus.Employed]
    }
  }

  it should "assign endogenous contract types from sector mixes" in {
    val rng     = RandomStream.seeded(42)
    val firms   = mkFirms(100)
    val network = Array.fill(1000)(Array.empty[Int])
    val hhs     = Household.Init.initialize(1000, firms, network, rng).households

    hhs.exists(_.contractType == ContractType.Zlecenie) shouldBe true
    hhs.exists(_.contractType == ContractType.B2B) shouldBe true
    hhs.count(_.contractType == ContractType.Permanent) should be < hhs.length
  }

  it should "assign positive savings to all households" in {
    val rng     = RandomStream.seeded(42)
    val firms   = mkFirms(50)
    val network = Array.fill(200)(Array.empty[Int])
    val init    = Household.Init.initialize(200, firms, network, rng)
    init.financialStocks.foreach(_.demandDeposit should be > PLN.Zero)
  }

  it should "have MPC in [0.5, 0.98]" in {
    val rng     = RandomStream.seeded(42)
    val firms   = mkFirms(50)
    val network = Array.fill(500)(Array.empty[Int])
    val hhs     = Household.Init.initialize(500, firms, network, rng).households
    hhs.foreach { hh =>
      hh.mpc should be >= Share(0.5)
      hh.mpc should be <= Share(0.98)
    }
  }

  it should "have skill in [0.3, 1.0]" in {
    val rng     = RandomStream.seeded(42)
    val firms   = mkFirms(50)
    val network = Array.fill(500)(Array.empty[Int])
    val hhs     = Household.Init.initialize(500, firms, network, rng).households
    hhs.foreach { hh =>
      hh.skill should be >= Share(0.3)
      hh.skill should be <= Share.One
    }
  }

  it should "have rent >= floor" in {
    val rng     = RandomStream.seeded(42)
    val firms   = mkFirms(50)
    val network = Array.fill(500)(Array.empty[Int])
    val hhs     = Household.Init.initialize(500, firms, network, rng).households
    hhs.foreach(_.monthlyRent should be >= p.household.rentFloor)
  }

  // --- Household.step ---

  "Household.step" should "not change bankrupt households" in {
    val rng     = RandomStream.seeded(42)
    val hhs     = Vector(
      mkHousehold(0, HhStatus.Bankrupt, savings = PLN(0.0)),
      mkHousehold(1, HhStatus.Employed(FirmId(0), SectorIdx(2), PLN(8000.0)), savings = PLN(50000.0)),
    )
    val updated = step(hhs, mkWorld(), PLN(8000.0), PLN(4666.0), Share(0.4), rng).households
    updated(0).status shouldBe HhStatus.Bankrupt
  }

  it should "increase unemployment months for unemployed" in {
    val rng     = RandomStream.seeded(42)
    val hhs     = Vector(
      mkHousehold(0, HhStatus.Unemployed(3), savings = PLN(50000.0)),
    )
    val updated = step(hhs, mkWorld(), PLN(8000.0), PLN(4666.0), Share(0.4), rng).households
    updated(0).status match
      case HhStatus.Unemployed(m)       => m should be >= 4
      case HhStatus.Retraining(_, _, _) => succeed // may enter retraining
      case HhStatus.Bankrupt            => succeed // may go bankrupt
      case other                        => fail(s"Unexpected status: $other")
  }

  it should "apply skill decay after scarring onset" in {
    val rng     = RandomStream.seeded(42)
    val hh      = mkHousehold(0, HhStatus.Unemployed(5), savings = PLN(100000.0), skill = 0.8)
    val updated = step(Vector(hh), mkWorld(), PLN(8000.0), PLN(4666.0), Share(0.4), rng).households
    updated(0).skill should be < Share(0.8)
  }

  it should "not decay skill before scarring onset" in {
    val rng     = RandomStream.seeded(42)
    val hh      = mkHousehold(0, HhStatus.Unemployed(1), savings = PLN(100000.0), skill = 0.8)
    val updated = step(Vector(hh), mkWorld(), PLN(8000.0), PLN(4666.0), Share(0.4), rng).households
    updated(0).skill shouldBe Share(0.8)
  }

  it should "apply health scarring after onset" in {
    val rng     = RandomStream.seeded(42)
    val hh      = mkHousehold(0, HhStatus.Unemployed(5), savings = PLN(100000.0), healthPenalty = 0.0)
    val updated = step(Vector(hh), mkWorld(), PLN(8000.0), PLN(4666.0), Share(0.4), rng).households
    updated(0).healthPenalty should be > Share.Zero
  }

  it should "not bankrupt household after a single month of deep distress" in {
    val rng     = RandomStream.seeded(42)
    val hh      = mkHousehold(0, HhStatus.Unemployed(1), savings = PLN(-10000.0), rent = PLN(1800.0))
    val updated = step(Vector(hh), mkWorld(), PLN(8000.0), PLN(4666.0), Share(0.4), rng).households
    updated(0).status should not be HhStatus.Bankrupt
    updated(0).financialDistressMonths shouldBe 1
  }

  it should "bankrupt household after persistent deep distress" in {
    val rng     = RandomStream.seeded(42)
    val hh      = mkHousehold(
      0,
      HhStatus.Unemployed(1),
      savings = PLN(-10000.0),
      rent = PLN(1800.0),
    ).copy(financialDistressMonths = p.household.bankruptcyDistressMonths - 1)
    val updated = step(Vector(hh), mkWorld(), PLN(8000.0), PLN(4666.0), Share(0.4), rng).households
    updated(0).status shouldBe HhStatus.Bankrupt
  }

  it should "default the remaining consumer credit balance after same-month debt service on bankruptcy" in {
    val rng                 = RandomStream.seeded(42)
    val world               = mkWorld()
    val openingLoan         = PLN(12000.0)
    val hh                  = mkHousehold(
      0,
      HhStatus.Unemployed(1),
      savings = PLN(-10000.0),
      rent = PLN(1800.0),
    ).copy(financialDistressMonths = p.household.bankruptcyDistressMonths - 1)
    val totalRate           = p.household.ccAmortRate + (world.nbp.referenceRate + p.household.ccSpread).monthly
    val expectedDebtService = openingLoan * totalRate
    val expectedDefault     = openingLoan - expectedDebtService

    financialById.update(
      hh.id.toInt,
      TestHouseholdState.financial(
        savings = PLN(-10000.0),
        debt = PLN.Zero,
        consumerDebt = openingLoan,
        equityWealth = PLN.Zero,
      ),
    )

    val result = step(Vector(hh), world, PLN(8000.0), PLN(4666.0), Share(0.4), rng)

    result.households.head.status shouldBe HhStatus.Bankrupt
    result.financialStocks.head.consumerLoan shouldBe PLN.Zero
    result.aggregates.totalConsumerDebtService shouldBe expectedDebtService
    result.aggregates.totalConsumerDefault shouldBe expectedDefault
    result.aggregates.totalConsumerDebtService + result.aggregates.totalConsumerDefault shouldBe openingLoan
  }

  it should "reset financial distress months after recovery" in {
    val rng     = RandomStream.seeded(42)
    val hh      = mkHousehold(
      0,
      HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(8000.0)),
      savings = PLN(20000.0),
      rent = PLN(1800.0),
    ).copy(financialDistressMonths = 2)
    val updated = step(Vector(hh), mkWorld(), PLN(8000.0), PLN(4666.0), Share(0.4), rng).households
    updated(0).status shouldBe a[HhStatus.Employed]
    updated(0).financialDistressMonths shouldBe 0
  }

  it should "return None for perBankHhFlows when bankRates not provided" in {
    val rng = RandomStream.seeded(42)
    val hhs = Vector(mkHousehold(0, HhStatus.Employed(FirmId(0), SectorIdx(2), PLN(8000.0)), savings = PLN(50000.0)))
    val pbf = step(hhs, mkWorld(), PLN(8000.0), PLN(4666.0), Share(0.4), rng).perBankFlows
    pbf shouldBe None
  }

  it should "let unemployed households smooth consumption by drawing down savings" in {
    val rng            = RandomStream.seeded(42)
    val hh             = mkHousehold(0, HhStatus.Unemployed(1), savings = PLN(30000.0), rent = PLN(1800.0))
    val openingSavings = hh.savings
    val result         = step(Vector(hh), mkWorld(), PLN(8000.0), PLN(4666.0), Share(0.4), rng)
    val updated        = result.households
    val agg            = result.aggregates

    agg.consumption should be > PLN.Zero
    updated(0).savings should be < openingSavings
  }

  it should "return ledger-facing financial balances in the step result" in {
    val rng     = RandomStream.seeded(42)
    val hh      = mkHousehold(0, HhStatus.Employed(FirmId(0), SectorIdx(2), PLN(8000.0)), savings = PLN(50000.0))
    val result  = step(Vector(hh), mkWorld(), PLN(8000.0), PLN(4666.0), Share(0.4), rng)
    val updated = result.households.head

    result.financialStocks.head shouldBe Household.FinancialStocks(
      demandDeposit = updated.savings,
      mortgageLoan = updated.debt,
      consumerLoan = updated.consumerDebt,
      equity = updated.equityWealth,
    )
  }

  // --- Variable-rate debt service + deposit interest ---

  "Household.step with bankRates" should "use variable lending rate for debt service" in {
    val rng         = RandomStream.seeded(42)
    val debt        = PLN(100000.0)
    val hhs         = Vector(
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
    val br          = BankRates(
      lendingRates = Vector(Rate(0.06), Rate(0.10)),
      depositRates = Vector(Rate(0.04), Rate(0.04)),
    )
    val maybePbf    =
      step(hhs, mkWorld(), PLN(8000.0), PLN(4666.0), Share(0.4), rng, nBanks = 2, bankRates = Some(br)).perBankFlows
    val pbf         = maybePbf.get
    // Expected debt service: debt * (HhBaseAmortRate + lendingRate/12)
    val expectedDs0 = td.toDouble(debt) * (td.toDouble(p.household.baseAmortRate) + 0.06 / 12.0)
    val expectedDs1 = td.toDouble(debt) * (td.toDouble(p.household.baseAmortRate) + 0.10 / 12.0)
    pbf(0).debtService shouldBe PLN(expectedDs0) +- PLN(10.0)
    pbf(1).debtService shouldBe PLN(expectedDs1) +- PLN(10.0)
    // Bank 1's higher rate should mean higher debt service
    pbf(1).debtService should be > pbf(0).debtService
  }

  it should "pay deposit interest to HH with positive savings" in {
    val rng            = RandomStream.seeded(42)
    val savings        = PLN(100000.0)
    val hhs            = Vector(
      mkHousehold(0, HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(8000.0)), savings = savings, bankId = 0),
    )
    val depRate        = 0.04 // 4% annual
    val br             = BankRates(
      lendingRates = Vector(Rate(0.07)),
      depositRates = Vector(Rate(depRate)),
    )
    val result         =
      step(hhs, mkWorld(), PLN(8000.0), PLN(4666.0), Share(0.4), rng, nBanks = 1, bankRates = Some(br))
    val agg            = result.aggregates
    val maybePbf       = result.perBankFlows
    val pbf            = maybePbf.get
    val expectedDepInt = depRate / 12.0 * td.toDouble(savings)
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
    val agg            = step(hhs, mkWorld(), PLN(wage), PLN(4666.0), Share(0.4), rng, nBanks = 1, bankRates = Some(br)).aggregates
    val expectedDepInt = depRate / 12.0 * td.toDouble(savings)
    val grossIncome    = wage + expectedDepInt
    val pitTax         = Household.computeMonthlyPit(PLN(grossIncome))
    // totalIncome = grossIncome - PIT + socialTransfer (0 children → no transfer)
    agg.totalIncome shouldBe PLN(grossIncome - td.toDouble(pitTax)) +- PLN(20.0)
  }

  it should "accumulate per-bank flows correctly for 2 banks" in {
    val rng        = RandomStream.seeded(42)
    val hhs        = Vector(
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
    val br         = BankRates(
      lendingRates = Vector(Rate(0.07), Rate(0.08)),
      depositRates = Vector(Rate(0.035), Rate(0.035)),
    )
    val maybePbf   =
      step(hhs, mkWorld(), PLN(8000.0), PLN(4666.0), Share(0.4), rng, nBanks = 2, bankRates = Some(br)).perBankFlows
    val pbf        = maybePbf.get
    // Bank 0 has HH 0 and 1: income should include both
    pbf(0).income should be > PLN.Zero
    pbf(1).income should be > PLN.Zero
    // Bank 0 deposit interest: (50000 + 30000) * 0.035/12
    val expDepInt0 = (50000.0 + 30000.0) * 0.035 / 12.0
    pbf(0).depositInterest shouldBe PLN(expDepInt0) +- PLN(10.0)
    // Bank 1 deposit interest: 80000 * 0.035/12
    val expDepInt1 = 80000.0 * 0.035 / 12.0
    pbf(1).depositInterest shouldBe PLN(expDepInt1) +- PLN(10.0)
  }

  it should "not pay deposit interest on negative savings" in {
    val rng      = RandomStream.seeded(42)
    val hhs      = Vector(
      mkHousehold(0, HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(8000.0)), savings = PLN(-5000.0), bankId = 0),
    )
    val br       = BankRates(
      lendingRates = Vector(Rate(0.07)),
      depositRates = Vector(Rate(0.04)),
    )
    val result   =
      step(hhs, mkWorld(), PLN(8000.0), PLN(4666.0), Share(0.4), rng, nBanks = 1, bankRates = Some(br))
    val agg      = result.aggregates
    val maybePbf = result.perBankFlows
    val pbf      = maybePbf.get
    // Deposit interest on negative savings is floored at 0
    pbf(0).depositInterest shouldBe PLN.Zero
    agg.totalDepositInterest shouldBe PLN.Zero
  }

  // --- Immigration: remittance deduction ---

  "Household.step" should "not deduct remittances from non-immigrant HH" in {
    val rng  = RandomStream.seeded(42)
    val wage = 8000.0
    val hhs  = Vector(
      mkHousehold(0, HhStatus.Employed(FirmId(0), SectorIdx(2), PLN(wage)), savings = PLN(50000.0))
        .copy(isImmigrant = false),
    )
    val agg  = step(hhs, mkWorld(), PLN(wage), PLN(4666.0), Share(0.4), rng).aggregates
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
    val agg = computeAggregates(hhs, PLN(8000.0), PLN(4666.0), Share(0.4), 0, 0)
    agg.employed shouldBe 2
    agg.unemployed shouldBe 1
    agg.retraining shouldBe 1
    agg.bankrupt shouldBe 1
    agg.bankruptcyRate shouldBe Share(0.2) +- Share(0.001)
  }

  // --- helpers ---

  private def mkFirms(n: Int): Vector[Firm.State] =
    (0 until n).map { i =>
      TestFirmState(
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
    val hh = TestHouseholdState(
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
    financialById.update(id, TestHouseholdState.financial(savings = savings, debt = debt, consumerDebt = PLN.Zero, equityWealth = PLN.Zero))
    hh

  private def mkWorld(): World =
    Generators.testWorld(
      totalPopulation = 100000,
      employed = 100000,
      forex = OpenEconomy.ForexState(ExchangeRate(4.33), PLN(0.0), PLN(190000000), PLN(0.0), PLN(0.0)),
      marketWage = p.household.baseWage,
      reservationWage = p.household.baseReservationWage,
    )
