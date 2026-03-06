package sfc.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.accounting.{BankState, ForexState, GovState}
import sfc.config.{Config, SECTORS}
import sfc.engine.World
import sfc.types.*

import scala.util.Random

class HouseholdSpec extends AnyFlatSpec with Matchers:

  // --- HouseholdInit ---

  "Household.Init.initialize" should "create correct number of households" in {
    val rng = new Random(42)
    val firms = mkFirms(100)
    val network = Array.fill(1000)(Array.empty[Int])
    val hhs = Household.Init.initialize(1000, 100, firms, network, rng)
    hhs.length shouldBe 1000
  }

  it should "start all households as Employed" in {
    val rng = new Random(42)
    val firms = mkFirms(100)
    val network = Array.fill(500)(Array.empty[Int])
    val hhs = Household.Init.initialize(500, 100, firms, network, rng)
    hhs.foreach { hh =>
      hh.status shouldBe a[HhStatus.Employed]
    }
  }

  it should "assign positive savings to all households" in {
    val rng = new Random(42)
    val firms = mkFirms(50)
    val network = Array.fill(200)(Array.empty[Int])
    val hhs = Household.Init.initialize(200, 50, firms, network, rng)
    hhs.foreach(_.savings.toDouble should be > 0.0)
  }

  it should "have MPC in [0.5, 0.98]" in {
    val rng = new Random(42)
    val firms = mkFirms(50)
    val network = Array.fill(500)(Array.empty[Int])
    val hhs = Household.Init.initialize(500, 50, firms, network, rng)
    hhs.foreach { hh =>
      hh.mpc.toDouble should be >= 0.5
      hh.mpc.toDouble should be <= 0.98
    }
  }

  it should "have skill in [0.3, 1.0]" in {
    val rng = new Random(42)
    val firms = mkFirms(50)
    val network = Array.fill(500)(Array.empty[Int])
    val hhs = Household.Init.initialize(500, 50, firms, network, rng)
    hhs.foreach { hh =>
      hh.skill.toDouble should be >= 0.3
      hh.skill.toDouble should be <= 1.0
    }
  }

  it should "have rent >= floor" in {
    val rng = new Random(42)
    val firms = mkFirms(50)
    val network = Array.fill(500)(Array.empty[Int])
    val hhs = Household.Init.initialize(500, 50, firms, network, rng)
    hhs.foreach(_.monthlyRent.toDouble should be >= Config.HhRentFloor)
  }

  // --- Household.step ---

  "Household.step" should "not change bankrupt households" in {
    val rng = new Random(42)
    val hhs = Vector(
      mkHousehold(0, HhStatus.Bankrupt, savings = PLN(0.0)),
      mkHousehold(1, HhStatus.Employed(FirmId(0), SectorIdx(2), PLN(8000.0)), savings = PLN(50000.0))
    )
    val (updated, _, _) = Household.step(hhs, mkWorld(), 0.0, 8000.0, 4666.0, 0.4, rng)
    updated(0).status shouldBe HhStatus.Bankrupt
  }

  it should "increase unemployment months for unemployed" in {
    val rng = new Random(42)
    val hhs = Vector(
      mkHousehold(0, HhStatus.Unemployed(3), savings = PLN(50000.0))
    )
    val (updated, _, _) = Household.step(hhs, mkWorld(), 2000.0, 8000.0, 4666.0, 0.4, rng)
    updated(0).status match
      case HhStatus.Unemployed(m) => m should be >= 4
      case HhStatus.Retraining(_, _, _) => succeed  // may enter retraining
      case HhStatus.Bankrupt => succeed  // may go bankrupt
      case other => fail(s"Unexpected status: $other")
  }

  it should "apply skill decay after scarring onset" in {
    val rng = new Random(42)
    val hh = mkHousehold(0, HhStatus.Unemployed(5), savings = PLN(100000.0), skill = 0.8)
    val (updated, _, _) = Household.step(Vector(hh), mkWorld(), 2000.0, 8000.0, 4666.0, 0.4, rng)
    updated(0).skill.toDouble should be < 0.8
  }

  it should "not decay skill before scarring onset" in {
    val rng = new Random(42)
    val hh = mkHousehold(0, HhStatus.Unemployed(1), savings = PLN(100000.0), skill = 0.8)
    val (updated, _, _) = Household.step(Vector(hh), mkWorld(), 2000.0, 8000.0, 4666.0, 0.4, rng)
    updated(0).skill shouldBe Ratio(0.8)
  }

  it should "apply health scarring after onset" in {
    val rng = new Random(42)
    val hh = mkHousehold(0, HhStatus.Unemployed(5), savings = PLN(100000.0), healthPenalty = 0.0)
    val (updated, _, _) = Household.step(Vector(hh), mkWorld(), 2000.0, 8000.0, 4666.0, 0.4, rng)
    updated(0).healthPenalty.toDouble should be > 0.0
  }

  it should "bankrupt household when savings fall below threshold" in {
    val rng = new Random(42)
    val hh = mkHousehold(0, HhStatus.Unemployed(1), savings = PLN(-10000.0), rent = PLN(1800.0))
    val (updated, _, _) = Household.step(Vector(hh), mkWorld(), 0.0, 8000.0, 4666.0, 0.4, rng)
    updated(0).status shouldBe HhStatus.Bankrupt
  }

  it should "return None for perBankHhFlows when bankRates not provided" in {
    val rng = new Random(42)
    val hhs = Vector(mkHousehold(0, HhStatus.Employed(FirmId(0), SectorIdx(2), PLN(8000.0)), savings = PLN(50000.0)))
    val (_, _, pbf) = Household.step(hhs, mkWorld(), 2000.0, 8000.0, 4666.0, 0.4, rng)
    pbf shouldBe None
  }

  // --- Variable-rate debt service + deposit interest ---

  "Household.step with bankRates" should "use variable lending rate for debt service" in {
    val rng = new Random(42)
    val debt = PLN(100000.0)
    val hhs = Vector(
      mkHousehold(0, HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(8000.0)), savings = PLN(50000.0), debt = debt, bankId = 0),
      mkHousehold(1, HhStatus.Employed(FirmId(1), SectorIdx(0), PLN(8000.0)), savings = PLN(50000.0), debt = debt, bankId = 1)
    )
    // Bank 0: 6% annual lending rate, Bank 1: 10% annual
    val br = BankRates(
      lendingRates = Array(0.06, 0.10),
      depositRates = Array(0.04, 0.04)
    )
    val (_, agg, Some(pbf)) = Household.step(
      hhs, mkWorld(), 2000.0, 8000.0, 4666.0, 0.4, rng, nBanks = 2, bankRates = Some(br))
    // Expected debt service: debt * (HhBaseAmortRate + lendingRate/12)
    val expectedDs0 = debt.toDouble * (Config.HhBaseAmortRate + 0.06 / 12.0)
    val expectedDs1 = debt.toDouble * (Config.HhBaseAmortRate + 0.10 / 12.0)
    pbf.debtService(0) shouldBe expectedDs0 +- 0.01
    pbf.debtService(1) shouldBe expectedDs1 +- 0.01
    // Bank 1's higher rate should mean higher debt service
    pbf.debtService(1) should be > pbf.debtService(0)
  }

  it should "pay deposit interest to HH with positive savings" in {
    val rng = new Random(42)
    val savings = PLN(100000.0)
    val hhs = Vector(
      mkHousehold(0, HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(8000.0)), savings = savings, bankId = 0)
    )
    val depRate = 0.04  // 4% annual
    val br = BankRates(
      lendingRates = Array(0.07),
      depositRates = Array(depRate)
    )
    val (_, agg, Some(pbf)) = Household.step(
      hhs, mkWorld(), 0.0, 8000.0, 4666.0, 0.4, rng, nBanks = 1, bankRates = Some(br))
    val expectedDepInt = depRate / 12.0 * savings.toDouble
    pbf.depositInterest(0) shouldBe expectedDepInt +- 0.01
    agg.totalDepositInterest.toDouble shouldBe expectedDepInt +- 0.01
  }

  it should "include deposit interest in totalIncome" in {
    val rng = new Random(42)
    val savings = PLN(200000.0)
    val wage = 8000.0
    val bdp = 2000.0
    val depRate = 0.04
    val hhs = Vector(
      mkHousehold(0, HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(wage)), savings = savings, bankId = 0)
    )
    val br = BankRates(
      lendingRates = Array(0.07),
      depositRates = Array(depRate)
    )
    val (_, agg, _) = Household.step(
      hhs, mkWorld(), bdp, wage, 4666.0, 0.4, rng, nBanks = 1, bankRates = Some(br))
    val expectedDepInt = depRate / 12.0 * savings.toDouble
    // totalIncome should include wage + bdp + deposit interest
    agg.totalIncome.toDouble shouldBe (wage + bdp + expectedDepInt) +- 0.01
  }

  it should "accumulate per-bank flows correctly for 2 banks" in {
    val rng = new Random(42)
    val hhs = Vector(
      mkHousehold(0, HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(8000.0)), savings = PLN(50000.0), debt = PLN(0.0), bankId = 0),
      mkHousehold(1, HhStatus.Employed(FirmId(1), SectorIdx(0), PLN(7000.0)), savings = PLN(30000.0), debt = PLN(0.0), bankId = 0),
      mkHousehold(2, HhStatus.Employed(FirmId(2), SectorIdx(0), PLN(9000.0)), savings = PLN(80000.0), debt = PLN(0.0), bankId = 1)
    )
    val br = BankRates(
      lendingRates = Array(0.07, 0.08),
      depositRates = Array(0.035, 0.035)
    )
    val (_, _, Some(pbf)) = Household.step(
      hhs, mkWorld(), 0.0, 8000.0, 4666.0, 0.4, rng, nBanks = 2, bankRates = Some(br))
    // Bank 0 has HH 0 and 1: income should include both
    pbf.income(0) should be > 0.0
    pbf.income(1) should be > 0.0
    // Bank 0 deposit interest: (50000 + 30000) * 0.035/12
    val expDepInt0 = (50000.0 + 30000.0) * 0.035 / 12.0
    pbf.depositInterest(0) shouldBe expDepInt0 +- 0.01
    // Bank 1 deposit interest: 80000 * 0.035/12
    val expDepInt1 = 80000.0 * 0.035 / 12.0
    pbf.depositInterest(1) shouldBe expDepInt1 +- 0.01
  }

  it should "not pay deposit interest on negative savings" in {
    val rng = new Random(42)
    val hhs = Vector(
      mkHousehold(0, HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(8000.0)), savings = PLN(-5000.0), bankId = 0)
    )
    val br = BankRates(
      lendingRates = Array(0.07),
      depositRates = Array(0.04)
    )
    val (_, agg, Some(pbf)) = Household.step(
      hhs, mkWorld(), 0.0, 8000.0, 4666.0, 0.4, rng, nBanks = 1, bankRates = Some(br))
    // Deposit interest on negative savings is floored at 0
    pbf.depositInterest(0) shouldBe 0.0
    agg.totalDepositInterest.toDouble shouldBe 0.0
  }

  // --- Immigration: remittance deduction ---

  "Household.step" should "not deduct remittances from non-immigrant HH" in {
    val rng = new Random(42)
    val wage = 8000.0
    val hhs = Vector(
      mkHousehold(0, HhStatus.Employed(FirmId(0), SectorIdx(2), PLN(wage)), savings = PLN(50000.0)).copy(isImmigrant = false)
    )
    val (updated, agg, _) = Household.step(hhs, mkWorld(), 2000.0, wage, 4666.0, 0.4, rng)
    agg.totalRemittances.toDouble shouldBe 0.0
  }

  it should "not deduct remittances from immigrant HH when disabled" in {
    val rng = new Random(42)
    val wage = 8000.0
    val hhs = Vector(
      mkHousehold(0, HhStatus.Employed(FirmId(0), SectorIdx(2), PLN(wage)), savings = PLN(50000.0)).copy(isImmigrant = true)
    )
    // ImmigEnabled is false by default → no remittance deduction
    val (updated, agg, _) = Household.step(hhs, mkWorld(), 2000.0, wage, 4666.0, 0.4, rng)
    agg.totalRemittances.toDouble shouldBe 0.0
  }

  it should "track totalRemittances in aggregates" in {
    val rng = new Random(42)
    val hhs = Vector(
      mkHousehold(0, HhStatus.Employed(FirmId(0), SectorIdx(2), PLN(8000.0)), savings = PLN(50000.0)).copy(isImmigrant = false),
      mkHousehold(1, HhStatus.Employed(FirmId(1), SectorIdx(2), PLN(7000.0)), savings = PLN(50000.0)).copy(isImmigrant = true)
    )
    // ImmigEnabled=false → totalRemittances=0 regardless of isImmigrant
    val (_, agg, _) = Household.step(hhs, mkWorld(), 0.0, 8000.0, 4666.0, 0.4, rng)
    agg.totalRemittances.toDouble shouldBe 0.0
  }

  // --- Household.giniSorted ---

  "Household.giniSorted" should "return 0 for equal values" in {
    Household.giniSorted(Array(100.0, 100.0, 100.0, 100.0)) shouldBe 0.0 +- 0.001
  }

  it should "return 0 for single element" in {
    Household.giniSorted(Array(42.0)) shouldBe 0.0
  }

  it should "return value in [0, 1] for typical distribution" in {
    val values = Array(1000.0, 2000.0, 3000.0, 5000.0, 10000.0, 50000.0)
    val g = Household.giniSorted(values)
    g should be >= 0.0
    g should be <= 1.0
  }

  it should "increase with more inequality" in {
    val equal = Array(1000.0, 1000.0, 1000.0, 1000.0)
    val unequal = Array(0.0, 0.0, 0.0, 4000.0)
    Household.giniSorted(unequal) should be > Household.giniSorted(equal)
  }

  // --- Household.computeAggregates ---

  "Household.computeAggregates" should "count statuses correctly" in {
    val hhs = Vector(
      mkHousehold(0, HhStatus.Employed(FirmId(0), SectorIdx(2), PLN(8000.0))),
      mkHousehold(1, HhStatus.Employed(FirmId(1), SectorIdx(2), PLN(7000.0))),
      mkHousehold(2, HhStatus.Unemployed(3)),
      mkHousehold(3, HhStatus.Retraining(4, SectorIdx(1), PLN(5000.0))),
      mkHousehold(4, HhStatus.Bankrupt)
    )
    val agg = Household.computeAggregates(hhs, 8000.0, 4666.0, 0.4, 0, 0)
    agg.employed shouldBe 2
    agg.unemployed shouldBe 1
    agg.retraining shouldBe 1
    agg.bankrupt shouldBe 1
    agg.bankruptcyRate.toDouble shouldBe 0.2 +- 0.001
  }

  // --- helpers ---

  private def mkFirms(n: Int): Array[Firm.State] =
    (0 until n).map { i =>
      Firm.State(FirmId(i), PLN(50000.0), PLN(0.0), TechState.Traditional(10), Ratio(0.5), 1.0, Ratio(0.5),
        SectorIdx(i % SECTORS.length), Array.empty[Int])
    }.toArray

  private def mkHousehold(id: Int, status: HhStatus,
                          savings: PLN = PLN(20000.0), debt: PLN = PLN(0.0),
                          rent: PLN = PLN(1800.0), skill: Double = 0.7,
                          healthPenalty: Double = 0.0, mpc: Double = 0.82,
                          bankId: Int = 0): Household.State =
    Household.State(id, savings, debt, rent, Ratio(skill), Ratio(healthPenalty), Ratio(mpc), status, Array.empty[Int], BankId(bankId))

  private def mkWorld(): World =
    World(
      month = 31,
      inflation = Rate(0.02),
      priceLevel = 1.0,
      gov = GovState(false, PLN(0.0), PLN(0.0), PLN(0.0), PLN(0.0), PLN(0.0)),
      nbp = Nbp.State(Rate(0.0575)),
      bank = BankState(PLN(1000000), PLN(10000), PLN(500000), PLN(1000000)),
      forex = ForexState(4.33, PLN(0.0), PLN(190000000), PLN(0.0), PLN(0.0)),
      hh = Household.SectorState(100000, PLN(Config.BaseWage), PLN(Config.BaseReservationWage), PLN(0.0), PLN(0.0), PLN(0.0), PLN(0.0)),
      automationRatio = Ratio.Zero,
      hybridRatio = Ratio.Zero,
      gdpProxy = 1e9,
      currentSigmas = SECTORS.map(_.sigma).toVector
    )
