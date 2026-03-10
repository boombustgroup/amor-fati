package sfc.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.accounting.{BankingAggregate, ForexState, GovState}
import sfc.config.SimParams
import sfc.engine.*
import sfc.types.*

import scala.util.Random

class FirmSpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  // --- Firm.isAlive ---

  "Firm.isAlive" should "return true for Traditional" in {
    Firm.isAlive(mkFirm(TechState.Traditional(10))) shouldBe true
  }

  it should "return true for Hybrid" in {
    Firm.isAlive(mkFirm(TechState.Hybrid(5, 1.2))) shouldBe true
  }

  it should "return true for Automated" in {
    Firm.isAlive(mkFirm(TechState.Automated(1.5))) shouldBe true
  }

  it should "return false for Bankrupt" in {
    Firm.isAlive(mkFirm(TechState.Bankrupt(BankruptReason.Other("test")))) shouldBe false
  }

  // --- Firm.workers ---

  "Firm.workerCount" should "return workers for Traditional" in {
    Firm.workerCount(mkFirm(TechState.Traditional(10))) shouldBe 10
  }

  it should "return workers for Hybrid" in {
    Firm.workerCount(mkFirm(TechState.Hybrid(7, 1.0))) shouldBe 7
  }

  it should "return skeletonCrew for Automated" in {
    val f = mkFirm(TechState.Automated(1.5))
    Firm.workerCount(f) shouldBe Firm.skeletonCrew(f)
  }

  it should "return 0 for Bankrupt" in {
    Firm.workerCount(mkFirm(TechState.Bankrupt(BankruptReason.Other("test")))) shouldBe 0
  }

  // --- Firm.capacity ---

  "Firm.computeCapacity" should "be positive for alive firms" in {
    Firm.computeCapacity(mkFirm(TechState.Traditional(10))) should be > PLN.Zero
    Firm.computeCapacity(mkFirm(TechState.Hybrid(5, 1.2))) should be > PLN.Zero
    Firm.computeCapacity(mkFirm(TechState.Automated(1.5))) should be > PLN.Zero
  }

  it should "be 0 for Bankrupt" in {
    Firm.computeCapacity(mkFirm(TechState.Bankrupt(BankruptReason.Other("test")))) shouldBe PLN.Zero
  }

  // --- Firm.aiCapex / hybridCapex ---

  "Firm.computeAiCapex" should "be positive and scale with multipliers" in {
    val f  = mkFirm(TechState.Traditional(10))
    Firm.computeAiCapex(f) should be > PLN.Zero
    // With higher innovationCostFactor → higher capex
    val f2 = f.copy(innovationCostFactor = 1.5)
    Firm.computeAiCapex(f2) should be > Firm.computeAiCapex(f)
  }

  "Firm.computeHybridCapex" should "be positive" in {
    val f = mkFirm(TechState.Traditional(10))
    Firm.computeHybridCapex(f) should be > PLN.Zero
  }

  // --- Firm.sigmaThreshold ---

  "Firm.sigmaThreshold" should "be monotonically increasing with sigma" in {
    // Sectors ordered by sigma: Public(1.0) < Healthcare(2.0) < Agriculture(3.0) < Retail(5.0) < Manuf(10.0) < BPO(50.0)
    val sigmasOrdered = Vector(1.0, 2.0, 3.0, 5.0, 10.0, 50.0)
    val thresholds    = sigmasOrdered.map(Firm.sigmaThreshold)
    for i <- 0 until thresholds.length - 1 do thresholds(i) should be <= thresholds(i + 1)
  }

  it should "be bounded in [0, 1]" in {
    for s <- p.sectorDefs do
      val t = Firm.sigmaThreshold(s.sigma)
      t should be >= 0.0
      t should be <= 1.0
  }

  // --- Firm.localAutoRatio ---

  "Firm.computeLocalAutoRatio" should "return 0.0 when no automated neighbors" in {
    val firms = Vector(
      mkFirmWithNeighbors(0, TechState.Traditional(10), Vector(FirmId(1), FirmId(2))),
      mkFirmWithNeighbors(1, TechState.Traditional(10), Vector(FirmId(0))),
      mkFirmWithNeighbors(2, TechState.Traditional(10), Vector(FirmId(0))),
    )
    Firm.computeLocalAutoRatio(firms(0), firms) shouldBe 0.0
  }

  it should "return 1.0 when all neighbors are Automated" in {
    val firms = Vector(
      mkFirmWithNeighbors(0, TechState.Traditional(10), Vector(FirmId(1), FirmId(2))),
      mkFirmWithNeighbors(1, TechState.Automated(1.2), Vector(FirmId(0))),
      mkFirmWithNeighbors(2, TechState.Automated(1.1), Vector(FirmId(0))),
    )
    Firm.computeLocalAutoRatio(firms(0), firms) shouldBe 1.0
  }

  it should "count Hybrid as automated in ratio" in {
    val firms = Vector(
      mkFirmWithNeighbors(0, TechState.Traditional(10), Vector(FirmId(1), FirmId(2), FirmId(3))),
      mkFirmWithNeighbors(1, TechState.Automated(1.2), Vector(FirmId(0))),
      mkFirmWithNeighbors(2, TechState.Hybrid(5, 1.0), Vector(FirmId(0))),
      mkFirmWithNeighbors(3, TechState.Traditional(10), Vector(FirmId(0))),
    )
    Firm.computeLocalAutoRatio(firms(0), firms) shouldBe (2.0 / 3.0 +- 0.001)
  }

  it should "return 0.0 for firm with no neighbors" in {
    val firms = Vector(mkFirmWithNeighbors(0, TechState.Traditional(10), Vector.empty[FirmId]))
    Firm.computeLocalAutoRatio(firms(0), firms) shouldBe 0.0
  }

  // --- Firm.process ---

  "Firm.process" should "keep a Bankrupt firm bankrupt with zero tax/capex" in {
    val f      = mkFirm(TechState.Bankrupt(BankruptReason.Other("test")))
    val result = Firm.process(f, mkWorld(), Rate(0.07), _ => true, Vector(f), new Random(42))
    result.taxPaid shouldBe PLN.Zero
    result.capexSpent shouldBe PLN.Zero
    result.firm.tech shouldBe a[TechState.Bankrupt]
  }

  it should "keep an Automated firm alive with large cash" in {
    val f      = mkFirm(TechState.Automated(1.5)).copy(cash = PLN(10000000.0))
    val result = Firm.process(f, mkWorld(), Rate(0.07), _ => true, Vector(f), new Random(42))
    Firm.isAlive(result.firm) shouldBe true
  }

  it should "bankrupt an Automated firm with negative cash when P&L is negative" in {
    // Very low cash + high price level = deep losses → bankrupt
    val f      = mkFirm(TechState.Automated(0.1)).copy(cash = PLN(-500000.0), debt = PLN(5000000.0))
    val w      = mkWorld().copy(priceLevel = 0.3, flows = mkWorld().flows.copy(sectorDemandMult = Vector.fill(6)(0.1)))
    val result = Firm.process(f, w, Rate(0.20), _ => true, Vector(f), new Random(42))
    result.firm.tech shouldBe a[TechState.Bankrupt]
  }

  // --- helpers ---

  private def mkFirmWithNeighbors(id: Int, tech: TechState, neighbors: Vector[FirmId]): Firm.State =
    Firm.State(
      FirmId(id),
      PLN(50000.0),
      PLN.Zero,
      tech,
      Ratio(0.5),
      1.0,
      Ratio(0.5),
      SectorIdx(0),
      neighbors,
      bankId = BankId(0),
      equityRaised = PLN.Zero,
      initialSize = 10,
      capitalStock = PLN.Zero,
      bondDebt = PLN.Zero,
      foreignOwned = false,
      inventory = PLN.Zero,
      greenCapital = PLN.Zero,
    )

  private def mkFirm(tech: TechState, sector: Int = 2): Firm.State =
    Firm.State(
      FirmId(0),
      PLN(50000.0),
      PLN.Zero,
      tech,
      Ratio(0.5),
      1.0,
      Ratio(0.5),
      SectorIdx(sector),
      Vector.empty[FirmId],
      bankId = BankId(0),
      equityRaised = PLN.Zero,
      initialSize = 10,
      capitalStock = PLN.Zero,
      bondDebt = PLN.Zero,
      foreignOwned = false,
      inventory = PLN.Zero,
      greenCapital = PLN.Zero,
    )

  private def mkWorld(): World =
    World(
      month = 31,
      inflation = Rate(0.02),
      priceLevel = 1.0,
      gdpProxy = 1e9,
      currentSigmas = p.sectorDefs.map(_.sigma).toVector,
      totalPopulation = 100000,
      gov = GovState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      nbp = Nbp.State(Rate(0.0575), PLN.Zero, false, PLN.Zero, PLN.Zero, PLN.Zero),
      bank = BankingAggregate(PLN(1000000), PLN(10000), PLN(500000), PLN(1000000), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      bankingSector = Banking.initialize(PLN(1e9), PLN(5e8), PLN(5e8), PLN.Zero, PLN.Zero, Banking.DefaultConfigs),
      forex = ForexState(4.33, PLN.Zero, PLN(190000000), PLN.Zero, PLN.Zero),
      hhAgg = Household.Aggregates(
        employed = 100000,
        unemployed = 0,
        retraining = 0,
        bankrupt = 0,
        totalIncome = PLN.Zero,
        consumption = PLN.Zero,
        domesticConsumption = PLN.Zero,
        importConsumption = PLN.Zero,
        marketWage = PLN(p.household.baseWage.toDouble),
        reservationWage = PLN(p.household.baseReservationWage.toDouble),
        giniIndividual = Ratio.Zero,
        giniWealth = Ratio.Zero,
        meanSavings = PLN.Zero,
        medianSavings = PLN.Zero,
        povertyRate50 = Ratio.Zero,
        bankruptcyRate = Ratio.Zero,
        meanSkill = 0.0,
        meanHealthPenalty = 0.0,
        retrainingAttempts = 0,
        retrainingSuccesses = 0,
        consumptionP10 = PLN.Zero,
        consumptionP50 = PLN.Zero,
        consumptionP90 = PLN.Zero,
        meanMonthsToRuin = 0.0,
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
      ),
      social = SocialState.zero,
      financial = FinancialMarketsState.zero,
      external = ExternalState.zero,
      real = RealState.zero,
      mechanisms = MechanismsState.zero,
      plumbing = MonetaryPlumbingState.zero,
      flows = FlowState.zero,
    )
