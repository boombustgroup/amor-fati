package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import org.scalatest.flatspec.AnyFlatSpec
import com.boombustgroup.amorfati.Generators
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.*
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.markets.OpenEconomy
import com.boombustgroup.amorfati.types.*

import com.boombustgroup.amorfati.random.RandomStream

class FirmSpec extends AnyFlatSpec with Matchers:

  given SimParams              = SimParams.defaults
  private val p: SimParams     = summon[SimParams]
  private val ExecutionMonth31 = ExecutionMonth(31)

  private def hiringDiagnostics(firm: Firm.State, world: World): Firm.HiringDiagnostics =
    Firm.hiringDiagnostics(firm, world, OperationalSignals.fromDecisionSignals(world.seedIn, world.pipeline.operationalHiringSlack))

  private def process(
      firm: Firm.State,
      world: World,
      lendRate: Rate,
      bankCanLend: PLN => Boolean,
      allFirms: Vector[Firm.State],
      rng: RandomStream,
  ): Firm.Result =
    Firm.process(
      firm,
      world,
      ExecutionMonth31,
      OperationalSignals.fromDecisionSignals(world.seedIn, world.pipeline.operationalHiringSlack),
      lendRate,
      bankCanLend,
      allFirms,
      rng,
      PLN.Zero,
    )

  // --- Firm.isAlive ---

  "Firm.isAlive" should "return true for Traditional" in {
    Firm.isAlive(mkFirm(TechState.Traditional(10))) shouldBe true
  }

  it should "return true for Hybrid" in {
    Firm.isAlive(mkFirm(TechState.Hybrid(5, Multiplier(1.2)))) shouldBe true
  }

  it should "return true for Automated" in {
    Firm.isAlive(mkFirm(TechState.Automated(Multiplier(1.5)))) shouldBe true
  }

  it should "return false for Bankrupt" in {
    Firm.isAlive(mkFirm(TechState.Bankrupt(BankruptReason.Other("test")))) shouldBe false
  }

  // --- Firm.workers ---

  "Firm.workerCount" should "return workers for Traditional" in {
    Firm.workerCount(mkFirm(TechState.Traditional(10))) shouldBe 10
  }

  it should "return workers for Hybrid" in {
    Firm.workerCount(mkFirm(TechState.Hybrid(7, Multiplier.One))) shouldBe 7
  }

  it should "return skeletonCrew for Automated" in {
    val f = mkFirm(TechState.Automated(Multiplier(1.5)))
    Firm.workerCount(f) shouldBe Firm.skeletonCrew(f)
  }

  it should "return 0 for Bankrupt" in {
    Firm.workerCount(mkFirm(TechState.Bankrupt(BankruptReason.Other("test")))) shouldBe 0
  }

  "Firm.applyOperationalHiringSlack" should "compress worker targets when aggregate labor plans exceed supply" in {
    Firm.applyOperationalHiringSlack(rawTarget = 24, minWorkers = 3, slackFactor = Multiplier(0.5)) shouldBe 12
  }

  it should "respect the workforce floor when compression is severe" in {
    Firm.applyOperationalHiringSlack(rawTarget = 4, minWorkers = 3, slackFactor = Multiplier(0.25)) shouldBe 3
  }

  "Firm.hiringDiagnostics" should "delay non-micro hiring until demand persists" in {
    val world       = mkWorld().copy(
      pipeline = PipelineState(
        sectorDemandMult = Vector.fill(p.sectorDefs.length)(Multiplier.One),
        sectorDemandPressure = Vector.fill(p.sectorDefs.length)(Multiplier(1.75)),
        sectorHiringSignal = Vector.fill(p.sectorDefs.length)(Multiplier(1.75)),
        operationalHiringSlack = Share.One,
      ),
      flows = FlowState.zero,
    )
    val firstSignal = hiringDiagnostics(mkFirm(TechState.Traditional(8), sector = 3), world)
    firstSignal.desiredWorkers should be > firstSignal.workers
    firstSignal.feasibleWorkers shouldBe firstSignal.workers
    firstSignal.signalMonths shouldBe 1
    firstSignal.requiredSignalMonths shouldBe 2

    val persistedSignal = hiringDiagnostics(mkFirm(TechState.Traditional(8), sector = 3).copy(hiringSignalMonths = 1), world)
    persistedSignal.desiredWorkers should be > persistedSignal.workers
    persistedSignal.feasibleWorkers should be > persistedSignal.workers
    persistedSignal.signalMonths shouldBe 2
  }

  it should "allow micro firms to react on the first sustained positive gap" in {
    val world = mkWorld().copy(
      pipeline = PipelineState(
        sectorDemandMult = Vector.fill(p.sectorDefs.length)(Multiplier.One),
        sectorDemandPressure = Vector.fill(p.sectorDefs.length)(Multiplier(1.75)),
        sectorHiringSignal = Vector.fill(p.sectorDefs.length)(Multiplier(1.75)),
        operationalHiringSlack = Share.One,
      ),
      flows = FlowState.zero,
    )
    val diag  = hiringDiagnostics(mkFirm(TechState.Traditional(4), sector = 3), world)
    diag.desiredWorkers should be > diag.workers
    diag.feasibleWorkers should be > diag.workers
    diag.requiredSignalMonths shouldBe 1
  }

  it should "keep startup firms on their startup staffing target even under weak demand" in {
    val world   = mkWorld().copy(
      pipeline = PipelineState(
        sectorDemandMult = Vector.fill(p.sectorDefs.length)(Multiplier(0.8)),
        sectorDemandPressure = Vector.fill(p.sectorDefs.length)(Multiplier(0.8)),
        sectorHiringSignal = Vector.fill(p.sectorDefs.length)(Multiplier(0.8)),
        operationalHiringSlack = Share.One,
      ),
      flows = FlowState.zero,
    )
    val startup = mkFirm(TechState.Traditional(1), sector = 3).copy(startupMonthsLeft = 4, startupTargetWorkers = 3)
    val diag    = hiringDiagnostics(startup, world)
    diag.desiredWorkers shouldBe 3
    diag.feasibleWorkers shouldBe 2
    diag.signalMonths shouldBe 1
  }

  it should "prefer explicit OperationalSignals over bridged world signal fields" in {
    val weakWorld         = mkWorld().copy(
      pipeline = PipelineState(
        sectorDemandMult = Vector.fill(p.sectorDefs.length)(Multiplier(0.4)),
        sectorDemandPressure = Vector.fill(p.sectorDefs.length)(Multiplier(0.4)),
        sectorHiringSignal = Vector.fill(p.sectorDefs.length)(Multiplier(0.4)),
        operationalHiringSlack = Share.One,
      ),
      flows = FlowState.zero,
    )
    val strongOperational = OperationalSignals(
      sectorDemandMult = Vector.fill(p.sectorDefs.length)(Multiplier(1.75)),
      sectorDemandPressure = Vector.fill(p.sectorDefs.length)(Multiplier(1.75)),
      sectorHiringSignal = Vector.fill(p.sectorDefs.length)(Multiplier(1.75)),
      operationalHiringSlack = Share.One,
    )
    val firm              = mkFirm(TechState.Traditional(8), sector = 3).copy(hiringSignalMonths = 1)
    val bridged           = hiringDiagnostics(firm, weakWorld)
    val explicit          = Firm.hiringDiagnostics(firm, weakWorld, strongOperational)

    explicit.desiredWorkers should be > bridged.desiredWorkers
    explicit.feasibleWorkers should be >= bridged.feasibleWorkers
  }

  "Firm.hasWorkingCapitalGrace" should "give startups a larger temporary liquidity runway" in {
    val startup   = mkFirm(TechState.Traditional(2), sector = 3).copy(startupMonthsLeft = 4, startupTargetWorkers = 3)
    val incumbent = mkFirm(TechState.Traditional(2), sector = 3)
    val pnl       = Firm.PnL(
      revenue = PLN(1000.0),
      costs = PLN(900.0),
      tax = PLN.Zero,
      netAfterTax = PLN(100.0),
      profitShiftCost = PLN.Zero,
      energyCost = PLN.Zero,
      newAccumulatedLoss = PLN.Zero,
    )
    val cashGap   = PLN(-15000.0)
    Firm.hasWorkingCapitalGrace(startup, pnl, cashGap) shouldBe true
    Firm.hasWorkingCapitalGrace(incumbent, pnl, cashGap) shouldBe false
  }

  "Firm.canFundUpsize" should "let startups hire against their startup runway" in {
    val startup         = mkFirm(TechState.Traditional(2), sector = 3).copy(startupMonthsLeft = 4, startupTargetWorkers = 4)
    val incumbent       = mkFirm(TechState.Traditional(2), sector = 3)
    val pnl             = Firm.PnL(
      revenue = PLN(1000.0),
      costs = PLN(1000.0),
      tax = PLN.Zero,
      netAfterTax = PLN.Zero,
      profitShiftCost = PLN.Zero,
      energyCost = PLN.Zero,
      newAccumulatedLoss = PLN.Zero,
    )
    val cashAfterHiring = PLN(-12000.0)
    Firm.canFundUpsize(startup, pnl, cashAfterHiring, addedWorkers = 1, marketWage = p.household.baseWage) shouldBe true
    Firm.canFundUpsize(incumbent, pnl, cashAfterHiring, addedWorkers = 1, marketWage = p.household.baseWage) shouldBe false
  }

  // --- Firm.capacity ---

  "Firm.computeCapacity" should "be positive for alive firms" in {
    Firm.computeCapacity(mkFirm(TechState.Traditional(10))) should be > PLN.Zero
    Firm.computeCapacity(mkFirm(TechState.Hybrid(5, Multiplier(1.2)))) should be > PLN.Zero
    Firm.computeCapacity(mkFirm(TechState.Automated(Multiplier(1.5)))) should be > PLN.Zero
  }

  it should "be 0 for Bankrupt" in {
    Firm.computeCapacity(mkFirm(TechState.Bankrupt(BankruptReason.Other("test")))) shouldBe PLN.Zero
  }

  // --- Firm.aiCapex / hybridCapex ---

  "Firm.computeAiCapex" should "be positive and scale with multipliers" in {
    val f  = mkFirm(TechState.Traditional(10))
    Firm.computeAiCapex(f) should be > PLN.Zero
    // With higher innovationCostFactor → higher capex
    val f2 = f.copy(innovationCostFactor = Multiplier(1.5))
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
    val thresholds    = sigmasOrdered.map(s => Firm.sigmaThreshold(Sigma(s)))
    for i <- 0 until thresholds.length - 1 do thresholds(i) should be <= thresholds(i + 1)
  }

  it should "be bounded in [0, 1]" in {
    for s <- p.sectorDefs do
      val t = Firm.sigmaThreshold(s.sigma)
      t.bd should be >= BigDecimal(0)
      t.bd should be <= BigDecimal("1.0")
  }

  // --- Firm.localAutoRatio ---

  "Firm.computeLocalAutoRatio" should "return 0.0 when no automated neighbors" in {
    val firms = Vector(
      mkFirmWithNeighbors(0, TechState.Traditional(10), Vector(FirmId(1), FirmId(2))),
      mkFirmWithNeighbors(1, TechState.Traditional(10), Vector(FirmId(0))),
      mkFirmWithNeighbors(2, TechState.Traditional(10), Vector(FirmId(0))),
    )
    Firm.computeLocalAutoRatio(firms(0), firms) shouldBe Share.Zero
  }

  it should "return 1.0 when all neighbors are Automated" in {
    val firms = Vector(
      mkFirmWithNeighbors(0, TechState.Traditional(10), Vector(FirmId(1), FirmId(2))),
      mkFirmWithNeighbors(1, TechState.Automated(Multiplier(1.2)), Vector(FirmId(0))),
      mkFirmWithNeighbors(2, TechState.Automated(Multiplier(1.1)), Vector(FirmId(0))),
    )
    Firm.computeLocalAutoRatio(firms(0), firms) shouldBe Share.One
  }

  it should "count Hybrid as automated in ratio" in {
    val firms = Vector(
      mkFirmWithNeighbors(0, TechState.Traditional(10), Vector(FirmId(1), FirmId(2), FirmId(3))),
      mkFirmWithNeighbors(1, TechState.Automated(Multiplier(1.2)), Vector(FirmId(0))),
      mkFirmWithNeighbors(2, TechState.Hybrid(5, Multiplier.One), Vector(FirmId(0))),
      mkFirmWithNeighbors(3, TechState.Traditional(10), Vector(FirmId(0))),
    )
    Firm.computeLocalAutoRatio(firms(0), firms).bd shouldBe (BigDecimal(2) / 3 +- BigDecimal("0.001"))
  }

  "Firm.adoptionWillingnessMultiplier" should "increase with local demonstration effects above threshold" in {
    val below = Firm.adoptionWillingnessMultiplier(month = ExecutionMonth(12), localAuto = Share(0.30))
    val above = Firm.adoptionWillingnessMultiplier(month = ExecutionMonth(12), localAuto = Share(0.80))

    above.should(be > below)
  }

  it should "increase over time until the ramp saturates" in {
    val early = Firm.adoptionWillingnessMultiplier(month = ExecutionMonth.First, localAuto = Share.Zero)
    val mid   = Firm.adoptionWillingnessMultiplier(month = ExecutionMonth(18), localAuto = Share.Zero)
    val late  = Firm.adoptionWillingnessMultiplier(month = ExecutionMonth(72), localAuto = Share.Zero)

    mid.should(be > early)
    late.should(be >= mid)
  }

  it should "start at the documented base willingness in the first execution month" in {
    val first = Firm.adoptionWillingnessMultiplier(month = ExecutionMonth.First, localAuto = Share.Zero)

    first.shouldBe(Share(0.15))
  }

  it should "return 0.0 for firm with no neighbors" in {
    val firms = Vector(mkFirmWithNeighbors(0, TechState.Traditional(10), Vector.empty[FirmId]))
    Firm.computeLocalAutoRatio(firms(0), firms) shouldBe Share.Zero
  }

  "Firm.computePnL" should "keep ETS surcharge at baseline in the first execution month" in {
    val firm         = mkFirm(TechState.Traditional(10), sector = 1)
    val commodity    = PriceIndex.Base
    val pnl          = Firm.computePnL(
      firm = firm,
      wage = p.household.baseWage,
      sectorDemandMult = Multiplier.One,
      domesticPrice = PriceIndex.Base,
      importPrice = PriceIndex.Base,
      commodityPrice = commodity,
      lendRate = Rate.Zero,
      month = ExecutionMonth.First,
    )
    val baseEnergy   = pnl.revenue * p.climate.energyCostShares(firm.sector.toInt)
    val expectedCost = commodity * baseEnergy

    pnl.energyCost shouldBe expectedCost
  }

  // --- Firm.process ---

  "Firm.process" should "keep a Bankrupt firm bankrupt with zero tax/capex" in {
    val f      = mkFirm(TechState.Bankrupt(BankruptReason.Other("test")))
    val result = process(f, mkWorld(), Rate(0.07), _ => true, Vector(f), RandomStream.seeded(42))
    result.taxPaid shouldBe PLN.Zero
    result.capexSpent shouldBe PLN.Zero
    result.firm.tech shouldBe a[TechState.Bankrupt]
  }

  it should "keep an Automated firm alive with large cash" in {
    val f      = mkFirm(TechState.Automated(Multiplier(1.5))).copy(cash = PLN(10000000.0))
    val result = process(f, mkWorld(), Rate(0.07), _ => true, Vector(f), RandomStream.seeded(42))
    Firm.isAlive(result.firm) shouldBe true
  }

  it should "bankrupt an Automated firm with negative cash when P&L is negative" in {
    // Very low cash + high price level = deep losses → bankrupt
    val f      = mkFirm(TechState.Automated(Multiplier(0.1))).copy(cash = PLN(-500000.0), debt = PLN(5000000.0))
    val baseW  = mkWorld()
    val w      = baseW.copy(
      priceLevel = PriceIndex(0.3),
      pipeline = baseW.pipeline.copy(
        sectorDemandMult = Vector.fill(baseW.pipeline.sectorDemandMult.length)(Multiplier(0.1)),
      ),
    )
    val result = process(f, w, Rate(0.20), _ => true, Vector(f), RandomStream.seeded(42))
    result.firm.tech shouldBe a[TechState.Bankrupt]
  }

  it should "use the carried informal adjustment from world state for CIT evasion" in {
    val firm       = mkFirm(TechState.Traditional(10), sector = 2).copy(cash = PLN(500000.0))
    val baseWorld  =
      mkWorld().copy(
        pipeline = mkWorld().pipeline.copy(
          sectorDemandMult = Vector.fill(p.sectorDefs.length)(Multiplier(2.5)),
          sectorDemandPressure = Vector.fill(p.sectorDefs.length)(Multiplier(2.5)),
          sectorHiringSignal = Vector.fill(p.sectorDefs.length)(Multiplier(2.5)),
        ),
      )
    val lowAdj     = baseWorld.copy(mechanisms = baseWorld.mechanisms.copy(informalCyclicalAdj = 0.0))
    val highAdj    = baseWorld.copy(mechanisms = baseWorld.mechanisms.copy(informalCyclicalAdj = 0.4))
    val lowResult  = process(firm, lowAdj, Rate(0.07), _ => true, Vector(firm), RandomStream.seeded(42))
    val highResult =
      process(firm, highAdj, Rate(0.07), _ => true, Vector(firm), RandomStream.seeded(42))

    lowResult.taxPaid should be > PLN.Zero
    highResult.taxPaid should be > PLN.Zero
    highResult.citEvasion should be > lowResult.citEvasion
  }

  // --- helpers ---

  private def mkFirmWithNeighbors(id: Int, tech: TechState, neighbors: Vector[FirmId]): Firm.State =
    Firm.State(
      FirmId(id),
      PLN(50000.0),
      PLN.Zero,
      tech,
      Share(0.5),
      Multiplier.One,
      Share(0.5),
      SectorIdx(0),
      neighbors,
      bankId = BankId(0),
      equityRaised = PLN.Zero,
      initialSize = 10,
      capitalStock = PLN.Zero,
      foreignOwned = false,
      inventory = PLN.Zero,
      greenCapital = PLN.Zero,
      accumulatedLoss = PLN.Zero,
    )

  private def mkFirm(tech: TechState, sector: Int = 2): Firm.State =
    Firm.State(
      FirmId(0),
      PLN(50000.0),
      PLN.Zero,
      tech,
      Share(0.5),
      Multiplier.One,
      Share(0.5),
      SectorIdx(sector),
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

  private def mkWorld(): World =
    Generators.testWorld(
      totalPopulation = 100000,
      employed = 100000,
      forex = OpenEconomy.ForexState(ExchangeRate(4.33), PLN.Zero, PLN(190000000), PLN.Zero, PLN.Zero),
      marketWage = p.household.baseWage,
      reservationWage = p.household.baseReservationWage,
    )
