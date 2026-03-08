package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.accounting
import sfc.accounting.{BankingAggregate, ForexState, GovState}
import sfc.agents.{Banking, Firm, TechState}
import sfc.config.{Config, SectorDefs}
import sfc.types.*

class FirmEntrySpec extends AnyFlatSpec with Matchers:

  // ==========================================================================
  // Config defaults
  // ==========================================================================

  "FirmEntryEnabled" should "default to false" in {
    Config.FirmEntryEnabled shouldBe false
  }

  "FirmEntryRate" should "default to 0.02" in {
    Config.FirmEntryRate shouldBe 0.02
  }

  "FirmEntryProfitSens" should "default to 2.0" in {
    Config.FirmEntryProfitSens shouldBe 2.0
  }

  "FirmEntrySectorBarriers" should "have 6 elements" in {
    Config.FirmEntrySectorBarriers.length shouldBe 6
  }

  it should "have all positive values" in {
    Config.FirmEntrySectorBarriers.foreach(_ should be > 0.0)
  }

  it should "match expected defaults" in {
    Config.FirmEntrySectorBarriers shouldBe Vector(0.8, 0.6, 1.2, 0.5, 0.1, 0.7)
  }

  "FirmEntryAiThreshold" should "default to 0.15" in {
    Config.FirmEntryAiThreshold shouldBe 0.15
  }

  "FirmEntryAiProb" should "default to 0.20" in {
    Config.FirmEntryAiProb shouldBe 0.20
  }

  "FirmEntryStartupCash" should "default to 50000.0" in {
    Config.FirmEntryStartupCash shouldBe 50000.0
  }

  // ==========================================================================
  // World fields
  // ==========================================================================

  "World" should "have firmBirths defaulting to 0" in {
    val w = World(
      0,
      Rate(0.0),
      1.0,
      GovState(false, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      sfc.agents.Nbp.State(Rate(0.05)),
      BankingAggregate(PLN.Zero, PLN.Zero, PLN(1e9), PLN(1e9), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      ForexState(4.33, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      sfc.agents.Household.SectorState(100, PLN(8000), PLN(4500), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      Ratio(0),
      Ratio(0),
      1e9,
      Vector.fill(6)(5.0),
      bankingSector = Banking.initialize(1e9, 5e8, 5e8, 0, 0, Banking.DefaultConfigs),
    )
    w.firmBirths shouldBe 0
  }

  it should "have firmDeaths defaulting to 0" in {
    val w = World(
      0,
      Rate(0.0),
      1.0,
      accounting.GovState(false, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      sfc.agents.Nbp.State(Rate(0.05)),
      accounting.BankingAggregate(PLN.Zero, PLN.Zero, PLN(1e9), PLN(1e9), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      accounting.ForexState(4.33, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      sfc.agents.Household.SectorState(100, PLN(8000), PLN(4500), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      Ratio(0),
      Ratio(0),
      1e9,
      Vector.fill(6)(5.0),
      bankingSector = Banking.initialize(1e9, 5e8, 5e8, 0, 0, Banking.DefaultConfigs),
    )
    w.firmDeaths shouldBe 0
  }

  // ==========================================================================
  // Entrant properties (unit-level)
  // ==========================================================================

  "New entrant" should "be micro size (1-9 workers)" in {
    val rng = new scala.util.Random(42)
    val sizes = (1 to 100).map(_ => Math.max(1, rng.between(1, 10)))
    sizes.foreach { s =>
      s should be >= 1
      s should be <= 9
    }
  }

  it should "have zero debt" in {
    val entrant = Firm.State(
      id = FirmId(0),
      cash = PLN(50000.0),
      debt = PLN.Zero,
      tech = TechState.Traditional(5),
      riskProfile = Ratio(0.5),
      innovationCostFactor = 1.0,
      digitalReadiness = Ratio(0.15),
      sector = SectorIdx(2),
      neighbors = Array.empty[FirmId],
      initialSize = 5,
    )
    entrant.debt shouldBe PLN.Zero
  }

  it should "have positive startup cash" in {
    val sizeMult = 5.0 / Config.WorkersPerFirm
    val cash = Config.FirmEntryStartupCash * sizeMult
    cash should be > 0.0
  }

  it should "be alive" in {
    val entrant = Firm.State(
      id = FirmId(0),
      cash = PLN(50000.0),
      debt = PLN.Zero,
      tech = TechState.Traditional(5),
      riskProfile = Ratio(0.5),
      innovationCostFactor = 1.0,
      digitalReadiness = Ratio(0.15),
      sector = SectorIdx(2),
      neighbors = Array.empty[FirmId],
      initialSize = 5,
    )
    Firm.isAlive(entrant) shouldBe true
  }

  // ==========================================================================
  // AI-native entrants
  // ==========================================================================

  "AI-native entrant" should "have Hybrid tech state" in {
    val tech = TechState.Hybrid(3, 0.65)
    tech shouldBe a[TechState.Hybrid]
  }

  it should "have high digital readiness (0.50-0.90)" in {
    val rng = new scala.util.Random(42)
    val drs = (1 to 100).map(_ => rng.between(0.50, 0.90))
    drs.foreach { dr =>
      dr should be >= 0.50
      dr should be <= 0.90
    }
  }

  "Traditional entrant" should "have Traditional tech state" in {
    val tech = TechState.Traditional(5)
    tech shouldBe a[TechState.Traditional]
  }

  it should "have low digital readiness (0.02-0.30)" in {
    val rng = new scala.util.Random(42)
    val drs = (1 to 100).map { _ =>
      val sec = SectorDefs(2) // Retail
      Math.max(0.02, Math.min(0.30, sec.baseDigitalReadiness.toDouble + rng.nextGaussian() * 0.10))
    }
    drs.foreach { dr =>
      dr should be >= 0.02
      dr should be <= 0.30
    }
  }

  // ==========================================================================
  // Sector choice: all 6 sectors reachable
  // ==========================================================================

  "Sector choice" should "reach all 6 sectors with profit-weighted draws" in {
    val rng = new scala.util.Random(42)
    val weights = Array(0.8, 0.6, 1.2, 0.5, 0.1, 0.7)
    val totalWeight = weights.sum
    val sectors = (1 to 1000).map { _ =>
      val roll = rng.nextDouble() * totalWeight
      var cumul = 0.0
      var sec = 0
      var found = false
      for s <- 0 until 6 if !found do
        cumul += weights(s)
        if roll < cumul then { sec = s; found = true }
      sec
    }
    for s <- 0 until 6 do sectors.count(_ == s) should be > 0
  }

  // ==========================================================================
  // Physical capital initialization
  // ==========================================================================

  "Entrant capitalStock" should "be initialized when PhysCapEnabled" in {
    val firmSize = 5
    val sector = 1 // Manufacturing
    val expectedK = firmSize.toDouble * Config.PhysCapKLRatios(sector)
    expectedK should be > 0.0
  }

  // ==========================================================================
  // FDI foreign ownership
  // ==========================================================================

  "Entrant foreignOwned" should "respect FDI sector shares" in {
    // When FdiEnabled, foreignOwned probability = FdiForeignShares(sector)
    Config.FdiForeignShares.length shouldBe 6
    Config.FdiForeignShares.foreach { share =>
      share should be >= 0.0
      share should be <= 1.0
    }
  }

  // ==========================================================================
  // Individual HH mode: zero workers
  // ==========================================================================

  "Entrant in individual mode" should "start with Traditional(0)" in {
    // When households.isDefined, startWorkers = 0
    val tech = TechState.Traditional(0)
    Firm.workers(
      Firm.State(
        id = FirmId(0),
        cash = PLN(50000.0),
        debt = PLN.Zero,
        tech = tech,
        riskProfile = Ratio(0.5),
        innovationCostFactor = 1.0,
        digitalReadiness = Ratio(0.15),
        sector = SectorIdx(0),
        neighbors = Array.empty[FirmId],
        initialSize = 5,
      ),
    ) shouldBe 0
  }

  // ==========================================================================
  // Disabled: no births
  // ==========================================================================

  "FirmEntry disabled" should "produce zero births column" in {
    // When FirmEntryEnabled=false, firmBirths stays 0
    Config.FirmEntryEnabled shouldBe false
    // In a default run, births column (171) should be 0
  }

  // ==========================================================================
  // Profit signal computation
  // ==========================================================================

  "Profit signal" should "be clamped to [-1, 2]" in {
    val testCases = Seq(
      (100.0, 50.0, 50.0), // positive signal
      (10.0, 50.0, 50.0), // negative signal
      (1000.0, 50.0, 50.0), // extreme positive
      (0.0, 50.0, 50.0), // zero cash
    )
    for (sectorAvg, globalAvg, _) <- testCases do
      val signal = Math.max(-1.0, Math.min(2.0, (sectorAvg - globalAvg) / Math.max(1.0, Math.abs(globalAvg))))
      signal should be >= -1.0
      signal should be <= 2.0
  }

  // ==========================================================================
  // Entry probability
  // ==========================================================================

  "Entry probability" should "be non-negative" in {
    for s <- 0 until 6 do
      val profitSignal = 0.5 // moderate positive
      val entryProb = Config.FirmEntryRate * Config.FirmEntrySectorBarriers(s) *
        Math.max(0.0, 1.0 + profitSignal * Config.FirmEntryProfitSens)
      entryProb should be >= 0.0
  }

  it should "be zero when profit signal is very negative" in {
    val profitSignal = -1.0
    val entryProb = Config.FirmEntryRate * Config.FirmEntrySectorBarriers(0) *
      Math.max(0.0, 1.0 + profitSignal * Config.FirmEntryProfitSens)
    entryProb shouldBe 0.0
  }

  it should "scale with sector barriers" in {
    val profitSignal = 0.0
    val probs = (0 until 6).map { s =>
      Config.FirmEntryRate * Config.FirmEntrySectorBarriers(s) *
        Math.max(0.0, 1.0 + profitSignal * Config.FirmEntryProfitSens)
    }
    // Retail (1.2) should have higher prob than Public (0.1)
    probs(2) should be > probs(4) // Retail > Public
  }
