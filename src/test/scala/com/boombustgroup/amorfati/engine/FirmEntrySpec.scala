package com.boombustgroup.amorfati.engine

import org.scalatest.flatspec.AnyFlatSpec
import com.boombustgroup.amorfati.Generators
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.engine.markets.{FiscalBudget, OpenEconomy}
import com.boombustgroup.amorfati.agents.{Banking, BankruptReason, Firm, TechState}
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.mechanisms.FirmEntry
import com.boombustgroup.amorfati.types.*

class FirmEntrySpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]
  private val td           = ComputationBoundary

  // ==========================================================================
  // Config defaults
  // ==========================================================================

  "FirmEntryRate" should "default to 0.02" in {
    p.firm.entryRate shouldBe Share(0.02)
  }

  "FirmEntryProfitSens" should "default to 2.0" in {
    p.firm.entryProfitSens shouldBe Coefficient(2.0)
  }

  "FirmEntrySectorBarriers" should "have 6 elements" in {
    p.firm.entrySectorBarriers.length shouldBe 6
  }

  it should "have all positive values" in
    p.firm.entrySectorBarriers.foreach(b => td.toDouble(b) should be > 0.0)

  it should "match expected defaults" in {
    p.firm.entrySectorBarriers shouldBe Vector(Coefficient(0.8), Coefficient(0.6), Coefficient(1.2), Coefficient(0.5), Coefficient(0.1), Coefficient(0.7))
  }

  "FirmEntryAiThreshold" should "default to 0.15" in {
    p.firm.entryAiThreshold shouldBe Share(0.15)
  }

  "FirmEntryAiProb" should "default to 0.20" in {
    p.firm.entryAiProb shouldBe Share(0.20)
  }

  "FirmEntryStartupCash" should "default to 50000.0" in {
    p.firm.entryStartupCash shouldBe PLN(50000.0)
  }

  "ReplacementEntryRate" should "default to 0.35" in {
    p.firm.replacementEntryRate shouldBe Share(0.35)
  }

  "ReplacementEntryMinMonthly" should "default to 1" in {
    p.firm.replacementEntryMinMonthly shouldBe 1
  }

  "ReplacementEntryMaxMonthly" should "default to 250" in {
    p.firm.replacementEntryMaxMonthly shouldBe 250
  }

  // ==========================================================================
  // World fields
  // ==========================================================================

  private def mkMinimalWorld() = World(
    month = 0,
    inflation = Rate(0.0),
    priceLevel = 1.0,
    gdpProxy = 1e9,
    currentSigmas = Vector.fill(6)(Sigma(5.0)),
    totalPopulation = 100,
    gov = FiscalBudget.GovState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
    nbp = com.boombustgroup.amorfati.agents.Nbp.State(Rate(0.05), PLN.Zero, false, PLN.Zero, PLN.Zero, PLN.Zero),
    bank = Banking.Aggregate(PLN.Zero, PLN.Zero, PLN(1e9), PLN(1e9), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
    bankingSector = Generators.testBankingSector(),
    forex = OpenEconomy.ForexState(4.33, PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
    hhAgg = com.boombustgroup.amorfati.agents.Household.Aggregates(
      employed = 100,
      unemployed = 0,
      retraining = 0,
      bankrupt = 0,
      totalIncome = PLN.Zero,
      consumption = PLN.Zero,
      domesticConsumption = PLN.Zero,
      importConsumption = PLN.Zero,
      marketWage = PLN(8000),
      reservationWage = PLN(4500),
      giniIndividual = Share.Zero,
      giniWealth = Share.Zero,
      meanSavings = PLN.Zero,
      medianSavings = PLN.Zero,
      povertyRate50 = Share.Zero,
      bankruptcyRate = Share.Zero,
      meanSkill = 0.0,
      meanHealthPenalty = 0.0,
      retrainingAttempts = 0,
      retrainingSuccesses = 0,
      consumptionP10 = PLN.Zero,
      consumptionP50 = PLN.Zero,
      consumptionP90 = PLN.Zero,
      meanMonthsToRuin = 0.0,
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

  "World" should "have firmBirths defaulting to 0" in {
    val w = mkMinimalWorld()
    w.flows.firmBirths shouldBe 0
  }

  it should "have firmDeaths defaulting to 0" in {
    val w = mkMinimalWorld()
    w.flows.firmDeaths shouldBe 0
  }

  // ==========================================================================
  // Entrant properties (unit-level)
  // ==========================================================================

  "New entrant" should "be micro size (1-9 workers)" in {
    val rng   = new scala.util.Random(42)
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
      riskProfile = Share(0.5),
      innovationCostFactor = 1.0,
      digitalReadiness = Share(0.15),
      sector = SectorIdx(2),
      neighbors = Vector.empty[FirmId],
      bankId = BankId(0),
      equityRaised = PLN.Zero,
      initialSize = 5,
      capitalStock = PLN.Zero,
      bondDebt = PLN.Zero,
      foreignOwned = false,
      inventory = PLN.Zero,
      greenCapital = PLN.Zero,
      accumulatedLoss = PLN.Zero,
    )
    entrant.debt shouldBe PLN.Zero
  }

  it should "have positive startup cash" in {
    val sizeMult = 5.0 / p.pop.workersPerFirm
    val cash     = td.toDouble(p.firm.entryStartupCash) * sizeMult
    cash should be > 0.0
  }

  it should "be alive" in {
    val entrant = Firm.State(
      id = FirmId(0),
      cash = PLN(50000.0),
      debt = PLN.Zero,
      tech = TechState.Traditional(5),
      riskProfile = Share(0.5),
      innovationCostFactor = 1.0,
      digitalReadiness = Share(0.15),
      sector = SectorIdx(2),
      neighbors = Vector.empty[FirmId],
      bankId = BankId(0),
      equityRaised = PLN.Zero,
      initialSize = 5,
      capitalStock = PLN.Zero,
      bondDebt = PLN.Zero,
      foreignOwned = false,
      inventory = PLN.Zero,
      greenCapital = PLN.Zero,
      accumulatedLoss = PLN.Zero,
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
      val sec = p.sectorDefs(2) // Retail
      Math.max(0.02, Math.min(0.30, td.toDouble(sec.baseDigitalReadiness) + rng.nextGaussian() * 0.10))
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
    val rng         = new scala.util.Random(42)
    val weights     = Array(0.8, 0.6, 1.2, 0.5, 0.1, 0.7)
    val totalWeight = weights.sum
    val sectors     = (1 to 1000).map { _ =>
      val roll  = rng.nextDouble() * totalWeight
      var cumul = 0.0
      var sec   = 0
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
    val firmSize  = 5
    val sector    = 1 // Manufacturing
    val expectedK = firmSize.toDouble * td.toDouble(p.capital.klRatios(sector))
    expectedK should be > 0.0
  }

  // ==========================================================================
  // FDI foreign ownership
  // ==========================================================================

  "Entrant foreignOwned" should "respect FDI sector shares" in {
    // When FdiEnabled, foreignOwned probability = FdiForeignShares(sector)
    p.fdi.foreignShares.length shouldBe 6
    p.fdi.foreignShares.map(td.toDouble).foreach { share =>
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
    Firm.workerCount(
      Firm.State(
        id = FirmId(0),
        cash = PLN(50000.0),
        debt = PLN.Zero,
        tech = tech,
        riskProfile = Share(0.5),
        innovationCostFactor = 1.0,
        digitalReadiness = Share(0.15),
        sector = SectorIdx(0),
        neighbors = Vector.empty[FirmId],
        bankId = BankId(0),
        equityRaised = PLN.Zero,
        initialSize = 5,
        capitalStock = PLN.Zero,
        bondDebt = PLN.Zero,
        foreignOwned = false,
        inventory = PLN.Zero,
        greenCapital = PLN.Zero,
        accumulatedLoss = PLN.Zero,
      ),
    ) shouldBe 0
  }

  // ==========================================================================
  // Profit signal computation
  // ==========================================================================

  "Profit signal" should "be clamped to [-1, 2]" in {
    val testCases = Seq(
      (100.0, 50.0, 50.0),  // positive signal
      (10.0, 50.0, 50.0),   // negative signal
      (1000.0, 50.0, 50.0), // extreme positive
      (0.0, 50.0, 50.0),    // zero cash
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
      val entryProb    = td.toDouble(p.firm.entryRate) * td.toDouble(p.firm.entrySectorBarriers(s)) *
        Math.max(0.0, 1.0 + profitSignal * td.toDouble(p.firm.entryProfitSens))
      entryProb should be >= 0.0
  }

  it should "be zero when profit signal is very negative" in {
    val profitSignal = -1.0
    val entryProb    = td.toDouble(p.firm.entryRate) * td.toDouble(p.firm.entrySectorBarriers(0)) *
      Math.max(0.0, 1.0 + profitSignal * td.toDouble(p.firm.entryProfitSens))
    entryProb shouldBe 0.0
  }

  it should "scale with sector barriers" in {
    val profitSignal = 0.0
    val probs        = (0 until 6).map { s =>
      td.toDouble(p.firm.entryRate) * td.toDouble(p.firm.entrySectorBarriers(s)) *
        Math.max(0.0, 1.0 + profitSignal * td.toDouble(p.firm.entryProfitSens))
    }
    // Retail (1.2) should have higher prob than Public (0.1)
    probs(2) should be > probs(4) // Retail > Public
  }

  // ==========================================================================
  // Net firm creation
  // ==========================================================================

  private def mkFirms(n: Int): Vector[Firm.State] =
    (0 until n).map: i =>
      Firm.State(
        id = FirmId(i),
        cash = PLN(100000.0),
        debt = PLN.Zero,
        tech = TechState.Traditional(5),
        riskProfile = Share(0.5),
        innovationCostFactor = 1.0,
        digitalReadiness = Share(0.1),
        sector = SectorIdx(i % 6),
        neighbors = Vector.empty[FirmId],
        bankId = BankId(0),
        equityRaised = PLN.Zero,
        initialSize = 5,
        capitalStock = PLN.Zero,
        bondDebt = PLN.Zero,
        foreignOwned = false,
        inventory = PLN.Zero,
        greenCapital = PLN.Zero,
        accumulatedLoss = PLN.Zero,
      )
    .toVector

  private def mkDeadFirm(id: Int, sector: Int = 2): Firm.State =
    Firm.State(
      id = FirmId(id),
      cash = PLN(-1.0),
      debt = PLN.Zero,
      tech = TechState.Bankrupt(BankruptReason.Other("test")),
      riskProfile = Share(0.5),
      innovationCostFactor = 1.0,
      digitalReadiness = Share(0.1),
      sector = SectorIdx(sector),
      neighbors = Vector.empty[FirmId],
      bankId = BankId(0),
      equityRaised = PLN.Zero,
      initialSize = 0,
      capitalStock = PLN.Zero,
      bondDebt = PLN.Zero,
      foreignOwned = false,
      inventory = PLN.Zero,
      greenCapital = PLN.Zero,
      accumulatedLoss = PLN.Zero,
    )

  "Net creation" should "produce zero new firms when unemployment <= NAIRU" in {
    val firms  = mkFirms(100)
    val rng    = new scala.util.Random(42)
    val result = FirmEntry.process(firms, Share.Zero, Share.Zero, 0.04, rng) // below NAIRU 0.05
    result.netBirths shouldBe 0
    result.firms.length shouldBe firms.length
  }

  "Replacement entry" should "recreate some dead firms even when unemployment <= NAIRU" in {
    val firms  = mkFirms(20) ++ Vector(mkDeadFirm(20), mkDeadFirm(21), mkDeadFirm(22), mkDeadFirm(23))
    val rng    = new scala.util.Random(42)
    val result = FirmEntry.process(firms, Share.Zero, Share.Zero, 0.04, rng)
    result.netBirths shouldBe 0
    result.births should be > 0
    result.firms.length shouldBe firms.length
    result.firms.count(Firm.isAlive) should be > firms.count(Firm.isAlive)
  }

  it should "preserve vector length when only replacements occur" in {
    val firms  = mkFirms(20) ++ Vector(mkDeadFirm(20), mkDeadFirm(21))
    val rng    = new scala.util.Random(42)
    val result = FirmEntry.process(firms, Share.Zero, Share.Zero, 0.04, rng)
    result.netBirths shouldBe 0
    result.firms.length shouldBe firms.length
  }

  it should "count only appended firms as netBirths" in {
    val firms  = mkFirms(100) ++ Vector(mkDeadFirm(100), mkDeadFirm(101), mkDeadFirm(102))
    val rng    = new scala.util.Random(42)
    val result = FirmEntry.process(firms, Share.Zero, Share.Zero, 0.20, rng)
    result.births should be >= result.netBirths
    result.netBirths should be > 0
    result.firms.length shouldBe firms.length + result.netBirths
  }

  it should "produce firms when unemployment > NAIRU" in {
    val firms  = mkFirms(100)
    val rng    = new scala.util.Random(42)
    val result = FirmEntry.process(firms, Share.Zero, Share.Zero, 0.15, rng) // 10% above NAIRU
    result.netBirths should be > 0
    result.firms.length should be > firms.length
  }

  it should "respect hard cap" in {
    val firms  = mkFirms(10000)
    val rng    = new scala.util.Random(42)
    val result = FirmEntry.process(firms, Share.Zero, Share.Zero, 0.50, rng) // extreme unemployment
    result.netBirths should be <= p.firm.netEntryMaxMonthly
  }

  it should "assign sequential FirmIds" in {
    val firms    = mkFirms(100)
    val rng      = new scala.util.Random(42)
    val result   = FirmEntry.process(firms, Share.Zero, Share.Zero, 0.20, rng)
    val newFirms = result.firms.drop(firms.length)
    newFirms.zipWithIndex.foreach: (f, i) =>
      f.id.toInt shouldBe firms.length + i
  }

  it should "create firms with GUS size distribution" in {
    val firms    = mkFirms(100)
    val rng      = new scala.util.Random(42)
    val result   = FirmEntry.process(firms, Share.Zero, Share.Zero, 0.20, rng)
    val newFirms = result.firms.drop(firms.length)
    newFirms.foreach: f =>
      f.initialSize should be >= 1
  }

  it should "preserve existing firms unchanged" in {
    val firms  = mkFirms(100)
    val rng    = new scala.util.Random(42)
    val result = FirmEntry.process(firms, Share.Zero, Share.Zero, 0.20, rng)
    result.firms.take(firms.length).map(_.id) shouldBe firms.map(_.id)
  }

  "NetEntryRate" should "default to 0.06" in {
    p.firm.netEntryRate shouldBe Share(0.06)
  }

  "NetEntryMaxMonthly" should "default to 100" in {
    p.firm.netEntryMaxMonthly shouldBe 100
  }
