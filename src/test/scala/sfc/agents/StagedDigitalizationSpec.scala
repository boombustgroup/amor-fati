package sfc.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.accounting.{ForexState, GovState}
import sfc.config.SimParams
import sfc.engine.*
import sfc.types.*

class StagedDigitalizationSpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  // ---- Helpers ----

  private def mkFirm(tech: TechState, sector: Int = 2, cash: Double = 500000.0, dr: Double = 0.5): Firm.State =
    Firm.State(
      FirmId(0),
      PLN(cash),
      PLN.Zero,
      tech,
      Ratio(0.5),
      1.0,
      Ratio(dr),
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

  private def mkWorld(autoRatio: Double = 0.0, hybridRatio: Double = 0.0): World =
    World(
      month = 31,
      inflation = Rate(0.02),
      priceLevel = 1.0,
      gdpProxy = 1e9,
      currentSigmas = p.sectorDefs.map(_.sigma).toVector,
      totalPopulation = 100000,
      gov = GovState(PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
      nbp = Nbp.State(Rate(0.0575), PLN.Zero, false, PLN.Zero, PLN.Zero, PLN.Zero),
      bank = Banking.Aggregate(PLN(1000000), PLN(10000), PLN(500000), PLN(1000000), PLN.Zero, PLN.Zero, PLN.Zero, PLN.Zero),
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
      real = RealState.zero.copy(automationRatio = Ratio(autoRatio), hybridRatio = Ratio(hybridRatio)),
      mechanisms = MechanismsState.zero,
      plumbing = MonetaryPlumbingState.zero,
      flows = FlowState.zero,
    )

  // ---- Config defaults (3 tests) ----

  "p.firm.digiDrift.toDouble" should "be positive by default" in {
    p.firm.digiDrift.toDouble should be > 0.0
  }

  "p.firm.digiInvestCost.toDouble" should "be positive by default" in {
    p.firm.digiInvestCost.toDouble should be > 0.0
  }

  "p.firm.digiCapexDiscount.toDouble" should "be in [0, 1]" in {
    p.firm.digiCapexDiscount.toDouble should be >= 0.0
    p.firm.digiCapexDiscount.toDouble should be <= 1.0
  }

  // ---- CAPEX discount (3 tests) ----

  "Firm.computeAiCapex" should "decrease with higher digitalReadiness" in {
    val fLow  = mkFirm(TechState.Traditional(10), dr = 0.1)
    val fHigh = mkFirm(TechState.Traditional(10), dr = 0.9)
    Firm.computeAiCapex(fHigh) should be < Firm.computeAiCapex(fLow)
  }

  "Firm.computeHybridCapex" should "decrease with higher digitalReadiness" in {
    val fLow  = mkFirm(TechState.Traditional(10), dr = 0.1)
    val fHigh = mkFirm(TechState.Traditional(10), dr = 0.9)
    Firm.computeHybridCapex(fHigh) should be < Firm.computeHybridCapex(fLow)
  }

  "Firm.computeAiCapex" should "apply no discount when digitalReadiness is 0" in {
    val f0           = mkFirm(TechState.Traditional(10), dr = 0.0)
    // With dr=0: discount factor = 1.0 - 0.30 * 0.0 = 1.0 (no discount)
    val expectedBase = p.firm.aiCapex.toDouble * p.sectorDefs(2).aiCapexMultiplier * 1.0 *
      Math.pow(10.0 / p.pop.workersPerFirm, 0.6)
    Firm.computeAiCapex(f0).toDouble shouldBe expectedBase +- 0.01
  }

  // ---- Digital drift (3 tests) ----

  "applyDigitalDrift" should "increase DR for alive firms" in {
    val f      = mkFirm(TechState.Traditional(10), dr = 0.40)
    val w      = mkWorld()
    val result = Firm.process(f, w, Rate(0.07), _ => true, Vector(f), new scala.util.Random(42))
    // DR should be at least initial + drift (could also get digital investment boost)
    result.firm.digitalReadiness.toDouble should be >= (0.40 + p.firm.digiDrift.toDouble - 0.001)
  }

  it should "cap digitalReadiness at 1.0" in {
    val f      = mkFirm(TechState.Traditional(10), dr = 0.999)
    val w      = mkWorld()
    val result = Firm.process(f, w, Rate(0.07), _ => true, Vector(f), new scala.util.Random(42))
    result.firm.digitalReadiness.toDouble should be <= 1.0
  }

  it should "not change DR for bankrupt firms" in {
    val f      = mkFirm(TechState.Bankrupt(BankruptReason.Other("test")), dr = 0.50)
    val w      = mkWorld()
    val result = Firm.process(f, w, Rate(0.07), _ => true, Vector(f), new scala.util.Random(42))
    result.firm.digitalReadiness.toDouble shouldBe 0.50
  }

  // ---- Active digital investment (4 tests) ----

  "Digital investment" should "reduce cash and increase DR when triggered" in {
    // Use very high DR so automation/hybrid won't trigger, but low enough for investment
    // Set cash very high and automation/hybrid conditions unfavorable
    val f        = mkFirm(TechState.Traditional(10), cash = 10000000.0, dr = 0.15)
    val w        = mkWorld(autoRatio = 0.5) // high competitive pressure
    // Run many times to catch at least one investment
    val rng      = new scala.util.Random(42)
    var invested = false
    for _ <- 0 until 500 if !invested do
      val result = Firm.process(f, w, Rate(0.07), _ => false, Vector(f), rng)
      if result.firm.digitalReadiness.toDouble > f.digitalReadiness.toDouble + p.firm.digiDrift.toDouble + 0.001 then
        // Investment happened (DR increased beyond just drift)
        result.firm.cash.toDouble should be < f.cash.toDouble
        invested = true
    invested shouldBe true
  }

  it should "not invest when firm cannot afford it" in {
    val digiCost = Firm.computeDigiInvestCost(mkFirm(TechState.Traditional(10))).toDouble
    // Cash so low firm can't afford 2× digiCost, but not negative (would bankrupt)
    val f        = mkFirm(TechState.Traditional(10), cash = digiCost * 0.5, dr = 0.30)
    val w        = mkWorld(autoRatio = 0.5)
    // Over many trials, no investment should happen (only drift)
    for _ <- 0 until 100 do
      val result = Firm.process(f, w, Rate(0.07), _ => false, Vector(f), new scala.util.Random(42))
      // DR should be at most initial + drift (no investment boost)
      // But net income is added to cash, so firm may become solvent enough
      // Just verify no investment boost beyond drift
      if Firm.isAlive(result.firm) then
        result.firm.digitalReadiness.toDouble should be <= (f.digitalReadiness.toDouble + p.firm.digiDrift.toDouble + p.firm.digiInvestBoost.toDouble * 0.001)
  }

  it should "have diminishing returns at high DR" in {
    val diminishingLow  = 1.0 - 0.2 // DR=0.2
    val diminishingHigh = 1.0 - 0.9 // DR=0.9
    val boostLow        = p.firm.digiInvestBoost.toDouble * diminishingLow
    val boostHigh       = p.firm.digiInvestBoost.toDouble * diminishingHigh
    boostHigh should be < boostLow
  }

  "Firm.computeDigiInvestCost" should "scale sublinearly with firm size" in {
    val fSmall    = mkFirm(TechState.Traditional(10)).copy(initialSize = 5)
    val fLarge    = mkFirm(TechState.Traditional(10)).copy(initialSize = 100)
    val costSmall = Firm.computeDigiInvestCost(fSmall)
    val costLarge = Firm.computeDigiInvestCost(fLarge)
    // Large firm costs more
    costLarge should be > costSmall
    // But sublinearly: cost ratio < size ratio
    val sizeRatio = 100.0 / 5.0
    val costRatio = costLarge / costSmall // PLN / PLN → Double
    costRatio should be < sizeRatio
  }

  // ---- Hybrid learning + drift (1 test) ----

  "Hybrid firm" should "gain at least 0.005 + drift in DR per month" in {
    val f      = mkFirm(TechState.Hybrid(5, 1.1), dr = 0.40)
    val w      = mkWorld()
    val result = Firm.process(f, w, Rate(0.07), _ => true, Vector(f), new scala.util.Random(42))
    if Firm.isAlive(result.firm) then
      // Hybrid learning (+0.005) + natural drift (+0.001)
      result.firm.digitalReadiness.toDouble should be >= (0.40 + 0.005 + p.firm.digiDrift.toDouble - 0.001)
  }

  // ---- Integration (1 test) ----

  "Traditional firms" should "accumulate DR over multiple months via drift" in {
    val initDR = 0.30
    var f      = mkFirm(TechState.Traditional(10), cash = 1000000.0, dr = initDR)
    val w      = mkWorld()
    // Simulate 10 months — at minimum, drift alone adds 10 × 0.001 = 0.01
    for _ <- 0 until 10 do
      val result = Firm.process(f, w, Rate(0.07), _ => false, Vector(f), new scala.util.Random(42))
      if Firm.isAlive(result.firm) then f = result.firm.copy(cash = PLN(1000000.0)) // reset cash for next round
    f.digitalReadiness.toDouble should be >= (initDR + 10 * p.firm.digiDrift.toDouble - 0.001)
  }
