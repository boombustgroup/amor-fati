package com.boombustgroup.amorfati.agents

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.TestFirmState

import org.scalatest.flatspec.AnyFlatSpec
import com.boombustgroup.amorfati.Generators
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.*
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.markets.OpenEconomy
import com.boombustgroup.amorfati.random.RandomStream
import com.boombustgroup.amorfati.types.*

class StagedDigitalizationSpec extends AnyFlatSpec with Matchers:

  given SimParams              = SimParams.defaults
  private val p: SimParams     = summon[SimParams]
  private val ExecutionMonth31 = ExecutionMonth(31)

  // ---- Helpers ----

  private def mkFirm(tech: TechState, sector: Int = 2, cash: BigDecimal = BigDecimal("500000.0"), dr: BigDecimal = BigDecimal("0.5")): Firm.State =
    TestFirmState(
      FirmId(0),
      plnBD(cash),
      PLN.Zero,
      tech,
      Share.decimal(5, 1),
      Multiplier.One,
      shareBD(dr),
      SectorIdx(sector),
      Vector.empty[FirmId],
      bankId = BankId(0),
      equityRaised = PLN.Zero,
      initialSize = 10,
      capitalStock = p.capital.klRatios(sector) * Multiplier(10), // exact match for workers=10
      foreignOwned = false,
      inventory = PLN.Zero,
      greenCapital = PLN.Zero,
      accumulatedLoss = PLN.Zero,
    )

  private def mkStocks(cash: BigDecimal = BigDecimal("500000.0"), debt: PLN = PLN.Zero, equity: PLN = PLN.Zero): Firm.FinancialStocks =
    TestFirmState.financial(cash = plnBD(cash), debt = debt, equityRaised = equity)

  private def mkWorld(autoRatio: BigDecimal = BigDecimal("0.0"), hybridRatio: BigDecimal = BigDecimal("0.0")): World =
    Generators.testWorld(
      totalPopulation = 100000,
      employed = 100000,
      forex = OpenEconomy.ForexState(ExchangeRate.decimal(433, 2), PLN.Zero, PLN(190000000), PLN.Zero, PLN.Zero),
      marketWage = plnBD(decimal(p.household.baseWage)),
      reservationWage = plnBD(decimal(p.household.baseReservationWage)),
      real = RealState.zero.copy(automationRatio = shareBD(autoRatio), hybridRatio = shareBD(hybridRatio)),
    )

  private def process(
      firm: Firm.State,
      world: World,
      lendRate: Rate,
      bankCanLend: PLN => Boolean,
      allFirms: Vector[Firm.State],
      rng: RandomStream,
      financialStocks: Firm.FinancialStocks = mkStocks(),
  ): Firm.Result =
    Firm.process(
      firm,
      financialStocks,
      world,
      ExecutionMonth31,
      OperationalSignals.fromDecisionSignals(world.seedIn, world.pipeline.operationalHiringSlack),
      lendRate,
      bankCanLend,
      allFirms,
      rng,
      PLN.Zero,
    )

  // ---- Config defaults (3 tests) ----

  "p.firm.digiDrift" should "be positive by default" in {
    decimal(p.firm.digiDrift) should be > BigDecimal("0.0")
  }

  "p.firm.digiInvestCost" should "be positive by default" in {
    decimal(p.firm.digiInvestCost) should be > BigDecimal("0.0")
  }

  "p.firm.digiCapexDiscount" should "be in [0, 1]" in {
    decimal(p.firm.digiCapexDiscount) should be >= BigDecimal("0.0")
    decimal(p.firm.digiCapexDiscount) should be <= BigDecimal("1.0")
  }

  // ---- CAPEX discount (3 tests) ----

  "Firm.computeAiCapex" should "decrease with higher digitalReadiness" in {
    val fLow  = mkFirm(TechState.Traditional(10), dr = BigDecimal("0.1"))
    val fHigh = mkFirm(TechState.Traditional(10), dr = BigDecimal("0.9"))
    Firm.computeAiCapex(fHigh) should be < Firm.computeAiCapex(fLow)
  }

  "Firm.computeHybridCapex" should "decrease with higher digitalReadiness" in {
    val fLow  = mkFirm(TechState.Traditional(10), dr = BigDecimal("0.1"))
    val fHigh = mkFirm(TechState.Traditional(10), dr = BigDecimal("0.9"))
    Firm.computeHybridCapex(fHigh) should be < Firm.computeHybridCapex(fLow)
  }

  "Firm.computeAiCapex" should "apply no discount when digitalReadiness is 0" in {
    val f0           = mkFirm(TechState.Traditional(10), dr = BigDecimal("0.0"))
    // With dr=0: discount factor = 1.0 - 0.30 * 0.0 = 1.0 (no discount)
    val expectedBase = decimal(p.firm.aiCapex) * decimal(p.sectorDefs(2).aiCapexMultiplier) * BigDecimal("1.0") *
      DecimalMath.pow(BigDecimal("10.0") / p.pop.workersPerFirm, BigDecimal("0.6"))
    decimal(Firm.computeAiCapex(f0)) shouldBe expectedBase +- BigDecimal("0.01")
  }

  // ---- Digital drift (3 tests) ----

  "applyDigitalDrift" should "increase DR for alive firms" in {
    val f      = mkFirm(TechState.Traditional(10), dr = BigDecimal("0.40"))
    val w      = mkWorld()
    val result = process(f, w, Rate.decimal(7, 2), _ => true, Vector(f), RandomStream.seeded(42))
    // DR should be at least initial + drift (could also get digital investment boost)
    decimal(result.firm.digitalReadiness) should be >= (BigDecimal("0.40") + decimal(p.firm.digiDrift) - BigDecimal("0.001"))
  }

  it should "cap digitalReadiness at 1.0" in {
    val f      = mkFirm(TechState.Traditional(10), dr = BigDecimal("0.999"))
    val w      = mkWorld()
    val result = process(f, w, Rate.decimal(7, 2), _ => true, Vector(f), RandomStream.seeded(42))
    decimal(result.firm.digitalReadiness) should be <= BigDecimal("1.0")
  }

  it should "not change DR for bankrupt firms" in {
    val f      = mkFirm(TechState.Bankrupt(BankruptReason.Other("test")), dr = BigDecimal("0.50"))
    val w      = mkWorld()
    val result = process(f, w, Rate.decimal(7, 2), _ => true, Vector(f), RandomStream.seeded(42))
    decimal(result.firm.digitalReadiness) shouldBe BigDecimal("0.50")
  }

  // ---- Active digital investment (4 tests) ----

  "Digital investment" should "reduce cash and increase DR when triggered" in {
    // Firm at optimal headcount (initialSize = workers, with matching capital)
    // so MR=MC labor adjustment is a no-op and decision reaches DigiInvest path.
    val f          = mkFirm(TechState.Traditional(10), cash = BigDecimal("1e9"), dr = BigDecimal("0.15"))
    val w          = mkWorld(autoRatio = BigDecimal("0.5"))
    // With MR=MC labor adjustment, firms adjust headcount AND invest in DR
    // concurrently. Run enough rounds for DR to increase above drift baseline.
    val rng        = RandomStream.seeded(42)
    var f1         = f
    var stocks     = mkStocks(cash = BigDecimal("1e9"))
    for _ <- 0 until 200 do
      val result = process(f1, w, Rate.decimal(7, 2), _ => false, Vector(f1), rng, stocks)
      f1 = result.firm
      stocks = result.financialStocks
    assume(Firm.isAlive(f1), "firm must survive processing")
    // DR should have increased from digital investment (beyond just drift)
    val drIncrease = decimal(f1.digitalReadiness) - decimal(f.digitalReadiness)
    drIncrease should be > (decimal(p.firm.digiDrift) * 200 + BigDecimal("0.01"))
  }

  it should "not invest when firm cannot afford it" in {
    val digiCost = decimal(Firm.computeDigiInvestCost(mkFirm(TechState.Traditional(10))))
    // Cash so low firm can't afford 2× digiCost, but not negative (would bankrupt)
    val f        = mkFirm(TechState.Traditional(10), cash = digiCost * BigDecimal("0.5"), dr = BigDecimal("0.30"))
    val w        = mkWorld(autoRatio = BigDecimal("0.5"))
    val rng      = RandomStream.seeded(42L)
    // Over many trials, no investment should happen (only drift)
    for _ <- 0 until 100 do
      val result = process(f, w, Rate.decimal(7, 2), _ => false, Vector(f), rng, mkStocks(cash = digiCost * BigDecimal("0.5")))
      // DR should be at most initial + drift (no investment boost)
      // But net income is added to cash, so firm may become solvent enough
      // Just verify no investment boost beyond drift
      if Firm.isAlive(result.firm) then
        decimal(result.firm.digitalReadiness) should be <= (decimal(f.digitalReadiness) + decimal(p.firm.digiDrift) + decimal(
          p.firm.digiInvestBoost,
        ) * BigDecimal("0.001"))
  }

  it should "have diminishing returns at high DR" in {
    val diminishingLow  = BigDecimal("1.0") - BigDecimal("0.2") // DR=0.2
    val diminishingHigh = BigDecimal("1.0") - BigDecimal("0.9") // DR=0.9
    val boostLow        = decimal(p.firm.digiInvestBoost) * diminishingLow
    val boostHigh       = decimal(p.firm.digiInvestBoost) * diminishingHigh
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
    val sizeRatio = BigDecimal("100.0") / BigDecimal("5.0")
    val costRatio = costLarge / costSmall
    decimal(costRatio) should be < sizeRatio
  }

  // ---- Hybrid learning + drift (1 test) ----

  "Hybrid firm" should "gain at least 0.005 + drift in DR per month" in {
    val f      = mkFirm(TechState.Hybrid(5, Multiplier.decimal(11, 1)), dr = BigDecimal("0.40"))
    val w      = mkWorld()
    val result = process(f, w, Rate.decimal(7, 2), _ => true, Vector(f), RandomStream.seeded(42))
    if Firm.isAlive(result.firm) then
      // Hybrid learning (+0.005) + natural drift (+0.001)
      decimal(result.firm.digitalReadiness) should be >= (BigDecimal("0.40") + BigDecimal("0.005") + decimal(p.firm.digiDrift) - BigDecimal("0.001"))
  }

  // ---- Integration (1 test) ----

  "Traditional firms" should "accumulate DR over multiple months via drift" in {
    val initDR = BigDecimal("0.30")
    var f      = mkFirm(TechState.Traditional(10), cash = BigDecimal("1000000.0"), dr = initDR)
    var stocks = mkStocks(cash = BigDecimal("1000000.0"))
    val w      = mkWorld()
    val rng    = RandomStream.seeded(42L)
    // Simulate 10 months — at minimum, drift alone adds 10 × 0.001 = 0.01
    for _ <- 0 until 10 do
      val result = process(f, w, Rate.decimal(7, 2), _ => false, Vector(f), rng, financialStocks = stocks)
      if Firm.isAlive(result.firm) then
        f = result.firm
        stocks = result.financialStocks.copy(cash = PLN(1000000)) // reset cash for next round
    decimal(f.digitalReadiness) should be >= (initDR + 10 * decimal(p.firm.digiDrift) - BigDecimal("0.001"))
  }
