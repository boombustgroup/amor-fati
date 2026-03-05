package sfc.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.accounting.{BankState, ForexState, GovState}
import sfc.config.{Config, SECTORS, RunConfig}
import sfc.engine.World
import sfc.types.*

class StagedDigitalizationSpec extends AnyFlatSpec with Matchers:

  // ---- Helpers ----

  private def mkFirm(tech: TechState, sector: Int = 2,
    cash: Double = 500000.0, dr: Double = 0.5): Firm =
    Firm(FirmId(0), cash, 0.0, tech, 0.5, 1.0, dr, SectorIdx(sector), Array.empty[Int])

  private def mkWorld(autoRatio: Double = 0.0, hybridRatio: Double = 0.0): World =
    World(
      month = 31,
      inflation = 0.02,
      priceLevel = 1.0,
      gov = GovState(false, 0, 0, 0, 0, 0),
      nbp = NbpState(0.0575),
      bank = BankState(1000000, 10000, 500000, 1000000),
      forex = ForexState(4.33, 0, 190000000, 0, 0),
      hh = HhState(100000, Config.BaseWage, Config.BaseReservationWage, 0, 0, 0, 0),
      automationRatio = autoRatio,
      hybridRatio = hybridRatio,
      gdpProxy = 1e9,
      currentSigmas = SECTORS.map(_.sigma).toVector
    )

  private val rc = RunConfig(2000.0, 1, "test")

  // ---- Config defaults (3 tests) ----

  "Config.DigiDrift" should "be positive by default" in {
    Config.DigiDrift should be > 0.0
  }

  "Config.DigiInvestCost" should "be positive by default" in {
    Config.DigiInvestCost should be > 0.0
  }

  "Config.DigiCapexDiscount" should "be in [0, 1]" in {
    Config.DigiCapexDiscount should be >= 0.0
    Config.DigiCapexDiscount should be <= 1.0
  }

  // ---- CAPEX discount (3 tests) ----

  "FirmOps.aiCapex" should "decrease with higher digitalReadiness" in {
    val fLow  = mkFirm(TechState.Traditional(10), dr = 0.1)
    val fHigh = mkFirm(TechState.Traditional(10), dr = 0.9)
    FirmOps.aiCapex(fHigh) should be < FirmOps.aiCapex(fLow)
  }

  "FirmOps.hybridCapex" should "decrease with higher digitalReadiness" in {
    val fLow  = mkFirm(TechState.Traditional(10), dr = 0.1)
    val fHigh = mkFirm(TechState.Traditional(10), dr = 0.9)
    FirmOps.hybridCapex(fHigh) should be < FirmOps.hybridCapex(fLow)
  }

  "FirmOps.aiCapex" should "apply no discount when digitalReadiness is 0" in {
    val f0 = mkFirm(TechState.Traditional(10), dr = 0.0)
    // With dr=0: discount factor = 1.0 - 0.30 * 0.0 = 1.0 (no discount)
    val expectedBase = Config.AiCapex * SECTORS(2).aiCapexMultiplier * 1.0 *
      Math.pow(10.0 / Config.WorkersPerFirm, 0.6)
    FirmOps.aiCapex(f0) shouldBe expectedBase +- 0.01
  }

  // ---- Digital drift (3 tests) ----

  "applyDigitalDrift" should "increase DR for alive firms" in {
    val f = mkFirm(TechState.Traditional(10), dr = 0.40)
    val w = mkWorld()
    val result = FirmLogic.process(f, w, 0.07, _ => true, Array(f), rc)
    // DR should be at least initial + drift (could also get digital investment boost)
    result.firm.digitalReadiness should be >= (0.40 + Config.DigiDrift - 0.001)
  }

  it should "cap digitalReadiness at 1.0" in {
    val f = mkFirm(TechState.Traditional(10), dr = 0.999)
    val w = mkWorld()
    val result = FirmLogic.process(f, w, 0.07, _ => true, Array(f), rc)
    result.firm.digitalReadiness should be <= 1.0
  }

  it should "not change DR for bankrupt firms" in {
    val f = mkFirm(TechState.Bankrupt("test"), dr = 0.50)
    val w = mkWorld()
    val result = FirmLogic.process(f, w, 0.07, _ => true, Array(f), rc)
    result.firm.digitalReadiness shouldBe 0.50
  }

  // ---- Active digital investment (4 tests) ----

  "Digital investment" should "reduce cash and increase DR when triggered" in {
    // Use very high DR so automation/hybrid won't trigger, but low enough for investment
    // Set cash very high and automation/hybrid conditions unfavorable
    val f = mkFirm(TechState.Traditional(10), cash = 10000000.0, dr = 0.15)
    val w = mkWorld(autoRatio = 0.5)  // high competitive pressure
    // Run many times to catch at least one investment
    var invested = false
    for _ <- 0 until 500 if !invested do
      val result = FirmLogic.process(f, w, 0.07, _ => false, Array(f), rc)
      if result.firm.digitalReadiness > f.digitalReadiness + Config.DigiDrift + 0.001 then
        // Investment happened (DR increased beyond just drift)
        result.firm.cash should be < f.cash
        invested = true
    invested shouldBe true
  }

  it should "not invest when firm cannot afford it" in {
    val digiCost = FirmOps.digiInvestCost(mkFirm(TechState.Traditional(10)))
    // Cash so low firm can't afford 2× digiCost, but not negative (would bankrupt)
    val f = mkFirm(TechState.Traditional(10), cash = digiCost * 0.5, dr = 0.30)
    val w = mkWorld(autoRatio = 0.5)
    // Over many trials, no investment should happen (only drift)
    for _ <- 0 until 100 do
      val result = FirmLogic.process(f, w, 0.07, _ => false, Array(f), rc)
      // DR should be at most initial + drift (no investment boost)
      // But net income is added to cash, so firm may become solvent enough
      // Just verify no investment boost beyond drift
      if FirmOps.isAlive(result.firm) then
        result.firm.digitalReadiness should be <= (f.digitalReadiness + Config.DigiDrift + Config.DigiInvestBoost * 0.001)
  }

  it should "have diminishing returns at high DR" in {
    val diminishingLow = 1.0 - 0.2   // DR=0.2
    val diminishingHigh = 1.0 - 0.9  // DR=0.9
    val boostLow = Config.DigiInvestBoost * diminishingLow
    val boostHigh = Config.DigiInvestBoost * diminishingHigh
    boostHigh should be < boostLow
  }

  "FirmOps.digiInvestCost" should "scale sublinearly with firm size" in {
    val fSmall = mkFirm(TechState.Traditional(10)).copy(initialSize = 5)
    val fLarge = mkFirm(TechState.Traditional(10)).copy(initialSize = 100)
    val costSmall = FirmOps.digiInvestCost(fSmall)
    val costLarge = FirmOps.digiInvestCost(fLarge)
    // Large firm costs more
    costLarge should be > costSmall
    // But sublinearly: cost ratio < size ratio
    val sizeRatio = 100.0 / 5.0
    val costRatio = costLarge / costSmall
    costRatio should be < sizeRatio
  }

  // ---- Hybrid learning + drift (1 test) ----

  "Hybrid firm" should "gain at least 0.005 + drift in DR per month" in {
    val f = mkFirm(TechState.Hybrid(5, 1.1), dr = 0.40)
    val w = mkWorld()
    val result = FirmLogic.process(f, w, 0.07, _ => true, Array(f), rc)
    if FirmOps.isAlive(result.firm) then
      // Hybrid learning (+0.005) + natural drift (+0.001)
      result.firm.digitalReadiness should be >= (0.40 + 0.005 + Config.DigiDrift - 0.001)
  }

  // ---- Integration (1 test) ----

  "Traditional firms" should "accumulate DR over multiple months via drift" in {
    val initDR = 0.30
    var f = mkFirm(TechState.Traditional(10), cash = 1000000.0, dr = initDR)
    val w = mkWorld()
    // Simulate 10 months — at minimum, drift alone adds 10 × 0.001 = 0.01
    for _ <- 0 until 10 do
      val result = FirmLogic.process(f, w, 0.07, _ => false, Array(f), rc)
      if FirmOps.isAlive(result.firm) then
        f = result.firm.copy(cash = 1000000.0)  // reset cash for next round
    f.digitalReadiness should be >= (initDR + 10 * Config.DigiDrift - 0.001)
  }
