package sfc.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.accounting.{BankState, ForexState, GovState}
import sfc.config.{Config, SECTORS, RunConfig}
import sfc.engine.World
import sfc.types.*

import scala.util.Random

class FirmSpec extends AnyFlatSpec with Matchers:

  // --- FirmOps.isAlive ---

  "FirmOps.isAlive" should "return true for Traditional" in {
    FirmOps.isAlive(mkFirm(TechState.Traditional(10))) shouldBe true
  }

  it should "return true for Hybrid" in {
    FirmOps.isAlive(mkFirm(TechState.Hybrid(5, 1.2))) shouldBe true
  }

  it should "return true for Automated" in {
    FirmOps.isAlive(mkFirm(TechState.Automated(1.5))) shouldBe true
  }

  it should "return false for Bankrupt" in {
    FirmOps.isAlive(mkFirm(TechState.Bankrupt("test"))) shouldBe false
  }

  // --- FirmOps.workers ---

  "FirmOps.workers" should "return workers for Traditional" in {
    FirmOps.workers(mkFirm(TechState.Traditional(10))) shouldBe 10
  }

  it should "return workers for Hybrid" in {
    FirmOps.workers(mkFirm(TechState.Hybrid(7, 1.0))) shouldBe 7
  }

  it should "return skeletonCrew for Automated" in {
    val f = mkFirm(TechState.Automated(1.5))
    FirmOps.workers(f) shouldBe FirmOps.skeletonCrew(f)
  }

  it should "return 0 for Bankrupt" in {
    FirmOps.workers(mkFirm(TechState.Bankrupt("test"))) shouldBe 0
  }

  // --- FirmOps.capacity ---

  "FirmOps.capacity" should "be positive for alive firms" in {
    FirmOps.capacity(mkFirm(TechState.Traditional(10))) should be > 0.0
    FirmOps.capacity(mkFirm(TechState.Hybrid(5, 1.2))) should be > 0.0
    FirmOps.capacity(mkFirm(TechState.Automated(1.5))) should be > 0.0
  }

  it should "be 0 for Bankrupt" in {
    FirmOps.capacity(mkFirm(TechState.Bankrupt("test"))) shouldBe 0.0
  }

  // --- FirmOps.aiCapex / hybridCapex ---

  "FirmOps.aiCapex" should "be positive and scale with multipliers" in {
    val f = mkFirm(TechState.Traditional(10))
    FirmOps.aiCapex(f) should be > 0.0
    // With higher innovationCostFactor → higher capex
    val f2 = f.copy(innovationCostFactor = 1.5)
    FirmOps.aiCapex(f2) should be > FirmOps.aiCapex(f)
  }

  "FirmOps.hybridCapex" should "be positive" in {
    val f = mkFirm(TechState.Traditional(10))
    FirmOps.hybridCapex(f) should be > 0.0
  }

  // --- FirmOps.sigmaThreshold ---

  "FirmOps.sigmaThreshold" should "be monotonically increasing with sigma" in {
    // Sectors ordered by sigma: Public(1.0) < Healthcare(2.0) < Agriculture(3.0) < Retail(5.0) < Manuf(10.0) < BPO(50.0)
    val sigmasOrdered = Vector(1.0, 2.0, 3.0, 5.0, 10.0, 50.0)
    val thresholds = sigmasOrdered.map(FirmOps.sigmaThreshold)
    for i <- 0 until thresholds.length - 1 do
      thresholds(i) should be <= thresholds(i + 1)
  }

  it should "be bounded in [0, 1]" in {
    for s <- SECTORS do
      val t = FirmOps.sigmaThreshold(s.sigma)
      t should be >= 0.0
      t should be <= 1.0
  }

  // --- FirmLogic.process ---

  "FirmLogic.process" should "keep a Bankrupt firm bankrupt with zero tax/capex" in {
    Random.setSeed(42)
    val f = mkFirm(TechState.Bankrupt("test"))
    val rc = RunConfig(0.0, 1, "test")
    val result = FirmLogic.process(f, mkWorld(), 0.07, _ => true, Array(f), rc)
    result.taxPaid shouldBe 0.0
    result.capexSpent shouldBe 0.0
    result.firm.tech shouldBe a[TechState.Bankrupt]
  }

  it should "keep an Automated firm alive with large cash" in {
    Random.setSeed(42)
    val f = mkFirm(TechState.Automated(1.5)).copy(cash = 10000000.0)
    val rc = RunConfig(0.0, 1, "test")
    val result = FirmLogic.process(f, mkWorld(), 0.07, _ => true, Array(f), rc)
    FirmOps.isAlive(result.firm) shouldBe true
  }

  it should "bankrupt an Automated firm with negative cash when P&L is negative" in {
    Random.setSeed(42)
    // Very low cash + high price level = deep losses → bankrupt
    val f = mkFirm(TechState.Automated(0.1)).copy(cash = -500000.0, debt = 5000000.0)
    val w = mkWorld().copy(priceLevel = 0.3, sectorDemandMult = Vector.fill(6)(0.1))
    val rc = RunConfig(0.0, 1, "test")
    val result = FirmLogic.process(f, w, 0.20, _ => true, Array(f), rc)
    result.firm.tech shouldBe a[TechState.Bankrupt]
  }

  // --- helpers ---

  private def mkFirm(tech: TechState, sector: Int = 2): Firm =
    Firm(FirmId(0), 50000.0, 0.0, tech, 0.5, 1.0, 0.5, SectorIdx(sector), Array.empty[Int])

  private def mkWorld(): World =
    World(
      month = 31,
      inflation = 0.02,
      priceLevel = 1.0,
      gov = GovState(false, 0, 0, 0, 0, 0),
      nbp = NbpState(0.0575),
      bank = BankState(1000000, 10000, 500000, 1000000),
      forex = ForexState(4.33, 0, 190000000, 0, 0),
      hh = HhState(100000, Config.BaseWage, Config.BaseReservationWage, 0, 0, 0, 0),
      automationRatio = 0.0,
      hybridRatio = 0.0,
      gdpProxy = 1e9,
      currentSigmas = SECTORS.map(_.sigma).toVector
    )
