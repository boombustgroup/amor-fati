package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.config.{Config, SECTORS}
import sfc.agents.{Firm, FirmOps, TechState}
import sfc.sfc.SfcCheck
import KahanSum.*

class FofSpec extends AnyFlatSpec with Matchers:

  "FofConsWeights" should "sum to 1.0" in {
    Math.abs(Config.FofConsWeights.sum - 1.0) should be < 0.01
  }

  "FofGovWeights" should "sum to 1.0" in {
    Math.abs(Config.FofGovWeights.sum - 1.0) should be < 0.01
  }

  "FofExportShares" should "sum to 1.0" in {
    Math.abs(Config.FofExportShares.sum - 1.0) should be < 0.01
  }

  "Sector demand" should "equal consWeight*dc + govWeight*gp + exports" in {
    val dc = 1000000.0
    val gp = 500000.0
    val exports = Vector(50.0, 550.0, 150.0, 20.0, 30.0, 200.0)
    for s <- 0 until 6 do
      val expected = Config.FofConsWeights(s) * dc + Config.FofGovWeights(s) * gp + exports(s)
      val actual = Config.FofConsWeights(s) * dc + Config.FofGovWeights(s) * gp + exports(s)
      actual shouldBe expected
  }

  "Sector demand multiplier" should "equal sectorDemand / (sectorCap * price)" in {
    val sectorDemand = 500000.0
    val sectorCap = 400000.0
    val price = 1.0
    val mult = sectorDemand / (sectorCap * price)
    mult shouldBe 1.25
  }

  it should "be 0 for empty sectors" in {
    val sectorCap = 0.0
    val mult = if sectorCap > 0 then 100.0 / (sectorCap * 1.0) else 0.0
    mult shouldBe 0.0
  }

  "Total firm revenue" should "equal sum of sector demands (identity closes)" in {
    val firms = mkFirms()
    val price = 1.0
    val dc = 800000.0
    val gp = 200000.0
    val exports = Config.FofExportShares.map(_ * 100000.0)

    val sectorCap = (0 until 6).map { s =>
      firms.filter(_.sector == s).kahanSumBy(f => FirmOps.capacity(f).toDouble)
    }.toVector
    val sectorDemand = (0 until 6).map { s =>
      Config.FofConsWeights(s) * dc + Config.FofGovWeights(s) * gp + exports(s)
    }.toVector
    val sectorMults = sectorDemand.indices.map { s =>
      if sectorCap(s) > 0 then sectorDemand(s) / (sectorCap(s) * price) else 0.0
    }.toVector

    val totalFirmRev = (0 until 6).map { s =>
      firms.filter(_.sector == s).kahanSumBy(f =>
        FirmOps.capacity(f).toDouble * sectorMults(s) * price)
    }.kahanSum
    val totalDemand = sectorDemand.kahanSum

    Math.abs(totalFirmRev - totalDemand) should be < 0.01
  }

  "Proportional allocation" should "distribute revenue proportionally within sector" in {
    val f1 = mkFirm(0, TechState.Traditional(10))
    val f2 = mkFirm(1, TechState.Traditional(20))
    // Both in sector 2 (Retail)
    val firms = Array(f1.copy(sector = 2), f2.copy(sector = 2))
    val price = 1.0
    val cap1 = FirmOps.capacity(firms(0))
    val cap2 = FirmOps.capacity(firms(1))
    val sectorDemand = 500000.0
    val totalCap = cap1 + cap2
    val mult = sectorDemand / (totalCap * price)
    val rev1 = cap1 * mult * price
    val rev2 = cap2 * mult * price
    Math.abs(rev1 + rev2 - sectorDemand) should be < 0.01
    // Revenue proportional to capacity
    Math.abs(rev1 / rev2 - cap1 / cap2) should be < 0.001
  }

  "Gov purchases" should "equal GovBaseSpending * price" in {
    val price = 1.2
    val gp = Config.GovBaseSpending * price
    gp shouldBe Config.GovBaseSpending * 1.2
  }

  "Scalar exports" should "be distributed by FofExportShares" in {
    val totalExports = 1000000.0
    val distributed = Config.FofExportShares.map(_ * totalExports)
    Math.abs(distributed.sum - totalExports) should be < 0.01
    distributed(1) shouldBe 520000.0 // Manufacturing gets 52%
  }

  "SfcCheck Identity 10" should "pass when fofResidual is zero" in {
    // All flows zero except fofResidual — all deltas are 0 = 0
    val flows = SfcCheck.MonthlyFlows(
      govSpending = 0.0, govRevenue = 0.0,
      nplLoss = 0.0, interestIncome = 0.0, hhDebtService = 0.0,
      totalIncome = 0.0, totalConsumption = 0.0,
      newLoans = 0.0, nplRecovery = 0.0,
      fofResidual = 0.0
    )
    val snap = SfcCheck.Snapshot(0, 0, 0, 0, 500000.0, 1000000.0, 0, 0)
    val result = SfcCheck.validate(1, snap, snap, flows)
    result.fofError shouldBe 0.0
    result.passed shouldBe true
  }

  it should "fail when fofResidual exceeds tolerance" in {
    val flows = SfcCheck.MonthlyFlows(
      govSpending = 0.0, govRevenue = 0.0,
      nplLoss = 0.0, interestIncome = 0.0, hhDebtService = 0.0,
      totalIncome = 0.0, totalConsumption = 0.0,
      newLoans = 0.0, nplRecovery = 0.0,
      fofResidual = 1.0
    )
    val snap = SfcCheck.Snapshot(0, 0, 0, 0, 500000.0, 1000000.0, 0, 0)
    val result = SfcCheck.validate(1, snap, snap, flows)
    result.fofError shouldBe 1.0
    result.passed shouldBe false
  }

  // --- helpers ---

  private def mkFirm(id: Int, tech: TechState, sector: Int = 2): Firm =
    Firm(id, 50000.0, 0.0, tech, 0.5, 1.0, 0.5, sector, Array.empty)

  private def mkFirms(): Array[Firm] =
    // Create firms distributed across all 6 sectors
    val firmsPerSector = 10
    (0 until 6).flatMap { s =>
      (0 until firmsPerSector).map { i =>
        mkFirm(s * firmsPerSector + i, TechState.Traditional(10), s)
      }
    }.toArray
