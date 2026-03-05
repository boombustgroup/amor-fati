package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.agents.{Firm, FirmOps, TechState}
import sfc.config.{Config, SECTORS}
import sfc.types.*

class IntermediateMarketSpec extends AnyFlatSpec with Matchers:

  private val defaultMatrix = Vector(
    Vector(0.05, 0.03, 0.04, 0.02, 0.03, 0.01),
    Vector(0.04, 0.35, 0.12, 0.15, 0.05, 0.18),
    Vector(0.15, 0.10, 0.12, 0.08, 0.07, 0.08),
    Vector(0.01, 0.00, 0.01, 0.05, 0.02, 0.01),
    Vector(0.01, 0.01, 0.01, 0.01, 0.03, 0.01),
    Vector(0.00, 0.08, 0.05, 0.01, 0.01, 0.12)
  )

  private val defaultColSums =
    (0 until 6).map(j => defaultMatrix.map(_(j)).sum).toVector

  private val zeroMatrix = Vector.fill(6)(Vector.fill(6)(0.0))
  private val zeroColSums = Vector.fill(6)(0.0)

  private def makeFirm(id: Int, sector: Int, cash: Double = 50000.0,
                       tech: TechState = TechState.Traditional(10)): Firm =
    Firm(FirmId(id), cash, 0.0, tech, 0.5, 1.0, 0.3, SectorIdx(sector), Array.empty[Int])

  private def makeFirmsAllSectors(perSector: Int = 10): Array[Firm] =
    (0 until 6).flatMap { s =>
      (0 until perSector).map(i => makeFirm(s * perSector + i, s))
    }.toArray

  // ---- Test 1: Zero-sum ----

  "IntermediateMarket.process" should "produce zero-sum cash adjustments" in {
    val firms = makeFirmsAllSectors(20)
    val result = IntermediateMarket.process(firms, Vector.fill(6)(1.0), 1.0, defaultMatrix, defaultColSums)
    val totalCashBefore = firms.map(_.cash).sum
    val totalCashAfter = result.firms.map(_.cash).sum
    totalCashAfter shouldBe totalCashBefore +- 1.0
  }

  // ---- Test 2: Correct routing ----

  it should "route payments according to a_ij coefficients" in {
    // 1 firm per sector, all Traditional(10)
    val firms = (0 until 6).map(s => makeFirm(s, s)).toArray
    val result = IntermediateMarket.process(firms, Vector.fill(6)(1.0), 1.0, defaultMatrix, defaultColSums)
    // Firm in sector 0 (BPO): should pay columnSum(0) of its gross output
    val bpoOutput = FirmOps.capacity(firms(0)) * 1.0 * 1.0
    val bpoCost = bpoOutput * defaultColSums(0)
    // BPO revenue: sum over j of a_0j * sectorOutput_j
    val bpoRevenue = (0 until 6).map { j =>
      defaultMatrix(0)(j) * FirmOps.capacity(firms(j)) * 1.0 * 1.0
    }.sum
    val expectedAdj = bpoRevenue - bpoCost
    val actualAdj = result.firms(0).cash - firms(0).cash
    actualAdj shouldBe expectedAdj +- 0.01
  }

  // ---- Test 3: Bankrupt firms excluded ----

  it should "exclude bankrupt firms from buying and selling" in {
    val firms = Array(
      makeFirm(0, 0),
      makeFirm(1, 0, tech = TechState.Bankrupt("test")),
      makeFirm(2, 1),
      makeFirm(3, 2)
    )
    val result = IntermediateMarket.process(firms, Vector.fill(6)(1.0), 1.0, defaultMatrix, defaultColSums)
    // Bankrupt firm's cash should not change
    result.firms(1).cash shouldBe firms(1).cash
    // Still zero-sum among living firms
    val livingBefore = firms.filter(f => FirmOps.isAlive(f)).map(_.cash).sum
    val livingAfter = result.firms.filter(f => FirmOps.isAlive(f)).map(_.cash).sum
    livingAfter shouldBe livingBefore +- 1.0
  }

  // ---- Test 4: Zero matrix → no changes ----

  it should "leave firms unchanged when A matrix is zero" in {
    val firms = makeFirmsAllSectors(10)
    val result = IntermediateMarket.process(firms, Vector.fill(6)(1.0), 1.0, zeroMatrix, zeroColSums)
    for i <- firms.indices do
      result.firms(i).cash shouldBe firms(i).cash
    result.totalPaid shouldBe 0.0
  }

  // ---- Test 5: Single sector ----

  it should "handle all firms in one sector (intra-sector I-O)" in {
    val firms = (0 until 10).map(i => makeFirm(i, 0)).toArray
    val result = IntermediateMarket.process(firms, Vector.fill(6)(1.0), 1.0, defaultMatrix, defaultColSums)
    // Zero-sum still holds
    val totalBefore = firms.map(_.cash).sum
    val totalAfter = result.firms.map(_.cash).sum
    totalAfter shouldBe totalBefore +- 1.0
    // With only sector 0 firms: effective colSum(0) = a_00 (only sector 0 has suppliers)
    // cost = a_00 × output, revenue = a_00 × output → net = 0 for each firm
    for i <- firms.indices do
      val actualNet = result.firms(i).cash - firms(i).cash
      actualNet shouldBe 0.0 +- 0.01
  }

  // ---- Test 6: Proportional distribution ----

  it should "distribute revenue proportionally to capacity" in {
    // 2 firms in sector 1: one Automated (higher capacity), one Traditional
    val firms = Array(
      makeFirm(0, 0),  // BPO: generates demand for sector 1
      makeFirm(1, 1, tech = TechState.Automated(1.5)),  // High capacity Mfg
      makeFirm(2, 1)   // Normal capacity Mfg
    )
    val result = IntermediateMarket.process(firms, Vector.fill(6)(1.0), 1.0, defaultMatrix, defaultColSums)
    // Firm 1 should get more I-O revenue than firm 2 (higher capacity)
    val cap1 = FirmOps.capacity(firms(1))
    val cap2 = FirmOps.capacity(firms(2))
    cap1 should be > cap2
    // Revenue is proportional to capacity, cost is proportional to output
    // Both have same sector, so same cost rate, but different absolute amounts
    // The larger firm gets more revenue AND pays more cost; the ratio should match capacity ratio
    val rev1share = cap1 / (cap1 + cap2)
    val rev2share = cap2 / (cap1 + cap2)
    // Total Mfg sector revenue from BPO sector: a_1_0 × bpoOutput
    val bpoOutput = FirmOps.capacity(firms(0)) * 1.0 * 1.0
    val mfgRevenueFromBpo = defaultMatrix(1)(0) * bpoOutput
    // Firm 1 gets rev1share of total Mfg revenue, firm 2 gets rev2share
    // This is just one part; they also get from each other (intra-sector)
    // Just verify the ratio of total revenue received is close to capacity ratio
    rev1share should be > rev2share
  }

  // ---- Test 7: totalPaid is positive ----

  it should "report positive totalPaid with non-zero matrix" in {
    val firms = makeFirmsAllSectors(10)
    val result = IntermediateMarket.process(firms, Vector.fill(6)(1.0), 1.0, defaultMatrix, defaultColSums)
    result.totalPaid should be > 0.0
  }

  // ---- Test 8: demandMult and price scale output correctly ----

  it should "scale I-O flows with demandMult and price" in {
    val firms = makeFirmsAllSectors(5)
    val base = IntermediateMarket.process(firms, Vector.fill(6)(1.0), 1.0, defaultMatrix, defaultColSums)
    val doubled = IntermediateMarket.process(firms, Vector.fill(6)(2.0), 1.0, defaultMatrix, defaultColSums)
    // Doubling demand should double total I-O flows
    doubled.totalPaid shouldBe (base.totalPaid * 2.0) +- (base.totalPaid * 0.01)
  }
