package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.agents.{Firm, TechState}
import sfc.types.*

class IntermediateMarketSpec extends AnyFlatSpec with Matchers:

  private val defaultMatrix = Vector(
    Vector(0.05, 0.03, 0.04, 0.02, 0.03, 0.01),
    Vector(0.04, 0.35, 0.12, 0.15, 0.05, 0.18),
    Vector(0.15, 0.10, 0.12, 0.08, 0.07, 0.08),
    Vector(0.01, 0.00, 0.01, 0.05, 0.02, 0.01),
    Vector(0.01, 0.01, 0.01, 0.01, 0.03, 0.01),
    Vector(0.00, 0.08, 0.05, 0.01, 0.01, 0.12),
  )

  private val defaultColSums =
    (0 until 6).map(j => defaultMatrix.map(_(j)).sum).toVector

  private val zeroMatrix = Vector.fill(6)(Vector.fill(6)(0.0))
  private val zeroColSums = Vector.fill(6)(0.0)

  private def makeFirm(
    id: Int,
    sector: Int,
    cash: Double = 50000.0,
    tech: TechState = TechState.Traditional(10),
  ): Firm.State =
    Firm.State(FirmId(id), PLN(cash), PLN.Zero, tech, Ratio(0.5), 1.0, Ratio(0.3), SectorIdx(sector), Array.empty[Int])

  private def makeFirmsAllSectors(perSector: Int = 10): Array[Firm.State] =
    (0 until 6).flatMap { s =>
      (0 until perSector).map(i => makeFirm(s * perSector + i, s))
    }.toArray

  // ---- Test 1: Zero-sum ----

  "IntermediateMarket.process" should "produce zero-sum cash adjustments" in {
    val firms = makeFirmsAllSectors(20)
    val result = IntermediateMarket.process(firms, Vector.fill(6)(1.0), 1.0, defaultMatrix, defaultColSums)
    val totalCashBefore = firms.map(_.cash.toDouble).sum
    val totalCashAfter = result.firms.map(_.cash.toDouble).sum
    totalCashAfter shouldBe totalCashBefore +- 1.0
  }

  // ---- Test 2: Correct routing ----

  it should "route payments according to a_ij coefficients" in {
    // 1 firm per sector, all Traditional(10)
    val firms = (0 until 6).map(s => makeFirm(s, s)).toArray
    val result = IntermediateMarket.process(firms, Vector.fill(6)(1.0), 1.0, defaultMatrix, defaultColSums)
    // Firm in sector 0 (BPO): should pay columnSum(0) of its gross output
    val bpoOutput = Firm.capacity(firms(0)) * 1.0 * 1.0
    val bpoCost = bpoOutput * defaultColSums(0)
    // BPO revenue: sum over j of a_0j * sectorOutput_j
    val bpoRevenue = (0 until 6).map { j =>
      defaultMatrix(0)(j) * Firm.capacity(firms(j)) * 1.0 * 1.0
    }.sum
    val expectedAdj = bpoRevenue - bpoCost
    val actualAdj = (result.firms(0).cash - firms(0).cash).toDouble
    actualAdj shouldBe expectedAdj +- 0.01
  }

  // ---- Test 3: Bankrupt firms excluded ----

  it should "exclude bankrupt firms from buying and selling" in {
    val firms = Array(
      makeFirm(0, 0),
      makeFirm(1, 0, tech = TechState.Bankrupt("test")),
      makeFirm(2, 1),
      makeFirm(3, 2),
    )
    val result = IntermediateMarket.process(firms, Vector.fill(6)(1.0), 1.0, defaultMatrix, defaultColSums)
    // Bankrupt firm's cash should not change
    result.firms(1).cash.toDouble shouldBe firms(1).cash.toDouble
    // Still zero-sum among living firms
    val livingBefore = firms.filter(f => Firm.isAlive(f)).map(_.cash.toDouble).sum
    val livingAfter = result.firms.filter(f => Firm.isAlive(f)).map(_.cash.toDouble).sum
    livingAfter shouldBe livingBefore +- 1.0
  }

  // ---- Test 4: Zero matrix → no changes ----

  it should "leave firms unchanged when A matrix is zero" in {
    val firms = makeFirmsAllSectors(10)
    val result = IntermediateMarket.process(firms, Vector.fill(6)(1.0), 1.0, zeroMatrix, zeroColSums)
    for i <- firms.indices do result.firms(i).cash.toDouble shouldBe firms(i).cash.toDouble
    result.totalPaid shouldBe 0.0
  }

  // ---- Test 5: Single sector ----

  it should "handle all firms in one sector (intra-sector I-O)" in {
    val firms = (0 until 10).map(i => makeFirm(i, 0)).toArray
    val result = IntermediateMarket.process(firms, Vector.fill(6)(1.0), 1.0, defaultMatrix, defaultColSums)
    // Zero-sum still holds
    val totalBefore = firms.map(_.cash.toDouble).sum
    val totalAfter = result.firms.map(_.cash.toDouble).sum
    totalAfter shouldBe totalBefore +- 1.0
    // With only sector 0 firms: effective colSum(0) = a_00 (only sector 0 has suppliers)
    // cost = a_00 × output, revenue = a_00 × output → net = 0 for each firm
    for i <- firms.indices do
      val actualNet = (result.firms(i).cash - firms(i).cash).toDouble
      actualNet shouldBe 0.0 +- 0.01
  }

  // ---- Test 6: Proportional distribution ----

  it should "distribute revenue proportionally to capacity" in {
    // Many firms across all sectors so net I-O flows are meaningful;
    // sector 1 has one Automated (higher capacity) and one Traditional.
    val baseFirms = (0 until 6).flatMap { s =>
      (0 until 10).map(i => makeFirm(s * 10 + i, s))
    }.toArray
    // Replace two sector-1 firms with our test subjects
    val firm1 = makeFirm(100, 1, tech = TechState.Automated(1.5)) // High capacity Mfg
    val firm2 = makeFirm(101, 1) // Normal capacity Mfg
    val firms = baseFirms.filter(_.sector.toInt != 1) ++ Array(firm1, firm2)
    val result = IntermediateMarket.process(firms, Vector.fill(6)(1.0), 1.0, defaultMatrix, defaultColSums)
    val r1 = result.firms.find(_.id == FirmId(100)).get
    val r2 = result.firms.find(_.id == FirmId(101)).get
    // Higher capacity firm receives more I-O revenue, so net cash gain is higher
    val delta1 = r1.cash.toDouble - firm1.cash.toDouble
    val delta2 = r2.cash.toDouble - firm2.cash.toDouble
    delta1 should be > delta2
    result.totalPaid should be > 0.0
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
