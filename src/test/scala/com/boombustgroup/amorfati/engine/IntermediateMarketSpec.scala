package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.TestFirmState

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.agents.{BankruptReason, Firm, TechState}
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.markets.IntermediateMarket
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IntermediateMarketSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  private val defaultMatrix = Vector(
    Vector(Share.decimal(5, 2), Share.decimal(3, 2), Share.decimal(4, 2), Share.decimal(2, 2), Share.decimal(3, 2), Share.decimal(1, 2)),
    Vector(Share.decimal(4, 2), Share.decimal(35, 2), Share.decimal(12, 2), Share.decimal(15, 2), Share.decimal(5, 2), Share.decimal(18, 2)),
    Vector(Share.decimal(15, 2), Share.decimal(10, 2), Share.decimal(12, 2), Share.decimal(8, 2), Share.decimal(7, 2), Share.decimal(8, 2)),
    Vector(Share.decimal(1, 2), Share.Zero, Share.decimal(1, 2), Share.decimal(5, 2), Share.decimal(2, 2), Share.decimal(1, 2)),
    Vector(Share.decimal(1, 2), Share.decimal(1, 2), Share.decimal(1, 2), Share.decimal(1, 2), Share.decimal(3, 2), Share.decimal(1, 2)),
    Vector(Share.Zero, Share.decimal(8, 2), Share.decimal(5, 2), Share.decimal(1, 2), Share.decimal(1, 2), Share.decimal(12, 2)),
  )

  private val defaultColSums =
    (0 until 6).map(j => defaultMatrix.map(_(j)).foldLeft(Share.Zero)(_ + _)).toVector

  private val zeroMatrix  = Vector.fill(6)(Vector.fill(6)(Share.Zero))
  private val zeroColSums = Vector.fill(6)(Share.Zero)

  private def makeFirm(
      id: Int,
      sector: Int,
      cash: BigDecimal = BigDecimal("50000.0"),
      tech: TechState = TechState.Traditional(10),
  ): Firm.State =
    TestFirmState(
      FirmId(id),
      plnBD(cash),
      PLN.Zero,
      tech,
      Share.decimal(5, 1),
      Multiplier.One,
      Share.decimal(3, 1),
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

  @annotation.nowarn("msg=unused private member") // default used by callers
  private def makeFirmsAllSectors(perSector: Int = 10): Vector[Firm.State] =
    (0 until 6).flatMap { s =>
      (0 until perSector).map(i => makeFirm(s * perSector + i, s))
    }.toVector

  private def baseInput(firms: Vector[Firm.State]) = IntermediateMarket.Input(
    firms = firms,
    sectorMults = Vector.fill(6)(Multiplier.One),
    price = PriceIndex.Base,
    ioMatrix = defaultMatrix,
    columnSums = defaultColSums,
  )

  // ---- Test 1: Zero-sum ----

  "IntermediateMarket.process" should "produce zero-sum cash adjustments" in {
    val firms  = makeFirmsAllSectors(20)
    val result = IntermediateMarket.process(baseInput(firms))
    decimal(result.cashAdjustments.sumPln) shouldBe (decimal(PLN.Zero) +- decimal(PLN(1)))
    result.firms shouldBe firms
  }

  // ---- Test 2: Correct routing ----

  it should "route payments according to a_ij coefficients" in {
    // 1 firm per sector, all Traditional(10)
    val firms       = (0 until 6).map(s => makeFirm(s, s)).toVector
    val result      = IntermediateMarket.process(baseInput(firms))
    // Firm in sector 0 (BPO): should pay columnSum(0) of its gross output
    val bpoOutput   = Firm.computeCapacity(firms(0)).bd
    val bpoCost     = bpoOutput * defaultColSums(0).bd
    // BPO revenue: sum over j of a_0j * sectorOutput_j
    val bpoRevenue  = (0 until 6).map { j =>
      defaultMatrix(0)(j).bd * Firm.computeCapacity(firms(j)).bd
    }.sum
    val expectedAdj = bpoRevenue - bpoCost
    val actualAdj   = result.cashAdjustments(0).bd
    actualAdj shouldBe (expectedAdj +- BigDecimal("0.01"))
  }

  // ---- Test 3: Bankrupt firms excluded ----

  it should "exclude bankrupt firms from buying and selling" in {
    val firms  = Vector(
      makeFirm(0, 0),
      makeFirm(1, 0, tech = TechState.Bankrupt(BankruptReason.Other("test"))),
      makeFirm(2, 1),
      makeFirm(3, 2),
    )
    val result = IntermediateMarket.process(baseInput(firms))
    // Bankrupt firm should not receive a cash settlement.
    result.cashAdjustments(1) shouldBe PLN.Zero
    // Still zero-sum among living firms
    decimal(firms.zip(result.cashAdjustments).filter((firm, _) => Firm.isAlive(firm)).map(_._2).sumPln) shouldBe (decimal(PLN.Zero) +- decimal(PLN(1)))
  }

  // ---- Test 4: Zero matrix -> no changes ----

  it should "leave firms unchanged when A matrix is zero" in {
    val firms  = makeFirmsAllSectors(10)
    val result = IntermediateMarket.process(baseInput(firms).copy(ioMatrix = zeroMatrix, columnSums = zeroColSums))
    result.cashAdjustments.foreach(_ shouldBe PLN.Zero)
    result.totalPaid shouldBe PLN.Zero
  }

  // ---- Test 5: Single sector ----

  it should "handle all firms in one sector (intra-sector I-O)" in {
    val firms  = (0 until 10).map(i => makeFirm(i, 0)).toVector
    val result = IntermediateMarket.process(baseInput(firms))
    // Zero-sum still holds
    decimal(result.cashAdjustments.sumPln) shouldBe (decimal(PLN.Zero) +- decimal(PLN(1)))
    // With only sector 0 firms: effective colSum(0) = a_00 (only sector 0 has suppliers)
    // cost = a_00 x output, revenue = a_00 x output -> net = 0 for each firm
    for i <- firms.indices do
      val actualNet = result.cashAdjustments(i).bd
      actualNet shouldBe (BigDecimal(0) +- BigDecimal("0.01"))
  }

  // ---- Test 6: Proportional distribution ----

  it should "distribute revenue proportionally to capacity" in {
    // Many firms across all sectors so net I-O flows are meaningful;
    // sector 1 has one Automated (higher capacity) and one Traditional.
    val baseFirms = (0 until 6).flatMap { s =>
      (0 until 10).map(i => makeFirm(s * 10 + i, s))
    }.toVector
    // Replace two sector-1 firms with our test subjects
    val firm1     = makeFirm(100, 1, tech = TechState.Automated(Multiplier.decimal(15, 1))) // High capacity Mfg
    val firm2     = makeFirm(101, 1)                                                        // Normal capacity Mfg
    val firms     = baseFirms.filter(_.sector.toInt != 1) ++ Vector(firm1, firm2)
    val result    = IntermediateMarket.process(baseInput(firms))
    // Higher capacity firm receives more I-O revenue, so net cash gain is higher
    val delta1    = result.cashAdjustments(firms.indexWhere(_.id == FirmId(100))).bd
    val delta2    = result.cashAdjustments(firms.indexWhere(_.id == FirmId(101))).bd
    delta1 should be > delta2
    result.totalPaid should be > PLN.Zero
  }

  // ---- Test 7: totalPaid is positive ----

  it should "report positive totalPaid with non-zero matrix" in {
    val firms  = makeFirmsAllSectors(10)
    val result = IntermediateMarket.process(baseInput(firms))
    result.totalPaid should be > PLN.Zero
  }

  // ---- Test 8: demandMult and price scale output correctly ----

  it should "scale I-O flows with demandMult and price" in {
    val firms   = makeFirmsAllSectors(5)
    val base    = IntermediateMarket.process(baseInput(firms))
    val doubled = IntermediateMarket.process(baseInput(firms).copy(sectorMults = Vector.fill(6)(Multiplier(2))))
    // Doubling demand should double total I-O flows
    doubled.totalPaid.bd shouldBe ((base.totalPaid.bd * 2) +- (base.totalPaid.bd * BigDecimal("0.01")))
  }
