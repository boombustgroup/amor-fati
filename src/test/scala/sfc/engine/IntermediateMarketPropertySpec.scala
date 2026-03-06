package sfc.engine

import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import sfc.Generators.*
import sfc.agents.{Firm, TechState}
import sfc.config.Config
import sfc.types.*

class IntermediateMarketPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  private val defaultMatrix = Config.IoMatrix
  private val defaultColSums = Config.IoColumnSums

  private def makeFirms(n: Int, sectors: Seq[Int] = Seq(0, 1, 2, 3, 4, 5)): Array[Firm.State] =
    (0 until n).map { i =>
      val sector = sectors(i % sectors.length)
      Firm.State(
        FirmId(i),
        PLN(500000.0),
        PLN.Zero,
        TechState.Traditional(10),
        Ratio(0.5),
        1.0,
        Ratio(0.4),
        SectorIdx(sector),
        Array.empty[Int],
      )
    }.toArray

  // --- Zero-sum property ---

  "IntermediateMarket.process" should "be zero-sum within tolerance" in {
    forAll(Gen.choose(0.8, 1.5), genPrice) { (demandMult: Double, price: Double) =>
      val firms = makeFirms(60)
      val r = IntermediateMarket.process(firms, Vector.fill(6)(demandMult), price, defaultMatrix, defaultColSums, 1.0)
      val totalAdj = r.firms.zip(firms).map { (nf, of) => (nf.cash - of.cash).toDouble }.sum
      Math.abs(totalAdj) should be < 1.0
    }
  }

  it should "produce no changes with zero matrix" in {
    val zeroMatrix = Vector.fill(6)(Vector.fill(6)(0.0))
    val zeroColSums = Vector.fill(6)(0.0)
    val firms = makeFirms(30)
    val r = IntermediateMarket.process(firms, Vector.fill(6)(1.0), 1.0, zeroMatrix, zeroColSums, 1.0)
    for i <- firms.indices do r.firms(i).cash.toDouble shouldBe firms(i).cash.toDouble
    r.totalPaid shouldBe 0.0
  }

  it should "exclude bankrupt firms" in {
    val firms = makeFirms(12).zipWithIndex.map { (f, i) =>
      if i == 0 then f.copy(tech = TechState.Bankrupt("test")) else f
    }
    val r = IntermediateMarket.process(firms, Vector.fill(6)(1.0), 1.0, defaultMatrix, defaultColSums, 1.0)
    r.firms(0).cash.toDouble shouldBe firms(0).cash.toDouble
  }

  it should "scale linearly with IO_SCALE" in {
    val firms = makeFirms(60)
    forAll(Gen.choose(0.1, 0.9)) { (scale: Double) =>
      val r1 = IntermediateMarket.process(firms, Vector.fill(6)(1.0), 1.0, defaultMatrix, defaultColSums, 1.0)
      val rS = IntermediateMarket.process(firms, Vector.fill(6)(1.0), 1.0, defaultMatrix, defaultColSums, scale)
      if r1.totalPaid > 0 then rS.totalPaid shouldBe (r1.totalPaid * scale +- (r1.totalPaid * 0.01))
    }
  }

  it should "produce no changes with scale=0" in {
    val firms = makeFirms(30)
    val r = IntermediateMarket.process(firms, Vector.fill(6)(1.0), 1.0, defaultMatrix, defaultColSums, 0.0)
    for i <- firms.indices do r.firms(i).cash.toDouble shouldBe firms(i).cash.toDouble
    r.totalPaid shouldBe 0.0
  }

  it should "scale with sectorMults" in {
    val firms = makeFirms(60)
    val r1 = IntermediateMarket.process(firms, Vector.fill(6)(1.0), 1.0, defaultMatrix, defaultColSums, 1.0)
    val r2 = IntermediateMarket.process(firms, Vector.fill(6)(2.0), 1.0, defaultMatrix, defaultColSums, 1.0)
    if r1.totalPaid > 0 then r2.totalPaid should be > r1.totalPaid
  }

  it should "scale with price" in {
    val firms = makeFirms(60)
    val r1 = IntermediateMarket.process(firms, Vector.fill(6)(1.0), 1.0, defaultMatrix, defaultColSums, 1.0)
    val r2 = IntermediateMarket.process(firms, Vector.fill(6)(1.0), 2.0, defaultMatrix, defaultColSums, 1.0)
    if r1.totalPaid > 0 then r2.totalPaid should be > r1.totalPaid
  }

  it should "produce non-negative totalPaid" in {
    forAll(Gen.choose(0.5, 2.0), genPrice, Gen.choose(0.0, 1.0)) { (dm: Double, price: Double, scale: Double) =>
      val firms = makeFirms(30)
      val r = IntermediateMarket.process(firms, Vector.fill(6)(dm), price, defaultMatrix, defaultColSums, scale)
      r.totalPaid should be >= 0.0
    }
  }

  it should "have net zero adjustment for single-sector intra-trade" in {
    val firms = makeFirms(10, Seq(1))
    val r = IntermediateMarket.process(firms, Vector.fill(6)(1.0), 1.0, defaultMatrix, defaultColSums, 1.0)
    val totalAdj = r.firms.zip(firms).map { (nf, of) => (nf.cash - of.cash).toDouble }.sum
    Math.abs(totalAdj) should be < 1.0
  }

  it should "distribute revenue proportionally within sector" in {
    val f1 = Firm.State(
      FirmId(0),
      PLN(500000.0),
      PLN.Zero,
      TechState.Traditional(10),
      Ratio(0.5),
      1.0,
      Ratio(0.4),
      SectorIdx(0),
      Array.empty[Int],
    )
    val f2 = Firm.State(
      FirmId(1),
      PLN(500000.0),
      PLN.Zero,
      TechState.Traditional(10),
      Ratio(0.5),
      1.0,
      Ratio(0.4),
      SectorIdx(0),
      Array.empty[Int],
    )
    val f3 = Firm.State(
      FirmId(2),
      PLN(500000.0),
      PLN.Zero,
      TechState.Traditional(10),
      Ratio(0.5),
      1.0,
      Ratio(0.4),
      SectorIdx(1),
      Array.empty[Int],
    )
    val firms = Array(f1, f2, f3)
    val r = IntermediateMarket.process(firms, Vector.fill(6)(1.0), 1.0, defaultMatrix, defaultColSums, 1.0)
    val adj1 = (r.firms(0).cash - firms(0).cash).toDouble
    val adj2 = (r.firms(1).cash - firms(1).cash).toDouble
    adj1 shouldBe (adj2 +- 1e-6)
  }
