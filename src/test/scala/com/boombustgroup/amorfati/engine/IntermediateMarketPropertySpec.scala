package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.Generators.*
import com.boombustgroup.amorfati.agents.{BankruptReason, Firm, TechState}
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.markets.IntermediateMarket
import com.boombustgroup.amorfati.types.*
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class IntermediateMarketPropertySpec extends AnyFlatSpec with Matchers with ScalaCheckPropertyChecks:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 200)

  private val defaultMatrix  = p.io.matrix
  private val defaultColSums = p.io.columnSums

  private def makeFirms(n: Int, sectors: Seq[Int] = Seq(0, 1, 2, 3, 4, 5)): Vector[Firm.State] =
    (0 until n).map { i =>
      val sector = sectors(i % sectors.length)
      Firm.State(
        FirmId(i),
        PLN(500000.0),
        PLN.Zero,
        TechState.Traditional(10),
        Share(0.5),
        Multiplier.One,
        Share(0.4),
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
    }.toVector

  private def baseInput(
      firms: Vector[Firm.State],
      scale: Multiplier = Multiplier.One,
      price: PriceIndex = PriceIndex.Base,
      demandMult: Multiplier = Multiplier.One,
  ) =
    IntermediateMarket.Input(
      firms = firms,
      sectorMults = Vector.fill(6)(demandMult),
      price = price,
      ioMatrix = defaultMatrix,
      columnSums = defaultColSums,
      scale = scale,
    )

  // --- Zero-sum property ---

  "IntermediateMarket.process" should "be zero-sum within tolerance" in
    forAll(Gen.choose(0.8, 1.5), genPrice) { (demandMult: Double, price: Double) =>
      val firms    = makeFirms(60)
      val r        = IntermediateMarket.process(baseInput(firms, price = PriceIndex(price), demandMult = Multiplier(demandMult)))
      val totalAdj = r.firms.zip(firms).map((nf, of) => (nf.cash - of.cash).bd).sum
      totalAdj.abs should be < BigDecimal("1.0")
    }

  it should "produce no changes with zero matrix" in {
    val zeroMatrix  = Vector.fill(6)(Vector.fill(6)(Share.Zero))
    val zeroColSums = Vector.fill(6)(Share.Zero)
    val firms       = makeFirms(30)
    val r           =
      IntermediateMarket.process(IntermediateMarket.Input(firms, Vector.fill(6)(Multiplier.One), PriceIndex.Base, zeroMatrix, zeroColSums))
    for i <- firms.indices do r.firms(i).cash shouldBe firms(i).cash
    r.totalPaid shouldBe PLN.Zero
  }

  it should "exclude bankrupt firms" in {
    val firms = makeFirms(12).zipWithIndex.map { (f, i) =>
      if i == 0 then f.copy(tech = TechState.Bankrupt(BankruptReason.Other("test"))) else f
    }
    val r     = IntermediateMarket.process(baseInput(firms))
    r.firms(0).cash shouldBe firms(0).cash
  }

  it should "scale linearly with IO_SCALE" in {
    val firms = makeFirms(60)
    forAll(Gen.choose(0.1, 0.9)) { (scale: Double) =>
      val r1 = IntermediateMarket.process(baseInput(firms))
      val rS = IntermediateMarket.process(baseInput(firms, scale = Multiplier(scale)))
      if r1.totalPaid > PLN.Zero then rS.totalPaid.bd shouldBe ((r1.totalPaid.bd * BigDecimal.decimal(scale)) +- (r1.totalPaid.bd * BigDecimal("0.01")))
    }
  }

  it should "produce no changes with scale=0" in {
    val firms = makeFirms(30)
    val r     = IntermediateMarket.process(baseInput(firms, scale = Multiplier.Zero))
    for i <- firms.indices do r.firms(i).cash shouldBe firms(i).cash
    r.totalPaid shouldBe PLN.Zero
  }

  it should "scale with sectorMults" in {
    val firms = makeFirms(60)
    val r1    = IntermediateMarket.process(baseInput(firms))
    val r2    = IntermediateMarket.process(baseInput(firms, demandMult = Multiplier(2.0)))
    if r1.totalPaid > PLN.Zero then r2.totalPaid should be > r1.totalPaid
  }

  it should "scale with price" in {
    val firms = makeFirms(60)
    val r1    = IntermediateMarket.process(baseInput(firms))
    val r2    = IntermediateMarket.process(baseInput(firms, price = PriceIndex(2.0)))
    if r1.totalPaid > PLN.Zero then r2.totalPaid should be > r1.totalPaid
  }

  it should "produce non-negative totalPaid" in
    forAll(Gen.choose(0.5, 2.0), genPrice, Gen.choose(0.0, 1.0)) { (dm: Double, price: Double, scale: Double) =>
      val firms = makeFirms(30)
      val r     =
        IntermediateMarket.process(baseInput(firms, scale = Multiplier(scale), price = PriceIndex(price), demandMult = Multiplier(dm)))
      r.totalPaid should be >= PLN.Zero
    }

  it should "have net zero adjustment for single-sector intra-trade" in {
    val firms    = makeFirms(10, Seq(1))
    val r        = IntermediateMarket.process(baseInput(firms))
    val totalAdj = r.firms.zip(firms).map((nf, of) => (nf.cash - of.cash).bd).sum
    totalAdj.abs should be < BigDecimal("1.0")
  }

  it should "distribute revenue proportionally within sector" in {
    def mkF(id: Int, sec: Int): Firm.State = Firm.State(
      FirmId(id),
      PLN(500000.0),
      PLN.Zero,
      TechState.Traditional(10),
      Share(0.5),
      Multiplier.One,
      Share(0.4),
      SectorIdx(sec),
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
    val f1                                 = mkF(0, 0)
    val f2                                 = mkF(1, 0)
    val f3                                 = mkF(2, 1)
    val firms                              = Vector(f1, f2, f3)
    val r                                  = IntermediateMarket.process(baseInput(firms))
    val adj1                               = (r.firms(0).cash - firms(0).cash).bd
    val adj2                               = (r.firms(1).cash - firms(1).cash).bd
    adj1.shouldBe(adj2 +- BigDecimal("0.000001"))
  }
