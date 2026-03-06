package sfc.engine.steps

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.agents.{Firm, TechState}
import sfc.types.*

import scala.util.Random

class DynamicNetworkSpec extends AnyFlatSpec with Matchers:

  "PriceEquityStep.rewireFirms" should "return unchanged firms when rho=0" in {
    Random.setSeed(42)
    val firms = mkFirms(20)
    val result = PriceEquityStep.rewireFirms(firms, 0.0)
    result shouldBe theSameInstanceAs(firms)
  }

  it should "preserve total firm count" in {
    Random.setSeed(42)
    val firms = mkFirmsWithBankrupt(20, 5)
    val result = PriceEquityStep.rewireFirms(firms, 1.0)
    result.length shouldBe 20
  }

  it should "replace bankrupt firms with Traditional when rho=1.0" in {
    Random.setSeed(42)
    val firms = mkFirmsWithBankrupt(20, 5)
    firms.count(!Firm.isAlive(_)) shouldBe 5

    val result = PriceEquityStep.rewireFirms(firms, 1.0)
    // All bankrupt firms should be replaced
    result.count(!Firm.isAlive(_)) shouldBe 0
    // Replaced firms should be Traditional
    for i <- 0 until 5 do result(i).tech shouldBe a[TechState.Traditional]
  }

  it should "give new firms neighbors" in {
    Random.setSeed(42)
    val firms = mkFirmsWithBankrupt(30, 3)
    val result = PriceEquityStep.rewireFirms(firms, 1.0)
    // Replaced firms (indices 0-2) should have neighbors
    for i <- 0 until 3 do result(i).neighbors.length should be > 0
  }

  it should "return unchanged when no bankrupt firms exist" in {
    Random.setSeed(42)
    val firms = mkFirms(20)
    val result = PriceEquityStep.rewireFirms(firms, 1.0)
    result shouldBe theSameInstanceAs(firms)
  }

  it should "preserve sector assignment" in {
    Random.setSeed(42)
    val firms = mkFirmsWithBankrupt(20, 3, sector = 2)
    val result = PriceEquityStep.rewireFirms(firms, 1.0)
    for i <- 0 until 3 do result(i).sector.toInt shouldBe 2
  }

  it should "set initialSize on new entrants matching their worker count" in {
    Random.setSeed(42)
    val firms = mkFirmsWithBankrupt(20, 3)
    val result = PriceEquityStep.rewireFirms(firms, 1.0)
    for i <- 0 until 3 do
      val f = result(i)
      f.initialSize should be > 0
      Firm.workers(f) shouldBe f.initialSize
  }

  private def mkFirms(n: Int): Array[Firm.State] =
    (0 until n).map { i =>
      Firm.State(
        FirmId(i),
        PLN(50000.0),
        PLN.Zero,
        TechState.Traditional(10),
        Ratio(0.5),
        1.0,
        Ratio(0.5),
        SectorIdx(0),
        Array(FirmId((i + 1) % n), FirmId((i - 1 + n) % n)),
      )
    }.toArray

  private def mkFirmsWithBankrupt(n: Int, nBankrupt: Int, sector: Int = 0): Array[Firm.State] =
    (0 until n).map { i =>
      val tech =
        if i < nBankrupt then TechState.Bankrupt("test")
        else TechState.Traditional(10)
      Firm.State(
        FirmId(i),
        PLN(50000.0),
        PLN.Zero,
        tech,
        Ratio(0.5),
        1.0,
        Ratio(0.5),
        SectorIdx(sector),
        Array(FirmId((i + 1) % n), FirmId((i - 1 + n) % n)),
      )
    }.toArray
