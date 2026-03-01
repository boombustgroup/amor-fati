package sfc.dynamics

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.agents.{Firm, FirmOps, TechState}
import sfc.config.Config

import scala.util.Random

class DynamicNetworkSpec extends AnyFlatSpec with Matchers:

  "DynamicNetwork.rewire" should "return unchanged firms when rho=0" in {
    Random.setSeed(42)
    val firms = mkFirms(20)
    val result = DynamicNetwork.rewire(firms, 0.0)
    result shouldBe theSameInstanceAs(firms)
  }

  it should "preserve total firm count" in {
    Random.setSeed(42)
    val firms = mkFirmsWithBankrupt(20, 5)
    val result = DynamicNetwork.rewire(firms, 1.0)
    result.length shouldBe 20
  }

  it should "replace bankrupt firms with Traditional when rho=1.0" in {
    Random.setSeed(42)
    val firms = mkFirmsWithBankrupt(20, 5)
    firms.count(!FirmOps.isAlive(_)) shouldBe 5

    val result = DynamicNetwork.rewire(firms, 1.0)
    // All bankrupt firms should be replaced
    result.count(!FirmOps.isAlive(_)) shouldBe 0
    // Replaced firms should be Traditional
    for i <- 0 until 5 do
      result(i).tech shouldBe a[TechState.Traditional]
  }

  it should "give new firms neighbors" in {
    Random.setSeed(42)
    val firms = mkFirmsWithBankrupt(30, 3)
    val result = DynamicNetwork.rewire(firms, 1.0)
    // Replaced firms (indices 0-2) should have neighbors
    for i <- 0 until 3 do
      result(i).neighbors.length should be > 0
  }

  it should "return unchanged when no bankrupt firms exist" in {
    Random.setSeed(42)
    val firms = mkFirms(20)
    val result = DynamicNetwork.rewire(firms, 1.0)
    result shouldBe theSameInstanceAs(firms)
  }

  it should "preserve sector assignment" in {
    Random.setSeed(42)
    val firms = mkFirmsWithBankrupt(20, 3, sector = 2)
    val result = DynamicNetwork.rewire(firms, 1.0)
    for i <- 0 until 3 do
      result(i).sector shouldBe 2
  }

  it should "set initialSize on new entrants matching their worker count" in {
    Random.setSeed(42)
    val firms = mkFirmsWithBankrupt(20, 3)
    val result = DynamicNetwork.rewire(firms, 1.0)
    for i <- 0 until 3 do
      val f = result(i)
      f.initialSize should be > 0
      FirmOps.workers(f) shouldBe f.initialSize
  }

  private def mkFirms(n: Int): Array[Firm] =
    (0 until n).map { i =>
      Firm(i, 50000.0, 0.0, TechState.Traditional(10), 0.5, 1.0, 0.5, 0,
        Array((i + 1) % n, (i - 1 + n) % n))
    }.toArray

  private def mkFirmsWithBankrupt(n: Int, nBankrupt: Int, sector: Int = 0): Array[Firm] =
    (0 until n).map { i =>
      val tech = if i < nBankrupt then TechState.Bankrupt("test")
                 else TechState.Traditional(10)
      Firm(i, 50000.0, 0.0, tech, 0.5, 1.0, 0.5, sector,
        Array((i + 1) % n, (i - 1 + n) % n))
    }.toArray
