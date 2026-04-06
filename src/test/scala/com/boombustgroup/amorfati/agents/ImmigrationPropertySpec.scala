package com.boombustgroup.amorfati.agents

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.types.*

import scala.util.Random

class ImmigrationPropertySpec extends AnyFlatSpec with Matchers:

  import com.boombustgroup.amorfati.config.SimParams
  given SimParams = SimParams.defaults

  "Immigration.computeInflow" should "always return non-negative" in {
    val rng = new Random(42)
    for _ <- 0 until 100 do
      val wage   = PLN(rng.nextDouble() * 20000)
      val unemp  = Share(rng.nextDouble())
      val result = Immigration.computeInflow(wage, unemp)
      result should be >= 0
  }

  "Immigration.computeOutflow" should "always return non-negative" in {
    val rng = new Random(42)
    for _ <- 0 until 100 do
      val stock  = rng.nextInt(10000)
      val result = Immigration.computeOutflow(stock)
      result should be >= 0
  }

  "Immigration.chooseSector" should "produce all 6 sectors over many draws" in {
    val rng     = new Random(42)
    val sectors = (0 until 1000).map(_ => Immigration.chooseSector(rng).toInt).toSet
    sectors.size shouldBe 6
    sectors.foreach { s =>
      s should be >= 0
      s should be < 6
    }
  }

  "Immigration.spawnImmigrants" should "all have isImmigrant=true" in {
    val rng = new Random(42)
    for _ <- 0 until 10 do
      val count      = rng.nextInt(50) + 1
      val immigrants = Immigration.spawnImmigrants(count, 0, rng)
      immigrants.length shouldBe count
      immigrants.foreach(_.isImmigrant shouldBe true)
  }

  it should "have savings in [0, 5000] range" in {
    val rng        = new Random(42)
    val immigrants = Immigration.spawnImmigrants(200, 0, rng)
    immigrants.foreach { h =>
      h.savings should be >= PLN.Zero
      h.savings should be <= PLN(5000.0)
    }
  }

  it should "have zero debt" in {
    val rng        = new Random(42)
    val immigrants = Immigration.spawnImmigrants(50, 0, rng)
    immigrants.foreach(_.debt shouldBe PLN.Zero)
  }

  it should "have rent >= 800" in {
    val rng        = new Random(42)
    val immigrants = Immigration.spawnImmigrants(100, 0, rng)
    immigrants.foreach(_.monthlyRent should be >= PLN(800.0))
  }

  "Immigration.step" should "preserve non-negative stock" in {
    val rng = new Random(42)
    for _ <- 0 until 100 do
      val prevStock = rng.nextInt(5000)
      val prev      = Immigration.State(prevStock, 0, 0, PLN.Zero)
      val result    = Immigration.step(prev, Vector.empty, PLN(8000.0), Share(0.05))
      result.immigrantStock should be >= 0
  }

  "Immigration.removeReturnMigrants" should "never remove more immigrants than exist" in {
    val hhs    = (0 until 10).map { i =>
      Household.State(
        HhId(i),
        PLN(1000.0),
        PLN.Zero,
        PLN(1800.0),
        Share(0.5),
        Share(0.0),
        Share(0.85),
        HhStatus.Employed(FirmId(0), SectorIdx(0), PLN(6000.0)),
        Array.empty[HhId],
        bankId = BankId(0),
        equityWealth = PLN.Zero,
        lastSectorIdx = SectorIdx(-1),
        isImmigrant = i >= 5,
        numDependentChildren = 0,
        consumerDebt = PLN.Zero,
        education = 2,
        taskRoutineness = Share(0.5),
        wageScar = Share.Zero,
      ) // 5 natives + 5 immigrants
    }.toVector
    // Request removing 100, but only 5 immigrants exist
    val result = Immigration.removeReturnMigrants(hhs, 100)
    result.length shouldBe 5 // 5 natives remain
    result.foreach(_.isImmigrant shouldBe false)
  }
