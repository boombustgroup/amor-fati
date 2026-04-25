package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.markets.GvcTrade
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import com.boombustgroup.amorfati.random.RandomStream

class CommodityPriceSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  private def mkInput(
      prev: GvcTrade.State,
      month: Int,
      seed: Long = 42L,
  ) =
    GvcTrade.StepInput(
      prev = prev,
      sectorOutputs = Vector.fill(6)(PLN("100000.0")),
      priceLevel = PriceIndex.Base,
      exchangeRate = ExchangeRate("4.33"),
      autoRatio = Share.Zero,
      month = ExecutionMonth(month),
      rng = RandomStream.seeded(seed * 31 + month),
    )

  "GvcTrade.step" should "initialize with PriceIndex.Base for all price indices" in {
    val init = GvcTrade.initial
    init.foreignPriceIndex shouldBe PriceIndex.Base
    init.importCostIndex shouldBe PriceIndex.Base
    init.commodityPriceIndex shouldBe PriceIndex.Base
  }

  it should "evolve commodityPriceIndex with drift" in {
    val init  = GvcTrade.initial
    val after = GvcTrade.step(mkInput(init, month = 1))
    // With default drift 2%/yr = ~0.17%/month, commodity should change
    after.commodityPriceIndex.should(not(be(PriceIndex.Base)))
  }

  it should "compute importCostIndex as foreignPrice × commodity" in {
    val init     = GvcTrade.initial
    val after    = GvcTrade.step(mkInput(init, month = 1))
    val expected = after.foreignPriceIndex * after.commodityPriceIndex
    after.importCostIndex.bd shouldBe (expected.bd +- BigDecimal("1e-10"))
  }

  it should "evolve commodity differently with different seeds (stochastic)" in {
    val init   = GvcTrade.initial
    val after1 = GvcTrade.step(mkInput(init, month = 1, seed = 1L))
    val after2 = GvcTrade.step(mkInput(init, month = 1, seed = 999L))
    after1.commodityPriceIndex.bd.should(not(be(after2.commodityPriceIndex.bd)))
  }

  it should "keep commodity close to 1.0 after 1 month with default params" in {
    val init  = GvcTrade.initial
    val after = GvcTrade.step(mkInput(init, month = 1))
    // Default drift is small (~2%/yr), so after 1 month commodity stays near base
    after.commodityPriceIndex.bd shouldBe (BigDecimal("1.0") +- BigDecimal("0.1"))
  }

  "PriceIndex" should "multiply two indices" in {
    val a = PriceIndex("1.5")
    val b = PriceIndex("2.0")
    (a * b).bd shouldBe (BigDecimal("3.0") +- BigDecimal("1e-10"))
  }

  it should "multiply with PLN" in {
    val idx  = PriceIndex("1.5")
    val cost = PLN("1000.0")
    (idx * cost).bd shouldBe (BigDecimal("1500.0") +- BigDecimal("1e-10"))
  }

  it should "divide two indices to get ratio" in {
    val a = PriceIndex("3.0")
    val b = PriceIndex("1.5")
    a.ratioTo(b).bd shouldBe (BigDecimal("2.0") +- BigDecimal("1e-10"))
  }
