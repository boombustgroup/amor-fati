package com.boombustgroup.amorfati.engine

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.markets.GvcTrade
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class CommodityPriceSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  private val td = ComputationBoundary

  private def mkInput(
      prev: GvcTrade.State,
      month: Int,
      seed: Long = 42L,
  ) =
    GvcTrade.StepInput(
      prev = prev,
      sectorOutputs = Vector.fill(6)(100000.0),
      priceLevel = 1.0,
      exchangeRate = 4.33,
      autoRatio = 0.0,
      month = month,
      rng = new Random(seed * 31 + month),
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
    after.commodityPriceIndex should not be PriceIndex.Base
  }

  it should "compute importCostIndex as foreignPrice × commodity" in {
    val init     = GvcTrade.initial
    val after    = GvcTrade.step(mkInput(init, month = 1))
    val expected = after.foreignPriceIndex * after.commodityPriceIndex
    td.toDouble(after.importCostIndex) shouldBe td.toDouble(expected) +- 1e-10
  }

  it should "evolve commodity differently with different seeds (stochastic)" in {
    val init   = GvcTrade.initial
    val after1 = GvcTrade.step(mkInput(init, month = 1, seed = 1L))
    val after2 = GvcTrade.step(mkInput(init, month = 1, seed = 999L))
    td.toDouble(after1.commodityPriceIndex) should not be td.toDouble(after2.commodityPriceIndex)
  }

  it should "keep commodity close to 1.0 after 1 month with default params" in {
    val init  = GvcTrade.initial
    val after = GvcTrade.step(mkInput(init, month = 1))
    // Default drift is small (~2%/yr), so after 1 month commodity stays near base
    td.toDouble(after.commodityPriceIndex) shouldBe 1.0 +- 0.1
  }

  "PriceIndex" should "multiply two indices" in {
    val a = PriceIndex(1.5)
    val b = PriceIndex(2.0)
    td.toDouble(a * b) shouldBe 3.0 +- 1e-10
  }

  it should "multiply with PLN" in {
    val idx  = PriceIndex(1.5)
    val cost = PLN(1000.0)
    td.toDouble(idx * cost) shouldBe 1500.0 +- 1e-10
  }

  it should "divide two indices to get ratio" in {
    val a = PriceIndex(3.0)
    val b = PriceIndex(1.5)
    (a / b) shouldBe 2.0 +- 1e-10
  }
