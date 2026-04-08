package com.boombustgroup.amorfati.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.markets.GvcTrade
import com.boombustgroup.amorfati.types.*

class ExternalSectorSpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]
  private val td           = ComputationBoundary

  private val sectorOutputs = Vector(30000.0, 160000.0, 450000.0, 60000.0, 220000.0, 80000.0)

  private def baseInput(
      prev: GvcTrade.State = GvcTrade.initial,
      er: Double = p.forex.baseExRate,
      price: Double = 1.0,
      autoR: Double = 0.0,
      month: Int = 30,
  ) = GvcTrade.StepInput(prev, sectorOutputs, PriceIndex(price), er, autoR, month, rng = new scala.util.Random(42))

  // ---- Initialization ----

  "GvcTrade.zero" should "have empty foreign firms" in {
    val s = GvcTrade.zero
    s.foreignFirms shouldBe empty
    td.toDouble(s.totalExports) shouldBe 0.0
    s.foreignPriceIndex shouldBe PriceIndex.Base
  }

  "GvcTrade.initial" should "create 12 foreign firms (6 sectors x 2 partners)" in {
    val s = GvcTrade.initial
    s.foreignFirms.length shouldBe 12
  }

  it should "have all sector IDs in 0-5" in {
    val s = GvcTrade.initial
    s.foreignFirms.map(_.sectorId).toSet shouldBe (0 to 5).toSet
  }

  it should "have both partner IDs (0=EU, 1=Non-EU)" in {
    val s = GvcTrade.initial
    s.foreignFirms.map(_.partnerId).toSet shouldBe Set(0, 1)
  }

  it should "have positive base export demand for all firms" in {
    val s = GvcTrade.initial
    s.foreignFirms.foreach(f => td.toDouble(f.baseExportDemand) should be > 0.0)
  }

  it should "have zero disruption initially" in {
    val s = GvcTrade.initial
    s.foreignFirms.foreach(f => td.toDouble(f.disruption) shouldBe 0.0)
  }

  it should "have trade concentration = HHI of partner shares" in {
    val s           = GvcTrade.initial
    val eu          = td.toDouble(p.gvc.euTradeShare)
    val expectedHhi = eu * eu + (1.0 - eu) * (1.0 - eu)
    td.toDouble(s.tradeConcentration) shouldBe expectedHhi +- 1e-10
  }

  // ---- Step ----

  "GvcTrade.step" should "produce positive total exports" in {
    val r = GvcTrade.step(baseInput())
    td.toDouble(r.totalExports) should be > 0.0
  }

  it should "produce positive total intermediate imports" in {
    val r = GvcTrade.step(baseInput())
    td.toDouble(r.totalIntermImports) should be > 0.0
  }

  it should "evolve foreign price index upward" in {
    val r = GvcTrade.step(baseInput())
    td.toDouble(r.foreignPriceIndex) should be > 1.0
  }

  it should "have 6-element sector exports" in {
    val r = GvcTrade.step(baseInput())
    r.sectorExports.length shouldBe 6
    r.sectorExports.foreach(e => td.toDouble(e) should be >= 0.0)
  }

  it should "have 6-element sector imports" in {
    val r = GvcTrade.step(baseInput())
    r.sectorImports.length shouldBe 6
    r.sectorImports.foreach(i => td.toDouble(i) should be >= 0.0)
  }

  it should "increase exports with higher automation" in {
    val r0 = GvcTrade.step(baseInput(autoR = 0.0))
    val r1 = GvcTrade.step(baseInput(autoR = 0.5))
    td.toDouble(r1.totalExports) should be > td.toDouble(r0.totalExports)
  }

  it should "have zero disruption when no shock applied" in {
    val r = GvcTrade.step(baseInput())
    td.toDouble(r.disruptionIndex) shouldBe 0.0
  }

  it should "have import cost index >= 1.0 (foreign inflation)" in {
    val r = GvcTrade.step(baseInput())
    td.toDouble(r.importCostIndex) should be >= 1.0
  }
