package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.McRunConfig
import sfc.config.SimParams
import sfc.engine.markets.GvcTrade
import sfc.types.*

class ExternalSectorSpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]

  private val rc            = McRunConfig(1, "test")
  private val sectorOutputs = Vector(30000.0, 160000.0, 450000.0, 60000.0, 220000.0, 80000.0)

  // ---- Initialization ----

  "GvcTrade.zero" should "have empty foreign firms" in {
    val s = GvcTrade.zero
    s.foreignFirms shouldBe empty
    s.totalExports.toDouble shouldBe 0.0
    s.foreignPriceIndex shouldBe 1.0
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
    s.foreignFirms.foreach(_.baseExportDemand.toDouble should be > 0.0)
  }

  it should "have zero disruption initially" in {
    val s = GvcTrade.initial
    s.foreignFirms.foreach(_.disruption.toDouble shouldBe 0.0)
  }

  it should "have trade concentration = HHI of partner shares" in {
    val s           = GvcTrade.initial
    val eu          = p.gvc.euTradeShare.toDouble
    val expectedHhi = eu * eu + (1.0 - eu) * (1.0 - eu)
    s.tradeConcentration.toDouble shouldBe expectedHhi +- 1e-10
  }

  // ---- Step ----

  "GvcTrade.step" should "produce positive total exports" in {
    val init = GvcTrade.initial
    val r    = GvcTrade.step(init, sectorOutputs, 1.0, p.forex.baseExRate, 0.0, 30, rc)
    r.totalExports.toDouble should be > 0.0
  }

  it should "produce positive total intermediate imports" in {
    val init = GvcTrade.initial
    val r    = GvcTrade.step(init, sectorOutputs, 1.0, p.forex.baseExRate, 0.0, 30, rc)
    r.totalIntermImports.toDouble should be > 0.0
  }

  it should "evolve foreign price index upward" in {
    val init = GvcTrade.initial
    val r    = GvcTrade.step(init, sectorOutputs, 1.0, p.forex.baseExRate, 0.0, 30, rc)
    r.foreignPriceIndex should be > 1.0
  }

  it should "have 6-element sector exports" in {
    val init = GvcTrade.initial
    val r    = GvcTrade.step(init, sectorOutputs, 1.0, p.forex.baseExRate, 0.0, 30, rc)
    r.sectorExports.length shouldBe 6
    r.sectorExports.map(_.toDouble).foreach(_ should be >= 0.0)
  }

  it should "have 6-element sector imports" in {
    val init = GvcTrade.initial
    val r    = GvcTrade.step(init, sectorOutputs, 1.0, p.forex.baseExRate, 0.0, 30, rc)
    r.sectorImports.length shouldBe 6
    r.sectorImports.map(_.toDouble).foreach(_ should be >= 0.0)
  }

  it should "increase exports with higher automation" in {
    val init = GvcTrade.initial
    val r0   = GvcTrade.step(init, sectorOutputs, 1.0, p.forex.baseExRate, 0.0, 30, rc)
    val r1   = GvcTrade.step(init, sectorOutputs, 1.0, p.forex.baseExRate, 0.5, 30, rc)
    r1.totalExports.toDouble should be > r0.totalExports.toDouble
  }

  it should "have zero disruption when no shock applied" in {
    val init = GvcTrade.initial
    val r    = GvcTrade.step(init, sectorOutputs, 1.0, p.forex.baseExRate, 0.0, 30, rc)
    r.disruptionIndex.toDouble shouldBe 0.0
  }

  it should "have import cost index >= 1.0 (foreign inflation)" in {
    val init = GvcTrade.initial
    val r    = GvcTrade.step(init, sectorOutputs, 1.0, p.forex.baseExRate, 0.0, 30, rc)
    r.importCostIndex should be >= 1.0
  }
