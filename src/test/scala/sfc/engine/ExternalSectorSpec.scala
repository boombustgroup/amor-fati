package sfc.engine

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import sfc.config.{Config, RunConfig, MonetaryRegime}

class ExternalSectorSpec extends AnyFlatSpec with Matchers:

  private val plnRc = RunConfig(2000.0, 1, "test", MonetaryRegime.Pln)
  private val eurRc = RunConfig(2000.0, 1, "test", MonetaryRegime.Eur)
  private val sectorOutputs = Vector(30000.0, 160000.0, 450000.0, 60000.0, 220000.0, 80000.0)

  // ---- Initialization ----

  "ExternalSector.zero" should "have empty foreign firms" in {
    val s = ExternalSector.zero
    s.foreignFirms shouldBe empty
    s.totalExports shouldBe 0.0
    s.foreignPriceIndex shouldBe 1.0
  }

  "ExternalSector.initial" should "create 12 foreign firms (6 sectors x 2 partners)" in {
    val s = ExternalSector.initial
    s.foreignFirms.length shouldBe 12
  }

  it should "have all sector IDs in 0-5" in {
    val s = ExternalSector.initial
    s.foreignFirms.map(_.sectorId).toSet shouldBe (0 to 5).toSet
  }

  it should "have both partner IDs (0=EU, 1=Non-EU)" in {
    val s = ExternalSector.initial
    s.foreignFirms.map(_.partnerId).toSet shouldBe Set(0, 1)
  }

  it should "have positive base export demand for all firms" in {
    val s = ExternalSector.initial
    s.foreignFirms.foreach(_.baseExportDemand should be > 0.0)
  }

  it should "have zero disruption initially" in {
    val s = ExternalSector.initial
    s.foreignFirms.foreach(_.disruption shouldBe 0.0)
  }

  it should "have trade concentration = HHI of partner shares" in {
    val s = ExternalSector.initial
    val eu = Config.GvcEuTradeShare
    val expectedHhi = eu * eu + (1.0 - eu) * (1.0 - eu)
    s.tradeConcentration shouldBe expectedHhi +- 1e-10
  }

  // ---- Step ----

  "ExternalSector.step" should "produce positive total exports" in {
    val init = ExternalSector.initial
    val r = ExternalSector.step(init, sectorOutputs, 1.0, Config.BaseExRate, 0.0, 30, plnRc)
    r.totalExports should be > 0.0
  }

  it should "produce positive total intermediate imports" in {
    val init = ExternalSector.initial
    val r = ExternalSector.step(init, sectorOutputs, 1.0, Config.BaseExRate, 0.0, 30, plnRc)
    r.totalIntermImports should be > 0.0
  }

  it should "evolve foreign price index upward" in {
    val init = ExternalSector.initial
    val r = ExternalSector.step(init, sectorOutputs, 1.0, Config.BaseExRate, 0.0, 30, plnRc)
    r.foreignPriceIndex should be > 1.0
  }

  it should "have 6-element sector exports" in {
    val init = ExternalSector.initial
    val r = ExternalSector.step(init, sectorOutputs, 1.0, Config.BaseExRate, 0.0, 30, plnRc)
    r.sectorExports.length shouldBe 6
    r.sectorExports.foreach(_ should be >= 0.0)
  }

  it should "have 6-element sector imports" in {
    val init = ExternalSector.initial
    val r = ExternalSector.step(init, sectorOutputs, 1.0, Config.BaseExRate, 0.0, 30, plnRc)
    r.sectorImports.length shouldBe 6
    r.sectorImports.foreach(_ should be >= 0.0)
  }

  it should "increase exports with higher automation" in {
    val init = ExternalSector.initial
    val r0 = ExternalSector.step(init, sectorOutputs, 1.0, Config.BaseExRate, 0.0, 30, plnRc)
    val r1 = ExternalSector.step(init, sectorOutputs, 1.0, Config.BaseExRate, 0.5, 30, plnRc)
    r1.totalExports should be > r0.totalExports
  }

  it should "have zero disruption when no shock applied" in {
    val init = ExternalSector.initial
    val r = ExternalSector.step(init, sectorOutputs, 1.0, Config.BaseExRate, 0.0, 30, plnRc)
    r.disruptionIndex shouldBe 0.0
  }

  it should "work with EUR regime" in {
    val init = ExternalSector.initial
    val r = ExternalSector.step(init, sectorOutputs, 1.0, Config.BaseExRate, 0.0, 30, eurRc)
    r.totalExports should be > 0.0
    r.totalIntermImports should be > 0.0
  }

  it should "have import cost index >= 1.0 (foreign inflation)" in {
    val init = ExternalSector.initial
    val r = ExternalSector.step(init, sectorOutputs, 1.0, Config.BaseExRate, 0.0, 30, plnRc)
    r.importCostIndex should be >= 1.0
  }
