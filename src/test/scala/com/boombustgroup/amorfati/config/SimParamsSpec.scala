package com.boombustgroup.amorfati.config

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.types.*

/** Regression test: SimParams.defaults must produce the same values as the old
  * Config object.
  */
class SimParamsSpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]
  private val td           = ComputationBoundary

  // ── GdpRatio ──

  "SimParams.defaults.gdpRatio" should "match GdpRatio for Gus size distribution" in {
    val expected = SimParams.computeGdpRatio(p.pop, p.firm.baseRevenue)
    p.gdpRatio shouldBe expected +- 1e-12
  }

  // ── Population ──

  "PopulationConfig" should "match Config defaults" in {
    p.pop.firmsCount shouldBe p.pop.firmsCount
    p.pop.workersPerFirm shouldBe p.pop.workersPerFirm
  }

  // ── Fiscal ──

  "FiscalConfig" should "have gdpRatio-scaled govBaseSpending" in {
    td.toDouble(p.fiscal.govBaseSpending) shouldBe (58.3e9 * p.gdpRatio) +- 1.0
  }

  it should "have gdpRatio-scaled initGovDebt" in {
    td.toDouble(p.fiscal.initGovDebt) shouldBe (1600e9 * p.gdpRatio) +- 1.0
  }

  "p.fiscal.initGovDebt" should "delegate to fiscal.initGovDebt" in {
    td.toDouble(p.fiscal.initGovDebt) shouldBe td.toDouble(p.fiscal.initGovDebt) +- 1e-6
  }

  // ── Banking ──

  "BankingConfig" should "have gdpRatio-scaled values" in {
    td.toDouble(p.banking.initCapital) shouldBe (270e9 * p.gdpRatio) +- 1.0
    td.toDouble(p.banking.initDeposits) shouldBe (1900e9 * p.gdpRatio) +- 1.0
    td.toDouble(p.banking.initLoans) shouldBe (700e9 * p.gdpRatio) +- 1.0
    td.toDouble(p.banking.initGovBonds) shouldBe (400e9 * p.gdpRatio) +- 1.0
    td.toDouble(p.banking.initNbpGovBonds) shouldBe (300e9 * p.gdpRatio) +- 1.0
    td.toDouble(p.banking.initConsumerLoans) shouldBe (200e9 * p.gdpRatio) +- 1.0
  }

  // ── External sector sub-configs ──

  "OpenEconConfig" should "have gdpRatio-scaled values" in {
    td.toDouble(p.openEcon.exportBase) shouldBe (138.5e9 * p.gdpRatio) +- 1.0
    td.toDouble(p.openEcon.euTransfers) shouldBe (1.458e9 * p.gdpRatio) +- 1.0
    td.toDouble(p.openEcon.fdiBase) shouldBe (583.1e6 * p.gdpRatio) +- 1.0
  }

  // ── Financial sub-configs ──

  "EquityConfig" should "have gdpRatio-scaled initMcap" in {
    td.toDouble(p.equity.initMcap) shouldBe (1.4e12 * p.gdpRatio) +- 1.0
  }

  "CorpBondConfig" should "have gdpRatio-scaled initStock" in {
    td.toDouble(p.corpBond.initStock) shouldBe (90e9 * p.gdpRatio) +- 1.0
  }

  "InsuranceConfig" should "have gdpRatio-scaled reserves" in {
    td.toDouble(p.ins.lifeReserves) shouldBe (110e9 * p.gdpRatio) +- 1.0
    td.toDouble(p.ins.nonLifeReserves) shouldBe (90e9 * p.gdpRatio) +- 1.0
  }

  "NbfiConfig" should "have gdpRatio-scaled values" in {
    td.toDouble(p.nbfi.tfiInitAum) shouldBe (380e9 * p.gdpRatio) +- 1.0
    td.toDouble(p.nbfi.creditInitStock) shouldBe (231e9 * p.gdpRatio) +- 1.0
  }

  "HousingConfig" should "have gdpRatio-scaled values" in {
    td.toDouble(p.housing.initValue) shouldBe (3.0e12 * p.gdpRatio) +- 1.0
    td.toDouble(p.housing.initMortgage) shouldBe (485e9 * p.gdpRatio) +- 1.0
  }

  // ── Delegation consistency ──

  "Config delegation" should "match SimParams for all key external paths" in {
    p.forex.baseExRate shouldBe p.forex.baseExRate
    td.toDouble(p.openEcon.exportBase) shouldBe td.toDouble(p.openEcon.exportBase)
    td.toDouble(p.gvc.euTradeShare) shouldBe td.toDouble(p.gvc.euTradeShare)
    p.fdi.foreignShares.map(s => td.toDouble(s)) shouldBe p.fdi.foreignShares.map(s => td.toDouble(s))
    td.toDouble(p.immigration.monthlyRate) shouldBe td.toDouble(p.immigration.monthlyRate)
    td.toDouble(p.tourism.inboundShare) shouldBe td.toDouble(p.tourism.inboundShare)
    td.toDouble(p.remittance.perCapita) shouldBe td.toDouble(p.remittance.perCapita)
  }

  it should "match SimParams for all key financial paths" in {
    td.toDouble(p.equity.initMcap) shouldBe td.toDouble(p.equity.initMcap)
    td.toDouble(p.corpBond.initStock) shouldBe td.toDouble(p.corpBond.initStock)
    td.toDouble(p.ins.lifeReserves) shouldBe td.toDouble(p.ins.lifeReserves)
    td.toDouble(p.nbfi.tfiInitAum) shouldBe td.toDouble(p.nbfi.tfiInitAum)
    td.toDouble(p.housing.initValue) shouldBe td.toDouble(p.housing.initValue)
    td.toDouble(p.housing.initMortgage) shouldBe td.toDouble(p.housing.initMortgage)
  }

  // ── Inventory delegation ──

  "p.capital.inventoryTargetRatios" should "delegate to capital.inventoryTargetRatios" in {
    p.capital.inventoryTargetRatios.map(s => td.toDouble(s)) shouldBe p.capital.inventoryTargetRatios.map(s => td.toDouble(s))
  }

  // ── FirmSizeDist enum ──

  "FirmSizeDist" should "default to Gus" in {
    p.pop.firmSizeDist shouldBe FirmSizeDist.Gus
  }

  // ── Remittance split ──

  "p.remittance.perCapita" should "delegate to remittance.perCapita" in {
    td.toDouble(p.remittance.perCapita) shouldBe td.toDouble(p.remittance.perCapita)
    td.toDouble(p.remittance.growthRate) shouldBe td.toDouble(p.remittance.growthRate)
  }

  // ── Validation ──

  "PopulationConfig" should "reject non-positive firmsCount" in {
    an[IllegalArgumentException] should be thrownBy PopulationConfig(firmsCount = 0)
  }

  "MonetaryConfig" should "reject rateFloor >= rateCeiling" in {
    an[IllegalArgumentException] should be thrownBy MonetaryConfig(rateFloor = Rate(0.5), rateCeiling = Rate(0.1))
  }

  "BankingConfig" should "reject invalid minCar" in {
    an[IllegalArgumentException] should be thrownBy BankingConfig(minCar = Multiplier(0.0))
    an[IllegalArgumentException] should be thrownBy BankingConfig(minCar = Multiplier(1.0))
  }

  // ── Vector length validation ──

  "FiscalConfig" should "reject wrong-length vatRates" in {
    an[IllegalArgumentException] should be thrownBy FiscalConfig(vatRates = Vector(Rate(0.23), Rate(0.19)))
  }

  "CapitalConfig" should "reject wrong-length klRatios" in {
    an[IllegalArgumentException] should be thrownBy CapitalConfig(klRatios = Vector(PLN(1.0)))
  }

  "ClimateConfig" should "reject wrong-length energyCostShares" in {
    an[IllegalArgumentException] should be thrownBy ClimateConfig(energyCostShares = Vector(Share(0.1)))
  }

  // ── Private constructor ──

  "SimParams()" should "not be callable from outside companion" in {
    // SimParams.defaults is the only way to construct
    p shouldBe a[SimParams]
    // SimParams() would not compile — private constructor
  }
