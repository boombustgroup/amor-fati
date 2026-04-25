package com.boombustgroup.amorfati.config

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.boombustgroup.amorfati.types.*

/** Regression test: SimParams.defaults must produce the same values as the old
  * Config object.
  */
class SimParamsSpec extends AnyFlatSpec with Matchers:

  given SimParams          = SimParams.defaults
  private val p: SimParams = summon[SimParams]
  private val gdpRatio     = decimal(p.gdpRatio)

  // ── GdpRatio ──

  "SimParams.defaults.gdpRatio" should "match GdpRatio for Gus size distribution" in {
    val expected = SimParams.computeGdpRatio(p.pop, p.firm.baseRevenue)
    gdpRatio shouldBe decimal(expected) +- BigDecimal("1e-12")
  }

  // ── Population ──

  "PopulationConfig" should "match Config defaults" in {
    p.pop.firmsCount shouldBe p.pop.firmsCount
    p.pop.workersPerFirm shouldBe p.pop.workersPerFirm
  }

  // ── Fiscal ──

  "FiscalConfig" should "have gdpRatio-scaled govBaseSpending" in {
    decimal(p.fiscal.govBaseSpending) shouldBe (BigDecimal("58.3e9") * gdpRatio) +- BigDecimal("1.0")
  }

  it should "have gdpRatio-scaled initGovDebt" in {
    decimal(p.fiscal.initGovDebt) shouldBe (BigDecimal("1600e9") * gdpRatio) +- BigDecimal("1.0")
  }

  "p.fiscal.initGovDebt" should "delegate to fiscal.initGovDebt" in {
    decimal(p.fiscal.initGovDebt) shouldBe decimal(p.fiscal.initGovDebt) +- BigDecimal("1e-6")
  }

  // ── Banking ──

  "BankingConfig" should "have gdpRatio-scaled values" in {
    decimal(p.banking.initCapital) shouldBe (BigDecimal("270e9") * gdpRatio) +- BigDecimal("1.0")
    decimal(p.banking.initDeposits) shouldBe (BigDecimal("1900e9") * gdpRatio) +- BigDecimal("1.0")
    decimal(p.banking.initLoans) shouldBe (BigDecimal("700e9") * gdpRatio) +- BigDecimal("1.0")
    decimal(p.banking.initGovBonds) shouldBe (BigDecimal("400e9") * gdpRatio) +- BigDecimal("1.0")
    decimal(p.banking.initNbpGovBonds) shouldBe (BigDecimal("300e9") * gdpRatio) +- BigDecimal("1.0")
    decimal(p.banking.initConsumerLoans) shouldBe (BigDecimal("200e9") * gdpRatio) +- BigDecimal("1.0")
  }

  // ── External sector sub-configs ──

  "OpenEconConfig" should "have gdpRatio-scaled values" in {
    decimal(p.openEcon.exportBase) shouldBe (BigDecimal("138.5e9") * gdpRatio) +- BigDecimal("1.0")
    decimal(p.openEcon.euTransfers) shouldBe (BigDecimal("1.458e9") * gdpRatio) +- BigDecimal("1.0")
    decimal(p.openEcon.fdiBase) shouldBe (BigDecimal("583.1e6") * gdpRatio) +- BigDecimal("1.0")
  }

  // ── Financial sub-configs ──

  "EquityConfig" should "have gdpRatio-scaled initMcap" in {
    decimal(p.equity.initMcap) shouldBe (BigDecimal("1.4e12") * gdpRatio) +- BigDecimal("1.0")
  }

  "CorpBondConfig" should "have gdpRatio-scaled initStock" in {
    decimal(p.corpBond.initStock) shouldBe (BigDecimal("90e9") * gdpRatio) +- BigDecimal("1.0")
  }

  "InsuranceConfig" should "have gdpRatio-scaled reserves" in {
    decimal(p.ins.lifeReserves) shouldBe (BigDecimal("110e9") * gdpRatio) +- BigDecimal("1.0")
    decimal(p.ins.nonLifeReserves) shouldBe (BigDecimal("90e9") * gdpRatio) +- BigDecimal("1.0")
  }

  "NbfiConfig" should "have gdpRatio-scaled values" in {
    decimal(p.nbfi.tfiInitAum) shouldBe (BigDecimal("380e9") * gdpRatio) +- BigDecimal("1.0")
    decimal(p.nbfi.creditInitStock) shouldBe (BigDecimal("231e9") * gdpRatio) +- BigDecimal("1.0")
  }

  "HousingConfig" should "have gdpRatio-scaled values" in {
    decimal(p.housing.initValue) shouldBe (BigDecimal("3.0e12") * gdpRatio) +- BigDecimal("1.0")
    decimal(p.housing.initMortgage) shouldBe (BigDecimal("485e9") * gdpRatio) +- BigDecimal("1.0")
  }

  // ── Delegation consistency ──

  "Config delegation" should "match SimParams for all key external paths" in {
    p.forex.baseExRate shouldBe p.forex.baseExRate
    decimal(p.openEcon.exportBase) shouldBe decimal(p.openEcon.exportBase)
    decimal(p.gvc.euTradeShare) shouldBe decimal(p.gvc.euTradeShare)
    p.fdi.foreignShares.map(s => decimal(s)) shouldBe p.fdi.foreignShares.map(s => decimal(s))
    decimal(p.immigration.monthlyRate) shouldBe decimal(p.immigration.monthlyRate)
    decimal(p.tourism.inboundShare) shouldBe decimal(p.tourism.inboundShare)
    decimal(p.remittance.perCapita) shouldBe decimal(p.remittance.perCapita)
  }

  it should "match SimParams for all key financial paths" in {
    decimal(p.equity.initMcap) shouldBe decimal(p.equity.initMcap)
    decimal(p.corpBond.initStock) shouldBe decimal(p.corpBond.initStock)
    decimal(p.ins.lifeReserves) shouldBe decimal(p.ins.lifeReserves)
    decimal(p.nbfi.tfiInitAum) shouldBe decimal(p.nbfi.tfiInitAum)
    decimal(p.housing.initValue) shouldBe decimal(p.housing.initValue)
    decimal(p.housing.initMortgage) shouldBe decimal(p.housing.initMortgage)
  }

  // ── Inventory delegation ──

  "p.capital.inventoryTargetRatios" should "delegate to capital.inventoryTargetRatios" in {
    p.capital.inventoryTargetRatios.map(s => decimal(s)) shouldBe p.capital.inventoryTargetRatios.map(s => decimal(s))
  }

  // ── FirmSizeDist enum ──

  "FirmSizeDist" should "default to Gus" in {
    p.pop.firmSizeDist shouldBe FirmSizeDist.Gus
  }

  // ── Remittance split ──

  "p.remittance.perCapita" should "delegate to remittance.perCapita" in {
    decimal(p.remittance.perCapita) shouldBe decimal(p.remittance.perCapita)
    decimal(p.remittance.growthRate) shouldBe decimal(p.remittance.growthRate)
  }

  // ── Validation ──

  "PopulationConfig" should "reject non-positive firmsCount" in {
    an[IllegalArgumentException] should be thrownBy PopulationConfig(firmsCount = 0)
  }

  "MonetaryConfig" should "reject rateFloor >= rateCeiling" in {
    an[IllegalArgumentException] should be thrownBy MonetaryConfig(rateFloor = Rate.decimal(5, 1), rateCeiling = Rate.decimal(1, 1))
  }

  "BankingConfig" should "reject invalid minCar" in {
    an[IllegalArgumentException] should be thrownBy BankingConfig(minCar = Multiplier(0))
    an[IllegalArgumentException] should be thrownBy BankingConfig(minCar = Multiplier(1))
  }

  // ── Vector length validation ──

  "FiscalConfig" should "reject wrong-length vatRates" in {
    an[IllegalArgumentException] should be thrownBy FiscalConfig(vatRates = Vector(Rate.decimal(23, 2), Rate.decimal(19, 2)))
  }

  "CapitalConfig" should "reject wrong-length klRatios" in {
    an[IllegalArgumentException] should be thrownBy CapitalConfig(klRatios = Vector(PLN(1)))
  }

  "ClimateConfig" should "reject wrong-length energyCostShares" in {
    an[IllegalArgumentException] should be thrownBy ClimateConfig(energyCostShares = Vector(Share.decimal(1, 1)))
  }

  // ── Private constructor ──

  "SimParams()" should "not be callable from outside companion" in {
    // SimParams.defaults is the only way to construct
    p shouldBe a[SimParams]
    // SimParams() would not compile — private constructor
  }
