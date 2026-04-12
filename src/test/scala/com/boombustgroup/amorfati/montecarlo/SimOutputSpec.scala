package com.boombustgroup.amorfati.montecarlo

import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SimOutputSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  private lazy val init = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))

  private val expectedColNames = Vector(
    "Month",
    "Inflation",
    "Unemployment",
    "PermanentShare",
    "ZlecenieShare",
    "B2BShare",
    "TotalAdoption",
    "ExRate",
    "MarketWage",
    "GovDebt",
    "NPL",
    "RefRate",
    "PriceLevel",
    "AutoRatio",
    "HybridRatio",
    "BPO_Auto",
    "Manuf_Auto",
    "Retail_Auto",
    "Health_Auto",
    "Public_Auto",
    "Agri_Auto",
    "BPO_Sigma",
    "Manuf_Sigma",
    "Retail_Sigma",
    "Health_Sigma",
    "Public_Sigma",
    "Agri_Sigma",
    "MeanDegree",
    "IoFlows",
    "IoGdpRatio",
    "NFA",
    "CurrentAccount",
    "CapitalAccount",
    "TradeBalance_OE",
    "Exports_OE",
    "TotalImports_OE",
    "ImportedInterm",
    "FDI",
    "GvcDisruptionIndex",
    "ForeignPriceIndex",
    "GvcTradeConcentration",
    "GvcExportDemandShock",
    "GvcImportCostIndex",
    "CommodityPriceIndex",
    "ImmigrantStock",
    "MonthlyImmigInflow",
    "RemittanceOutflow",
    "ImmigrantUnempRate",
    "FxReserves",
    "FxInterventionAmt",
    "FxInterventionActive",
    "DiasporaRemittanceInflow",
    "NetRemittances",
    "TourismExport",
    "TourismImport",
    "NetTourismBalance",
    "TourismSeasonalFactor",
    "UnempBenefitSpend",
    "OutputGap",
    "EffectivePitRate",
    "SocialTransferSpend",
    "GovCurrentSpend",
    "GovCapitalSpendDomestic",
    "GovDomesticBudgetDemand",
    "GovDomesticBudgetOutlays",
    "EuProjectCapitalTotal",
    "PublicCapitalStock",
    "EuCofinancingDomestic",
    "EuFundsMonthly",
    "EuCumulativeAbsorption",
    "MinWageLevel",
    "ExciseRevenue",
    "CustomsDutyRevenue",
    "DebtToGdp",
    "DeficitToGdp",
    "FiscalRuleBinding",
    "GovSpendingCutRatio",
    "BondYield",
    "WeightedCoupon",
    "BondsOutstanding",
    "BankBondHoldings",
    "ForeignBondHoldings",
    "NbpBondHoldings",
    "QeActive",
    "DebtService",
    "NbpRemittance",
    "ReserveInterest",
    "StandingFacilityNet",
    "DepositFacilityUsage",
    "InterbankInterestNet",
    "M0",
    "M1",
    "M2",
    "M3",
    "CreditMultiplier",
    "FofResidual",
    "InterbankRate",
    "MinBankCAR",
    "MaxBankNPL",
    "BankFailures",
    "MinBankLCR",
    "MinBankNSFR",
    "AvgTermDepositFrac",
    "WIBOR_1M",
    "WIBOR_3M",
    "WIBOR_6M",
    "ConsumerLoans",
    "ConsumerNplRatio",
    "ConsumerOrigination",
    "ConsumerDebtService",
    "GpwIndex",
    "GpwMarketCap",
    "GpwPE",
    "GpwDivYield",
    "EquityIssuanceTotal",
    "EquityFinancedFrac",
    "HhEquityWealth",
    "EquityWealthEffect",
    "DomesticDividends",
    "ForeignDividendOutflow",
    "CorpBondOutstanding",
    "CorpBondYield",
    "CorpBondIssuance",
    "CorpBondSpread",
    "BankCorpBondHoldings",
    "PpkCorpBondHoldings",
    "CorpBondAbsorptionRate",
    "InsLifeReserves",
    "InsNonLifeReserves",
    "InsGovBondHoldings",
    "InsLifePremium",
    "InsNonLifePremium",
    "InsLifeClaims",
    "InsNonLifeClaims",
    "NbfiTfiAum",
    "NbfiTfiGovBondHoldings",
    "NbfiLoanStock",
    "NbfiOrigination",
    "NbfiDefaults",
    "NbfiBankTightness",
    "QfBondsOutstanding",
    "QfNbpHoldings",
    "QfLoanPortfolio",
    "QfIssuance",
    "Esa2010DebtToGdp",
    "NbfiDepositDrain",
    "BankAfsBonds",
    "BankHtmBonds",
    "EclStage1",
    "EclStage2",
    "EclStage3",
    "BfgLevyTotal",
    "BfgFundBalance",
    "BailInLoss",
    "HousingPriceIndex",
    "HousingMarketValue",
    "MortgageStock",
    "AvgMortgageRate",
    "MortgageOrigination",
    "MortgageRepayment",
    "MortgageDefault",
    "MortgageInterestIncome",
    "HhHousingWealth",
    "HousingWealthEffect",
    "MortgageToGdp",
    "WawHpi",
    "KrkHpi",
    "WroHpi",
    "GdnHpi",
    "LdzHpi",
    "PozHpi",
    "RestHpi",
    "SectorMobilityRate",
    "CrossSectorHires",
    "VoluntaryQuits",
    "AggCapitalStock",
    "GrossInvestment",
    "CapitalDepreciation",
    "AggInventoryStock",
    "InventoryChange",
    "InventoryToGdp",
    "AggEnergyCost",
    "EnergyCostToGdp",
    "EtsPrice",
    "AggGreenCapital",
    "GreenInvestment",
    "GreenCapitalRatio",
    "JstRevenue",
    "JstSpending",
    "JstDebt",
    "JstDeposits",
    "JstDeficit",
    "ZusContributions",
    "ZusPensionPayments",
    "ZusGovSubvention",
    "FusBalance",
    "NfzContributions",
    "NfzSpending",
    "NfzBalance",
    "NfzGovSubvention",
    "PpkContributions",
    "PpkBondHoldings",
    "NRetirees",
    "WorkingAgePop",
    "MonthlyRetirements",
    "FpBalance",
    "FpContributions",
    "PfronBalance",
    "FgspBalance",
    "FgspSpending",
    "EarmarkedGovSubvention",
    "ExpectedInflation",
    "NbpCredibility",
    "ForwardGuidanceRate",
    "InflationForecastError",
    "CCyB",
    "CreditToGdpGap",
    "EffectiveMinCar",
    "FdiProfitShifting",
    "FdiRepatriation",
    "FdiGrossOutflow",
    "ForeignOwnedFrac",
    "FdiCitLoss",
    "FirmBirths",
    "FirmDeaths",
    "NetEntry",
    "LivingFirmCount",
    "NetFirmBirths",
    "TotalFirmCount",
    "RealizedTaxShadowShare",
    "NextTaxShadowShare",
    "TaxEvasionLoss",
    "EvasionToGdpRatio",
    "Unemp_Central",
    "Unemp_South",
    "Unemp_East",
    "Unemp_Northwest",
    "Unemp_Southwest",
    "Unemp_North",
  )

  private def computeRow(world: World): Array[Double] =
    SimOutput.compute(
      executionMonth = ExecutionMonth.First,
      world = world,
      firms = init.firms,
      households = init.households,
      banks = init.banks,
      householdAggregates = init.householdAggregates,
    )

  private def valueAt(row: Array[Double], name: String): Double =
    val idx = SimOutput.colNames.indexOf(name)
    idx.should(be >= 0)
    row(idx)

  "SimOutput" should "expose the stable schema contract" in {
    SimOutput.nCols shouldBe 239
    SimOutput.colNames.toVector shouldBe expectedColNames
  }

  it should "fail fast when currentSigmas does not match the sector schema" in {
    val err = intercept[IllegalArgumentException]:
      computeRow(init.world.copy(currentSigmas = init.world.currentSigmas.dropRight(1)))

    err.getMessage.should(include("world.currentSigmas"))
  }

  it should "map regional HPI columns using the documented Lodz-before-Poznan order" in {
    val regions    = init.world.real.housing.regions.getOrElse(fail("expected initialized regional housing data"))
    val updated    = regions.zipWithIndex.map: (region, idx) =>
      region.copy(priceIndex = PriceIndex(101.0 + idx.toDouble))
    val updatedRow = computeRow(init.world.copy(real = init.world.real.copy(housing = init.world.real.housing.copy(regions = Some(updated)))))

    valueAt(updatedRow, "WawHpi") shouldBe 101.0
    valueAt(updatedRow, "KrkHpi") shouldBe 102.0
    valueAt(updatedRow, "WroHpi") shouldBe 103.0
    valueAt(updatedRow, "GdnHpi") shouldBe 104.0
    valueAt(updatedRow, "LdzHpi") shouldBe 105.0
    valueAt(updatedRow, "PozHpi") shouldBe 106.0
    valueAt(updatedRow, "RestHpi") shouldBe 107.0
  }

  it should "reject malformed regional housing state shapes before output indexing" in {
    val regions = init.world.real.housing.regions.getOrElse(fail("expected initialized regional housing data"))
    val err     = intercept[IllegalArgumentException]:
      init.world.real.housing.copy(regions = Some(regions.dropRight(1)))

    err.getMessage.should(include("HousingMarket.State.regions must have 7 entries"))
  }
