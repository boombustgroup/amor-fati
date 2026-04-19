package com.boombustgroup.amorfati.montecarlo

import com.boombustgroup.amorfati.config.{HousingConfig, SimParams}
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.World
import com.boombustgroup.amorfati.engine.flows.FlowSimulation
import com.boombustgroup.amorfati.engine.ledger.LedgerFinancialState
import com.boombustgroup.amorfati.init.{InitRandomness, WorldInit}
import com.boombustgroup.amorfati.types.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class McTimeseriesSchemaSpec extends AnyFlatSpec with Matchers:

  given SimParams = SimParams.defaults

  private lazy val init      = WorldInit.initialize(InitRandomness.Contract.fromSeed(42L))
  private lazy val initState = FlowSimulation.SimState.fromInit(init)

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
    "GovernmentDividends",
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

  private def computeRow(world: World, ledgerFinancialState: LedgerFinancialState = initState.ledgerFinancialState): Array[Double] =
    McTimeseriesSchema.compute(
      executionMonth = ExecutionMonth.First,
      world = world,
      firms = init.firms,
      households = init.households,
      banks = init.banks,
      householdAggregates = init.householdAggregates,
      ledgerFinancialState = ledgerFinancialState,
    )

  private def valueAt(row: Array[Double], name: String): Double =
    val idx = McTimeseriesSchema.colNames.indexOf(name)
    idx.should(be >= 0)
    row(idx)

  "McTimeseriesSchema" should "expose the stable schema contract" in {
    McTimeseriesSchema.nCols shouldBe 240
    McTimeseriesSchema.colNames.toVector shouldBe expectedColNames
  }

  it should "fail fast when currentSigmas does not match the sector schema" in {
    val err = intercept[IllegalArgumentException]:
      computeRow(init.world.copy(currentSigmas = init.world.currentSigmas.dropRight(1)))

    err.getMessage.should(include("world.currentSigmas"))
  }

  it should "keep sector sigma columns aligned with the schema" in {
    val updatedSigma = Sigma(17.0)
    val updatedWorld = init.world.copy(currentSigmas = init.world.currentSigmas.updated(1, updatedSigma))
    val updatedRow   = computeRow(updatedWorld)

    valueAt(updatedRow, "Manuf_Sigma") shouldBe 17.0
    valueAt(updatedRow, "BPO_Sigma") shouldBe valueAt(computeRow(init.world), "BPO_Sigma")
  }

  it should "source ledger-owned household, public, and fund stock columns from LedgerFinancialState" in {
    val ledger = initState.ledgerFinancialState.copy(
      households = initState.ledgerFinancialState.households.map(_.copy(mortgageLoan = PLN(12.0), equity = PLN(11.0))),
      banks = initState.ledgerFinancialState.banks.map(_.copy(govBondAfs = PLN(10.0), govBondHtm = PLN(20.0))),
      government = initState.ledgerFinancialState.government.copy(govBondOutstanding = PLN(123.0)),
      foreign = initState.ledgerFinancialState.foreign.copy(govBondHoldings = PLN(45.0)),
      nbp = initState.ledgerFinancialState.nbp.copy(govBondHoldings = PLN(67.0)),
      funds = initState.ledgerFinancialState.funds.copy(
        jstCash = PLN(89.0),
        zusCash = PLN(90.0),
        nfzCash = PLN(91.0),
        ppkGovBondHoldings = PLN(92.0),
        fpCash = PLN(93.0),
        pfronCash = PLN(94.0),
        fgspCash = PLN(95.0),
        quasiFiscal = initState.ledgerFinancialState.funds.quasiFiscal.copy(
          bondsOutstanding = PLN(96.0),
          loanPortfolio = PLN(97.0),
        ),
      ),
    )
    val row    = computeRow(init.world, ledger)

    valueAt(row, "HhEquityWealth") shouldBe initState.ledgerFinancialState.households.length.toDouble * 11.0
    valueAt(row, "MortgageStock") shouldBe initState.ledgerFinancialState.households.length.toDouble * 12.0
    valueAt(row, "BondsOutstanding") shouldBe 123.0
    valueAt(row, "BankBondHoldings") shouldBe initState.ledgerFinancialState.banks.length.toDouble * 30.0
    valueAt(row, "ForeignBondHoldings") shouldBe 45.0
    valueAt(row, "NbpBondHoldings") shouldBe 67.0
    valueAt(row, "QfBondsOutstanding") shouldBe 96.0
    valueAt(row, "QfLoanPortfolio") shouldBe 97.0
    valueAt(row, "JstDeposits") shouldBe 89.0
    valueAt(row, "FusBalance") shouldBe 90.0
    valueAt(row, "NfzBalance") shouldBe 91.0
    valueAt(row, "PpkBondHoldings") shouldBe 92.0
    valueAt(row, "FpBalance") shouldBe 93.0
    valueAt(row, "PfronBalance") shouldBe 94.0
    valueAt(row, "FgspBalance") shouldBe 95.0
  }

  it should "map regional HPI columns by market identity and preserve schema order" in {
    val regions     = init.world.real.housing.regions.getOrElse(fail("expected initialized regional housing data"))
    val hpiByMarket = Map(
      HousingConfig.RegionalMarket.Warsaw       -> 101.0,
      HousingConfig.RegionalMarket.Krakow       -> 102.0,
      HousingConfig.RegionalMarket.Wroclaw      -> 103.0,
      HousingConfig.RegionalMarket.Gdansk       -> 104.0,
      HousingConfig.RegionalMarket.Lodz         -> 105.0,
      HousingConfig.RegionalMarket.Poznan       -> 106.0,
      HousingConfig.RegionalMarket.RestOfPoland -> 107.0,
    )
    val updated     = regions.reverse.map(region => region.copy(priceIndex = PriceIndex(hpiByMarket(region.market))))
    val updatedRow  = computeRow(init.world.copy(real = init.world.real.copy(housing = init.world.real.housing.copy(regions = Some(updated)))))

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

    err.getMessage.should(include("HousingMarket.State.regions must contain 7 entries covering markets"))
  }
