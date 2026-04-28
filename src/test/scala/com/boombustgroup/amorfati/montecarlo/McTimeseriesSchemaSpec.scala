package com.boombustgroup.amorfati.montecarlo

import com.boombustgroup.amorfati.FixedPointSpecSupport.*
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
    "MonthlyGdpProxy",
    "AnnualizedGdpProxy",
    "AutoRatio",
    "HybridRatio",
    "BPO_Auto",
    "Manuf_Auto",
    "Retail_Auto",
    "Health_Auto",
    "Public_Auto",
    "Agri_Auto",
    "BPO_Output",
    "Manuf_Output",
    "Retail_Output",
    "Health_Output",
    "Public_Output",
    "Agri_Output",
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

  private def computeRow(world: World, ledgerFinancialState: LedgerFinancialState = initState.ledgerFinancialState): Array[MetricValue] =
    McTimeseriesSchema.compute(
      executionMonth = ExecutionMonth.First,
      world = world,
      firms = init.firms,
      households = init.households,
      banks = init.banks,
      householdAggregates = init.householdAggregates,
      ledgerFinancialState = ledgerFinancialState,
    )

  private def valueAt(row: Array[MetricValue], name: String): MetricValue =
    val idx = McTimeseriesSchema.colNames.indexOf(name)
    idx.should(be >= 0)
    row(idx)

  private def polandScale(value: PLN): MetricValue =
    MetricValue.fromRaw((value / summon[SimParams].gdpRatio.toMultiplier).toLong)

  "McTimeseriesSchema" should "expose the stable schema contract" in {
    McTimeseriesSchema.nCols shouldBe 248
    McTimeseriesSchema.colNames.toVector shouldBe expectedColNames
  }

  it should "fail fast when currentSigmas does not match the sector schema" in {
    val err = intercept[IllegalArgumentException]:
      computeRow(init.world.copy(currentSigmas = init.world.currentSigmas.dropRight(1)))

    err.getMessage.should(include("world.currentSigmas"))
  }

  it should "keep sector sigma columns aligned with the schema" in {
    val updatedSigma = Sigma(17)
    val updatedWorld = init.world.copy(currentSigmas = init.world.currentSigmas.updated(1, updatedSigma))
    val updatedRow   = computeRow(updatedWorld)

    valueAt(updatedRow, "Manuf_Sigma") shouldBe MetricValue(17)
    valueAt(updatedRow, "BPO_Sigma") shouldBe valueAt(computeRow(init.world), "BPO_Sigma")
  }

  it should "emit Poland-scale GDP proxy and sector output columns" in {
    val sectorOutputs = Vector(PLN(10), PLN(20), PLN(30), PLN(40), PLN(50), PLN(60))
    val world         = init.world.copy(
      flows = init.world.flows.copy(
        monthlyGdpProxy = PLN(123),
        sectorOutputs = sectorOutputs,
      ),
    )
    val row           = computeRow(world)

    valueAt(row, "MonthlyGdpProxy") shouldBe polandScale(PLN(123))
    valueAt(row, "AnnualizedGdpProxy") shouldBe polandScale(PLN(1476))
    valueAt(row, "BPO_Output") shouldBe polandScale(PLN(10))
    valueAt(row, "Manuf_Output") shouldBe polandScale(PLN(20))
    valueAt(row, "Retail_Output") shouldBe polandScale(PLN(30))
    valueAt(row, "Health_Output") shouldBe polandScale(PLN(40))
    valueAt(row, "Public_Output") shouldBe polandScale(PLN(50))
    valueAt(row, "Agri_Output") shouldBe polandScale(PLN(60))
  }

  it should "reject malformed sector output vectors before output indexing" in {
    val err = intercept[IllegalArgumentException]:
      computeRow(init.world.copy(flows = init.world.flows.copy(sectorOutputs = Vector(PLN(1)))))

    err.getMessage.should(include("FlowState.sectorOutputs"))
  }

  it should "source ledger-owned household, public, and fund stock columns from LedgerFinancialState" in {
    val ledger = initState.ledgerFinancialState.copy(
      households = initState.ledgerFinancialState.households.map(_.copy(mortgageLoan = PLN(12), equity = PLN(11))),
      banks = initState.ledgerFinancialState.banks.map(_.copy(govBondAfs = PLN(10), govBondHtm = PLN(20))),
      government = initState.ledgerFinancialState.government.copy(govBondOutstanding = PLN(123)),
      foreign = initState.ledgerFinancialState.foreign.copy(govBondHoldings = PLN(45)),
      nbp = initState.ledgerFinancialState.nbp.copy(govBondHoldings = PLN(67)),
      funds = initState.ledgerFinancialState.funds.copy(
        jstCash = PLN(89),
        zusCash = PLN(90),
        nfzCash = PLN(91),
        ppkGovBondHoldings = PLN(92),
        fpCash = PLN(93),
        pfronCash = PLN(94),
        fgspCash = PLN(95),
        quasiFiscal = initState.ledgerFinancialState.funds.quasiFiscal.copy(
          bondsOutstanding = PLN(96),
          loanPortfolio = PLN(97),
          nbpHoldings = PLN(98),
        ),
      ),
    )
    val row    = computeRow(init.world, ledger)

    valueAt(row, "HhEquityWealth") shouldBe polandScale(PLN(11) * initState.ledgerFinancialState.households.length)
    valueAt(row, "MortgageStock") shouldBe polandScale(PLN(12) * initState.ledgerFinancialState.households.length)
    valueAt(row, "BondsOutstanding") shouldBe polandScale(PLN(123))
    valueAt(row, "BankBondHoldings") shouldBe polandScale(PLN(30) * initState.ledgerFinancialState.banks.length)
    valueAt(row, "ForeignBondHoldings") shouldBe polandScale(PLN(45))
    valueAt(row, "NbpBondHoldings") shouldBe polandScale(PLN(67))
    valueAt(row, "QfBondsOutstanding") shouldBe polandScale(PLN(96))
    valueAt(row, "QfLoanPortfolio") shouldBe polandScale(PLN(97))
    valueAt(row, "QfNbpHoldings") shouldBe polandScale(PLN(98))
    valueAt(row, "JstDeposits") shouldBe polandScale(PLN(89))
    valueAt(row, "FusBalance") shouldBe polandScale(PLN(90))
    valueAt(row, "NfzBalance") shouldBe polandScale(PLN(91))
    valueAt(row, "PpkBondHoldings") shouldBe polandScale(PLN(92))
    valueAt(row, "FpBalance") shouldBe polandScale(PLN(93))
    valueAt(row, "PfronBalance") shouldBe polandScale(PLN(94))
    valueAt(row, "FgspBalance") shouldBe polandScale(PLN(95))
  }

  it should "map regional HPI columns by market identity and preserve schema order" in {
    val regions     = init.world.real.housing.regions.getOrElse(fail("expected initialized regional housing data"))
    val hpiByMarket = Map(
      HousingConfig.RegionalMarket.Warsaw       -> BigDecimal("101.0"),
      HousingConfig.RegionalMarket.Krakow       -> BigDecimal("102.0"),
      HousingConfig.RegionalMarket.Wroclaw      -> BigDecimal("103.0"),
      HousingConfig.RegionalMarket.Gdansk       -> BigDecimal("104.0"),
      HousingConfig.RegionalMarket.Lodz         -> BigDecimal("105.0"),
      HousingConfig.RegionalMarket.Poznan       -> BigDecimal("106.0"),
      HousingConfig.RegionalMarket.RestOfPoland -> BigDecimal("107.0"),
    )
    val updated     = regions.reverse.map(region => region.copy(priceIndex = priceIndexBD(hpiByMarket(region.market))))
    val updatedRow  = computeRow(init.world.copy(real = init.world.real.copy(housing = init.world.real.housing.copy(regions = Some(updated)))))

    valueAt(updatedRow, "WawHpi") shouldBe MetricValue(101)
    valueAt(updatedRow, "KrkHpi") shouldBe MetricValue(102)
    valueAt(updatedRow, "WroHpi") shouldBe MetricValue(103)
    valueAt(updatedRow, "GdnHpi") shouldBe MetricValue(104)
    valueAt(updatedRow, "LdzHpi") shouldBe MetricValue(105)
    valueAt(updatedRow, "PozHpi") shouldBe MetricValue(106)
    valueAt(updatedRow, "RestHpi") shouldBe MetricValue(107)
  }

  it should "reject malformed regional housing state shapes before output indexing" in {
    val regions = init.world.real.housing.regions.getOrElse(fail("expected initialized regional housing data"))
    val err     = intercept[IllegalArgumentException]:
      init.world.real.housing.copy(regions = Some(regions.dropRight(1)))

    err.getMessage.should(include("HousingMarket.State.regions must contain 7 entries covering markets"))
  }
