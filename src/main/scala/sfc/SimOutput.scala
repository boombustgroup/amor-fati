package sfc

import sfc.agents.*
import sfc.config.{Config, SectorDefs}
import sfc.engine.*
import sfc.engine.mechanisms.Macroprudential
import sfc.types.*
import sfc.util.KahanSum.*

/** Typed column schema for simulation output.
  *
  * Each column is defined exactly once as a `(name, compute)` pair. Column handles are opaque — raw ints never appear
  * at call sites. Adding a column = adding one ColumnDef; names, indices, CSV headers, and `toArray` all follow
  * automatically.
  */
object SimOutput:

  /** A typed column handle. Wraps an ordinal — prevents raw Int column access. */
  opaque type Col = Int
  object Col:
    private var _next = 0
    private def auto(): Col = { val c = _next; _next += 1; c }

    // Named handles for columns referenced in tests/Main
    val Month: Col = auto() // 0
    val Inflation: Col = auto() // 1
    val Unemployment: Col = auto() // 2
    val TotalAdoption: Col = auto() // 3
    val ExRate: Col = auto() // 4
    val MarketWage: Col = auto() // 5
    val GovDebt: Col = auto() // 6
    val NPL: Col = auto() // 7
    val RefRate: Col = auto() // 8
    val PriceLevel: Col = auto() // 9
    val AutoRatio: Col = auto() // 10
    val HybridRatio: Col = auto() // 11
    // 12–17: sector auto ratios
    val BpoAuto: Col = auto()
    val ManufAuto: Col = auto()
    val RetailAuto: Col = auto()
    val HealthAuto: Col = auto()
    val PublicAuto: Col = auto()
    val AgriAuto: Col = auto()
    val EffectiveBDP: Col = auto() // 18
    // 19–24: sector sigmas
    val BpoSigma: Col = auto()
    val ManufSigma: Col = auto()
    val RetailSigma: Col = auto()
    val HealthSigma: Col = auto()
    val PublicSigma: Col = auto()
    val AgriSigma: Col = auto()
    val MeanDegree: Col = auto() // 25
    val IoFlows: Col = auto() // 26
    val IoGdpRatio: Col = auto() // 27
    val NFA: Col = auto() // 28
    val CurrentAccount: Col = auto() // 29
    val CapitalAccount: Col = auto() // 30
    val TradeBalance: Col = auto() // 31
    val Exports: Col = auto() // 32
    val TotalImports: Col = auto() // 33
    val ImportedInterm: Col = auto() // 34
    val FDI: Col = auto() // 35
    val UnempBenefitSpend: Col = auto() // 36
    val OutputGap: Col = auto() // 37
    val BondYield: Col = auto() // 38
    val BondsOutstanding: Col = auto() // 39
    val BankBondHoldings: Col = auto() // 40
    val NbpBondHoldings: Col = auto() // 41
    val QeActive: Col = auto() // 42
    val DebtService: Col = auto() // 43
    val NbpRemittance: Col = auto() // 44
    val FxReserves: Col = auto() // 45
    val FxInterventionAmt: Col = auto() // 46
    val FxInterventionActive: Col = auto() // 47
    val InterbankRate: Col = auto() // 48
    val MinBankCAR: Col = auto() // 49
    val MaxBankNPL: Col = auto() // 50
    val BankFailures: Col = auto() // 51
    val ReserveInterest: Col = auto() // 52
    val StandingFacilityNet: Col = auto() // 53
    val DepositFacilityUsage: Col = auto() // 54
    val InterbankInterestNet: Col = auto() // 55

    def sectorAuto(s: Int): Col = 12 + s
    def sectorSigma(s: Int): Col = 19 + s

  extension (c: Col) def ordinal: Int = c

  /** Column definition: name paired with its computation. */
  private case class ColumnDef(name: String, compute: Ctx => Double)

  /** Shared pre-computed context (computed once per timestep). */
  private class Ctx(
    val t: Int,
    val world: World,
    val firms: Vector[Firm.State],
    val households: Vector[Household.State],
    val living: Vector[Firm.State],
    val nLiving: Double,
    val aliveBanks: Vector[Banking.BankState],
  ):
    lazy val sectorAuto: IndexedSeq[Double] = SectorDefs.indices.map { s =>
      val secFirms = living.filter(_.sector.toInt == s)
      if secFirms.isEmpty then 0.0
      else
        secFirms
          .count(f => f.tech.isInstanceOf[TechState.Automated] || f.tech.isInstanceOf[TechState.Hybrid])
          .toDouble / secFirms.length
    }

    inline def unemployPct: Double = world.hh.unemploymentRate
    inline def effectiveBdp: Double = world.gov.effectiveBdpPerCapita(Config.TotalPopulation)

  /** The schema: ordered sequence of (name, computation) pairs. SINGLE SOURCE OF TRUTH. */
  private val schema: IndexedSeq[ColumnDef] = IndexedSeq(
    ColumnDef("Month", ctx => (ctx.t + 1).toDouble),
    ColumnDef("Inflation", ctx => ctx.world.inflation.toDouble),
    ColumnDef("Unemployment", ctx => ctx.unemployPct),
    ColumnDef("TotalAdoption", ctx => (ctx.world.automationRatio + ctx.world.hybridRatio).toDouble),
    ColumnDef("ExRate", ctx => ctx.world.forex.exchangeRate),
    ColumnDef("MarketWage", ctx => ctx.world.hh.marketWage.toDouble),
    ColumnDef("GovDebt", ctx => ctx.world.gov.cumulativeDebt.toDouble),
    ColumnDef("NPL", ctx => ctx.world.bankingSector.aggregate.nplRatio),
    ColumnDef("RefRate", ctx => ctx.world.nbp.referenceRate.toDouble),
    ColumnDef("PriceLevel", ctx => ctx.world.priceLevel),
    ColumnDef("AutoRatio", ctx => ctx.world.automationRatio.toDouble),
    ColumnDef("HybridRatio", ctx => ctx.world.hybridRatio.toDouble),
    // 12–17: per-sector automation ratios
    ColumnDef("BPO_Auto", ctx => ctx.sectorAuto(0)),
    ColumnDef("Manuf_Auto", ctx => ctx.sectorAuto(1)),
    ColumnDef("Retail_Auto", ctx => ctx.sectorAuto(2)),
    ColumnDef("Health_Auto", ctx => ctx.sectorAuto(3)),
    ColumnDef("Public_Auto", ctx => ctx.sectorAuto(4)),
    ColumnDef("Agri_Auto", ctx => ctx.sectorAuto(5)),
    ColumnDef("EffectiveBDP", ctx => ctx.effectiveBdp),
    // 19–24: per-sector current sigma
    ColumnDef("BPO_Sigma", ctx => ctx.world.currentSigmas(0)),
    ColumnDef("Manuf_Sigma", ctx => ctx.world.currentSigmas(1)),
    ColumnDef("Retail_Sigma", ctx => ctx.world.currentSigmas(2)),
    ColumnDef("Health_Sigma", ctx => ctx.world.currentSigmas(3)),
    ColumnDef("Public_Sigma", ctx => ctx.world.currentSigmas(4)),
    ColumnDef("Agri_Sigma", ctx => ctx.world.currentSigmas(5)),
    ColumnDef("MeanDegree", ctx => ctx.firms.kahanSumBy(_.neighbors.length.toDouble) / ctx.firms.length),
    ColumnDef("IoFlows", ctx => ctx.world.ioFlows.toDouble),
    ColumnDef(
      "IoGdpRatio",
      ctx => if ctx.world.gdpProxy > 0 then (ctx.world.ioFlows / ctx.world.gdpProxy).toDouble else 0.0,
    ),
    // BoP
    ColumnDef("NFA", ctx => ctx.world.bop.nfa.toDouble),
    ColumnDef("CurrentAccount", ctx => ctx.world.bop.currentAccount.toDouble),
    ColumnDef("CapitalAccount", ctx => ctx.world.bop.capitalAccount.toDouble),
    ColumnDef("TradeBalance_OE", ctx => ctx.world.bop.tradeBalance.toDouble),
    ColumnDef("Exports_OE", ctx => ctx.world.bop.exports.toDouble),
    ColumnDef("TotalImports_OE", ctx => ctx.world.bop.totalImports.toDouble),
    ColumnDef("ImportedInterm", ctx => ctx.world.bop.importedIntermediates.toDouble),
    ColumnDef("FDI", ctx => ctx.world.bop.fdi.toDouble),
    ColumnDef("UnempBenefitSpend", ctx => ctx.world.gov.unempBenefitSpend.toDouble),
    ColumnDef("OutputGap", ctx => (ctx.unemployPct - Config.NbpNairu) / Config.NbpNairu),
    // Bond market
    ColumnDef("BondYield", ctx => ctx.world.gov.bondYield.toDouble),
    ColumnDef("BondsOutstanding", ctx => ctx.world.gov.bondsOutstanding.toDouble),
    ColumnDef("BankBondHoldings", ctx => ctx.world.bank.govBondHoldings.toDouble),
    ColumnDef("NbpBondHoldings", ctx => ctx.world.nbp.govBondHoldings.toDouble),
    ColumnDef("QeActive", ctx => if ctx.world.nbp.qeActive then 1.0 else 0.0),
    ColumnDef("DebtService", ctx => ctx.world.gov.debtServiceSpend.toDouble),
    ColumnDef("NbpRemittance", ctx => (ctx.world.nbp.govBondHoldings * ctx.world.gov.bondYield / 12.0).toDouble),
    // FX
    ColumnDef("FxReserves", ctx => ctx.world.nbp.fxReserves.toDouble),
    ColumnDef("FxInterventionAmt", ctx => ctx.world.nbp.lastFxTraded.toDouble),
    ColumnDef("FxInterventionActive", ctx => if Config.NbpFxIntervention then 1.0 else 0.0),
    // Interbank
    ColumnDef("InterbankRate", ctx => ctx.world.bankingSector.interbankRate.toDouble),
    ColumnDef("MinBankCAR", ctx => if ctx.aliveBanks.isEmpty then 0.0 else ctx.aliveBanks.map(_.car).min),
    ColumnDef("MaxBankNPL", ctx => if ctx.aliveBanks.isEmpty then 0.0 else ctx.aliveBanks.map(_.nplRatio).max),
    ColumnDef("BankFailures", ctx => ctx.world.bankingSector.banks.count(_.failed).toDouble),
    // Monetary plumbing — now read from World
    ColumnDef("ReserveInterest", ctx => ctx.world.reserveInterestTotal),
    ColumnDef("StandingFacilityNet", ctx => ctx.world.standingFacilityNet),
    ColumnDef("DepositFacilityUsage", ctx => ctx.world.depositFacilityUsage),
    ColumnDef("InterbankInterestNet", ctx => ctx.world.interbankInterestNet),
    // Credit diagnostics
    ColumnDef("M1", ctx => ctx.world.monetaryAgg.map(_.m1.toDouble).getOrElse(ctx.world.bank.deposits.toDouble)),
    ColumnDef("MonetaryBase", ctx => ctx.world.monetaryAgg.map(_.monetaryBase.toDouble).getOrElse(0.0)),
    ColumnDef("CreditMultiplier", ctx => ctx.world.monetaryAgg.map(_.creditMultiplier).getOrElse(0.0)),
    // JST
    ColumnDef("JstRevenue", ctx => ctx.world.jst.revenue.toDouble),
    ColumnDef("JstSpending", ctx => ctx.world.jst.spending.toDouble),
    ColumnDef("JstDebt", ctx => ctx.world.jst.debt.toDouble),
    ColumnDef("JstDeposits", ctx => ctx.world.jst.deposits.toDouble),
    ColumnDef("JstDeficit", ctx => ctx.world.jst.deficit.toDouble),
    // LCR/NSFR
    ColumnDef("MinBankLCR", ctx => if ctx.aliveBanks.isEmpty then 0.0 else ctx.aliveBanks.map(_.lcr).min),
    ColumnDef("MinBankNSFR", ctx => if ctx.aliveBanks.isEmpty then 0.0 else ctx.aliveBanks.map(_.nsfr).min),
    ColumnDef(
      "AvgTermDepositFrac",
      ctx =>
        if ctx.aliveBanks.isEmpty then 0.0
        else
          ctx.aliveBanks
            .map(b => if b.deposits > PLN.Zero then b.termDeposits / b.deposits else 0.0)
            .sum / ctx.aliveBanks.length,
    ),
    // Term structure
    ColumnDef("WIBOR_1M", ctx => ctx.world.bankingSector.interbankCurve.map(_.wibor1m.toDouble).getOrElse(0.0)),
    ColumnDef("WIBOR_3M", ctx => ctx.world.bankingSector.interbankCurve.map(_.wibor3m.toDouble).getOrElse(0.0)),
    ColumnDef("WIBOR_6M", ctx => ctx.world.bankingSector.interbankCurve.map(_.wibor6m.toDouble).getOrElse(0.0)),
    // Full public sector
    ColumnDef("ZusContributions", ctx => ctx.world.zus.contributions.toDouble),
    ColumnDef("ZusPensionPayments", ctx => ctx.world.zus.pensionPayments.toDouble),
    ColumnDef("ZusGovSubvention", ctx => ctx.world.zus.govSubvention.toDouble),
    ColumnDef("FusBalance", ctx => ctx.world.zus.fusBalance.toDouble),
    ColumnDef("PpkContributions", ctx => ctx.world.ppk.contributions.toDouble),
    ColumnDef("PpkBondHoldings", ctx => ctx.world.ppk.bondHoldings.toDouble),
    ColumnDef("NRetirees", ctx => ctx.world.demographics.retirees.toDouble),
    ColumnDef("WorkingAgePop", ctx => ctx.world.demographics.workingAgePop.toDouble),
    ColumnDef("MonthlyRetirements", ctx => ctx.world.demographics.monthlyRetirements.toDouble),
    // Macroprudential
    ColumnDef("CCyB", ctx => ctx.world.macropru.ccyb.toDouble),
    ColumnDef("CreditToGdpGap", ctx => ctx.world.macropru.creditToGdpGap),
    ColumnDef(
      "EffectiveMinCar",
      ctx =>
        if ctx.aliveBanks.isEmpty then 0.0
        else ctx.aliveBanks.map(b => Macroprudential.effectiveMinCar(b.id.toInt, ctx.world.macropru.ccyb.toDouble)).max,
    ),
    // GPW Equity Market
    ColumnDef("GpwIndex", ctx => ctx.world.equity.index),
    ColumnDef("GpwMarketCap", ctx => ctx.world.equity.marketCap.toDouble),
    ColumnDef(
      "GpwPE",
      ctx => if ctx.world.equity.earningsYield.toDouble > 0 then 1.0 / ctx.world.equity.earningsYield.toDouble else 0.0,
    ),
    ColumnDef("GpwDivYield", ctx => ctx.world.equity.dividendYield.toDouble),
    ColumnDef("EquityIssuanceTotal", ctx => ctx.world.equity.lastIssuance.toDouble),
    ColumnDef(
      "EquityFinancedFrac",
      ctx =>
        ctx.living.kahanSumBy(_.equityRaised.toDouble) / Math.max(
          1.0,
          ctx.living.kahanSumBy(f => (f.debt + f.equityRaised).toDouble),
        ),
    ),
    ColumnDef("HhEquityWealth", ctx => ctx.world.equity.hhEquityWealth.toDouble),
    ColumnDef("EquityWealthEffect", ctx => ctx.world.equity.lastWealthEffect.toDouble),
    ColumnDef("DomesticDividends", ctx => ctx.world.equity.lastDomesticDividends.toDouble),
    ColumnDef("ForeignDividendOutflow", ctx => ctx.world.equity.lastForeignDividends.toDouble),
    // Housing Market
    ColumnDef("HousingPriceIndex", ctx => ctx.world.housing.priceIndex),
    ColumnDef("HousingMarketValue", ctx => ctx.world.housing.totalValue.toDouble),
    ColumnDef("MortgageStock", ctx => ctx.world.housing.mortgageStock.toDouble),
    ColumnDef("AvgMortgageRate", ctx => ctx.world.housing.avgMortgageRate.toDouble),
    ColumnDef("MortgageOrigination", ctx => ctx.world.housing.lastOrigination.toDouble),
    ColumnDef("MortgageRepayment", ctx => ctx.world.housing.lastRepayment.toDouble),
    ColumnDef("MortgageDefault", ctx => ctx.world.housing.lastDefault.toDouble),
    ColumnDef("MortgageInterestIncome", ctx => ctx.world.housing.mortgageInterestIncome.toDouble),
    ColumnDef("HhHousingWealth", ctx => ctx.world.housing.hhHousingWealth.toDouble),
    ColumnDef("HousingWealthEffect", ctx => ctx.world.housing.lastWealthEffect.toDouble),
    ColumnDef(
      "MortgageToGdp",
      ctx =>
        if ctx.world.gdpProxy > 0 && ctx.world.housing.mortgageStock > PLN.Zero
        then (ctx.world.housing.mortgageStock / (ctx.world.gdpProxy * 12.0)).toDouble
        else 0.0,
    ),
    // Sectoral Labor Mobility
    ColumnDef("SectorMobilityRate", ctx => ctx.world.sectoralMobility.sectorMobilityRate),
    ColumnDef("CrossSectorHires", ctx => ctx.world.sectoralMobility.crossSectorHires.toDouble),
    ColumnDef("VoluntaryQuits", ctx => ctx.world.sectoralMobility.voluntaryQuits.toDouble),
    // GVC / Deep External Sector
    ColumnDef("GvcDisruptionIndex", ctx => ctx.world.gvc.disruptionIndex.toDouble),
    ColumnDef("ForeignPriceIndex", ctx => ctx.world.gvc.foreignPriceIndex),
    ColumnDef("GvcTradeConcentration", ctx => ctx.world.gvc.tradeConcentration.toDouble),
    ColumnDef("GvcExportDemandShock", ctx => ctx.world.gvc.exportDemandShockMag.toDouble),
    ColumnDef("GvcImportCostIndex", ctx => ctx.world.gvc.importCostIndex),
    // Forward-Looking Expectations
    ColumnDef("ExpectedInflation", ctx => ctx.world.expectations.expectedInflation.toDouble),
    ColumnDef("NbpCredibility", ctx => ctx.world.expectations.credibility.toDouble),
    ColumnDef("ForwardGuidanceRate", ctx => ctx.world.expectations.forwardGuidanceRate.toDouble),
    ColumnDef("InflationForecastError", ctx => ctx.world.expectations.forecastError.toDouble),
    // Regional Housing Market
    ColumnDef("WawHpi", ctx => ctx.world.housing.regions.map(_(0).priceIndex).getOrElse(0.0)),
    ColumnDef("KrkHpi", ctx => ctx.world.housing.regions.map(_(1).priceIndex).getOrElse(0.0)),
    ColumnDef("WroHpi", ctx => ctx.world.housing.regions.map(_(2).priceIndex).getOrElse(0.0)),
    ColumnDef("GdnHpi", ctx => ctx.world.housing.regions.map(_(3).priceIndex).getOrElse(0.0)),
    ColumnDef("LdzHpi", ctx => ctx.world.housing.regions.map(_(4).priceIndex).getOrElse(0.0)),
    ColumnDef("PozHpi", ctx => ctx.world.housing.regions.map(_(5).priceIndex).getOrElse(0.0)),
    ColumnDef("RestHpi", ctx => ctx.world.housing.regions.map(_(6).priceIndex).getOrElse(0.0)),
    // Immigration
    ColumnDef("ImmigrantStock", ctx => ctx.world.immigration.immigrantStock.toDouble),
    ColumnDef("MonthlyImmigInflow", ctx => ctx.world.immigration.monthlyInflow.toDouble),
    ColumnDef("RemittanceOutflow", ctx => ctx.world.immigration.remittanceOutflow),
    ColumnDef(
      "ImmigrantUnempRate",
      ctx =>
        if ctx.world.immigration.immigrantStock > 0 then
          val immigrants = ctx.households.filter(_.isImmigrant)
          if immigrants.nonEmpty then
            immigrants.count(h => !h.status.isInstanceOf[HhStatus.Employed]).toDouble / immigrants.length
          else 0.0
        else 0.0,
    ),
    // PIT
    ColumnDef(
      "EffectivePitRate",
      ctx => {
        val agg = ctx.world.hhAgg.get
        val gross = agg.totalIncome + agg.totalPit
        if gross > PLN.Zero then (agg.totalPit / gross).toDouble else 0.0
      },
    ),
    // Social Transfers
    ColumnDef("SocialTransferSpend", ctx => ctx.world.gov.socialTransferSpend.toDouble),
    // Public Investment
    ColumnDef("GovCurrentSpend", ctx => ctx.world.gov.govCurrentSpend.toDouble),
    ColumnDef("GovCapitalSpend", ctx => ctx.world.gov.govCapitalSpend.toDouble),
    ColumnDef("PublicCapitalStock", ctx => ctx.world.gov.publicCapitalStock.toDouble),
    // EU Funds
    ColumnDef("EuCofinancing", ctx => ctx.world.gov.euCofinancing.toDouble),
    ColumnDef("EuFundsMonthly", ctx => ctx.world.bop.euFundsMonthly.toDouble),
    ColumnDef("EuCumulativeAbsorption", ctx => ctx.world.bop.euCumulativeAbsorption.toDouble),
    ColumnDef("MinWageLevel", ctx => ctx.world.hh.minWageLevel.toDouble),
    ColumnDef("FofResidual", ctx => ctx.world.fofResidual),
    // Consumer Credit
    ColumnDef("ConsumerLoans", ctx => ctx.world.bank.consumerLoans.toDouble),
    ColumnDef(
      "ConsumerNplRatio",
      ctx =>
        if ctx.world.bank.consumerLoans > PLN.Zero then ctx.world.bank.consumerNpl / ctx.world.bank.consumerLoans
        else 0.0,
    ),
    ColumnDef("ConsumerOrigination", ctx => ctx.world.hhAgg.map(_.totalConsumerOrigination.toDouble).getOrElse(0.0)),
    ColumnDef("ConsumerDebtService", ctx => ctx.world.hhAgg.map(_.totalConsumerDebtService.toDouble).getOrElse(0.0)),
    // Physical Capital
    ColumnDef("AggCapitalStock", ctx => ctx.living.kahanSumBy(_.capitalStock.toDouble)),
    ColumnDef("GrossInvestment", ctx => ctx.world.grossInvestment.toDouble),
    ColumnDef(
      "CapitalDepreciation",
      ctx =>
        if Config.PhysCapEnabled then
          ctx.living.kahanSumBy(f => (f.capitalStock * Config.PhysCapDepRates(f.sector.toInt) / 12.0).toDouble)
        else 0.0,
    ),
    // Excise & Customs
    ColumnDef("ExciseRevenue", ctx => ctx.world.gov.exciseRevenue.toDouble),
    ColumnDef("CustomsDutyRevenue", ctx => ctx.world.gov.customsDutyRevenue.toDouble),
    // Corporate Bonds / Catalyst (#40)
    ColumnDef("CorpBondOutstanding", ctx => ctx.world.corporateBonds.outstanding.toDouble),
    ColumnDef("CorpBondYield", ctx => ctx.world.corporateBonds.corpBondYield.toDouble),
    ColumnDef("CorpBondIssuance", ctx => ctx.world.corporateBonds.lastIssuance.toDouble),
    ColumnDef("CorpBondSpread", ctx => ctx.world.corporateBonds.creditSpread.toDouble),
    ColumnDef("BankCorpBondHoldings", ctx => ctx.world.corporateBonds.bankHoldings.toDouble),
    ColumnDef("PpkCorpBondHoldings", ctx => ctx.world.corporateBonds.ppkHoldings.toDouble),
    ColumnDef("CorpBondAbsorptionRate", ctx => ctx.world.corporateBonds.lastAbsorptionRate.toDouble),
    // Insurance Sector (#41)
    ColumnDef("InsLifeReserves", ctx => ctx.world.insurance.lifeReserves.toDouble),
    ColumnDef("InsNonLifeReserves", ctx => ctx.world.insurance.nonLifeReserves.toDouble),
    ColumnDef("InsGovBondHoldings", ctx => ctx.world.insurance.govBondHoldings.toDouble),
    ColumnDef("InsLifePremium", ctx => ctx.world.insurance.lastLifePremium.toDouble),
    ColumnDef("InsNonLifePremium", ctx => ctx.world.insurance.lastNonLifePremium.toDouble),
    ColumnDef("InsLifeClaims", ctx => ctx.world.insurance.lastLifeClaims.toDouble),
    ColumnDef("InsNonLifeClaims", ctx => ctx.world.insurance.lastNonLifeClaims.toDouble),
    // Shadow Banking / NBFI (#42)
    ColumnDef("NbfiTfiAum", ctx => ctx.world.nbfi.tfiAum.toDouble),
    ColumnDef("NbfiTfiGovBondHoldings", ctx => ctx.world.nbfi.tfiGovBondHoldings.toDouble),
    ColumnDef("NbfiLoanStock", ctx => ctx.world.nbfi.nbfiLoanStock.toDouble),
    ColumnDef("NbfiOrigination", ctx => ctx.world.nbfi.lastNbfiOrigination.toDouble),
    ColumnDef("NbfiDefaults", ctx => ctx.world.nbfi.lastNbfiDefaultAmount.toDouble),
    ColumnDef("NbfiBankTightness", ctx => ctx.world.nbfi.lastBankTightness.toDouble),
    ColumnDef("NbfiDepositDrain", ctx => ctx.world.nbfi.lastDepositDrain.toDouble),
    // FDI Composition (#33)
    ColumnDef("FdiProfitShifting", ctx => ctx.world.fdiProfitShifting.toDouble),
    ColumnDef("FdiRepatriation", ctx => ctx.world.fdiRepatriation.toDouble),
    ColumnDef("FdiGrossOutflow", ctx => (ctx.world.fdiProfitShifting + ctx.world.fdiRepatriation).toDouble),
    ColumnDef(
      "ForeignOwnedFrac",
      ctx => if ctx.nLiving > 0 then ctx.living.count(_.foreignOwned).toDouble / ctx.nLiving else 0.0,
    ),
    ColumnDef("FdiCitLoss", ctx => ctx.world.fdiCitLoss.toDouble),
    // Endogenous Firm Entry (#35)
    ColumnDef("FirmBirths", ctx => ctx.world.firmBirths.toDouble),
    ColumnDef("FirmDeaths", ctx => ctx.world.firmDeaths.toDouble),
    ColumnDef("NetEntry", ctx => (ctx.world.firmBirths - ctx.world.firmDeaths).toDouble),
    ColumnDef("LivingFirmCount", ctx => ctx.nLiving),
    // Inventories (#43)
    ColumnDef("AggInventoryStock", ctx => ctx.world.aggInventoryStock.toDouble),
    ColumnDef("InventoryChange", ctx => ctx.world.aggInventoryChange.toDouble),
    ColumnDef(
      "InventoryToGdp",
      ctx => if ctx.world.gdpProxy > 0 then (ctx.world.aggInventoryStock / ctx.world.gdpProxy).toDouble else 0.0,
    ),
    // Informal Economy (#45)
    ColumnDef("EffectiveShadowShare", ctx => ctx.world.effectiveShadowShare),
    ColumnDef("TaxEvasionLoss", ctx => ctx.world.taxEvasionLoss.toDouble),
    ColumnDef("InformalEmployment", ctx => ctx.world.informalEmployed.toDouble),
    ColumnDef(
      "EvasionToGdpRatio",
      ctx => if ctx.world.gdpProxy > 0 then (ctx.world.taxEvasionLoss / ctx.world.gdpProxy).toDouble else 0.0,
    ),
    // Energy / Climate (#36)
    ColumnDef("AggEnergyCost", ctx => ctx.world.aggEnergyCost.toDouble),
    ColumnDef(
      "EnergyCostToGdp",
      ctx => if ctx.world.gdpProxy > 0 then (ctx.world.aggEnergyCost / ctx.world.gdpProxy).toDouble else 0.0,
    ),
    ColumnDef("EtsPrice", ctx => ctx.world.etsPrice),
    ColumnDef("AggGreenCapital", ctx => ctx.world.aggGreenCapital.toDouble),
    ColumnDef("GreenInvestment", ctx => ctx.world.aggGreenInvestment.toDouble),
    ColumnDef(
      "GreenCapitalRatio",
      ctx => {
        val aggK = ctx.living.kahanSumBy(_.capitalStock.toDouble)
        if ctx.world.aggGreenCapital > PLN.Zero && aggK > 0 then ctx.world.aggGreenCapital.toDouble / aggK else 0.0
      },
    ),
    // Diaspora Remittances (#46)
    ColumnDef("DiasporaRemittanceInflow", ctx => ctx.world.diasporaRemittanceInflow.toDouble),
    ColumnDef(
      "NetRemittances",
      ctx => (ctx.world.diasporaRemittanceInflow - PLN(ctx.world.immigration.remittanceOutflow)).toDouble,
    ),
    // Tourism (#47)
    ColumnDef("TourismExport", ctx => ctx.world.tourismExport.toDouble),
    ColumnDef("TourismImport", ctx => ctx.world.tourismImport.toDouble),
    ColumnDef("NetTourismBalance", ctx => (ctx.world.tourismExport - ctx.world.tourismImport).toDouble),
    ColumnDef("TourismSeasonalFactor", ctx => ctx.world.tourismSeasonalFactor),
    // KNF/BFG (#48)
    ColumnDef("BfgLevyTotal", ctx => ctx.world.bfgLevyTotal),
    ColumnDef("BfgFundBalance", ctx => ctx.world.bfgFundBalance.toDouble),
    ColumnDef("BailInLoss", ctx => ctx.world.bailInLoss.toDouble),
  )

  /** Column names — derived from schema. */
  val colNames: Array[String] = schema.map(_.name).toArray

  /** Number of columns — derived from schema. */
  val nCols: Int = schema.length

  /** Compute one row. Returns Array[Double] for MC aggregation. */
  def compute(
    t: Int,
    world: World,
    firms: Vector[Firm.State],
    households: Vector[Household.State],
  ): Array[Double] =
    val living = firms.filter(Firm.isAlive)
    val aliveBanks = world.bankingSector.banks.filterNot(_.failed).toVector
    val ctx = Ctx(t, world, firms, households, living, living.length.toDouble, aliveBanks)
    val result = new Array[Double](schema.length)
    var i = 0
    while i < schema.length do
      result(i) = schema(i).compute(ctx)
      i += 1
    result

  /** Typed row access: row.at(Col.Inflation) instead of row(1). */
  extension (row: Array[Double]) def at(c: Col): Double = row(c.ordinal)
