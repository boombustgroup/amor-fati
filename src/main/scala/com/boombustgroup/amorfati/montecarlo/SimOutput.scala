package com.boombustgroup.amorfati.montecarlo

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.*
import com.boombustgroup.amorfati.engine.mechanisms.Macroprudential
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.amorfati.util.KahanSum.*

/** Typed column schema for simulation output.
  *
  * Composable: each group is a `Vector[ColumnDef]`, all composed with `++`.
  * Column handles (`Col`) are derived by name lookup — no mutable counter.
  */
object SimOutput:

  // -------------------------------------------------------------------------
  //  ColumnDef + Ctx
  // -------------------------------------------------------------------------

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
      val p: SimParams,
  ):
    given SimParams                         = p
    lazy val sectorAuto: IndexedSeq[Double] = p.sectorDefs.indices.map { s =>
      val secFirms = living.filter(_.sector.toInt == s)
      if secFirms.isEmpty then 0.0
      else
        secFirms
          .count(f => f.tech.isInstanceOf[TechState.Automated] || f.tech.isInstanceOf[TechState.Hybrid])
          .toDouble / secFirms.length
    }

    inline def unemployPct: Double = world.hhAgg.unemploymentRate(world.totalPopulation)

  // -------------------------------------------------------------------------
  //  Schema groups — composed with ++
  // -------------------------------------------------------------------------

  private def coreGroup: Vector[ColumnDef] = Vector(
    ColumnDef("Month", ctx => (ctx.t + 1).toDouble),
    ColumnDef("Inflation", ctx => ctx.world.inflation.toDouble),
    ColumnDef("Unemployment", ctx => ctx.unemployPct),
    ColumnDef("TotalAdoption", ctx => (ctx.world.real.automationRatio + ctx.world.real.hybridRatio).toDouble),
    ColumnDef("ExRate", ctx => ctx.world.forex.exchangeRate),
    ColumnDef("MarketWage", ctx => ctx.world.hhAgg.marketWage.toDouble),
    ColumnDef("GovDebt", ctx => ctx.world.gov.cumulativeDebt.toDouble),
    ColumnDef("NPL", ctx => ctx.world.bankingSector.aggregate.nplRatio.toDouble),
    ColumnDef("RefRate", ctx => ctx.world.nbp.referenceRate.toDouble),
    ColumnDef("PriceLevel", ctx => ctx.world.priceLevel),
    ColumnDef("AutoRatio", ctx => ctx.world.real.automationRatio.toDouble),
    ColumnDef("HybridRatio", ctx => ctx.world.real.hybridRatio.toDouble),
  )

  private def sectoralGroup: Vector[ColumnDef] = Vector(
    // per-sector automation ratios
    ColumnDef("BPO_Auto", ctx => ctx.sectorAuto(0)),
    ColumnDef("Manuf_Auto", ctx => ctx.sectorAuto(1)),
    ColumnDef("Retail_Auto", ctx => ctx.sectorAuto(2)),
    ColumnDef("Health_Auto", ctx => ctx.sectorAuto(3)),
    ColumnDef("Public_Auto", ctx => ctx.sectorAuto(4)),
    ColumnDef("Agri_Auto", ctx => ctx.sectorAuto(5)),
    // per-sector current sigma
    ColumnDef("BPO_Sigma", ctx => ctx.world.currentSigmas(0)),
    ColumnDef("Manuf_Sigma", ctx => ctx.world.currentSigmas(1)),
    ColumnDef("Retail_Sigma", ctx => ctx.world.currentSigmas(2)),
    ColumnDef("Health_Sigma", ctx => ctx.world.currentSigmas(3)),
    ColumnDef("Public_Sigma", ctx => ctx.world.currentSigmas(4)),
    ColumnDef("Agri_Sigma", ctx => ctx.world.currentSigmas(5)),
    ColumnDef("MeanDegree", ctx => ctx.firms.kahanSumBy(_.neighbors.length.toDouble) / ctx.firms.length),
    ColumnDef("IoFlows", ctx => ctx.world.flows.ioFlows.toDouble),
    ColumnDef(
      "IoGdpRatio",
      ctx => if ctx.world.gdpProxy > 0 then (ctx.world.flows.ioFlows / ctx.world.gdpProxy).toDouble else 0.0,
    ),
  )

  private def externalGroup: Vector[ColumnDef] = Vector(
    ColumnDef("NFA", ctx => ctx.world.bop.nfa.toDouble),
    ColumnDef("CurrentAccount", ctx => ctx.world.bop.currentAccount.toDouble),
    ColumnDef("CapitalAccount", ctx => ctx.world.bop.capitalAccount.toDouble),
    ColumnDef("TradeBalance_OE", ctx => ctx.world.bop.tradeBalance.toDouble),
    ColumnDef("Exports_OE", ctx => ctx.world.bop.exports.toDouble),
    ColumnDef("TotalImports_OE", ctx => ctx.world.bop.totalImports.toDouble),
    ColumnDef("ImportedInterm", ctx => ctx.world.bop.importedIntermediates.toDouble),
    ColumnDef("FDI", ctx => ctx.world.bop.fdi.toDouble),
    // GVC / Deep External Sector
    ColumnDef("GvcDisruptionIndex", ctx => ctx.world.external.gvc.disruptionIndex.toDouble),
    ColumnDef("ForeignPriceIndex", ctx => ctx.world.external.gvc.foreignPriceIndex.toDouble),
    ColumnDef("GvcTradeConcentration", ctx => ctx.world.external.gvc.tradeConcentration.toDouble),
    ColumnDef("GvcExportDemandShock", ctx => ctx.world.external.gvc.exportDemandShockMag.toDouble),
    ColumnDef("GvcImportCostIndex", ctx => ctx.world.external.gvc.importCostIndex.toDouble),
    ColumnDef("CommodityPriceIndex", ctx => ctx.world.external.gvc.commodityPriceIndex.toDouble),
    // Immigration
    ColumnDef("ImmigrantStock", ctx => ctx.world.external.immigration.immigrantStock.toDouble),
    ColumnDef("MonthlyImmigInflow", ctx => ctx.world.external.immigration.monthlyInflow.toDouble),
    ColumnDef("RemittanceOutflow", ctx => ctx.world.external.immigration.remittanceOutflow.toDouble),
    ColumnDef(
      "ImmigrantUnempRate",
      ctx =>
        if ctx.world.external.immigration.immigrantStock > 0 then
          val immigrants = ctx.households.filter(_.isImmigrant)
          if immigrants.nonEmpty then immigrants.count(h => !h.status.isInstanceOf[HhStatus.Employed]).toDouble / immigrants.length
          else 0.0
        else 0.0,
    ),
    // FX
    ColumnDef("FxReserves", ctx => ctx.world.nbp.fxReserves.toDouble),
    ColumnDef("FxInterventionAmt", ctx => ctx.world.nbp.lastFxTraded.toDouble),
    ColumnDef("FxInterventionActive", ctx => if ctx.p.flags.nbpFxIntervention then 1.0 else 0.0),
    // Diaspora Remittances
    ColumnDef("DiasporaRemittanceInflow", ctx => ctx.world.flows.diasporaRemittanceInflow.toDouble),
    ColumnDef(
      "NetRemittances",
      ctx => (ctx.world.flows.diasporaRemittanceInflow - ctx.world.external.immigration.remittanceOutflow).toDouble,
    ),
    // Tourism
    ColumnDef("TourismExport", ctx => ctx.world.flows.tourismExport.toDouble),
    ColumnDef("TourismImport", ctx => ctx.world.flows.tourismImport.toDouble),
    ColumnDef("NetTourismBalance", ctx => (ctx.world.flows.tourismExport - ctx.world.flows.tourismImport).toDouble),
    ColumnDef("TourismSeasonalFactor", ctx => ctx.world.external.tourismSeasonalFactor),
  )

  private def fiscalGroup: Vector[ColumnDef] = Vector(
    ColumnDef("UnempBenefitSpend", ctx => ctx.world.gov.unempBenefitSpend.toDouble),
    ColumnDef(
      "OutputGap",
      ctx => (ctx.unemployPct - ctx.p.monetary.nairu.toDouble) / ctx.p.monetary.nairu.toDouble,
    ),
    ColumnDef(
      "EffectivePitRate",
      ctx => {
        val agg   = ctx.world.hhAgg
        val gross = agg.totalIncome + agg.totalPit
        if gross > PLN.Zero then (agg.totalPit / gross).toDouble else 0.0
      },
    ),
    ColumnDef("SocialTransferSpend", ctx => ctx.world.gov.socialTransferSpend.toDouble),
    ColumnDef("GovCurrentSpend", ctx => ctx.world.gov.govCurrentSpend.toDouble),
    ColumnDef("GovCapitalSpend", ctx => ctx.world.gov.govCapitalSpend.toDouble),
    ColumnDef("PublicCapitalStock", ctx => ctx.world.gov.publicCapitalStock.toDouble),
    ColumnDef("EuCofinancing", ctx => ctx.world.gov.euCofinancing.toDouble),
    ColumnDef("EuFundsMonthly", ctx => ctx.world.bop.euFundsMonthly.toDouble),
    ColumnDef("EuCumulativeAbsorption", ctx => ctx.world.bop.euCumulativeAbsorption.toDouble),
    ColumnDef("MinWageLevel", ctx => ctx.world.gov.minWageLevel.toDouble),
    ColumnDef("ExciseRevenue", ctx => ctx.world.gov.exciseRevenue.toDouble),
    ColumnDef("CustomsDutyRevenue", ctx => ctx.world.gov.customsDutyRevenue.toDouble),
    // Fiscal rules
    ColumnDef(
      "DebtToGdp",
      ctx => if ctx.world.gdpProxy > 0 then ctx.world.gov.cumulativeDebt.toDouble / (ctx.world.gdpProxy * 12.0) else 0.0,
    ),
    ColumnDef(
      "DeficitToGdp",
      ctx => if ctx.world.gdpProxy > 0 then ctx.world.gov.deficit.toDouble / (ctx.world.gdpProxy * 12.0) else 0.0,
    ),
    ColumnDef("FiscalRuleBinding", ctx => ctx.world.flows.fiscalRuleSeverity.toDouble),
    ColumnDef("GovSpendingCutRatio", ctx => ctx.world.flows.govSpendingCutRatio.toDouble),
  )

  private def monetaryGroup: Vector[ColumnDef] = Vector(
    // Bond market
    ColumnDef("BondYield", ctx => ctx.world.gov.bondYield.toDouble),
    ColumnDef("BondsOutstanding", ctx => ctx.world.gov.bondsOutstanding.toDouble),
    ColumnDef("BankBondHoldings", ctx => ctx.world.bank.govBondHoldings.toDouble),
    ColumnDef("NbpBondHoldings", ctx => ctx.world.nbp.govBondHoldings.toDouble),
    ColumnDef("QeActive", ctx => if ctx.world.nbp.qeActive then 1.0 else 0.0),
    ColumnDef("DebtService", ctx => ctx.world.gov.debtServiceSpend.toDouble),
    ColumnDef("NbpRemittance", ctx => (ctx.world.nbp.govBondHoldings * ctx.world.gov.bondYield / 12.0).toDouble),
    // Monetary plumbing
    ColumnDef("ReserveInterest", ctx => ctx.world.plumbing.reserveInterestTotal.toDouble),
    ColumnDef("StandingFacilityNet", ctx => ctx.world.plumbing.standingFacilityNet.toDouble),
    ColumnDef("DepositFacilityUsage", ctx => ctx.world.plumbing.depositFacilityUsage.toDouble),
    ColumnDef("InterbankInterestNet", ctx => ctx.world.plumbing.interbankInterestNet.toDouble),
    // Monetary aggregates
    ColumnDef("M0", ctx => ctx.world.monetaryAgg.map(_.m0.toDouble).getOrElse(0.0)),
    ColumnDef("M1", ctx => ctx.world.monetaryAgg.map(_.m1.toDouble).getOrElse(ctx.world.bank.deposits.toDouble)),
    ColumnDef("M2", ctx => ctx.world.monetaryAgg.map(_.m2.toDouble).getOrElse(ctx.world.bank.deposits.toDouble)),
    ColumnDef("M3", ctx => ctx.world.monetaryAgg.map(_.m3.toDouble).getOrElse(ctx.world.bank.deposits.toDouble)),
    ColumnDef("CreditMultiplier", ctx => ctx.world.monetaryAgg.map(_.creditMultiplier).getOrElse(0.0)),
    ColumnDef("FofResidual", ctx => ctx.world.plumbing.fofResidual.toDouble),
  )

  private def financialGroup: Vector[ColumnDef] = Vector(
    // Interbank
    ColumnDef("InterbankRate", ctx => ctx.world.bankingSector.interbankRate.toDouble),
    ColumnDef("MinBankCAR", ctx => if ctx.aliveBanks.isEmpty then 0.0 else ctx.aliveBanks.map(_.car).min.toDouble),
    ColumnDef("MaxBankNPL", ctx => if ctx.aliveBanks.isEmpty then 0.0 else ctx.aliveBanks.map(_.nplRatio).max.toDouble),
    ColumnDef("BankFailures", ctx => ctx.world.bankingSector.banks.count(_.failed).toDouble),
    // LCR/NSFR
    ColumnDef(
      "MinBankLCR",
      ctx => { given SimParams = ctx.p; if ctx.aliveBanks.isEmpty then 0.0 else ctx.aliveBanks.map(_.lcr).min.toDouble },
    ),
    ColumnDef("MinBankNSFR", ctx => if ctx.aliveBanks.isEmpty then 0.0 else ctx.aliveBanks.map(_.nsfr).min.toDouble),
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
    // Consumer Credit
    ColumnDef("ConsumerLoans", ctx => ctx.world.bank.consumerLoans.toDouble),
    ColumnDef(
      "ConsumerNplRatio",
      ctx =>
        if ctx.world.bank.consumerLoans > PLN.Zero then ctx.world.bank.consumerNpl / ctx.world.bank.consumerLoans
        else 0.0,
    ),
    ColumnDef("ConsumerOrigination", ctx => ctx.world.hhAgg.totalConsumerOrigination.toDouble),
    ColumnDef("ConsumerDebtService", ctx => ctx.world.hhAgg.totalConsumerDebtService.toDouble),
    // GPW Equity Market
    ColumnDef("GpwIndex", ctx => ctx.world.financial.equity.index),
    ColumnDef("GpwMarketCap", ctx => ctx.world.financial.equity.marketCap.toDouble),
    ColumnDef(
      "GpwPE",
      ctx => if ctx.world.financial.equity.earningsYield.toDouble > 0 then 1.0 / ctx.world.financial.equity.earningsYield.toDouble else 0.0,
    ),
    ColumnDef("GpwDivYield", ctx => ctx.world.financial.equity.dividendYield.toDouble),
    ColumnDef("EquityIssuanceTotal", ctx => ctx.world.financial.equity.lastIssuance.toDouble),
    ColumnDef(
      "EquityFinancedFrac",
      ctx =>
        ctx.living.kahanSumBy(_.equityRaised.toDouble) / Math.max(
          1.0,
          ctx.living.kahanSumBy(f => (f.debt + f.equityRaised).toDouble),
        ),
    ),
    ColumnDef("HhEquityWealth", ctx => ctx.world.financial.equity.hhEquityWealth.toDouble),
    ColumnDef("EquityWealthEffect", ctx => ctx.world.financial.equity.lastWealthEffect.toDouble),
    ColumnDef("DomesticDividends", ctx => ctx.world.financial.equity.lastDomesticDividends.toDouble),
    ColumnDef("ForeignDividendOutflow", ctx => ctx.world.financial.equity.lastForeignDividends.toDouble),
    // Corporate Bonds / Catalyst
    ColumnDef("CorpBondOutstanding", ctx => ctx.world.financial.corporateBonds.outstanding.toDouble),
    ColumnDef("CorpBondYield", ctx => ctx.world.financial.corporateBonds.corpBondYield.toDouble),
    ColumnDef("CorpBondIssuance", ctx => ctx.world.financial.corporateBonds.lastIssuance.toDouble),
    ColumnDef("CorpBondSpread", ctx => ctx.world.financial.corporateBonds.creditSpread.toDouble),
    ColumnDef("BankCorpBondHoldings", ctx => ctx.world.financial.corporateBonds.bankHoldings.toDouble),
    ColumnDef("PpkCorpBondHoldings", ctx => ctx.world.financial.corporateBonds.ppkHoldings.toDouble),
    ColumnDef("CorpBondAbsorptionRate", ctx => ctx.world.financial.corporateBonds.lastAbsorptionRate.toDouble),
    // Insurance Sector
    ColumnDef("InsLifeReserves", ctx => ctx.world.financial.insurance.lifeReserves.toDouble),
    ColumnDef("InsNonLifeReserves", ctx => ctx.world.financial.insurance.nonLifeReserves.toDouble),
    ColumnDef("InsGovBondHoldings", ctx => ctx.world.financial.insurance.govBondHoldings.toDouble),
    ColumnDef("InsLifePremium", ctx => ctx.world.financial.insurance.lastLifePremium.toDouble),
    ColumnDef("InsNonLifePremium", ctx => ctx.world.financial.insurance.lastNonLifePremium.toDouble),
    ColumnDef("InsLifeClaims", ctx => ctx.world.financial.insurance.lastLifeClaims.toDouble),
    ColumnDef("InsNonLifeClaims", ctx => ctx.world.financial.insurance.lastNonLifeClaims.toDouble),
    // Shadow Banking / NBFI
    ColumnDef("NbfiTfiAum", ctx => ctx.world.financial.nbfi.tfiAum.toDouble),
    ColumnDef("NbfiTfiGovBondHoldings", ctx => ctx.world.financial.nbfi.tfiGovBondHoldings.toDouble),
    ColumnDef("NbfiLoanStock", ctx => ctx.world.financial.nbfi.nbfiLoanStock.toDouble),
    ColumnDef("NbfiOrigination", ctx => ctx.world.financial.nbfi.lastNbfiOrigination.toDouble),
    ColumnDef("NbfiDefaults", ctx => ctx.world.financial.nbfi.lastNbfiDefaultAmount.toDouble),
    ColumnDef("NbfiBankTightness", ctx => ctx.world.financial.nbfi.lastBankTightness.toDouble),
    ColumnDef("NbfiDepositDrain", ctx => ctx.world.financial.nbfi.lastDepositDrain.toDouble),
    // AFS/HTM bond portfolio split
    ColumnDef("BankAfsBonds", ctx => ctx.world.bank.afsBonds.toDouble),
    ColumnDef("BankHtmBonds", ctx => ctx.world.bank.htmBonds.toDouble),
    // KNF/BFG
    ColumnDef("BfgLevyTotal", ctx => ctx.world.flows.bfgLevyTotal),
    ColumnDef("BfgFundBalance", ctx => ctx.world.mechanisms.bfgFundBalance.toDouble),
    ColumnDef("BailInLoss", ctx => ctx.world.flows.bailInLoss.toDouble),
  )

  private def realGroup: Vector[ColumnDef] = Vector(
    // Housing Market
    ColumnDef("HousingPriceIndex", ctx => ctx.world.real.housing.priceIndex),
    ColumnDef("HousingMarketValue", ctx => ctx.world.real.housing.totalValue.toDouble),
    ColumnDef("MortgageStock", ctx => ctx.world.real.housing.mortgageStock.toDouble),
    ColumnDef("AvgMortgageRate", ctx => ctx.world.real.housing.avgMortgageRate.toDouble),
    ColumnDef("MortgageOrigination", ctx => ctx.world.real.housing.lastOrigination.toDouble),
    ColumnDef("MortgageRepayment", ctx => ctx.world.real.housing.lastRepayment.toDouble),
    ColumnDef("MortgageDefault", ctx => ctx.world.real.housing.lastDefault.toDouble),
    ColumnDef("MortgageInterestIncome", ctx => ctx.world.real.housing.mortgageInterestIncome.toDouble),
    ColumnDef("HhHousingWealth", ctx => ctx.world.real.housing.hhHousingWealth.toDouble),
    ColumnDef("HousingWealthEffect", ctx => ctx.world.real.housing.lastWealthEffect.toDouble),
    ColumnDef(
      "MortgageToGdp",
      ctx =>
        if ctx.world.gdpProxy > 0 && ctx.world.real.housing.mortgageStock > PLN.Zero
        then (ctx.world.real.housing.mortgageStock / (ctx.world.gdpProxy * 12.0)).toDouble
        else 0.0,
    ),
    // Regional Housing Market
    ColumnDef("WawHpi", ctx => ctx.world.real.housing.regions.map(_(0).priceIndex).getOrElse(0.0)),
    ColumnDef("KrkHpi", ctx => ctx.world.real.housing.regions.map(_(1).priceIndex).getOrElse(0.0)),
    ColumnDef("WroHpi", ctx => ctx.world.real.housing.regions.map(_(2).priceIndex).getOrElse(0.0)),
    ColumnDef("GdnHpi", ctx => ctx.world.real.housing.regions.map(_(3).priceIndex).getOrElse(0.0)),
    ColumnDef("LdzHpi", ctx => ctx.world.real.housing.regions.map(_(4).priceIndex).getOrElse(0.0)),
    ColumnDef("PozHpi", ctx => ctx.world.real.housing.regions.map(_(5).priceIndex).getOrElse(0.0)),
    ColumnDef("RestHpi", ctx => ctx.world.real.housing.regions.map(_(6).priceIndex).getOrElse(0.0)),
    // Sectoral Labor Mobility
    ColumnDef("SectorMobilityRate", ctx => ctx.world.real.sectoralMobility.sectorMobilityRate),
    ColumnDef("CrossSectorHires", ctx => ctx.world.real.sectoralMobility.crossSectorHires.toDouble),
    ColumnDef("VoluntaryQuits", ctx => ctx.world.real.sectoralMobility.voluntaryQuits.toDouble),
    // Physical Capital
    ColumnDef("AggCapitalStock", ctx => ctx.living.kahanSumBy(_.capitalStock.toDouble)),
    ColumnDef("GrossInvestment", ctx => ctx.world.real.grossInvestment.toDouble),
    ColumnDef(
      "CapitalDepreciation",
      ctx =>
        if ctx.p.flags.physCap then ctx.living.kahanSumBy(f => (f.capitalStock * ctx.p.capital.depRates.map(_.toDouble)(f.sector.toInt) / 12.0).toDouble)
        else 0.0,
    ),
    // Inventories
    ColumnDef("AggInventoryStock", ctx => ctx.world.flows.aggInventoryStock.toDouble),
    ColumnDef("InventoryChange", ctx => ctx.world.flows.aggInventoryChange.toDouble),
    ColumnDef(
      "InventoryToGdp",
      ctx => if ctx.world.gdpProxy > 0 then (ctx.world.flows.aggInventoryStock / ctx.world.gdpProxy).toDouble else 0.0,
    ),
    // Energy / Climate
    ColumnDef("AggEnergyCost", ctx => ctx.world.flows.aggEnergyCost.toDouble),
    ColumnDef(
      "EnergyCostToGdp",
      ctx => if ctx.world.gdpProxy > 0 then (ctx.world.flows.aggEnergyCost / ctx.world.gdpProxy).toDouble else 0.0,
    ),
    ColumnDef("EtsPrice", ctx => ctx.world.real.etsPrice),
    ColumnDef("AggGreenCapital", ctx => ctx.world.real.aggGreenCapital.toDouble),
    ColumnDef("GreenInvestment", ctx => ctx.world.real.aggGreenInvestment.toDouble),
    ColumnDef(
      "GreenCapitalRatio",
      ctx => {
        val aggK = ctx.living.kahanSumBy(_.capitalStock.toDouble)
        if ctx.world.real.aggGreenCapital > PLN.Zero && aggK > 0 then ctx.world.real.aggGreenCapital.toDouble / aggK else 0.0
      },
    ),
  )

  private def socialGroup: Vector[ColumnDef] = Vector(
    // JST
    ColumnDef("JstRevenue", ctx => ctx.world.social.jst.revenue.toDouble),
    ColumnDef("JstSpending", ctx => ctx.world.social.jst.spending.toDouble),
    ColumnDef("JstDebt", ctx => ctx.world.social.jst.debt.toDouble),
    ColumnDef("JstDeposits", ctx => ctx.world.social.jst.deposits.toDouble),
    ColumnDef("JstDeficit", ctx => ctx.world.social.jst.deficit.toDouble),
    // ZUS/PPK
    ColumnDef("ZusContributions", ctx => ctx.world.social.zus.contributions.toDouble),
    ColumnDef("ZusPensionPayments", ctx => ctx.world.social.zus.pensionPayments.toDouble),
    ColumnDef("ZusGovSubvention", ctx => ctx.world.social.zus.govSubvention.toDouble),
    ColumnDef("FusBalance", ctx => ctx.world.social.zus.fusBalance.toDouble),
    ColumnDef("NfzContributions", ctx => ctx.world.social.nfz.contributions.toDouble),
    ColumnDef("NfzSpending", ctx => ctx.world.social.nfz.spending.toDouble),
    ColumnDef("NfzBalance", ctx => ctx.world.social.nfz.balance.toDouble),
    ColumnDef("NfzGovSubvention", ctx => ctx.world.social.nfz.govSubvention.toDouble),
    ColumnDef("PpkContributions", ctx => ctx.world.social.ppk.contributions.toDouble),
    ColumnDef("PpkBondHoldings", ctx => ctx.world.social.ppk.bondHoldings.toDouble),
    ColumnDef("NRetirees", ctx => ctx.world.social.demographics.retirees.toDouble),
    ColumnDef("WorkingAgePop", ctx => ctx.world.social.demographics.workingAgePop.toDouble),
    ColumnDef("MonthlyRetirements", ctx => ctx.world.social.demographics.monthlyRetirements.toDouble),
    // Forward-Looking Expectations
    ColumnDef("ExpectedInflation", ctx => ctx.world.mechanisms.expectations.expectedInflation.toDouble),
    ColumnDef("NbpCredibility", ctx => ctx.world.mechanisms.expectations.credibility.toDouble),
    ColumnDef("ForwardGuidanceRate", ctx => ctx.world.mechanisms.expectations.forwardGuidanceRate.toDouble),
    ColumnDef("InflationForecastError", ctx => ctx.world.mechanisms.expectations.forecastError.toDouble),
  )

  private def mechanismsGroup: Vector[ColumnDef] = Vector(
    // Macroprudential
    ColumnDef("CCyB", ctx => ctx.world.mechanisms.macropru.ccyb.toDouble),
    ColumnDef("CreditToGdpGap", ctx => ctx.world.mechanisms.macropru.creditToGdpGap),
    ColumnDef(
      "EffectiveMinCar",
      ctx =>
        if ctx.aliveBanks.isEmpty then 0.0
        else {
          given SimParams = ctx.p;
          ctx.aliveBanks.map(b => Macroprudential.effectiveMinCar(b.id.toInt, ctx.world.mechanisms.macropru.ccyb.toDouble)).max
        },
    ),
    // FDI Composition
    ColumnDef("FdiProfitShifting", ctx => ctx.world.flows.fdiProfitShifting.toDouble),
    ColumnDef("FdiRepatriation", ctx => ctx.world.flows.fdiRepatriation.toDouble),
    ColumnDef("FdiGrossOutflow", ctx => (ctx.world.flows.fdiProfitShifting + ctx.world.flows.fdiRepatriation).toDouble),
    ColumnDef(
      "ForeignOwnedFrac",
      ctx => if ctx.nLiving > 0 then ctx.living.count(_.foreignOwned).toDouble / ctx.nLiving else 0.0,
    ),
    ColumnDef("FdiCitLoss", ctx => ctx.world.flows.fdiCitLoss.toDouble),
    // Endogenous Firm Entry
    ColumnDef("FirmBirths", ctx => ctx.world.flows.firmBirths.toDouble),
    ColumnDef("FirmDeaths", ctx => ctx.world.flows.firmDeaths.toDouble),
    ColumnDef("NetEntry", ctx => (ctx.world.flows.firmBirths - ctx.world.flows.firmDeaths).toDouble),
    ColumnDef("LivingFirmCount", ctx => ctx.nLiving),
    // Informal Economy
    ColumnDef("EffectiveShadowShare", ctx => ctx.world.mechanisms.effectiveShadowShare),
    ColumnDef("TaxEvasionLoss", ctx => ctx.world.flows.taxEvasionLoss.toDouble),
    ColumnDef("InformalEmployment", ctx => ctx.world.flows.informalEmployed),
    ColumnDef(
      "EvasionToGdpRatio",
      ctx => if ctx.world.gdpProxy > 0 then (ctx.world.flows.taxEvasionLoss / ctx.world.gdpProxy).toDouble else 0.0,
    ),
  )

  // -------------------------------------------------------------------------
  //  Flat schema — single source of truth
  // -------------------------------------------------------------------------

  private val schema: Array[ColumnDef] =
    (coreGroup
      ++ sectoralGroup
      ++ externalGroup
      ++ fiscalGroup
      ++ monetaryGroup
      ++ financialGroup
      ++ realGroup
      ++ socialGroup
      ++ mechanismsGroup).toArray

  // -------------------------------------------------------------------------
  //  Col — opaque Int, derived by name lookup
  // -------------------------------------------------------------------------

  /** A typed column handle. Wraps an ordinal — prevents raw Int column access.
    */
  opaque type Col = Int
  object Col:
    private val nameToIdx: Map[String, Int] =
      schema.iterator.zipWithIndex.map((cd, i) => cd.name -> i).toMap

    private def lookup(name: String): Col =
      nameToIdx.getOrElse(name, throw new NoSuchElementException(s"Unknown column: $name"))

    val Month: Col                  = lookup("Month")
    val Inflation: Col              = lookup("Inflation")
    val Unemployment: Col           = lookup("Unemployment")
    val TotalAdoption: Col          = lookup("TotalAdoption")
    val ExRate: Col                 = lookup("ExRate")
    val MarketWage: Col             = lookup("MarketWage")
    val GovDebt: Col                = lookup("GovDebt")
    val NPL: Col                    = lookup("NPL")
    val RefRate: Col                = lookup("RefRate")
    val PriceLevel: Col             = lookup("PriceLevel")
    val AutoRatio: Col              = lookup("AutoRatio")
    val HybridRatio: Col            = lookup("HybridRatio")
    val BpoAuto: Col                = lookup("BPO_Auto")
    val ManufAuto: Col              = lookup("Manuf_Auto")
    val RetailAuto: Col             = lookup("Retail_Auto")
    val HealthAuto: Col             = lookup("Health_Auto")
    val PublicAuto: Col             = lookup("Public_Auto")
    val AgriAuto: Col               = lookup("Agri_Auto")
    val BpoSigma: Col               = lookup("BPO_Sigma")
    val ManufSigma: Col             = lookup("Manuf_Sigma")
    val RetailSigma: Col            = lookup("Retail_Sigma")
    val HealthSigma: Col            = lookup("Health_Sigma")
    val PublicSigma: Col            = lookup("Public_Sigma")
    val AgriSigma: Col              = lookup("Agri_Sigma")
    val MeanDegree: Col             = lookup("MeanDegree")
    val IoFlows: Col                = lookup("IoFlows")
    val IoGdpRatio: Col             = lookup("IoGdpRatio")
    val NFA: Col                    = lookup("NFA")
    val CurrentAccount: Col         = lookup("CurrentAccount")
    val CapitalAccount: Col         = lookup("CapitalAccount")
    val TradeBalance: Col           = lookup("TradeBalance_OE")
    val Exports: Col                = lookup("Exports_OE")
    val TotalImports: Col           = lookup("TotalImports_OE")
    val ImportedInterm: Col         = lookup("ImportedInterm")
    val FDI: Col                    = lookup("FDI")
    val UnempBenefitSpend: Col      = lookup("UnempBenefitSpend")
    val OutputGap: Col              = lookup("OutputGap")
    val BondYield: Col              = lookup("BondYield")
    val BondsOutstanding: Col       = lookup("BondsOutstanding")
    val BankBondHoldings: Col       = lookup("BankBondHoldings")
    val NbpBondHoldings: Col        = lookup("NbpBondHoldings")
    val PpkBondHoldings: Col        = lookup("PpkBondHoldings")
    val InsGovBondHoldings: Col     = lookup("InsGovBondHoldings")
    val NbfiTfiGovBondHoldings: Col = lookup("NbfiTfiGovBondHoldings")
    val QeActive: Col               = lookup("QeActive")
    val DebtService: Col            = lookup("DebtService")
    val NbpRemittance: Col          = lookup("NbpRemittance")
    val FxReserves: Col             = lookup("FxReserves")
    val FxInterventionAmt: Col      = lookup("FxInterventionAmt")
    val FxInterventionActive: Col   = lookup("FxInterventionActive")
    val InterbankRate: Col          = lookup("InterbankRate")
    val MinBankCAR: Col             = lookup("MinBankCAR")
    val MaxBankNPL: Col             = lookup("MaxBankNPL")
    val BankFailures: Col           = lookup("BankFailures")
    val ReserveInterest: Col        = lookup("ReserveInterest")
    val StandingFacilityNet: Col    = lookup("StandingFacilityNet")
    val DepositFacilityUsage: Col   = lookup("DepositFacilityUsage")
    val InterbankInterestNet: Col   = lookup("InterbankInterestNet")

    private val sectorAutoNames  = Vector("BPO_Auto", "Manuf_Auto", "Retail_Auto", "Health_Auto", "Public_Auto", "Agri_Auto")
    private val sectorSigmaNames = Vector("BPO_Sigma", "Manuf_Sigma", "Retail_Sigma", "Health_Sigma", "Public_Sigma", "Agri_Sigma")

    def sectorAuto(s: Int): Col  = lookup(sectorAutoNames(s))
    def sectorSigma(s: Int): Col = lookup(sectorSigmaNames(s))

  extension (c: Col) def ordinal: Int = c

  // -------------------------------------------------------------------------
  //  Public API
  // -------------------------------------------------------------------------

  /** Column names — derived from schema. */
  val colNames: Array[String] = schema.map(_.name)

  /** Number of columns — derived from schema. */
  val nCols: Int = schema.length

  /** Compute one row. Returns Array[Double] for MC aggregation. */
  def compute(
      t: Int,
      world: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
  )(using p: SimParams): Array[Double] =
    val living     = firms.filter(Firm.isAlive)
    val aliveBanks = world.bankingSector.banks.filterNot(_.failed).toVector
    val ctx        = Ctx(t, world, firms, households, living, living.length.toDouble, aliveBanks, p)
    val result     = new Array[Double](schema.length)
    var i          = 0
    while i < schema.length do
      result(i) = schema(i).compute(ctx)
      i += 1
    result

  /** Typed row access: row.at(Col.Inflation) instead of row(1). */
  extension (row: Array[Double]) def at(c: Col): Double = row(c.ordinal)
