package com.boombustgroup.amorfati.montecarlo

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.SimParams
import com.boombustgroup.amorfati.engine.*
import com.boombustgroup.amorfati.engine.mechanisms.Macroprudential
import com.boombustgroup.amorfati.fp.ComputationBoundary
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.amorfati.agents.Region

/** Typed column schema for simulation output.
  *
  * Composable: each group is a `Vector[ColumnDef]`, all composed with `++`.
  * Column handles (`Col`) are derived by name lookup — no mutable counter.
  */
object SimOutput:

  private val td                                          = ComputationBoundary
  private def exchangeRateValue(er: ExchangeRate): Double = er.toLong.toDouble / com.boombustgroup.amorfati.fp.FixedPointBase.ScaleD

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
      val banks: Vector[Banking.BankState],
      val householdAggregates: Household.Aggregates,
      val living: Vector[Firm.State],
      val nLiving: Double,
      val aliveBanks: Vector[Banking.BankState],
      val p: SimParams,
  ):
    given SimParams                                          = p
    lazy val bankAgg: Banking.Aggregate                      = Banking.aggregateFromBanks(banks)
    lazy val hhAgg: Household.Aggregates                     = householdAggregates
    lazy val monetaryAgg: Option[Banking.MonetaryAggregates] = Some(
      Banking.MonetaryAggregates.compute(banks, world.financial.nbfi.tfiAum, world.financial.corporateBonds.outstanding),
    )
    lazy val monthlyGdp: PLN                                 = world.cachedMonthlyGdpProxy
    lazy val sectorAuto: IndexedSeq[Double]                  = p.sectorDefs.indices.map { s =>
      val secFirms = living.filter(_.sector.toInt == s)
      if secFirms.isEmpty then 0.0
      else
        secFirms
          .count(f => f.tech.isInstanceOf[TechState.Automated] || f.tech.isInstanceOf[TechState.Hybrid])
          .toDouble / secFirms.length
    }

    inline def unemployPct: Double = td.toDouble(world.unemploymentRate(hhAgg.employed))

  // -------------------------------------------------------------------------
  //  Schema groups — composed with ++
  // -------------------------------------------------------------------------

  private def coreGroup: Vector[ColumnDef] = Vector(
    ColumnDef("Month", ctx => (ctx.t + 1).toDouble),
    ColumnDef("Inflation", ctx => td.toDouble(ctx.world.inflation)),
    ColumnDef("Unemployment", ctx => ctx.unemployPct),
    ColumnDef(
      "PermanentShare",
      ctx =>
        val total = ctx.hhAgg.employed.toDouble
        if total > 0 then ctx.households.count(h => h.contractType == ContractType.Permanent && h.status.isInstanceOf[HhStatus.Employed]).toDouble / total
        else 0.0,
    ),
    ColumnDef(
      "ZlecenieShare",
      ctx =>
        val total = ctx.hhAgg.employed.toDouble
        if total > 0 then ctx.households.count(h => h.contractType == ContractType.Zlecenie && h.status.isInstanceOf[HhStatus.Employed]).toDouble / total
        else 0.0,
    ),
    ColumnDef(
      "B2BShare",
      ctx =>
        val total = ctx.hhAgg.employed.toDouble
        if total > 0 then ctx.households.count(h => h.contractType == ContractType.B2B && h.status.isInstanceOf[HhStatus.Employed]).toDouble / total else 0.0,
    ),
    ColumnDef("TotalAdoption", ctx => td.toDouble(ctx.world.real.automationRatio + ctx.world.real.hybridRatio)),
    ColumnDef("ExRate", ctx => exchangeRateValue(ctx.world.forex.exchangeRate)),
    ColumnDef("MarketWage", ctx => td.toDouble(ctx.world.householdMarket.marketWage)),
    ColumnDef("GovDebt", ctx => td.toDouble(ctx.world.gov.cumulativeDebt)),
    ColumnDef("NPL", ctx => td.toDouble(ctx.bankAgg.nplRatio)),
    ColumnDef("RefRate", ctx => td.toDouble(ctx.world.nbp.referenceRate)),
    ColumnDef("PriceLevel", ctx => ctx.world.priceLevel),
    ColumnDef("AutoRatio", ctx => td.toDouble(ctx.world.real.automationRatio)),
    ColumnDef("HybridRatio", ctx => td.toDouble(ctx.world.real.hybridRatio)),
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
    ColumnDef("BPO_Sigma", ctx => td.toDouble(ctx.world.currentSigmas(0))),
    ColumnDef("Manuf_Sigma", ctx => td.toDouble(ctx.world.currentSigmas(1))),
    ColumnDef("Retail_Sigma", ctx => td.toDouble(ctx.world.currentSigmas(2))),
    ColumnDef("Health_Sigma", ctx => td.toDouble(ctx.world.currentSigmas(3))),
    ColumnDef("Public_Sigma", ctx => td.toDouble(ctx.world.currentSigmas(4))),
    ColumnDef("Agri_Sigma", ctx => td.toDouble(ctx.world.currentSigmas(5))),
    ColumnDef("MeanDegree", ctx => ctx.firms.map(_.neighbors.length.toDouble).sum / ctx.firms.length),
    ColumnDef("IoFlows", ctx => td.toDouble(ctx.world.flows.ioFlows)),
    ColumnDef(
      "IoGdpRatio",
      ctx => if ctx.monthlyGdp > PLN.Zero then td.toDouble(ctx.world.flows.ioFlows) / td.toDouble(ctx.monthlyGdp) else 0.0,
    ),
  )

  private def externalGroup: Vector[ColumnDef] = Vector(
    ColumnDef("NFA", ctx => td.toDouble(ctx.world.bop.nfa)),
    ColumnDef("CurrentAccount", ctx => td.toDouble(ctx.world.bop.currentAccount)),
    ColumnDef("CapitalAccount", ctx => td.toDouble(ctx.world.bop.capitalAccount)),
    ColumnDef("TradeBalance_OE", ctx => td.toDouble(ctx.world.bop.tradeBalance)),
    ColumnDef("Exports_OE", ctx => td.toDouble(ctx.world.bop.exports)),
    ColumnDef("TotalImports_OE", ctx => td.toDouble(ctx.world.bop.totalImports)),
    ColumnDef("ImportedInterm", ctx => td.toDouble(ctx.world.bop.importedIntermediates)),
    ColumnDef("FDI", ctx => td.toDouble(ctx.world.bop.fdi)),
    // GVC / Deep External Sector
    ColumnDef("GvcDisruptionIndex", ctx => td.toDouble(ctx.world.external.gvc.disruptionIndex)),
    ColumnDef("ForeignPriceIndex", ctx => td.toDouble(ctx.world.external.gvc.foreignPriceIndex)),
    ColumnDef("GvcTradeConcentration", ctx => td.toDouble(ctx.world.external.gvc.tradeConcentration)),
    ColumnDef("GvcExportDemandShock", ctx => td.toDouble(ctx.world.external.gvc.exportDemandShockMag)),
    ColumnDef("GvcImportCostIndex", ctx => td.toDouble(ctx.world.external.gvc.importCostIndex)),
    ColumnDef("CommodityPriceIndex", ctx => td.toDouble(ctx.world.external.gvc.commodityPriceIndex)),
    // Immigration
    ColumnDef("ImmigrantStock", ctx => ctx.world.external.immigration.immigrantStock.toDouble),
    ColumnDef("MonthlyImmigInflow", ctx => ctx.world.external.immigration.monthlyInflow.toDouble),
    ColumnDef("RemittanceOutflow", ctx => td.toDouble(ctx.world.external.immigration.remittanceOutflow)),
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
    ColumnDef("FxReserves", ctx => td.toDouble(ctx.world.nbp.fxReserves)),
    ColumnDef("FxInterventionAmt", ctx => td.toDouble(ctx.world.nbp.lastFxTraded)),
    ColumnDef("FxInterventionActive", _ => 1.0),
    // Diaspora Remittances
    ColumnDef("DiasporaRemittanceInflow", ctx => td.toDouble(ctx.world.flows.diasporaRemittanceInflow)),
    ColumnDef(
      "NetRemittances",
      ctx => td.toDouble(ctx.world.flows.diasporaRemittanceInflow - ctx.world.external.immigration.remittanceOutflow),
    ),
    // Tourism
    ColumnDef("TourismExport", ctx => td.toDouble(ctx.world.flows.tourismExport)),
    ColumnDef("TourismImport", ctx => td.toDouble(ctx.world.flows.tourismImport)),
    ColumnDef("NetTourismBalance", ctx => td.toDouble(ctx.world.flows.tourismExport - ctx.world.flows.tourismImport)),
    ColumnDef("TourismSeasonalFactor", ctx => ctx.world.external.tourismSeasonalFactor),
  )

  private def fiscalGroup: Vector[ColumnDef] = Vector(
    ColumnDef("UnempBenefitSpend", ctx => td.toDouble(ctx.world.gov.unempBenefitSpend)),
    ColumnDef(
      "OutputGap",
      ctx => (ctx.unemployPct - td.toDouble(ctx.p.monetary.nairu)) / td.toDouble(ctx.p.monetary.nairu),
    ),
    ColumnDef(
      "EffectivePitRate",
      ctx => {
        val agg   = ctx.hhAgg
        val gross = agg.totalIncome + agg.totalPit
        if gross > PLN.Zero then agg.totalPit / gross else 0.0
      },
    ),
    ColumnDef("SocialTransferSpend", ctx => td.toDouble(ctx.world.gov.socialTransferSpend)),
    ColumnDef("GovCurrentSpend", ctx => td.toDouble(ctx.world.gov.govCurrentSpend)),
    ColumnDef("GovCapitalSpendDomestic", ctx => td.toDouble(ctx.world.gov.govCapitalSpend)),
    ColumnDef("GovDomesticBudgetDemand", ctx => td.toDouble(ctx.world.gov.domesticBudgetDemand)),
    ColumnDef("GovDomesticBudgetOutlays", ctx => td.toDouble(ctx.world.gov.domesticBudgetOutlays)),
    ColumnDef("EuProjectCapitalTotal", ctx => td.toDouble(ctx.world.gov.euProjectCapital)),
    ColumnDef("PublicCapitalStock", ctx => td.toDouble(ctx.world.gov.publicCapitalStock)),
    ColumnDef("EuCofinancingDomestic", ctx => td.toDouble(ctx.world.gov.euCofinancing)),
    ColumnDef("EuFundsMonthly", ctx => td.toDouble(ctx.world.bop.euFundsMonthly)),
    ColumnDef("EuCumulativeAbsorption", ctx => td.toDouble(ctx.world.bop.euCumulativeAbsorption)),
    ColumnDef("MinWageLevel", ctx => td.toDouble(ctx.world.gov.minWageLevel)),
    ColumnDef("ExciseRevenue", ctx => td.toDouble(ctx.world.gov.exciseRevenue)),
    ColumnDef("CustomsDutyRevenue", ctx => td.toDouble(ctx.world.gov.customsDutyRevenue)),
    // Fiscal rules
    ColumnDef(
      "DebtToGdp",
      ctx => if ctx.monthlyGdp > PLN.Zero then td.toDouble(ctx.world.gov.cumulativeDebt) / (td.toDouble(ctx.monthlyGdp) * 12.0) else 0.0,
    ),
    ColumnDef(
      "DeficitToGdp",
      ctx => if ctx.monthlyGdp > PLN.Zero then td.toDouble(ctx.world.gov.deficit) / (td.toDouble(ctx.monthlyGdp) * 12.0) else 0.0,
    ),
    ColumnDef("FiscalRuleBinding", ctx => ctx.world.pipeline.fiscalRuleSeverity.toDouble),
    ColumnDef("GovSpendingCutRatio", ctx => td.toDouble(ctx.world.pipeline.govSpendingCutRatio)),
  )

  private def monetaryGroup: Vector[ColumnDef] = Vector(
    // Bond market
    ColumnDef("BondYield", ctx => td.toDouble(ctx.world.gov.bondYield)),
    ColumnDef("WeightedCoupon", ctx => td.toDouble(ctx.world.gov.weightedCoupon)),
    ColumnDef("BondsOutstanding", ctx => td.toDouble(ctx.world.gov.bondsOutstanding)),
    ColumnDef("BankBondHoldings", ctx => td.toDouble(ctx.bankAgg.govBondHoldings)),
    ColumnDef("ForeignBondHoldings", ctx => td.toDouble(ctx.world.gov.foreignBondHoldings)),
    ColumnDef("NbpBondHoldings", ctx => td.toDouble(ctx.world.nbp.govBondHoldings)),
    ColumnDef("QeActive", ctx => if ctx.world.nbp.qeActive then 1.0 else 0.0),
    ColumnDef("DebtService", ctx => td.toDouble(ctx.world.gov.debtServiceSpend)),
    ColumnDef("NbpRemittance", ctx => td.toDouble(ctx.world.nbp.govBondHoldings) * td.toDouble(ctx.world.gov.bondYield.monthly)),
    // Monetary plumbing
    ColumnDef("ReserveInterest", ctx => td.toDouble(ctx.world.plumbing.reserveInterestTotal)),
    ColumnDef("StandingFacilityNet", ctx => td.toDouble(ctx.world.plumbing.standingFacilityNet)),
    ColumnDef("DepositFacilityUsage", ctx => td.toDouble(ctx.world.plumbing.depositFacilityUsage)),
    ColumnDef("InterbankInterestNet", ctx => td.toDouble(ctx.world.plumbing.interbankInterestNet)),
    // Monetary aggregates
    ColumnDef("M0", ctx => ctx.monetaryAgg.map(a => td.toDouble(a.m0)).getOrElse(0.0)),
    ColumnDef("M1", ctx => ctx.monetaryAgg.map(a => td.toDouble(a.m1)).getOrElse(td.toDouble(ctx.bankAgg.deposits))),
    ColumnDef("M2", ctx => ctx.monetaryAgg.map(a => td.toDouble(a.m2)).getOrElse(td.toDouble(ctx.bankAgg.deposits))),
    ColumnDef("M3", ctx => ctx.monetaryAgg.map(a => td.toDouble(a.m3)).getOrElse(td.toDouble(ctx.bankAgg.deposits))),
    ColumnDef("CreditMultiplier", ctx => ctx.monetaryAgg.map(a => td.toDouble(a.creditMultiplier)).getOrElse(0.0)),
    ColumnDef("FofResidual", ctx => td.toDouble(ctx.world.plumbing.fofResidual)),
  )

  private def financialGroup: Vector[ColumnDef] = Vector(
    // Interbank
    ColumnDef("InterbankRate", ctx => td.toDouble(ctx.world.bankingSector.interbankRate)),
    ColumnDef("MinBankCAR", ctx => if ctx.aliveBanks.isEmpty then 0.0 else td.toDouble(ctx.aliveBanks.map(_.car).min)),
    ColumnDef("MaxBankNPL", ctx => if ctx.aliveBanks.isEmpty then 0.0 else td.toDouble(ctx.aliveBanks.map(_.nplRatio).max)),
    ColumnDef("BankFailures", ctx => ctx.banks.count(_.failed).toDouble),
    // LCR/NSFR
    ColumnDef(
      "MinBankLCR",
      ctx => { given SimParams = ctx.p; if ctx.aliveBanks.isEmpty then 0.0 else td.toDouble(ctx.aliveBanks.map(_.lcr).min) },
    ),
    ColumnDef("MinBankNSFR", ctx => if ctx.aliveBanks.isEmpty then 0.0 else td.toDouble(ctx.aliveBanks.map(_.nsfr).min)),
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
    ColumnDef("WIBOR_1M", ctx => ctx.world.bankingSector.interbankCurve.map(c => td.toDouble(c.wibor1m)).getOrElse(0.0)),
    ColumnDef("WIBOR_3M", ctx => ctx.world.bankingSector.interbankCurve.map(c => td.toDouble(c.wibor3m)).getOrElse(0.0)),
    ColumnDef("WIBOR_6M", ctx => ctx.world.bankingSector.interbankCurve.map(c => td.toDouble(c.wibor6m)).getOrElse(0.0)),
    // Consumer Credit
    ColumnDef("ConsumerLoans", ctx => td.toDouble(ctx.bankAgg.consumerLoans)),
    ColumnDef(
      "ConsumerNplRatio",
      ctx =>
        if ctx.bankAgg.consumerLoans > PLN.Zero then ctx.bankAgg.consumerNpl / ctx.bankAgg.consumerLoans
        else 0.0,
    ),
    ColumnDef("ConsumerOrigination", ctx => td.toDouble(ctx.hhAgg.totalConsumerOrigination)),
    ColumnDef("ConsumerDebtService", ctx => td.toDouble(ctx.hhAgg.totalConsumerDebtService)),
    // GPW Equity Market
    ColumnDef("GpwIndex", ctx => ctx.world.financial.equity.index),
    ColumnDef("GpwMarketCap", ctx => td.toDouble(ctx.world.financial.equity.marketCap)),
    ColumnDef(
      "GpwPE",
      ctx => { val ey = td.toDouble(ctx.world.financial.equity.earningsYield); if ey > 0 then 1.0 / ey else 0.0 },
    ),
    ColumnDef("GpwDivYield", ctx => td.toDouble(ctx.world.financial.equity.dividendYield)),
    ColumnDef("EquityIssuanceTotal", ctx => td.toDouble(ctx.world.financial.equity.lastIssuance)),
    ColumnDef(
      "EquityFinancedFrac",
      ctx =>
        ctx.living.map(f => td.toDouble(f.equityRaised)).sum / Math.max(
          1.0,
          ctx.living.map(f => td.toDouble(f.debt + f.equityRaised)).sum,
        ),
    ),
    ColumnDef("HhEquityWealth", ctx => td.toDouble(ctx.world.financial.equity.hhEquityWealth)),
    ColumnDef("EquityWealthEffect", ctx => td.toDouble(ctx.world.financial.equity.lastWealthEffect)),
    ColumnDef("DomesticDividends", ctx => td.toDouble(ctx.world.financial.equity.lastDomesticDividends)),
    ColumnDef("ForeignDividendOutflow", ctx => td.toDouble(ctx.world.financial.equity.lastForeignDividends)),
    // Corporate Bonds / Catalyst
    ColumnDef("CorpBondOutstanding", ctx => td.toDouble(ctx.world.financial.corporateBonds.outstanding)),
    ColumnDef("CorpBondYield", ctx => td.toDouble(ctx.world.financial.corporateBonds.corpBondYield)),
    ColumnDef("CorpBondIssuance", ctx => td.toDouble(ctx.world.financial.corporateBonds.lastIssuance)),
    ColumnDef("CorpBondSpread", ctx => td.toDouble(ctx.world.financial.corporateBonds.creditSpread)),
    ColumnDef("BankCorpBondHoldings", ctx => td.toDouble(ctx.world.financial.corporateBonds.bankHoldings)),
    ColumnDef("PpkCorpBondHoldings", ctx => td.toDouble(ctx.world.financial.corporateBonds.ppkHoldings)),
    ColumnDef("CorpBondAbsorptionRate", ctx => td.toDouble(ctx.world.financial.corporateBonds.lastAbsorptionRate)),
    // Insurance Sector
    ColumnDef("InsLifeReserves", ctx => td.toDouble(ctx.world.financial.insurance.lifeReserves)),
    ColumnDef("InsNonLifeReserves", ctx => td.toDouble(ctx.world.financial.insurance.nonLifeReserves)),
    ColumnDef("InsGovBondHoldings", ctx => td.toDouble(ctx.world.financial.insurance.govBondHoldings)),
    ColumnDef("InsLifePremium", ctx => td.toDouble(ctx.world.financial.insurance.lastLifePremium)),
    ColumnDef("InsNonLifePremium", ctx => td.toDouble(ctx.world.financial.insurance.lastNonLifePremium)),
    ColumnDef("InsLifeClaims", ctx => td.toDouble(ctx.world.financial.insurance.lastLifeClaims)),
    ColumnDef("InsNonLifeClaims", ctx => td.toDouble(ctx.world.financial.insurance.lastNonLifeClaims)),
    // Shadow Banking / NBFI
    ColumnDef("NbfiTfiAum", ctx => td.toDouble(ctx.world.financial.nbfi.tfiAum)),
    ColumnDef("NbfiTfiGovBondHoldings", ctx => td.toDouble(ctx.world.financial.nbfi.tfiGovBondHoldings)),
    ColumnDef("NbfiLoanStock", ctx => td.toDouble(ctx.world.financial.nbfi.nbfiLoanStock)),
    ColumnDef("NbfiOrigination", ctx => td.toDouble(ctx.world.financial.nbfi.lastNbfiOrigination)),
    ColumnDef("NbfiDefaults", ctx => td.toDouble(ctx.world.financial.nbfi.lastNbfiDefaultAmount)),
    ColumnDef("NbfiBankTightness", ctx => td.toDouble(ctx.world.financial.nbfi.lastBankTightness)),
    // Quasi-fiscal (BGK/PFR)
    ColumnDef("QfBondsOutstanding", ctx => td.toDouble(ctx.world.financial.quasiFiscal.bondsOutstanding)),
    ColumnDef("QfNbpHoldings", ctx => td.toDouble(ctx.world.financial.quasiFiscal.nbpHoldings)),
    ColumnDef("QfLoanPortfolio", ctx => td.toDouble(ctx.world.financial.quasiFiscal.loanPortfolio)),
    ColumnDef("QfIssuance", ctx => td.toDouble(ctx.world.financial.quasiFiscal.monthlyIssuance)),
    ColumnDef(
      "Esa2010DebtToGdp",
      ctx =>
        val annualGdp = td.toDouble(ctx.monthlyGdp) * 12.0
        if annualGdp > 0 then td.toDouble(QuasiFiscal.esa2010Debt(ctx.world.gov.cumulativeDebt, ctx.world.financial.quasiFiscal.bondsOutstanding)) / annualGdp
        else 0.0,
    ),
    ColumnDef("NbfiDepositDrain", ctx => td.toDouble(ctx.world.financial.nbfi.lastDepositDrain)),
    // AFS/HTM bond portfolio split
    ColumnDef("BankAfsBonds", ctx => td.toDouble(ctx.bankAgg.afsBonds)),
    ColumnDef("BankHtmBonds", ctx => td.toDouble(ctx.bankAgg.htmBonds)),
    // IFRS 9 ECL staging (aggregate across banks)
    ColumnDef("EclStage1", ctx => ctx.banks.map(b => td.toDouble(b.eclStaging.stage1)).sum),
    ColumnDef("EclStage2", ctx => ctx.banks.map(b => td.toDouble(b.eclStaging.stage2)).sum),
    ColumnDef("EclStage3", ctx => ctx.banks.map(b => td.toDouble(b.eclStaging.stage3)).sum),
    // KNF/BFG
    ColumnDef("BfgLevyTotal", ctx => ctx.world.flows.bfgLevyTotal),
    ColumnDef("BfgFundBalance", ctx => td.toDouble(ctx.world.mechanisms.bfgFundBalance)),
    ColumnDef("BailInLoss", ctx => td.toDouble(ctx.world.flows.bailInLoss)),
  )

  private def realGroup: Vector[ColumnDef] = Vector(
    // Housing Market
    ColumnDef("HousingPriceIndex", ctx => ctx.world.real.housing.priceIndex),
    ColumnDef("HousingMarketValue", ctx => td.toDouble(ctx.world.real.housing.totalValue)),
    ColumnDef("MortgageStock", ctx => td.toDouble(ctx.world.real.housing.mortgageStock)),
    ColumnDef("AvgMortgageRate", ctx => td.toDouble(ctx.world.real.housing.avgMortgageRate)),
    ColumnDef("MortgageOrigination", ctx => td.toDouble(ctx.world.real.housing.lastOrigination)),
    ColumnDef("MortgageRepayment", ctx => td.toDouble(ctx.world.real.housing.lastRepayment)),
    ColumnDef("MortgageDefault", ctx => td.toDouble(ctx.world.real.housing.lastDefault)),
    ColumnDef("MortgageInterestIncome", ctx => td.toDouble(ctx.world.real.housing.mortgageInterestIncome)),
    ColumnDef("HhHousingWealth", ctx => td.toDouble(ctx.world.real.housing.hhHousingWealth)),
    ColumnDef("HousingWealthEffect", ctx => td.toDouble(ctx.world.real.housing.lastWealthEffect)),
    ColumnDef(
      "MortgageToGdp",
      ctx =>
        if ctx.monthlyGdp > PLN.Zero && ctx.world.real.housing.mortgageStock > PLN.Zero
        then td.toDouble(ctx.world.real.housing.mortgageStock) / (td.toDouble(ctx.monthlyGdp) * 12.0)
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
    ColumnDef("SectorMobilityRate", ctx => td.toDouble(ctx.world.real.sectoralMobility.sectorMobilityRate)),
    ColumnDef("CrossSectorHires", ctx => ctx.world.real.sectoralMobility.crossSectorHires.toDouble),
    ColumnDef("VoluntaryQuits", ctx => ctx.world.real.sectoralMobility.voluntaryQuits.toDouble),
    // Physical Capital
    ColumnDef("AggCapitalStock", ctx => ctx.living.map(f => td.toDouble(f.capitalStock)).sum),
    ColumnDef("GrossInvestment", ctx => td.toDouble(ctx.world.real.grossInvestment)),
    ColumnDef("CapitalDepreciation", ctx => ctx.living.map(f => td.toDouble(f.capitalStock) * td.toDouble(ctx.p.capital.depRates(f.sector.toInt).monthly)).sum),
    // Inventories
    ColumnDef("AggInventoryStock", ctx => td.toDouble(ctx.world.flows.aggInventoryStock)),
    ColumnDef("InventoryChange", ctx => td.toDouble(ctx.world.flows.aggInventoryChange)),
    ColumnDef(
      "InventoryToGdp",
      ctx => if ctx.monthlyGdp > PLN.Zero then td.toDouble(ctx.world.flows.aggInventoryStock) / td.toDouble(ctx.monthlyGdp) else 0.0,
    ),
    // Energy / Climate
    ColumnDef("AggEnergyCost", ctx => td.toDouble(ctx.world.flows.aggEnergyCost)),
    ColumnDef(
      "EnergyCostToGdp",
      ctx => if ctx.monthlyGdp > PLN.Zero then td.toDouble(ctx.world.flows.aggEnergyCost) / td.toDouble(ctx.monthlyGdp) else 0.0,
    ),
    ColumnDef("EtsPrice", ctx => ctx.world.real.etsPrice),
    ColumnDef("AggGreenCapital", ctx => td.toDouble(ctx.world.real.aggGreenCapital)),
    ColumnDef("GreenInvestment", ctx => td.toDouble(ctx.world.real.aggGreenInvestment)),
    ColumnDef(
      "GreenCapitalRatio",
      ctx => {
        val aggK = ctx.living.map(f => td.toDouble(f.capitalStock)).sum
        if ctx.world.real.aggGreenCapital > PLN.Zero && aggK > 0 then td.toDouble(ctx.world.real.aggGreenCapital) / aggK else 0.0
      },
    ),
  )

  private def socialGroup: Vector[ColumnDef] = Vector(
    // JST
    ColumnDef("JstRevenue", ctx => td.toDouble(ctx.world.social.jst.revenue)),
    ColumnDef("JstSpending", ctx => td.toDouble(ctx.world.social.jst.spending)),
    ColumnDef("JstDebt", ctx => td.toDouble(ctx.world.social.jst.debt)),
    ColumnDef("JstDeposits", ctx => td.toDouble(ctx.world.social.jst.deposits)),
    ColumnDef("JstDeficit", ctx => td.toDouble(ctx.world.social.jst.deficit)),
    // ZUS/PPK
    ColumnDef("ZusContributions", ctx => td.toDouble(ctx.world.social.zus.contributions)),
    ColumnDef("ZusPensionPayments", ctx => td.toDouble(ctx.world.social.zus.pensionPayments)),
    ColumnDef("ZusGovSubvention", ctx => td.toDouble(ctx.world.social.zus.govSubvention)),
    ColumnDef("FusBalance", ctx => td.toDouble(ctx.world.social.zus.fusBalance)),
    ColumnDef("NfzContributions", ctx => td.toDouble(ctx.world.social.nfz.contributions)),
    ColumnDef("NfzSpending", ctx => td.toDouble(ctx.world.social.nfz.spending)),
    ColumnDef("NfzBalance", ctx => td.toDouble(ctx.world.social.nfz.balance)),
    ColumnDef("NfzGovSubvention", ctx => td.toDouble(ctx.world.social.nfz.govSubvention)),
    ColumnDef("PpkContributions", ctx => td.toDouble(ctx.world.social.ppk.contributions)),
    ColumnDef("PpkBondHoldings", ctx => td.toDouble(ctx.world.social.ppk.bondHoldings)),
    ColumnDef("NRetirees", ctx => ctx.world.social.demographics.retirees.toDouble),
    ColumnDef("WorkingAgePop", ctx => ctx.world.social.demographics.workingAgePop.toDouble),
    ColumnDef("MonthlyRetirements", ctx => ctx.world.social.demographics.monthlyRetirements.toDouble),
    // Earmarked funds (FP, PFRON, FGŚP)
    ColumnDef("FpBalance", ctx => td.toDouble(ctx.world.social.earmarked.fpBalance)),
    ColumnDef("FpContributions", ctx => td.toDouble(ctx.world.social.earmarked.fpContributions)),
    ColumnDef("PfronBalance", ctx => td.toDouble(ctx.world.social.earmarked.pfronBalance)),
    ColumnDef("FgspBalance", ctx => td.toDouble(ctx.world.social.earmarked.fgspBalance)),
    ColumnDef("FgspSpending", ctx => td.toDouble(ctx.world.social.earmarked.fgspSpending)),
    ColumnDef("EarmarkedGovSubvention", ctx => td.toDouble(ctx.world.social.earmarked.totalGovSubvention)),
    // Forward-Looking Expectations
    ColumnDef("ExpectedInflation", ctx => td.toDouble(ctx.world.mechanisms.expectations.expectedInflation)),
    ColumnDef("NbpCredibility", ctx => td.toDouble(ctx.world.mechanisms.expectations.credibility)),
    ColumnDef("ForwardGuidanceRate", ctx => td.toDouble(ctx.world.mechanisms.expectations.forwardGuidanceRate)),
    ColumnDef("InflationForecastError", ctx => td.toDouble(ctx.world.mechanisms.expectations.forecastError)),
  )

  private def mechanismsGroup: Vector[ColumnDef] = Vector(
    // Macroprudential
    ColumnDef("CCyB", ctx => td.toDouble(ctx.world.mechanisms.macropru.ccyb)),
    ColumnDef("CreditToGdpGap", ctx => td.toDouble(ctx.world.mechanisms.macropru.creditToGdpGap)),
    ColumnDef(
      "EffectiveMinCar",
      ctx =>
        if ctx.aliveBanks.isEmpty then 0.0
        else {
          given SimParams = ctx.p;
          td.toDouble(ctx.aliveBanks.map(b => Macroprudential.effectiveMinCar(b.id.toInt, ctx.world.mechanisms.macropru.ccyb)).max)
        },
    ),
    // FDI Composition
    ColumnDef("FdiProfitShifting", ctx => td.toDouble(ctx.world.flows.fdiProfitShifting)),
    ColumnDef("FdiRepatriation", ctx => td.toDouble(ctx.world.flows.fdiRepatriation)),
    ColumnDef("FdiGrossOutflow", ctx => td.toDouble(ctx.world.flows.fdiProfitShifting + ctx.world.flows.fdiRepatriation)),
    ColumnDef(
      "ForeignOwnedFrac",
      ctx => if ctx.nLiving > 0 then ctx.living.count(_.foreignOwned).toDouble / ctx.nLiving else 0.0,
    ),
    ColumnDef("FdiCitLoss", ctx => td.toDouble(ctx.world.flows.fdiCitLoss)),
    // Endogenous Firm Entry
    ColumnDef("FirmBirths", ctx => ctx.world.flows.firmBirths.toDouble),
    ColumnDef("FirmDeaths", ctx => ctx.world.flows.firmDeaths.toDouble),
    ColumnDef("NetEntry", ctx => (ctx.world.flows.firmBirths - ctx.world.flows.firmDeaths).toDouble),
    ColumnDef("LivingFirmCount", ctx => ctx.nLiving),
    ColumnDef("NetFirmBirths", ctx => ctx.world.flows.netFirmBirths.toDouble),
    ColumnDef("TotalFirmCount", ctx => ctx.firms.length.toDouble),
    // Informal Economy
    ColumnDef("RealizedTaxShadowShare", ctx => ctx.world.flows.realizedTaxShadowShare),
    ColumnDef("NextTaxShadowShare", ctx => ctx.world.mechanisms.nextTaxShadowShare),
    ColumnDef("TaxEvasionLoss", ctx => td.toDouble(ctx.world.flows.taxEvasionLoss)),
    ColumnDef(
      "EvasionToGdpRatio",
      ctx => if ctx.monthlyGdp > PLN.Zero then td.toDouble(ctx.world.flows.taxEvasionLoss) / td.toDouble(ctx.monthlyGdp) else 0.0,
    ),
  )

  /** Regional unemployment rates per NUTS-1 macroregion. */
  private def regionalGroup: Vector[ColumnDef] =
    def regionUnemp(region: Region, label: String): ColumnDef =
      ColumnDef(
        s"Unemp_$label",
        ctx =>
          val regHh = ctx.households.filter(_.region == region)
          if regHh.isEmpty then 0.0
          else regHh.count(h => !h.status.isInstanceOf[HhStatus.Employed]).toDouble / regHh.length,
      )
    Vector(
      regionUnemp(Region.Central, "Central"),
      regionUnemp(Region.South, "South"),
      regionUnemp(Region.East, "East"),
      regionUnemp(Region.Northwest, "Northwest"),
      regionUnemp(Region.Southwest, "Southwest"),
      regionUnemp(Region.North, "North"),
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
      ++ mechanismsGroup
      ++ regionalGroup).toArray

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
    val ForeignBondHoldings: Col    = lookup("ForeignBondHoldings")
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
      banks: Vector[Banking.BankState],
      householdAggregates: Household.Aggregates,
  )(using p: SimParams): Array[Double] =
    val living     = firms.filter(Firm.isAlive)
    val aliveBanks = banks.filterNot(_.failed).toVector
    val ctx        = Ctx(t, world, firms, households, banks, householdAggregates, living, living.length.toDouble, aliveBanks, p)
    val result     = new Array[Double](schema.length)
    var i          = 0
    while i < schema.length do
      result(i) = schema(i).compute(ctx)
      i += 1
    result

  /** Typed row access: row.at(Col.Inflation) instead of row(1). */
  extension (row: Array[Double]) def at(c: Col): Double = row(c.ordinal)
