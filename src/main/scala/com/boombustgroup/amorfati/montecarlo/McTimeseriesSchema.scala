package com.boombustgroup.amorfati.montecarlo

import com.boombustgroup.amorfati.agents.*
import com.boombustgroup.amorfati.config.{HousingConfig, SimParams}
import com.boombustgroup.amorfati.engine.*
import com.boombustgroup.amorfati.engine.SimulationMonth.ExecutionMonth
import com.boombustgroup.amorfati.engine.ledger.{CorporateBondOwnership, LedgerFinancialState}
import com.boombustgroup.amorfati.engine.markets.HousingMarket
import com.boombustgroup.amorfati.engine.mechanisms.Macroprudential
import com.boombustgroup.amorfati.types.*
import com.boombustgroup.amorfati.agents.Region

/** Typed column schema for Monte Carlo timeseries output.
  *
  * Composable: each group is a `Vector[ColumnDef]`, all composed with `++`.
  * Column handles (`Col`) are derived by name lookup — no mutable counter.
  */
object McTimeseriesSchema:

  private case class SectorColumns(expectedSectorName: String, columnStem: String):
    def autoColName: String   = s"${columnStem}_Auto"
    def outputColName: String = s"${columnStem}_Output"
    def sigmaColName: String  = s"${columnStem}_Sigma"

  private val sectorSchemaPairs: Vector[(String, String)] =
    SimParams.SchemaSectors.map(schemaSector => schemaSector.name -> schemaSector.outputStem)

  require(
    sectorSchemaPairs.length == SimParams.SchemaSectorCount &&
      sectorSchemaPairs.map(_._1) == SimParams.SchemaSectorNames &&
      sectorSchemaPairs.map(_._2).distinct.length == sectorSchemaPairs.length,
    s"McTimeseriesSchema sector schema must define ${SimParams.SchemaSectorCount} unique (name, stem) pairs, got ${sectorSchemaPairs.mkString(", ")}",
  )

  private val sectorColumns: Vector[SectorColumns] =
    SimParams.SchemaSectors.map(schemaSector => SectorColumns(schemaSector.name, schemaSector.outputStem))

  // -------------------------------------------------------------------------
  //  ColumnDef + Ctx
  // -------------------------------------------------------------------------

  /** Column definition: name paired with its computation. */
  private final case class ColumnDef private (name: String, compute: Ctx => MetricValue)

  private object ColumnDef:
    def apply[A](name: String, compute: Ctx => A)(using encoder: MetricEncoder[A]): ColumnDef =
      new ColumnDef(name, ctx => encoder.encode(compute(ctx)))

  /** Shared pre-computed context (computed once per timestep). */
  private class Ctx(
      val executionMonth: ExecutionMonth,
      val world: World,
      val firms: Vector[Firm.State],
      val households: Vector[Household.State],
      val banks: Vector[Banking.BankState],
      val householdAggregates: Household.Aggregates,
      val ledgerFinancialState: LedgerFinancialState,
      val living: Vector[Firm.State],
      val nLiving: Int,
      val aliveBanks: Vector[Banking.BankState],
      val p: SimParams,
  ):
    require(
      world.currentSigmas.length == p.sectorDefs.length,
      s"McTimeseriesSchema requires world.currentSigmas to have ${p.sectorDefs.length} entries to match sectorDefs, got ${world.currentSigmas.length}",
    )

    given SimParams                                                                                 = p
    private val sectorIndexByName: Map[String, Int]                                                 = p.sectorDefs.iterator.map(_.name).zipWithIndex.map((name, idx) => name -> idx).toMap
    lazy val bankCorpBondHoldings: Banking.BankCorpBondHoldings                                     =
      Banking.bankCorpBondHoldingsFromVector(ledgerFinancialState.banks.map(_.corpBond))
    lazy val bankAgg: Banking.Aggregate                                                             =
      Banking.aggregateFromBankStocks(banks, ledgerFinancialState.banks.map(LedgerFinancialState.projectBankFinancialStocks), bankCorpBondHoldings)
    lazy val ledgerBankStocksById: Map[BankId, Banking.BankFinancialStocks]                         =
      banks.zip(ledgerFinancialState.banks).map((bank, balances) => bank.id -> LedgerFinancialState.projectBankFinancialStocks(balances)).toMap
    lazy val aliveBankRows: Vector[(Banking.BankState, Banking.BankFinancialStocks)]                =
      aliveBanks.map: bank =>
        val stocks = ledgerBankStocksById.getOrElse(bank.id, throw IllegalStateException(s"Missing ledger bank stocks for bank ${bank.id.toInt}"))
        bank -> stocks
    lazy val ledgerBankGovBondHoldings: PLN                                                         =
      ledgerFinancialState.banks.foldLeft(PLN.Zero)((acc, bank) => acc + bank.govBondAfs + bank.govBondHtm)
    lazy val ledgerHouseholdEquityWealth: PLN                                                       =
      ledgerFinancialState.households.foldLeft(PLN.Zero)((acc, household) => acc + household.equity)
    lazy val ledgerHouseholdMortgageStock: PLN                                                      =
      LedgerFinancialState.householdMortgageStock(ledgerFinancialState)
    lazy val ledgerFirmBalancesById: Map[FirmId, LedgerFinancialState.FirmBalances]                 =
      firms.zip(ledgerFinancialState.firms).map((firm, balances) => firm.id -> balances).toMap
    lazy val hhAgg: Household.Aggregates                                                            = householdAggregates
    lazy val monetaryAgg: Option[Banking.MonetaryAggregates]                                        = Some(
      Banking.MonetaryAggregates.computeFromBankStocks(
        banks,
        ledgerFinancialState.banks.map(LedgerFinancialState.projectBankFinancialStocks),
        ledgerFinancialState.funds.nbfi.tfiUnit,
        CorporateBondOwnership.issuerOutstanding(ledgerFinancialState),
      ),
    )
    lazy val monthlyGdp: PLN                                                                        = world.cachedMonthlyGdpProxy
    lazy val annualizedGdp: PLN                                                                     = monthlyGdp * 12
    lazy val sectorOutputs: Vector[PLN]                                                             =
      world.flows.sectorOutputs match
        case outputs if outputs.isEmpty                       => Vector.fill(p.sectorDefs.length)(PLN.Zero)
        case outputs if outputs.length == p.sectorDefs.length => outputs
        case outputs                                          =>
          throw IllegalArgumentException(
            s"McTimeseriesSchema requires FlowState.sectorOutputs to be empty or have ${p.sectorDefs.length} entries, got ${outputs.length}",
          )
    lazy val sectorAuto: Vector[Share]                                                              = sectorColumns.map { sector =>
      val sectorIdx = sectorIndexByName(sector.expectedSectorName)
      val secFirms  = living.filter(_.sector.toInt == sectorIdx)
      if secFirms.isEmpty then Share.Zero
      else Share.fraction(secFirms.count(f => f.tech.isInstanceOf[TechState.Automated] || f.tech.isInstanceOf[TechState.Hybrid]), secFirms.length)
    }
    lazy val housingRegionsByMarket: Map[HousingConfig.RegionalMarket, HousingMarket.RegionalState] =
      world.real.housing.regions
        .map(_.iterator.map(state => state.market -> state).toMap)
        .getOrElse(Map.empty)

    def sectorSigma(idx: Int): Sigma                                        = world.currentSigmas(idx)
    def housingRegionHpi(market: HousingConfig.RegionalMarket): MetricValue =
      housingRegionsByMarket.get(market).map(regionState => MetricValue.fromRaw(regionState.priceIndex.toLong)).getOrElse(MetricValue.Zero)

    inline def unemployPct: Share = world.unemploymentRate(hhAgg.employed)

  // -------------------------------------------------------------------------
  //  Schema groups — composed with ++
  // -------------------------------------------------------------------------

  private def coreGroup: Vector[ColumnDef] = Vector(
    ColumnDef("Month", ctx => ctx.executionMonth.toInt),
    ColumnDef("Inflation", ctx => ctx.world.inflation),
    ColumnDef("Unemployment", ctx => ctx.unemployPct),
    ColumnDef(
      "PermanentShare",
      ctx =>
        if ctx.hhAgg.employed > 0 then
          Share.fraction(ctx.households.count(h => h.contractType == ContractType.Permanent && h.status.isInstanceOf[HhStatus.Employed]), ctx.hhAgg.employed)
        else Share.Zero,
    ),
    ColumnDef(
      "ZlecenieShare",
      ctx =>
        if ctx.hhAgg.employed > 0 then
          Share.fraction(ctx.households.count(h => h.contractType == ContractType.Zlecenie && h.status.isInstanceOf[HhStatus.Employed]), ctx.hhAgg.employed)
        else Share.Zero,
    ),
    ColumnDef(
      "B2BShare",
      ctx =>
        if ctx.hhAgg.employed > 0 then
          Share.fraction(ctx.households.count(h => h.contractType == ContractType.B2B && h.status.isInstanceOf[HhStatus.Employed]), ctx.hhAgg.employed)
        else Share.Zero,
    ),
    ColumnDef("TotalAdoption", ctx => ctx.world.real.automationRatio + ctx.world.real.hybridRatio),
    ColumnDef("ExRate", ctx => ctx.world.forex.exchangeRate),
    ColumnDef("MarketWage", ctx => ctx.world.householdMarket.marketWage),
    ColumnDef("GovDebt", ctx => ctx.world.gov.cumulativeDebt),
    ColumnDef("NPL", ctx => ctx.bankAgg.nplRatio),
    ColumnDef("RefRate", ctx => ctx.world.nbp.referenceRate),
    ColumnDef("PriceLevel", ctx => ctx.world.priceLevel),
    ColumnDef("MonthlyGdpProxy", ctx => ctx.monthlyGdp),
    ColumnDef("AnnualizedGdpProxy", ctx => ctx.annualizedGdp),
    ColumnDef("AutoRatio", ctx => ctx.world.real.automationRatio),
    ColumnDef("HybridRatio", ctx => ctx.world.real.hybridRatio),
  )

  private def sectoralGroup: Vector[ColumnDef] =
    val autoColumns   = sectorColumns.zipWithIndex.map: (sector, idx) =>
      ColumnDef(sector.autoColName, ctx => ctx.sectorAuto(idx))
    val outputColumns = sectorColumns.zipWithIndex.map: (sector, idx) =>
      ColumnDef(sector.outputColName, ctx => ctx.sectorOutputs(idx))
    val sigmaColumns  = sectorColumns.zipWithIndex.map: (sector, idx) =>
      ColumnDef(sector.sigmaColName, ctx => ctx.sectorSigma(idx))

    (autoColumns ++ outputColumns ++ sigmaColumns ++ Vector(
      ColumnDef("MeanDegree", ctx => MetricValue.fraction(ctx.firms.map(_.neighbors.length).sum, ctx.firms.length)),
      ColumnDef("IoFlows", ctx => ctx.world.flows.ioFlows),
      ColumnDef(
        "IoGdpRatio",
        ctx => if ctx.monthlyGdp > PLN.Zero then ctx.world.flows.ioFlows / ctx.monthlyGdp else Scalar.Zero,
      ),
    )).toVector

  private def externalGroup: Vector[ColumnDef] = Vector(
    ColumnDef("NFA", ctx => ctx.world.bop.nfa),
    ColumnDef("CurrentAccount", ctx => ctx.world.bop.currentAccount),
    ColumnDef("CapitalAccount", ctx => ctx.world.bop.capitalAccount),
    ColumnDef("TradeBalance_OE", ctx => ctx.world.bop.tradeBalance),
    ColumnDef("Exports_OE", ctx => ctx.world.bop.exports),
    ColumnDef("TotalImports_OE", ctx => ctx.world.bop.totalImports),
    ColumnDef("ImportedInterm", ctx => ctx.world.bop.importedIntermediates),
    ColumnDef("FDI", ctx => ctx.world.bop.fdi),
    // GVC / Deep External Sector
    ColumnDef("GvcDisruptionIndex", ctx => ctx.world.external.gvc.disruptionIndex),
    ColumnDef("ForeignPriceIndex", ctx => ctx.world.external.gvc.foreignPriceIndex),
    ColumnDef("GvcTradeConcentration", ctx => ctx.world.external.gvc.tradeConcentration),
    ColumnDef("GvcExportDemandShock", ctx => ctx.world.external.gvc.exportDemandShockMag),
    ColumnDef("GvcImportCostIndex", ctx => ctx.world.external.gvc.importCostIndex),
    ColumnDef("CommodityPriceIndex", ctx => ctx.world.external.gvc.commodityPriceIndex),
    // Immigration
    ColumnDef("ImmigrantStock", ctx => ctx.world.external.immigration.immigrantStock),
    ColumnDef("MonthlyImmigInflow", ctx => ctx.world.external.immigration.monthlyInflow),
    ColumnDef("RemittanceOutflow", ctx => ctx.world.external.immigration.remittanceOutflow),
    ColumnDef(
      "ImmigrantUnempRate",
      ctx =>
        if ctx.world.external.immigration.immigrantStock > 0 then
          val immigrants = ctx.households.filter(_.isImmigrant)
          if immigrants.nonEmpty then Share.fraction(immigrants.count(h => !h.status.isInstanceOf[HhStatus.Employed]), immigrants.length)
          else Share.Zero
        else Share.Zero,
    ),
    // FX
    ColumnDef("FxReserves", ctx => ctx.ledgerFinancialState.nbp.foreignAssets),
    ColumnDef("FxInterventionAmt", ctx => ctx.world.nbp.lastFxTraded),
    ColumnDef("FxInterventionActive", _ => true),
    // Diaspora Remittances
    ColumnDef("DiasporaRemittanceInflow", ctx => ctx.world.flows.diasporaRemittanceInflow),
    ColumnDef(
      "NetRemittances",
      ctx => ctx.world.flows.diasporaRemittanceInflow - ctx.world.external.immigration.remittanceOutflow,
    ),
    // Tourism
    ColumnDef("TourismExport", ctx => ctx.world.flows.tourismExport),
    ColumnDef("TourismImport", ctx => ctx.world.flows.tourismImport),
    ColumnDef("NetTourismBalance", ctx => ctx.world.flows.tourismExport - ctx.world.flows.tourismImport),
    ColumnDef("TourismSeasonalFactor", ctx => ctx.world.external.tourismSeasonalFactor),
  )

  private def fiscalGroup: Vector[ColumnDef] = Vector(
    ColumnDef("UnempBenefitSpend", ctx => ctx.world.gov.unempBenefitSpend),
    ColumnDef(
      "OutputGap",
      ctx => (ctx.unemployPct - ctx.p.monetary.nairu) / ctx.p.monetary.nairu,
    ),
    ColumnDef(
      "EffectivePitRate",
      ctx => {
        val agg   = ctx.hhAgg
        val gross = agg.totalIncome + agg.totalPit
        if gross > PLN.Zero then agg.totalPit / gross else Scalar.Zero
      },
    ),
    ColumnDef("SocialTransferSpend", ctx => ctx.world.gov.socialTransferSpend),
    ColumnDef("GovCurrentSpend", ctx => ctx.world.gov.govCurrentSpend),
    ColumnDef("GovCapitalSpendDomestic", ctx => ctx.world.gov.govCapitalSpend),
    ColumnDef("GovDomesticBudgetDemand", ctx => ctx.world.gov.domesticBudgetDemand),
    ColumnDef("GovDomesticBudgetOutlays", ctx => ctx.world.gov.domesticBudgetOutlays),
    ColumnDef("EuProjectCapitalTotal", ctx => ctx.world.gov.euProjectCapital),
    ColumnDef("PublicCapitalStock", ctx => ctx.world.gov.publicCapitalStock),
    ColumnDef("EuCofinancingDomestic", ctx => ctx.world.gov.euCofinancing),
    ColumnDef("EuFundsMonthly", ctx => ctx.world.bop.euFundsMonthly),
    ColumnDef("EuCumulativeAbsorption", ctx => ctx.world.bop.euCumulativeAbsorption),
    ColumnDef("MinWageLevel", ctx => ctx.world.gov.minWageLevel),
    ColumnDef("ExciseRevenue", ctx => ctx.world.gov.exciseRevenue),
    ColumnDef("CustomsDutyRevenue", ctx => ctx.world.gov.customsDutyRevenue),
    // Fiscal rules
    ColumnDef(
      "DebtToGdp",
      ctx => if ctx.monthlyGdp > PLN.Zero then ctx.world.gov.cumulativeDebt / (ctx.monthlyGdp * 12) else Scalar.Zero,
    ),
    ColumnDef(
      "DeficitToGdp",
      ctx => if ctx.monthlyGdp > PLN.Zero then ctx.world.gov.deficit / (ctx.monthlyGdp * 12) else Scalar.Zero,
    ),
    ColumnDef("FiscalRuleBinding", ctx => ctx.world.pipeline.fiscalRuleSeverity),
    ColumnDef("GovSpendingCutRatio", ctx => ctx.world.pipeline.govSpendingCutRatio),
  )

  private def monetaryGroup: Vector[ColumnDef] = Vector(
    // Bond market
    ColumnDef("BondYield", ctx => ctx.world.gov.bondYield),
    ColumnDef("WeightedCoupon", ctx => ctx.world.gov.weightedCoupon),
    ColumnDef("BondsOutstanding", ctx => ctx.ledgerFinancialState.government.govBondOutstanding),
    ColumnDef("BankBondHoldings", ctx => ctx.ledgerBankGovBondHoldings),
    ColumnDef("ForeignBondHoldings", ctx => ctx.ledgerFinancialState.foreign.govBondHoldings),
    ColumnDef("NbpBondHoldings", ctx => ctx.ledgerFinancialState.nbp.govBondHoldings),
    ColumnDef("QeActive", ctx => ctx.world.nbp.qeActive),
    ColumnDef("DebtService", ctx => ctx.world.gov.debtServiceSpend),
    ColumnDef("NbpRemittance", ctx => ctx.ledgerFinancialState.nbp.govBondHoldings * ctx.world.gov.bondYield.monthly),
    // Monetary plumbing
    ColumnDef("ReserveInterest", ctx => ctx.world.plumbing.reserveInterestTotal),
    ColumnDef("StandingFacilityNet", ctx => ctx.world.plumbing.standingFacilityNet),
    ColumnDef("DepositFacilityUsage", ctx => ctx.world.plumbing.depositFacilityUsage),
    ColumnDef("InterbankInterestNet", ctx => ctx.world.plumbing.interbankInterestNet),
    // Monetary aggregates
    ColumnDef("M0", ctx => ctx.monetaryAgg.map(a => a.m0).getOrElse(PLN.Zero)),
    ColumnDef("M1", ctx => ctx.monetaryAgg.map(a => a.m1).getOrElse(ctx.bankAgg.deposits)),
    ColumnDef("M2", ctx => ctx.monetaryAgg.map(a => a.m2).getOrElse(ctx.bankAgg.deposits)),
    ColumnDef("M3", ctx => ctx.monetaryAgg.map(a => a.m3).getOrElse(ctx.bankAgg.deposits)),
    ColumnDef("CreditMultiplier", ctx => ctx.monetaryAgg.map(a => a.creditMultiplier).getOrElse(Multiplier.Zero)),
    ColumnDef("FofResidual", ctx => ctx.world.plumbing.fofResidual),
  )

  private def financialGroup: Vector[ColumnDef] = Vector(
    // Interbank
    ColumnDef("InterbankRate", ctx => ctx.world.bankingSector.interbankRate),
    ColumnDef(
      "MinBankCAR",
      ctx =>
        if ctx.aliveBankRows.isEmpty then Multiplier.Zero
        else ctx.aliveBankRows.map((bank, stocks) => Banking.car(bank, stocks, ctx.bankCorpBondHoldings(bank.id))).min,
    ),
    ColumnDef(
      "MaxBankNPL",
      ctx => if ctx.aliveBankRows.isEmpty then Share.Zero else ctx.aliveBankRows.map((bank, stocks) => Banking.nplRatio(bank, stocks)).max,
    ),
    ColumnDef("BankFailures", ctx => ctx.banks.count(_.failed)),
    // LCR/NSFR
    ColumnDef(
      "MinBankLCR",
      ctx => { given SimParams = ctx.p; if ctx.aliveBankRows.isEmpty then Multiplier.Zero else ctx.aliveBankRows.map((_, stocks) => Banking.lcr(stocks)).min },
    ),
    ColumnDef(
      "MinBankNSFR",
      ctx =>
        if ctx.aliveBankRows.isEmpty then Multiplier.Zero
        else ctx.aliveBankRows.map((bank, stocks) => Banking.nsfr(bank, stocks, ctx.bankCorpBondHoldings(bank.id))).min,
    ),
    ColumnDef(
      "AvgTermDepositFrac",
      ctx =>
        if ctx.aliveBankRows.isEmpty then Scalar.Zero
        else
          val total = ctx.aliveBankRows.foldLeft(Scalar.Zero): (acc, row) =>
            val (_, stocks) = row
            acc + (if stocks.totalDeposits > PLN.Zero then stocks.termDeposit / stocks.totalDeposits else Scalar.Zero)
          total / ctx.aliveBankRows.length,
    ),
    // Term structure
    ColumnDef("WIBOR_1M", ctx => ctx.world.bankingSector.interbankCurve.map(c => c.wibor1m).getOrElse(Rate.Zero)),
    ColumnDef("WIBOR_3M", ctx => ctx.world.bankingSector.interbankCurve.map(c => c.wibor3m).getOrElse(Rate.Zero)),
    ColumnDef("WIBOR_6M", ctx => ctx.world.bankingSector.interbankCurve.map(c => c.wibor6m).getOrElse(Rate.Zero)),
    // Consumer Credit
    ColumnDef("ConsumerLoans", ctx => ctx.bankAgg.consumerLoans),
    ColumnDef(
      "ConsumerNplRatio",
      ctx =>
        if ctx.bankAgg.consumerLoans > PLN.Zero then ctx.bankAgg.consumerNpl / ctx.bankAgg.consumerLoans
        else Scalar.Zero,
    ),
    ColumnDef("ConsumerOrigination", ctx => ctx.hhAgg.totalConsumerOrigination),
    ColumnDef("ConsumerDebtService", ctx => ctx.hhAgg.totalConsumerDebtService),
    // GPW Equity Market
    ColumnDef("GpwIndex", ctx => ctx.world.financialMarkets.equity.index),
    ColumnDef("GpwMarketCap", ctx => ctx.world.financialMarkets.equity.marketCap),
    ColumnDef(
      "GpwPE",
      ctx =>
        val ey = ctx.world.financialMarkets.equity.earningsYield
        if ey > Rate.Zero then ey.toScalar.reciprocal else Scalar.Zero,
    ),
    ColumnDef("GpwDivYield", ctx => ctx.world.financialMarkets.equity.dividendYield),
    ColumnDef("EquityIssuanceTotal", ctx => ctx.world.financialMarkets.equity.lastIssuance),
    ColumnDef(
      "EquityFinancedFrac",
      ctx =>
        val balances = ctx.living.flatMap(f => ctx.ledgerFirmBalancesById.get(f.id))
        val equity   = balances.foldLeft(PLN.Zero)((acc, b) => acc + b.equity)
        val funding  = balances.foldLeft(PLN.Zero)((acc, b) => acc + b.firmLoan + b.equity)
        if funding > PLN.Zero then equity / funding else Scalar.Zero,
    ),
    ColumnDef("HhEquityWealth", ctx => ctx.ledgerHouseholdEquityWealth),
    ColumnDef("EquityWealthEffect", ctx => ctx.world.financialMarkets.equity.lastWealthEffect),
    ColumnDef("DomesticDividends", ctx => ctx.world.financialMarkets.equity.lastDomesticDividends),
    ColumnDef("ForeignDividendOutflow", ctx => ctx.world.financialMarkets.equity.lastForeignDividends),
    ColumnDef("GovernmentDividends", ctx => ctx.world.gov.govDividendRevenue),
    // Corporate Bonds / Catalyst
    ColumnDef("CorpBondOutstanding", ctx => CorporateBondOwnership.issuerOutstanding(ctx.ledgerFinancialState)),
    ColumnDef("CorpBondYield", ctx => ctx.world.financialMarkets.corporateBonds.corpBondYield),
    ColumnDef("CorpBondIssuance", ctx => ctx.world.financialMarkets.corporateBonds.lastIssuance),
    ColumnDef("CorpBondSpread", ctx => ctx.world.financialMarkets.corporateBonds.creditSpread),
    ColumnDef("BankCorpBondHoldings", ctx => ctx.ledgerFinancialState.banks.foldLeft(PLN.Zero)((acc, bank) => acc + bank.corpBond)),
    ColumnDef("PpkCorpBondHoldings", ctx => ctx.ledgerFinancialState.funds.ppkCorpBondHoldings),
    ColumnDef("CorpBondAbsorptionRate", ctx => ctx.world.financialMarkets.corporateBonds.lastAbsorptionRate),
    // Insurance Sector
    ColumnDef("InsLifeReserves", ctx => ctx.ledgerFinancialState.insurance.lifeReserve),
    ColumnDef("InsNonLifeReserves", ctx => ctx.ledgerFinancialState.insurance.nonLifeReserve),
    ColumnDef("InsGovBondHoldings", ctx => ctx.ledgerFinancialState.insurance.govBondHoldings),
    ColumnDef("InsLifePremium", ctx => ctx.world.financialMarkets.insurance.lastLifePremium),
    ColumnDef("InsNonLifePremium", ctx => ctx.world.financialMarkets.insurance.lastNonLifePremium),
    ColumnDef("InsLifeClaims", ctx => ctx.world.financialMarkets.insurance.lastLifeClaims),
    ColumnDef("InsNonLifeClaims", ctx => ctx.world.financialMarkets.insurance.lastNonLifeClaims),
    // Shadow Banking / NBFI
    ColumnDef("NbfiTfiAum", ctx => ctx.ledgerFinancialState.funds.nbfi.tfiUnit),
    ColumnDef("NbfiTfiGovBondHoldings", ctx => ctx.ledgerFinancialState.funds.nbfi.govBondHoldings),
    ColumnDef("NbfiLoanStock", ctx => ctx.ledgerFinancialState.funds.nbfi.nbfiLoanStock),
    ColumnDef("NbfiOrigination", ctx => ctx.world.financialMarkets.nbfi.lastNbfiOrigination),
    ColumnDef("NbfiDefaults", ctx => ctx.world.financialMarkets.nbfi.lastNbfiDefaultAmount),
    ColumnDef("NbfiBankTightness", ctx => ctx.world.financialMarkets.nbfi.lastBankTightness),
    // Quasi-fiscal (BGK/PFR)
    ColumnDef("QfBondsOutstanding", ctx => ctx.ledgerFinancialState.funds.quasiFiscal.bondsOutstanding),
    ColumnDef("QfNbpHoldings", ctx => ctx.ledgerFinancialState.funds.quasiFiscal.nbpHoldings),
    ColumnDef("QfLoanPortfolio", ctx => ctx.ledgerFinancialState.funds.quasiFiscal.loanPortfolio),
    ColumnDef("QfIssuance", ctx => ctx.world.financialMarkets.quasiFiscal.monthlyIssuance),
    ColumnDef(
      "Esa2010DebtToGdp",
      ctx =>
        val annualGdp = ctx.monthlyGdp * 12
        if annualGdp > PLN.Zero then
          QuasiFiscal.esa2010Debt(ctx.world.gov.cumulativeDebt, ctx.ledgerFinancialState.funds.quasiFiscal.bondsOutstanding) / annualGdp
        else Scalar.Zero,
    ),
    ColumnDef("NbfiDepositDrain", ctx => ctx.world.financialMarkets.nbfi.lastDepositDrain),
    // AFS/HTM bond portfolio split
    ColumnDef("BankAfsBonds", ctx => ctx.bankAgg.afsBonds),
    ColumnDef("BankHtmBonds", ctx => ctx.bankAgg.htmBonds),
    // IFRS 9 ECL staging (aggregate across banks)
    ColumnDef("EclStage1", ctx => ctx.banks.map(b => b.eclStaging.stage1).sumPln),
    ColumnDef("EclStage2", ctx => ctx.banks.map(b => b.eclStaging.stage2).sumPln),
    ColumnDef("EclStage3", ctx => ctx.banks.map(b => b.eclStaging.stage3).sumPln),
    // KNF/BFG
    ColumnDef("BfgLevyTotal", ctx => ctx.world.flows.bfgLevyTotal),
    ColumnDef("BfgFundBalance", ctx => ctx.world.mechanisms.bfgFundBalance),
    ColumnDef("BailInLoss", ctx => ctx.world.flows.bailInLoss),
  )

  private def realGroup: Vector[ColumnDef] =
    val regionalHousingColumns = HousingConfig.RegionalMarket.all.map: market =>
      ColumnDef(market.hpiColName, ctx => ctx.housingRegionHpi(market))

    (Vector(
      // Housing Market
      ColumnDef("HousingPriceIndex", ctx => ctx.world.real.housing.priceIndex),
      ColumnDef("HousingMarketValue", ctx => ctx.world.real.housing.totalValue),
      ColumnDef("MortgageStock", ctx => ctx.ledgerHouseholdMortgageStock),
      ColumnDef("AvgMortgageRate", ctx => ctx.world.real.housing.avgMortgageRate),
      ColumnDef("MortgageOrigination", ctx => ctx.world.real.housing.lastOrigination),
      ColumnDef("MortgageRepayment", ctx => ctx.world.real.housing.lastRepayment),
      ColumnDef("MortgageDefault", ctx => ctx.world.real.housing.lastDefault),
      ColumnDef("MortgageInterestIncome", ctx => ctx.world.real.housing.mortgageInterestIncome),
      ColumnDef("HhHousingWealth", ctx => ctx.world.real.housing.hhHousingWealth),
      ColumnDef("HousingWealthEffect", ctx => ctx.world.real.housing.lastWealthEffect),
      ColumnDef(
        "MortgageToGdp",
        ctx =>
          if ctx.monthlyGdp > PLN.Zero && ctx.ledgerHouseholdMortgageStock > PLN.Zero
          then ctx.ledgerHouseholdMortgageStock / (ctx.monthlyGdp * 12)
          else Scalar.Zero,
      ),
    ) ++ regionalHousingColumns ++ Vector(
      // Sectoral Labor Mobility
      ColumnDef("SectorMobilityRate", ctx => ctx.world.real.sectoralMobility.sectorMobilityRate),
      ColumnDef("CrossSectorHires", ctx => ctx.world.real.sectoralMobility.crossSectorHires),
      ColumnDef("VoluntaryQuits", ctx => ctx.world.real.sectoralMobility.voluntaryQuits),
      // Physical Capital
      ColumnDef("AggCapitalStock", ctx => ctx.living.map(f => f.capitalStock).sumPln),
      ColumnDef("GrossInvestment", ctx => ctx.world.real.grossInvestment),
      ColumnDef(
        "CapitalDepreciation",
        ctx => ctx.living.map(f => f.capitalStock * ctx.p.capital.depRates(f.sector.toInt).monthly).sumPln,
      ),
      // Inventories
      ColumnDef("AggInventoryStock", ctx => ctx.world.flows.aggInventoryStock),
      ColumnDef("InventoryChange", ctx => ctx.world.flows.aggInventoryChange),
      ColumnDef(
        "InventoryToGdp",
        ctx => if ctx.monthlyGdp > PLN.Zero then ctx.world.flows.aggInventoryStock / ctx.monthlyGdp else Scalar.Zero,
      ),
      // Energy / Climate
      ColumnDef("AggEnergyCost", ctx => ctx.world.flows.aggEnergyCost),
      ColumnDef(
        "EnergyCostToGdp",
        ctx => if ctx.monthlyGdp > PLN.Zero then ctx.world.flows.aggEnergyCost / ctx.monthlyGdp else Scalar.Zero,
      ),
      ColumnDef("EtsPrice", ctx => ctx.world.real.etsPrice),
      ColumnDef("AggGreenCapital", ctx => ctx.world.real.aggGreenCapital),
      ColumnDef("GreenInvestment", ctx => ctx.world.real.aggGreenInvestment),
      ColumnDef(
        "GreenCapitalRatio",
        ctx => {
          val aggK = ctx.living.map(f => f.capitalStock).sumPln
          if ctx.world.real.aggGreenCapital > PLN.Zero && aggK > PLN.Zero then ctx.world.real.aggGreenCapital / aggK else Scalar.Zero
        },
      ),
    )).toVector

  private def socialGroup: Vector[ColumnDef] = Vector(
    // JST
    ColumnDef("JstRevenue", ctx => ctx.world.social.jst.revenue),
    ColumnDef("JstSpending", ctx => ctx.world.social.jst.spending),
    ColumnDef("JstDebt", ctx => ctx.world.social.jst.debt),
    ColumnDef("JstDeposits", ctx => ctx.ledgerFinancialState.funds.jstCash),
    ColumnDef("JstDeficit", ctx => ctx.world.social.jst.deficit),
    // ZUS/PPK
    ColumnDef("ZusContributions", ctx => ctx.world.social.zus.contributions),
    ColumnDef("ZusPensionPayments", ctx => ctx.world.social.zus.pensionPayments),
    ColumnDef("ZusGovSubvention", ctx => ctx.world.social.zus.govSubvention),
    ColumnDef("FusBalance", ctx => ctx.ledgerFinancialState.funds.zusCash),
    ColumnDef("NfzContributions", ctx => ctx.world.social.nfz.contributions),
    ColumnDef("NfzSpending", ctx => ctx.world.social.nfz.spending),
    ColumnDef("NfzBalance", ctx => ctx.ledgerFinancialState.funds.nfzCash),
    ColumnDef("NfzGovSubvention", ctx => ctx.world.social.nfz.govSubvention),
    ColumnDef("PpkContributions", ctx => ctx.world.social.ppk.contributions),
    ColumnDef("PpkBondHoldings", ctx => ctx.ledgerFinancialState.funds.ppkGovBondHoldings),
    ColumnDef("NRetirees", ctx => ctx.world.social.demographics.retirees),
    ColumnDef("WorkingAgePop", ctx => ctx.world.social.demographics.workingAgePop),
    ColumnDef("MonthlyRetirements", ctx => ctx.world.social.demographics.monthlyRetirements),
    // Earmarked funds (FP, PFRON, FGŚP)
    ColumnDef("FpBalance", ctx => ctx.ledgerFinancialState.funds.fpCash),
    ColumnDef("FpContributions", ctx => ctx.world.social.earmarked.fpContributions),
    ColumnDef("PfronBalance", ctx => ctx.ledgerFinancialState.funds.pfronCash),
    ColumnDef("FgspBalance", ctx => ctx.ledgerFinancialState.funds.fgspCash),
    ColumnDef("FgspSpending", ctx => ctx.world.social.earmarked.fgspSpending),
    ColumnDef("EarmarkedGovSubvention", ctx => ctx.world.social.earmarked.totalGovSubvention),
    // Forward-Looking Expectations
    ColumnDef("ExpectedInflation", ctx => ctx.world.mechanisms.expectations.expectedInflation),
    ColumnDef("NbpCredibility", ctx => ctx.world.mechanisms.expectations.credibility),
    ColumnDef("ForwardGuidanceRate", ctx => ctx.world.mechanisms.expectations.forwardGuidanceRate),
    ColumnDef("InflationForecastError", ctx => ctx.world.mechanisms.expectations.forecastError),
  )

  private def mechanismsGroup: Vector[ColumnDef] = Vector(
    // Macroprudential
    ColumnDef("CCyB", ctx => ctx.world.mechanisms.macropru.ccyb),
    ColumnDef("CreditToGdpGap", ctx => ctx.world.mechanisms.macropru.creditToGdpGap),
    ColumnDef(
      "EffectiveMinCar",
      ctx =>
        if ctx.aliveBanks.isEmpty then Multiplier.Zero
        else {
          given SimParams = ctx.p;
          ctx.aliveBanks.map(b => Macroprudential.effectiveMinCar(b.id.toInt, ctx.world.mechanisms.macropru.ccyb)).max
        },
    ),
    // FDI Composition
    ColumnDef("FdiProfitShifting", ctx => ctx.world.flows.fdiProfitShifting),
    ColumnDef("FdiRepatriation", ctx => ctx.world.flows.fdiRepatriation),
    ColumnDef("FdiGrossOutflow", ctx => ctx.world.flows.fdiProfitShifting + ctx.world.flows.fdiRepatriation),
    ColumnDef(
      "ForeignOwnedFrac",
      ctx => if ctx.nLiving > 0 then Share.fraction(ctx.living.count(_.foreignOwned), ctx.nLiving) else Share.Zero,
    ),
    ColumnDef("FdiCitLoss", ctx => ctx.world.flows.fdiCitLoss),
    // Endogenous Firm Entry
    ColumnDef("FirmBirths", ctx => ctx.world.flows.firmBirths),
    ColumnDef("FirmDeaths", ctx => ctx.world.flows.firmDeaths),
    ColumnDef("NetEntry", ctx => ctx.world.flows.firmBirths - ctx.world.flows.firmDeaths),
    ColumnDef("LivingFirmCount", ctx => ctx.nLiving),
    ColumnDef("NetFirmBirths", ctx => ctx.world.flows.netFirmBirths),
    ColumnDef("TotalFirmCount", ctx => ctx.firms.length),
    // Informal Economy
    ColumnDef("RealizedTaxShadowShare", ctx => ctx.world.flows.realizedTaxShadowShare),
    ColumnDef("NextTaxShadowShare", ctx => ctx.world.mechanisms.nextTaxShadowShare),
    ColumnDef("TaxEvasionLoss", ctx => ctx.world.flows.taxEvasionLoss),
    ColumnDef(
      "EvasionToGdpRatio",
      ctx => if ctx.monthlyGdp > PLN.Zero then ctx.world.flows.taxEvasionLoss / ctx.monthlyGdp else Scalar.Zero,
    ),
  )

  /** Regional unemployment rates per NUTS-1 macroregion. */
  private def regionalGroup: Vector[ColumnDef] =
    def regionUnemp(region: Region, label: String): ColumnDef =
      ColumnDef(
        s"Unemp_$label",
        ctx =>
          val regHh = ctx.households.filter(_.region == region)
          if regHh.isEmpty then Share.Zero
          else Share.fraction(regHh.count(h => !h.status.isInstanceOf[HhStatus.Employed]), regHh.length),
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
    val MonthlyGdpProxy: Col        = lookup("MonthlyGdpProxy")
    val AnnualizedGdpProxy: Col     = lookup("AnnualizedGdpProxy")
    val AutoRatio: Col              = lookup("AutoRatio")
    val HybridRatio: Col            = lookup("HybridRatio")
    val BpoAuto: Col                = lookup("BPO_Auto")
    val ManufAuto: Col              = lookup("Manuf_Auto")
    val RetailAuto: Col             = lookup("Retail_Auto")
    val HealthAuto: Col             = lookup("Health_Auto")
    val PublicAuto: Col             = lookup("Public_Auto")
    val AgriAuto: Col               = lookup("Agri_Auto")
    val BpoOutput: Col              = lookup("BPO_Output")
    val ManufOutput: Col            = lookup("Manuf_Output")
    val RetailOutput: Col           = lookup("Retail_Output")
    val HealthOutput: Col           = lookup("Health_Output")
    val PublicOutput: Col           = lookup("Public_Output")
    val AgriOutput: Col             = lookup("Agri_Output")
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

    private val sectorAutoNames   = sectorColumns.map(_.autoColName)
    private val sectorOutputNames = sectorColumns.map(_.outputColName)
    private val sectorSigmaNames  = sectorColumns.map(_.sigmaColName)

    private def sectorCol(names: Vector[String], sectorIndex: Int, kind: String): Col =
      names
        .lift(sectorIndex)
        .map(lookup)
        .getOrElse(throw new IndexOutOfBoundsException(s"$kind sector index must be between 0 and ${names.length - 1}, got $sectorIndex"))

    def sectorAuto(s: Int): Col   = sectorCol(sectorAutoNames, s, "sectorAuto")
    def sectorOutput(s: Int): Col = sectorCol(sectorOutputNames, s, "sectorOutput")
    def sectorSigma(s: Int): Col  = sectorCol(sectorSigmaNames, s, "sectorSigma")

  extension (c: Col) def ordinal: Int = c

  // -------------------------------------------------------------------------
  //  Public API
  // -------------------------------------------------------------------------

  /** Column names — derived from schema. */
  val colNames: Array[String] = schema.map(_.name)

  /** Number of columns — derived from schema. */
  val nCols: Int = schema.length

  private[montecarlo] val csvSchema: McCsvSchema[(ExecutionMonth, Array[MetricValue])] =
    McCsvSchema(
      header = colNames.mkString(";"),
      render = (month, row) =>
        val sb = new StringBuilder
        sb.append(month.toInt)
        for c <- 1 until nCols do sb.append(";").append(row(c).format(6))
        sb.toString,
    )

  /** Compute one row. Returns fixed-point metric values for MC aggregation. */
  def compute(
      executionMonth: ExecutionMonth,
      world: World,
      firms: Vector[Firm.State],
      households: Vector[Household.State],
      banks: Vector[Banking.BankState],
      householdAggregates: Household.Aggregates,
      ledgerFinancialState: LedgerFinancialState,
  )(using p: SimParams): Array[MetricValue] =
    val living     = firms.filter(Firm.isAlive)
    val aliveBanks = banks.filterNot(_.failed).toVector
    val ctx        = Ctx(executionMonth, world, firms, households, banks, householdAggregates, ledgerFinancialState, living, living.length, aliveBanks, p)
    val result     = new Array[MetricValue](schema.length)
    var i          = 0
    while i < schema.length do
      result(i) = schema(i).compute(ctx)
      i += 1
    result

  /** Typed row access: row.at(Col.Inflation) instead of row(1). */
  extension (row: Array[MetricValue]) def at(c: Col): MetricValue = row(c.ordinal)
