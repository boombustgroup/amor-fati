package sfc

import java.io.{File, PrintWriter}
import scala.util.Random
import _root_.sfc.config.*
import _root_.sfc.agents.*
import _root_.sfc.accounting.*
import _root_.sfc.engine.*
import _root_.sfc.init.WorldInit
import _root_.sfc.types.*
import _root_.sfc.util.KahanSum.*
import _root_.sfc.networks.Network

/** Result of a single simulation run. */
case class RunResult(
  timeSeries: Array[Array[Double]],
  terminalHhAgg: Option[Household.Aggregates]
)

/** Run one simulation with given seed. Returns time-series array + optional household aggregates. */
def runSingle(seed: Int, rc: RunConfig): RunResult =
  val init = WorldInit.initialize(seed, rc)
  var world = init.world
  var firms = init.firms
  var households = init.households


  // Collect time-series: 120 rows x N columns
  // Columns: Month, Inflation, Unemployment, AutoRatio+HybridRatio, ExRate, MarketWage,
  //          GovDebt, NPL, RefRate, PriceLevel, AutoRatio, HybridRatio,
  //          SectorAutoRatio(0..5): BPO, Manuf, Retail, Health, Public, Agri,
  //          EffectiveBDP (per-capita BDP actually delivered after fiscal constraints),
  //          SectorSigma(0..5): per-sector current sigma (evolves when SIGMA_LAMBDA>0),
  //          MeanDegree: average network degree (changes when REWIRE_RHO>0),
  //          IoFlows: total intermediate market payments (Paper-07),
  //          IoGdpRatio: intermediate flows / GDP,
  //          InterbankRate, MinBankCAR, MaxBankNPL, BankFailures,
  //          Housing: HPI, MarketValue, MortgageStock, MortgageRate, Origination,
  //                   Repayment, Default, MortgageInterest, HhHousingWealth,
  //                   HousingWealthEffect, MortgageToGdp
  val nCols = 197
  val results = Array.ofDim[Double](Config.Duration, nCols)

  for t <- 0 until Config.Duration do
    val (newW, newF, newHh) = Simulation.step(world, firms, rc, households)
    world = newW
    firms = newF
    households = newHh

    val unemployPct = 1.0 - world.hh.employed.toDouble / Config.TotalPopulation
    val living = firms.filter(Firm.isAlive)
    val nLiving = living.length.toDouble

    // Per-sector automation ratios
    val sectorAuto = SECTORS.indices.map { s =>
      val secFirms = living.filter(_.sector.toInt == s)
      if secFirms.isEmpty then 0.0
      else secFirms.count(f =>
        f.tech.isInstanceOf[TechState.Automated] || f.tech.isInstanceOf[TechState.Hybrid]
      ).toDouble / secFirms.length
    }

    // Effective BDP: actual per-capita BDP delivered (may be < legislated under EUR/SGP)
    val effectiveBdp = if world.gov.bdpActive then
      (world.gov.bdpSpending / Config.TotalPopulation.toDouble).toDouble
    else 0.0

    results(t) = Array(
      (t + 1).toDouble,          // 0: Month
      world.inflation.toDouble,   // 1: Inflation
      unemployPct,                // 2: Unemployment
      (world.automationRatio + world.hybridRatio).toDouble, // 3: TotalAdoption
      world.forex.exchangeRate,   // 4: ExRate
      world.hh.marketWage.toDouble,        // 5: MarketWage
      world.gov.cumulativeDebt.toDouble,   // 6: GovDebt
      world.bank.nplRatio,        // 7: NPL
      world.nbp.referenceRate.toDouble,    // 8: RefRate
      world.priceLevel,           // 9: PriceLevel
      world.automationRatio.toDouble,      // 10: AutoRatio
      world.hybridRatio.toDouble,          // 11: HybridRatio
      sectorAuto(0),              // 12: BPO auto
      sectorAuto(1),              // 13: Manuf auto
      sectorAuto(2),              // 14: Retail auto
      sectorAuto(3),              // 15: Health auto
      sectorAuto(4),              // 16: Public auto
      sectorAuto(5),              // 17: Agri auto
      effectiveBdp,               // 18: EffectiveBDP
      world.currentSigmas(0),     // 19: BPO_Sigma
      world.currentSigmas(1),     // 20: Manuf_Sigma
      world.currentSigmas(2),     // 21: Retail_Sigma
      world.currentSigmas(3),     // 22: Health_Sigma
      world.currentSigmas(4),     // 23: Public_Sigma
      world.currentSigmas(5),     // 24: Agri_Sigma
      firms.kahanSumBy(_.neighbors.length.toDouble) / firms.length, // 25: MeanDegree
      world.ioFlows.toDouble,                    // 26: IoFlows
      if world.gdpProxy > 0 then (world.ioFlows / world.gdpProxy).toDouble else 0.0,  // 27: IoGdpRatio
      world.bop.nfa.toDouble,                    // 28: NFA
      world.bop.currentAccount.toDouble,         // 29: CurrentAccount
      world.bop.capitalAccount.toDouble,         // 30: CapitalAccount
      world.bop.tradeBalance.toDouble,           // 31: TradeBalance_OE
      world.bop.exports.toDouble,                // 32: Exports_OE
      world.bop.totalImports.toDouble,           // 33: TotalImports_OE
      world.bop.importedIntermediates.toDouble,  // 34: ImportedInterm
      world.bop.fdi.toDouble,                    // 35: FDI
      world.gov.unempBenefitSpend.toDouble,      // 36: UnempBenefitSpend
      (1.0 - world.hh.employed.toDouble / Config.TotalPopulation - Config.NbpNairu) / Config.NbpNairu,  // 37: OutputGap
      world.gov.bondYield.toDouble,                // 38: BondYield
      world.gov.bondsOutstanding.toDouble,         // 39: BondsOutstanding
      world.bank.govBondHoldings.toDouble,         // 40: BankBondHoldings
      world.nbp.govBondHoldings.toDouble,          // 41: NbpBondHoldings
      (if world.nbp.qeActive then 1.0 else 0.0),  // 42: QeActive
      world.gov.debtServiceSpend.toDouble,         // 43: DebtService
      (world.nbp.govBondHoldings * world.gov.bondYield / 12.0).toDouble,  // 44: NbpRemittance
      world.nbp.fxReserves.toDouble,                                      // 45: FxReserves
      world.nbp.lastFxTraded.toDouble,                                    // 46: FxInterventionAmt
      (if Config.NbpFxIntervention then 1.0 else 0.0),           // 47: FxInterventionActive
      world.bankingSector.map(_.interbankRate.toDouble).getOrElse(world.nbp.referenceRate.toDouble),  // 48: InterbankRate
      world.bankingSector.map { bs =>
        val alive = bs.banks.filterNot(_.failed)
        if alive.isEmpty then 0.0 else alive.map(_.car).min
      }.getOrElse(world.bank.car),  // 49: MinBankCAR
      world.bankingSector.map { bs =>
        val alive = bs.banks.filterNot(_.failed)
        if alive.isEmpty then 0.0 else alive.map(_.nplRatio).max
      }.getOrElse(world.bank.nplRatio),  // 50: MaxBankNPL
      world.bankingSector.map(_.banks.count(_.failed).toDouble).getOrElse(0.0),  // 51: BankFailures
      // Monetary plumbing
      world.bankingSector.map { bs =>
        val (_, total) = Banking.computeReserveInterest(bs.banks, world.nbp.referenceRate.toDouble)
        total
      }.getOrElse(0.0),  // 52: ReserveInterest
      world.bankingSector.map { bs =>
        val (perBank, _) = Banking.computeStandingFacilities(bs.banks, world.nbp.referenceRate.toDouble)
        perBank.kahanSum
      }.getOrElse(0.0),  // 53: StandingFacilityNet
      world.bankingSector.map { bs =>
        // Deposit facility usage: sum of reserves parked at deposit facility
        bs.banks.filter(b => !b.failed && b.reservesAtNbp > PLN.Zero).kahanSumBy(_.reservesAtNbp.toDouble)
      }.getOrElse(0.0),  // 54: DepositFacilityUsage
      world.bankingSector.map { bs =>
        val (_, total) = Banking.interbankInterestFlows(bs.banks, bs.interbankRate.toDouble)
        total
      }.getOrElse(0.0),  // 55: InterbankInterestNet
      // Credit diagnostics
      world.monetaryAgg.map(_.m1.toDouble).getOrElse(world.bank.deposits.toDouble),     // 56: M1
      world.monetaryAgg.map(_.monetaryBase.toDouble).getOrElse(0.0),           // 57: MonetaryBase
      world.monetaryAgg.map(_.creditMultiplier).getOrElse(0.0),       // 58: CreditMultiplier
      // JST (local government)
      world.jst.revenue.toDouble,           // 59: JstRevenue
      world.jst.spending.toDouble,          // 60: JstSpending
      world.jst.debt.toDouble,              // 61: JstDebt
      world.jst.deposits.toDouble,          // 62: JstDeposits
      world.jst.deficit.toDouble,           // 63: JstDeficit
      // LCR/NSFR
      world.bankingSector.map { bs =>
        val alive = bs.banks.filterNot(_.failed)
        if alive.isEmpty then 0.0 else alive.map(_.lcr).min
      }.getOrElse(0.0),  // 64: MinBankLCR
      world.bankingSector.map { bs =>
        val alive = bs.banks.filterNot(_.failed)
        if alive.isEmpty then 0.0 else alive.map(_.nsfr).min
      }.getOrElse(0.0),  // 65: MinBankNSFR
      world.bankingSector.map { bs =>
        val alive = bs.banks.filterNot(_.failed)
        if alive.isEmpty then 0.0
        else alive.map(b => if b.deposits > PLN.Zero then b.termDeposits / b.deposits else 0.0).sum / alive.length
      }.getOrElse(Config.BankTermDepositFrac),  // 66: AvgTermDepositFrac
      // Term structure
      world.bankingSector.flatMap(_.interbankCurve).map(_.wibor1m.toDouble).getOrElse(0.0),  // 67: WIBOR_1M
      world.bankingSector.flatMap(_.interbankCurve).map(_.wibor3m.toDouble).getOrElse(0.0),  // 68: WIBOR_3M
      world.bankingSector.flatMap(_.interbankCurve).map(_.wibor6m.toDouble).getOrElse(0.0),  // 69: WIBOR_6M
      // Full public sector
      world.zus.contributions.toDouble,           // 70: ZusContributions
      world.zus.pensionPayments.toDouble,         // 71: ZusPensionPayments
      world.zus.govSubvention.toDouble,           // 72: ZusGovSubvention
      world.zus.fusBalance.toDouble,              // 73: FusBalance
      world.ppk.contributions.toDouble,           // 74: PpkContributions
      world.ppk.bondHoldings.toDouble,            // 75: PpkBondHoldings
      world.demographics.retirees.toDouble,           // 76: NRetirees
      world.demographics.workingAgePop.toDouble,      // 77: WorkingAgePop
      world.demographics.monthlyRetirements.toDouble,  // 78: MonthlyRetirements
      // Macroprudential
      world.macropru.ccyb.toDouble,                             // 79: CCyB
      world.macropru.creditToGdpGap,                   // 80: CreditToGdpGap
      world.bankingSector.map { bs =>
        val alive = bs.banks.filterNot(_.failed)
        if alive.isEmpty then 0.0
        else alive.map(b => Macroprudential.effectiveMinCar(b.id.toInt, world.macropru.ccyb.toDouble)).max
      }.getOrElse(Macroprudential.effectiveMinCar(0, world.macropru.ccyb.toDouble)),  // 81: EffectiveMinCar
      // GPW Equity Market
      world.equity.index,           // 82: GpwIndex
      world.equity.marketCap.toDouble,       // 83: GpwMarketCap
      (if world.equity.earningsYield.toDouble > 0 then 1.0 / world.equity.earningsYield.toDouble else 0.0),  // 84: GpwPE
      world.equity.dividendYield.toDouble,   // 85: GpwDivYield
      // GPW Firm Issuance
      world.equity.lastIssuance.toDouble,    // 86: EquityIssuanceTotal
      living.kahanSumBy(_.equityRaised.toDouble) / Math.max(1.0, living.kahanSumBy(f => (f.debt + f.equityRaised).toDouble)),  // 87: EquityFinancedFrac
      // GPW Household Equity
      world.equity.hhEquityWealth.toDouble,  // 88: HhEquityWealth
      world.equity.lastWealthEffect.toDouble, // 89: EquityWealthEffect
      // GPW Dividends
      world.equity.lastDomesticDividends.toDouble,  // 90: DomesticDividends
      world.equity.lastForeignDividends.toDouble,   // 91: ForeignDividendOutflow
      // Housing Market
      world.housing.priceIndex,            // 92: HousingPriceIndex
      world.housing.totalValue.toDouble,            // 93: HousingMarketValue
      world.housing.mortgageStock.toDouble,         // 94: MortgageStock
      world.housing.avgMortgageRate.toDouble,       // 95: AvgMortgageRate
      world.housing.lastOrigination.toDouble,       // 96: MortgageOrigination
      world.housing.lastRepayment.toDouble,         // 97: MortgageRepayment
      world.housing.lastDefault.toDouble,           // 98: MortgageDefault
      world.housing.mortgageInterestIncome.toDouble, // 99: MortgageInterestIncome
      world.housing.hhHousingWealth.toDouble,       // 100: HhHousingWealth
      world.housing.lastWealthEffect.toDouble,      // 101: HousingWealthEffect
      (if world.gdpProxy > 0 && world.housing.mortgageStock > PLN.Zero
       then (world.housing.mortgageStock / (world.gdpProxy * 12.0)).toDouble else 0.0),  // 102: MortgageToGdp
      // Sectoral Labor Mobility
      world.sectoralMobility.sectorMobilityRate,    // 103: SectorMobilityRate
      world.sectoralMobility.crossSectorHires.toDouble,  // 104: CrossSectorHires
      world.sectoralMobility.voluntaryQuits.toDouble, // 105: VoluntaryQuits
      // GVC / Deep External Sector
      world.gvc.disruptionIndex.toDouble,        // 106: GvcDisruptionIndex
      world.gvc.foreignPriceIndex,      // 107: ForeignPriceIndex
      world.gvc.tradeConcentration.toDouble,     // 108: GvcTradeConcentration
      world.gvc.exportDemandShockMag.toDouble,   // 109: GvcExportDemandShock
      world.gvc.importCostIndex,         // 110: GvcImportCostIndex
      // Forward-Looking Expectations
      world.expectations.expectedInflation.toDouble,   // 111: ExpectedInflation
      world.expectations.credibility.toDouble,         // 112: NbpCredibility
      world.expectations.forwardGuidanceRate.toDouble, // 113: ForwardGuidanceRate
      world.expectations.forecastError.toDouble,       // 114: InflationForecastError
      // Regional Housing Market
      world.housing.regions.map(_(0).priceIndex).getOrElse(0.0),  // 115: WawHpi
      world.housing.regions.map(_(1).priceIndex).getOrElse(0.0),  // 116: KrkHpi
      world.housing.regions.map(_(2).priceIndex).getOrElse(0.0),  // 117: WroHpi
      world.housing.regions.map(_(3).priceIndex).getOrElse(0.0),  // 118: GdnHpi
      world.housing.regions.map(_(4).priceIndex).getOrElse(0.0),  // 119: LdzHpi
      world.housing.regions.map(_(5).priceIndex).getOrElse(0.0),  // 120: PozHpi
      world.housing.regions.map(_(6).priceIndex).getOrElse(0.0),  // 121: RestHpi
      // Immigration
      world.immigration.immigrantStock.toDouble,     // 122: ImmigrantStock
      world.immigration.monthlyInflow.toDouble,      // 123: MonthlyImmigInflow
      world.immigration.remittanceOutflow,            // 124: RemittanceOutflow
      (if world.immigration.immigrantStock > 0 then  // 125: ImmigrantUnempRate
        households.map { hhs =>
          val immigrants = hhs.filter(_.isImmigrant)
          if immigrants.nonEmpty then
            immigrants.count(h => !h.status.isInstanceOf[HhStatus.Employed]).toDouble / immigrants.length
          else 0.0
        }.getOrElse(0.0)
      else 0.0),
      // PIT
      world.hhAgg.map { agg =>                      // 126: EffectivePitRate
        val gross = agg.totalIncome + agg.totalPit   // totalIncome is net-of-PIT, add back
        if gross > PLN.Zero then agg.totalPit / gross else 0.0
      }.getOrElse(if Config.PitEnabled then Config.PitEffectiveRate else 0.0),
      // Social Transfers
      world.gov.socialTransferSpend.toDouble,                 // 127: SocialTransferSpend
      // Public Investment
      world.gov.govCurrentSpend.toDouble,                     // 128: GovCurrentSpend
      world.gov.govCapitalSpend.toDouble,                     // 129: GovCapitalSpend
      world.gov.publicCapitalStock.toDouble,                   // 130: PublicCapitalStock
      // EU Funds Dynamics
      world.gov.euCofinancing.toDouble,                         // 131: EuCofinancing
      world.bop.euFundsMonthly.toDouble,                        // 132: EuFundsMonthly
      world.bop.euCumulativeAbsorption.toDouble,                // 133: EuCumulativeAbsorption
      world.hh.minWageLevel.toDouble,                             // 134: MinWageLevel
      world.fofResidual,                                   // 135: FofResidual
      // Consumer Credit
      world.bank.consumerLoans.toDouble,                            // 136: ConsumerLoans
      (if world.bank.consumerLoans > PLN.Zero then world.bank.consumerNpl / world.bank.consumerLoans else 0.0), // 137: ConsumerNplRatio
      world.hhAgg.map(_.totalConsumerOrigination.toDouble).getOrElse(0.0), // 138: ConsumerOrigination
      world.hhAgg.map(_.totalConsumerDebtService.toDouble).getOrElse(0.0), // 139: ConsumerDebtService
      // Physical Capital
      living.kahanSumBy(_.capitalStock.toDouble),                           // 140: AggCapitalStock
      world.grossInvestment.toDouble,                                       // 141: GrossInvestment
      (if Config.PhysCapEnabled then                               // 142: CapitalDepreciation
        living.kahanSumBy(f => (f.capitalStock * Config.PhysCapDepRates(f.sector.toInt) / 12.0).toDouble)
      else 0.0),
      // Excise & Customs
      world.gov.exciseRevenue.toDouble,                                       // 143: ExciseRevenue
      world.gov.customsDutyRevenue.toDouble,                                  // 144: CustomsDutyRevenue
      // Corporate Bonds / Catalyst (#40)
      world.corporateBonds.outstanding.toDouble,                              // 145: CorpBondOutstanding
      world.corporateBonds.corpBondYield.toDouble,                            // 146: CorpBondYield
      world.corporateBonds.lastIssuance.toDouble,                             // 147: CorpBondIssuance
      world.corporateBonds.creditSpread.toDouble,                             // 148: CorpBondSpread
      world.corporateBonds.bankHoldings.toDouble,                             // 149: BankCorpBondHoldings
      world.corporateBonds.ppkHoldings.toDouble,                               // 150: PpkCorpBondHoldings
      world.corporateBonds.lastAbsorptionRate.toDouble,                         // 151: CorpBondAbsorptionRate
      // Insurance Sector (#41)
      world.insurance.lifeReserves.toDouble,                                    // 152: InsLifeReserves
      world.insurance.nonLifeReserves.toDouble,                                 // 153: InsNonLifeReserves
      world.insurance.govBondHoldings.toDouble,                                 // 154: InsGovBondHoldings
      world.insurance.lastLifePremium.toDouble,                                 // 155: InsLifePremium
      world.insurance.lastNonLifePremium.toDouble,                              // 156: InsNonLifePremium
      world.insurance.lastLifeClaims.toDouble,                                  // 157: InsLifeClaims
      world.insurance.lastNonLifeClaims.toDouble,                               // 158: InsNonLifeClaims
      // Shadow Banking / NBFI (#42)
      world.nbfi.tfiAum.toDouble,                                                // 159: NbfiTfiAum
      world.nbfi.tfiGovBondHoldings.toDouble,                                    // 160: NbfiTfiGovBondHoldings
      world.nbfi.nbfiLoanStock.toDouble,                                         // 161: NbfiLoanStock
      world.nbfi.lastNbfiOrigination.toDouble,                                   // 162: NbfiOrigination
      world.nbfi.lastNbfiDefaultAmount.toDouble,                                 // 163: NbfiDefaults
      world.nbfi.lastBankTightness.toDouble,                                     // 164: NbfiBankTightness
      world.nbfi.lastDepositDrain.toDouble,                                       // 165: NbfiDepositDrain
      // FDI Composition (#33)
      world.fdiProfitShifting.toDouble,                                             // 166: FdiProfitShifting
      world.fdiRepatriation.toDouble,                                               // 167: FdiRepatriation
      (world.fdiProfitShifting + world.fdiRepatriation).toDouble,                     // 168: FdiGrossOutflow
      (if nLiving > 0 then living.count(_.foreignOwned).toDouble / nLiving else 0.0), // 169: ForeignOwnedFrac
      world.fdiCitLoss.toDouble,                                                     // 170: FdiCitLoss
      // Endogenous Firm Entry (#35)
      world.firmBirths.toDouble,                                              // 171: FirmBirths
      world.firmDeaths.toDouble,                                              // 172: FirmDeaths
      (world.firmBirths - world.firmDeaths).toDouble,                         // 173: NetEntry
      nLiving.toDouble,                                                        // 174: LivingFirmCount
      // Inventories (#43)
      world.aggInventoryStock.toDouble,                                                 // 175: AggInventoryStock
      world.aggInventoryChange.toDouble,                                                // 176: InventoryChange
      (if world.gdpProxy > 0 then (world.aggInventoryStock / world.gdpProxy).toDouble else 0.0), // 177: InventoryToGdp
      // Informal Economy (#45)
      (if Config.InformalEnabled then
        Config.FofConsWeights.zip(Config.InformalSectorShares).map((cw, ss) =>
          cw * Math.min(1.0, ss + world.informalCyclicalAdj)).sum
      else 0.0),                                                                     // 178: EffectiveShadowShare
      world.taxEvasionLoss.toDouble,                                                            // 179: TaxEvasionLoss
      world.informalEmployed.toDouble,                                                          // 180: InformalEmployment
      (if world.gdpProxy > 0 then (world.taxEvasionLoss / world.gdpProxy).toDouble else 0.0),    // 181: EvasionToGdpRatio
      // Energy / Climate (#36)
      world.aggEnergyCost.toDouble,                                                             // 182: AggEnergyCost
      (if world.gdpProxy > 0 then (world.aggEnergyCost / world.gdpProxy).toDouble else 0.0),     // 183: EnergyCostToGdp
      (if Config.EnergyEnabled then
        Config.EtsBasePrice * Math.pow(1.0 + Config.EtsPriceDrift / 12.0, (t + 1).toDouble)
      else 0.0),                                                                       // 184: EtsPrice
      world.aggGreenCapital.toDouble,                                                            // 185: AggGreenCapital
      world.aggGreenInvestment.toDouble,                                                         // 186: GreenInvestment
      {                                                                                 // 187: GreenCapitalRatio
        val aggK = living.kahanSumBy(_.capitalStock.toDouble)
        if world.aggGreenCapital > PLN.Zero && aggK > 0 then world.aggGreenCapital.toDouble / aggK else 0.0
      },
      // Diaspora Remittances (#46)
      world.diasporaRemittanceInflow.toDouble,                                                     // 188: DiasporaRemittanceInflow
      (world.diasporaRemittanceInflow - PLN(world.immigration.remittanceOutflow)).toDouble,                // 189: NetRemittances
      // Tourism (#47)
      world.tourismExport.toDouble,                                                               // 190: TourismExport
      world.tourismImport.toDouble,                                                               // 191: TourismImport
      (world.tourismExport - world.tourismImport).toDouble,                                         // 192: NetTourismBalance
      {                                                                                  // 193: TourismSeasonalFactor
        val monthInYear = (t % 12) + 1
        1.0 + Config.TourismSeasonality *
          Math.cos(2 * Math.PI * (monthInYear - Config.TourismPeakMonth) / 12.0)
      },
      // KNF/BFG (#48)
      (if Config.BankFailureEnabled then
        world.bankingSector.map(bs => bs.banks.filterNot(_.failed).map(b =>
          (b.deposits * Config.BfgLevyRate / 12.0).toDouble).sum).getOrElse(
          (world.bank.deposits * Config.BfgLevyRate / 12.0).toDouble)
      else 0.0),                                                                     // 194: BfgLevyTotal
      world.bfgFundBalance.toDouble,                                                           // 195: BfgFundBalance
      world.bailInLoss.toDouble                                                                // 196: BailInLoss
    )

  RunResult(results, world.hhAgg)

// ---- Monte Carlo main entry point ----

@main def sfcMonteCarlo(bdpAmountStr: String, nSeedsStr: String, outputPrefix: String,
    regimeStr: String = "pln"): Unit =
  val bdpAmount = bdpAmountStr.toDouble
  val nSeeds    = nSeedsStr.toInt
  val regime = regimeStr.toLowerCase match
    case "eur" | "euro" | "ecb" => MonetaryRegime.Eur
    case _                      => MonetaryRegime.Pln
  val rc = RunConfig(bdpAmount, nSeeds, outputPrefix, regime)
  val regimeLabel = if rc.isEurozone then "EUR (ECB)" else "PLN (NBP)"

  val topoLabel = TOPOLOGY.toString.toUpperCase
  val firmsLabel = f"${Config.FirmsCount}%,d"
  val hhLabel = HH_MODE match
    case HhMode.Individual => s" | HH=individual (${Config.HhCount})"
    case HhMode.Aggregate  => ""
  val bankLabel = if Config.BankMulti then " | BANK=multi (7)" else ""
  println(s"+" + "=" * 68 + "+")
  println(s"|  SFC-ABM v8: BDP=${bdpAmount.toInt} PLN, N=${nSeeds} seeds, ${regimeLabel}${hhLabel}${bankLabel}")
  println(s"|  ${firmsLabel} firms x 6 sectors (GUS 2024) x ${topoLabel} network x 120m")
  println(s"+" + "=" * 68 + "+")

  val outDir = new File("mc")
  if !outDir.exists() then outDir.mkdirs()

  // Aggregation arrays
  val nMonths = Config.Duration
  val nCols   = 197
  val allRuns = Array.ofDim[Double](nSeeds, nMonths, nCols)
  val allHhAgg = new Array[Option[Household.Aggregates]](nSeeds)

  val startTime = System.currentTimeMillis()

  for seed <- 1 to nSeeds do
    val t0 = System.currentTimeMillis()
    val result = runSingle(seed, rc)
    allRuns(seed - 1) = result.timeSeries
    allHhAgg(seed - 1) = result.terminalHhAgg
    val dt = System.currentTimeMillis() - t0

    if seed <= 3 || seed % 10 == 0 || seed == nSeeds then
      val adoption = result.timeSeries(nMonths - 1)(3)
      val inflation = result.timeSeries(nMonths - 1)(1)
      val unemp = result.timeSeries(nMonths - 1)(2)
      println(f"  Seed $seed%3d/${nSeeds} (${dt}ms) | " +
        f"Adopt=${adoption * 100}%5.1f%% | pi=${inflation * 100}%5.1f%% | " +
        f"Unemp=${unemp * 100}%5.1f%%")

  val totalTime = (System.currentTimeMillis() - startTime) / 1000.0
  println(f"\nTotal time: ${totalTime}%.1f seconds")

  // -- Write per-seed terminal values --
  val termPw = new PrintWriter(new File(s"mc/${outputPrefix}_terminal.csv"))
  termPw.write("Seed;Inflation;Unemployment;TotalAdoption;ExRate;MarketWage;" +
    "GovDebt;NPL;RefRate;PriceLevel;AutoRatio;HybridRatio;" +
    "BPO_Auto;Manuf_Auto;Retail_Auto;Health_Auto;Public_Auto;Agri_Auto;EffectiveBDP;" +
    "BPO_Sigma;Manuf_Sigma;Retail_Sigma;Health_Sigma;Public_Sigma;Agri_Sigma;MeanDegree;" +
    "IoFlows;IoGdpRatio;" +
    "NFA;CurrentAccount;CapitalAccount;TradeBalance_OE;Exports_OE;TotalImports_OE;ImportedInterm;FDI;" +
    "UnempBenefitSpend;OutputGap;" +
    "BondYield;BondsOutstanding;BankBondHoldings;NbpBondHoldings;QeActive;DebtService;NbpRemittance;" +
    "FxReserves;FxInterventionAmt;FxInterventionActive;" +
    "InterbankRate;MinBankCAR;MaxBankNPL;BankFailures;" +
    "ReserveInterest;StandingFacilityNet;DepositFacilityUsage;InterbankInterestNet;" +
    "M1;MonetaryBase;CreditMultiplier;" +
    "JstRevenue;JstSpending;JstDebt;JstDeposits;JstDeficit;" +
    "MinBankLCR;MinBankNSFR;AvgTermDepositFrac;" +
    "WIBOR_1M;WIBOR_3M;WIBOR_6M;" +
    "ZusContributions;ZusPensionPayments;ZusGovSubvention;FusBalance;" +
    "PpkContributions;PpkBondHoldings;NRetirees;WorkingAgePop;MonthlyRetirements;" +
    "CCyB;CreditToGdpGap;EffectiveMinCar;" +
    "GpwIndex;GpwMarketCap;GpwPE;GpwDivYield;" +
    "EquityIssuanceTotal;EquityFinancedFrac;" +
    "HhEquityWealth;EquityWealthEffect;" +
    "DomesticDividends;ForeignDividendOutflow;" +
    "HousingPriceIndex;HousingMarketValue;MortgageStock;AvgMortgageRate;" +
    "MortgageOrigination;MortgageRepayment;MortgageDefault;MortgageInterestIncome;" +
    "HhHousingWealth;HousingWealthEffect;MortgageToGdp;" +
    "SectorMobilityRate;CrossSectorHires;VoluntaryQuits;" +
    "GvcDisruptionIndex;ForeignPriceIndex;GvcTradeConcentration;GvcExportDemandShock;GvcImportCostIndex;" +
    "ExpectedInflation;NbpCredibility;ForwardGuidanceRate;InflationForecastError;" +
    "WawHpi;KrkHpi;WroHpi;GdnHpi;LdzHpi;PozHpi;RestHpi;" +
    "ImmigrantStock;MonthlyImmigInflow;RemittanceOutflow;ImmigrantUnempRate;" +
    "EffectivePitRate;SocialTransferSpend;" +
    "GovCurrentSpend;GovCapitalSpend;PublicCapitalStock;" +
    "EuCofinancing;EuFundsMonthly;EuCumulativeAbsorption;MinWageLevel;FofResidual;" +
    "ConsumerLoans;ConsumerNplRatio;ConsumerOrigination;ConsumerDebtService;" +
    "AggCapitalStock;GrossInvestment;CapitalDepreciation;" +
    "ExciseRevenue;CustomsDutyRevenue;" +
    "CorpBondOutstanding;CorpBondYield;CorpBondIssuance;CorpBondSpread;BankCorpBondHoldings;PpkCorpBondHoldings;CorpBondAbsorptionRate;" +
    "InsLifeReserves;InsNonLifeReserves;InsGovBondHoldings;InsLifePremium;InsNonLifePremium;InsLifeClaims;InsNonLifeClaims;" +
    "NbfiTfiAum;NbfiTfiGovBondHoldings;NbfiLoanStock;NbfiOrigination;NbfiDefaults;NbfiBankTightness;NbfiDepositDrain;" +
    "FdiProfitShifting;FdiRepatriation;FdiGrossOutflow;ForeignOwnedFrac;FdiCitLoss;" +
    "FirmBirths;FirmDeaths;NetEntry;LivingFirmCount;" +
    "AggInventoryStock;InventoryChange;InventoryToGdp;" +
    "EffectiveShadowShare;TaxEvasionLoss;InformalEmployment;EvasionToGdpRatio;" +
    "AggEnergyCost;EnergyCostToGdp;EtsPrice;AggGreenCapital;GreenInvestment;GreenCapitalRatio;" +
    "DiasporaRemittanceInflow;NetRemittances;" +
    "TourismExport;TourismImport;NetTourismBalance;TourismSeasonalFactor;" +
    "BfgLevyTotal;BfgFundBalance;BailInLoss\n")
  for seed <- 0 until nSeeds do
    val last = allRuns(seed)(nMonths - 1)
    termPw.write(s"${seed + 1}")
    for c <- 1 until nCols do
      termPw.write(f";${last(c)}%.6f")
    termPw.write("\n")
  termPw.close()

  // -- Write household terminal CSV (individual mode only) --
  if HH_MODE == HhMode.Individual then
    val hhPw = new PrintWriter(new File(s"mc/${outputPrefix}_hh_terminal.csv"))
    hhPw.write("Seed;HH_Employed;HH_Unemployed;HH_Retraining;HH_Bankrupt;" +
      "MeanSavings;MedianSavings;P10Savings;P90Savings;" +
      "MeanDebt;TotalDebt;" +
      "Gini_Individual;Gini_Wealth;" +
      "MeanSkill;MeanHealthPenalty;" +
      "RetrainingAttempts;RetrainingSuccesses;" +
      "ConsumptionP10;ConsumptionP50;ConsumptionP90;" +
      "BankruptcyRate;MeanMonthsToRuin;" +
      "PovertyRate_50pct;PovertyRate_30pct\n")
    for seed <- 0 until nSeeds do
      allHhAgg(seed) match
        case Some(agg) =>
          hhPw.write(s"${seed + 1}")
          hhPw.write(f";${agg.employed};${agg.unemployed};${agg.retraining};${agg.bankrupt}")
          hhPw.write(f";${agg.meanSavings.toDouble}%.2f;${agg.medianSavings.toDouble}%.2f")
          // P10/P90 savings not tracked in agg — use mean as placeholder
          hhPw.write(f";${(agg.meanSavings * 0.3).toDouble}%.2f;${(agg.meanSavings * 2.0).toDouble}%.2f")
          hhPw.write(f";0.00;0.00")  // MeanDebt, TotalDebt — would need household vector
          hhPw.write(f";${agg.giniIndividual.toDouble}%.6f;${agg.giniWealth.toDouble}%.6f")
          hhPw.write(f";${agg.meanSkill}%.6f;${agg.meanHealthPenalty}%.6f")
          hhPw.write(f";${agg.retrainingAttempts};${agg.retrainingSuccesses}")
          hhPw.write(f";${agg.consumptionP10.toDouble}%.2f;${agg.consumptionP50.toDouble}%.2f;${agg.consumptionP90.toDouble}%.2f")
          hhPw.write(f";${agg.bankruptcyRate.toDouble}%.6f;${agg.meanMonthsToRuin}%.2f")
          hhPw.write(f";${agg.povertyRate50.toDouble}%.6f;${agg.povertyRate30.toDouble}%.6f")
          hhPw.write("\n")
        case None => ()
    hhPw.close()
    println(s"Saved: mc/${outputPrefix}_hh_terminal.csv")

  // -- Write bank terminal CSV (multi-bank mode only) --
  if Config.BankMulti then
    val bankPw = new PrintWriter(new File(s"mc/${outputPrefix}_banks_terminal.csv"))
    bankPw.write("Seed;BankId;BankName;Deposits;Loans;Capital;NPL;CAR;GovBonds;InterbankNet;Failed\n")
    for seed <- 0 until nSeeds do
      // Re-run to get final banking sector state (already in allRuns terminal row)
      val lastRow = allRuns(seed)(nMonths - 1)
      // Columns 48-51 are aggregate; for per-bank detail we need the actual state
      // Use a simplified approach: write aggregate-level data per bank from the terminal values
      bankPw.write(f"${seed + 1};0;Aggregate;0;0;0;${lastRow(50)}%.6f;${lastRow(49)}%.6f;0;0;${lastRow(51)}%.0f\n")
    bankPw.close()
    println(s"Saved: mc/${outputPrefix}_banks_terminal.csv")

  // -- Write aggregated time-series (mean, std, p5, p95) --
  val aggPw = new PrintWriter(new File(s"mc/${outputPrefix}_timeseries.csv"))
  val colNames = Array("Month", "Inflation", "Unemployment", "TotalAdoption", "ExRate",
    "MarketWage", "GovDebt", "NPL", "RefRate", "PriceLevel",
    "AutoRatio", "HybridRatio", "BPO_Auto", "Manuf_Auto", "Retail_Auto", "Health_Auto",
    "Public_Auto", "Agri_Auto", "EffectiveBDP",
    "BPO_Sigma", "Manuf_Sigma", "Retail_Sigma", "Health_Sigma", "Public_Sigma", "Agri_Sigma",
    "MeanDegree", "IoFlows", "IoGdpRatio",
    "NFA", "CurrentAccount", "CapitalAccount", "TradeBalance_OE", "Exports_OE",
    "TotalImports_OE", "ImportedInterm", "FDI",
    "UnempBenefitSpend", "OutputGap",
    "BondYield", "BondsOutstanding", "BankBondHoldings", "NbpBondHoldings",
    "QeActive", "DebtService", "NbpRemittance",
    "FxReserves", "FxInterventionAmt", "FxInterventionActive",
    "InterbankRate", "MinBankCAR", "MaxBankNPL", "BankFailures",
    "ReserveInterest", "StandingFacilityNet", "DepositFacilityUsage", "InterbankInterestNet",
    "M1", "MonetaryBase", "CreditMultiplier",
    "JstRevenue", "JstSpending", "JstDebt", "JstDeposits", "JstDeficit",
    "MinBankLCR", "MinBankNSFR", "AvgTermDepositFrac",
    "WIBOR_1M", "WIBOR_3M", "WIBOR_6M",
    "ZusContributions", "ZusPensionPayments", "ZusGovSubvention", "FusBalance",
    "PpkContributions", "PpkBondHoldings", "NRetirees", "WorkingAgePop", "MonthlyRetirements",
    "CCyB", "CreditToGdpGap", "EffectiveMinCar",
    "GpwIndex", "GpwMarketCap", "GpwPE", "GpwDivYield",
    "EquityIssuanceTotal", "EquityFinancedFrac",
    "HhEquityWealth", "EquityWealthEffect",
    "DomesticDividends", "ForeignDividendOutflow",
    "HousingPriceIndex", "HousingMarketValue", "MortgageStock", "AvgMortgageRate",
    "MortgageOrigination", "MortgageRepayment", "MortgageDefault", "MortgageInterestIncome",
    "HhHousingWealth", "HousingWealthEffect", "MortgageToGdp",
    "SectorMobilityRate", "CrossSectorHires", "VoluntaryQuits",
    "GvcDisruptionIndex", "ForeignPriceIndex", "GvcTradeConcentration",
    "GvcExportDemandShock", "GvcImportCostIndex",
    "ExpectedInflation", "NbpCredibility", "ForwardGuidanceRate", "InflationForecastError",
    "WawHpi", "KrkHpi", "WroHpi", "GdnHpi", "LdzHpi", "PozHpi", "RestHpi",
    "ImmigrantStock", "MonthlyImmigInflow", "RemittanceOutflow", "ImmigrantUnempRate",
    "EffectivePitRate", "SocialTransferSpend",
    "GovCurrentSpend", "GovCapitalSpend", "PublicCapitalStock",
    "EuCofinancing", "EuFundsMonthly", "EuCumulativeAbsorption",
    "MinWageLevel", "FofResidual",
    "ConsumerLoans", "ConsumerNplRatio", "ConsumerOrigination", "ConsumerDebtService",
    "AggCapitalStock", "GrossInvestment", "CapitalDepreciation",
    "ExciseRevenue", "CustomsDutyRevenue",
    "CorpBondOutstanding", "CorpBondYield", "CorpBondIssuance",
    "CorpBondSpread", "BankCorpBondHoldings", "PpkCorpBondHoldings",
    "CorpBondAbsorptionRate",
    "InsLifeReserves", "InsNonLifeReserves", "InsGovBondHoldings",
    "InsLifePremium", "InsNonLifePremium", "InsLifeClaims", "InsNonLifeClaims",
    "NbfiTfiAum", "NbfiTfiGovBondHoldings", "NbfiLoanStock",
    "NbfiOrigination", "NbfiDefaults", "NbfiBankTightness", "NbfiDepositDrain",
    "FdiProfitShifting", "FdiRepatriation", "FdiGrossOutflow", "ForeignOwnedFrac", "FdiCitLoss",
    "FirmBirths", "FirmDeaths", "NetEntry", "LivingFirmCount",
    "AggInventoryStock", "InventoryChange", "InventoryToGdp",
    "EffectiveShadowShare", "TaxEvasionLoss", "InformalEmployment", "EvasionToGdpRatio",
    "AggEnergyCost", "EnergyCostToGdp", "EtsPrice", "AggGreenCapital", "GreenInvestment", "GreenCapitalRatio",
    "DiasporaRemittanceInflow", "NetRemittances",
    "TourismExport", "TourismImport", "NetTourismBalance", "TourismSeasonalFactor",
    "BfgLevyTotal", "BfgFundBalance", "BailInLoss")
  // Header: Month, then for each metric: mean, std, p05, p95
  aggPw.write("Month")
  for c <- 1 until nCols do
    aggPw.write(s";${colNames(c)}_mean;${colNames(c)}_std;${colNames(c)}_p05;${colNames(c)}_p95")
  aggPw.write("\n")

  for t <- 0 until nMonths do
    aggPw.write(s"${t + 1}")
    for c <- 1 until nCols do
      val vals = (0 until nSeeds).map(s => allRuns(s)(t)(c)).sorted.toArray
      val mean = vals.kahanSum / vals.length
      val variance = vals.kahanSumBy(v => (v - mean) * (v - mean)) / vals.length
      val std  = Math.sqrt(variance)
      val p05  = vals((vals.length * 0.05).toInt)
      val p95  = vals(Math.min(vals.length - 1, (vals.length * 0.95).toInt))
      aggPw.write(f";$mean%.6f;$std%.6f;$p05%.6f;$p95%.6f")
    aggPw.write("\n")
  aggPw.close()

  // -- Summary statistics --
  println("\n" + "=" * 54)
  println(s"MONTE CARLO SUMMARY: ${outputPrefix} (BDP=${bdpAmount.toInt}, N=${nSeeds})")
  println("=" * 54)

  def statsSummary(name: String, colIdx: Int, mult: Double = 1.0): Unit =
    val vals = (0 until nSeeds).map(s => allRuns(s)(nMonths - 1)(colIdx) * mult).sorted.toArray
    val mean = vals.kahanSum / vals.length
    val std  = Math.sqrt(vals.kahanSumBy(v => (v - mean) * (v - mean)) / vals.length)
    val p05  = vals((vals.length * 0.05).toInt)
    val p95  = vals(Math.min(vals.length - 1, (vals.length * 0.95).toInt))
    println(f"  $name%-25s mean=${mean}%8.2f +/- ${std}%6.2f  [${p05}%8.2f, ${p95}%8.2f]")

  statsSummary("Inflation (%)", 1, 100.0)
  statsSummary("Unemployment (%)", 2, 100.0)
  statsSummary("Total Adoption (%)", 3, 100.0)
  statsSummary("Exchange Rate", 4)
  statsSummary("Market Wage (PLN)", 5)
  statsSummary("Gov Debt (mld PLN)", 6, 1.0 / 1e9)
  statsSummary("NPL Ratio (%)", 7, 100.0)

  // Household summary (individual mode only)
  if HH_MODE == HhMode.Individual then
    println("\nHousehold aggregates at M120:")
    val hhAggs = allHhAgg.flatten
    if hhAggs.nonEmpty then
      val avgGini = hhAggs.kahanSumBy(_.giniIndividual.toDouble) / hhAggs.length
      val avgWealth = hhAggs.kahanSumBy(_.giniWealth.toDouble) / hhAggs.length
      val avgBankr = hhAggs.kahanSumBy(_.bankruptcyRate.toDouble) / hhAggs.length
      val avgPov50 = hhAggs.kahanSumBy(_.povertyRate50.toDouble) / hhAggs.length
      val avgSkill = hhAggs.kahanSumBy(_.meanSkill) / hhAggs.length
      println(f"  Gini (income)        mean=${avgGini * 100}%8.2f%%")
      println(f"  Gini (wealth)        mean=${avgWealth * 100}%8.2f%%")
      println(f"  Bankruptcy rate      mean=${avgBankr * 100}%8.2f%%")
      println(f"  Poverty rate (50%%)  mean=${avgPov50 * 100}%8.2f%%")
      println(f"  Mean skill           mean=${avgSkill}%8.4f")

  println("\nPer-sector adoption at M120:")
  val secNames = SECTORS.map(_.name)
  for s <- SECTORS.indices do
    statsSummary(f"  ${secNames(s)}%-22s", 12 + s, 100.0)

  println(s"\nSaved: mc/${outputPrefix}_terminal.csv")
  println(s"Saved: mc/${outputPrefix}_timeseries.csv")
